//----------------------------------------------------------------------------
/// \file  euds.cpp
//----------------------------------------------------------------------------
/// \brief Implementation of Erlang Unix Domain Socket NIFs.
//----------------------------------------------------------------------------
// Copyright (c) 2015 Serge Aleynikov <saleyn@gmail.com>
// Created: 2015-01-23
//----------------------------------------------------------------------------
/*
   ***** BEGIN LICENSE BLOCK *****

   This file is part of the euds open-source project.

   Copyright (C) 2015 Serge Aleynikov <saleyn@gmail.com>

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

   ***** END LICENSE BLOCK *****
*/
#include "erl_nif.h"
#include <sys/socket.h>
#include <sys/un.h>
#include <string.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <errno.h>

#ifndef enif_is_number
#define enif_is_number(X,Y) Y
#endif

static ERL_NIF_TERM am_ok;
static ERL_NIF_TERM am_error;
static ERL_NIF_TERM am_true;
static ERL_NIF_TERM am_false;
static ERL_NIF_TERM am_stream;
static ERL_NIF_TERM am_dgram;
static ERL_NIF_TERM am_reuseaddr;
static ERL_NIF_TERM am_timeout;

static ERL_NIF_TERM sock_connect(ErlNifEnv*, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM sock_bind   (ErlNifEnv*, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM sock_send   (ErlNifEnv*, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM sock_close  (ErlNifEnv*, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM sock_send_fd(ErlNifEnv*, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM sock_recv_fd(ErlNifEnv*, int argc, const ERL_NIF_TERM argv[]);

namespace
{
	struct sock_ancil_data {
      struct cmsghdr h;
      int            fd[1];
    };
}

bool decode_flags(ErlNifEnv* env, ERL_NIF_TERM list, bool& stream, bool& reuse)
{
    int   arity;
    const ERL_NIF_TERM* tuple;
    ERL_NIF_TERM head;

    while (enif_get_list_cell(env, list, &head, &list)) {
        if (head == am_reuseaddr)
            reuse = true;
        else if (head == am_stream)
            stream = true;
        else if (head == am_dgram)
            stream = false;
        else if (enif_get_tuple(env, head, &arity, &tuple) && arity == 2) {
            if (tuple[0] == am_reuseaddr && (tuple[1] == am_true || tuple[1] == am_false))
                reuse = tuple[1] == am_true;
            else
                return false;
        } else
            return false;
    }

    return true;
}

static ERL_NIF_TERM describe_error(ErlNifEnv* env, int err) {
    switch (err) {
        case EACCES:        return enif_make_atom(env, "eacces");
        case EPERM:         return enif_make_atom(env, "eperm");
        case EADDRINUSE:    return enif_make_atom(env, "eaddrinuse");
        case EAFNOSUPPORT:  return enif_make_atom(env, "eafnosupport");
        case EAGAIN:        return enif_make_atom(env, "eagain");
        case EALREADY:      return enif_make_atom(env, "ealready");
        case EBADF:         return enif_make_atom(env, "ebadf");
        case ECONNREFUSED:  return enif_make_atom(env, "econnrefused");
        case ECONNRESET:    return enif_make_atom(env, "econnreset");
        case EFAULT:        return enif_make_atom(env, "efault");
        case EINPROGRESS:   return enif_make_atom(env, "einprogress");
        case EINVAL:        return enif_make_atom(env, "einval");
        case EINTR:         return enif_make_atom(env, "eintr");
        case EISCONN:       return enif_make_atom(env, "eisconn");
        case ENETUNREACH:   return enif_make_atom(env, "enetunreach");
        case ENOTSOCK:      return enif_make_atom(env, "enotsock");
        case ETIMEDOUT:     return enif_make_atom(env, "etimedout");
        case EMFILE:        return enif_make_atom(env, "emfile"); // Too many open files
        default:            return enif_make_int (env, err);
    }
}

// connect(Addr::list(), Options) -> {ok, FD::integer()} | {error, Reason}
//  Options :: [{type, stream | dgram} | reuseaddr | {reuseaddr, boolean()}]
static int sock_open(ErlNifEnv* env, ERL_NIF_TERM list) {
    bool stream = true;
    bool reuse  = false;

    if (!enif_is_list(env, list))
        return enif_make_badarg(env);

    if (!decode_flags(env, list, stream, reuse))
        return enif_make_badarg(env);

    int fd = socket(PF_UNIX, stream ? SOCK_STREAM : SOCK_DGRAM, 0);

    if (fd < 0)
        return fd;

    if (reuse) {
        int       opt = reuse;
        socklen_t len = sizeof(opt);
        int       rc  = setsockopt(fd, SOL_SOCKET, SO_REUSEADDR, &opt, len);
        if (rc < 0) {
            int err = errno;
            close(fd);
            errno = err;
            return -1;
        }
    }

    return fd;
}

// connect(Addr::list(), Options) -> {ok, FD::integer()} | {error, Reason}
//  Options :: [{type, stream | dgram} | reuseaddr | {reuseaddr, boolean()}]
static ERL_NIF_TERM sock_connect(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    struct sockaddr_un addr;
    char path[sizeof(addr.sun_path)];

    if (argc != 2 || !enif_is_list(env,argv[0]) || !enif_is_list(env,argv[1]))
        return enif_make_badarg(env);

    memset(&addr, 0, sizeof(struct sockaddr_un));
    memset(path,  0, sizeof(addr.sun_path));
    enif_get_string(env, argv[0], path, sizeof(addr.sun_path)-1, ERL_NIF_LATIN1);

    int fd = sock_open(env, argv[1]);

    if (fd < 0)
        return enif_make_tuple2(env, am_error, describe_error(env, errno));

    addr.sun_family = AF_UNIX;
    snprintf(addr.sun_path, sizeof(addr.sun_path) -1 , "%s", path);
    if (connect(fd, (struct sockaddr*)&addr, sizeof(struct sockaddr_un)))
        return enif_make_tuple2(env, am_error, describe_error(env, errno));

    return enif_make_tuple2(env, am_ok, enif_make_int(env, fd));
}

static ERL_NIF_TERM sock_bind(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    struct sockaddr_un addr;
    char path[sizeof(addr.sun_path)];

    if (argc != 2 || !enif_is_list(env,argv[0]) || !enif_is_list(env,argv[1]))
        return enif_make_badarg(env);

    memset(&addr, 0, sizeof(struct sockaddr_un));
    memset(path,  0, sizeof(addr.sun_path));
    enif_get_string(env, argv[0], path, sizeof(addr.sun_path)-1, ERL_NIF_LATIN1);

    int fd = sock_open(env, argv[1]);

    if (fd < 0)
        return enif_make_tuple2(env, am_error, describe_error(env, errno));

    addr.sun_family = AF_UNIX;
    snprintf(addr.sun_path, sizeof(addr.sun_path) -1 , "%s", path);

    return (bind(fd, (struct sockaddr*)&addr, sizeof(struct sockaddr_un)) < 0)
        ? enif_make_tuple2(env, am_error, describe_error(env, errno))
        : enif_make_tuple2(env, am_ok, enif_make_int(env, fd));
}

static ERL_NIF_TERM sock_send(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    int fd;
    ErlNifBinary bin;

    if (argc != 2 ||
        !enif_get_int(env, argv[0], &fd) ||
        !enif_inspect_binary(env,argv[1], &bin))
        return enif_make_badarg(env);

    int rc = send(fd, bin.data, bin.size, 0);

    if (rc != int(bin.size))
        return enif_make_tuple2(env, am_error, describe_error(env, errno));

    return am_ok;
}

static ERL_NIF_TERM sock_close(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    int fd = 0; 

    if (argc != 1 || !enif_get_int(env, argv[0], &fd))
        return enif_make_badarg(env);

    close(fd);

    return am_ok;
}

static ERL_NIF_TERM sock_send_fd(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    int fd;
    int sendfd;

    if (argc != 2 ||
        !enif_get_int(env, argv[0], &fd) ||
        !enif_get_int(env, argv[1], &sendfd))
        return enif_make_badarg(env);

    sock_ancil_data buf;
    struct msghdr   msghdr;
    char            nothing = '!';
    struct iovec    nothing_ptr = {&nothing, 1};
    struct cmsghdr* cmsg;


    msghdr.msg_name        = NULL;
    msghdr.msg_namelen     = 0;
    msghdr.msg_iov         = &nothing_ptr;
    msghdr.msg_iovlen      = 1;
    msghdr.msg_flags       = 0;
    msghdr.msg_control     = &buf;
    msghdr.msg_controllen  = sizeof(struct cmsghdr) + sizeof(int);
    cmsg = CMSG_FIRSTHDR(&msghdr);
    cmsg->cmsg_len         = msghdr.msg_controllen;
    cmsg->cmsg_level       = SOL_SOCKET;
    cmsg->cmsg_type        = SCM_RIGHTS;
    *(int*)CMSG_DATA(cmsg) = sendfd;
    if (sendmsg(fd, &msghdr, 0) < 0)
        return enif_make_tuple2(env, am_error, describe_error(env, errno));

    return am_ok;
}

static ERL_NIF_TERM sock_recv_fd(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    int fd;

    if (argc != 1 ||
        !enif_get_int(env, argv[0], &fd))
        return enif_make_badarg(env);

    sock_ancil_data buf;
    struct msghdr   msghdr;
    char            nothing;
    struct iovec    nothing_ptr = {&nothing, 1};
    struct cmsghdr* cmsg;

    msghdr.msg_name        = NULL;
    msghdr.msg_namelen     = 0;
    msghdr.msg_iov         = &nothing_ptr;
    msghdr.msg_iovlen      = 1;
    msghdr.msg_flags       = 0;
    msghdr.msg_control     = &buf;
    msghdr.msg_controllen  = sizeof(struct cmsghdr) + sizeof(int);
    cmsg = CMSG_FIRSTHDR(&msghdr);
    cmsg->cmsg_len         = msghdr.msg_controllen;
    cmsg->cmsg_level       = SOL_SOCKET;
    cmsg->cmsg_type        = SCM_RIGHTS;
    *(int*)CMSG_DATA(cmsg) = -1;

    if (recvmsg(fd, &msghdr, MSG_DONTWAIT) < 0)
		return errno == EAGAIN
            ? am_timeout
            : enif_make_tuple2(env, am_error, describe_error(env, errno));

    int recvfd = *(int*)CMSG_DATA(cmsg);

    return enif_make_tuple2(env, am_ok, enif_make_int(env, recvfd));
}

static int on_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    am_ok        = enif_make_atom(env, "ok");
    am_error     = enif_make_atom(env, "error");
    am_true      = enif_make_atom(env, "true");
    am_false     = enif_make_atom(env, "false");
    am_stream    = enif_make_atom(env, "stream");
    am_dgram     = enif_make_atom(env, "dgram");
    am_reuseaddr = enif_make_atom(env, "reuseaddr");
    am_timeout   = enif_make_atom(env, "timeout");
    return 0;
}

static int on_reload (ErlNifEnv* env, void**,         ERL_NIF_TERM) { return 0; }
static int on_upgrade(ErlNifEnv* env, void**, void**, ERL_NIF_TERM) { return 0; }

static ErlNifFunc nif_funcs[] = {
    {"do_connect", 2, sock_connect},
    {"do_bind",    2, sock_bind},
    {"do_send",    2, sock_send},
    {"do_close",   1, sock_close},
    {"do_send_fd", 2, sock_send_fd},
    {"do_recv_fd", 1, sock_recv_fd},
};

ERL_NIF_INIT(gen_uds, nif_funcs, &on_load, &on_reload, &on_upgrade, NULL)
