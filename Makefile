all : compile

.PHONY: compile release clean test
compile:
	rebar compile

test:
	rebar eunit

clean:
	rebar clean

docs: doc ebin clean-docs
	@rebar doc skip_deps=true

doc ebin:
	mkdir -p $@

clean-docs:
	rm -f doc/*.{css,html,png} doc/edoc-info

github-docs:
	@if git branch | grep -q gh-pages ; then \
		git checkout gh-pages; \
	else \
		git checkout -b gh-pages; \
	fi
	git checkout master -- src
	git checkout master -- Makefile rebar.*
	make docs
	mv doc/*.* .
	make clean
	rm -fr src c_src include Makefile erl_crash.dump priv rebar.* README*
	@FILES=`git st -uall --porcelain | sed -n '/^?? [A-Za-z0-9]/{s/?? //p}'`; \
	for f in $$FILES ; do \
		echo "Adding $$f"; git add $$f; \
	done
	@sh -c "ret=0; set +e; \
		if   git commit -a --amend -m 'Documentation updated'; \
		then git push origin +gh-pages; echo 'Pushed gh-pages to origin'; \
		else ret=1; git reset --hard; \
		fi; \
		set -e; git checkout master && echo 'Switched to master'; exit $$ret"

