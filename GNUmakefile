TAGS=$(shell git for-each-ref --sort=taggerdate --format '%(refname)' refs/tags | sed -e 's|refs/tags/||g')

Sandbox/%.rda:
	export DIR=Sandbox ; ../mkGitTagFiles -r $(patsubst %.rda,%,$(notdir $@))

timings: $(patsubst %,Sandbox/%.rda,$(TAGS))

tags:
	@for f in $(TAGS); do echo "$$f" ; done
