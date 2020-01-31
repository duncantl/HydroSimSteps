ifeq ($(DIR),)
 DIR=Sandbox
endif

TAGS=$(shell git for-each-ref --sort=taggerdate --format '%(refname)' refs/tags | sed -e 's|refs/tags/||g')

Sandbox/%.rda:
	if ! test -d $(DIR); then mkdir $(DIR) ; fi
	export DIR=$(DIR) ; ../mkGitTagFiles -r $(patsubst %.rda,%,$(notdir $@))

timings: $(patsubst %,Sandbox/%.rda,$(TAGS))

tags:
	@for f in $(TAGS); do echo "$$f" ; done

$(DIR):
	mkdir $(DIR)
