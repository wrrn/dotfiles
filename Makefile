packages.stow = $(addsuffix .stow,$(wildcard *))
packages.unstow = $(addsuffix .unstow,$(wildcard *))

.PHONY: $(packages.stow)
$(packages.stow): 
	stow --no-folding --restow $(basename $@)

.PHONY: $(packages.unstow)
$(packages.unstow): 
	stow -D $(basename $@)


