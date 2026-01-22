
packages = $(wildcard *)
packages.stow = $(addsuffix .stow,$(packages))
packages.unstow = $(addsuffix .unstow,$(packages))
packages.stow.force = $(addsuffix .force,$(packages.stow))
packages.conflicts.delete = $(addsuffix .conflicts.delete,$(packages))




$(packages.stow): # Placing this here gives us auto complete
%.stow:
	stow --no-folding --dotfiles --restow $(basename $@)

$(packages.stow.force): #Here for the auto completion
%.stow.force: package = $(@:.stow.force=)
%.stow.force:  %.conflicts.delete %.stow
	@#Only run the dependencies

$(packages.conflicts.delete): # Here for the autocompletion
%.conflicts.delete: package = $*
%.conflicts.delete: dotfiles = $(shell fd --type file --base-directory $(package) --hidden)
%.conflicts.delete:	paths = $(addprefix ../,$(dotfiles))
%.conflicts.delete:
	rm -rf $(paths)

.PHONY: FORCE
FORCE:
	@echo forced




$(packages.unstow):
%.unstow:
	stow --dotfiles -D $(basename $@)


