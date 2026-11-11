# Which top-level directories are stow packages, and which of those the Home
# Manager module can link. Every other top-level directory is a package whose
# layout mirrors $HOME.
{
  # Top-level directories that aren't packages at all.
  notPackages = [
    "design-log"
    "nix"
  ];

  # Packages whose layout doesn't mirror $HOME. Consumers read their files
  # directly (`${inputs.dotfiles}/firefox/userChrome.css`), so the module
  # declares no options for them.
  notLinkable = [
    "bettertouchtool"
    "brew"
    "firefox"
  ];
}
