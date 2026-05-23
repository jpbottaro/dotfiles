# dotfiles

Personal dotfiles + cross-distro machine setup. Everything under `home/` is
symlinked into `$HOME` by `./bootstrap` (see `README`).

## Commit message modules

Prefix every commit subject with `[module]`. Valid modules:

- `bash` — `home/bashrc`, `home/bash_aliases`
- `tmux` — `home/tmux.conf`
- `vim` — `home/vimrc`, `home/config/nvim/`
- `git` — `home/gitconfig`, `home/gitignore_global`
- `inputrc` — `home/inputrc`
- `build` — `bootstrap`, `packages/`
- `docs` — `README`, this file
