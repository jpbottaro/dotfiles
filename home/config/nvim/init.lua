-- Neovim entry point.
-- Leader must be set before lazy/plugins load.
vim.g.mapleader = ","
vim.g.maplocalleader = ","

require("config.options")
require("config.keymaps")
require("config.lazy")
