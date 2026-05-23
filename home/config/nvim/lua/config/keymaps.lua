-- General keymaps.
local map = vim.keymap.set

-- jk leaves insert mode (kept from the old vimrc)
map("i", "jk", "<Esc>", { desc = "Exit insert mode" })

-- clear search highlight
map("n", "<Esc>", "<cmd>nohlsearch<cr>", { desc = "Clear search highlight" })

-- buffer navigation (replaces the old C-Tab maps + Bclose function)
map("n", "<S-l>", "<cmd>bnext<cr>", { desc = "Next buffer" })
map("n", "<S-h>", "<cmd>bprevious<cr>", { desc = "Previous buffer" })
map("n", "<leader>bd", "<cmd>bdelete<cr>", { desc = "Delete buffer" })

-- file explorer (built-in netrw, same <C-o> binding NERDTree used)
map("n", "<C-o>", "<cmd>Explore<cr>", { desc = "File explorer" })
