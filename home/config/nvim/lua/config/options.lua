-- Editor options (ported from the old vimrc, modernized).
local opt = vim.opt

-- ui
opt.number = true
opt.signcolumn = "yes"
opt.scrolloff = 5
opt.showmatch = true
opt.termguicolors = true
opt.cursorline = true

-- search
opt.ignorecase = true
opt.smartcase = true
opt.hlsearch = false
opt.incsearch = true

-- indentation: 4-space, expand tabs
opt.shiftwidth = 4
opt.tabstop = 4
opt.expandtab = true
opt.autoindent = true
opt.smartindent = true
opt.smarttab = true
opt.textwidth = 80

-- files: no backup/swap, persistent undo (nvim picks its own undodir)
opt.backup = false
opt.writebackup = false
opt.swapfile = false
opt.undofile = true
opt.autoread = true

-- behaviour
opt.hidden = true
opt.completeopt = { "menu", "menuone", "noselect" }
opt.updatetime = 250
opt.timeoutlen = 400

-- system clipboard: native tools when there's a display, OSC52 when headless
-- (e.g. editing on the Pi over ssh/mosh) so yanks reach the local clipboard
opt.clipboard = "unnamedplus"
if not (os.getenv("DISPLAY") or os.getenv("WAYLAND_DISPLAY")) then
  local osc52 = require("vim.ui.clipboard.osc52")
  vim.g.clipboard = {
    name = "osc52",
    copy = { ["+"] = osc52.copy("+"), ["*"] = osc52.copy("*") },
    paste = { ["+"] = osc52.paste("+"), ["*"] = osc52.paste("*") },
  }
end

-- reload files changed outside of nvim
vim.api.nvim_create_autocmd({ "FocusGained", "BufEnter" }, { command = "checktime" })

-- colorscheme: built-in desert (classic vim look, no plugin)
vim.cmd.colorscheme("desert")
