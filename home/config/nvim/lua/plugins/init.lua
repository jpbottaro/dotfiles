-- Plugin specs, imported by lazy.nvim. Deliberately minimal.
return {
  -- statusline (replaces vim-airline)
  {
    "nvim-lualine/lualine.nvim",
    dependencies = { "nvim-tree/nvim-web-devicons" },
    opts = {
      options = { theme = "auto", globalstatus = true },
    },
  },

  -- syntax / indent via treesitter (replaces `syntax on`)
  {
    "nvim-treesitter/nvim-treesitter",
    branch = "master",  -- stable API; the repo default `main` is a WIP rewrite
    build = ":TSUpdate",
    config = function()
      require("nvim-treesitter.configs").setup({
        ensure_installed = {
          "lua", "vim", "vimdoc", "bash",
          "python", "rust", "terraform", "hcl",
          "json", "yaml", "toml", "markdown",
        },
        auto_install = true,
        highlight = { enable = true },
        indent = { enable = true },
      })
    end,
  },
}
