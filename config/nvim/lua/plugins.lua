vim.cmd [[packadd packer.nvim]]

return require('packer').startup(function(use)
  print('Loading plugins')
  use 'wbthomason/packer.nvim'
  use 'nvim-treesitter/nvim-treesitter'
  use 'neovim/nvim-lspconfig'
  use 'ziglang/zig.vim'

  require'nvim-treesitter.configs'.setup {
    ensure_installed = { "c", "lua", "zig"},
    sync_install= false,
    auto_install = true,
    highlight = {
      enable = true,
      disable = {"markdown"}
    },
  }
end)

