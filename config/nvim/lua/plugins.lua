vim.cmd [[packadd packer.nvim]]

return require('packer').startup(function(use)
  print('Loading plugins')
  use 'wbthomason/packer.nvim'
  use('mjlbach/onedark.nvim')
  use {'nvim-treesitter/nvim-treesitter', run = ':TSUpdate'}
  use {'lewis6991/gitsigns.nvim',
       config=function()
         require('gitsigns').setup()
       end}

  require'nvim-treesitter.configs'.setup {
    ensure_installed = { "c", "lua" },
    sync_install= false,
    auto_install = true,
    highlight = {
      enable = true,
    },
  }
end)

