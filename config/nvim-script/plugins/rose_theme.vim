"https://github.com/rose-pine/
Plug 'rose-pine/neovim', { 'as': 'rose-pine' }

augroup ThemeOverrides
    autocmd!
    autocmd User PlugLoaded ++nested colorscheme rose-pine
augroup end
