Plug 'iamcco/markdown-preview.nvim', { 'do': { -> mkdp#util#install() }, 'for': ['markdown', 'vim-plug']}

let g:mkdp_markdown_css = expand('~/gitblog/css/style.css')

noremap <leader>mp :MarkdownPreview<CR>
