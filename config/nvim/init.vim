" https://github.com/jessarcher/dotfiles/blob/master/nvim/init.vim
"--------------------------------------------------------------------------
" General settings
"--------------------------------------------------------------------------

syntax on     " Enables syntax highlighing
set hidden " Open multiple buffers at once
set ruler  " always show cursor
set clipboard=unnamedplus " clipboard accessible from vim

set relativenumber
set number
set nowrap

set tabstop=2     " Insert 2 spaces for a tab
set softtabstop=2 " Insert 2 spaces for a tab
set shiftwidth=2  " Change the number of space characters
set smarttab      " Makes tabbing smarter 
set expandtab     " Converts tabs to spaces
set smartindent   " Makes indenting smart
set autoindent    " Good auto indent

set hlsearch      " Highlight search results

set splitright
set splitbelow

set list
set listchars=tab:▸\ ,trail:·

set signcolumn=yes:2

set scrolloff=8
set sidescrolloff=8

"--------------------------------------------------------------------------
" FileTypes
"--------------------------------------------------------------------------
filetype plugin indent on " auto indent depends on filetype
autocmd BufNewFile,BufRead *.rs setlocal ft=rust
autocmd BufNewFile,BufRead *.adoc setlocal ft=asciidoc
autocmd BufNewFile,BufRead *.md setlocal ft=markdown

autocmd FileType md,markdown setlocal spell spelllang=en_us
autocmd FileType md,markdown setlocal wrap
autocmd FileType md,markdown setlocal linebreak

"--------------------------------------------------------------------------
" Key maps
"--------------------------------------------------------------------------

let mapleader="\<Space>"
let maplocalleader="\<Space>"

" Vim Conf quick editing
nmap <leader>ve :edit ~/.config/nvim/init.vim<cr>
nmap <leader>vr :source ~/.config/nvim/init.vim<cr>

" Close all open buffers
nmap <leader>Q :bufdo bdelete<cr>

" Misc
imap jj <ESC>
map gf :edit <cfile><cr>

" PANEL NAVIGATION use alt+hjkl to move between split/vsplit panels
tnoremap <C-h> <C-\><C-n><C-w>h
tnoremap <C-j> <C-\><C-n><C-w>j
tnoremap <C-k> <C-\><C-n><C-w>k
tnoremap <C-l> <C-\><C-n><C-w>l
nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l

nnoremap <TAB> :bnext<CR>       " TAB to next buffer
nnoremap <S-TAB> :bprevious<CR> " SHIFT-TAB will go back
noremap <leader>q :bd<CR>       " leader q closes the buffer

"--------------------------------------------------------------------------
" Plugins
"--------------------------------------------------------------------------

" Automatically install vim-plug
let data_dir = has('nvim') ? stdpath('data') . '/site' : '~/.vim'
if empty(glob(data_dir . '/autoload/plug.vim'))
  silent execute '!curl -fLo '.data_dir.'/autoload/plug.vim --create-dirs  https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

call plug#begin(data_dir . '/plugins')

source ~/.config/nvim/plugins/airline.vim
"source ~/.config/nvim/plugins/coc.vim
source ~/.config/nvim/plugins/conjure.vim
source ~/.config/nvim/plugins/dracula.vim
source ~/.config/nvim/plugins/markdown-preview.vim
source ~/.config/nvim/plugins/nerdtree.vim
source ~/.config/nvim/plugins/polyglot.vim
source ~/.config/nvim/plugins/stripws.vim
source ~/.config/nvim/plugins/zig.vim

"" Plugins to look at
" vim-commentary
" vim surround
" floatterm

call plug#end()
doautocmd User PlugLoaded
