local global = vim.o
local window = vim.wo
local buffer = vim.bo
local set = vim.opt
---------------
--Basic setup--
---------------

set.relativenumber = true
set.number = true

buffer.expandtab = true --tabs to spaces
buffer.shiftwidth = 2   --number of spaces

set.list = true
set.listchars = {tab = '▸ ', trail = '·'}

set.hlsearch = true

set.scrolloff = 20
set.sidescrolloff = 20

set.clipboard = 'unnamedplus' -- Use Linux system clipboard

global.errorbells = false

-----------
--Keymaps--
-----------
--leader and local leader both space
vim.g.mapleader = ' '
vim.g.maplocalleader = ' '

-- jj as cancel
vim.keymap.set('i', 'jj', '<ESC>')
-- tab management: LD+q to close buffer, tab for next.
vim.keymap.set('n', '<Leader>q', ':bd<CR>')
vim.keymap.set('n', '<TAB>', ':bnext<CR>')
-- gf for goto highlighted file
vim.keymap.set('n', 'gf', ':edit <cfile><CR>')

-----------
-- TO DO --
-----------
-- CLang Format on save
-- get Clojure repl working again
-- Markdown preview working
-- Fennel dev environment, conjure (fireplace?)
-- Some of the tpope standards
-- Bufferline? Might write from scratch, good fennel project. Bufferline is a plugin
-- Code completion? maybe nvim-cmp
-- vim commentary? Doesn't seem like a plugin is needed for this, just a function?
-- floaterm?
-- Telescope? Need a use case really, don't feel I need it much
------------
-- Archer --
------------
-- See what Jess did https://github.com/jessarcher/dotfiles
-- Keymap
-- * Lk to nohlsearch? What's the difference between :noh and :nohlsearch?
-- * Insertion of trailing ; with ;; in insert mode - can this be C-mode only?
-- * Moving lines up and down with - what? Ctrl+hjkl? Sort of like VSCode
-- * Some sort of save short cut. double-tap s maybe?
-- * Way to paste in insert mode - pp?
-- Opts - what do they do?
-- breakindent, termguicolors, undofile, title, ignorecase, smartcase, wildmode, showmode, confirm, fillchars, shortmess

