# Notes on my Emacs configuration

## Modal keybinds

I use a custom-build command-layer / mode system. You are either in
Insert Mode (in which case the cursor is a line), or command mode
(where the cursor is a block).

### Insert Mode

Insert mode keybinds are the same as regular emacs (assuming you
haven't rebound anything) with the exception that:

* `C-g` exits Insert Mode and enters command mode
* `TAB` autocomplete with dabbrev-complete. (in command mode, TAB is
  generally used for indentation)

### Command Mode

- `f` forward word
- `b` backward word
- `a` beginning of line
- `m` back to indendatation
- `e` end of line
- `n` next line
- `p` previous line

- `j` forward sexp
- `h` backward sexp
- `d` down list
- `u` up list

- ',' back defun
- '.' forward defun

- `<` start of buffer
- `>` end of buffer

- `y` yank
- '^' join with line above
- `l` recenter
- `;` comment line
- `\` whitespace shrink
- `q` reflow

- `/` undo
- `o` other window
- `w` delete other windows
- `x` execute extended command
- `g` set mark
- `z` repeat

- digits: number arguments
- `-` negative argument

All the stand C and M modifiers work fine, so you can still use most
standard emacs functions (like foward-char with `C-f`) from the
command layer. Brackets and (most) other punction also work from command
mode.

### General Leader v

- `f` find file
- `s` save buffer
- `k` kill buffer
- `b` switch buffer
- `d` dired
- `r` recent files
- `v` magit status
- `\` Clean buffer whitespace
- `t` rectangle insert

- `1` delete other windows
- `2` split below
- `3` split right

- `[` start macro
- `]` end macro
- `m` call macro

### Search Leader s

- `s` search
- `r` seach backward
- `o` occur
- `a` ag-search
- `q` find-replace
- `h` highlight
- `i` imenu
- `I` imenu in a buffer

### Kill Leader k

- `f` forward word
- `b` backward word
- `j` forward sexp
- `h` backward sexp
- `k` line from point
- `l` whole line
- `w` region
- `s` save region
- `6` join with line above
- `r` rectangle

### Project Leader v-p

- `f` find file
- `s` save files
- `k` kill all buffers
- `d` dired
- `q` query replace
- `b` switch buffer
- `c` compile
- `r` recompile

## Themes

I use prot's modus-operandi themes, which come with emacs.

## Configuration

I do the normal tidy up stuff: remove scratch buffer message, remove
audible bell in favor of a visible one, make lock/backup files
bearable, spaces over tabs.

I have transient-mark-mode (i.e. highlight mode) off.

I turn relative linenumbers on globally.

I use FIDO vertical.

I do a `display-buffer-alist` customizations for occur, compilation,
imenu and magit.

I have whitespace-mode on nearly everything, with a 120 column limit.

I save everything in history, and enable desktop-save-mode.

I have some custom functions for pipeing recentf-list into a
minibuffer so it can be used like find file. And a custom function
which pipes imenu to a compilation-mode buffer so it's like an 'index'
of the file.

## Packages

I use diff-hl, magit, visible-mark, and ag (assuming silver searcher
is installed). Sometimes I'll use company mode, but not often.

Then whatever mode-packages for the languages/formats I'm using.
