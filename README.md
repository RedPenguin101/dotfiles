# Dotfiles

## Emacs
Packages to try
* dumb-jump: jump to function definition
* multiple-cursors
* magit/gitgutter/gittimeline
* darkroom mode (or olivetti)
* use-package
* which-key
* beacon
* some tab thing
* Something for bookmarking
* Maybe vertico as replacement for ivy
* dimmer.el (dims non-active windows)
* highlight-symbol:  (maybe symbol overlay)
* whitespace - highlights for trailing?
* rainbox delimiters
* flycheck? syntax checking
* apheliei for formatting
* crux 
* paredit

### My custom keybinds
* avy: `C-'`: 2char search
* avy: `C-"`: goto line
* `C-c p` projectile
  * `f`: find file in project
* `C-xr` open a recent file

## Setting a solid color background in Gnome

From https://andrewmccarthy.ie/setting-a-blank-desktop-background-in-gnome.html

```bash
gsettings set org.gnome.desktop.background picture-uri none
gsettings set org.gnome.desktop.background primary-color '#e6fcfc'
gsettings set org.gnome.desktop.background color-shading-type 'solid'
```

## NeoVim
All files in this subfolder should be put to _~/.config/nvim/_

Custom leader (`space`) functions:

* `ve`/`vr`: edit/re-source config
* `q`: close current buffer
* `Q`: Close all buffers
* `mp`: Markdown Preview

### Conjure
* `er` eval top level ('root')
* `ee` eval expression
* `ecr` eval root with comment output
* `e!` eval and replace
* `eb` eval buffer
* `K` look up docs
* `ta`/`tn`/`tc` test all / namespace / under cursor
* `rr` refresh changed namespaces
* `ls`/`lv`: log buffer open hor/vert

### Vim-Sexp
[Link](https://github.com/guns/vim-sexp), [tpope](https://github.com/tpope/vim-sexp-mappings-for-regular-people)

* `==` Auto-indent
* `<L>i/w` wrap compound/element in `()`
* `<L>(e)[/{` wrap (element) compound in `[]/{}`
* `<Alt-S-l>` Slurp fwd (h=back)
* `<Alt-S-k>` Barf fwd (j=back)
* `>)` slurp fwd
* `<)` barf fwd
* `()` move to nearest bracket
* `W/B`etc are now sexp motions (tpope)

### Other custom keymaps
* `jj`: escape insert mode
* `C-hjkl`: Move windows
* `Tab`: next buffer (shift for previous)
* `C-b`: Nerdtree window toggle

### Easymotion
Easy motion makes it easier to navigate around your vim buffer. Using the below shortcuts will highlight all matches in the buffer, and give you a shortcut key (in red) to jump to that location.

* Easymotion leader key: `<Leader>s`
* Search for char 'g': `<Leader>sg enter`
* Search for char-pair 'mo': `<Leader>smo`

## Kitty
conf files live in _~/.config/kitty/_

Kitty is installed by default in _/home/me/.local/kitty.app/bin/kitty_. You have to symlink it to you _/usr/bin/kitty_.

### Shortcuts
* New tab `ctrl+shift+t`
* Change tab `ctrl+shift+h/j/k/l`
* Rename tab `ctrl+shift+alt+t`
* New Window `ctrl+shift+enter`
* Change Window rel `ctrl+shift+]/[`
* Change Window abs `ctrl+shift+1,2` etc.
* Toggle stack layout `ctrl+shift+l`
* Change layout `ctrl+alt+shift+l`

### Layouts
* Fat -- One windows are shown full width on the top
* Grid
* Horizontal
* Splits -- Windows arranged in arbitrary patterns created using horizontal and vertical splits
* Stack -- Only a single maximized window is shown at a time
* Tall -- One windows are shown full height on the left
* Vertical -- All windows are shown one below the other

## Surf
* Ctrl+h/l back/fwd
* Crtl+g go to url
* Ctrl+Shift+k/j: zoom in/out
* Ctrl+Shift+q: reset zoom

