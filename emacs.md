# Notes on my Emacs configuration

Most of my emacs config is pretty standard.
There are a few packages and features I consider essential:

* Ivy / precient for making the minibuffer effective
* beacon-mode for showing where you are
* recentf-mode

Which-key is useful but not essential when you know things well.

Some config is probably in 90% of other peoples configs: removing menu bar, startup, bell.
I simplify the mode line so it's just the file name and the save state - nothing else needed.

The major changes are to the motion controls. Instead of n/p/u/d (which IMO are terrible), I remap almost all motion to hjkl. This replaces a few things, but the only real critical one is `C-h` replacing help. I use this infrequently enough that using the M-x menu (or `C-x C-m` in my case.)

## Motion

Most motions use the `hjkl` bindings for left, down, up and right.
They are used with modifiers `C-`, `C-M-` and `M-`. Each of these respectively will move by larger 'chunks'.

In most modes, `C-` moves by word (left/right), and line (up/down). `C-M-` moves sentence and paragraph, `M-` moves page and buffer.

The exception is for lisp modes, where the `C-` bindings are replaced by sexp previous/next for left/right, and sexp in/out for up/down.

Other common move commands are beginning and end of line: `C-a` and `C-e` respectively, as in default emacs.

`C-;` is recenter-top-bottom, which puts the cursor line at the top, middle or bottom of the buffer on respective hits.

## Kill and copy

Almost all kill and copy is done by selecting the region (`C-<space>` and `C-M-<space>`) and doing `<mod>-p` (kill) or `<mod>-n` (copy).

`C-w` is kill backwards word, since this is such a common shortcut outside of emacs.

`C-y` is yank, as usual, as is `C-M-y` for tempo.

## Window and buffer controls

Mostly the changes are just shortcuts for very commonly used things.

`C-x C-r` for recent files buffer
`C-f` is find file in project.
`C-b` is switch to buffer (a shortcut for `C-x b`).
`M-o` is other-window (`C-x o` shortcut).
`C-d` is delete-other-windows (`C-x 1` shortcut).
`C-+` and `C--` increase and decrease the text scale.

## Other keyboard things

The only other notable binding change is `C-'` for dabbrev-expand, which by default is on `C-M-/` but is moved for convenience.
