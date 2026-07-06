# Emacs

This page is a reference for me on how emacs works, focusing on stuff I tend to forget.

## VC

Simple, fast, and consistent.

vc-mode doesn't have the git-specific concept of 'staging'. You can include only a certain set of files in a commit using marks.

`C-x v d` vc-dir
`C-x p v` project-vc-dir

`C-x v =` see diff to last commit
`C-x v v` commit file

### VC Mode

`P` push
`+` pull

`b c` branch create
`b l` branch log
`b s` branch switch
(oddly there is no command for listing branches)

`v` next action - register, commit
`m` mark file (allows you to commit only the selection)
`M` mark all files with this status
`u/U` unmark (all)
`l` vc-print-log (over header or file)

### Commit mode

`C-c C-c` commit
`C-c C-k` cancel
`C-c C-d` diff
`C-c C-e` toggle amending of last commit

### Partial commits

You can _technically_ do partial commits, but it's a bit inelegant and limited. Use `C-x v =` to see the diff. `k`ill any hunk you don't want to commit (`C-c C-s` to split a hunk), the `C-x v v` from the diff buffer to commit any unkilled hunks.

### Conflict resolution

`vc-resolve-conflicts`
`vc-ediff`

## Dired

https://www.youtube.com/watch?v=hshWBtefSmE

### Getting to it

`C-x C-j` dired jump (`v d` in my command mode)
`C-x d` dired
`find-name-dired` to find all files matching pattern and pipe to dired.

### Sorting and Detail

`s` toggle alpha-sort vs. timestamp sort
`(` toggle hide details
`C-u s` redisplay with custom listing flags (defaults are in `dired-listing-switches` var - I like `-al --group-directories-first`)

(How to sort dirs first?)

### Writable Dired

`C-x C-q` start wdired
`C-c C-c` finish

rename `file1` to `folder1/file1` to move a file into a directory

if `wdired-allow-to-change-permissions` is set, you can directly edit the permission bits

### Moving around

`n/p` next/prev. `spc` also next
`>/<` next/prev folders only
`M-{/}` move between marked files

### Opening

`ret` `f` or `e` all open the file
`o` for open file in new window. `C-o` for open but retain focus
`v` open in view mode

### Directory navigation

`^` up one level
`ret` on folder, open the folder

### Subdirs

`i` list subdirectory in current buffer 
(note: mark is set, so return to original with mark pop)
`C-M-n/p` move between dir lists
`C-M-u/d` move up or down subdir (`C-M-d` doesn't work for me - swallowed by macos?)
`$` toggle hide files in subdir (supposedly `M-$` to toggle all)
`M-G` jump to directory header

### Flag and Mark

`u` unmark / unflag
`U` unmark / unflag all

`d` flag for deletion
`x` delete flagged for deletion
`%d` flag with regexp

`m` mark
`%m` mark with regexp (also `*%`)
`**` mark executables
`*@` mark symlinks
`*/` mark directories
`*s` mark files/dirs in current subdir
`t` transpose mark (things with mark becomes unmarked and vv)

### Actions

`+` new subdir
`R` move marked files (or rename single file)
`C` copy marked files (set `dired-create-destination-dirs` to autocreate, or `ask` to ask)
`E` open marked files in appropriate program
`H/S` hard/symlinks
`M/G/O` chmod/grp/own
`A` search regex in all marked (XREF style find)
`Q` regexp replace in all marked
`!` shell command (`&` for async)

notable ls switches:
- `a` include `./..`
- `l` long form
- `t` sort by modified time, as opposed to alphanum
- `u` sort by accessed time
- `r` reverse sort

## Completion

A "completion" is when you have a string, and you ask emacs for a list of candidates that could match that string. Emacs has a lot of options around how that candidate list is generated, how it is filtered, and how it's displayed.

Completions happen in three interelated contexts:

1. In the minibuffer, like when you do `C-x f` or `M-x`
2. In the code buffer, like an autocomplete in an IDE
3. In a dedicated `*Completions*` window that pops up

All of these start at the same place: `completion-at-point`. This is the thing that generates the candidate list. The two main things that determine the candidate list are:

1. `completion-at-point-functions`
2. `completion-styles`

A completion at point function (CAPF) initially generates a list of candidates. For the minibuffer this is pretty simple, it's like the list of commands for M-x. Or the list of files in a folder. For code it can the result of a dabbrev (`dabbrev-capf`), or a tags table (`tags-completion-at-point-function`).

Completion styles determine how 'fuzzy' the matching is. There are styles like `basic` `initial` `partial-completion` `flex`, all of which offer different strictness of matching. Generally these are arranged in the `completion-styles` list in most-to-least strict order.

### Completion Window

Completion windows have been around in long time in emacs. You would be in a buffer or minibuffer, you would type part of a thing, and then ask for a completion window which would list out all the candidates. You'd look at the candidates, and either 

- just continue typing what you want
- navigate and select from the completion window indirectly with `M-<down/up/RET>`
- focus the completion window and navigate/select with `n/p/RET`

This is still a fully usable system for both minibuffer and buffer completions. Try opening a vanilla emacs session (`emacs -Q`), doing `M-x` and then typing `find`, then hitting tab. Or, opening a code file which has an associated tags table somewhere, typing a letter and hitting `M-C-i` to open the completions window.

### Minibuffer completion and FIDO

Completion windows used to be how things worked. But it wasn't very reactive in the minibuffer. You had to type, then bring up the completions window, then scan, then select. There was no immediate feedback.

This changed with icomplete-mode. icomplete mode made it so when you typed in the minibuffer, the candidate completions were displayed _as you typed_. This same idea was extended with Ido mode, and later Fido mode, which does the same thing better and is the usual out of the box option today.

Turn on `fido-mode` to see it in action.

### Buffer completion

Completion in buffers never really got much more sophisticated than the completion window approach, mainly because LSPs took over the role of giving you options of how to complete your code fragments.


