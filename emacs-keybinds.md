# Emacs keybinds

## Movement in Program

Default keys to change windows in frame: `C+<num>`.
`M-o` to switch window.

`C-f` to find file in project, `C-b` to find buffer.

## Movement in Buffer

Movement keys are `hjkl`, vim style.
Modifiers escalate from `C` to `C-M` to `M`.
`C` is (fwd/back) word and (up/down) line.
`C-M` is (f/b) sentence and (u/d) para.

_Except_ for when in lisp mode, where `C-M` are sexp bindings

`M` is (f/b) buffer, (u/d) page.

`C-;` is recenter.

## Copy/Kill/Paste

```
 kill rebinds
       w       d           e
 C   bk-wd   fw-char   [end of line]
 M   copy-r  fw-word   Kill region
 CM  region  sent      Kill region
```

```
 kill rebinds
        N       M            Y
 C   nextline newline       yank
 M   [none]   firstnon-ws   yank-pop
 CM  fwdsexp  [none]        yank
```

If there's a region marked, any modifer with N will copy, any modifier with M will kill.

`M-f` kill forward word

## Others

`C-'` autofill word
`C(-M)-SPC` set mark (added C-M to maintain tempo)
`C-x v p` VC Pull (to match `C-x v P` Push)
