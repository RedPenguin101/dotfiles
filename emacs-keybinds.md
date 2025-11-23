# Emacs keybinds

## Modes and leaders

- `i` Insert
- `I` Insert after
- `O` Insert overwrite
- `A` Insert beginning of line
- `E` Insert end of line
- `C-RET` Insert next line
- `S-RET` Insert prev line

- `C-j` (from insert mode) Command mode

## Vertical Motion

- `n` next line
- `p` previous line
- `C-v` page (half) down
- `M-v` page (half) up
- `l` move screen around cursor
- `M-r` move cursor to center/top/bottom
- `<` beginning of buffer
- `>` end of buffer

## Horizontal Motion

- `a` beginning of line
- `m` back to indentation
- `e` end of line
- `f` forward word
- `M-f` forward word
- `C-f` forward char
- `b` backward word
- `M-b` backward word
- `C-b` backward char

## Search

- `ss` search forward
- `sr` search back
- `sa` ag-project
- `so` occur
- `sq` query replace
- `vpq` project query replace
- `sh` highlight phrase
- `si` imenu
- `sI` imenu-to-buffer
- `v-SPC` set register
- `vj` jump to register

## Other

- `x` execute command
- `g` set mark transient
- `gg` set highlight
- `;` comment line
- `y` yank
- `q` reindent function
- `M-q` fill para
- `/` undo
- `\` shrink whitespace
- `vv` magit
- `v\` whitespace cleanup
- `vt` insert rectangle
- `v[` macro start
- `v]` macro end
- `vm` macro run
- `vpc` project compile
- `vpr` recompile
- `M-/` expand
- `TAB` indent or expand
- `C-M-\` indent region

## Windows and files

- `w` delete other windows
- `o` other window
- `M-o` other window
- `v1` delete other window
- `v2` split window below
- `v3` split window right
- `vf` find file
- `vpf` project find file
- `vs` save file
- `vps` save project buffers
- `vk` kill buffer
- `vpk` project kill buffers
- `vd` dired
- `vpd` project root dired
- `vr` recent files
- `vb` switch buffer
- `vpb` project switch to buffer

## Sexp motions

- `d` down list
- `u` up list
- `h` back sexp
- `j` fwd sexp
- `,` beginning of defun
- `.` end of defun

## Kills

- `kf` kill word
- `kb` kill back
- `kj` kill sexp
- `kn` kill inner sexp
- `kh` kill sexp back
- `kk` kill line
- `kl` kill whole line
- `kw` kill region
- `C-w` kill region
- `ks` kill ring save
- `M-w` kill ring save
- `k6` join line
- `kr` kill rectangle
- `kz` zap to character
- `ki` kill inner word

## Key semantics

Keys have semi-standard semantics in Emacs.

| key | editing            | file ops   | finding    | other                        |
|-----|--------------------|------------|------------|------------------------------|
| a   | beginning          |            |            | abbrev                       |
| b   | backward           | buffer     |            |                              |
| c   | capitalize         |            |            | close, commit/confirm        |
| d   | delete, down       | dired      |            | diff                         |
| e   | end                |            |            | eval, edit                   |
| f   | forward            | find       |            |                              |
| g   |                    |            | goto       | abort                        |
| h   | mark               |            |            | help                         |
| i   |                    |            | imenu      | info,indent/complete,inverse |
| j   | newline            |            |            |                              |
| k   | kill               | kill       |            |                              |
| l   | lowcase, move line |            |            |                              |
| m   | first significant  |            |            |                              |
| n   | next               |            |            | no, goal                     |
| o   | newline            | other      | occur      |                              |
| p   | previous, page     |            |            | project                      |
| q   | fill               |            |            | quit                         |
| r   | move cursor        | read only  | search bkw | rectangle, register, rename  |
| s   |                    | save       | search     |                              |
| t   | transpose          | tab        |            |                              |
| u   | up, upcase         | undo       |            | universal                    |
| v   | screen/page        |            |            | version control              |
| w   | region, kill       | write      | word       |                              |
| x   |                    |            |            | execute                      |
| y   | yank               |            |            | yes                          |
| z   | zap (kill)         |            |            | suspend                      |
| SPC |                    | mark       |            |                              |
| <>  | first/last         |            |            |                              |
| /   | expand             | undo       |            |                              |
| TAB | indent             |            |            | completion                   |
| ;   | comment            |            |            |                              |
| ^   | join               |            |            |                              |
| \   | whitespace         |            |            |                              |
| $   | spelling check     |            |            |                              |
| %   |                    |            | replace    |                              |
| .   |                    |            | tags       |                              |
| !   |                    |            |            | shell, all                   |
| &   |                    |            |            | ashell                       |
| -   |                    |            |            | negative                     |
| 0   |                    | this       |            |                              |
| 1   |                    | all others |            |                              |
| 2   |                    | below      |            |                              |
| 3   |                    | right      |            |                              |
| 4   |                    | other      |            |                              |
| 5   |                    | frame      |            |                              |
