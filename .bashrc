#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'
alias la='ls -lah'
alias v='nvim'
PS1='[\u@\h \W]\$ '
