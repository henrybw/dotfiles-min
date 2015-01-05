#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'
PS1='[\u@\h \W]\$ '

#------------------------------------------------------------------------------

export PATH=$PATH:~/bin

# Gotta have my vim...
export EDITOR=vim
export VISUAL=vim
export GIT_EDITOR=vim
alias info="info --vi-keys"

complete -cf sudo       # Enable autocompletion in sudo

export TERM=xterm-256color
export CLICOLORS='Yes'

