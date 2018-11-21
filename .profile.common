#!/bin/bash

export TERM=xterm-256color
export CLICOLORS='Yes'
export LESS='-R'
export EDITOR=vim
export VISUAL=vim
export GIT_EDITOR=vim
export CLICOLOR='Yes'
unset LS_COLORS  # By default these get messed up for some reason

stty -ixon

# Banner!
if (which fortune &> /dev/null && which cowsay &> /dev/null); then
    fortune hitchhiker | cowsay -f dragon-and-cow
fi

# https://stackoverflow.com/a/29394504
function ver {
    printf "%03d%03d%03d%03d" $(echo "$1" | tr '.' ' ')
}

alias l='ls'
alias la='ls -a'
alias ll='ls -l'
alias ll='ls -lh'
alias lla='ls -lah'
alias info="info --vi-keys"
alias grep='grep --color=always'
alias egrep='egrep --color=always'
alias tmux='tmux -2'
alias ta='tmux attach-session -t'
alias tl='tmux list-sessions'
alias tn='tmux new-session -s'
alias irssi='TERM=screen-256color irssi'
alias cgdb='TERM=screen-256color cgdb'
alias htop='TERM=screen-256color htop'
alias info='TERM=screen-256color info --vi-keys'
alias g='git'
alias v='vim'
alias e='emacsclient -c'
alias lag='ag --pager=less'
alias alert='echo -ne "\a"';

complete -cf sudo       # Enable autocompletion in sudo

# Set the gaudy prompt.

# ^O puts terminal back into default text mode for every prompt,
# preventing random glyphs.
resetterm="\[\017\]"

# \[ and \] prevent the shell from counting these characters
# against the line length.
usercolor="\[\033[35m\]"   # purple
hostcolor="\[\033[4"$(( ( $(hostname|cksum|cut -d\  -f1|cut -d\  -f1) / 7 ) % 7))"m\033[1;3"$(( $(hostname|cksum|cut -d\  -f1|cut -d\    -f1) % 7))"m\]"   # bold-random-color on random-color
timecolor="\[\033[35m\]"   # purple
normcolor="\[\033[0m\]"    # prompt bg
resetcolor="\[\033[0m\]"   # terminal default

# To put in xterm titlebar, \007 ends string but \r backs over it
# for non-parsing terminals.  Have to change this for non-parsing
# terminals so that they won't beep at me for every line.
title="\[\033]0;b:\w\007\r\]"

export PS1="$resetterm$hostcolor[\u@\h]$normcolor - [\w]\$STATUS_EXT\n[\$?] \$(if [ \$? == 0 ]; then echo \"\[\033[01;32m\]\342\234\223\"; else echo \"\[\033[01;31m\]\342\234\227\"; fi)\[\033[00m\] \$ "
