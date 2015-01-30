#!/bin/bash

export TERM=xterm-256color
export CLICOLORS='Yes'
export LESS='-R'
export EDITOR=vim
export VISUAL=vim
export GIT_EDITOR=vim
export CLICOLOR='Yes'
unset LS_COLORS  # By default these get messed up for some reason

# Banner!
fortune hitchhiker | cowsay -f dragon-and-cow

ulimit -c unlimited  # Unlimited core files
stty -ixon

source ~/.profile.clx

if [ -x /bin/zsh ]; then
    exec /bin/zsh -l
else
    export PS1="[\u@\h] - [\w]\n[\$?] \$(if [[ \$? == 0 ]]; then echo \"\[\033[01;32m\]\342\234\223\"; else echo \"\[\033[01;31m\]\342\234\227\"; fi)\[\033[00m\] \$ "
    complete -cf sudo  # Enable autocompletion in sudo
fi
