#
# I used to use oh-my-zsh, but it was way too bloated and made my shell startup
# times frustratingly slow. So I ripped out all the good bits and threw away the
# rest of oh-my-zsh.
#

# Color config
autoload colors; colors;
export LSCOLORS="Gxfxcxdxbxegedabagacad"
setopt auto_cd
setopt multios
setopt prompt_subst

## Command history configuration
if [ -z "$HISTFILE" ]; then
    HISTFILE=$HOME/.zsh_history
fi
HISTSIZE=10000
SAVEHIST=10000
setopt extended_history
setopt hist_expire_dups_first
setopt hist_ignore_dups # ignore duplication command history list
setopt hist_ignore_space
setopt hist_verify
setopt inc_append_history

#
# Vi-mode extensions
#

# Ensures that $terminfo values are valid and updates editor information when
# the keymap changes.
function zle-keymap-select zle-line-init zle-line-finish {
  local STATUS=$?

  # The terminal must be in application mode when ZLE is active for $terminfo
  # values to be valid.
  if (( ${+terminfo[smkx]} )); then
    printf '%s' ${terminfo[smkx]}
  fi
  if (( ${+terminfo[rmkx]} )); then
    printf '%s' ${terminfo[rmkx]}
  fi

  # Workaround for older versions of zsh to preserve status code after redraw
  (return $STATUS)

  zle reset-prompt
  zle -R
}

zle -N zle-line-init
zle -N zle-line-finish
zle -N zle-keymap-select
zle -N edit-command-line

bindkey -v

# allow v to edit the command line (standard behaviour)
autoload -Uz edit-command-line
bindkey -M vicmd 'v' edit-command-line

function vi_mode_prompt_info() {
  echo "${${KEYMAP/vicmd/$MODE_INDICATOR}/(main|viins)/}"
}

#
# Prompt config
#

local return_code="%(?.%{$fg_bold[green]%}✔ %{$fg[green]%}0 %{$reset_color%}.%{$fg_bold[red]%}✘ %{$fg[red]%}%? %{$reset_color%})"

PS1='%{${fg[cyan]}%}[%n@%m] %{${fg[green]}%}[%3~]
${return_code}%{${fg_bold[blue]}%}$%{${reset_color}%} '

RPS1='$(vi_mode_prompt_info)'
MODE_INDICATOR="%{$fg_bold[magenta]%}<%{$reset_color%}%{$fg[magenta]%}<<%{$reset_color%}"


autoload -U compinit
compinit -i

# Compile zcompdump if necessary
if [ ~/.zcompdump -nt ~/.zcompdump.zwc -o ! -e ~/.zcompdump.zwc ]; then
  zcompile ~/.zcompdump
fi

#-------------------------------------------------------------------------------

alias ls='ls --color'
alias la='ls -a'
alias lla='ls -la'
alias info="info --vi-keys"
alias grep='grep --color=always'
alias egrep='egrep --color=always'
alias tmux='tmux -2'
alias irssi='TERM=screen-256color irssi'
alias cgdb='TERM=screen-256color cgdb'
alias g='git'
alias lag='ag --pager=less'
alias alert='echo -ne "\a"';

bindkey '^R' history-incremental-search-backward
bindkey '^S' history-incremental-search-forward
bindkey '^[[2~' overwrite-mode              # insert key overwrite mode
bindkey '^[[3~' delete-char                 # delete key fix
if [ -n "$TMUX" ]; then
    bindkey '^[[1~' beginning-of-line       # home key
    bindkey '^[[4~' end-of-line             # end key
else
    bindkey '^[[H' beginning-of-line        # home key
    bindkey '^[[F' end-of-line              # end key
fi

export KEYTIMEOUT=1

setopt autocd extendedglob notify completeinword
unsetopt sharehistory

zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}'
zstyle ':completion:*:killall:*' command 'ps -u $USER -o cmd'
zstyle ':completion:*' list-colors "${(@s.:.)LS_COLORS}"
zle -A .self-insert self-insert

autoload select-word-style
select-word-style shell

stty stop undef
stty start undef
