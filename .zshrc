#
# I used to use oh-my-zsh, but it was way too bloated and made my shell startup
# times frustratingly slow. So I ripped out all the good bits and threw away the
# rest of oh-my-zsh.
#

#
# Color config
#
autoload colors; colors;
export LSCOLORS="Gxfxcxdxbxegedabagacad"
export LS_COLORS=""
setopt auto_cd
setopt multios
setopt prompt_subst

#
# Command history configuration
#
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
# Keybinding config
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

# http://zsh.sourceforge.net/Doc/Release/Zsh-Line-Editor.html
# http://zsh.sourceforge.net/Doc/Release/Zsh-Line-Editor.html#Zle-Builtins
# http://zsh.sourceforge.net/Doc/Release/Zsh-Line-Editor.html#Standard-Widgets

# Make sure that the terminal is in application mode when zle is active, since
# only then values from $terminfo are valid
if (( ${+terminfo[smkx]} )) && (( ${+terminfo[rmkx]} )); then
  function zle-line-init() {
    echoti smkx
  }
  function zle-line-finish() {
    echoti rmkx
  }
  zle -N zle-line-init
  zle -N zle-line-finish
fi

bindkey '\ew' kill-region                             # [Esc-w] - Kill from the cursor to the mark
bindkey -s '\el' 'ls\n'                               # [Esc-l] - run command: ls
bindkey '^r' history-incremental-search-backward      # [Ctrl-r] - Search backward incrementally for a specified string. The string may begin with ^ to anchor the search to the beginning of the line.
if [[ "${terminfo[kpp]}" != "" ]]; then
  bindkey "${terminfo[kpp]}" up-line-or-history       # [PageUp] - Up a line of history
fi
if [[ "${terminfo[knp]}" != "" ]]; then
  bindkey "${terminfo[knp]}" down-line-or-history     # [PageDown] - Down a line of history
fi

if [[ "${terminfo[kcuu1]}" != "" ]]; then
  bindkey "${terminfo[kcuu1]}" up-line-or-search      # start typing + [Up-Arrow] - fuzzy find history forward
fi
if [[ "${terminfo[kcud1]}" != "" ]]; then
  bindkey "${terminfo[kcud1]}" down-line-or-search    # start typing + [Down-Arrow] - fuzzy find history backward
fi

if [[ "${terminfo[khome]}" != "" ]]; then
  bindkey "${terminfo[khome]}" beginning-of-line      # [Home] - Go to beginning of line
fi
if [[ "${terminfo[kend]}" != "" ]]; then
  bindkey "${terminfo[kend]}"  end-of-line            # [End] - Go to end of line
fi

bindkey ' ' magic-space                               # [Space] - do history expansion

bindkey '^[[1;5C' forward-word                        # [Ctrl-RightArrow] - move forward one word
bindkey '^[[1;5D' backward-word                       # [Ctrl-LeftArrow] - move backward one word

if [[ "${terminfo[kcbt]}" != "" ]]; then
  bindkey "${terminfo[kcbt]}" reverse-menu-complete   # [Shift-Tab] - move through the completion menu backwards
fi

bindkey '^?' backward-delete-char                     # [Backspace] - delete backward
if [[ "${terminfo[kdch1]}" != "" ]]; then
  bindkey "${terminfo[kdch1]}" delete-char            # [Delete] - delete forward
else
  bindkey "^[[3~" delete-char
  bindkey "^[3;5~" delete-char
  bindkey "\e[3~" delete-char
fi

# Edit the current command line in $EDITOR
autoload -U edit-command-line
zle -N edit-command-line
bindkey '\C-x\C-e' edit-command-line

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
