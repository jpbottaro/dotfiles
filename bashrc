# ~/.bashrc: executed by bash(1) for non-login shells.

# If not running interactively, don't do anything
[ -z "$PS1" ] && return

# editor
export EDITOR=vim

# java
export JAVA_HOME=$(/usr/libexec/java_home)

# path
export PATH=~/bin:$PATH

# use CTRL-T to go forward in history search
bind '\C-t:forward-search-history'

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=10000
HISTFILESIZE=20000
# don't put duplicate lines in the history. See bash(1) for more options
# ... or force ignoredups and ignorespace
HISTCONTROL=ignoredups:ignorespace

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# fix some spelling errors in cd
shopt -s cdspell

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "$debian_chroot" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

RED="\[\033[0;31m\]"
YELLOW="\[\033[0;33m\]"
GREEN="\[\033[0;32m\]"
WHITE="\[\033[0;29m\]"

PS1="$RED\u@\h $YELLOW\w$GREEN\$(__git_ps1) $WHITE\$ "

#PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '

alias ls='ls -G'

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# some more ls aliases
alias ll='ls -laF | grep -v "^-" && ls -laF | grep "^-"'
alias la='ls -A'

# enable programmable completion features (you don't need to enable
# this if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if [ -f /etc/bash_completion ] && ! shopt -oq posix; then
    . /etc/bash_completion
fi

if type brew > /dev/null 2>&1; then
    if [ -f $(brew --prefix)/etc/bash_completion ]; then
      . $(brew --prefix)/etc/bash_completion
    fi
fi

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

history() {
    _bash_history_sync
    builtin history "$@"
}

_bash_history_sync() {
    builtin history -a         #1
    HISTFILESIZE=$HISTFILESIZE #2
}

PROMPT_COMMAND=_bash_history_sync

# set dual monitors
dual () {
    xrandr --output HDMI1 --primary --auto --right-of eDP1 --output eDP1 --auto
}

# set single monitor
single () {
    xrandr --output eDP1 --primary --auto --output HDMI1 --off
}
