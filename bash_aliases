__git_shortcut () {
    alias $1="git $2 $3"
    __git_complete $1 _git_$2 &>/dev/null
}

__git_shortcut  ga    add
__git_shortcut  gp    push
__git_shortcut  gpu   pull
__git_shortcut  gco   checkout
__git_shortcut  gc    commit -v
__git_shortcut  gd    diff
__git_shortcut  gr    rm

alias gs='git status -sb'
alias gca='git commit -a -v'
alias gdl='git diff @{1}..'
alias gdc='git diff --cached'
alias gl='git log --no-merges --stat'
alias gls='git log --graph --decorate --oneline'
alias gr='git rm'

alias shut='su -c "shutdown -h now"'
alias reb='su -c "shutdown -r now"'
alias att='tmux attach'
