__git_shortcut () { 
    alias $1="git $2 $3" 
    __git_complete $1 _git_$2 &>/dev/null
} 

__git_shortcut  ga    add 
__git_shortcut  gs    status -sb
__git_shortcut  gb    branch 
__git_shortcut  gp    push 
__git_shortcut  gpu   pull 
__git_shortcut  gba   branch -a 
__git_shortcut  gco   checkout 
__git_shortcut  gc    commit -v 
__git_shortcut  gca   commit '-a -v' 
__git_shortcut  gd    diff 
__git_shortcut  gdl   diff @{1}..
__git_shortcut  gdc   diff --cached 
__git_shortcut  gds   diff --stat 
__git_shortcut  gf    fetch 
__git_shortcut  gl    log '--no-merges --stat' 
__git_shortcut  gls   log '--graph --decorate --oneline'

alias up='su -c "apt-get update && apt-get dist-upgrade -y"'
alias shut='su -c "shutdown -h now"'
alias reb='su -c "shutdown -r now"'
alias att='tmux attach'
