# git aliases
alias ga='git add'
alias gb='git blame'
alias gbr='git branch -a'
alias gc='git commit -v'
alias gco='git checkout'
alias gd='git diff'
alias gl='git log'
alias gp='git push'
alias gpu='git pull'
alias gs='git status'
alias gso='git show'
alias gm='git merge'

# git aliases completion
source /etc/bash_completion.d/git
alias my_complete='complete -o default -o nospace -F'
my_complete _git          g
my_complete _git_add      ga
my_complete _git_branch   gbr
my_complete _git_commit   gc
my_complete _git_checkout gco
my_complete _git_diff     gd
my_complete _git_log      gl
my_complete _git_push     gp
my_complete _git_pull     gpu
my_complete _git_show     gso
my_complete _git_merge    gm
unalias my_complete

alias up='su -c "apt-get update && apt-get upgrade"'
alias shut='su -c "shutdown -h now"'
alias reb='su -c "shutdown -r now"'
alias att='tmux attach'
