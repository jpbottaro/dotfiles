alias my_complete='complete -o default -o nospace -F'

# git aliases
alias g='git'
alias ga='git add'
alias gb='git blame'
alias gbr='git branch'
alias gc='git commit'
alias gco='git checkout'
alias gd='git diff'
alias gl='git log'
alias gp='git push'
alias gs='git status'

# git aliases completion
source /etc/bash_completion.d/git
my_complete _git          g
my_complete _git_add      ga
#my_complete _git_blame    gb
my_complete _git_branch   gbr
my_complete _git_commit   gc
my_complete _git_checkout gco
my_complete _git_diff     gd
my_complete _git_log      gl
my_complete _git_push     gp
my_complete _git_status   gs

unalias my_complete
