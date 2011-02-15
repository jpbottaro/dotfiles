alias my_complete='complete -o default -o nospace -F'

# git aliases
alias g='git'
alias gs='git status'
alias ga='git add'
alias gc='git commit'
alias gb='git branch'
alias gco='git checkout'

# git aliases completion
source /etc/bash_completion.d/git
my_complete _git          g
my_complete _git_status   gs
my_complete _git_add      ga
my_complete _git_commit   gc
my_complete _git_branch   gb
my_complete _git_checkout gco

unalias my_complete
