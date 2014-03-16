shopt -s histappend
export HISTSIZE=10000
export HISTIGNORE=' *:&:ls:cd ~:cd ..:[bf]g:exit:h:history:mu find *'
export HISTCONTROL=erasedups
export PROMPT_COMMAND='history -a'
#export GIT_PROXY_COMMAND=~/bin/proxy-wrapper.sh 
export SCALA_HOME=/usr/share/java
export PATH=$PATH:/usr/local/bin:/usr/local/sbin:/home/matt/bin:$SCALA_HOME/bin

PATH=$PATH:$HOME/.rvm/bin # Add RVM to PATH for scripting

