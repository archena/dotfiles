shopt -s histappend
export HISTSIZE=10000
export HISTIGNORE=' *:&:ls:cd ~:cd ..:[bf]g:exit:h:history'
export HISTCONTROL=erasedups
export PROMPT_COMMAND='history -a'
export SCALA_HOME=/usr/share/java
export PATH=$PATH:/usr/local/bin:/home/matt/bin:$SCALA_HOME/bin
