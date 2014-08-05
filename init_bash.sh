if [ "${INSIDE_EMACS/*tramp*/tramp}" != "tramp" ]
then
    export EDITOR=emacsclient
    export VISUAL=emacsclient
    export GIT_EDITOR=emacsclient
    export GIT_PAGER=''
else
    echo
    PS1="[\u@\h \W]\$ "
    if [ -z ${LS_COLORS:+null} ] && [ -e /etc/DIR_COLORS ]
    then
	sed -e "0,/^TERM .\+/ s//TERM $TERM/" /etc/DIR_COLORS > $HOME/.dir_colors.$TERM 
	. $HOME/.bashrc
    fi
    
    EDITOR_PATH=$HOME/.remote_edit_starter
    # create_remote_editor $EDITOR_PATH
    chmod +x $EDITOR_PATH
    export EDITOR=$EDITOR_PATH
    export VISUAL=$EDITOR_PATH
fi
