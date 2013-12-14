function create_remote_editor {
    echo "echo \"Go ahead and \\\`M-x attach-current-remote-editing', m'kay?\"" > $1

    echo 'echo -n "-<0=0>-${*}-<0=0>-"' >> $1
    echo 'read' >> $1

    echo 'exit 0' >> $1

    chmod +x $1
}

if [ "${INSIDE_EMACS/*tramp*/tramp}" != "tramp" ]
then
    export EDITOR=emacsclient
    export VISUAL=emacsclient
else
    echo
    PS1="[\u@\h \W]\$ "
    if [ -z ${LS_COLORS:+null} ] && [ -e /etc/DIR_COLORS ]
    then
	sed -e "0,/^TERM .\+/ s//TERM $TERM/" /etc/DIR_COLORS > $HOME/.dir_colors.$TERM 
	. $HOME/.bashrc
    fi
    
    EDITOR_PATH=$HOME/remote_edit_starter
    create_remote_editor $EDITOR_PATH
    export EDITOR=$EDITOR_PATH
    export VISUAL=$EDITOR_PATH
fi
