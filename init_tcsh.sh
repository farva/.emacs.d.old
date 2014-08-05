if ( "$INSIDE_EMACS" !~ "*tramp*" ) then
setenv EDITOR emacsclient &&\
setenv VISUAL emacsclient &&\
setenv GIT_EDITOR emacsclient &&\
setenv GIT_PAGER ''
endif
