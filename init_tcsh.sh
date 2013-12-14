if ( "$INSIDE_EMACS" !~ "*tramp*" ) then
setenv EDITOR emacsclient &&\
setenv VISUAL emacsclient
endif
