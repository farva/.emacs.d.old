(my:install-package-if-needed 'use-package)
(require 'use-package)
(defmacro my:use-package-ensure (pkg &rest forms)
  `(use-package ,pkg :ensure t
     ,@(when forms
         `(:config
           (progn
             ,@forms)))))

(provide 'init-use-package)
