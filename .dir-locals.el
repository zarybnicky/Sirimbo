;;; Directory Local Variables            -*- no-byte-compile: t -*-
;;; For more information see (info "(emacs) Directory Variables")

((typescript-mode . ((lsp-eslint-working-directories . ["frontend/" "backend/"])
                     (eval . (let
                                 ((project-directory
                                   (car
                                    (dir-locals-find-file default-directory))))
                               (setq lsp-clients-typescript-server-args
                                     `("--stdio"))))))
 (typescript-tsx-mode . ((lsp-eslint-working-directories . ["frontend/" "backend/"])
                         (eval . (let
                                     ((project-directory
                                       (car
                                        (dir-locals-find-file default-directory))))
                                   (setq lsp-clients-typescript-server-args
                                         `("--stdio")))))))
