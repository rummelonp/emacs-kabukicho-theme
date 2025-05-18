(advice-add #'x-apply-session-resources :override #'ignore)
(setq make-backup-files nil
      auto-save-default nil
      visible-bell t
      warning-minimum-level :debug
      warning-minimum-log-level :debug)
(toggle-debug-on-error)

(load "kabukicho-theme")
(load-theme 'kabukicho t)
