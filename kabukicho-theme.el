;;; kabukicho-theme.el --- Kabukicho Theme -*- lexical-binding: t; -*-

;; Copyright 2015-present Dracula Theme
;; Copyright 2022-present Kazuya Takeshima <rummelonp@gmail.com>
;;
;; Code licensed under the MIT license

;; Maintainer: Kazuya Takeshima <rummelonp@gmail.com>
;; Author: film42
;; Version: 1.2.0
;; Package-Requires: ((emacs "25.1"))
;; URL: https://github.com/rummelonp/emacs-kabukicho-theme

;;; Commentary:

;; A dark theme for Emacs
;;
;; The code is based on dracula.
;; The theme is based on kabukicho-vscode, merged with
;; synthwave-x-fluoromachine.
;;
;; See url
;; https://github.com/dracula/emacs
;; https://github.com/victoriadrake/kabukicho-vscode
;; https://github.com/webrender/synthwave-x-fluoromachine

;;; Code:
(deftheme kabukicho)


;;;; Configuration options:

(defgroup kabukicho nil
  "Kabukicho theme options.

The theme has to be reloaded after changing anything in this group."
  :group 'faces)

(defcustom kabukicho-enlarge-headings t
  "Use different font sizes for some headings and titles."
  :type 'boolean
  :group 'kabukicho)

(defcustom kabukicho-height-title-1 1.3
  "Font size 130%."
  :type 'number
  :group 'kabukicho)

(defcustom kabukicho-height-title-2 1.1
  "Font size 110%."
  :type 'number
  :group 'kabukicho)

(defcustom kabukicho-height-title-3 1.0
  "Font size 100%."
  :type 'number
  :group 'kabukicho)

(defcustom kabukicho-height-doc-title 1.44
  "Font size 144%."
  :type 'number
  :group 'kabukicho)

(defcustom kabukicho-alternate-mode-line-and-minibuffer nil
  "Use less bold and pink in the minibuffer."
  :type 'boolean
  :group 'kabukicho)

(defvar kabukicho-use-24-bit-colors-on-256-colors-terms nil
  "Use true colors even on terminals announcing less capabilities.

Beware the use of this variable.  Using it may lead to unwanted
behavior, the most common one being an ugly blue background on
terminals, which don't understand 24 bit colors.  To avoid this
blue background, when using this variable, one can try to add the
following lines in their config file after having load the
Kabukicho theme:

    (unless (display-graphic-p)
      (set-face-background \\='default \"black\" nil))

There is a lot of discussion behind the 256 colors theme (see URL
`https://github.com/kabukicho/emacs/pull/57').  Please take time to
read it before opening a new issue about your will.")


;;;; Theme definition:

;; Assigment form: VARIABLE COLOR [256-COLOR [TTY-COLOR]]
(let ((colors '(;; Upstream theme color
                (kabukicho-bg      "#1f1529" "unspecified-bg" "unspecified-bg")
                (kabukicho-fg      "#f8f8f2" "#ffffff" "brightwhite")
                (kabukicho-current "#44475a" "#303030" "brightblack")
                (kabukicho-comment "#6071cc" "#5f5faf" "blue")
                (kabukicho-cyan    "#61e2ff" "#5fd7ff" "brightcyan")
                (kabukicho-green   "#54e484" "#5fd787" "green")
                (kabukicho-orange  "#e0b401" "#ffd700" "brightred")
                (kabukicho-pink    "#f92aad" "#ff00af" "magenta")
                (kabukicho-purple  "#b141f1" "#af5fff" "brightmagenta")
                (kabukicho-red     "#ff5555" "#ff5f5f" "red")
                (kabukicho-yellow  "#f4ff81" "#ffff87" "yellow")
                ;; Other colors
                (bg2             "#373844" "#121212" "brightblack")
                (bg3             "#565761" "#444444" "brightblack")
                (fg2             "#e2e2dc" "#e4e4e4" "brightwhite")
                (fg3             "#ccccc7" "#c6c6c6" "white")
                (fg4             "#b6b6b2" "#b2b2b2" "white")
                ;;(dark-red        "#880000" "#870000" "red")
                ;;(dark-green      "#037a22" "#00af00" "green")
                (dark-blue       "#495495" "#5f5faf" "brightblue")))
      (faces '(;; default / basic faces
               (cursor :background ,fg3)
               (default :background ,kabukicho-bg :foreground ,kabukicho-fg)
               (default-italic :slant italic)
               (error :foreground ,kabukicho-red)
               (ffap :foreground ,fg4)
               (fringe :background ,kabukicho-bg :foreground ,fg4)
               (header-line :inherit 'mode-line)
               (highlight :foreground ,fg3 :background ,kabukicho-current)
               (hl-line :background ,bg2 :extend t)
               (info-quoted-name :foreground ,kabukicho-orange)
               (info-string :foreground ,kabukicho-yellow)
               (lazy-highlight :foreground ,fg2 :background ,bg2)
               (link :foreground ,kabukicho-cyan :underline t)
               (linum :slant italic :foreground ,bg3 :background ,kabukicho-bg)
               (line-number :slant italic :foreground ,bg3 :background ,kabukicho-bg)
               (match :background ,kabukicho-yellow :foreground ,kabukicho-bg)
               (menu :background ,kabukicho-current :inverse-video nil
                     ,@(if kabukicho-alternate-mode-line-and-minibuffer
                           (list :foreground fg3)
                         (list :foreground kabukicho-fg)))
               (minibuffer-prompt
                ,@(if kabukicho-alternate-mode-line-and-minibuffer
                      (list :weight 'normal :foreground kabukicho-fg)
                    (list :weight 'bold :foreground kabukicho-pink)))
               (mode-line :background ,kabukicho-current
                          :box ,kabukicho-current :inverse-video nil
                          ,@(if kabukicho-alternate-mode-line-and-minibuffer
                                (list :foreground fg3)
                              (list :foreground kabukicho-fg)))
               (mode-line-inactive
                :background ,kabukicho-bg :inverse-video nil
                ,@(if kabukicho-alternate-mode-line-and-minibuffer
                      (list :foreground kabukicho-comment :box kabukicho-bg)
                    (list :foreground fg4 :box bg2)))
               (read-multiple-choice-face :inherit completions-first-difference)
               (region :background ,kabukicho-current :extend nil)
               (shadow :foreground ,kabukicho-comment)
               (success :foreground ,kabukicho-green)
               (tooltip :foreground ,kabukicho-fg :background ,kabukicho-current)
               (trailing-whitespace :background ,kabukicho-orange)
               (vertical-border :foreground ,bg2)
               (warning :foreground ,kabukicho-orange)
               ;; syntax / font-lock
               (font-lock-builtin-face :foreground ,kabukicho-cyan :slant normal)
               (font-lock-comment-face :inherit shadow)
               (font-lock-comment-delimiter-face :inherit shadow)
               (font-lock-constant-face :foreground ,kabukicho-purple)
               (font-lock-doc-face :foreground ,kabukicho-comment)
               (font-lock-function-name-face :foreground ,kabukicho-orange :weight bold)
               (font-lock-keyword-face :foreground ,kabukicho-pink :weight bold)
               (font-lock-negation-char-face :foreground ,kabukicho-cyan)
               (font-lock-number-face :foreground ,kabukicho-purple)
               (font-lock-operator-face :foreground ,kabukicho-pink)
               (font-lock-preprocessor-face :foreground ,kabukicho-orange)
               (font-lock-reference-face :inherit font-lock-constant-face) ;; obsolete
               (font-lock-regexp-grouping-backslash :foreground ,kabukicho-cyan)
               (font-lock-regexp-grouping-construct :foreground ,kabukicho-purple)
               (font-lock-string-face :foreground ,kabukicho-yellow)
               (font-lock-type-face :inherit font-lock-builtin-face)
               (font-lock-variable-name-face :foreground ,kabukicho-green :weight bold)
               (font-lock-warning-face :inherit warning :background ,bg2)
               ;; auto-complete
               (ac-completion-face :underline t :foreground ,kabukicho-pink)
               ;; ansi-color
               (ansi-color-black :foreground ,kabukicho-bg :background ,kabukicho-bg)
               (ansi-color-bright-black :foreground "black" :background "black")
               (ansi-color-blue :foreground ,kabukicho-purple :background ,kabukicho-purple)
               (ansi-color-bright-blue :foreground ,kabukicho-purple
                                       :background ,kabukicho-purple
                                       :weight bold)
               (ansi-color-cyan :foreground ,kabukicho-cyan :background ,kabukicho-cyan)
               (ansi-color-bright-cyan :foreground ,kabukicho-cyan
                                       :background ,kabukicho-cyan
                                       :weight bold)
               (ansi-color-green :foreground ,kabukicho-green :background ,kabukicho-green)
               (ansi-color-bright-green :foreground ,kabukicho-green
                                        :background ,kabukicho-green
                                        :weight bold)
               (ansi-color-magenta :foreground ,kabukicho-pink :background ,kabukicho-pink)
               (ansi-color-bright-magenta :foreground ,kabukicho-pink
                                          :background ,kabukicho-pink
                                          :weight bold)
               (ansi-color-red :foreground ,kabukicho-red :background ,kabukicho-red)
               (ansi-color-bright-red :foreground ,kabukicho-red
                                      :background ,kabukicho-red
                                      :weight bold)
               (ansi-color-white :foreground ,kabukicho-fg :background ,kabukicho-fg)
               (ansi-color-bright-white :foreground "white" :background "white")
               (ansi-color-yellow :foreground ,kabukicho-yellow :background ,kabukicho-yellow)
               (ansi-color-bright-yellow :foreground ,kabukicho-yellow
                                         :background ,kabukicho-yellow
                                         :weight bold)
               ;; bookmarks
               (bookmark-face :foreground ,kabukicho-pink)
               ;; company
               (company-echo-common :foreground ,kabukicho-bg :background ,kabukicho-fg)
               (company-preview :background ,kabukicho-current :foreground ,dark-blue)
               (company-preview-common :inherit company-preview
                                       :foreground ,kabukicho-pink)
               (company-preview-search :inherit company-preview
                                       :foreground ,kabukicho-green)
               (company-scrollbar-bg :background ,kabukicho-comment)
               (company-scrollbar-fg :foreground ,dark-blue)
               (company-tooltip :inherit tooltip)
               (company-tooltip-search :foreground ,kabukicho-green
                                       :underline t)
               (company-tooltip-search-selection :background ,kabukicho-green
                                                 :foreground ,kabukicho-bg)
               (company-tooltip-selection :inherit match)
               (company-tooltip-mouse :background ,kabukicho-bg)
               (company-tooltip-common :foreground ,kabukicho-pink :weight bold)
               ;;(company-tooltip-common-selection :inherit company-tooltip-common)
               (company-tooltip-annotation :foreground ,kabukicho-cyan)
               ;;(company-tooltip-annotation-selection :inherit company-tooltip-annotation)
               ;; completions (minibuffer.el)
               (completions-annotations :inherit font-lock-comment-face)
               (completions-common-part :foreground ,kabukicho-green)
               (completions-first-difference :foreground ,kabukicho-pink :weight bold)
               ;; diff
               ;;(diff-added :background ,dark-green :foreground ,kabukicho-fg :extend t)
               ;;(diff-removed :background ,dark-red :foreground ,kabukicho-fg :extend t)
               ;;(diff-refine-added :background ,kabukicho-green
               ;;                   :foreground ,kabukicho-bg)
               ;;(diff-refine-removed :background ,kabukicho-red
               ;;                     :foreground ,kabukicho-fg)
               ;;(diff-indicator-added :foreground ,kabukicho-green)
               ;;(diff-indicator-removed :foreground ,kabukicho-red)
               ;;(diff-indicator-changed :foreground ,kabukicho-orange)
               ;;(diff-error :foreground ,kabukicho-red, :background ,kabukicho-bg
               ;;            :weight bold)
               ;; diff-hl
               (diff-hl-change :foreground ,kabukicho-orange :background ,kabukicho-orange)
               (diff-hl-delete :foreground ,kabukicho-red :background ,kabukicho-red)
               (diff-hl-insert :foreground ,kabukicho-green :background ,kabukicho-green)
               ;; dired
               (dired-directory :foreground ,kabukicho-green :weight normal)
               (dired-flagged :foreground ,kabukicho-pink)
               (dired-header :foreground ,fg3 :background ,kabukicho-bg)
               (dired-ignored :inherit shadow)
               (dired-mark :foreground ,kabukicho-fg :weight bold)
               (dired-marked :foreground ,kabukicho-orange :weight bold)
               (dired-perm-write :foreground ,fg3 :underline t)
               (dired-symlink :foreground ,kabukicho-yellow :weight normal :slant italic)
               (dired-warning :foreground ,kabukicho-orange :underline t)
               (diredp-compressed-file-name :foreground ,fg3)
               (diredp-compressed-file-suffix :foreground ,fg4)
               (diredp-date-time :foreground ,kabukicho-fg)
               (diredp-deletion-file-name :foreground ,kabukicho-pink :background ,kabukicho-current)
               (diredp-deletion :foreground ,kabukicho-pink :weight bold)
               (diredp-dir-heading :foreground ,fg2 :background ,bg3)
               (diredp-dir-name :inherit dired-directory)
               (diredp-dir-priv :inherit dired-directory)
               (diredp-executable-tag :foreground ,kabukicho-orange)
               (diredp-file-name :foreground ,kabukicho-fg)
               (diredp-file-suffix :foreground ,fg4)
               (diredp-flag-mark-line :foreground ,fg2 :slant italic :background ,kabukicho-current)
               (diredp-flag-mark :foreground ,fg2 :weight bold :background ,kabukicho-current)
               (diredp-ignored-file-name :foreground ,kabukicho-fg)
               (diredp-mode-line-flagged :foreground ,kabukicho-orange)
               (diredp-mode-line-marked :foreground ,kabukicho-orange)
               (diredp-no-priv :foreground ,kabukicho-fg)
               (diredp-number :foreground ,kabukicho-cyan)
               (diredp-other-priv :foreground ,kabukicho-orange)
               (diredp-rare-priv :foreground ,kabukicho-orange)
               (diredp-read-priv :foreground ,kabukicho-purple)
               (diredp-write-priv :foreground ,kabukicho-pink)
               (diredp-exec-priv :foreground ,kabukicho-yellow)
               (diredp-symlink :foreground ,kabukicho-orange)
               (diredp-link-priv :foreground ,kabukicho-orange)
               (diredp-autofile-name :foreground ,kabukicho-yellow)
               (diredp-tagged-autofile-name :foreground ,kabukicho-yellow)
               ;; ediff
               ;;(ediff-current-diff-A :background ,dark-red)
               ;;(ediff-fine-diff-A :background ,kabukicho-red :foreground ,kabukicho-fg)
               ;;(ediff-current-diff-B :background ,dark-green)
               ;;(ediff-fine-diff-B :background ,kabukicho-green :foreground ,kabukicho-bg)
               ;;(ediff-current-diff-C :background ,dark-blue)
               ;;(ediff-fine-diff-C :background ,kabukicho-cyan :foreground ,kabukicho-bg)
               ;; eglot
               (eglot-diagnostic-tag-unnecessary-face :inherit warning)
               (eglot-diagnostic-tag-deprecated-face :inherit warning :strike-through t)
               ;; eldoc-box
               (eldoc-box-border :background ,kabukicho-current)
               (eldoc-box-body :background ,kabukicho-current)
               ;; elfeed
               (elfeed-search-date-face :foreground ,kabukicho-comment)
               (elfeed-search-title-face :foreground ,kabukicho-fg)
               (elfeed-search-unread-title-face :foreground ,kabukicho-pink :weight bold)
               (elfeed-search-feed-face :foreground ,kabukicho-fg :weight bold)
               (elfeed-search-tag-face :foreground ,kabukicho-green)
               (elfeed-search-last-update-face :weight bold)
               (elfeed-search-unread-count-face :foreground ,kabukicho-pink)
               (elfeed-search-filter-face :foreground ,kabukicho-green :weight bold)
               ;;(elfeed-log-date-face :inherit font-lock-type-face)
               (elfeed-log-error-level-face :foreground ,kabukicho-red)
               (elfeed-log-warn-level-face :foreground ,kabukicho-orange)
               (elfeed-log-info-level-face :foreground ,kabukicho-cyan)
               (elfeed-log-debug-level-face :foreground ,kabukicho-comment)
               ;; elpher
               (elpher-gemini-heading1 :inherit bold :foreground ,kabukicho-pink
                                       ,@(when kabukicho-enlarge-headings
                                           (list :height kabukicho-height-title-1)))
               (elpher-gemini-heading2 :inherit bold :foreground ,kabukicho-purple
                                       ,@(when kabukicho-enlarge-headings
                                           (list :height kabukicho-height-title-2)))
               (elpher-gemini-heading3 :weight normal :foreground ,kabukicho-green
                                       ,@(when kabukicho-enlarge-headings
                                           (list :height kabukicho-height-title-3)))
               (elpher-gemini-preformatted :inherit fixed-pitch
                                           :foreground ,kabukicho-orange)
               ;; enh-ruby
               (enh-ruby-heredoc-delimiter-face :foreground ,kabukicho-yellow)
               (enh-ruby-op-face :foreground ,kabukicho-pink)
               (enh-ruby-regexp-delimiter-face :foreground ,kabukicho-yellow)
               (enh-ruby-string-delimiter-face :foreground ,kabukicho-yellow)
               ;; flyspell
               (flyspell-duplicate :underline (:style wave :color ,kabukicho-orange))
               (flyspell-incorrect :underline (:style wave :color ,kabukicho-red))
               ;; font-latex (auctex)
               (font-latex-bold-face :foreground ,kabukicho-purple)
               (font-latex-italic-face :foreground ,kabukicho-pink :slant italic)
               (font-latex-match-reference-keywords :foreground ,kabukicho-cyan)
               (font-latex-match-variable-keywords :foreground ,kabukicho-fg)
               (font-latex-math-face :foreground ,kabukicho-orange)
               (font-latex-script-char-face :inherit font-latex-math-face)
               (font-latex-sectioning-0-face :foreground ,kabukicho-pink :weight bold
                                             ,@(when kabukicho-enlarge-headings
                                                 (list :height kabukicho-height-title-1)))
               (font-latex-sectioning-1-face :foreground ,kabukicho-purple :weight bold
                                             ,@(when kabukicho-enlarge-headings
                                                 (list :height kabukicho-height-title-1)))
               (font-latex-sectioning-2-face :foreground ,kabukicho-green :weight bold
                                             ,@(when kabukicho-enlarge-headings
                                                 (list :height kabukicho-height-title-2)))
               (font-latex-sectioning-3-face :foreground ,kabukicho-yellow :weight bold
                                             ,@(when kabukicho-enlarge-headings
                                                 (list :height kabukicho-height-title-3)))
               (font-latex-sectioning-4-face :foreground ,kabukicho-cyan :weight bold)
               (font-latex-sectioning-5-face :foreground ,kabukicho-orange :weight bold)
               (font-latex-sedate-face :foreground ,kabukicho-pink)
               (font-latex-string-face :foreground ,kabukicho-yellow)
               (font-latex-verbatim-face :foreground ,kabukicho-orange)
               (font-latex-warning-face :foreground ,kabukicho-red)
               ;; gemini
               (gemini-heading-face-1 :inherit bold :foreground ,kabukicho-pink
                                      ,@(when kabukicho-enlarge-headings
                                          (list :height kabukicho-height-title-1)))
               (gemini-heading-face-2 :inherit bold :foreground ,kabukicho-purple
                                      ,@(when kabukicho-enlarge-headings
                                          (list :height kabukicho-height-title-2)))
               (gemini-heading-face-3 :weight normal :foreground ,kabukicho-green
                                      ,@(when kabukicho-enlarge-headings
                                          (list :height kabukicho-height-title-3)))
               (gemini-heading-face-rest :weight normal :foreground ,kabukicho-yellow)
               (gemini-quote-face :foreground ,kabukicho-purple)
               ;; go-test
               (go-test--ok-face :inherit success)
               (go-test--error-face :inherit error)
               (go-test--warning-face :inherit warning)
               (go-test--pointer-face :foreground ,kabukicho-pink)
               (go-test--standard-face :foreground ,kabukicho-cyan)
               ;; gnus-group
               (gnus-group-mail-1 :foreground ,kabukicho-pink :weight bold)
               (gnus-group-mail-1-empty :inherit gnus-group-mail-1 :weight normal)
               (gnus-group-mail-2 :foreground ,kabukicho-cyan :weight bold)
               (gnus-group-mail-2-empty :inherit gnus-group-mail-2 :weight normal)
               (gnus-group-mail-3 :foreground ,kabukicho-comment :weight bold)
               (gnus-group-mail-3-empty :inherit gnus-group-mail-3 :weight normal)
               (gnus-group-mail-low :foreground ,kabukicho-current :weight bold)
               (gnus-group-mail-low-empty :inherit gnus-group-mail-low :weight normal)
               (gnus-group-news-1 :foreground ,kabukicho-pink :weight bold)
               (gnus-group-news-1-empty :inherit gnus-group-news-1 :weight normal)
               (gnus-group-news-2 :foreground ,kabukicho-cyan :weight bold)
               (gnus-group-news-2-empty :inherit gnus-group-news-2 :weight normal)
               (gnus-group-news-3 :foreground ,kabukicho-comment :weight bold)
               (gnus-group-news-3-empty :inherit gnus-group-news-3 :weight normal)
               (gnus-group-news-4 :inherit gnus-group-news-low)
               (gnus-group-news-4-empty :inherit gnus-group-news-low-empty)
               (gnus-group-news-5 :inherit gnus-group-news-low)
               (gnus-group-news-5-empty :inherit gnus-group-news-low-empty)
               (gnus-group-news-6 :inherit gnus-group-news-low)
               (gnus-group-news-6-empty :inherit gnus-group-news-low-empty)
               (gnus-group-news-low :foreground ,kabukicho-current :weight bold)
               (gnus-group-news-low-empty :inherit gnus-group-news-low :weight normal)
               (gnus-header-content :foreground ,kabukicho-purple)
               (gnus-header-from :foreground ,kabukicho-fg)
               (gnus-header-name :foreground ,kabukicho-green)
               (gnus-header-subject :foreground ,kabukicho-pink :weight bold)
               (gnus-summary-markup-face :foreground ,kabukicho-cyan)
               (gnus-summary-high-unread :foreground ,kabukicho-pink :weight bold)
               (gnus-summary-high-read :inherit gnus-summary-high-unread :weight normal)
               (gnus-summary-high-ancient :inherit gnus-summary-high-read)
               (gnus-summary-high-ticked :inherit gnus-summary-high-read :underline t)
               (gnus-summary-normal-unread :foreground ,dark-blue :weight bold)
               (gnus-summary-normal-read :foreground ,kabukicho-comment :weight normal)
               (gnus-summary-normal-ancient :inherit gnus-summary-normal-read :weight light)
               (gnus-summary-normal-ticked :foreground ,kabukicho-pink :weight bold)
               (gnus-summary-low-unread :foreground ,kabukicho-comment :weight bold)
               (gnus-summary-low-read :inherit gnus-summary-low-unread :weight normal)
               (gnus-summary-low-ancient :inherit gnus-summary-low-read)
               (gnus-summary-low-ticked :inherit gnus-summary-low-read :underline t)
               (gnus-summary-selected :inverse-video t)
               ;; haskell-mode
               (haskell-operator-face :foreground ,kabukicho-pink)
               (haskell-constructor-face :foreground ,kabukicho-purple)
               ;; helm
               (helm-bookmark-w3m :foreground ,kabukicho-purple)
               (helm-buffer-not-saved :foreground ,kabukicho-purple :background ,kabukicho-bg)
               (helm-buffer-process :foreground ,kabukicho-orange :background ,kabukicho-bg)
               (helm-buffer-saved-out :foreground ,kabukicho-fg :background ,kabukicho-bg)
               (helm-buffer-size :foreground ,kabukicho-fg :background ,kabukicho-bg)
               (helm-candidate-number :foreground ,kabukicho-bg :background ,kabukicho-fg)
               (helm-ff-directory :foreground ,kabukicho-green :background ,kabukicho-bg :weight bold)
               (helm-ff-dotted-directory :foreground ,kabukicho-green :background ,kabukicho-bg :weight normal)
               (helm-ff-executable :foreground ,dark-blue :background ,kabukicho-bg :weight normal)
               (helm-ff-file :foreground ,kabukicho-fg :background ,kabukicho-bg :weight normal)
               (helm-ff-invalid-symlink :foreground ,kabukicho-pink :background ,kabukicho-bg :weight bold)
               (helm-ff-prefix :foreground ,kabukicho-bg :background ,kabukicho-pink :weight normal)
               (helm-ff-symlink :foreground ,kabukicho-pink :background ,kabukicho-bg :weight bold)
               (helm-grep-cmd-line :foreground ,kabukicho-fg :background ,kabukicho-bg)
               (helm-grep-file :foreground ,kabukicho-fg :background ,kabukicho-bg)
               (helm-grep-finish :foreground ,fg2 :background ,kabukicho-bg)
               (helm-grep-lineno :foreground ,kabukicho-fg :background ,kabukicho-bg)
               (helm-grep-match :inherit match)
               (helm-grep-running :foreground ,kabukicho-green :background ,kabukicho-bg)
               (helm-header :foreground ,fg2 :background ,kabukicho-bg :underline nil :box nil)
               (helm-moccur-buffer :foreground ,kabukicho-green :background ,kabukicho-bg)
               (helm-selection :background ,bg2 :underline nil)
               (helm-selection-line :background ,bg2)
               (helm-separator :foreground ,kabukicho-purple :background ,kabukicho-bg)
               (helm-source-go-package-godoc-description :foreground ,kabukicho-yellow)
               (helm-source-header :foreground ,kabukicho-pink :background ,kabukicho-bg :underline nil :weight bold)
               (helm-time-zone-current :foreground ,kabukicho-orange :background ,kabukicho-bg)
               (helm-time-zone-home :foreground ,kabukicho-purple :background ,kabukicho-bg)
               (helm-visible-mark :foreground ,kabukicho-bg :background ,kabukicho-current)
               ;; highlight-indentation minor mode
               (highlight-indentation-face :background ,bg2)
               ;; icicle
               (icicle-whitespace-highlight :background ,kabukicho-fg)
               (icicle-special-candidate :foreground ,fg2)
               (icicle-extra-candidate :foreground ,fg2)
               (icicle-search-main-regexp-others :foreground ,kabukicho-fg)
               (icicle-search-current-input :foreground ,kabukicho-pink)
               (icicle-search-context-level-8 :foreground ,kabukicho-orange)
               (icicle-search-context-level-7 :foreground ,kabukicho-orange)
               (icicle-search-context-level-6 :foreground ,kabukicho-orange)
               (icicle-search-context-level-5 :foreground ,kabukicho-orange)
               (icicle-search-context-level-4 :foreground ,kabukicho-orange)
               (icicle-search-context-level-3 :foreground ,kabukicho-orange)
               (icicle-search-context-level-2 :foreground ,kabukicho-orange)
               (icicle-search-context-level-1 :foreground ,kabukicho-orange)
               (icicle-search-main-regexp-current :foreground ,kabukicho-fg)
               (icicle-saved-candidate :foreground ,kabukicho-fg)
               (icicle-proxy-candidate :foreground ,kabukicho-fg)
               (icicle-mustmatch-completion :foreground ,kabukicho-purple)
               (icicle-multi-command-completion :foreground ,fg2 :background ,bg2)
               (icicle-msg-emphasis :foreground ,kabukicho-green)
               (icicle-mode-line-help :foreground ,fg4)
               (icicle-match-highlight-minibuffer :foreground ,kabukicho-orange)
               (icicle-match-highlight-Completions :foreground ,kabukicho-green)
               (icicle-key-complete-menu-local :foreground ,kabukicho-fg)
               (icicle-key-complete-menu :foreground ,kabukicho-fg)
               (icicle-input-completion-fail-lax :foreground ,kabukicho-pink)
               (icicle-input-completion-fail :foreground ,kabukicho-pink)
               (icicle-historical-candidate-other :foreground ,kabukicho-fg)
               (icicle-historical-candidate :foreground ,kabukicho-fg)
               (icicle-current-candidate-highlight :foreground ,kabukicho-orange :background ,kabukicho-current)
               (icicle-Completions-instruction-2 :foreground ,fg4)
               (icicle-Completions-instruction-1 :foreground ,fg4)
               (icicle-completion :foreground ,kabukicho-fg)
               (icicle-complete-input :foreground ,kabukicho-orange)
               (icicle-common-match-highlight-Completions :foreground ,kabukicho-purple)
               (icicle-candidate-part :foreground ,kabukicho-fg)
               (icicle-annotation :foreground ,fg4)
               ;; icomplete
               (icompletep-determined :foreground ,kabukicho-orange)
               ;; ido
               (ido-first-match
                ,@(if kabukicho-alternate-mode-line-and-minibuffer
                      (list :weight 'normal :foreground kabukicho-green)
                    (list :weight 'bold :foreground kabukicho-pink)))
               (ido-only-match :foreground ,kabukicho-orange)
               (ido-subdir :foreground ,kabukicho-yellow)
               (ido-virtual :foreground ,kabukicho-cyan)
               (ido-incomplete-regexp :inherit font-lock-warning-face)
               (ido-indicator :foreground ,kabukicho-fg :background ,kabukicho-pink)
               ;; ivy
               (ivy-current-match
                ,@(if kabukicho-alternate-mode-line-and-minibuffer
                      (list :weight 'normal :background kabukicho-current :foreground kabukicho-green)
                    (list :weight 'bold :background kabukicho-current :foreground kabukicho-pink)))
               ;; Highlights the background of the match.
               (ivy-minibuffer-match-face-1 :background ,kabukicho-current)
               ;; Highlights the first matched group.
               (ivy-minibuffer-match-face-2 :background ,kabukicho-green
                                            :foreground ,kabukicho-bg)
               ;; Highlights the second matched group.
               (ivy-minibuffer-match-face-3 :background ,kabukicho-yellow
                                            :foreground ,kabukicho-bg)
               ;; Highlights the third matched group.
               (ivy-minibuffer-match-face-4 :background ,kabukicho-pink
                                            :foreground ,kabukicho-bg)
               (ivy-confirm-face :foreground ,kabukicho-orange)
               (ivy-match-required-face :foreground ,kabukicho-red)
               (ivy-subdir :foreground ,kabukicho-yellow)
               (ivy-remote :foreground ,kabukicho-pink)
               (ivy-virtual :foreground ,kabukicho-cyan)
               ;; isearch
               (isearch :inherit match :weight bold)
               (isearch-fail :foreground ,kabukicho-bg :background ,kabukicho-orange)
               ;; jde-java
               (jde-java-font-lock-constant-face :foreground ,kabukicho-cyan)
               (jde-java-font-lock-modifier-face :foreground ,kabukicho-pink)
               (jde-java-font-lock-number-face :foreground ,kabukicho-fg)
               (jde-java-font-lock-package-face :foreground ,kabukicho-fg)
               (jde-java-font-lock-private-face :foreground ,kabukicho-pink)
               (jde-java-font-lock-public-face :foreground ,kabukicho-pink)
               ;; js2-mode
               (js2-external-variable :foreground ,kabukicho-purple)
               (js2-function-param :foreground ,kabukicho-cyan)
               (js2-jsdoc-html-tag-delimiter :foreground ,kabukicho-yellow)
               (js2-jsdoc-html-tag-name :foreground ,dark-blue)
               (js2-jsdoc-value :foreground ,kabukicho-yellow)
               (js2-private-function-call :foreground ,kabukicho-cyan)
               (js2-private-member :foreground ,fg3)
               ;; js3-mode
               (js3-error-face :underline ,kabukicho-orange)
               (js3-external-variable-face :foreground ,kabukicho-fg)
               (js3-function-param-face :foreground ,kabukicho-pink)
               (js3-instance-member-face :foreground ,kabukicho-cyan)
               (js3-jsdoc-tag-face :foreground ,kabukicho-pink)
               (js3-warning-face :underline ,kabukicho-pink)
               ;; lsp
               (lsp-ui-peek-peek :background ,kabukicho-bg)
               (lsp-ui-peek-list :background ,bg2)
               (lsp-ui-peek-filename :foreground ,kabukicho-pink :weight bold)
               (lsp-ui-peek-line-number :foreground ,kabukicho-fg)
               (lsp-ui-peek-highlight :inherit highlight :distant-foreground ,kabukicho-bg)
               (lsp-ui-peek-header :background ,kabukicho-current :foreground ,fg3, :weight bold)
               (lsp-ui-peek-footer :inherit lsp-ui-peek-header)
               (lsp-ui-peek-selection :inherit match)
               (lsp-ui-sideline-symbol :foreground ,fg4 :box (:line-width -1 :color ,fg4) :height 0.99)
               (lsp-ui-sideline-current-symbol :foreground ,kabukicho-fg :weight ultra-bold
                                               :box (:line-width -1 :color ,kabukicho-fg) :height 0.99)
               (lsp-ui-sideline-code-action :foreground ,kabukicho-yellow)
               (lsp-ui-sideline-symbol-info :slant italic :height 0.99)
               (lsp-ui-doc-background :background ,kabukicho-bg)
               (lsp-ui-doc-header :foreground ,kabukicho-bg :background ,kabukicho-cyan)
               ;; magit
               (magit-branch-local :foreground ,kabukicho-cyan)
               (magit-branch-remote :foreground ,kabukicho-green)
               (magit-refname :foreground ,dark-blue)
               (magit-tag :foreground ,kabukicho-orange)
               (magit-hash :foreground ,kabukicho-comment)
               (magit-dimmed :foreground ,kabukicho-comment)
               (magit-section-heading :foreground ,kabukicho-pink :weight bold)
               (magit-section-highlight :background ,kabukicho-current :extend t)
               (magit-diff-context :foreground ,fg3 :extend t)
               (magit-diff-context-highlight :inherit magit-section-highlight
                                             :foreground ,kabukicho-fg)
               (magit-diff-revision-summary :foreground ,kabukicho-orange
                                            :background ,kabukicho-bg
                                            :weight bold)
               (magit-diff-revision-summary-highlight :inherit magit-section-highlight
                                                      :foreground ,kabukicho-orange
                                                      :weight bold)
               (magit-diff-added :background ,kabukicho-bg :foreground ,kabukicho-green)
               (magit-diff-added-highlight :background ,kabukicho-current
                                           :foreground ,kabukicho-green)
               (magit-diff-removed :background ,kabukicho-bg :foreground ,kabukicho-red)
               (magit-diff-removed-highlight :background ,kabukicho-current
                                             :foreground ,kabukicho-red)
               (magit-diff-file-heading :foreground ,kabukicho-fg)
               (magit-diff-file-heading-highlight :inherit magit-section-highlight
                                                  :weight bold)
               (magit-diff-file-heading-selection
                :inherit magit-diff-file-heading-highlight
                :foreground ,kabukicho-pink)
               (magit-diff-hunk-heading :inherit magit-diff-context
                                        :background ,bg3)
               (magit-diff-hunk-heading-highlight
                :inherit magit-diff-context-highlight
                :weight bold)
               (magit-diff-hunk-heading-selection
                :inherit magit-diff-hunk-heading-highlight
                :foreground ,kabukicho-pink)
               (magit-diff-lines-heading
                :inherit magit-diff-hunk-heading-highlight
                :foreground ,kabukicho-pink)
               (magit-diff-lines-boundary :background ,kabukicho-pink)
               (magit-diffstat-added :foreground ,kabukicho-green)
               (magit-diffstat-removed :foreground ,kabukicho-red)
               (magit-log-author :foreground ,kabukicho-comment)
               (magit-log-date :foreground ,kabukicho-comment)
               (magit-log-graph :foreground ,kabukicho-yellow)
               (magit-process-ng :foreground ,kabukicho-orange :weight bold)
               (magit-process-ok :foreground ,kabukicho-green :weight bold)
               (magit-signature-good :foreground ,kabukicho-green)
               (magit-signature-bad :foreground ,kabukicho-red :weight bold)
               (magit-signature-untrusted :foreground ,kabukicho-cyan)
               (magit-signature-expired :foreground ,kabukicho-orange)
               (magit-signature-revoked :foreground ,kabukicho-purple)
               (magit-signature-error :foreground ,kabukicho-cyan)
               (magit-cherry-unmatched :foreground ,kabukicho-cyan)
               (magit-cherry-equivalent :foreground ,kabukicho-purple)
               ;; markdown
               (markdown-blockquote-face :foreground ,kabukicho-yellow
                                         :slant italic)
               (markdown-code-face :foreground ,kabukicho-orange)
               (markdown-footnote-face :foreground ,dark-blue)
               (markdown-header-face :weight normal)
               (markdown-header-face-1
                :inherit bold :foreground ,kabukicho-pink
                ,@(when kabukicho-enlarge-headings
                    (list :height kabukicho-height-title-1)))
               (markdown-header-face-2
                :inherit bold :foreground ,kabukicho-purple
                ,@(when kabukicho-enlarge-headings
                    (list :height kabukicho-height-title-2)))
               (markdown-header-face-3
                :foreground ,kabukicho-green
                ,@(when kabukicho-enlarge-headings
                    (list :height kabukicho-height-title-3)))
               (markdown-header-face-4 :foreground ,kabukicho-yellow)
               (markdown-header-face-5 :foreground ,kabukicho-cyan)
               (markdown-header-face-6 :foreground ,kabukicho-orange)
               (markdown-header-face-7 :foreground ,dark-blue)
               (markdown-header-face-8 :foreground ,kabukicho-fg)
               (markdown-inline-code-face :foreground ,kabukicho-green)
               (markdown-plain-url-face :inherit link)
               (markdown-pre-face :foreground ,kabukicho-orange)
               (markdown-table-face :foreground ,kabukicho-purple)
               (markdown-list-face :foreground ,kabukicho-cyan)
               (markdown-language-keyword-face :foreground ,kabukicho-comment)
               ;; message
               (message-header-to :foreground ,kabukicho-fg :weight bold)
               (message-header-cc :foreground ,kabukicho-fg :bold bold)
               (message-header-subject :foreground ,kabukicho-orange)
               (message-header-newsgroups :foreground ,kabukicho-purple)
               (message-header-other :foreground ,kabukicho-purple)
               (message-header-name :foreground ,kabukicho-green)
               (message-header-xheader :foreground ,kabukicho-cyan)
               (message-separator :foreground ,kabukicho-cyan :slant italic)
               (message-cited-text :foreground ,kabukicho-purple)
               (message-cited-text-1 :foreground ,kabukicho-purple)
               (message-cited-text-2 :foreground ,kabukicho-orange)
               (message-cited-text-3 :foreground ,kabukicho-comment)
               (message-cited-text-4 :foreground ,fg2)
               (message-mml :foreground ,kabukicho-green :weight normal)
               ;; mini-modeline
               (mini-modeline-mode-line :inherit mode-line :height 0.1 :box nil)
               ;; mu4e
               (mu4e-unread-face :foreground ,kabukicho-pink :weight normal)
               (mu4e-view-url-number-face :foreground ,kabukicho-purple)
               (mu4e-highlight-face :background ,kabukicho-bg
                                    :foreground ,kabukicho-yellow
                                    :extend t)
               (mu4e-header-highlight-face :background ,kabukicho-current
                                           :foreground ,kabukicho-fg
                                           :underline nil :weight bold
                                           :extend t)
               (mu4e-header-key-face :inherit message-mml)
               (mu4e-header-marks-face :foreground ,kabukicho-purple)
               (mu4e-cited-1-face :foreground ,kabukicho-purple)
               (mu4e-cited-2-face :foreground ,kabukicho-orange)
               (mu4e-cited-3-face :foreground ,kabukicho-comment)
               (mu4e-cited-4-face :foreground ,fg2)
               (mu4e-cited-5-face :foreground ,fg3)
               ;; neotree
               (neo-banner-face :foreground ,kabukicho-orange :weight bold)
               ;;(neo-button-face :underline nil)
               (neo-dir-link-face :foreground ,kabukicho-purple)
               (neo-expand-btn-face :foreground ,kabukicho-fg)
               (neo-file-link-face :foreground ,kabukicho-cyan)
               (neo-header-face :background ,kabukicho-bg
                                :foreground ,kabukicho-fg
                                :weight bold)
               (neo-root-dir-face :foreground ,kabukicho-purple :weight bold)
               (neo-vc-added-face :foreground ,kabukicho-orange)
               (neo-vc-conflict-face :foreground ,kabukicho-red)
               (neo-vc-default-face :inherit neo-file-link-face)
               (neo-vc-edited-face :foreground ,kabukicho-orange)
               (neo-vc-ignored-face :foreground ,kabukicho-comment)
               (neo-vc-missing-face :foreground ,kabukicho-red)
               (neo-vc-needs-merge-face :foreground ,kabukicho-red
                                        :weight bold)
               ;;(neo-vc-needs-update-face :underline t)
               ;;(neo-vc-removed-face :strike-through t)
               (neo-vc-unlocked-changes-face :foreground ,kabukicho-red)
               ;;(neo-vc-unregistered-face nil)
               (neo-vc-up-to-date-face :foreground ,kabukicho-green)
               (neo-vc-user-face :foreground ,kabukicho-purple)
               ;; org
               (org-agenda-date :foreground ,kabukicho-cyan :underline nil)
               (org-agenda-dimmed-todo-face :foreground ,kabukicho-comment)
               (org-agenda-done :foreground ,kabukicho-green)
               (org-agenda-structure :foreground ,kabukicho-purple)
               (org-block :foreground ,kabukicho-orange)
               (org-code :foreground ,kabukicho-green)
               (org-column :background ,bg3)
               (org-column-title :inherit org-column :weight bold :underline t)
               (org-date :foreground ,kabukicho-cyan :underline t)
               (org-document-info :foreground ,dark-blue)
               (org-document-info-keyword :foreground ,kabukicho-comment)
               (org-document-title :weight bold :foreground ,kabukicho-orange
                                   ,@(when kabukicho-enlarge-headings
                                       (list :height kabukicho-height-doc-title)))
               (org-done :foreground ,kabukicho-green)
               (org-ellipsis :foreground ,kabukicho-comment)
               (org-footnote :foreground ,dark-blue)
               (org-formula :foreground ,kabukicho-pink)
               (org-headline-done :foreground ,kabukicho-comment
                                  :weight normal :strike-through t)
               (org-hide :foreground ,kabukicho-bg :background ,kabukicho-bg)
               (org-level-1 :inherit bold :foreground ,kabukicho-pink
                            ,@(when kabukicho-enlarge-headings
                                (list :height kabukicho-height-title-1)))
               (org-level-2 :inherit bold :foreground ,kabukicho-purple
                            ,@(when kabukicho-enlarge-headings
                                (list :height kabukicho-height-title-2)))
               (org-level-3 :weight normal :foreground ,kabukicho-green
                            ,@(when kabukicho-enlarge-headings
                                (list :height kabukicho-height-title-3)))
               (org-level-4 :weight normal :foreground ,kabukicho-yellow)
               (org-level-5 :weight normal :foreground ,kabukicho-cyan)
               (org-level-6 :weight normal :foreground ,kabukicho-orange)
               (org-level-7 :weight normal :foreground ,dark-blue)
               (org-level-8 :weight normal :foreground ,kabukicho-fg)
               (org-link :foreground ,kabukicho-cyan :underline t)
               (org-priority :foreground ,kabukicho-cyan)
               (org-quote :foreground ,kabukicho-yellow :slant italic)
               (org-scheduled :foreground ,kabukicho-green)
               (org-scheduled-previously :foreground ,kabukicho-yellow)
               (org-scheduled-today :foreground ,kabukicho-green)
               (org-sexp-date :foreground ,fg4)
               (org-special-keyword :foreground ,kabukicho-yellow)
               (org-table :foreground ,kabukicho-purple)
               (org-tag :foreground ,kabukicho-pink :weight bold :background ,bg2)
               (org-todo :foreground ,kabukicho-orange :weight bold :background ,bg2)
               (org-upcoming-deadline :foreground ,kabukicho-yellow)
               (org-verbatim :inherit org-quote)
               (org-warning :weight bold :foreground ,kabukicho-pink)
               ;; outline
               (outline-1 :foreground ,kabukicho-pink)
               (outline-2 :foreground ,kabukicho-purple)
               (outline-3 :foreground ,kabukicho-green)
               (outline-4 :foreground ,kabukicho-yellow)
               (outline-5 :foreground ,kabukicho-cyan)
               (outline-6 :foreground ,kabukicho-orange)
               ;; perspective
               (persp-selected-face :weight bold :foreground ,kabukicho-pink)
               ;; powerline
               (powerline-active1 :background ,kabukicho-bg :foreground ,kabukicho-pink)
               (powerline-active2 :background ,kabukicho-bg :foreground ,kabukicho-pink)
               (powerline-inactive1 :background ,bg2 :foreground ,kabukicho-purple)
               (powerline-inactive2 :background ,bg2 :foreground ,kabukicho-purple)
               (powerline-evil-base-face :foreground ,bg2)
               (powerline-evil-emacs-face :inherit powerline-evil-base-face :background ,kabukicho-yellow)
               (powerline-evil-insert-face :inherit powerline-evil-base-face :background ,kabukicho-cyan)
               (powerline-evil-motion-face :inherit powerline-evil-base-face :background ,kabukicho-purple)
               (powerline-evil-normal-face :inherit powerline-evil-base-face :background ,kabukicho-green)
               (powerline-evil-operator-face :inherit powerline-evil-base-face :background ,kabukicho-pink)
               (powerline-evil-replace-face :inherit powerline-evil-base-face :background ,kabukicho-red)
               (powerline-evil-visual-face :inherit powerline-evil-base-face :background ,kabukicho-orange)
               ;; rainbow-delimiters
               (rainbow-delimiters-depth-1-face :foreground ,kabukicho-fg)
               (rainbow-delimiters-depth-2-face :foreground ,kabukicho-cyan)
               (rainbow-delimiters-depth-3-face :foreground ,kabukicho-purple)
               (rainbow-delimiters-depth-4-face :foreground ,kabukicho-pink)
               (rainbow-delimiters-depth-5-face :foreground ,kabukicho-orange)
               (rainbow-delimiters-depth-6-face :foreground ,kabukicho-green)
               (rainbow-delimiters-depth-7-face :foreground ,kabukicho-yellow)
               (rainbow-delimiters-depth-8-face :foreground ,dark-blue)
               (rainbow-delimiters-unmatched-face :foreground ,kabukicho-orange)
               ;; rpm-spec
               (rpm-spec-dir-face :foreground ,kabukicho-green)
               (rpm-spec-doc-face :foreground ,kabukicho-pink)
               (rpm-spec-ghost-face :foreground ,kabukicho-purple)
               (rpm-spec-macro-face :foreground ,kabukicho-yellow)
               (rpm-spec-obsolete-tag-face :inherit font-lock-warning-face)
               (rpm-spec-package-face :foreground ,kabukicho-purple)
               (rpm-spec-section-face :foreground ,kabukicho-yellow)
               (rpm-spec-tag-face :foreground ,kabukicho-cyan)
               (rpm-spec-var-face :foreground ,kabukicho-orange)
               ;; rst (reStructuredText)
               (rst-level-1 :foreground ,kabukicho-pink :weight bold)
               (rst-level-2 :foreground ,kabukicho-purple :weight bold)
               (rst-level-3 :foreground ,kabukicho-green)
               (rst-level-4 :foreground ,kabukicho-yellow)
               (rst-level-5 :foreground ,kabukicho-cyan)
               (rst-level-6 :foreground ,kabukicho-orange)
               (rst-level-7 :foreground ,dark-blue)
               (rst-level-8 :foreground ,kabukicho-fg)
               ;; selectrum-mode
               (selectrum-current-candidate :weight bold)
               (selectrum-primary-highlight :foreground ,kabukicho-pink)
               (selectrum-secondary-highlight :foreground ,kabukicho-green)
               ;; show-paren
               (show-paren-match-face :background unspecified
                                      :foreground ,kabukicho-cyan
                                      :weight bold)
               (show-paren-match :background unspecified
                                 :foreground ,kabukicho-cyan
                                 :weight bold)
               (show-paren-match-expression :inherit match)
               (show-paren-mismatch :inherit font-lock-warning-face)
               ;; shr
               (shr-h1 :foreground ,kabukicho-pink :weight bold :height 1.3)
               (shr-h2 :foreground ,kabukicho-purple :weight bold)
               (shr-h3 :foreground ,kabukicho-green :slant italic)
               (shr-h4 :foreground ,kabukicho-yellow)
               (shr-h5 :foreground ,kabukicho-cyan)
               (shr-h6 :foreground ,kabukicho-orange)
               ;; slime
               (slime-repl-inputed-output-face :foreground ,kabukicho-purple)
               ;; solaire-mode
               (solaire-default-face :background ,bg2)
               ;; spam
               (spam :inherit gnus-summary-normal-read :foreground ,kabukicho-orange
                     :strike-through t :slant oblique)
               ;; speedbar (and sr-speedbar)
               (speedbar-button-face :foreground ,kabukicho-green)
               (speedbar-file-face :foreground ,kabukicho-cyan)
               (speedbar-directory-face :foreground ,kabukicho-purple)
               (speedbar-tag-face :foreground ,kabukicho-yellow)
               (speedbar-selected-face :foreground ,kabukicho-pink)
               (speedbar-highlight-face :inherit match)
               (speedbar-separator-face :background ,kabukicho-bg
                                        :foreground ,kabukicho-fg
                                        :weight bold)
               ;; tab-bar & tab-line (since Emacs 27.1)
               (tab-bar :inherit variable-pitch
                        :foreground ,kabukicho-purple
                        :background ,kabukicho-current)
               (tab-bar-tab :foreground ,kabukicho-pink :background ,kabukicho-bg
                            :box (:line-width 2 :color ,kabukicho-bg :style nil))
               (tab-bar-tab-inactive :foreground ,kabukicho-purple :background ,bg2
                                     :box (:line-width 2 :color ,bg2 :style nil))
               (tab-line :inherit variable-pitch
                         :foreground ,kabukicho-purple
                         :background ,kabukicho-current
                         :height 0.92)
               (tab-line-close-highlight :foreground ,kabukicho-red)
               (tab-line-highlight :weight bold)
               (tab-line-tab :foreground ,kabukicho-purple :background ,bg2
                             :box (:line-width 4 :color ,bg2 :style nil))
               (tab-line-tab-current :foreground ,kabukicho-pink :background ,kabukicho-bg
                                     :box (:line-width 4 :color ,kabukicho-bg :style nil)
                                     :weight bold)
               (tab-line-tab-group :background ,kabukicho-comment)
               (tab-line-tab-inactive :inherit tab-line-tab)
               (tab-line-tab-inactive-alternate :background ,bg3)
               (tab-line-tab-modified :slant italic)
               (tab-line-tab-special :foreground ,kabukicho-green)
               ;; telephone-line
               (telephone-line-accent-active :background ,kabukicho-bg :foreground ,kabukicho-pink)
               (telephone-line-accent-inactive :background ,bg2 :foreground ,kabukicho-purple)
               (telephone-line-unimportant :background ,kabukicho-bg :foreground ,kabukicho-comment)
               ;; term
               (term :foreground ,kabukicho-fg :background ,kabukicho-bg)
               (term-color-black :foreground ,kabukicho-bg :background ,kabukicho-comment)
               (term-color-blue :foreground ,kabukicho-purple :background ,kabukicho-purple)
               (term-color-cyan :foreground ,kabukicho-cyan :background ,kabukicho-cyan)
               (term-color-green :foreground ,kabukicho-green :background ,kabukicho-green)
               (term-color-magenta :foreground ,kabukicho-pink :background ,kabukicho-pink)
               (term-color-red :foreground ,kabukicho-red :background ,kabukicho-red)
               (term-color-white :foreground ,kabukicho-fg :background ,kabukicho-fg)
               (term-color-yellow :foreground ,kabukicho-yellow :background ,kabukicho-yellow)
               ;; TeX (auctex)
               (TeX-error-description-error :inherit error)
               (TeX-error-description-tex-said :foreground ,kabukicho-cyan)
               (TeX-error-description-warning :inherit warning)
               ;; tree-sitter
               (tree-sitter-hl-face:attribute :inherit font-lock-constant-face)
               (tree-sitter-hl-face:comment :inherit font-lock-comment-face)
               (tree-sitter-hl-face:constant :inherit font-lock-constant-face)
               (tree-sitter-hl-face:constant.builtin :inherit font-lock-builtin-face)
               (tree-sitter-hl-face:constructor :inherit font-lock-constant-face)
               (tree-sitter-hl-face:escape :foreground ,kabukicho-pink)
               (tree-sitter-hl-face:function :inherit font-lock-function-name-face)
               (tree-sitter-hl-face:function.builtin :inherit font-lock-builtin-face)
               (tree-sitter-hl-face:function.call :inherit font-lock-function-name-face
                                                  :weight normal)
               (tree-sitter-hl-face:function.macro :inherit font-lock-preprocessor-face)
               (tree-sitter-hl-face:function.special :inherit font-lock-preprocessor-face)
               (tree-sitter-hl-face:keyword :inherit font-lock-keyword-face)
               (tree-sitter-hl-face:punctuation :foreground ,kabukicho-pink)
               (tree-sitter-hl-face:punctuation.bracket :foreground ,kabukicho-fg)
               (tree-sitter-hl-face:punctuation.delimiter :foreground ,kabukicho-fg)
               (tree-sitter-hl-face:punctuation.special :foreground ,kabukicho-pink)
               (tree-sitter-hl-face:string :inherit font-lock-string-face)
               (tree-sitter-hl-face:string.special :foreground ,kabukicho-red)
               (tree-sitter-hl-face:tag :inherit font-lock-keyword-face)
               (tree-sitter-hl-face:type :inherit font-lock-type-face)
               (tree-sitter-hl-face:type.parameter :foreground ,kabukicho-pink)
               (tree-sitter-hl-face:variable :inherit font-lock-variable-name-face)
               (tree-sitter-hl-face:variable.parameter :inherit tree-sitter-hl-face:variable
                                                       :weight normal)
               ;; undo-tree
               (undo-tree-visualizer-current-face :foreground ,kabukicho-orange)
               (undo-tree-visualizer-default-face :foreground ,fg2)
               (undo-tree-visualizer-register-face :foreground ,kabukicho-purple)
               (undo-tree-visualizer-unmodified-face :foreground ,kabukicho-fg)
               ;; web-mode
               (web-mode-builtin-face :inherit font-lock-builtin-face)
               (web-mode-block-delimiter-face :foreground ,kabukicho-cyan)
               (web-mode-comment-face :inherit font-lock-comment-face)
               (web-mode-constant-face :inherit font-lock-constant-face)
               (web-mode-css-property-name-face :foreground ,kabukicho-pink)
               (web-mode-css-selector-face :foreground ,kabukicho-green)
               (web-mode-doctype-face :inherit font-lock-comment-face)
               (web-mode-function-name-face :inherit font-lock-function-name-face)
               (web-mode-html-attr-equal-face :foreground ,kabukicho-fg)
               (web-mode-html-attr-name-face :foreground ,kabukicho-pink)
               (web-mode-html-attr-value-face :foreground ,kabukicho-purple)
               (web-mode-html-tag-bracket-face :foreground ,kabukicho-cyan)
               (web-mode-html-tag-face :foreground ,kabukicho-orange :weight bold)
               (web-mode-keyword-face :foreground ,kabukicho-pink)
               (web-mode-preprocessor-face :foreground ,kabukicho-pink)
               (web-mode-string-face :foreground ,kabukicho-yellow)
               (web-mode-type-face :inherit font-lock-type-face)
               (web-mode-warning-face :inherit font-lock-warning-face)
               ;; which-func
               (which-func :inherit font-lock-function-name-face)
               ;; which-key
               (which-key-key-face :inherit font-lock-builtin-face)
               (which-key-command-description-face :inherit default)
               (which-key-separator-face :inherit font-lock-comment-delimiter-face)
               (which-key-local-map-description-face :foreground ,kabukicho-green)
               ;; whitespace
               (whitespace-big-indent :background ,kabukicho-red :foreground ,kabukicho-red)
               (whitespace-empty :background ,kabukicho-orange :foreground ,kabukicho-red)
               (whitespace-hspace :background ,kabukicho-current :foreground ,kabukicho-comment)
               (whitespace-indentation :background ,kabukicho-orange :foreground ,kabukicho-red)
               (whitespace-line :background ,kabukicho-bg :foreground ,kabukicho-pink)
               (whitespace-newline :foreground ,kabukicho-comment)
               (whitespace-space :background ,kabukicho-bg :foreground ,kabukicho-comment)
               (whitespace-space-after-tab :background ,kabukicho-orange :foreground ,kabukicho-red)
               (whitespace-space-before-tab :background ,kabukicho-orange :foreground ,kabukicho-red)
               (whitespace-tab :background ,bg2 :foreground ,kabukicho-comment)
               (whitespace-trailing :inherit trailing-whitespace)
               ;; yard-mode
               (yard-tag-face :inherit font-lock-builtin-face)
               (yard-directive-face :inherit font-lock-builtin-face))))

  (apply #'custom-theme-set-faces
         'kabukicho
         (let ((expand-with-func
                (lambda (func spec)
                  (let (reduced-color-list)
                    (dolist (col colors reduced-color-list)
                      (push (list (car col) (funcall func col))
                            reduced-color-list))
                    (eval `(let ,reduced-color-list
                             (backquote ,spec))))))
               whole-theme)
           (pcase-dolist (`(,face . ,spec) faces)
             (push `(,face
                     ((((min-colors 16777216)) ; fully graphical envs
                       ,(funcall expand-with-func 'cadr spec))
                      (((min-colors 256))      ; terminal withs 256 colors
                       ,(if kabukicho-use-24-bit-colors-on-256-colors-terms
                            (funcall expand-with-func 'cadr spec)
                          (funcall expand-with-func 'caddr spec)))
                      (t                       ; should be only tty-like envs
                       ,(funcall expand-with-func 'cadddr spec))))
                   whole-theme))
           whole-theme)))


;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'kabukicho)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; kabukicho-theme.el ends here
