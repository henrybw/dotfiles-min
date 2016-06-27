;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load. If it is the symbol `all' instead
   ;; of a list then all discovered layers will be installed.
   dotspacemacs-configuration-layers
   '(
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press <SPC f e R> (Vim style) or
     ;; <M-m f e R> (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     better-defaults
     emacs-lisp
     lua
     osx
     git
     markdown
     org
     cscope
     auto-completion
     eyebrowse
     (shell :variables
            shell-default-shell 'ansi-term
            shell-default-height 30
            shell-default-position 'bottom)
     ;; irony-mode
     ;; spell-checking
     ;; syntax-checking
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages
   '(
     undo-tree
     ac-etags
     bison-mode
     )
   ;; A list of packages and/or extensions that will not be install and loaded.
   dotspacemacs-excluded-packages '()
   ;; If non-nil spacemacs will delete any orphan packages, i.e. packages that
   ;; are declared in a layer which is not a member of
   ;; the list `dotspacemacs-configuration-layers'. (default t)
   dotspacemacs-delete-orphan-packages t))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t
   ;; Maximum allowed time in seconds to contact an ELPA repository.
   dotspacemacs-elpa-timeout 5
   ;; If non nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. (default t)
   dotspacemacs-check-for-update nil
   ;; One of `vim', `emacs' or `hybrid'. Evil is always enabled but if the
   ;; variable is `emacs' then the `holy-mode' is enabled at startup. `hybrid'
   ;; uses emacs key bindings for vim's insert mode, but otherwise leaves evil
   ;; unchanged. (default 'vim)
   dotspacemacs-editing-style 'vim
   ;; If non nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'random
   ;; List of items to show in the startup buffer. If nil it is disabled.
   ;; Possible values are: `recents' `bookmarks' `projects'.
   ;; (default '(recents projects))
   dotspacemacs-startup-lists '(recents projects)
   ;; Number of recent files to show in the startup buffer. Ignored if
   ;; `dotspacemacs-startup-lists' doesn't include `recents'. (default 5)
   dotspacemacs-startup-recent-list-size 5
   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(solarized-dark
                         solarized-light
                         monokai
                         spacemacs-dark
                         spacemacs-light
                         leuven
                         zenburn)
   ;; If non nil the cursor color matches the state color in GUI Emacs.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m)
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs C-i, TAB and C-m, RET.
   ;; Setting it to a non-nil value, allows for separate commands under <C-i>
   ;; and TAB or <C-m> and RET.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil
   ;; (Not implemented) dotspacemacs-distinguish-gui-ret nil
   ;; The command key used for Evil commands (ex-commands) and
   ;; Emacs commands (M-x).
   ;; By default the command key is `:' so ex-commands are executed like in Vim
   ;; with `:' and Emacs commands are executed with `<leader> :'.
   dotspacemacs-command-key ":"
   ;; If non nil `Y' is remapped to `y$'. (default t)
   dotspacemacs-remap-Y-to-y$ t
   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"
   ;; If non nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil
   ;; If non nil then the last auto saved layouts are resume automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache
   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5
   ;; If non nil then `ido' replaces `helm' for some commands. For now only
   ;; `find-files' (SPC f f), `find-spacemacs-file' (SPC f e s), and
   ;; `find-contrib-file' (SPC f e c) are replaced. (default nil)
   dotspacemacs-use-ido nil
   ;; If non nil, `helm' will try to minimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil
   ;; if non nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content. (default nil)
   dotspacemacs-enable-paste-micro-state nil
   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4
   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar nil
   ;; If non nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup t
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90
   ;; If non nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters the
   ;; point when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling nil
   ;; If non nil line numbers are turned on in all `prog-mode' and `text-mode'
   ;; derivatives. If set to `relative', also turns on relative line numbers.
   ;; (default nil)
   dotspacemacs-line-numbers t
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all
   ;; If non nil advises quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   ;; (default '("ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now. (default nil)
   dotspacemacs-default-package-repository nil
   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed'to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup nil
   )
  ;; Default font. `powerline-scale' allows to quickly tweak the mode-line
  ;; size to make separators look not too crappy.
  (cond ((equal system-type 'darwin)
         (setq-default dotspacemacs-default-font '("Menlo"
                                      :size 10
                                      :weight normal
                                      :width normal
                                      :powerline-scale 1.5)))
        ((equal system-type 'gnu/linux)
         (setq-default dotspacemacs-default-font '("Hack"
                                      :size 12
                                      :weight normal
                                      :width normal
                                      :powerline-scale 1.0)))
        (setq-default dotspacemacs-default-font '("Source Code Pro"
                                     :size 12
                                     :weight normal
                                     :width normal
                                     :powerline-scale 1.1)))
  )

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init', before layer configuration
executes.
 This function is mostly useful for variables that need to be set
before packages are loaded. If you are unsure, you should try in setting them in
`dotspacemacs/user-config' first."

  (defun set-transparent-bg ()
    "Disable drawing background colors when running in a terminal"
    (unless (display-graphic-p (selected-frame))
      (set-face-background 'default "unspecified-bg" (selected-frame))
      (set-face-background 'linum "unspecified-bg" (selected-frame))))

  (add-hook 'window-setup-hook 'set-transparent-bg)
  (add-hook 'server-switch-hook 'set-transparent-bg)

  ;; irony-mode settings (disabled for now)
  ;; (setq-default dotspacemacs-configuration-layers
  ;;               '((c-c++ :variables c-c++-enable-clang-support t)))
  )

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration.
This is the place where most of your configurations should be done. Unless it is
explicitly specified that a variable should be set before a package is loaded,
you should place your code here."

  ;;;
  ;;; Utilities
  ;;;

  (evil-define-command evil-window-split-with-focus (&optional count file)
    "Splits the window horizontally and keeps focus."
    :repeat nil
    (interactive "P<f>")
    (evil-window-split count file)
    (other-window (if (null count) 1 count)))

  (evil-define-command evil-window-vsplit-with-focus (&optional count file)
    "Splits the window vertically and keeps focus."
    :repeat nil
    (interactive "P<f>")
    (evil-window-vsplit count file)
    (other-window (if (null count) 1 count)))

  ;; This is the equivalent of my vim "map" commands, i.e. apply everywhere
  (defun evil-define-key-for-states (states keycode handler)
    "Defines a keybinding mapping KEYCODE to HANDLER only for the evil state
maps STATES."
    (loop for state in states
          collect (define-key (symbol-value state) keycode handler)))

  (defvar evil-non-insert-states '(evil-normal-state-map
                                   evil-visual-state-map
                                   evil-replace-state-map
                                   evil-operator-state-map
                                   evil-motion-state-map)
    "All non-insert evil state maps")

  (defun what-face (pos)
    "Identifies the font face used at the given POS."
    (interactive "d")
    (let ((face (or (get-char-property (point) 'read-face-name)
                    (get-char-property (point) 'face))))
      (if face (message "Face: %s" face) (message "No face at %d" pos))))
  (evil-define-key-for-states evil-non-insert-states (kbd "<f2>") 'what-face)
  (define-key evil-insert-state-map (kbd "<f2>") 'what-face)

  ;;;
  ;;; Key bindings
  ;;;

  ;; C-c as general purpose escape key sequence.
  ;; A HUGE THANK YOU TO: https://www.emacswiki.org/emacs/Evil#toc16
  (defun my-esc (prompt)
    "Functionality for escaping generally. Includes exiting Evil insert state
and C-g binding."
    (cond
     ((or (derived-mode-p 'magit-mode) (derived-mode-p 'term-mode))
      (kbd "C-c"))
     ;; If we're in one of the Evil states that defines [escape] key, return
     ;; [escape] so as Key Lookup will use it.
     ((or (evil-insert-state-p) (evil-normal-state-p) (evil-replace-state-p)
          (evil-visual-state-p))
      [escape])
     ;; This is the best way I could infer for now to have C-c work during
     ;; evil-read-key. Note: As long as I return [escape] in normal-state, I
     ;; don't need this. eq overriding-terminal-local-map evil-read-key-map)
     ;; (keyboard-quit) (kbd ""))
     (t (kbd "C-g"))))
  (define-key key-translation-map (kbd "C-c") 'my-esc)

  ;; Works around the fact that Evil uses read-event directly when in operator
  ;; state, which doesn't use the key-translation-map.
  (define-key evil-operator-state-map (kbd "C-c") 'keyboard-quit)
  (setq-default evil-escape-key-sequence (kbd "C-c"))
  (setq-default evil-esc-delay 0)

  ;; Muscle memory from my vimrc leader keybindings
  (evil-leader/set-key (kbd "\\") 'spacemacs/toggle-fill-column-indicator)
  (evil-leader/set-key (kbd "RET") 'delete-trailing-whitespace)

  ;; SPC-` is already defined to something, which overrides our binding
  (define-key evil-normal-state-map (kbd "SPC `") nil)
  (evil-leader/set-key (kbd "`" ) 'spacemacs/alternate-buffer)

  ;; Reload .spacemacs (instead of just syncing configuration layers)
  (defun dotspacemacs/reload-dotfile ()
    (interactive)
    (dotspacemacs/load-file)
    (dotspacemacs/user-init)
    (dotspacemacs/user-config))
  (evil-leader/set-key "f e r" 'dotspacemacs/reload-dotfile)

  ;; Port of cscope_maps.vim
  ;; XXX - TYPOS ლ(ಠ益ಠლ)
  (defalias 'helm-cscope-find-calling-this-function
    'helm-cscope-find-calling-this-funtcion)
  (let ((cscope-key-maps
         '(("s" . helm-cscope-find-this-symbol)
           ("g" . helm-cscope-find-global-definition)
           ("c" . helm-cscope-find-calling-this-function)
           ("C" . helm-cscope-find-called-function)
           ("t" . helm-cscope-find-this-text-string)
           ("e" . helm-cscope-find-egrep-pattern)
           ("f" . helm-cscope-find-this-file)
           ("i" . helm-cscope-find-files-including-file)
           ("a" . helm-cscope-find-assignments-to-this-symbol))))
    (loop for key-map in cscope-key-maps
          collect (lexical-let* ((key (car key-map))
                                 (handler (cdr key-map)))
                    (evil-ex-define-cmd (replace-regexp-in-string
                                         "helm-" "" (symbol-name handler))
                                        handler)
                    ;; To do the first type of search, hit 'CTRL-\', followed
                    ;; by one of the cscope search types above. The result of
                    ;; your cscope search will be displayed in the current
                    ;; window.  You can use CTRL-T to go back to where you
                    ;; were before the search.
                    (define-key evil-normal-state-map
                      (kbd (concat "C-\\ " key))
                      (defalias (intern (replace-regexp-in-string
                                         "helm-cscope-"
                                         "cs-"
                                         (symbol-name handler)))
                        (lambda () (interactive)
                          (funcall handler (thing-at-point 'symbol)))))
                    ;; Using 'CTRL-\ spacebar' then a search type makes the
                    ;; window split horizontally, with search result displayed
                    ;; in the new window.
                    (define-key evil-normal-state-map
                      (kbd (concat "C-\\ SPC " key))
                      (defalias (intern (replace-regexp-in-string
                                         "helm-cscope-"
                                         "cs-sp-"
                                         (symbol-name handler)))
                        (lambda () (interactive) (evil-window-split-with-focus)
                          (funcall handler (thing-at-point 'symbol)))))
                    ;; Hitting 'CTRL-\';; then space *twice* before the search
                    ;; type does a vertical split instead of a horizontal one.
                    (define-key evil-normal-state-map
                      (kbd (concat "C-\\ SPC SPC " key))
                      (defalias (intern (replace-regexp-in-string
                                         "helm-cscope-"
                                         "cs-vsp-"
                                         (symbol-name handler)))
                        (lambda () (interactive) (evil-window-vsplit-with-focus)
                          (funcall handler (thing-at-point 'symbol))))))))

  ;; Versions of cc and o that don't exit normal mode
  (evil-leader/set-key "cc"
    (defalias 'evil-clear-line
      (lambda () (interactive) (evil-change-line 0 (evil-end-of-line)))))
  (evil-leader/set-key "o"
    (defalias 'evil-add-newline-below
      (lambda () (interactive) (interactive) (evil-insert-newline-below))))
  (evil-leader/set-key "O"
    (defalias 'evil-add-newline-above
      (lambda () (interactive) (interactive) (evil-insert-newline-above))))

  ;; I find just having zt and zb limiting; oftentimes, I'll want to put the
  ;; current piece of code I'm looking at in either top or bottom 'quadrant' of
  ;; the screen. Hence, the scrolling to 25% and 75% commands.
  (define-key evil-motion-state-map "zv"
    (defalias 'evil-scroll-line-to-25-pct
      (lambda () (interactive) (recenter (- (round (* (window-body-height) 0.25)))))))
  (define-key evil-motion-state-map "zr"
    (defalias 'evil-scroll-line-to-75-pct
      (lambda () (interactive) (recenter (- (round (* (window-body-height) 0.75)))))))
  ;; The normal map binding of 'zr' will override our motion map binding unless
  ;; we undefine it.
  (define-key evil-normal-state-map "zr" nil)

  ;; Open tags in splits
  (define-key evil-window-map (kbd "]")
    (defalias 'evil-split-jump-to-tag
      (lambda ()
        (interactive) (evil-window-split-with-focus) (evil-jump-to-tag))))
  (define-key evil-window-map (kbd "\\")
    (defalias 'evil-vsplit-jump-to-tag
      (lambda () (interactive) (evil-window-vsplit-with-focus) (evil-jump-to-tag))))

  ;; Support for tag completion in the ex command line. Implementation adapted
  ;; from tags-complete-tags-table-file.
  (defun tags-complete-tags (string pred action)
    "Provide completions for etags."
    (with-current-buffer (current-buffer)
      (save-excursion
        ;; If we need to ask for the tag table, allow that
        (let ((enable-recursive-minibuffers t))
          (visit-tags-table-buffer))
        (complete-with-action action (tags-completion-table) string pred))))
  (evil-ex-define-argument-type tag
    "Handles a tag argument, with completion support."
    :collection tags-complete-tags)
  (evil-define-interactive-code "<t>"
    "Tag buffer argument."
    :ex-arg tag
    (list (when (evil-ex-p) evil-ex-argument)))

  ;; Vim-style tag jumping ex command
  ;; TODO HBW - Make C-t also pop cscope tags
  (evil-define-command evil-find-tag (tagname)
    "Jump to the tag TAGNAME."
    :repeat nil
    (interactive "<t>")
    (find-tag tagname))
  (evil-ex-define-cmd "ta[g]" 'evil-find-tag)
  (evil-define-command evil-prev-tag ()
    "Jump to the previous matching tag."
    :repeat nil
    (interactive)
    (find-tag "" '-))
  (evil-ex-define-cmd "tp[rev]" 'evil-prev-tag)
  (evil-leader/set-key "[" 'evil-prev-tag)
  (evil-define-command evil-next-tag ()
    "Jump to the next matching tag."
    :repeat nil
    (interactive)
    (find-tag "" t))
  (evil-ex-define-cmd "tn[ext]" 'evil-next-tag)
  (evil-leader/set-key "]" 'evil-next-tag)

  ;; Map eyebrowse workspaces as an analog to vim tabs
  (defun eyebrowse-create-workspace (count)
    "Creates a new workspace after the last existing workspace. When used with
a numerical argument, switch to workspace number COUNT."
    ;; Adapted from eyebrowse-next-window-config
    (interactive "P")
    (let* ((window-configs (eyebrowse--get 'window-configs))
           (last-window (1- (length window-configs))))
      (if count
          (eyebrowse-switch-to-window-config count)
        (eyebrowse-switch-to-window-config
         (1+ (car (nth last-window window-configs)))))))
  (evil-ex-define-cmd "tabnew" 'eyebrowse-create-workspace)
  (evil-ex-define-cmd "tabe[dit]" 'eyebrowse-create-workspace)
  (evil-ex-define-cmd "tabn[ext]" 'eyebrowse-next-window-config)
  (evil-ex-define-cmd "tabp[rev]" 'eyebrowse-prev-window-config)
  (evil-ex-define-cmd "tabc[lose]" 'eyebrowse-close-window-config)
  (evil-ex-define-cmd "tabs" 'eyebrowse-switch-to-window-config)

  ;; Too easy to be scrolling with C-u/d and then hit j and have it insert a
  ;; line accidentally.
  (global-unset-key (kbd "C-j"))

  ;; Make K perform the reverse analog of J (i.e. split a line)
  (define-key evil-motion-state-map "K" (kbd "\"_s RET C-c"))
  ;; The normal map binding of 'K' will override our motion map binding unless
  ;; we undefine it.
  (define-key evil-normal-state-map "K" nil)

  ;; Having smart doc lookup is still nice, so use C-k for that and move
  ;; kill-line over to insert mode (Which is more consistent with C-w, etc.).
  (define-key evil-normal-state-map (kbd "C-k") 'spacemacs/evil-smart-doc-lookup)
  (define-key evil-insert-state-map (kbd "C-k") 'kill-line)

  ;; Speaking of readline...
  (define-key evil-insert-state-map (kbd "C-u") 'kill-whole-line)

  ;; Mimic the behavior of the "r" formatoption in vim (i.e. automatically
  ;; insert the current comment leader after hitting RET).
  ;; TODO HBW - join comment lines
  (defun new-line-with-comment ()
    (interactive)
    ;; c-indent-new-comment-line can intelligently continue comment leader
    ;; characters (e.g. "*" in multi-line C comments), but it's not available
    ;; outside of cc mode.
    (if (bound-and-true-p c-buffer-is-cc-mode)
        (progn (c-indent-new-comment-line)
               (c-indent-line))
      (comment-indent-new-line)))
  (define-key evil-motion-state-map (kbd "RET") 'new-line-with-comment)
  (define-key evil-insert-state-map (kbd "RET") 'new-line-with-comment)

  (evil-define-key-for-states '(evil-insert-state-map evil-motion-state-map)
                              (kbd "M-j") 'evil-ret)

  ;; Make tab insert indentation up to the next tab stop
  (define-key evil-insert-state-map (kbd "TAB") 'tab-to-tab-stop)

  ;; Add some familiar functionality to helm buffers
  (defun add-helm-keymaps ()
    (define-key helm-map (kbd "C-u") 'kill-whole-line)
    (define-key helm-map (kbd "C-e") 'helm-next-line)
    (define-key helm-map (kbd "C-y") 'helm-previous-line)
    (define-key helm-map (kbd "C-w") 'backward-kill-word)
    (define-key helm-map (kbd "C-f") 'helm-next-page)
    (define-key helm-map (kbd "C-b") 'helm-previous-page))
  (add-hook 'helm-mode-hook 'add-helm-keymaps)

  ;; I was not convinced :P
  ;; <http://spacemacs.org/doc/DOCUMENTATION.html#orgheadline59>
  (evil-define-key 'visual evil-surround-mode-map "s" 'evil-substitute)
  (evil-define-key 'visual evil-surround-mode-map "S" 'evil-surround-region)

  ;; For some reason this doesn't work quite correctly out of the box
  (define-key evil-motion-state-map (kbd "C-i") 'evil-jump-forward)

  ;; I fat finger these all the damn time
  (define-key evil-window-map (kbd "C-h") 'evil-window-left)
  (define-key evil-window-map (kbd "C-j") 'evil-window-down)
  (define-key evil-window-map (kbd "C-k") 'evil-window-up)
  (define-key evil-window-map (kbd "C-l") 'evil-window-right)

  ;; Mimic vim's window-switching behavior and focus the new split
  (define-key evil-window-map (kbd "s") 'evil-window-split-with-focus)
  (define-key evil-window-map (kbd "v") 'evil-window-vsplit-with-focus)

  ;; Mimic C-w x for exchanging windows... but even better (no count required,
  ;; because ace-swap-window is interactive).
  (define-key evil-window-map (kbd "x")
    (defalias 'evil-window-exchange (lambda () (interactive) (ace-swap-window))))

  (evil-define-key-for-states evil-non-insert-states
                              (kbd "C-q") 'evil-visual-block)
  (evil-define-key-for-states evil-non-insert-states
                              (kbd "C-l") 'redraw-display)
  (evil-define-key-for-states evil-non-insert-states
                              (kbd "C-p") 'helm-projectile-find-file)

  ;; Often I hold shift too long when issuing these commands
  ;; (adapted from <http://www2.mathematik.hu-berlin.de/~altmeyrx/BZQ/vimrc>)
  (evil-ex-define-cmd "Q" 'evil-quit-all)
  (evil-ex-define-cmd "Qa[ll]" 'evil-quit-all)
  (evil-ex-define-cmd "W" 'evil-write)
  (evil-ex-define-cmd "Wq" 'evil-save-and-quit)
  (evil-ex-define-cmd "WQ" 'evil-save-and-quit)
  (evil-ex-define-cmd "Wa[ll]" 'evil-write-all)
  (evil-ex-define-cmd "E" 'evil-edit)

  ;; Make control codes work in terminal mode
  (delete 'term-mode evil-insert-state-modes)
  (delete 'shell-mode evil-insert-state-modes)
  (add-to-list 'evil-emacs-state-modes 'term-mode)
  (add-to-list 'evil-emacs-state-modes 'shell-mode)

  ;;;
  ;;; Theming
  ;;;

  ;; I usually use vertical splits to follow tags / call chains, so I want them
  ;; to progress left-to-right. However, I tend to use horizontal splits to
  ;; examine definitions, for which I'd like to have them open upward (so the
  ;; opened buffer with the definition appears above the current buffer I'm in).
  (setq-default evil-split-window-right)
  (setq-default evil-split-window-below)

  ;; Keep line numbers right aligned and dynamically padded
  (setq-default linum-format 'dynamic)

  ;; Add a space between line number column and the text buffer, because these
  ;; run into each other when emacs renders inside a terminal. However, when in
  ;; GUI mode, the line numbers need to be left-padded because emacs uses a
  ;; glyph instead of a pipe character for drawing the split separator.
  (unless (display-graphic-p (selected-frame))
    (defadvice linum-update-window (around linum-dynamic activate)
      (let* ((w (length (number-to-string
                         (count-lines (point-min) (point-max)))))
             (linum-format (concat "%" (number-to-string w) "d ")))
        ad-do-it)))

  ;; Hide menu bar
  (menu-bar-mode -1)

  ;; When in terminal, change the modeline to be more vim-like, because it
  ;; doesn't rely on glyphs and symbols as much (and, unlike the GUI, line
  ;; height is strictly uniform and cannot be changed).
  (unless (display-graphic-p (selected-frame))
    (powerline-vim-theme))

  ;; Always display column number
  (setq-default column-number-mode t)

  ;; Move minimum when cursor exits view, instead of recentering
  (setq-default scroll-conservatively 101)

  ;; Mouse scroll moves 1 line at a time, instead of 5 lines
  (setq-default mouse-wheel-scroll-amount '(1))

  ;; On a long mouse scroll keep scrolling by 1 line
  (setq-default mouse-wheel-progressive-speed nil)

  ;; Get mouse working in terminal mode
  (global-set-key (kbd "<mouse-4>")
                  (defalias 'mouse-wheel-scroll-down
                    (lambda () (interactive) (scroll-down 1))))
  (global-set-key (kbd "<mouse-5>")
                  (defalias 'mouse-wheel-scroll-up
                    (lambda () (interactive) (scroll-up 1))))

  ;; Skip the startup message and display first buffer directly
  (setq-default inhibit-startup-message t)

  ;; Make powerline separator an arrow like it is in vim by default
  (setq powerline-default-separator 'arrow)

  (show-paren-mode t)
  (setq-default spacemacs-show-trailing-whitespace t)
  (setq-default font-lock-maximum-decoration t)

  ;;;
  ;;; Formatting
  ;;;

  (global-company-mode nil)
  (setq-default comment-multi-line t)
  (global-linum-mode t)
  (auto-fill-mode)

  (setq-default helm-buffers-fuzzy-matching t)
  (setq-default helm-recentf-fuzzy-match t)
  (setq-default helm-etags-fuzzy-match t)

  (setq-default truncate-lines t)  ; Turn off word wrap

  ;; Always re-read the tags file without prompting
  (setq-default tags-revert-without-query t)

  (sp-pair "'" nil :actions :rem)
  (sp-pair "\"" nil :actions :rem)

  ;;; These were taken from https://www.emacswiki.org/emacs/NoTabs#toc2

  (defun how-many-region (begin end regexp &optional interactive)
    "Print number of non-trivial matches for REGEXP in region.
Non-interactive arguments are Begin End Regexp"
    (interactive "r\nsHow many matches for (regexp): \np")
    (let ((count 0) opoint)
      (save-excursion
        (setq end (or end (point-max)))
        (goto-char (or begin (point)))
        (while (and (< (setq opoint (point)) end)
                    (re-search-forward regexp end t))
          (if (= opoint (point))
              (forward-char 1)
            (setq count (1+ count))))
        (if interactive (message "%d occurrences" count))
        count)))

  (defun infer-indentation-style ()
    "If our source file uses tabs, we use tabs, if spaces spaces, and if
neither, we use the current indent-tabs-mode."
    (let ((space-count (how-many-region (point-min) (point-max) "^  "))
          (tab-count (how-many-region (point-min) (point-max) "^\t")))
      (if (> space-count tab-count) (setq indent-tabs-mode nil))
      (if (> tab-count space-count) (setq indent-tabs-mode t))))

  (defun my-c-mode-font-lock-if0 (limit)
    ;; This is a function copied from stackoverflow to facify #if 0/#else/#endif
    ;; keywords. The comments are added by myself to make it understandable.
    ;; https://gist.github.com/fortitudezhang/8505367
    (save-restriction
      (widen)
      (save-excursion
        (goto-char (point-min))
        (let ((depth 0) str start start-depth)
          ;; Search #if/#else/#endif using regular expression.
          (while (re-search-forward "^\\s-*#\\s-*\\(if\\|else\\|endif\\)" limit 'move)
            (setq str (match-string 1))
            ;; Handle #if.
            (if (string= str "if")
                (progn
                  (setq depth (1+ depth))
                  ;; Handle neariest 0.
                  (when (and (null start) (looking-at "\\s-+0"))
                    (setq start (match-end 0)
                          start-depth depth)))
              ;; Handle #else, here we can decorate #if 0->#else block using 'font-lock-comment-face'.
              (when (and start (= depth start-depth))
                (c-put-font-lock-face start (match-beginning 0) 'font-lock-comment-face)
                (setq start nil))
              ;; Handle #endif, return to upper block if possible.
              (when (string= str "endif")
                (setq depth (1- depth)))))
          ;; Corner case when there are only #if 0 (May be you are coding now:))
          (when (and start (> depth 0))
            (c-put-font-lock-face start (point) 'font-lock-comment-face)))))
    nil)

  (defun c-mode-highlight-function-calls (limit)
    (let ((retval nil))
      (save-restriction
        (widen)
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward "\\(\\w+\\)\\s-*\(" limit 'move)
            (let* ((start (match-beginning 1))
                   (end (match-end 1))
                   (face (or (get-char-property start 'read-face-name)
                             (get-char-property start 'face))))
              (when (or (null face)
                        ;; Macro block constructs get incorrectly tagged as variable names
                        (eq face font-lock-variable-name-face))
                (c-put-font-lock-face start end 'font-lock-function-name-face)
                (setq retval t))))))
      retval))

  (defun c-mode-common-settings ()
    ;; C/C++ formatting style should be K&R, but with 4-space indents
    (setq c-default-style "k&r")
    (setq c-basic-offset 4)
    (setq tab-width 4)
    (setq fill-column 81)  ; So the fill column is drawn *after* the 80th column of text
    ;; Line up each line in a paren list with the position of the opening paren
    (c-set-offset 'arglist-close 'c-lineup-arglist)
    ;; Force preprocessor macros to be aligned to the first column
    (setq c-electric-pound-behavior '(alignleft))
    ;; Make C block comments continue with stars on each line
    (setq c-block-comment-prefix "* ")
    ;; When automatically commenting out regions, use C++ style one-line comments
    (setq comment-start "//" comment-end "")

    ;; Use spaces for indentation, unless the file says otherwise
    (setq indent-tabs-mode nil)
    (infer-indentation-style)

    (define-key evil-normal-state-map (kbd "SPC `") nil)
    (evil-leader/set-key (kbd "`" ) 'spacemacs/alternate-buffer)

    ;; Highlight function calls and #if 0 blocks
    (font-lock-add-keywords nil
                            '((c-mode-highlight-function-calls . font-lock-function-name-face)
                              ;; Sometimes operations like undo will confuse font-lock and the
                              ;; custom function name highlighting logic specified above.
                              ("\\(\\w+\\)\\s-*\(" (1 font-lock-function-name-face))
                              (my-c-mode-font-lock-if0 (0 font-lock-comment-face prepend)))
                            'add-to-end)
    )

  (add-hook 'c-mode-common-hook 'c-mode-common-settings)

  (defun shell-mode-config ()
    (linum-mode -1)
    (use-local-map (copy-keymap shell-mode-map))
    (local-set-key (kbd "C-g") 'comint-interrupt-subjob)
    (local-set-key (kbd "C-d") 'comint-delchar-or-maybe-eof))

  (add-hook 'shell-mode-hook 'shell-mode-config)

  (defun term-mode-config ()
    (use-local-map (copy-keymap term-mode-map))
    (local-set-key (kbd "C-g") 'comint-interrupt-subjob))

  (add-hook 'term-mode-hook 'term-mode-config)

  ;; irony-mode stuff (disabled)
  ;; (global-flycheck-mode -1)
  ;; Tweak default flycheck face to be less offensive
  ;; (custom-set-faces
  ;;   '(default ((t (:background nil))))
  ;;   '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
  ;;   '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil))))
  ;;   '(flycheck-error ((((class color) (min-colors 89)) (:background nil :foreground "red" :bold t)))))

;;; --------------------------------------------------------------------------
;;; vim-aurora-theme.el --- Emacs theme with a dark background.
;;; TODO HBW - actually package this up

;; Copyright (C) 2016, Henry Baba-Weiss

;; Author: Henry Baba-Weiss
;; https://github.com/henrybw/vim-colors-aurora
;; Version: 0.1
;; Package-Requires: ((emacs "24"))
;; Created with emacs-theme-generator, https://github.com/mswift42/theme-creator.


;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of Emacs.

;;; Commentary:
;;;
;;; 'Aurora' is my custom vim color scheme, modeled closely after the Solarized
;;; Dark color scheme. It was designed using Solarized Dark as a base starting
;;; point, but it diverges according to the following design principles:
;;;
;;;  o Compatibility/consistency between gvim and terminal vim.
;;;    Solarized doesn't tend to work so well using xterm 256 colors, possibly
;;;    because its color choice is so specialized. Initially, my custom color
;;;    scheme (that would eventually turn into Aurora) was an attempt to mimic
;;;    Solarized, but with the constraint of only using xterm colors. As much as
;;;    possible, I've strived to keep the 24-bit gvim colors identical, or at
;;;    least very similar, to the color choices used for the terminal color scheme.
;;;
;;;  o Emphasis on higher contrast color groupings.
;;;    My biggest complaint with Solarized Dark is that, when editing code, the
;;;    color choice makes various syntax elements blend together too much. The
;;;    most notable example is the color of comments: a dark gray that makes them
;;;    very easy to gloss over when scanning through code.
;;;
;;;    Aurora, on the other hand, emphasizes contrast between major syntactical
;;;    elements, e.g. comments, identifiers, and keywords. This makes it much
;;;    easier to recognize and visually group together syntactical features while
;;;    scanning through code.
;;;
;;;  o Harmonious color choices.
;;;    While I do not profess to know much about formal color theory, I chose
;;;    these particular colors in an attempt to keep them 'harmonious' with each
;;;    other. By that, I mean that this color scheme is heavily centered around
;;;    blue/green tones, accented by light grays and dark turquoises. There was no
;;;    formal process or method to my selection; I simply hand-tweaked until I
;;;    found the colors aesthetically pleasing and, to my eye, balanced.
;;;

;;; Code:

(deftheme vim-aurora)
(let ((class '((class color) (min-colors 89)))
      (fg1        "#bcbcbc")
      (fg2        "#ababab")
      (fg3        "#9b9b9b")
      (fg4        "#8b8b8b")
      (bg1        "#002b36")
      (bg2        "#303030")
      (bg3        "#2b4852")
      (bg4        "#3e5861")
      (key2       "#96b832")
      (key3       "#789a0a")
      (builtin    "#00afff")
      (operator   "#87afdf")
      (keyword    "#87af00")
      (func       "#00afff")
      (const      "#00dfdf")
      (comment    "#55ffff")
      (doc        "#87dfdf")
      (str        "#00dfdf")
      (type       "#87ff5f")
      (preproc    "#87afff")
      (var        "#87afdf")
      (warning    "#dfaf87")
      (cursor     "#87afaf")
      (matchparen "#00afaf")
      (searchfg   "#000000")
      (searchbg   "#00ffff")
      (persisthl  "#008787")
      (region     "#005f5f")
      (linum      "#606060")
      (fillcolumn "#00af87")
      (trailing   "#af0000")
      (diffhdr-fg "#87af00")
      (diffhdr-bg "#00005f")
      (diffadd-fg "#00ff00")
      (diffadd-bg "#005f00")
      (diffdel-fg "#ff0000")
      (diffdel-bg "#5f0000")
      (diff3-fg   "#005f5f")
      (diff3-bg   "#00ffff")
      (magit-bg   "#444444")
      (modeline   "#000000")
      (mode-fg    "#dfdfdf")
      )
  (custom-theme-set-faces
   'vim-aurora
   `(default ((,class (:background ,bg1 :foreground ,fg1))))
   `(font-lock-builtin-face ((,class (:foreground ,builtin))))
   `(font-lock-comment-face ((,class (:foreground ,comment))))
   `(font-lock-comment-delimiter-face ((,class (:foreground ,comment))))
   `(font-lock-negation-char-face ((,class (:foreground ,const))))
   `(font-lock-reference-face ((,class (:foreground ,const))))
   `(font-lock-constant-face ((,class (:foreground ,const))))
   `(font-lock-doc-face ((,class (:foreground ,doc))))
   `(font-lock-function-name-face ((,class (:foreground ,func))))
   `(font-lock-keyword-face ((,class :foreground ,keyword)))
   `(font-lock-string-face ((,class (:foreground ,str))))
   `(font-lock-type-face ((,class (:foreground ,type ))))
   `(font-lock-variable-name-face ((,class (:foreground ,var))))
   `(font-lock-preprocessor-face ((,class (:foreground ,preproc))))
   `(font-lock-negation-char-face ((,class (:foreground ,operator))))
   `(font-lock-warning-face ((,class (:foreground ,warning :background ,bg2))))
   `(region ((,class (:background ,fg1 :foreground ,bg1))))
   `(highlight ((,class (:foreground ,fg3 :background ,bg3))))
   `(hl-line ((,class (:background ,bg2))))
   `(hl-todo ((,class (:foreground ,warning))))
   `(highlight-numbers-number ((,class (:foreground ,const))))
   `(highlight-symbol-face ((,class (:foreground ,func))))
   `(isearch ((,class (:foreground ,searchfg :background ,searchbg))))
   `(evil-search-highlight-persist-highlight-face ((,class (:background ,persisthl))))
   `(region ((,class (:foreground nil :background ,region))))
   `(secondary-selection ((,class (:foreground nil :background ,region))))
   `(linum ((,class (:foreground ,linum :background nil))))
   `(fringe ((,class (:background nil :foreground ,fg4))))
   `(cursor ((,class (:background ,cursor))))
   `(show-paren-match-face ((,class (:background ,matchparen))))
   `(sp-pair-overlay-face ((,class (:background nil))))
   `(fci-rule-color ((,class (:foreground ,fillcolumn))))
   `(trailing-whitespace ((,class (:background unspecified :foreground ,trailing
                                               :inverse-video t))))
   `(whitespace-space ((,class (:background unspecified :foreground ,trailing
                                            :inverse-video unspecified :slant italic))))
   `(whitespace-hspace ((,class (:background unspecified :foreground ,trailing
                                             :inverse-video unspecified))))
   `(whitespace-tab ((,class (:background unspecified :foreground ,trailing
                                          :inverse-video t))))
   `(whitespace-newline ((,class(:background unspecified :foreground ,trailing
                                             :inverse-video unspecified))))
   `(whitespace-trailing ((,class (:background unspecified :foreground ,trailing
                                               :inverse-video t))))
   `(whitespace-line ((,class (:background unspecified :foreground ,trailing
                                           :inverse-video unspecified))))
   `(whitespace-space-before-tab ((,class (:background ,trailing :foreground unspecified
                                                       :inverse-video unspecified))))
   `(whitespace-indentation ((,class (:background unspecified :foreground ,trailing
                                                  :inverse-video unspecified))))
   `(whitespace-empty ((,class (:background unspecified :foreground ,trailing
                                            :inverse-video t))))
   `(whitespace-space-after-tab ((,class (:background unspecified :foreground ,trailing
                                                      :inverse-video t))))
   `(mode-line ((,class (:box (:line-width 1 :color nil) :foreground ,mode-fg :background ,modeline))))
   `(mode-line-inactive ((,class (:box (:line-width 1 :color nil :style pressed-button)
                                       :foreground ,fg3 :background ,modeline :weight normal))))
   `(mode-line-buffer-id ((,class (:foreground ,func :background ,modeline))))
   `(mode-line-highlight ((,class (:foreground ,keyword :box nil))))
   `(mode-line-emphasis ((,class (:foreground ,mode-fg :background ,modeline))))
   `(diff-header  ((,class (:background ,diffhdr-bg :foreground ,diffhdr-fg))))
   `(diff-file-header ((,class (:background ,diffhdr-bg :foreground ,diffhdr-fg))))
   `(diff-context ((,class (:foreground nil))))
   `(diff-added   ((,class (:foreground ,diffadd-fg))))
   `(diff-changed ((,class (:foreground nil))))
   `(diff-removed ((,class (:foreground ,diffdel-fg))))
   `(ediff-fine-diff-A ((,class (:background ,diffadd-bg))))
   `(ediff-fine-diff-B ((,class (:background ,diff3-bg))))
   `(ediff-fine-diff-C ((,class (:background ,diffdel-bg))))
   `(ediff-current-diff-C ((,class (:background ,region))))
   `(vertical-border ((,class (:foreground ,fg3))))
   `(minibuffer-prompt ((,class (:foreground ,keyword))))
   `(term ((,class (:foreground ,fg1 :background nil))))
   `(default-italic ((,class (:italic t))))
   `(link ((,class (:foreground ,const :underline t))))
   `(org-code ((,class (:foreground ,fg2))))
   `(org-hide ((,class (:foreground ,fg4))))
   `(org-level-1 ((,class (:foreground ,fg2 :height 1.1))))
   `(org-level-2 ((,class (:foreground ,fg3))))
   `(org-level-3 ((,class (:foreground ,fg4))))
   `(org-level-4 ((,class (:foreground ,bg4))))
   `(org-date ((,class (:underline t :foreground ,var) )))
   `(org-footnote  ((,class (:underline t :foreground ,fg4))))
   `(org-link ((,class (:underline t :foreground ,type ))))
   `(org-special-keyword ((,class (:foreground ,func))))
   `(org-block ((,class (:foreground ,fg3))))
   `(org-quote ((,class (:inherit org-block :slant italic))))
   `(org-verse ((,class (:inherit org-block :slant italic))))
   `(org-todo ((,class (:box (:line-width 1 :color ,fg3) :foreground ,keyword))))
   `(org-done ((,class (:box (:line-width 1 :color ,bg3) :foreground ,bg4))))
   `(org-warning ((,class (:underline t :foreground ,warning))))
   `(org-agenda-structure ((,class (:weight bold :foreground ,fg3 :box (:color ,fg4) :background ,bg3))))
   `(org-agenda-date ((,class (:foreground ,var :height 1.1 ))))
   `(org-agenda-date-weekend ((,class (:weight normal :foreground ,fg4))))
   `(org-agenda-date-today ((,class (:weight bold :foreground ,keyword :height 1.4))))
   `(org-agenda-done ((,class (:foreground ,bg4))))
   `(org-scheduled ((,class (:foreground ,type))))
   `(org-scheduled-today ((,class (:foreground ,func :weight bold :height 1.2))))
   `(org-ellipsis ((,class (:foreground ,builtin))))
   `(org-verbatim ((,class (:foreground ,fg4))))
   `(org-document-info-keyword ((,class (:foreground ,func))))
   `(font-latex-bold-face ((,class (:foreground ,type))))
   `(font-latex-italic-face ((,class (:foreground ,key3 :italic t))))
   `(font-latex-string-face ((,class (:foreground ,str))))
   `(font-latex-match-reference-keywords ((,class (:foreground ,const))))
   `(font-latex-match-variable-keywords ((,class (:foreground ,var))))
   `(ido-only-match ((,class (:foreground ,warning))))
   `(org-sexp-date ((,class (:foreground ,fg4))))
   `(ido-first-match ((,class (:foreground ,keyword))))
   `(gnus-header-content ((,class (:foreground ,keyword))))
   `(gnus-header-from ((,class (:foreground ,var))))
   `(gnus-header-name ((,class (:foreground ,type))))
   `(gnus-header-subject ((,class (:foreground ,func))))
   `(mu4e-view-url-number-face ((,class (:foreground ,type))))
   `(mu4e-cited-1-face ((,class (:foreground ,fg2))))
   `(mu4e-cited-7-face ((,class (:foreground ,fg3))))
   `(mu4e-header-marks-face ((,class (:foreground ,type))))
   `(ffap ((,class (:foreground ,fg4))))
   `(js2-private-function-call ((,class (:foreground ,const))))
   `(js2-jsdoc-html-tag-delimiter ((,class (:foreground ,str))))
   `(js2-jsdoc-html-tag-name ((,class (:foreground ,key2))))
   `(js2-external-variable ((,class (:foreground ,type  ))))
   `(js2-function-param ((,class (:foreground ,const))))
   `(js2-jsdoc-value ((,class (:foreground ,str))))
   `(js2-private-member ((,class (:foreground ,fg3))))
   `(js3-warning-face ((,class (:underline ,keyword))))
   `(js3-error-face ((,class (:underline ,warning))))
   `(js3-external-variable-face ((,class (:foreground ,var))))
   `(js3-function-param-face ((,class (:foreground ,key3))))
   `(js3-jsdoc-tag-face ((,class (:foreground ,keyword))))
   `(js3-instance-member-face ((,class (:foreground ,const))))
   `(warning ((,class (:foreground ,warning))))
   `(ac-completion-face ((,class (:underline t :foreground ,keyword))))
   `(info-quoted-name ((,class (:foreground ,builtin))))
   `(info-string ((,class (:foreground ,str))))
   `(icompletep-determined ((,class :foreground ,builtin)))
   `(undo-tree-visualizer-current-face ((,class :foreground ,builtin)))
   `(undo-tree-visualizer-default-face ((,class :foreground ,fg2)))
   `(undo-tree-visualizer-unmodified-face ((,class :foreground ,var)))
   `(undo-tree-visualizer-register-face ((,class :foreground ,type)))
   `(slime-repl-inputed-output-face ((,class (:foreground ,type))))
   `(rainbow-delimiters-depth-1-face ((,class :foreground ,fg1)))
   `(rainbow-delimiters-depth-2-face ((,class :foreground ,type)))
   `(rainbow-delimiters-depth-3-face ((,class :foreground ,var)))
   `(rainbow-delimiters-depth-4-face ((,class :foreground ,const)))
   `(rainbow-delimiters-depth-5-face ((,class :foreground ,keyword)))
   `(rainbow-delimiters-depth-6-face ((,class :foreground ,fg1)))
   `(rainbow-delimiters-depth-7-face ((,class :foreground ,type)))
   `(rainbow-delimiters-depth-8-face ((,class :foreground ,var)))
   `(magit-item-highlight ((,class :background nil)))
   `(magit-section-highlight ((,class (:background nil))))
   `(magit-section-heading ((,class (:foreground ,type :weight bold))))
   `(magit-blame-heading ((,class (:background ,magit-bg))))
   `(magit-blame-summary ((,class (:foreground ,fg1 :background ,magit-bg))))
   `(magit-blame-hash ((,class (:foreground ,preproc :background ,magit-bg))))
   `(magit-blame-date ((,class (:foreground ,preproc :background ,magit-bg))))
   `(magit-blame-name ((,class (:foreground ,func :background ,magit-bg))))
   `(magit-hunk-heading           ((,class (:background nil))))
   `(magit-hunk-heading-highlight ((,class (:background nil))))
   `(magit-diff-file-header ((,class (:foreground ,fg2 :background ,bg3))))
   `(magit-diff-added ((,class (:background nil :foreground ,diffadd-fg))))
   `(magit-diff-added-highlight ((,class (:background nil :foreground ,diffadd-fg))))
   `(magit-diff-removed ((,class (:background nil :foreground ,diffdel-fg))))
   `(magit-diff-removed-highlight ((,class (:background nil :foreground ,diffdel-fg))))
   `(magit-diff-context ((,class (:background nil :foreground ,fg3))))
   `(magit-diff-context-highlight ((,class (:background nil :foreground ,fg1))))
   `(magit-diffstat-added   ((,class (:foreground ,type))))
   `(magit-diffstat-removed ((,class (:foreground ,var))))
   `(magit-process-ok ((,class (:foreground ,func :weight bold))))
   `(magit-process-ng ((,class (:foreground ,warning :weight bold))))
   `(magit-branch ((,class (:foreground ,const))))
   `(magit-branch-remote ((,class (:foreground ,keyword))))
   `(magit-branch-local ((,class (:foreground ,const))))
   `(magit-branch-current ((,class (:foreground ,func))))
   `(magit-branch-head ((,class (:foreground ,preproc))))
   `(magit-log-author ((,class (:foreground ,fg1))))
   `(magit-hash ((,class (:foreground ,fg2))))
   `(lazy-highlight ((,class (:foreground ,fg2 :background ,bg3))))
   `(rainbow-delimiters-unmatched-face ((,class :foreground ,warning)))
   `(helm-header ((,class (:foreground ,fg2 :background ,bg1 :underline nil :box nil))))
   `(helm-source-header ((,class (:foreground ,keyword :background ,bg1 :underline nil :weight bold))))
   `(helm-selection ((,class (:background ,bg2 :underline nil))))
   `(helm-selection-line ((,class (:background ,bg2))))
   `(helm-visible-mark ((,class (:foreground ,bg1 :background ,bg3))))
   `(helm-candidate-number ((,class (:foreground ,bg1 :background ,fg1))))
   `(helm-separator ((,class (:foreground ,type :background ,bg1))))
   `(helm-time-zone-current ((,class (:foreground ,builtin :background ,bg1))))
   `(helm-time-zone-home ((,class (:foreground ,type :background ,bg1))))
   `(helm-buffer-not-saved ((,class (:foreground ,type :background ,bg1))))
   `(helm-buffer-process ((,class (:foreground ,builtin :background ,bg1))))
   `(helm-buffer-saved-out ((,class (:foreground ,fg1 :background ,bg1))))
   `(helm-buffer-size ((,class (:foreground ,fg1 :background ,bg1))))
   `(helm-ff-directory ((,class (:foreground ,func :background ,bg1 :weight bold))))
   `(helm-ff-file ((,class (:foreground ,fg1 :background ,bg1 :weight normal))))
   `(helm-ff-executable ((,class (:foreground ,key2 :background ,bg1 :weight normal))))
   `(helm-ff-invalid-symlink ((,class (:foreground ,key3 :background ,bg1 :weight bold))))
   `(helm-ff-symlink ((,class (:foreground ,keyword :background ,bg1 :weight bold))))
   `(helm-ff-prefix ((,class (:foreground ,bg1 :background ,keyword :weight normal))))
   `(helm-grep-cmd-line ((,class (:foreground ,fg1 :background ,bg1))))
   `(helm-grep-file ((,class (:foreground ,fg1 :background ,bg1))))
   `(helm-grep-finish ((,class (:foreground ,fg2 :background ,bg1))))
   `(helm-grep-lineno ((,class (:foreground ,fg1 :background ,bg1))))
   `(helm-grep-match ((,class (:foreground nil :background nil :inherit helm-match))))
   `(helm-grep-running ((,class (:foreground ,func :background ,bg1))))
   `(helm-moccur-buffer ((,class (:foreground ,func :background ,bg1))))
   `(helm-source-go-package-godoc-description ((,class (:foreground ,str))))
   `(helm-bookmark-w3m ((,class (:foreground ,type))))
   `(company-echo-common ((,class (:foreground ,bg1 :background ,fg1))))
   `(company-preview ((,class (:background ,bg1 :foreground ,key2))))
   `(company-preview-common ((,class (:foreground ,bg2 :foreground ,fg3))))
   `(company-preview-search ((,class (:foreground ,type :background ,bg1))))
   `(company-scrollbar-bg ((,class (:background ,bg3))))
   `(company-scrollbar-fg ((,class (:foreground ,keyword))))
   `(company-tooltip ((,class (:foreground ,fg2 :background ,bg1))))
   `(company-tooltop-annotation ((,class (:foreground ,const))))
   `(company-tooltip-common ((,class ( :foreground ,fg3))))
   `(company-tooltip-common-selection ((,class (:foreground ,str))))
   `(company-tooltip-mouse ((,class (:inherit highlight))))
   `(company-tooltip-selection ((,class (:background ,bg3 :foreground ,fg3))))
   `(company-template-field ((,class (:inherit region))))
   `(web-mode-builtin-face ((,class (:inherit ,font-lock-builtin-face))))
   `(web-mode-comment-face ((,class (:inherit ,font-lock-comment-face))))
   `(web-mode-constant-face ((,class (:inherit ,font-lock-constant-face))))
   `(web-mode-keyword-face ((,class (:foreground ,keyword))))
   `(web-mode-doctype-face ((,class (:inherit ,font-lock-comment-face))))
   `(web-mode-function-name-face ((,class (:inherit ,font-lock-function-name-face))))
   `(web-mode-string-face ((,class (:foreground ,str))))
   `(web-mode-type-face ((,class (:inherit ,font-lock-type-face))))
   `(web-mode-html-attr-name-face ((,class (:foreground ,func))))
   `(web-mode-html-attr-value-face ((,class (:foreground ,keyword))))
   `(web-mode-warning-face ((,class (:inherit ,font-lock-warning-face))))
   `(web-mode-html-tag-face ((,class (:foreground ,builtin))))
   `(jde-java-font-lock-package-face ((t (:foreground ,var))))
   `(jde-java-font-lock-public-face ((t (:foreground ,keyword))))
   `(jde-java-font-lock-private-face ((t (:foreground ,keyword))))
   `(jde-java-font-lock-constant-face ((t (:foreground ,const))))
   `(jde-java-font-lock-modifier-face ((t (:foreground ,key3))))
   `(jde-jave-font-lock-protected-face ((t (:foreground ,keyword))))
   `(jde-java-font-lock-number-face ((t (:foreground ,var))))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(disable-theme 'solarized-light)
(disable-theme 'solarized-dark)
(provide-theme 'vim-aurora)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; vim-aurora-theme.el ends here
;;; --------------------------------------------------------------------------

  (set-transparent-bg)
  )

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
