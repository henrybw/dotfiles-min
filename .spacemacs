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
     python
     c-c++
     osx
     git
     markdown
     org
     cscope
     agda
     (shell :variables
            shell-default-shell 'ansi-term
            shell-default-height 30
            shell-default-position 'bottom)
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages
   '(
     undo-tree
     bison-mode
     ;; XXX HBW - local doesn't work for some reason...
     (vim-aurora :location "~/.emacs.d/private/local")
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
   dotspacemacs-line-numbers nil
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

  ;; This makes up/down work during isearch
  (setq-default evil-search-module 'evil-search)

  ;; irony-mode settings (disabled for now)
  (setq-default dotspacemacs-configuration-layers
                '((c-c++ :variables c-c++-enable-clang-support nil)))

  (setq tramp-ssh-controlmaster-options
        "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=no")

  ;; Yes, I set environment variables in my .zshrc. It's not good practice, but
  ;; you don't need to keep reminding me about that, emacs...
  (setq-default exec-path-from-shell-check-startup-files nil)
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

  ;; Spacemacs added a "check large file" hook when visiting files to back off
  ;; from trying to parse large files and slow down performance. However, this
  ;; change also introduced a mandatory prompt for these cases. This doesn't
  ;; play very nicely with automatically reloading tags databases, which can
  ;; often grow very large, resulting in constant and intrusive prompts to load
  ;; the tags database literally.
  ;;
  ;; This is a stupid hack, done using emacs's advice mechanism, to override
  ;; the spacemacs large file check and always open tags files literally without
  ;; prompting.
  (when (fboundp 'spacemacs/check-large-file)
    (advice-add 'spacemacs/check-large-file
                :before-while (lambda ()
                                (if (let ((case-fold-search nil))
                                      (string-match-p
                                       "TAGS"
                                       (file-name-base (buffer-file-name))))
                                    ;; Just open the tags file literally without
                                    ;; throwing up a prompt.
                                    (progn
                                      (setq buffer-read-only t)
                                      (buffer-disable-undo)
                                      (fundamental-mode)
                                      nil)
                                  ;; Not a tags file, run the original
                                  ;; check-large-file function as usual.
                                  t))))

  ;;;
  ;;; Key bindings
  ;;;

  ;; C-c as general purpose escape key sequence.
  ;; A HUGE THANK YOU TO: https://www.emacswiki.org/emacs/Evil#toc16
  (defun ctrl-c-escape (prompt)
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
     ;; Helm Projectile throws an error when using C-g to quit
     (projectile-mode [escape])
     ;; This is the best way I could infer for now to have C-c work during
     ;; evil-read-key. Note: As long as I return [escape] in normal-state, I
     ;; don't need this. eq overriding-terminal-local-map evil-read-key-map)
     ;; (keyboard-quit) (kbd ""))
     (t (kbd "C-g"))))
  (defun ctrl-g-major-mode (prompt)
    "Swaps the traditional emacs C-c major mode prefix key with C-g."
    (if (or (evil-insert-state-p) (evil-normal-state-p) (evil-replace-state-p)
            (evil-visual-state-p))
        (kbd "C-c")
      (kbd "C-g")))
  (define-key key-translation-map (kbd "C-g") 'ctrl-g-major-mode)
  (define-key key-translation-map (kbd "C-c") 'ctrl-c-escape)

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

  (evil-leader/set-key "s c"
    (defalias 'evil-clear-all-search-highlights
      (lambda ()
        (interactive)
        (evil-ex-nohighlight)
        (evil-search-highlight-persist-remove-all))))

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
      (lambda () (interactive) (evil-window-split-with-focus) (evil-jump-to-tag))))
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

  ;; Mimic the behavior of the "j" formatoption in vim (i.e. automatically
  ;; remove comment leaders when joining comment lines). Taken from:
  ;; https://bitbucket.org/lyro/evil/issues/606/joining-comment-lines#comment-25313052
  ;; http://emacs.stackexchange.com/questions/7519/how-can-i-detect-if-the-current-character-is-a-comment-character-for-that-major
  (evil-define-operator evil-join-comment-aware (beg end)
    "Join the selected lines."
    :motion evil-line

    (defun pull-up-line ()
      "Join the following line onto the current one (analogous to `C-e', `C-d') or
`C-u M-^' or `C-u M-x join-line'.

If the current line is a comment and the pulled-up line is also a comment,
remove the comment characters from that line."
      (interactive)
      (join-line -1)
      ;; If the current line is a comment
      (when (nth 4 (syntax-ppss))
        ;; Remove the comment prefix chars from the pulled-up line if present
        (save-excursion
          (forward-char)
          ;; Delete all comment-start or space characters
          (while (looking-at
                  (concat "\\s<" ; comment-start char as per syntax table
                          "\\|" (substring comment-start 0 1) ; first char of `comment-start'
                          (when (bound-and-true-p c-buffer-is-cc-mode)
                            "\\|\\*[^/]")  ; leading '*' chars for C comments
                          "\\|" "\\s-")) ; extra spaces
            (delete-forward-char 1)))))

    (let* ((count (count-lines beg end))
           ;; we join pairs at a time
           (count (if (> count 1) (1- count) count))
           ;; the mark at the middle of the joined pair of lines
           (fixup-mark (make-marker)))
      (dotimes (var count)
        (if (and (bolp) (eolp))
            (join-line 1)
          (let* ((end (line-beginning-position 3))
                 (fill-column (1+ (- end beg))))
            ;; save the mark at the middle of the pair
            (set-marker fixup-mark (line-end-position))
            ;; join it via pull-up
            (pull-up-line)
            ;; jump back to the middle
            (goto-char fixup-mark)
            (fixup-whitespace))))
      ;; remove the mark
      (set-marker fixup-mark nil)))
  (define-key evil-normal-state-map (kbd "J") 'evil-join-comment-aware)

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
    (define-key helm-map (kbd "C-b") 'helm-previous-page)
    ;; helm-follow-mode is sweet, and doesn't deserve to be obscured by C-c
    (define-key helm-map (kbd "C-s") 'helm-follow-mode))
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
  (unless (display-graphic-p (selected-frame))
    (setq-default linum-format 'dynamic))

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

  ;; Disable the built-in VC package, which gets picked up by the modeline,
  ;; because we use per-VCS tools instead (e.g. magit). Plus, at least for git,
  ;; this causes stale revision information to be displayed in the modeline.
  (setq-default vc-handled-backends nil)

  (show-paren-mode t)
  (setq-default spacemacs-show-trailing-whitespace t)
  (setq-default font-lock-maximum-decoration t)

  (setq-default helm-buffers-fuzzy-matching t)
  (setq-default helm-etags-fuzzy-match t)

  (global-linum-mode -1)
  (auto-fill-mode)

  ;;;
  ;;; Formatting
  ;;;

  (setq-default git-commit-summary-max-length 72)
  (setq-default comment-multi-line t)
  (setq-default truncate-lines t)  ; Turn off word wrap

  ;; Always re-read the tags file without prompting
  (setq-default tags-revert-without-query t)

  ;; Disable smart autocompletion of quotes
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
    "Highlighter function for cc-mode font-lock that highlights function calls using the
function name font face."
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
    (setq evil-shift-width 4)  ; FOR SRS
    (setq tab-width 4)
    (setq c-label-minimum-indentation 0)
    (setq fill-column 81)  ; So the fill column is drawn *after* the 80th column of text
    ;; Line up each line in a paren list with the position of the opening paren
    (c-set-offset 'arglist-close 'c-lineup-arglist)
    (c-set-offset 'innamespace 0)
    (c-set-offset 'access-label '-)   ; C++ access modifiers indented at same
                                      ; level as class
    (c-set-offset 'label (vector 0))  ; goto labels are always unindented
    (c-set-offset 'inextern-lang 0)   ; extern "C" is unindented
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
                            '(
                              ;; XXX HBW - this doesn't work reliably, and can get dropped in
                              ;; certain cases (like when undoing an edit inside a function name).
                              (c-mode-highlight-function-calls . font-lock-function-name-face)
                              ;; Sometimes operations like undo will confuse font-lock and the
                              ;; custom function name highlighting logic specified above.
                              ("\\(\\w+\\)\\s-*\(" (1 font-lock-function-name-face append))
                              (my-c-mode-font-lock-if0 (0 font-lock-comment-face prepend)))
                            'add-to-end)
    )

  (add-hook 'c-mode-common-hook 'c-mode-common-settings)

  (add-hook 'shell-mode-hook
            (lambda ()
              (linum-mode -1)
              (use-local-map (copy-keymap shell-mode-map))
              (local-set-key (kbd "C-g") 'comint-interrupt-subjob)
              (local-set-key (kbd "C-d") 'comint-delchar-or-maybe-eof)))

  (add-hook 'term-mode-hook
            (lambda ()
              (use-local-map (copy-keymap term-mode-map))
              (local-set-key (kbd "C-g") 'comint-interrupt-subjob)))

  ;; irony-mode stuff (disabled)
  ;; (global-flycheck-mode -1)
  ;; Tweak default flycheck face to be less offensive
  ;; (custom-set-faces
  ;;   '(default ((t (:background nil))))
  ;;   '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
  ;;   '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil))))
  ;;   '(flycheck-error ((((class color) (min-colors 89)) (:background nil :foreground "red" :bold t)))))

  (require 'vim-aurora)
  (enable-theme 'vim-aurora)
  (set-transparent-bg)
  )

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
