;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Henry Baba-Weiss"
      user-mail-address "henry.babaweiss@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))
(setq doom-font (font-spec :family "SF Mono" :size 12)
      doom-unicode-font (font-spec :family "SF Mono" :size 12))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'borealis)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type nil)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

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

(defun config:window-keys ()
  (defun evil-split-lookup-definition ()
    (interactive)
    (evil-window-split-with-focus)
    (+lookup/definition (thing-at-point 'symbol)))

  (defun evil-vsplit-lookup-definition ()
    (interactive)
    (evil-window-vsplit-with-focus)
    (+lookup/definition (thing-at-point 'symbol)))

  ;; Open tags in splits
  (define-key evil-window-map (kbd "]") #'evil-split-lookup-definition)
  (define-key evil-window-map (kbd "\\") #'evil-vsplit-lookup-definition)

  ;; Mimic vim's window-switching behavior and focus the new split
  (define-key evil-window-map (kbd "s") #'evil-window-split-with-focus)
  (define-key evil-window-map (kbd "v") #'evil-window-vsplit-with-focus))

(defun config:leader-keys ()
  (map! :leader :desc "Delete trailing whitespace"
        "RET" #'delete-trailing-whitespace)
  (map! :leader :desc "Toggle display of fill-column indicator"
        "\\" #'+fill-column/toggle)
  (map! :leader (:prefix-map ("s" . "search")
                 (:desc "Disable the active search highlightings"
                  "c" #'evil-ex-nohighlight)))
  (map! :leader (:prefix-map ("t" . "toggle")
                 (:desc "Highlight trailing whitespace"
                  "t" (lambda () (interactive)
                        (setq show-trailing-whitespace
                              (not show-trailing-whitespace)))))))

(defun config:motion-keys ()
  ;; I find just having zt and zb limiting; oftentimes, I'll want to put the
  ;; current piece of code I'm looking at in either top or bottom 'quadrant' of
  ;; the screen. Hence, the scrolling to 25% and 75% commands.
  (define-key evil-motion-state-map "zv"
    (defalias 'evil-scroll-line-to-25-pct
      (lambda () (interactive) (recenter (- (round (* (window-body-height)
                                                      0.25)))))))
  (define-key evil-motion-state-map "zr"
    (defalias 'evil-scroll-line-to-75-pct
      (lambda () (interactive) (recenter (- (round (* (window-body-height)
                                                      0.75)))))))
  ;; The normal map binding of 'zr' will override our motion map binding unless
  ;; we undefine it.
  (define-key evil-normal-state-map "zr" nil))

(defun config:insert-mode-keys ()
  (evil-define-key '(insert) 'global (kbd "TAB") #'tab-to-tab-stop))

(defun config:non-insert-mode-keys ()
  (defun join-comment-aware ()
    ;; Mimic the behavior of the "j" formatoption in vim (i.e. automatically
    ;; remove comment leaders when joining comment lines). Taken from:
    ;; https://bitbucket.org/lyro/evil/issues/606/joining-comment-lines#comment-25313052
    ;; http://emacs.stackexchange.com/questions/7519/how-can-i-detect-if-the-current-character-is-a-comment-character-for-that-major
    (evil-define-operator evil-join-comment-aware (beg end)
      "Join the selected lines."
      :motion evil-line

      (defun config:pull-up-line ()
        "Join the following line onto the current one (analogous to `C-e',`C-d')
or `C-u M-^' or `C-u M-x join-line'.

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
                            ;; first char of `comment-start'
                            "\\|" (substring comment-start 0 1)
                            (when (bound-and-true-p c-buffer-is-cc-mode)
                              "\\|\\*[^/]") ; leading '*' chars for C comments
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
              (config:pull-up-line)
              ;; jump back to the middle
              (goto-char fixup-mark)
              (fixup-whitespace))))
        ;; remove the mark
        (set-marker fixup-mark nil)))

    (evil-define-key '(normal visual operator) 'global
      (kbd "J") #'evil-join-comment-aware))

  ;; Vim-style tag popping
  (defun pop-tag ()
    (interactive)
    (pop-tag-mark))

  (join-comment-aware)
  (evil-define-key '(normal visual operator) 'global
    (kbd "C-]") #'+lookup/definition)
  (evil-define-key '(normal visual operator) 'global
    (kbd "C-t") #'pop-tag)
  (evil-define-key '(normal visual operator) 'global
    (kbd "C-q") #'evil-visual-block)
  (evil-define-key '(normal visual operator) 'global
    (kbd "C-l") #'redraw-display)
  (evil-define-key '(normal visual operator) 'global
    (kbd "C-p") #'projectile-find-file))

(defun config:tag-commands ()
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
  (evil-define-command evil-find-tag (tagname)
    "Jump to the tag TAGNAME."
    :repeat nil
    (interactive "<t>")
    (find-tag tagname))
  (evil-ex-define-cmd "ta[g]" #'evil-find-tag)

  (evil-define-command evil-prev-tag ()
    "Jump to the previous matching tag."
    :repeat nil
    (interactive)
    (find-tag "" '-))
  (evil-ex-define-cmd "tp[rev]" #'evil-prev-tag)
  (map! :leader :desc "Jump to the previous matching tag."
        "[" #'evil-prev-tag)

  (evil-define-command evil-next-tag ()
    "Jump to the next matching tag."
    :repeat nil
    (interactive)
    (find-tag "" t))
  (evil-ex-define-cmd "tn[ext]" #'evil-next-tag)
  (map! :leader :desc "Jump to the next matching tag."
        "]" #'evil-next-tag))

(defun config:c-mode ()
  (defun c-mode-highlight-function-calls (limit)
    "Highlighter function for cc-mode font-lock that highlights function calls
using the function name font face."
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
                        ;; Macro block constructs get incorrectly tagged as
                        ;; variable names
                        (eq face font-lock-variable-name-face))
                (c-put-font-lock-face start end 'font-lock-function-name-face)
                (setq retval t))))))
      retval))

  ;; https://stackoverflow.com/a/4554658
  (defun c-mode-font-lock-if0 (limit)
    (save-restriction
      (widen)
      (save-excursion
        (goto-char (point-min))
        (let ((depth 0) str start start-depth)
          (while (re-search-forward "^\\s-*#\\s-*\\(if\\|else\\|endif\\)" limit
                                    'move)
            (setq str (match-string 1))
            (if (string= str "if")
                (progn
                  (setq depth (1+ depth))
                  (when (and (null start) (looking-at "\\s-+0"))
                    (setq start (match-end 0)
                          start-depth depth)))
              (when (and start (= depth start-depth))
                (c-put-font-lock-face start (match-beginning 0)
                                      'font-lock-comment-face)
                (setq start nil))
              (when (string= str "endif")
                (setq depth (1- depth)))))
          (when (and start (> depth 0))
            (c-put-font-lock-face start (point) 'font-lock-comment-face)))))
    nil)

  ;; C/C++ formatting style should be K&R, but with 4-space indents
  (c-set-style "k&r")
  (setq c-default-style "k&r")
  (setq c-basic-offset 4)
  (setq evil-shift-width 4)  ; FOR SRS
  (setq tab-width 4)
  (setq c-label-minimum-indentation 0)
  (setq fill-column 80)
  ;; Line up each line in a paren list with the position of the opening paren
  (c-set-offset 'arglist-close 'c-lineup-arglist)
  (c-set-offset 'innamespace 0)
  ;; C++ access modifiers indented at same level as class
  (c-set-offset 'access-label '-)
  (c-set-offset 'label (vector 0))  ; goto labels are always unindented
  (c-set-offset 'inextern-lang 0)   ; extern "C" is unindented
  (setq comment-start "//" comment-end "")

  ;; Highlight function calls and #if 0 blocks
  (font-lock-add-keywords
   nil
   '((c-mode-highlight-function-calls (1 font-lock-function-name-face append))
     (c-mode-font-lock-if0 (0 font-lock-comment-face prepend)))
   'add-to-end))

(defun config:cmake-mode ()
  (setq tab-width 4) ; FOR SRS
  (setq cmake-tab-width 4))

(defun config:magit-mode ()
  (setq git-commit-summary-max-length 75)
  (setq git-commit-fill-column 75))

(config:window-keys)
(config:leader-keys)
(config:motion-keys)
(config:insert-mode-keys)
(config:non-insert-mode-keys)
(config:tag-commands)
(setq comment-multi-line t)

(add-hook 'c-mode-common-hook 'config:c-mode)
(add-hook 'cmake-mode-hook 'config:cmake-mode)
(add-hook 'magit-mode-hook 'config:magit-mode)
(add-to-list 'auto-mode-alist '("\\(\\.m\\'\\)" . objc-mode))
