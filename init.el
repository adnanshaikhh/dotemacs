(message "Booting Emacs up!")

;; Packages
;; Update package-archive lists
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))
(package-initialize)

;; Install 'use-package' if necessary
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Enable use-package
(eval-when-compile
  (require 'use-package))
(require 'diminish) ;; if you use :diminish
(require 'bind-key) ;; if you use any :bind variant

;;/////////////////////////////Appearance/////////////////////////////

(use-package gruvbox-theme
  :ensure t
  :defer t
  :init (load-theme 'gruvbox t)
  )

;(use-package zenburn-theme
;  :ensure t
;  :defer t
;  :init (load-theme 'zenburn t)
;  )

;(use-package atom-one-dark-theme
;  :ensure t
;  :defer t
;  :init (load-theme 'atom-one-dark t)
;  )

;;Set Window Title
(setq frame-title-format "emacs")

;; No Menu Bar
(menu-bar-mode -1)

;; No toolbar
 (when (functionp 'tool-bar-mode)
   (tool-bar-mode -1))

;;Highlight current line
(if (boundp 'global-hl-line-sticky-flag) ;introduced in emacs 24.1
    (setq global-hl-line-sticky-flag t))
(global-hl-line-mode 1)

(set-face-attribute 'font-lock-comment-face nil :slant 'italic)
(set-face-attribute 'font-lock-comment-face nil :weight 'semibold)

;;Point to last place where you previously where on the file

;;Emacs 24.5 and older versions
(require 'saveplace)
(setq-default save-place t)

;;Emacs 25.1 and newer versions
;(use-package saveplace
;  :init (save-place-mode))

 ;; Powerline config //git clone git://github.com/jonathanchu/emacs-powerline.git
(add-to-list 'load-path "~/.emacs.d/vendor/emacs-powerline")
(require 'powerline)

;; Set Powerline colors
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(mode-line ((t (:foreground "#030303" :background "#AFEC1F" :box nil))))
 '(mode-line-inactive ((t (:foreground "#f9f9f9" :background "#666666" :box nil)))))

;; Set Cursor to a bar
(setq-default cursor-type 'bar)

(require 'whitespace)
(setq-default show-trailing-whitespace t)

;;------------------------------------------------------------------------------
;; Global Config
;;------------------------------------------------------------------------------

;; Disable startup buffer
(setq inhibit-startup-screen t)

;; No message on sratch buffer
(setq initial-scratch-message nil)

;; Disable backup files
(setq make-backup-files nil)

;; Self explanatory
(fset 'yes-or-no-p 'y-or-n-p)

;; Set Autosave directory
(setq auto-save-file-name-transforms
  `((".*" "~/.emacs.d/emacs-autosaves/" t)))

;; Keep cursor at same position when scrolling
(setq scroll-preserve-screen-position t)

;; Instantly display current key sequence in mini buffer
(setq echo-keystrokes 0.02)

;; Scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(2 ((shift) . 2))) ;One line at a time
(setq mouse-wheel-progressive-speed nil) ;Don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;Scroll window under mouse
(setq scroll-step 1) ;Keyboard scroll one line at a time
(setq scroll-conservatively 10000)
(setq scroll-margin 0)

;; No Word Wrap
(setq-default truncate-lines 1)

;; Set encoding
(prefer-coding-system 'utf-8)

;; UTF-8 is the future
(define-coding-system-alias 'UTF-8 'utf-8)
(define-coding-system-alias 'utf8 'utf-8)

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; Delete selection, insert text
(delete-selection-mode 1)

;; Reload files when they change on disk.
(global-auto-revert-mode 1)

;; Highlight matching parentheses when the point is on them.
(show-paren-mode 1)

;;Remove Delay
(setq show-paren-delay 0.01)

;;Show offscreen parenthesis in minibuffer
(defadvice show-paren-function
    (after show-matching-paren-offscreen activate)
  "If the matching paren is offscreen, show the matching line in the
        echo area. Has no effect if the character before point is not of
        the syntax class ')'."
  (interactive)
  (let* ((cb (char-before (point)))
	 (matching-text (and cb
			     (char-equal (char-syntax cb) ?\) )
			     (blink-matching-open))))
    (when matching-text (message matching-text))))

;; Auto scroll compilation buffer.
(setq compilation-scroll-output 't)

;;------------------------------------------------------------------------------
;; Packages
;;------------------------------------------------------------------------------

;; Helm
(use-package helm
  :ensure t
  :bind (("M-x" . helm-M-x)
	 ("C-x b" . helm-mini)
	 ("C-x C-f" . helm-find-files))
  :init
  (require 'helm-config)
  (setq helm-mode-fuzzy-match t))

;; Replace default isearch
(use-package helm-swoop
  :ensure t
  :bind (("C-c C-SPC" . helm-swoop-without-pre-input)
         ("C-c o" . helm-multi-swoop-all)
         ("C-s"   . helm-swoop-without-pre-input)
         ("C-r"   . helm-resume)))

(require 'linum)
(set-face-attribute 'linum nil
                    :background (face-attribute 'default :background)
                    :foreground (face-attribute 'font-lock-comment-face :foreground)
		    :height 125)
(defface linum-current-line-face
  `((t :background "gray30" :foreground "gold" :height 125))
  "Face for the currently active Line number")
(defvar my-linum-current-line-number 0)
(setq my-linum-format-string " %d ")
(defun my-linum-format (line-number)
  (propertize (format my-linum-format-string line-number) 'face
              (if (eq line-number my-linum-current-line-number)
                  'linum-current-line-face
                'linum)))
(setq linum-format 'my-linum-format)
(defadvice linum-update (around my-linum-update)
  (let ((my-linum-current-line-number (line-number-at-pos)))
    ad-do-it))
(ad-activate 'linum-update)

(global-linum-mode 1)

;(set-face-attribute 'linum nil :height 125)
;;------------------------------------------------------------------------------
;; Key Bindings and Defuns
;;------------------------------------------------------------------------------

(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

(global-set-key [remap move-beginning-of-line]
                'smarter-move-beginning-of-line)

;; Try to fix left and right touchpad scroll issues
(global-set-key (kbd "<mouse-7>") '(lambda ()
                                     (interactive)
                                     (scroll-left 4)))
(global-set-key (kbd "<mouse-6>") '(lambda ()
                                     (interactive)
                                     (scroll-right 4)))

(defun move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

(global-set-key [(meta up)]  'move-line-up)
(global-set-key [(meta down)]  'move-line-down)

(defun line-at-click ()
  (save-excursion
    (let ((click-y (cdr (cdr (mouse-position))))
          (line-move-visual-store line-move-visual))
      (setq line-move-visual t)
      (goto-char (window-start))
      (next-line (1- click-y))
      (setq line-move-visual line-move-visual-store)
      ;; If you are using tabbar substitute the next line with
      ;; (line-number-at-pos))))
      (1+ (line-number-at-pos)))))

(defun md-select-linum ()
  (interactive)
  (goto-line (line-at-click)))

(global-set-key (kbd "<left-margin> <down-mouse-1>") 'md-select-linum)
(global-set-key (kbd "<left-margin> <mouse-1>") 'md-select-linum)
(global-set-key (kbd "<left-margin> <drag-mouse-1>") 'md-select-linum)

(defun duplicate-line (arg)
  "Duplicate current line, leaving point in lower line."
  (interactive "*p")

  ;; save the point for undo
  (setq buffer-undo-list (cons (point) buffer-undo-list))

  ;; local variables for start and end of line
  (let ((bol (save-excursion (beginning-of-line) (point)))
        eol)
    (save-excursion

      ;; don't use forward-line for this, because you would have
      ;; to check whether you are at the end of the buffer
      (end-of-line)
      (setq eol (point))

      ;; store the line and disable the recording of undo information
      (let ((line (buffer-substring bol eol))
            (buffer-undo-list t)
            (count arg))
        ;; insert the line arg times
        (while (> count 0)
          (newline)         ;; because there is no newline in 'line'
          (insert line)
          (setq count (1- count)))
        )

      ;; create the undo information
      (setq buffer-undo-list (cons (cons eol (point)) buffer-undo-list)))
    ) ; end-of-let

  ;; put the point in the lowest line and return
  (next-line arg))

(global-set-key (kbd "C-c d") 'duplicate-line)



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (helm-swoop helm gruvbox-theme use-package))))
