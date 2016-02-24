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

(use-package atom-one-dark-theme
  :ensure t
  :defer t
  :init (load-theme 'atom-one-dark t)
  )

;; No Menu Bar
(menu-bar-mode -1)

;; No toolbar
 (when (functionp 'tool-bar-mode)
   (tool-bar-mode -1))

;; Powerline config //git clone git://github.com/jonathanchu/emacs-powerline.git
(add-to-list 'load-path "~/.emacs.d/vendor/emacs-powerline")
(require 'powerline)

;; Set Powerline colors
(custom-set-faces
 '(mode-line ((t (:foreground "#030303" :background "#AFEC1F" :box nil))))
 '(mode-line-inactive ((t (:foreground "#f9f9f9" :background "#666666" :box nil)))))

;; Set Cursor to a bar
 (setq-default cursor-type 'bar)

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

;; Auto scroll compilation buffer.
(setq compilation-scroll-output 't)

;;------------------------------------------------------------------------------
;; Packages
;;------------------------------------------------------------------------------

;; Enable company mode
(use-package company
  :ensure t
  :bind ("C-;" . company-complete-common)
  :defer 1
  :init (global-company-mode))

;; Documentation popups
(use-package company-quickhelp
  :ensure t
  :defer 1
  :init (company-quickhelp-mode 1))

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
  :bind (("C-c C-SPC" . helm-swoop)
         ("C-c o" . helm-multi-swoop-all)
         ("C-s"   . helm-swoop)
         ("C-r"   . helm-resume)))

;; Show pretty line numbers
(require 'linum)
(global-linum-mode 1)

;; Make pretty line numbers 10x prettier
(setq linum-format
      (lambda (line) (propertize
		      (format (let ((w (length (number-to-string (count-lines (point-min) (point-max))))))
				(concat " %" (number-to-string w) "d ")) line) 'face 'linum)))
(custom-set-faces '(linum ((t (:foreground "white" :background "#353B45" :box nil)))) )
(set-face-attribute 'linum nil :height 125)

;; Live Syntax Checking 
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

;(use-package flycheck
;  :ensure t
;  :commands global-flycheck-mode
;  :init (global-flycheck-mode)
;  :config (progn
;            (setq flycheck-check-syntax-automatically '(save mode-enabled))
;            (setq flycheck-standard-error-navigation nil)
;            ;; flycheck errors on a tooltip (doesnt work on console)
;            (when (display-graphic-p (selected-frame))
;              (eval-after-load 'flycheck
;                '(custom-set-variables
;                  '(flycheck-display-errors-function #'flycheck-pos-tip-error-messages)))
;              )))

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
