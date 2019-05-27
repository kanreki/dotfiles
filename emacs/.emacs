
;(require 'package)
(package-initialize)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
;; (unless package-archive-contents
;;   (package-refresh-contents)
;;   (package-install 'use-package))
;; (require 'use-package)

(let ((local-settings-file (expand-file-name "site-settings" "~"))
      (missing-ok t))
  (load local-settings-file missing-ok))

;;
;; simple editing functions that I find useful
;;

(defun bfn (&optional arg)
  "Display and copy the name of the file being visited.
With prefix arg, refrain from putting it in the kill ring."
  (interactive "P")
  (let ((n (buffer-file-name)))
    (if (null n)
        (message "(no file)")
      (or arg
          (kill-new n))
      (message "%s" n))))

;; (autoload 'visit-imported "java" 
;;   "Warp to Java class source (or package dir) named on current import line" t)
;; (autoload 'visit-exception-source "java"
;;   "Visit source for Java class mentioned in a stack trace." t)
;; (autoload 'visit-java-source "java"
;;   "Visit the source code file of Java class CLASS-NAME." t)
;; (autoload 'printf "c"
;;   "Add a debugging print statement to C code." t)
;; (autoload 'insert-cygwinized-file-name "misc")
;; (autoload 'my-local-vc-diff "misc"
;;   "Run Ediff between base and working copy of Subversion file.")
;; (autoload 'macro-cont-pp "c"
;;   "Prettify line continuation backslashes for a C #define.")
;; (autoload 'display-db-code-for-mail "misc"
;;   "Decorate a chunk of DB code for display in a mail message.")
(autoload 'ledger-mode "ledger")

;; (global-set-key "\C-cf" 'visit-imported)
;; (global-set-key "\C-ce" 'visit-exception-source)
;; (global-set-key "\C-cj" 'visit-java-source)
;; (global-set-key "\C-cy" 'insert-cygwinized-file-name)
;; (global-set-key "\C-cd" 'my-local-vc-diff)
;; (global-set-key [f9] 'printf)

(defun mydel () 
  (interactive)
  (if (and mark-active
	   (< (region-beginning) (region-end)))
      (delete-region (region-beginning) (region-end))
    (delete-char 1)))
	
(defun sfn (newName)
  (interactive "sNew frame name: \n")
  (modify-frame-parameters nil (list (cons 'name newName))))


;;
;; key bindings
;;
(global-set-key [f1] 'bfn)
(global-set-key [S-f7] "\C-u1\C-v")     ; (scroll-up 1)
(global-set-key [S-f6] "\C-u1\M-v")     ; (scroll-down 1)
(global-set-key [f7] "\C-u8\C-v")
(global-set-key [f6] "\C-u8\M-v")

;;
;;   2. for functions that don't usually have any key bindings
;;
(global-set-key [C-delete] 'delete-region)
(global-set-key [f4] 'replace-regexp)
(global-set-key [C-f4] 'query-replace-regexp)
(global-set-key [C-S-f4] 'query-replace)
(global-set-key [S-f4] 'replace-string)
(global-set-key [f8] 'auto-fill-mode)
(global-set-key "\C-x\C-b" 'buffer-menu)

;;
;;   3. standard PC keyboard
;;
(global-set-key [S-prior] 'beginning-of-buffer)
(global-set-key [S-next] 'end-of-buffer)
(global-set-key [f14] 'undo)
(global-set-key [f16] 'kill-ring-save)
(global-set-key [f18] 'yank)
(global-set-key [f20] 'kill-region)
(global-set-key [f31] 'recenter)
(global-set-key [home] 'beginning-of-line)
(global-set-key [end] 'end-of-line)
(global-set-key [f29] 'scroll-down)
(global-set-key [f35] 'scroll-up)


;;
;; configuration variables and other settings
;;
(if (fboundp 'resize-minibuffer-mode)
    (resize-minibuffer-mode 1))
(auto-compression-mode 1)

(setq holidays-in-diary-buffer nil)

(setq scroll-conservatively 9999)       ; scroll just a single line at a time
(setq dabbrev-case-replace nil)
(setq kill-whole-line t)
(setq next-line-add-newlines nil)
(setq transient-mark-mode t)
(setq highlight-nonselected-windows nil)
(setq suggest-key-bindings nil)
(setq find-file-visit-truename t)
(setq find-file-wildcards nil)

(setq dired-listing-switches "-ltn")

;;
;; I like to make sure all my own files are clean, in that they end
;; with a nice newline.  But it's more important not to make
;; gratuitous changes to files versioned in a repository.
;;
(setq require-final-newline t)
;; (add-hook 'find-file-hooks
;;           '(lambda()
;;              (when (vc-backend (buffer-file-name)) ;is this file managed by version control?
;;                (make-local-variable 'require-final-newline)
;;                (setq require-final-newline nil))))

(setq vc-handled-backends nil)          ; it's too slow!

(add-hook
 'go-mode-hook
 #'(lambda() (setq tab-width 4)))

;; customizations for all of c-mode, c++-mode, objc-mode, java-mode
;; 
(defun fix-c-syntax-table() 
  "Include underscore as word syntax."
  (modify-syntax-entry ?_ "w"))
(add-hook
 'c-mode-common-hook
 '(lambda()
    (c-set-offset 'substatement-open 0)
    ;;  (c-set-offset (quote knr-argdecl-intro) 0 nil)
    (setq c-basic-offset 4)
    
    (setq c-hanging-braces-alist 
          '((brace-list-open) 
            (substatement-open before after) 
            (block-close . c-snug-do-while)))
    
    ;;(set-variable 'tab-width 4)	; this was a Cloudscape convention
    (fix-c-syntax-table)
    
    (local-set-key "\C-c\\" 'macro-cont-pp)
    ;;
    ;; I haven't yet figured out why, but it seems that on my
    ;; Windows box the c-context-line-break doesn't exist.  I guess
    ;; it's an older version of c-mode.  Instead it has to be
    ;; 'newline-and-indent.
    ;;
    (define-key c-mode-base-map "\C-m" 'c-context-line-break)))

(add-hook 
 'java-mode-hook
 '(lambda()
    (c-toggle-electric-state -1)

    ;; After something like @Override, the continuation of a method
    ;; declaration should not be indented:
    ;; 
    (c-set-offset 'topmost-intro-cont 0)

    ;; Align continuation of assignment statement after the '=' sign,
    ;; but for other statement types just one basic indent.
    ;; 
    (c-set-offset 'statement-cont '(first c-lineup-assignments +))))

(put 'narrow-to-region 'disabled nil)
(put 'eval-expression 'disabled nil)

;; I never use these, and I hate it when I accidentally type them
(put 'set-goal-column 'disabled t)
(global-set-key "\C-xf" 'find-file)  ; usually set-fill-column
(global-set-key "\C-x\C-d" 'dired)     ; usually list-directory

(add-hook 'text-mode-hook
          #'(lambda()
              (turn-on-auto-fill)))

(add-hook 'ledger-mode-hook 
          (lambda()
            (auto-fill-mode -1)
            (let ((load-path (cons nil load-path)))
              (require 'bills)
              (load-bills)
              (local-set-key "\C-cb" 'view-bills))))

;; 
;; insert a "+" before the ">" that (I hope, still) occurs in the
;; adaptive fill regexp.  The regexp currently looks like this:
;;
;;     "[ 	]*\\([-|#;>*]+[ 	]*\\|(?[0-9]+[.)][ 	]*\\)*"
;;
;; but I thought it might make things more resilient to change to do
;; it this way.
;;
(require 'cl)
(let ((p (position ?> adaptive-fill-regexp)))
  (setq adaptive-fill-regexp (concat (substring adaptive-fill-regexp 0 p)
                                     "+"
                                     (substring adaptive-fill-regexp p))))
        


;;
;; Add some things to the Emacs auto-mode-alist.  First define a
;; function to add one entry onto the list; then call it as many times
;; as necessary.  This is better than before, because it limits the
;; codification of how it's done to a single place.  (The DRY
;; principle.)
;;

(defun register-mode-suffix(suffix mode)
  (let ((regexp (format "\\.%s\\'" suffix)))
    (push (cons regexp mode) auto-mode-alist)))

;; (register-mode-suffix "[ar]html" 'html-mode)
;; (register-mode-suffix "rbx" 'ruby-mode)
;; (register-mode-suffix "[rwe]ar" 'archive-mode)
(register-mode-suffix "t" 'indented-text-mode)
;; (register-mode-suffix "cs" 'java-mode)  ; for Microsoft.NET C#
;; (register-mode-suffix "pl" 'perl-mode)



;;;
;;; generic remove (all occurrences of) something from a list
;;;
(defun remove-from-list(the-list a-function user-data)
  (let (p			; pointer to current element in list
	(anchor the-list)	; previous element in list, so we can delete
	ele)			; element in the current cons cell
    
    (while (setq p (cdr anchor))
      (setq ele (car p))
      (and (funcall a-function ele user-data)
	   (setcdr anchor (cdr p)))
      (setq anchor (cdr anchor)))))

(setq completion-auto-help nil)
(setq completion-ignored-extensions (remove "log" completion-ignored-extensions))

(when window-system

  ;; I have my own grand plans for these keys
  (defun keysym-comp(cons-cell key-sym)
    (eq (car cons-cell) key-sym))
  (remove-from-list function-key-map 'keysym-comp 'delete)
  (remove-from-list key-translation-map 'keysym-comp 'f1)
  
  (global-set-key [delete] 'mydel)
  (global-set-key [S-return] 'execute-extended-command)
  
  (tool-bar-mode 0)
  ;; the following is obsolete, but it shows how you can add an entry 
  ;; to the (front of the) function key map
  ;; (setq function-key-map (cons 'keymap (cons '(f11 . [7]) 
  ;;					 (cdr function-key-map))))
  )

(when (>= emacs-major-version 21)
  (blink-cursor-mode 0)
  (menu-bar-mode 0))

(defun my-shell-paster(the-arg)
  (interactive "e")
  (let ((w (car (car (cdr the-arg)))))
    (select-window w)
    (goto-char (point-max))
    (yank)))
(require 'disp-table)
(add-hook 'lisp-mode-hook
	  (function (lambda ()
		      (modify-syntax-entry ?- "w"))))
(defun pin-buffer()
  "Require confirmation to kill current buffer.
This makes it harder to lose an important buffer accidentally."
  (interactive)
  (make-local-variable 'kill-buffer-query-functions)
  (setq kill-buffer-query-functions
      (list (function
             (lambda()
             (yes-or-no-p "Really kill this buffer? "))))))
(defun show-shell-timestamp ()
  (interactive)
  (let ((ts (get-text-property (point) 'my-timestamp)))
    (when ts (message "%s" (current-time-string ts)))))
(add-hook 'shell-mode-hook
	  (function (lambda ()
		      (local-set-key [f27] 'comint-bol)
		      (local-set-key [mouse-2] (quote my-shell-paster))
                      (local-set-key (kbd "C-c h") #'show-shell-timestamp)
		      (pin-buffer)
		      (setq mode-line-format 
			    '("" mode-line-modified
			      mode-line-buffer-identification "  " 
			      default-directory "   " global-mode-string 
			      "   %[(" mode-name mode-line-process 
			      minor-mode-alist "%n" ")%]--" 
			      (line-number-mode "L%l--") 
			      (column-number-mode "C%c--") (-3 . "%p") "-%-"))
		      (setq comint-scroll-to-bottom-on-input 'this)
                      (setq comint-scroll-show-maximum-output nil)
                      (add-hook 'comint-preoutput-filter-functions
                                #'(lambda (s)
                                    (put-text-property 0 (length s) 'my-timestamp (current-time) s)
                                    s))

		      ;; undo the stupid effects of winnt.el, to get forward slashes
		      (setq comint-completion-addsuffix t)))
	  t				; APPEND
	  )
(add-hook 'gdb-mode-hook
	  (function (lambda ()
		      (make-local-variable 'kill-buffer-query-functions)
		      (setq kill-buffer-query-functions
			    (list (function
				   (lambda()
				     (yes-or-no-p "Really kill this buffer? ")))))
                      (setq comint-scroll-show-maximum-output nil)
		      (local-set-key [mouse-2] (quote my-shell-paster)))))
;;; consider using (get-buffer-process (current-buffer)) to prevent
;;; accidental deletion of GDB buffers that are still active
;;; hey, maybe kill-buffer-query-functions is better!

;;; when loading dired, load dired-x too, and set a configuration
(add-hook 'dired-load-hook 
	  (function (lambda () 
                      (load "dired-x")
                      (add-to-list 'dired-omit-extensions ".svn-base")
                      (setq dired-omit-files (concat dired-omit-files "\\|^CVS$")))))

(setq lisp-interaction-mode-hook lisp-mode-hook)
(setq emacs-lisp-mode-hook lisp-mode-hook)

;;; if user has specified an initial Emacs configuration via an environment 
;;; variable, check the appropriate initialization file.  If it really exists, 
;;; load it.
(and (setq cfg (getenv "EMACS_CFG"))
     (let ((fn (expand-file-name cfg (expand-file-name "~/emacs_cfg"))))
       (and (file-readable-p fn)
	    (load-file fn))))

(setq load-path (append (list "~/lisp") load-path))
(add-to-list 'load-path "~/lisp")
(add-to-list 'load-path "/usr/local/go/misc/emacs" t)
;(require 'go-mode-load)

(add-to-list 'display-buffer-alist
     '("^\\*shell\\*$" . (display-buffer-same-window)))

;;
;; do system-dependent stuff, including font preference
;;
(case system-type
  ('windows-nt
   (setq my-font-choice "-*-Fixedsys-normal-r-*-*-16-120-96-96-c-*-iso8859-1")
   (setq Man-switches "")
   (setq ediff-diff3-options "")        ; (?)
   (setq archive-zip-use-pkzip nil)
   
   ;; setting shell-file-name to "bash" makes find-dired and friends work
   ;; on Windows.
   (setq shell-file-name "bash")
   
   (setq ps-printer-name "\\\\sleeper\\DellMFP")

   (defun get-cyg-path(p) 
     (shell-command-to-string
      (format "cygpath -m %s | tr -d '\\n'" p)))
   (mapcar #'(lambda (path)
               (push (get-cyg-path path) Info-default-directory-list))
           '("/usr/share/info" "/usr/info" "/usr/local/info"))

   (defvar *gnuserv-port* 21490         ; TODO pretty sure gnuserv is obsolete
     "The TCP port on which gnuserv listens.")
   (condition-case v
       ;; See if we can find an already running gnuserv.  Only if we
       ;; can't do we start one of our own.
       ;; 
       (let ((p (open-network-stream "probe" nil "localhost" *gnuserv-port*)))
         (delete-process p))
     (file-error 
      (let ((err (second v))
            (reason (third v)))
        (if (and (equal err "connection failed")
                 (equal reason "connection refused"))
            (progn
              (load-library "gnuserv")
              (gnuserv-start)
              (setq gnuserv-frame (selected-frame)))
          (error "Probe for running gnuserv: %s" (error-message-string v)))))))

  ('darwin
   (setq visible-bell t)
   (push "/opt/local/share/info" Info-default-directory-list)
   ;; If I'm coming in via SSH, it's probably from Linux, so choose
   ;; font that looks best on that display.  TODO: actually take a
   ;; look at the value of SSH_CLIENT and try to figure out where
   ;; we're coming from, and/or ... is there a way to pass an env
   ;; variable in from the ssh command starting at the originating
   ;; side, I wonder?
   ;; 
   (setq my-font-choice
         (if (getenv "SSH_CLIENT")
             "-b&h-lucidatypewriter-medium-r-normal-sans-12-120-75-*-*-*-*-*"
           "lucidasanstypewriter-12"))
   (server-start)
   (setq dired-use-ls-dired nil))
  ('gnu/linux
   (setq printer-name "rlp")
   (setq my-font-choice "-b&h-lucidatypewriter-medium-r-normal-sans-12-120-75-*-*-*-*-*")

   ;; On Debian Linux, man switches "-7" avoids some weirdness with some
   ;; kind of fancy hyphens -- I guess they're "soft hyphens" or
   ;; something, according to some comments I skimmed over in man.el on
   ;; Win32 Emacs version 21.3.  But it totally breaks the "man" command
   ;; on Win32!
   ;; 
   (setq Man-switches "-7")
   ;; (server-start)
   ))
   
(sfn "Emacs")

(setq explicit-shell-file-name "bash")
(setq comint-input-ring-size 100)       ;was 32, let's be more generous :-)
(shell)
(rename-buffer "sh")
;(if (fboundp 'comint-watch-for-password-prompt)
;    (add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt))

;(require 'tex-site)
(setq tex-default-mode 'latex-mode)

;(prefer-coding-system (quote sjis))
;(setq sendmail-coding-system (quote sjis))
;(register-input-method
; "japanese" "Japanese" 'quail-use-package
; "Aあ" "Romaji -> Hiragana -> Kanji&Kana"
; "quail/japanese")

(setq Man-notify-method 'bully)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(comint-highlight-input ((t (:weight normal)))))

(setq-default indent-tabs-mode nil)

(setq compile-command "cd ../build_unix;make  ")
(global-set-key "\C-cn" 'simple-record)
(global-set-key "\C-csc" 'cscope-find-functions-calling-this-function)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(line-move-visual nil)
 '(org-agenda-files (quote ("~/plan.org")))
 '(package-selected-packages (quote (magit scala-mode ensime)))
 '(safe-local-variable-values (quote ((tab-stop-list 16 42)))))
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)
   (shell . t)))
(require 'org)
(push 'org-id org-modules)

(setq inhibit-startup-screen t)

;; New feature introduced in Emacs 24.4; but my bills mode hack
;; relies on "ledger" file buffer not having its name changed.  Turn
;; this off until I fix that.
(setq uniquify-buffer-name-style nil)
