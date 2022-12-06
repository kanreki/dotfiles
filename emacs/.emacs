(add-to-list 'load-path "~/lisp")
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)

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
(global-set-key [f7] (lambda () (interactive) (scroll-up 8)))
(global-set-key [f6] (lambda () (interactive) (scroll-down 8)))
(global-set-key [S-f7] 'scroll-up-line)
(global-set-key [S-f6] 'scroll-down-line)

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

(setq scroll-conservatively 9999)       ; scroll just a single line at a time
(setq dabbrev-case-replace nil)
(setq kill-whole-line t)
(setq suggest-key-bindings nil)
(setq find-file-visit-truename t)
(setq find-file-wildcards nil)

(setq dired-listing-switches "-ltn")

;; TODO: reconsider this in the case of contributing work to a body of
;; source code that I don't own (so as to avoid introducing gratuitous
;; noise to diffs); something like what I used to do here, but perhaps
;; more clever.
(setq require-final-newline t)

(setq vc-handled-backends nil)          ; it's too slow!
;; TODO: ... is it though?  Magit recommends leaving it on.

(add-hook 'go-mode-hook (lambda() (setq tab-width 2)))

;; customizations for all of c-mode, c++-mode, objc-mode, java-mode
;; 
(defun fix-c-syntax-table() 
  "Include underscore as word syntax."
  (modify-syntax-entry ?_ "w"))
(add-hook
 'c-mode-common-hook
 (lambda()
   (c-set-offset 'substatement-open 0)
   (setq c-basic-offset 4)

   (setq c-hanging-braces-alist
         '((brace-list-open)
           (substatement-open before after)
           (block-close . c-snug-do-while)))

   (fix-c-syntax-table)

   ;;
   ;; I haven't yet figured out why, but it seems that on my
   ;; Windows box the c-context-line-break doesn't exist.  I guess
   ;; it's an older version of c-mode.  Instead it has to be
   ;; 'newline-and-indent.
   ;;
   (define-key c-mode-base-map "\C-m" 'c-context-line-break)))

(add-hook 
 'java-mode-hook
 (lambda()
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

(add-hook 'text-mode-hook #'turn-on-auto-fill)

(require 'my-al nil t)
(add-hook 'ledger-mode-hook 
          (lambda()
            (setq tab-width 4)
            (setq indent-tabs-mode t)
	    (modify-syntax-entry ?. "w")
	    (modify-syntax-entry ?/ "w")
	    (modify-syntax-entry ?$ ".")
            ;; TODO: could this be defined as a file-local variable, and marked as safe,
            ;; possibly even using defcustom?  That would make it possible for different
            ;; files to have different masters (though I've never needed such flexibility).
            (setq-local ledger-master-file "inc")
            (turn-off-auto-fill)
            (local-set-key "\C-cb" #'init-bills)))

(add-hook 'ledger-reconcile-mode-hook (lambda() (local-set-key "\C-cv" #'reconc-visa)))

;; TODO: review the latest techniques for controlling adaptive filling, and
;; reconsider whether/why I at one point thought that "+" ought to be included
;; in the list of "bullet" characters.

;; Not used much lately, but I used to like to indicate a text file by
;; a *.t suffix.
;; 
(add-to-list 'auto-mode-alist '("\\.t\\'" . indented-text-mode))

;;;
;;; generic remove (all occurrences of) something from a list
;;;
;;; TODO: WTF was I doing with these translation maps; and in any case
;;; why didn't I use CL-REMOVE (from the CL package)?
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

(when (display-graphic-p)

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

(blink-cursor-mode 0)
(menu-bar-mode 0)

(defun my-shell-paster(the-arg)
  (interactive "e")
  (let ((w (car (car (cdr the-arg)))))
    (select-window w)
    (goto-char (point-max))
    (yank)))
(require 'disp-table)
(add-hook 'lisp-mode-hook (lambda () (modify-syntax-entry ?- "w")))
(defun pin-buffer()
  "Require confirmation to kill current buffer.
This makes it harder to lose an important buffer accidentally."
  (interactive)
  (make-local-variable 'kill-buffer-query-functions)
  (setq kill-buffer-query-functions
        (list (lambda()
                (yes-or-no-p "Really kill this buffer? ")))))

(defun my-asker (prompt)
  (let* ((prog-name "ssh-askpass")
         (exec-file (executable-find prog-name)))
    (if (or (null exec-file) (null (file-executable-p exec-file)))
        nil
      (let ((fname (make-temp-file "askpass")))
        (unwind-protect
            (with-temp-buffer
              (let ((ret (call-process prog-name nil (list (current-buffer) fname) nil prompt)))
                (if (zerop ret)
                    (string-trim-right (buffer-string) "[\n]")
                  (insert-file-contents fname)
                  (let ((msg (string-trim-right (buffer-string) "[\n]")))
                    (when (zerop (length msg))
                      (setq msg "(empty stderr)"))
                    (error "the \"%s\" utility failed to get a password: %s" prog-name msg)))))
          (delete-file fname))))))

(defun show-shell-timestamp ()
  (interactive)
  (let ((ts (get-text-property (point) 'my-timestamp)))
    (when ts (message "%s" (current-time-string ts)))))
(add-hook 'shell-mode-hook
	  (lambda ()
	    (local-set-key [f27] #'comint-bol)
	    (local-set-key [mouse-2] #'my-shell-paster)
            (local-set-key (kbd "C-c h") #'show-shell-timestamp)
            ;; is this really needed?  Emacs already does something
            ;; like this, at least if the shell is still running.
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
            (setq comint-password-function #'my-asker)
            (add-hook 'comint-preoutput-filter-functions
                      (lambda (s)
                        (put-text-property 0 (length s) 'my-timestamp (current-time) s)
                        s))

	    ;; undo the stupid effects of winnt.el, to get forward slashes
	    (setq comint-completion-addsuffix t))
	  t				; APPEND
	  )
(add-hook 'gdb-mode-hook
	  (lambda ()
            (pin-buffer)
            (setq comint-scroll-show-maximum-output nil)
	    (local-set-key [mouse-2] #'my-shell-paster)))
;;; consider using (get-buffer-process (current-buffer)) to prevent
;;; accidental deletion of GDB buffers that are still active
;;; hey, maybe kill-buffer-query-functions is better!

(add-hook 'dired-load-hook (lambda () (load "dired-x")))

(setq lisp-interaction-mode-hook lisp-mode-hook)
(setq emacs-lisp-mode-hook lisp-mode-hook)

(add-to-list 'display-buffer-alist
             '("^\\*shell\\*$" . (display-buffer-same-window)))

;;
;; do system-dependent stuff, including font preference
;;
(cl-case system-type
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
   (setq my-font-choice "Noto Mono-11")

   ;; On Debian Linux, man switches "-7" avoids some weirdness with some
   ;; kind of fancy hyphens -- I guess they're "soft hyphens" or
   ;; something, according to some comments I skimmed over in man.el on
   ;; Win32 Emacs version 21.3.  But it totally breaks the "man" command
   ;; on Win32!
   ;; 
   (setq Man-switches "-7")
   (server-start)
   ))

(sfn "Emacs")

(let ((my-frame-attributes `((background-color . "midnight blue")
			     (foreground-color . "wheat")
                             (font . ,my-font-choice)
			     (cursor-color . "yellow"))))
  (mapcar (lambda (cons)
	    (add-to-list 'default-frame-alist cons))
	  my-frame-attributes))

(setq comint-input-ring-size 100)       ;was 32, let's be more generous :-)
(shell)
(rename-buffer "sh")

(setq Man-notify-method 'bully)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(comint-highlight-input ((t (:weight normal))))
 '(minibuffer-prompt ((t (:foreground "wheat")))))
 ;TODO: only in non-X (terminal) window?

(setq-default indent-tabs-mode nil)

(setq eval-expression-print-length nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ledger-highlight-xact-under-point nil)
 '(ledger-mode-should-check-version nil)
 '(ledger-post-auto-align nil)
 '(ledger-reconcile-buffer-header "" nil nil "How do you set a 'string to nil?")
 '(ledger-reconcile-buffer-line-format "%(date)s %-4(code)s %-30(payee)s %15(amount)s
")
 '(line-move-visual nil)
 '(package-selected-packages '(ledger-mode magit)))

(setq inhibit-startup-screen t)
