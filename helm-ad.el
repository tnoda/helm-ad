;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; helm-ad
;;; 
(eval-when-compile
  (require 'cl))

(require 'dash)

(defvar helm-source-ad-action-alist nil)

(unless helm-source-ad-action-alist
  (setq helm-source-ad-action-alist
        `(("user". ("email" "tel" "office"))
          ("contact" . ("email" "tel" "office")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Helper functions
;;; 
(defun helm-ad-dsget-function (cmd prop)
  (lexical-let ((cmd cmd)
                (prop prop))
    (lambda (dn)
      (with-current-buffer (get-buffer-create "*dsget*")
        (erase-buffer)
        (call-process "dsget" nil t nil
                      cmd
                      (substring dn 1 (1- (length dn)))
                      (concat "-" prop))
        (goto-char (point-min))
        (forward-line)
        (re-search-forward "[^ ]+" nil t)
        (kill-new (match-string-no-properties 0)))
      (insert (car kill-ring)))))

(defun helm-source-ad-command-action (cmd)
  (-map (lambda (prop)
          `(,prop . ,(helm-ad-dsget-function cmd prop)))
        (assoc-default cmd helm-source-ad-action-alist)))

(defun helm-source-ad-command-candidates-function (cmd)
  (lexical-let ((cmd cmd))
    (lambda ()
      (with-temp-buffer
        (call-process "dsquery" nil t nil cmd "-name"
                      (concat helm-pattern "*"))
        (split-string (buffer-string) "\n")))))

(defun helm-source-ad-command (cmd)
  (lexical-let ((cmd cmd))
    `((name . ,(format "Active Directory %s" cmd))
      (candidates . ,(helm-source-ad-command-candidates-function cmd))
      (volatile)
      (requires-pattern . 2)
      (action . ,(helm-source-ad-command-action cmd)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; helm-ad-user
;;;
(defvar helm-source-ad-user (helm-source-ad-command "user"))

(defun helm-ad-user ()
  (interactive)
  (helm-other-buffer '(helm-source-ad-user) "*helm ad user*"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; helm-ad-contact
;;; 
(defun helm-ad-contact ()
  (interactive)
  (helm-other-buffer `(,(helm-source-ad-command "contact"))
                     "helm ad contact"))
