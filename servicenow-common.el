;;; -*- lexical-binding: t -*-

(require 'request)
(require 'dom)

(defvar servicenow--username nil)
(defvar servicenow--password nil)
(defvar servicenow--sysparm_ck nil)

(defun servicenow--get-sysparm_ck (dom)
  (alist-get 'value (dom-attributes (dom-by-id dom "sysparm_ck"))))

(defun servicenow--get-script-output (dom)
  (let ((strings (cl-remove-if-not 'stringp (dom-children (dom-by-tag dom 'pre)))))
    (mapcar (lambda (x) (replace-regexp-in-string "^\*\*\* Script:\s" "" x)) strings)))

(defun servicenow--is-script-not-authorized (dom)
  (let ((body)
        (text))
    (setq body (car (dom-by-tag dom 'body)))
    (setq text (dom-text (car (dom-children body))))
    (string= text "not authorized")))

(defun servicenow--logout (cb)
  (request
   (format "https://%s/logout.do" servicenow-instance)
   :type "GET"
   :success (cl-function
             (lambda (&key data &allow-other-keys)
               (funcall cb)))))

(defun servicenow--login1 ()
  (request
   (format "https://%s/login.do" servicenow-instance)
   :type "POST"
   :data `((user_name . ,servicenow--username)
           (user_password . ,(servicenow--read-password servicenow--password))
           (sys_action . sysverb_login))
   :parser (lambda () (libxml-parse-html-region (point) (point-max)))
   :success (cl-function
             (lambda (&key data &allow-other-keys)
               (setq servicenow--sysparm_ck (servicenow--get-sysparm_ck data))
               (servicenow--check-login)))))

(defun servicenow--check-login ()
  (request
   (format "https://%s/sys.scripts.do" servicenow-instance)
   :type "POST"
   :data `((script . "gs.print('Hello World')")
           (sysparm_ck . ,servicenow--sysparm_ck)
           (runscript . "Run script")
           (sys_scope . "global")
           (quota_managed_transaction . "on"))
   :parser (lambda () (libxml-parse-html-region (point) (point-max)))
   :success (cl-function
             (lambda (&key data &allow-other-keys)
               (if (servicenow--is-script-not-authorized data)
                   (message "Not authorized to execute script!")
                 (let ((output (servicenow--get-script-output data)))
                   (if (string= (car output) "Hello World")
                       (message "Success!")
                     (message "Login not successful!"))))))
   :error (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                         (message "Error: %S" error-thrown)))))

;;;###autoload
(defun servicenow-instance (instance)
  (interactive "sInstance Name: \n")
  (if (member instance servicenow-instances)
      (customize-save-variable 'servicenow-instance instance)
    (message "%s not in list of ServiceNow instances %S (See ServiceNow customization)" instance servicenow-instances)))

;;;###autoload
(defun servicenow-username (username)
  (interactive "sUser Name: \n")
  (setq servicenow--username username))

;;;###autoload
(defun servicenow-password ()
  (interactive)
  (setq servicenow--password (base64-encode-string (read-passwd "Password: "))))

(defun servicenow--read-password (password)
  (base64-decode-string password))

;;;###autoload
(defun servicenow-login ()
  (interactive)
  (unless servicenow-instance
    (call-interactively 'servicenow-instance))
  (unless servicenow--username
    (call-interactively 'servicenow-username))
  (unless servicenow--password
    (call-interactively 'servicenow-password))
  (servicenow--logout 'servicenow--login1))

