
;;;###autoload
(defun servicenow-run-script ()
  (interactive)
  (if (boundp 'servicenow--sysparm_ck)
      (servicenow--run-script)
    (progn
      (servicenow-login '(lambda () (servicenow--run-script))))))

;;;###autoload
(defun servicenow-run-script-kill-script-background ()
  (interactive)
  (servicenow-kill-script-background)
  (servicenow-run-script))

(defun servicenow--run-script ()
  (let ((bufcontent (buffer-string))
        (origbufname (buffer-name)))
    (message "Executing: %s" origbufname)
    (request
     (format "https://%s/sys.scripts.do" servicenow-instance)
     :type "POST"
     :data `((script . ,bufcontent)
             (sysparm_ck . ,servicenow--sysparm_ck)
             (runscript . "Run script")
             (sys_scope . "global")
             (quota_managed_transaction . "on"))
     :parser (lambda () (libxml-parse-html-region (point) (point-max)))
     :success (cl-function
               (lambda (&key data &allow-other-keys)
                 (let* ((scriptbufname (buffer-name))
                        (bufname (servicenow--background-script-output-buffer-name scriptbufname))
                        (origbufname (buffer-name))
                        (scriptoutput (servicenow--get-script-output data)))
                   (if (car scriptoutput)
                       (progn
                         (message "Finished executing %s" origbufname)
                         (servicenow--split-window-apply-function bufname
                                                                  `(lambda ()
                                                                     (mapcar 'insert scriptoutput)
                                                                     (insert "\n")
                                                                     (funcall 'servicenow-backgroundscriptoutputmode)
                                                                     (servicenow--background-script-link-line-number (buffer-name) ,scriptbufname)
                                                                     (servicenow--background-script-link-line-number-caused-by (buffer-name) ,scriptbufname)
                                                                     (read-only-mode)
                                                                     (goto-line 1))))
                     (message "Script did not execute!")))))
     :error (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                           (message "Error: %S" error-thrown))))))

(defun servicenow--background-script-output-buffer-name (scriptbufname)
  (format "Script - Background (%s)" scriptbufname))

(defun servicenow--background-script-link-line-number (bufname scriptbufname)
  (with-current-buffer bufname
    (save-excursion
      (goto-char (point-min))
      (let ((linelength (- (line-end-position)
                           (line-beginning-position))))
        (if (and (> linelength 0) (re-search-forward "; \\(line \\([0-9]+\\)\\))" linelength t))
            (progn
              (let* ((matchbeg (match-beginning 1))
                     (matchstr (match-string 1))
                     (matchlen (length matchstr))
                     (linum (string-to-number (match-string 2))))
                (make-button matchbeg
                             (+ matchbeg matchlen)
                             'follow-link t
                             'action `(lambda(x)
                                        (with-current-buffer ,scriptbufname
                                          (switch-to-buffer-other-window ,scriptbufname)
                                          (goto-line ,linum)))))))))))

(defun servicenow--background-script-link-line-number-caused-by (bufname scriptbufname)
  (with-current-buffer bufname
    (save-excursion
      (goto-char (point-max))
      (if (re-search-backward "   Caused by error in script at \\(line \\([0-9]+\\)\\)" nil t)
          (progn
            (let* ((matchbeg (match-beginning 1))
                   (matchstr (match-string 1))
                   (matchlen (length matchstr))
                   (linum (string-to-number (match-string 2))))
              (message "pos: %d len %d linum %d" (point) matchlen linum)
              (make-button matchbeg
                           (+ matchbeg matchlen)
                           'follow-link t
                           'action `(lambda(x)
                                      (with-current-buffer ,scriptbufname
                                        (switch-to-buffer-other-window ,scriptbufname)
                                        (goto-line ,linum))))))))))

;;;###autoload
(defun servicenow-kill-script-background ()
  (interactive)
  (servicenow--kill-script-background (servicenow--background-script-output-buffer-name (buffer-name))))

;;;###autoload
(defun servicenow-kill-all-script-background ()
  (interactive)
  (servicenow--kill-script-background "Script - Background ("))

(defun servicenow--kill-script-background (prefix)
  (dolist (buf (buffer-list))
    (if (string-prefix-p prefix (buffer-name buf))
        (kill-buffer buf))))

