
;;;###autoload
(defun servicenow-run-script ()
  (interactive)
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
                 (let ((bufname (format "Script - Background (%s)" (buffer-name)))
                       (origbufname (buffer-name)))
                   (message "Finished executing %s" origbufname)
                   (servicenow--split-window-apply-function bufname
                                                            '(lambda ()
                                                               (mapcar 'insert (servicenow--get-script-output data))
                                                               (insert "\n")
                                                               (funcall 'servicenow-backgroundscriptoutputmode)
                                                               (read-only-mode)
                                                               (goto-line 1))))))
     :error (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                           (message "Error: %S" error-thrown))))))
