;; Copied from yasnippet-snippets

(require 'yasnippet)

(defconst yasnippet-snippets-dir
  (expand-file-name
   "snippets"
   (file-name-directory
    ;; Copied from ‘f-this-file’ from f.el.
    (cond
     (load-in-progress load-file-name)
     ((and (boundp 'byte-compile-current-file) byte-compile-current-file)
      byte-compile-current-file)
     (:else (buffer-file-name))))))

;;;###autoload
(defun yasnippet-snippets-initialize ()
  (add-to-list 'yas-snippet-dirs 'yasnippet-snippets-dir t)
  (yas-load-directory yasnippet-snippets-dir))

;;;###autoload
(eval-after-load 'yasnippet
  '(yasnippet-snippets-initialize))

;;;###autoload
(define-derived-mode servicenow-mode js2-mode "ServiceNow")

;;;###autoload
(defgroup servicenow nil
  "ServiceNow mode customizations."
  :tag "ServiceNow"
  :group 'convenience)

;;;###autoload
(defcustom servicenow-instances nil
  "Instances allowed.
List of instances that can use servicenow-mode."
  :tag "Instances"
  :group 'servicenow
  :type '(repeat string))

;;;###autoload
(defcustom servicenow-instance nil
  "Current instance.
Current instance being used for servicenow-mode."
  :tag "Instance"
  :group 'servicenow
  :type '(string))

(provide 'servicenow-mode)
