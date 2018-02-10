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
(define-derived-mode servicenow-backgroundscriptoutputmode fundamental-mode "ServiceNowBackgroundScriptOutput"
  "major mode for ServiceNow background script output"
  (setq font-lock-defaults '('(("\\(Javascript compiler exception:\\) \\(.*\\) \\((\\)\\(.*\\); \\(line [0-9]+\\)\\()\\) \\(in:\\)" . ((1 font-lock-doc-face)
                                                                                                                                       (2 font-lock-warning-face) ;; error
                                                                                                                                       (3 nil) ;; (
                                                                                                                                       (4 font-lock-function-name-face) ;; script
                                                                                                                                       (5 font-lock-comment-face) ;; line
                                                                                                                                       (6 nil) ;; )
                                                                                                                                       (7 font-lock-doc-face) ;; in:
                                                                                                                                       ))))))


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

