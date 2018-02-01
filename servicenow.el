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

(provide 'servicenow-mode)
