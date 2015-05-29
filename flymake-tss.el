(require 'tss)

(defconst flymake-tss--err-line-pattern
  '("\\`\\(.+?\\.ts\\) (\\([0-9]+\\),\\([0-9]+\\)): \\(.+\\)" 1 2 3 4)
  "Error line pattern, converted from `tss--get-errors's format by `'")

(defun flymake-tss--err-formatter (ret)
  "Format RET from `tss--get-errors' into
`flymake-tss--err-line-pattern'."
  (mapcar (lambda (e)
            (let* ((file (cdr (assoc 'file e)))
                   (start (cdr (assoc 'start e)))
                   (line (cdr (assoc 'line start)))
                   (col (cdr (or (assoc 'character start)
                                 (assoc 'col start))))
                   (text (cdr (assoc 'text e))))
              (format "%s (%d,%d): %s" file (or line 0) (or col 0) text)))
          ret))

(defun flymake-tss-init ()
  (let* ((errors-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace)
                      ;; the above one is inefficient, but can use built-in clean function, so ;P
                      ;;
                      ;; (flymake-create-temp-inplace (buffer-file-name)
                      ;;                              "flymake-tss")
                      ))
    (let ((errors (tss--get-errors))
          res)
      (with-temp-file errors-file
        (loop for line in (flymake-tss--err-formatter errors)
              do (insert line "\n"))))
    ;; TODO a workaround on the limit of `flymake' tool, it requires subprocess
    ;; anyway. (Actually `flycheck' also has this requirement...)
    (list "cat" (list errors-file))))


;;;: Configurations
;; (require 'flymake-tss)
;; (defun flymake-tss-configure ())
;; (add-to-list 'flymake-err-line-patterns flymake-tss--err-line-pattern)
;; (add-to-list 'flymake-allowed-file-name-masks
;;              '(".+\\.ts$"
;;                flymake-tss-init
;;                flymake-simple-cleanup
;;                flymake-get-real-file-name))
;;; `flymake-no-changes-timeout' should be short for things to be useful.

(provide 'flymake-tss)
