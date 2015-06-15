(require 'etss)

(defconst flymake-etss--err-line-pattern
  '("\\`\\(.+?\\.ts\\) (\\([0-9]+\\),\\([0-9]+\\)): \\(.+\\)" 1 2 3 4)
  "Error line pattern, converted from `etss--get-errors's format by `'")

(defun flymake-etss--err-formatter (ret)
  "Format RET from `etss--get-errors' into
`flymake-etss--err-line-pattern'."
  (mapcar (lambda (e)
            (let* ((file (cdr (assoc 'file e)))
                   (start (cdr (assoc 'start e)))
                   (line (cdr (assoc 'line start)))
                   (col (cdr (or (assoc 'character start)
                                 (assoc 'col start))))
                   (text (cdr (assoc 'text e))))
              (format "%s (%d,%d): %s" file (or line 0) (or col 0) text)))
          ret))

;; NOTE `flymake-no-changes-timeout' need to be short for things to be useful.
(defun flymake-etss-init ()
  (let* ((errors-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace)
                      ;; the above one is inefficient, but can use built-in
                      ;; clean function, so ;P
                      ;;
                      ;; (flymake-create-temp-inplace (buffer-file-name)
                      ;;                              "flymake-etss")
                      ))
    (let ((errors (etss--get-errors))
          res)
      (with-temp-file errors-file
        (loop for line in (flymake-etss--err-formatter errors)
              do (insert line "\n"))))
    ;; TODO a workaround on the limit of `flymake' tool, it requires subprocess
    ;; anyway. (Actually `flycheck' also has this requirement...)
    (list "cat" (list errors-file))))


(defun flymake-etss-setup ()
  "Setup `flymake-etss'."
  (add-to-list 'flymake-err-line-patterns flymake-etss--err-line-pattern)
  (add-to-list 'flymake-allowed-file-name-masks
               '(".+\\.ts$"
                 flymake-etss-init
                 flymake-simple-cleanup
                 flymake-get-real-file-name)))

(provide 'flymake-etss)
