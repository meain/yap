;;; yap-utils.el --- Bunch of util functions for yap -*- lexical-binding: t; -*-

;;; Commentary:
;; Bunch of util functions for yap

(defun yap--parse-csv-line (line)
  "Parse a CSV LINE into a list of fields, handling quotes properly."
  (let ((result '())
        (field "")
        (in-quote nil)
        (i 0))
    (while (< i (length line))
      (let ((char (aref line i)))
        (cond
         ((and (not in-quote) (eq char ?,))
          ;; Comma outside quotes: end of field
          (push (string-trim field) result)
          (setq field ""))
         ((eq char ?\")
          ;; Quote character
          (if in-quote
              (if (and (< (1+ i) (length line)) (eq (aref line (1+ i)) ?\"))
                  ;; Escaped quote
                  (progn
                    (setq field (concat field "\""))
                    (cl-incf i))
                ;; End of quoted field
                (setq in-quote nil))
            ;; Start of quoted field
            (setq in-quote t)))
         (t
          ;; Any other character
          (setq field (concat field (char-to-string char)))))
        (cl-incf i)))
    ;; Add the last field
    (push (string-trim field) result)
    ;; Return the fields in correct order
    (nreverse result)))

(provide 'yap-utils)
;;; yap-utils.el ends here