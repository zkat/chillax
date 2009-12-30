(in-package :chillax)

(defun strcat (string &rest more-strings)
  "Concatenates a series of strings."
  (apply 'concatenate 'string string more-strings))

(defun double-quote (string)
  "Wraps STRING with escaped double quotes."
  (strcat "\"" string "\""))

(defun ensure-keyword (obj)
  (etypecase obj
    (keyword obj)
    (symbol (intern (symbol-name obj) :keyword))
    (string (intern obj :keyword))))

(defun ensure-string (obj)
  (etypecase obj
    (symbol (symbol-name obj))
    (string obj)))

