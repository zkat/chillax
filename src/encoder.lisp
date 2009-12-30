(in-package :chillax)

(defparameter *json-lisp-escaped-chars*
  `((#\" . #\")
    (#\\ . #\\)
    (#\/ . #\/)
    (#\b . #\Backspace)
    (#\f . ,(code-char 12))
    (#\n . #\Newline)
    (#\r . #\Return)
    (#\t . #\Tab)))

(defun lisp-special-char-to-json (lisp-char)
    (car (rassoc lisp-char *json-lisp-escaped-chars*)))

(defun write-json-chars (s stream)
  (declare (inline lisp-special-char-to-json))
  (loop for ch across s
        for code = (char-code ch)
        for special = (lisp-special-char-to-json ch)
        do
        (cond
          ((and special (not (char= special #\/)))
           (write-char #\\ stream)
           (write-char special stream))
          ((<= code #x1f)
           (format stream "\\u~4,'0x" code))
          (t (write-char ch stream)))))

(defun write-json-string (s stream)
  (write-char #\" stream)
  (write-json-chars s stream)
  (write-char #\" stream))

(defun write-json-number (nr stream)
  (if (integerp nr)
      (format stream "~d" nr)
      (format stream "~f" nr)))

(defun write-json-symbol(symbol stream)
  (cond
    ((null symbol) (write-json-chars "null" stream))
    ((eq 't symbol) (write-json-chars "true" stream))
    (t (write-json-string (ensure-string symbol) stream))))

(defun keyword-assocp (e)
  "Return true if element is a list that begins with a keyword. This
  is used to help determine associative list-ness."
  (and (listp e) (keywordp (car e))))

(defun assoclp (e)
  "Return true if expression is, or really looks like, an associative
list. Dead giveaways include cons elements in the list that begin with
a keyword. Returns the element that produced a positive result, or
nil."
  (labels ((improperlistp (list) 
             (and (listp list)
                  (not (listp (cdr list)))))
           (test (list)
             (cond ((or (null list) (not (listp list)))
                    nil)
                   ((keyword-assocp (car list))
                    (car list))
                   ((improperlistp (car list))
                    (car list)))))
    (and (listp e) (test e))))

(defun write-alist (d stream)
  (write-char #\{ stream)
  (loop for e on d 
     do 
       (let ((cons (car e)))
         (cond ((stringp (car cons))
                (write-string (double-quote (car cons)) stream))
               ((symbolp (car cons))
                (write-json-symbol (car cons) stream)))
         (write-char #\: stream)
         (encode (cdr (car e)) stream))
     when (cdr e) do (write-char #\, stream))
  (write-char #\} stream))

(defun write-list (d stream)
  (write-char #\[ stream)
  (loop for e on d 
     do (encode (car e) stream)
     when (cdr e) do (write-char #\, stream))
  (write-char #\] stream))

(defun hash->alist (hash)
  (loop for val being the hash-values of hash
       using (hash-key key)
       collect (cons key val)))

(defgeneric encode (object stream)
  (:method (object stream)
    (declare (ignore stream))
    (error "Don't know how to encode ~A" object))
  (:method ((list null) stream)
    (declare (ignore list))
    (write-string "null" stream))
  (:method ((number number) stream)
    (write-json-number number stream))
  (:method ((symbol symbol) stream)
    (write-json-symbol symbol stream))
  (:method ((string string) stream)
    (write-json-string string stream))
  (:method ((list list) stream)
    (cond ((assoclp list)
           (write-alist list stream))
          (t
           (write-list list stream))))
  (:method ((table hash-table) stream)
    (encode (hash->alist table) stream)))

(defun document-to-json-stream (doc stream)
  "Encode document to stream with special support for detecting and
handling associative lists."
  (if (null doc)
      (write-string "{}" stream)
      (encode doc stream)))

(defun document-to-json (doc)
  "Encode document to string with special support for detecting and
handling associative lists."
 (with-output-to-string (stream)
   (document-to-json-stream doc stream)))
