(in-package :chillax)

;;;
;;; Design Doc basics
;;;
(defun view-cleanup (db)
  (handle-request (response db "_view_cleanup" :method :post)
    (:accepted response)))

(defun compact-design-doc (db design-doc-name)
  (handle-request (response db (strcat "_compact/" design-doc-name) :method :post)
    (:accepted response)
    (:not-found (error 'document-not-found :db db :id design-doc-name))))

(defun design-doc-info (db design-doc-name)
  (handle-request (response db (strcat "_design/" design-doc-name "/_info"))
    (:ok response)
    (:not-found (error 'document-not-found :db db :id design-doc-name))))

;;;
;;; Views
;;;
(defgeneric view-map-definition (view)
  (:method ((view hash-table))
    (at view "map")))
(defgeneric view-reduce-definition (view)
  (:method ((view hash-table))
    (at view "reduce")))

(defun get-temporary-view (db view &optional (language "common-lisp"))
  (let ((json (with-output-to-string (s)
                (format s "{")
                (format s "\"map\":~S" (view-map-definition view))
                (when (view-reduce-definition view)
                  (format s ",\"reduce\":~S" (view-reduce-definition view)))
                (format s ",\"language\":~S" language)
                (format s "}"))))
    (handle-request (response db "_temp_view" :method :post
                              :content json
                              :convert-data-p nil)
      (:ok response))))

;;;
;;; Unstable
;;;
(defun make-view (map-def &optional reduce-def)
  (let ((view (mkhash "map" map-def)))
    (when reduce-def (setf (at view "reduce") reduce-def))
    view))

(defun ensure-design-doc (&optional (language "common-lisp"))
  (mkhash "language" language))

(defun ensure-view (design-doc view-name view)
  (let ((existing-views (at design-doc "views")))
    (if existing-views
        (setf (at design-doc "views" view-name) view)
        (setf (at design-doc "views") (mkhash view-name view))))
  design-doc)

(defun save-design-doc (db name design-doc)
  (put-document db (strcat "_design/" name) design-doc))
