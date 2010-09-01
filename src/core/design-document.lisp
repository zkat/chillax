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

(defun invoke-design-doc (db design-doc-name &rest all-keys)
  (apply #'get-document db (strcat "_design/" design-doc-name) all-keys))

;;;
;;; Views
;;;
(defun get-temporary-view (db &key
                           (language "common-lisp")
                           (map (error "Must provide a map function for temporary views."))
                           reduce)
  (let ((json (with-output-to-string (s)
                (format s "{")
                (format s ",\"language\":~S" language)
                (format s "\"map\":~S" map)
                (when reduce
                  (format s ",\"reduce\":~S" reduce))
                (format s "}"))))
    (handle-request (response db "_temp_view" :method :post
                              :content json
                              :convert-data-p nil)
      (:ok response))))

;; (defun make-view (map-def &optional reduce-def)
;;   (let ((view (mkhash "map" map-def)))
;;     (when reduce-def (setf (at view "reduce") reduce-def))
;;     view))

;; (defun ensure-design-doc (db name &key (language "common-lisp") views
;;                           &aux (full-name (strcat "_design/" name)))
;;   (let ((old-doc (get-document db full-name))
;;         (new-doc (mkhash "language" language
;;                          "views" views)))
;;     (when old-doc
;;       (setf (at new-doc "_rev") (at old-doc "_rev")))
;;     (put-document db full-name new-doc)
;;     full-name))

;; (defun ensure-view (design-doc view-name view)
;;   (if (at design-doc "views")
;;       (setf (at design-doc "views" view-name) view)
;;       (setf (at design-doc "views") (mkhash view-name view)))
;;   design-doc)
