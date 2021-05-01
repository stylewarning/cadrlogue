(in-package #:cadrlogue)

;;; Barcode reader is assumed to write to stdin, and append a CR to
;;; the end of the read.

(define-condition http-status (condition)
  ((code :initarg :code :reader http-status-code)
   (reason :initarg :reason :reader http-status-reason)))

(define-condition http-error-status (http-status error)
  ())

(defun http-get (uri &optional headers)
  (multiple-value-bind (result code headers uri-from stream closed? reason)
      (drakma:http-request uri :additional-headers headers)
    (declare (ignore headers uri-from stream closed?))
    (cond
      ((= 200 code)
       (signal 'http-status :code code :reason reason)
       (etypecase result
         (string result)
         (vector (flexi-streams:octets-to-string result))))
      ((<= 400 code 499)
       (error 'http-error-status :code code :reason reason))
      ((<= 500 code 599)
       (error 'http-error-status :code code :reason reason))
      (t
       (signal 'http-status :code code :reason reason)))))

(defun isbn? (x)
  (and (stringp x)
       (every #'digit-char-p x)
       (or (= 10 (length x))
           (= 13 (length x)))))

(defun open-library-isbn-uri (isbn)
  (assert (isbn? isbn))
  (format nil "https://openlibrary.org/isbn/~A.json" isbn))

(defparameter *isbndb-api-key* (uiop:getenvp "ISBNDB_API_KEY"))

(defun isbndb-uri (isbn)
  (assert (isbn? isbn))
  (format nil "https://api2.isbndb.com/book/~A?with_prices=1" isbn))

(defclass isbn-result ()
  ((requested-isbn :initarg :requested-isbn :reader isbn-result-requested-isbn)
   (isbn10 :initarg :isbn10 :reader isbn-result-isbn10)
   (isbn13 :initarg :isbn13 :reader isbn-result-isbn13)
   (language :initarg :language :reader isbn-result-language)
   (short-title :initarg :short-title :reader isbn-result-short-title)
   (full-title :initarg :title :reader isbn-result-title)
   (authors :initarg :authors :reader isbn-result-authors)
   (publishers :initarg :publishers :reader isbn-result-publishers)
   (publish-date :initarg :publish-date :reader isbn-result-publish-date)
   (page-count :initarg :page-count :reader isbn-result-page-count)
   (dewey-classes :initarg :dewey-classes :reader isbn-result-dewey-classes)
   (subjects :initarg :subjects :reader isbn-result-subjects)
   (msrp :initarg :msrp :reader isbn-result-msrp)
   (offers :initarg :offers :reader isbn-result-offers)))

(defgeneric isbn-result-isbn (result)
  (:method ((result isbn-result))
    (or (isbn-result-isbn13 result)
        (isbn-result-isbn10 result)
        (isbn-result-requested-isbn result))))

(defun parse-price-string (string)
  (multiple-value-bind (match dollars-cents)
      (cl-ppcre:scan-to-strings "^\\$?(\\d*)(?:\\.(?:(\\d\\d))?)?$" string)
    (when (null match)
      (error "bad price string: ~S" string))
    (let ((dollars (aref dollars-cents 0))
          (cents   (or (aref dollars-cents 1)
                       "00")))
      (when (zerop (length dollars))
        (setf dollars "0"))
      (+ (parse-integer dollars)
         (/ (parse-integer cents) 100)))))

(defclass offer ()
  ((creation-time :initarg :creation-time :reader offer-creation-time)
   (condition :initarg :condition :reader offer-condition)
   (price :initarg :price :reader offer-price)))

(defmethod print-object ((o offer) stream)
  (print-unreadable-object (o stream :type t :identity nil)
    (format stream "~A for $~A"
            (offer-condition o)
            (format-price (offer-price o)))))

(defun lookup (isbn)
  (let ((result (gethash "book" (yason:parse
                                 (http-get
                                  (isbndb-uri isbn)
                                  `(("Authorization" . ,*isbndb-api-key*)))))))
    (cond
      ((null result)
       (warn "Failed to get result...")
       (sleep 1)
       (lookup isbn))
      (t
       (make-instance 'isbn-result
                      :requested-isbn isbn
                      :isbn10 (gethash "isbn" result)
                      :isbn13 (gethash "isbn13" result)
                      :language (gethash "language" result)
                      :short-title (gethash "title" result)
                      :title (gethash "title_long" result)
                      :authors (gethash "authors" result)
                      :publishers (alexandria:ensure-list
                                   (gethash "publisher" result))
                      :publish-date (gethash "date_published" result)
                      :page-count (gethash "pages" result)
                      :dewey-classes (alexandria:ensure-list
                                      (gethash "dewey_decimal" result))
                      :subjects (gethash "subjects" result)
                      :msrp (parse-price-string (gethash "msrp" result))
                      :offers (sort (loop :for offer :in (gethash "prices" result)
                                          :collect (make-instance
                                                    'offer
                                                    :creation-time (get-universal-time)
                                                    :condition (gethash "condition" offer)
                                                    :price (parse-price-string
                                                            (gethash "price" offer))))
                                    #'<
                                    :key #'offer-price))))))

(defun format-price (price)
  (multiple-value-bind (whole part)
      (truncate price 1)
    (format nil "~D.~2,'0D"
            whole
            (round (* 100 part)))))

(defun print-info (record)
  (format t "Found a book with ~D page~:P:~%" (isbn-result-page-count record))
  (format t "  Title: ~A~%" (isbn-result-title record))
  (format t "  Authors: ~{~%    * ~A~}~%" (isbn-result-authors record))
  (format t "  Subjects: ~{~%    * ~A~}~%" (isbn-result-subjects record))
  (format t "  Retail Value: $~A~%" (format-price (isbn-result-msrp record)))
  (format t "  Offers:")
  (cond
    ((null (isbn-result-offers record))
     (format t " None.~%"))
    (t
     (loop :for offer :in (isbn-result-offers record)
           :do (format t "~%    * $~A in ~S condition"
                       (format-price (offer-price offer))
                       (offer-condition offer)))
     (fresh-line))))

(defun entry-summary (entry)
  (flet ((chop (n str)
           (if (<= (length str) n)
               str
               (subseq str 0 n))))
    (cond
      ((or (null entry) (keywordp entry))
       "")
      ((and (typep entry 'isbn-result)
            (stringp (isbn-result-title entry)))
       (string-trim '(#\Space) (chop 20 (isbn-result-title entry))))
      (t
       "???"))))

;;; database

(defmacro with-db ((db path) &body body)
  `(sqlite:with-open-database (,db ,path)
     (sqlite:execute-non-query ,db "PRAGMA foreign_keys = ON;")
     ,@body))

(defun find-or-make-database (&key (root (user-homedir-pathname)))
  (let ((db-path (merge-pathnames ".cadrlogue/media.db" root)))
    (ensure-directories-exist db-path)
    (cond
      ((probe-file db-path)
       db-path)
      (t
       (format t "~&Creating database...~%")
       (with-db (db db-path)
         (sqlite:with-transaction db
           (sqlite:execute-non-query db "CREATE TABLE details (id INTEGER PRIMARY KEY, isbn13 STRING NULL, title STRING NULL, msrp_cents INTEGER NULL)")
           (sqlite:execute-non-query db "CREATE TABLE offers (id INTEGER PRIMARY KEY, timestamp INTEGER NOT NULL, condition STRING NULL, price_cents INTEGER NOT NULL, item INTEGER NOT NULL, FOREIGN KEY(item) REFERENCES details(id))")
           (sqlite:execute-non-query db "CREATE TABLE media (physical_id INTEGER PRIMARY KEY AUTOINCREMENT, isbn STRING NULL, status STRING NOT NULL, info INTEGER NULL, FOREIGN KEY(info) REFERENCES details(id))")))
       db-path))))

;;; Cataloguing flow

(defun prompt-line (prompt)
  (write-string prompt)
  (finish-output)
  (read-line))

(defun find-details-id-by-isbn (db isbn)
  (assert (isbn? isbn))
  (caar (sqlite:execute-to-list db "SELECT id FROM details WHERE isbn13=? LIMIT 1"
                                isbn)))

(defun record (db entry)
  (etypecase entry
    (isbn-result
     (sqlite:with-transaction db
       (let ((info (find-details-id-by-isbn db (isbn-result-isbn entry))))
         (when (null info)
           (sqlite:execute-non-query db
                                     "INSERT INTO details (isbn13, title, msrp_cents) VALUES (?, ?, ?)"
                                     (isbn-result-isbn13 entry)
                                     (or (isbn-result-title entry)
                                         (isbn-result-short-title entry))
                                     (round (* 100 (isbn-result-msrp entry))))
           (setf info (sqlite:last-insert-rowid db))
           (dolist (offer (isbn-result-offers entry))
             (sqlite:execute-non-query db
                                       "INSERT INTO offers (timestamp, condition, price_cents,item) VALUES (?,?,?,?)"
                                       (offer-creation-time offer)
                                       (offer-condition offer)
                                       (round (* 100 (offer-price offer)))
                                       info)))
         (sqlite:execute-non-query db
                                   "INSERT INTO media (isbn, status,info) VALUES (?, ?,?)"
                                   (isbn-result-requested-isbn entry)
                                   "ACTIVE"
                                   info))
       (sqlite:last-insert-rowid db)))
    ((member :UNKNOWN)
     (sqlite:execute-non-query db
                               "INSERT INTO media (isbn, status) VALUES (?, ?)"
                               nil
                               "ACTIVE")
     (sqlite:last-insert-rowid db))))

(defun print-barcode (id)
  (format t "~&==> ID~64,'0B~%" id))

(defun run (&key)
  (with-db (db (find-or-make-database))
    (loop :repeat 4 :do
      (let ((scan (prompt-line "Scan: ")))
        (cond
          ((isbn? scan)
           (let* ((record (lookup scan))
                  (id (record db record)))
             (print-barcode id)))
          ((string-equal scan "WEEE INSERT")
           (print-barcode (record db ':unknown)))
          (t
           (format t "Unknown thing: ~S~%" scan)))))))
