(in-package #:cadrlogue)

(defun isbn? (x)
  (and (stringp x)
       (every #'digit-char-p x)
       (or (= 10 (length x))
           (= 13 (length x)))))

(defvar *isbndb-api-key* (uiop:getenvp "ISBNDB_API_KEY"))

(defun isbndb-uri (isbn)
  (assert (isbn? isbn))
  (format nil "https://api.pro.isbndb.com/book/~A?with_prices=1" isbn))

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

(defun lookup-isbn (isbn &key (retry-attempts 3))
  (let* ((json (http-get
                (isbndb-uri isbn)
                `(("Authorization" . ,*isbndb-api-key*))))
         (result (gethash "book" (yason:parse json))))
    (cond
      ((null result)
       (warn "Failed to get result: ~A" json)
       (cond
         ((plusp retry-attempts)
          (sleep 1)
          (lookup-isbn isbn :retry-attempts (1- retry-attempts)))
         (t
          (error "Exhausted retry limit"))))
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

;;; Cataloguing flow

(defun prompt-line (prompt)
  (write-string prompt)
  (finish-output)
  (read-line))

(defun find-details-id-by-isbn (db isbn)
  (assert (isbn? isbn))
  (caar (sqlite:execute-to-list db "SELECT pub_id FROM publication_details WHERE isbn=? LIMIT 1"
                                isbn)))

(defun record-anonymous (db)
  "Record an anonymous artifact in the database. Details may be added later."
  (sqlite:execute-non-query db
                            "INSERT INTO media (timestamp, status) VALUES (?, ?)"
                            (get-universal-time)
                            "ACTIVE")
  (sqlite:last-insert-rowid db))

(defun record-known (db pub-id &key requested-isbn)
  "Record an artifact with a known publication ID."
  (sqlite:execute-non-query db
                            "INSERT INTO media (imprinted_isbn, timestamp, pub_id, status) VALUES (?, ?, ?,?)"
                            requested-isbn
                            (get-universal-time)
                            pub-id
                            "ACTIVE")
  (sqlite:last-insert-rowid db))

(defun record (db requested-isbn entry)
  (check-type entry isbn-result)
  (let ((info (find-details-id-by-isbn db (isbn-result-isbn entry))))
    (when (null info)
      (sqlite:execute-non-query db
                                "INSERT INTO publication_details (isbn, title, msrp_cents) VALUES (?, ?, ?)"
                                (isbn-result-isbn entry)
                                (or (isbn-result-title entry)
                                    (isbn-result-short-title entry))
                                (round (* 100 (isbn-result-msrp entry))))
      (setf info (sqlite:last-insert-rowid db))
      (dolist (offer (isbn-result-offers entry))
        (sqlite:execute-non-query db
                                  "INSERT INTO offers (timestamp, condition, price_cents, pub_id) VALUES (?, ?, ?, ?)"
                                  (offer-creation-time offer)
                                  (offer-condition offer)
                                  (round (* 100 (offer-price offer)))
                                  info)))
    (record-known db info :requested-isbn requested-isbn)))

(defvar *barcode-printer* (make-instance 'epl2-printer :serial-device "/dev/ttyUSB3"))
(defvar *barcode-format* ':code128)

(defun print-id-as-barcode (id)
  (assert (<= 0 id 9999999999))
  (let ((id-string (format nil "~10,'0D" id)))
    (format t "~&==> printing code for ~A~%" id-string)
    (print-barcode *barcode-printer* *barcode-format* id-string)
    nil))


(defun run (&key)
  (with-db (db (find-or-make-database))
    (loop
      (let ((scan (prompt-line "Scan: ")))
        (cond
          ((isbn? scan)
           ;; First check if we've seen this before...
           (let ((details-id (find-details-id-by-isbn db scan)))
             (cond
               ;; We haven't...
               ((null details-id)
                ;; ... so we'll try to look it up.
                (multiple-value-bind (isbn-record error) (ignore-errors (lookup-isbn scan))
                  (cond
                    ;; Lookup failed, just record the thing and move on.
                    ((or (not (null error))
                         (null isbn-record))
                     (warn "Failed to look up ISBN ~S" scan)
                     (print-id-as-barcode (record-anonymous db)))
                    ;; Lookup succeeded. Store it.
                    (t
                     (print-id-as-barcode (record db scan isbn-record))))))
               ;; We have seen this kind of before.
               (t
                (print-id-as-barcode (record-known db details-id :requested-isbn scan))))))
          ((string-equal scan "DEFER")
           (print-id-as-barcode (record-anonymous db)))
          ((string-equal scan "QUIT")
           (return-from run))
          (t
           (warn "Unknown command ~S, ignoring..." scan)))))))

