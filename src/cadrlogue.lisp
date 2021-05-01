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

(defun parse-price-string (string)
  (multiple-value-bind (match dollars-cents)
      (cl-ppcre:scan-to-strings "\\$?(\\d*)\\.(\\d\\d)" string)
    (when (null match)
      (error "bad price string: ~S" string))
    (let ((dollars (aref dollars-cents 0))
          (cents   (aref dollars-cents 1)))
      (when (zerop (length dollars))
        (setf dollars "0"))
      (+ (parse-integer dollars)
         (/ (parse-integer cents) 100)))))

(defclass offer ()
  ((condition :initarg :condition :reader offer-condition)
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
    (make-instance 'isbn-result
                   :requested-isbn isbn
                   :isbn10 (gethash "isbn" result)
                   :isbn13 (gethash "isbn_13" result)
                   :language (gethash "language" result)
                   :short-title (gethash "title" result)
                   :title (gethash "title_long" result)
                   :subtitle (gethash "subtitle" result)
                   :authors (gethash "authors" result)
                   :publishers (list (gethash "publisher" result))
                   :publish-date (gethash "date_published" result)                   
                   :page-count (gethash "pages" result)
                   :dewey-classes (alexandria:ensure-list
                                   (gethash "dewey_decimal" result))
                   :subjects (gethash "subjects" result)
                   :msrp (parse-price-string (gethash "msrp" result))
                   :offers (sort (loop :for offer :in (gethash "prices" result)
                                       :collect (make-instance
                                                 'offer
                                                 :condition (gethash "condition" offer)
                                                 :price (parse-price-string
                                                         (gethash "price" offer))))
                                 #'<
                                 :key #'offer-price))))

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

(defun prompt-line (prompt)
  (write-string prompt)
  (finish-output)
  (read-line))

(defun run (n)
  (loop :repeat n
        :for line := (prompt-line "ISBN: ")
        :do (cond
              ((isbn? line)
               (fresh-line)
               (print-info (lookup line))
               (fresh-line)
               (terpri))
              (t
               (format t "~&Invalid thing: ~S~%" line)))))
