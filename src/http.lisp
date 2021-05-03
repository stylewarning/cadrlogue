(in-package #:cadrlogue)

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
