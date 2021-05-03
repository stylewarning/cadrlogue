(in-package #:cadrlogue)

;;; This file is a "best effort" file. It isn't particularly robust or
;;; extensible. It also contains lots of hard-coded constants that
;;; work in my particular use-case.


;;; Barcode printing protocol

(defvar *barcode-height* 64)

(defgeneric print-barcode (printer kind string))

;;; Supported kinds:
;;;
;;;     :code128



;;; POS Printers
;;;
;;; These are usually receipt printers, but can be used to print
;;; barcodes.

(defclass pos-printer ()
  ((serial-device :initarg :serial-device
                  :reader pos-printer-serial-device)))

(defun write-esc (f &optional string)
  (write-byte 27 f)
  (etypecase string
    (character (write-char string f))
    (string    (write-string string f))
    ((unsigned-byte 8) (write-byte string f))))

(defun pos-feed-lines (f n)
  (check-type n (unsigned-byte 8))
  (write-esc f #\d)
  (write-byte n f))

(defun pos-cut-paper (f)
  (write-byte 29 f)
  (write-char #\V f)
  ;(write-byte 1 f)
  (write-byte 66 f)
  (write-byte 1 f)
  ;;(write-esc f #\i)
  )

(defun lf (f &optional (n 1))
  (loop :repeat n :do
    (write-byte 10 f)))

(defun numeric-char-p (c)
  (char<= #\0 c #\9))

(defun pos-write-code128-barcode (f string)
  (assert (<= 0 (length string) 253))
  (when (find #\{ string)
    (warn "{ characters need to be escaped!"))
  ;; Set height
  (write-byte 29 f)                     ; GS
  (write-char #\h f)
  (write-byte *barcode-height* f)
  ;; Set width
  (write-byte 29 f)                     ; GS
  (write-char #\w f)
  (write-byte 2 f)                      ; 2-6; 2 smallest
  ;; Set HRI location
  (write-byte 29 f)                     ; GS
  (write-char #\H f)
  (write-byte 2 f)                      ; Below barcode
  ;; Set HRI font
  (write-byte 29 f)                     ; GS
  (write-char #\f f)
  (write-byte 1 f)                      ; 0 bigger, 1 smaller
  ;; Set up for CODE39
  (write-byte 29 f)                     ; GS
  (write-char #\k f)
  (write-byte 73 f)                     ; CODE 128
  (write-byte (+ 2 (length string)) f)
  (write-string "{B" f)
  (write-string string f))

(defmethod print-barcode ((printer pos-printer) (type (eql :code128)) (string string))
  (with-open-file (f (pos-printer-serial-device printer)
                     :direction ':output
                     :if-does-not-exist ':error
                     :if-exists ':append
                     :element-type '(unsigned-byte 8))
    (let ((f (flexi-streams:make-flexi-stream f :external-format ':ascii)))
      (write-esc f #\R)                 ; language = EN
      (write-byte 0 f)
      (write-esc f #\a)                 ; Center justify
      (write-byte 1 f)
      (pos-write-code128-barcode f string)
      (pos-feed-lines f 1)
      (pos-cut-paper f)
      (pos-feed-lines f 1)
      (terpri f))))


;;; EPL2 (like Zebra) Printers

(defclass epl2-printer ()
  ((serial-device :initarg :serial-device
                  :reader epl2-printer-serial-device)))

(defun epl2-code128-command (string &key (x 0)
                                         (y 0)
                                         (rotatep nil)
                                         (thin 2)
                                         (thick (round (* 1.5 thin)))
                                         (height *barcode-height*)
                                         (human-readable-annotation-p t))
  ;; THICK is actually ignored by CODE128 in EPL2.
  (format nil "B~D,~D,~:[0~;2~],1B,~D,~D,~D,~:[N~;B~],~S"
          x
          y
          rotatep
          thin
          thick
          height
          human-readable-annotation-p
          string))

(defmethod print-barcode ((printer epl2-printer) (kind (eql :code128)) (string string))
  (with-open-file (f (epl2-printer-serial-device printer)
                     :direction ':output
                     :if-does-not-exist ':error
                     :if-exists ':append
                     :element-type '(unsigned-byte 8))
    (let ((f (flexi-streams:make-flexi-stream f :external-format ':ascii)))
      (write-line "N" f)                ; Clear image buffer
      (write-line (epl2-code128-command string :x 0 :y 25 :thin 3) f)
      (write-line "P1" f)               ; Print 1 copy
      nil)))
