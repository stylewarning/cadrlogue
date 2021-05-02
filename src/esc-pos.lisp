(in-package #:cadrlogue)

(defparameter *device* "/dev/ttyUSB3")

(defparameter *barcode-height* 60)

(defun write-esc (f &optional string)
  (write-byte 27 f)
  (etypecase string
    (character (write-char string f))
    (string    (write-string string f))
    ((unsigned-byte 8) (write-byte string f))))

(defun feed-lines (f n)
  (check-type n (unsigned-byte 8))
  (write-esc f #\d)
  (write-byte n f))

(defun cut-paper (f)
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

(defun write-barcode (f string)
  (assert (<= 0 (length string) 253))
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

(defun print-barcode-label (string)
  (with-open-file (f *device* :direction ':output
                              :if-does-not-exist ':error
                              :if-exists ':append
                              :element-type '(unsigned-byte 8))
    (let ((f (flexi-streams:make-flexi-stream f :external-format ':ascii)))
      (write-esc f #\R)                 ; language = EN
      (write-byte 0 f)
      (write-esc f #\a)                 ; Center justify
      (write-byte 1 f)
      ;(write-string "This is a test!" f)
      ;(lf f)
      (write-barcode f string)
      (feed-lines f 1)
      (cut-paper f)
      (feed-lines f 1)
      (terpri f))))

