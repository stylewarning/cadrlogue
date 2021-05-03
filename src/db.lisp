(in-package #:cadrlogue)

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
           (mapc
            (lambda (stmt)
              (sqlite:execute-non-query db stmt))
            '(
              "CREATE TABLE publication_details (pub_id INTEGER PRIMARY KEY AUTOINCREMENT, isbn STRING NULL, title STRING NULL, msrp_cents INTEGER NULL)"
              "CREATE TABLE offers (id INTEGER PRIMARY KEY, timestamp INTEGER NOT NULL, condition STRING NULL, price_cents INTEGER NOT NULL, pub_id INTEGER NOT NULL, FOREIGN KEY(pub_id) REFERENCES publication_details(pub_id))"
              "CREATE TABLE media (physical_id INTEGER PRIMARY KEY AUTOINCREMENT, timestamp INTEGER NOT NULL, imprinted_isbn STRING NULL, condition STRING NULL, status STRING NOT NULL, pub_id INTEGER NULL, FOREIGN KEY(pub_id) REFERENCES publication_details(pub_id))"
              ))))
       db-path))))
