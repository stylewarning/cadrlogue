;;; CADRLOGUE - A media cataloguing system.
;;; Copyright (C) 2021 Robert Smith <robert@stylewarning.com>
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


(defsystem #:cadrlogue
  :author "Robert Smith <robert@stylewarning.com>"
  :license "GPLv3"
  :description "Book and media cataloguing system"
  :depends-on (#:alexandria
               #:uiop
               #:drakma
               #:yason
               #:flexi-streams
               #:sqlite)
  :pathname "src/"
  :serial t
  :components ((:static-file "LICENSE.txt")
               (:file "package")
               (:file "http")
               (:file "db")
               (:file "barcode-printer")
               (:file "cadrlogue")))
