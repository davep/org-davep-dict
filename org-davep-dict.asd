;;; org-davep-dict --- RFC2229 client for Common Lisp.
;;
;; org-davep-dict.asd --- asdf package defintion file.
;; Copyright 2003,2004 by Dave Pearson <davep@davep.org>
;; $Revision: 1.5 $
;;
;; This software is Copyright (C) Dave Pearson <davep@davep.org> 2003, 2004.
;;
;; Dave Pearson grants you the rights to distribute and use this software as
;; governed by the terms of the Lisp Lesser GNU Public License
;; <URL:http://opensource.franz.com/preamble.html>, known as the LLGPL.

;;; Commentary:
;;
;; The following code provides a set of RFC 2229 client classes, functions
;; and macros for Common Lisp. See <URL:http://www.dict.org/> for more
;; details about dictd.
;;
;; You can always find the latest version of this code at:
;;
;;   <URL:http://www.davep.org/lisp/#org-davep-dict>
;;
;; See <URL:http://www.davep.org/lisp/#org-davep-dictrepl> as a quick
;; example of how this code might be used.

(defpackage #:org-davep-dict-system
  (:use #:common-lisp #:asdf))

(in-package :org-davep-dict-system)

(defsystem org-davep-dict
  :name        "org-davep-dict"
  :author      "Dave Pearson <davep@davep.org>"
  :maintainer  "Dave Pearson <davep@davep.org>"
  :licence     "LLGPL"
  :version     "2.3"
  :description "RFC2229 client for Common Lisp."
  :long-description
  "org-davep-dict provides a set of RFC 2229 client classes, functions and macros
for Common Lisp. See See <URL:http://www.dict.org/> for more
details about dict servers and clients.

See <URL:http://www.davep.org/lisp/#org-davep-dict> for the latest version of
this package."
  :depends-on  (:split-sequence :acl-compat :cl-ppcre)
  :components  ((:file "packages")
                (:file "dict" :depends-on ("packages"))))

;;; org-davep-dict.asd ends here.
