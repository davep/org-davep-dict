;;; org-dave-dict --- RFC2229 client for Common Lisp.
;;
;; packages.lisp --- Defines packages for org-dave-dict.
;; Copyright 2003,2004 by Dave Pearson <davep@davep.org>
;;
;; This software is Copyright (C) Dave Pearson <davep@davep.org> 2003,2004
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

;; Create the dict package.
(defpackage #:org.davep.dict
  (:nicknames #:dict #:dict-client #:rfc2229-client)
  (:use #:common-lisp #:split-sequence #:cl-ppcre)
  (:documentation "RFC 2229 client classes, functions and macros")
  (:export "+FIRST-DB+"                 ; Global constants.
           "+ALL-DB+"
           "+DEFAULT-MATCH-STRATEGY+"
           "+EXACT-MATCH-STRATEGY+"
           "+PREFIX-MATCH-STRATEGY+"
           "*DEFAULT-DICT-HOST*"        ; Global config variables.
           "*DEFAULT-DICT-PORT*"
           "*DEFAULT-CLIENT-ID*"
           "NAME"                       ; DICT-GENERAL-INFO
           "DESCRIPTION"
           "WORD"                       ; DICT-WORD-DEFINITION
           "DATABASE"
           "NAME"
           "DEFINITION"
           "DEFINITION-AS-STRING"
           "SEE-ALSO"
           "CODE"                       ; DICT-CLIENT-RESPONSE
           "RESPONSE"
           "DATA"
           "MAKE-DICT-CLIENT"           ; DICT-CLIENT
           "HOST"
           "PORT"
           "BANNER"
           "SERVER-DETAILS"
           "CAPABILITIES"
           "MESSAGE-ID"
           "CONNECTEDP"
           "CONNECT"
           "DISCONNECT"
           "DEFINE"
           "MATCH"
           "DATABASES"
           "STRATEGIES"
           "INFO"
           "SERVER-HELP"
           "SERVER-INFO"
           "WITH-DICT-CLIENT"))         ; High level utility stuff.

;;; packages.lisp ends here.
