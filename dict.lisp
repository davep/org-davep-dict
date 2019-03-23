;;; org-davep-dict --- RFC2229 client for Common Lisp.
;;
;; dict.lisp --- Main code for org-davep-dict.
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
;;   <URL:https://github.com/davep/org-davep-dict>
;;
;; See <URL:https://github.com/davep/org-davep-dictrepl> as a quick example
;; of how this code might be used.

;;; TODO:
;;
;; o Add support for AUTH.

;;; Code:

(in-package :org.davep.dict)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global default variables.

(defvar *default-dict-host* "localhost"
  "Host name to use when a default host is required.")

(defvar *default-dict-port* 2628
  "Port to use when a default port is required.")

(defvar *default-client-id* "org-davep-dict"
  "The default client ID that will be announced to the dictionary server.

If you would prefer that your application announces itself in a different
way then you should modify the value of this variable.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global constants

(eval-when (:compile-toplevel :load-toplevel :execute)

  (unless (boundp '+first-db+)
    (defconstant +first-db+ "!"
      "Database name to use when the first definition/match is required."))

  (unless (boundp '+all-db+)
    (defconstant +all-db+ "*"
      "Database name to use when all definitions/matches are required."))

  (unless (boundp '+default-match-strategy+)
    (defconstant +default-match-strategy+ "."
      "Match strategy name meaning \"the server's default\"."))

  (unless (boundp '+exact-match-strategy+)
    (defconstant +exact-match-strategy+ "exact"
      "Match strategy name for an exact match."))

  (unless (boundp '+prefix-match-strategy+)
    (defconstant +prefix-match-strategy+ "prefix"
      "Match strategy name for a prefix match.")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utility functions and macros.

(defmacro with-safe-reading (&body body)
  `(with-standard-io-syntax
    (let ((*read-eval* nil))
      ,@body)))

(defun read-string-as-list (string)
  "Take STRING and read it as a list."
  (read-from-string (format nil "(~A)" string)))

(defun read-string-as-list-preserving-case (string)
  "Take STRING and read it as a list while preserving case."
  (let ((*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :preserve)
    (read-string-as-list string)))

(defun end-of-data-p (line)
  "Is LINE an end of data marker?"
  (string= line "."))

(defun keyword->dict-string (keyword)
  "Convert KEYWORD into a dict string constant."
  (if (keywordp keyword)
      (case keyword
        (:first   +first-db+)
        (:all     +all-db+)
        (:default +default-match-strategy+)
        (:exact   +exact-match-strategy+)
        (:prefix  +prefix-match-strategy+)
        (otherwise (symbol-name keyword)))
    keyword))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dict response class.

(defclass dict-client-response ()
  ((code
    :accessor      code
    :type          integer
    :documentation "The numeric code associated with the response.")
   (response
    :accessor      response
    :type          string
    :documentation "The text of the response received from the server.")
   (data
    :accessor      data
    :type          list
    :initform      nil
    :documentation "Any data associated with the response."))
  (:documentation "Class to hold a response from a dictionary server.

This class has three slots:

CODE     - The numeric code associated with the response.
RESPONSE - The text of the response received from the server.
DATA     - Any data associated with the response."))

(defmethod read-response ((response dict-client-response) (text string))
  "Break up the response TEXT and populate the slots in RESPONSE.

RESPONSE is returned."
  (multiple-value-bind (code text-offset) (with-safe-reading (read-from-string text))
    (cond ((integerp code)
           (setf (code     response) code
                 (response response) (subseq text text-offset)))
          (t
           (error "Unexpected response from dictionary server: \"~A\"" text))))
  response)

(defmethod preliminary-reply-p ((response dict-client-response))
  "Is RESPONSE a preliminary reply?"
  (< 99 (code response) 200))

(defmethod completion-reply-p ((response dict-client-response))
  "Is RESPONSE a completion reply?"
  (< 199 (code response) 300))

(defmethod intermediate-reply-p ((response dict-client-response))
  "Is RESPONSE an intermediate reply?"
  (< 299 (code response) 400))

(defmethod temporary-error-p ((response dict-client-response))
  "Is RESPONSE a temporary error?"
  (< 399 (code response) 500))

(defmethod permanent-error-p ((response dict-client-response))
  "Is RESPONSE a permanent error?"
  (< 499 (code response) 600))

(defmethod errorp ((response dict-client-response))
  "Is RESPONSE any kind of error?"
  (or (temporary-error-p response) (permanent-error-p response)))

(defmethod print-object ((response dict-client-response) (stream stream))
  "Format the DICT-CLIENT-RESPONSE for easy reading when output to STREAM."
  (print-unreadable-object (response stream :type t)
    (format stream "~S ~S" (code response) (response response))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dict general information class.

(defclass dict-general-info ()
  ((name
    :accessor      name
    :type          string
    :documentation "The name attribute of the information item.")
   (description
    :accessor      description
    :type          string
    :documentation "The description attribute of the information item."))
  (:documentation "Class to hold general information returned from the server.

This class is used for holding general information that is returned from the
dictionary server, this includes things like items in the list of databases
or the list of match stratagies.

This class has two slots:

NAME        - The name attribute of the information item.
DESCRIPTION - The description attribute of the information item."))

(defmethod read-info ((info dict-general-info) (text string))
  "Break up TEXT and populate the slots in INFO.

INFO is returned."
  (let ((info-items (with-safe-reading (read-string-as-list-preserving-case text))))
    (setf (name info)        (symbol-name (first info-items))
          (description info) (second info-items)))
  info)

(defmethod print-object ((info dict-general-info) (stream stream))
  "Format the INFO for easy reading when output to STREAM."
  (print-unreadable-object (info stream :type t)
    (format stream "~S ~S" (name info) (description info))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dict word definition class.

(defclass dict-word-definition ()
  ((word
    :accessor      word
    :type          string
    :documentation "The word that is being defined.")
   (database
    :accessor      database
    :type          string
    :documentation "The name of the database that the definition came from.")
   (name
    :accessor      name
    :type          string
    :documentation "The long name of the database that the definition came from.")
   (definition
    :accessor      definition
    :initform      nil
    :type          list
    :documentation "The definition of the word.
This is a list of strings, each item in the list is a line from the
definition."))
  (:documentation "Class to hold a word definition.

This class is used to hold a word definition, it has the following slots:

WORD       - The word that is being defined.
DATABASE   - The name of the database that the definition came from.
NAME       - The long name of the database that the definition came from.
DEFINITION - The definition of the word. This is a list of strings, each
             item in the list is a line from the definition.

Methods associated with this class are:

SEE-ALSO - Get a list of links found in the defintion."))

(defmethod read-word ((definition dict-word-definition) (header dict-client-response))
  "Break up the HEADER and populate the slots in DEFINITION.

DEFINITION is returned."
  (let ((info (with-safe-reading (read-string-as-list-preserving-case (response header)))))
    (setf (word definition)     (first info)
          (database definition) (symbol-name (second info))
          (name     definition) (third info)))
  definition)

(defmethod definition-as-string ((definition dict-word-definition) &key (strip-leading-whitespace nil))
  "Return the text of DEFINITION as a string."
  (apply #'concatenate (cons 'string (loop for line in (definition definition)
                                           for start = (when strip-leading-whitespace (position #\Space (the string line) :test #'char/=))
                                           if start collect (subseq line (max 0 (1- start)))
                                           else collect line))))

(defmethod see-also ((definition dict-word-definition))
  "Get a list of \"see also\" items for the definition."
  ;; Get the defintion as one long string and reduce any multiple highlight
  ;; markers down to single highlights.
  (let ((text (the string (regex-replace-all "{{+" (regex-replace-all "}}+" (definition-as-string definition :strip-leading-whitespace t) "}") "{"))))
    ;; Pull out any highlighted text.
    (loop for start = (position #\{ text) then (position #\{ text :start (1+ start))
          while start collect (subseq text (1+ start) (position #\} text :start start)))))

(defmethod print-object ((definition dict-word-definition) (stream stream))
  "Format the DICT-WORD-DEFINITION for easy reading when output to STREAM."
  (print-unreadable-object (definition stream :type t)
    (format stream "~S ~S ~S" (word definition) (database definition) (name definition))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dict client class.

(defclass dict-client ()
  ((host
    :accessor      host
    :initarg       :host
    :initform      *default-dict-host*
    :type          string
    :documentation "The name of the dictionary host that the client will connect to.
The default value for this slot is the value of `*DEFAULT-DICT-HOST*'.")
   (port
    :accessor      port
    :initarg       :port
    :initform      *default-dict-port*
    :type          integer
    :documentation "The number of the port that the client will connect to.
The default value for this slot is the value of `*DEFAULT-DICT-PORT*'.")
   (socket
    :accessor      socket
    :initform      nil
    :type          (or null stream)
    :documentation "The socket used to talk to the host.")
   (banner
    :accessor      banner
    :initform      nil
    :type          (or null dict-client-response)
    :documentation "The banner message we got when connected to the host.
This slot, when populated, is a `DICT-CLIENT-RESPONSE' object.")
   (server-details
    :accessor      server-details
    :initform      ""
    :type          string
    :documentation "The server details as pulled from the banner after a successful
connection.")
   (capabilities
    :accessor      capabilities
    :initform      nil
    :type          list
    :documentation "Server's capability list. A list of keywords.")
   (message-id
    :accessor      message-id
    :initform      ""
    :type          string
    :documentation "The message ID given to us after a successful connection."))
  (:documentation "RFC2229 client class.

This class has the following slots:

HOST           - The name of the dictionary host.
PORT           - The dictionary port on the host.
SOCKET         - When connected this is the socket stream that is being used.
BANNER         - An instance of `DICT-CLIENT-RESPONSE' that contains the
                 banner we got from the dictionary server when we connected.
SERVER-DETAILS - The server details as pulled from the BANNER.
CAPABILITIES   - List of server capabilities as pulled from the BANNER.
MESSAGE-ID     - The message-ID for this connection as pulled from the BANNER.

Methods associated with this class are:

CONNECTEDP  - Check if the client is connected.
CONNECT     - Connect to the server.
DISCONNECT  - Disconnect from the server.
DEFINE      - Get the defintions for a word.
MATCH       - Get a list of matches for a word.
DATABASES   - Get a list of databases on the server.
STRATAGIES  - Get a list of matching stratagies supported by the server.
INFO        - Get information about a database.
SERVER-HELP - Get the help text of the server.
SERVER-INFO - Get information about the server.

Functions associted with this class are:

MAKE-DICT-CLIENT - Create a dictionary client object."))

(defun make-dict-client (&key (host *default-dict-host*) (port *default-dict-port*))
  "Create a dictionary client."
  (make-instance 'dict-client :host host :port port))

(defmethod connectedp ((dict-client dict-client))
  "Is DICT-CLIENT connected to a dictionary server?"
  (not (null (socket dict-client))))

(defmethod connected-check ((dict-client dict-client))
  "Check that DICT-CLIENT is connected to a server, error if not."
  (unless (connectedp dict-client)
    (error "Not connected to a dictionary server")))

(defmethod print-object ((dict-client dict-client) (stream stream))
  "Format the DICT-CLIENT for easy reading when output to STREAM."
  (print-unreadable-object (dict-client stream :type t)
    (format stream "~A:~D (~:[not ~;~]connected)"
            (host dict-client)
            (port dict-client)
            (connectedp dict-client))))

(defmethod get-line :before ((dict-client dict-client))
  "Ensure that we're connected before getting a line."
  (connected-check dict-client))

(defmethod get-line ((dict-client dict-client))
  "Read a line from the dictionary server."
  (let* ((line   (read-line (socket dict-client)))
         (length (length line)))
    (if (and (> length 0) (char= (char line (1- length)) #\Return))
        (subseq line 0 (1- length))
      line)))

(defmethod put-line :before ((dict-client dict-client) (line string))
  "Ensure that we're connected before putting a line."
  (connected-check dict-client))

(defmethod put-line ((dict-client dict-client) (line string))
  "Send a line to the dict server."
  (format (socket dict-client) "~A~C~C" line #\Return #\NewLine)
  (finish-output (socket dict-client)))

(defmethod get-short-response ((dict-client dict-client))
  "Get a short (one line) response from the dict server."
  (read-response (make-instance 'dict-client-response) (get-line dict-client)))

(defmethod get-long-response ((dict-client dict-client))
  "Get a long (multi line) response from the dict server."
  ;; Get the initial response line from the server.
  (let ((response (get-short-response dict-client)))
    ;; If it didn't indicate a problem.
    (unless (errorp response)
      ;; Gather up the data associated with the response.
      (setf (data response)
            (loop for line = (get-line dict-client)
                  until (end-of-data-p line)
                  collect line))
      ;; Eat the trailing response line that comes after the EOD marker.
      (let ((final-response (get-short-response dict-client)))
        (when (errorp final-response)
          (error "Unexpected final response from server: \"~A\"" (response final-response)))))
    response))

(defmethod short-command ((dict-client dict-client) (command string))
  "Send COMMAND to server and get its single line response."
  (put-line dict-client command)
  (get-short-response dict-client))

(defmethod long-command ((dict-client dict-client) (command string))
  "Send COMMAND to server and get its multi-line response."
  (put-line dict-client command)
  (get-long-response dict-client))

(defmethod info-command ((dict-client dict-client) (command string))
  "Send COMMAND to the server and grahter up the resulting information."
  (let ((response (long-command dict-client command)))
    (unless (errorp response)
      (setf (data response)
            (loop for line in (data response)
                  collect (read-info (make-instance 'dict-general-info) line))))
    response))

(defun cap-string->keyword-list (cap-string)
  "Convert CAP-STRING to a list of keywords."
  (declare (type string cap-string))
  (mapcar (lambda (cap)
            (intern (string-upcase cap) :keyword))
          (delete-if (lambda (cap)
                       (string= cap ""))
                     (split-sequence #\. cap-string))))

(defmethod populate-slots-from-banner ((dict-client dict-client))
  "Populate various slots from the banner.

DICT-CLIENT is returned."
  (let* ((text      (the string (response (banner dict-client))))
         (cap-start (position #\< text                       :test #'char=))
         (cap-end   (position #\> text :start (1+ cap-start) :test #'char=))
         (id-start  (position #\< text :start (1+ cap-end)   :test #'char=))
         (id-end    (position #\> text :start (1+ id-start)  :test #'char=)))
    (setf (server-details dict-client) (subseq text 0 (1- cap-start))
          (capabilities dict-client)   (cap-string->keyword-list (subseq text (1+ cap-start) cap-end))
          (message-id dict-client)     (subseq text id-start (1+ id-end)))
    dict-client))

(defmethod connect :before ((dict-client dict-client))
  "Ensure that we're not already connected."
  (when (connectedp dict-client)
    (error "Already connected to host ~A on port ~A" (host dict-client) (port dict-client))))

(defmethod connect ((dict-client dict-client))
  "Connect the dictionary client to the dictionary server."
  ;; Open the connection to the server.
  (setf (socket dict-client) (acl-socket:make-socket :remote-host (host dict-client) :remote-port (port dict-client) :format :text))
  ;; Read back the banner.
  (setf (banner dict-client) (read-response (make-instance 'dict-client-response) (get-line dict-client)))
  ;; Did the banner indicate any sort of error?
  (if (errorp (banner dict-client))
      ;; Yes, throw an error.
      (error "Can't connect to server, reason: \"~A\"" (response (banner dict-client)))
    ;; The banner was ok, send our announcement to the server
    (if (errorp (short-command dict-client (format nil "client ~A" *default-client-id*)))
        (error "Server didn't like our client announcement.")
      ;; If we made it this far then everything is ok. Pull the useful
      ;; information out of the banner and return the client object.
      (populate-slots-from-banner dict-client))))

(defmethod disconnect :before ((dict-client dict-client))
  "Ensure that we're connected before we disconnect."
  (connected-check dict-client))

(defmethod disconnect ((dict-client dict-client))
  "Disconnect the dictionary client from the dictionary server."
  (unless (errorp (short-command dict-client "quit"))
    (close (socket dict-client))
    (setf (socket dict-client) nil)))

(defmethod read-definition ((definition dict-word-definition) (dict-client dict-client))
  "Read the definition of a word from DICT-CLIENT."
  (setf (definition definition) (loop for line = (get-line dict-client)
                                      until (end-of-data-p line)
                                      collect line))
  definition)

(defmethod define ((dict-client dict-client) (word string) &key (database :all))
  "Define a word.

A list of definitions is returned, the list being empty if no definitions are
found. Where definitions are found the list comprised of objects of the class
`DICT-WORD-DEFINITION'."
  (when (preliminary-reply-p (short-command dict-client (format nil "define ~A \"~A\"" (keyword->dict-string database) word)))
    (loop for response = (get-short-response dict-client)
          until (completion-reply-p response)
          when (preliminary-reply-p response)
          collect (read-definition (read-word (make-instance 'dict-word-definition) response) dict-client))))

(defmethod match ((dict-client dict-client) (word string) &key (database :all) (strategy :default))
  "Match a word.

An object of the class `DICT-CLIENT-RESPONSE' is returned."
  (info-command dict-client (format nil "match ~A ~A \"~A\"" (keyword->dict-string database) (keyword->dict-string strategy) word)))

(defmethod databases ((dict-client dict-client))
  "Get a list of databases available on the server.

An object of the class `DICT-CLIENT-RESPONSE' is returned."
  (info-command dict-client "show db"))

(defmethod strategies ((dict-client dict-client))
  "Get a list of strategies available on the server.

An object of the class `DICT-CLIENT-RESPONSE' is returned."
  (info-command dict-client "show strat"))

(defmethod info ((dict-client dict-client) (database string))
  "Get information about DATABASE.

An object of the class `DICT-CLIENT-RESPONSE' is returned."
  (long-command dict-client (format nil "show info \"~A\"" database)))

(defmethod server-info ((dict-client dict-client))
  "Get information about the server.

An object of the class `DICT-CLIENT-RESPONSE' is returned."
  (long-command dict-client "show server"))

(defmethod server-help ((dict-client dict-client))
  "Request help from the dictionary server.

An object of the class `DICT-CLIENT-RESPONSE' is returned."
  (long-command dict-client "help"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; High level utility stuff.

(defmacro with-dict-client ((client &key (host *default-dict-host*) (port *default-dict-port*)) &body body)
  "Create a dictionary client called CLIENT and evaluate BODY."
  `(let ((,client (make-dict-client :host ,host :port ,port)))
     (connect ,client)
     (unwind-protect
         (progn ,@body)
       (when (connectedp ,client)
         (disconnect ,client)))))

;;; dict.lisp ends here.
