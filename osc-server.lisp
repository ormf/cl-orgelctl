;;; 
;;; osc-server.lisp
;;;
;;; **********************************************************************
;;; Copyright (c) 2024 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>
;;;
;;; Revision history: See git repository.
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the Gnu Public License, version 2 or
;;; later. See https://www.gnu.org/licenses/gpl-2.0.html for the text
;;; of this agreement.
;;; 
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;; GNU General Public License for more details.
;;;
;;; **********************************************************************

;;; The osc server is connected to the pd patch and to additional
;;; optional registered clients like lisp clients etc. The protocol is
;;; very simple: Connections are established with an osc message
;;; "/register-client "si" <ip> <port>", <ip> and <port> are the
;;; callback address of the client. The server then sends back an id
;;; to the client using the osc-message "/client-id "s" <client-id>"
;;; and then the complete current state of the papierrohrorgel.
;;;
;;; The client-id is used used as first parameter when sending osc
;;; messages to the server to avoid feedbacks in state update.


(in-package :cl-orgelctl)

(defparameter *orgel-server-osc-responder* (make-hash-table))

(defparameter *clients* (make-hash-table :test #'equal))
(defparameter *lisp-server-port* 3011)
(defparameter *oscin-lisp-server* nil)
(defparameter *curr-client-id* 0)

(defparameter *local-host* "127.0.0.1")
;;; (defparameter *pd-in-port* 3011)
(defparameter *pd-out-host* "127.0.0.1")
(defparameter *pd-out-port* 3010)

(defparameter *osc-responder-registry*
  (make-array *orgelcount*
              :initial-contents (loop
                                  for i below *orgelcount*
                                  collect (make-instance 'orgel-registry))))
#|
(loop for target in *orgel-global-targets*
collect `(setf (,(read-from-string (format nil "orgel-registry-~a" target)) (aref *orgel-responder-registry* orgelidx)) nil))

|#

;;; The funcalls are spelled out for speed. Too lazy to do it with a macro...

(defun start-lisp-server ()
  (format t "~&starting lisp server...")
  (when *oscin-lisp-server*
    (incudine.osc:close *oscin-lisp-server*))
  (setf *oscin-lisp-server*
        (incudine.osc:open :host *local-host*
                           :port *lisp-server-port*
                           :direction :input))
  (incudine:remove-all-responders *oscin-lisp-server*)
  (make-all-server-responders)
  (incudine:make-osc-responder
   *oscin-lisp-server*
   "/connect-client"
   "si"
   (lambda (host port)
     (incudine.util:msg :info "connect-client: ~a ~a" host port)
     (let ((id (register-client host port)))
       (incudine.util:msg :info "registered new lisp client: ~a" id))))
  (incudine:make-osc-responder
   *oscin-lisp-server*
   "/disconnect-client"
   "s"
   (lambda (client)
     (incudine.util:msg :info "disconnect-client: ~a" client)
     (unregister-client client)
     (incudine.util:msg :info "unregistered lisp client: ~a" client)))
  (incudine:recv-start *oscin-lisp-server*)
  (register-pd-client *pd-out-host* *pd-out-port*)
  (format t "lisp server started~%"))

(defclass client ()
  ((id :initarg :id :accessor id)
   (host :initarg :host :accessor host)
   (port :initarg :port :accessor port)
   (oscout :initarg :oscout :accessor oscout)))

(defun new-client-id ()
  (format nil "client~d" (incf *curr-client-id*)))

(defparameter *lisp-server-osc-receiver* nil)

(defun register-pd-client (host port)
  (or (gethash "papierrohrorgel-pd" *clients*)
      (setf (gethash "papierrohrorgel-pd" *clients*)
            (make-instance 'client
                           :host host
                           :port port
                           :id "papierrohrorgel-pd"
                           :oscout (incudine.osc:open :host host :port port :direction :output))))
  (send-orgel-state (gethash "papierrohrorgel-pd" *clients*)))

;;; (unregister-client "pd")

;;; (register-pd-client *pd-out-host* *pd-out-port*)

(defun register-client (host port)
  (incudine.util:msg :info "register-client ~a ~a" host port)
  (let* ((old-client
          (block nil
            (maphash (lambda (id client)
                       (declare (ignore id))
                       (when (and
                              (string= host (host client))
                              (eql port (port client)))
                         (return client)))
                     *clients*)))
         (curr-client (or old-client
                          (make-instance 'client
                                         :host host
                                         :port port
                                         :id (new-client-id)
                                         :oscout (incudine.osc:open :host host :port port :direction :output)))))
    (unless old-client (setf (gethash (id curr-client) *clients*) curr-client ))
    (incudine.osc:message (oscout curr-client) "/client-id" "s" (id curr-client))
    (send-orgel-state curr-client)
    (id curr-client)))

(defun unregister-client (client-or-id)
  (unless
      (remhash
       (typecase client-or-id
         (string client-or-id)
         (client (id client-or-id)))
       *clients*)
    (warn "Couldn't unregister client: ~S not found" client-or-id)))

(defun send-orgel-state (client)
  (let ((oscout (oscout client)))
    (dotimes (orgelidx *orgelcount*)
      (let ((orgel-name (orgel-name (1+ orgelidx))))
        (dolist (target-sym *orgel-global-target-syms*)
          (let ((val (val (slot-value (aref *curr-state* orgelidx) target-sym))))
            (incudine:at (incudine:now) #'incudine.osc:message oscout
                         (format nil "/~a/~a" orgel-name target-sym) "f" (float val 1.0))))
        (dolist (target-sym *orgel-fader-target-syms*)
          (dotimes (faderidx 16)
            (let ((val (val (aref (slot-value (aref *curr-state* orgelidx) target-sym) faderidx))))
              (incudine:at (incudine:now) #'incudine.osc:message oscout
                           (format nil "/~a/~a" orgel-name target-sym) "ff" (float (1+ faderidx) 1.0) (float val 1.0)))))))))



;;; (incudine:at (incudine:now) #'incudine.osc:message (oscout client) (format nil "/~a/~a" orgel-name slot-key) "f"  (float val 1.0))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; server responders
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro define-server-orgel-fader-responder (stream orgelidx target)
  "responder for the fader controllers of the 16 partials (level, delay,
bp, gain, osc-level)."
  `(progn
     (list ,target
           (let ((target-sym ',(read-from-string (format nil "~a" (symbol-value target)))))
             (incudine::make-osc-responder
              ,stream ,(format nil "/orgel~2,'0d/~a" (1+ orgelidx) (symbol-value target)) "sff"
              (lambda (client faderno value)
                (set-cell (aref (slot-value (aref *curr-state* ,orgelidx) target-sym) (1- (round faderno))) value :src client)
                (incudine.util:msg :info "orgel~2,'0d in: ~S ~a ~a ~a~%" ,(1+ orgelidx) client ,target (round faderno) value)
                ))))))

(defmacro get-server-orgel-fader-responders (stream orgelidx targets)
  `(append
     ,@(loop for target in (symbol-value targets)
             collect `(define-server-orgel-fader-responder ,stream ,orgelidx ,target))))

(defmacro define-server-orgel-global-responder (stream orgelidx target)
  "responder for the global parameters of the organ (ramps, base-freq,
amps, etc.)"
  `(list ,target
         (let ((target-sym ',(read-from-string (format nil "~a" (symbol-value target)))))
           (incudine::make-osc-responder
            ,stream ,(format nil "/orgel~2,'0d/~a" (1+ orgelidx) (symbol-value target)) "sf"
            (lambda (client value)
              (set-cell (slot-value (aref *curr-state* ,orgelidx) target-sym) value :src client)
              (incudine.util:msg :info "orgel~2,'0d in: ~S ~a ~a~%" ,(1+ orgelidx) client ,target value))))))

;;; (define-server-orgel-global-responder 'osc-stream 0 :base-freq)

(defmacro get-server-orgel-global-responders (stream orgelidx targets)
  `(append
     ,@(loop for target in (symbol-value targets)
             collect `(define-server-orgel-global-responder ,stream ,orgelidx ,target))))

(defmacro define-server-orgel-level-meter-responder (stream orgelidx target)
  "responder for the 16 output level meters."
  `(list ,target
         (incudine::make-osc-responder
          ,stream ,(format nil "/orgel~2,'0d/~a" (1+ orgelidx) (symbol-value target)) "sff"
          (lambda (client faderno value)
            (set-cell (aref (aref *orgel-mlevel* ,orgelidx) (1- (round faderno))) value :src client)
            (incudine.util:msg :info "orgel~2,'0d: ~S ~a ~a ~a~%" ,(1+ orgelidx) client ,target (round faderno) value)))))

(defmacro get-server-orgel-level-meter-responders (stream orgelidx targets)
  `(append
     ,@(loop for target in (symbol-value targets)
             collect `(define-server-orgel-level-meter-responder ,stream ,orgelidx ,target))))

(defmacro define-server-preset-responder (stream path fn)
  `(incudine:make-osc-responder
    ,stream
    ,(format nil "/preset-ctl/~a" path)
    "s"
    (lambda (client)
      ,fn
      (incudine.util:msg :info "preset-ctl: ~S ~a~%" client ,path))))

(defmacro define-server-orgel-ccin-responder (stream)
  "responder for external ccin."
  `(list :ccin
         (incudine::make-osc-responder
          ,stream "/ccin"  "sfff"
          (lambda (client ccval ccnum channel)
            (setf (val (aref (aref *midi-cc-state* channel) ccnum)) ccval)
            (incudine.util:msg :info "ccin: ~S ~a ~a ~a~%" client ccnum ccval channel)))))

(defmacro define-server-orgel-notein-responder (stream)
  "responder for external notein."
  `(list :notein
         (incudine::make-osc-responder
          ,stream "/notein"  "sfff"
          (lambda (client keynum velo channel)
            (setf (val (aref (aref *midi-note-state* channel) keynum)) velo)
            (incudine.util:msg :info "ccin: ~S ~a ~a ~a~%" client keynum velo channel)))))

(defmacro get-server-preset-responders (stream)
  `(list
     (define-server-preset-responder ,stream "prev-preset" '(previous-orgel-preset))
     (define-server-preset-responder ,stream "next-preset" '(next-orgel-preset))
     (define-server-preset-responder ,stream "recall-preset" '(recall-orgel-preset *curr-orgel-preset-nr*))
     (define-server-preset-responder ,stream "store-preset" '(store-orgel-preset *curr-orgel-preset-nr*))
     (define-server-preset-responder ,stream "load-presets" '(load-orgel-presets))
     (define-server-preset-responder ,stream "save-presets" '(save-orgel-presets))
     (incudine:make-osc-responder
      ,stream
      "/preset-ctl/preset-no" "sf"
      (lambda (client f)
        (setf *curr-orgel-preset-nr* (round f))
        (incudine.util:msg :info "preset-ctl: ~S preset-no ~a~%" client (round f))))))

#|
(defun define-orgel-plist-responders (stream)
  (incudine.util:msg :info "defining plist responders on ~a." stream)
  `(let ((curr-plist nil) (trigger nil))
     (incudine:make-osc-responder
      ,stream "/plist-ctl/start" ""
      (lambda ()
        (setf curr-plist nil)
        (incudine.util:msg :info "plist-ctl: start")))
     (incudine:make-osc-responder
      ,stream "/plist-ctl/stop" ""
      (lambda ()
        (incudine.util:msg :info "plist: ~a" curr-plist)
        (switch-targets (reverse curr-plist) :old '*global-targets* :trigger trigger)
        (setf curr-plist nil)
        (incudine.util:msg :info "plist-ctl: stop~%")))
     (incudine:make-osc-responder
      ,stream "/plist-ctl/fader" "sfff"
      (lambda (target orgelno partial value)
        (push (list (my-make-symbol (string-upcase target)) (round orgelno) (round partial) value) curr-plist)
        (incudine.util:msg :info "plist-ctl/fader: ~a" (list (my-make-symbol (string-upcase target)) (round orgelno) (round partial) value) curr-plist orgelno partial value)))))
|#

(defmacro %make-all-server-responders ()
  (let ((maxorgel (eval '*orgelcount*))
        (stream '*oscin-lisp-server*))
    `(progn
       (incudine:remove-all-responders ,stream)
       (get-server-preset-responders ,stream)
       ;;       ,(define-orgel-plist-responders stream)
       ,@(loop
           for orgelidx below maxorgel
           collect `(setf (gethash ,(ou:make-keyword (format nil "orgel~2,'0d" (1+ orgelidx))) *orgel-server-osc-responder*)
                          (append
                           (get-server-orgel-fader-responders ,stream ,orgelidx *orgel-fader-targets*)
                           (get-server-orgel-global-responders ,stream ,orgelidx *orgel-global-targets*)
                           (get-server-orgel-level-meter-responders ,stream ,orgelidx *orgel-level-meter-targets*)
                           

;;;                           (define-orgel-ccin-responder ,stream)
;;;                           (define-orgel-notein-responder ,stream)
                           )))
       nil)))

(defun make-all-server-responders ()
  (%make-all-server-responders))

;;; (make-all-server-responders)

(defun orgel-ctl-fader (orgel target partial val)
  (let ((orgelidx (gethash orgel *orgeltargets*)))
    (unless orgelidx (error "Orgel \"~S\" doesn't exist" orgel))
;;;  (break "orgel: ~a ~a ~a ~a" orgel target idx val)
    (set-cell
     (aref
      (slot-value (aref *curr-state* orgelidx)
                  (target-key->sym target))
      (1- partial))
     (float val 1.0))))

;;; (orgel-ctl-fader :orgel04 :level 2 0.5)


(declaim (inline target-key))
(defun target-key (target)
  (if (keywordp target) target
      (format nil "orgel~2,'0d" target)))

#|
(defun orgel-ctl (orgeltarget target val)
  (let ((form (cond ((listp target) target)
                    ((keywordp target) (gethash target *observed*))
                    (t (list target))))
        (orgeltarget (target-key orgeltarget)))
    (incudine.util:msg :info (format nil "/~a/~a" orgeltarget (first form)))
    (unless form (error "target ~S doesn't exist" target))
    (if (cdr form)
        (incudine.osc:message *oscout* (format nil "/~a/~a" orgeltarget (first form)) "if"
                              (second form) (float val 1.0))
        (incudine.osc:message *oscout* (format nil "/~a/~a" orgeltarget (first form)) "f" (float val 1.0)))))
|#

(defun orgel-ctl (orgel target val)
  (if (eql orgel :all)
      (dotimes (orgelidx *orgelcount*)
        (set-cell (slot-value (aref *curr-state* orgelidx)
                          (target-key->sym target))
              (float val 1.0)))
  (let ((orgelidx (gethash orgel *orgeltargets*)))
    (unless orgelidx (error "Orgel \"~S\" doesn't exist" orgel))
;;;  (break "orgel: ~a ~a ~a ~a" orgel target idx val)
    (set-cell (slot-value (aref *curr-state* orgelidx)
                          (target-key->sym target))
              (float val 1.0)))))

;;; (orgel-ctl :orgel01 :base-freq 431)

#|
(defun global-to-pd (orgeltarget target val)
  "send global orgel slot values to pd via osc."
  (let ((form (cond ((listp target) target)
                    ((keywordp target) (gethash target *observed*))
                    (t (list target))))
        (orgeltarget (target-key orgeltarget)))
    (unless form (error "target ~S doesn't exist" target))
    (unless (eq (incudine:rt-status) :started) (error "Incudine dsp engine not started!"))
    (if (cdr form)
        (let ((address (format nil "/~a/~a" orgeltarget (first form))))
          (incudine.util:nrt-msg :info "osc-out: ~a ~a ~a ~a" address "if" (second form) (float val 1.0))
          (incudine:at (incudine:now) (lambda () (incudine.osc:message *oscout* address "if" (second form) (float val 1.0)))))
        (let ((address (format nil "/~a/~a" orgeltarget (first form))))
          (incudine.util:nrt-msg :info "osc-out: ~a ~a ~a" address "f" (float val 1.0))
          (incudine:at (incudine:now) (lambda () (incudine.osc:message *oscout* address "f" (float val 1.0))))
         ;;; (incudine.osc:message *oscout* address "f" (float val 1.0))
          ))))

(defun fader-to-pd (orgel target idx val)
  "send orgel fader slot values to pd via osc."
  (unless (gethash orgel *orgeltargets*) (error "Orgel \"~S\" doesn't exist" orgel))
  (unless (eq (incudine:rt-status) :started) (error "Incudine dsp engine not started!"))
;;;  (break "orgel: ~a ~a ~a ~a" orgel target idx val)
  (let ((address (format nil "/~a/~a" orgel target)))
    (incudine.util:nrt-msg :info "osc-out: ~a ~a ~a ~a" address "if" (round idx) (float val 1.0))
;;;    (incudine.osc:message *oscout* address "if" (round idx) (float val 1.0))
    (incudine:at (incudine:now) (lambda () (incudine.osc:message *oscout* address "if" (round idx) (float val 1.0))))))


|#

;; (start-lisp-server)


;; (unregister-client "client2")

#|

*clients*
*orgel-server-osc-responder*
*orgel-osc-responder*
(setf (incudine.util:logger-level) :info)

(incudine.util:msg :info "Hallo")

(length (gethash *oscin-lisp-server* incudine::*responders*))

(equal (type-of "hallo") 'array)
(unregister-client (gethash "client3" *clients*))

(unregister-client "client2")

(register-client "192.168.67.20" 3012)
(register-client "192.168.67.21" 3012)

*clients*

|#
