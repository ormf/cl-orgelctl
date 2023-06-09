;;; 
;;; orgel-accessor-fns.lisp
;;;
;;; definition of getter and setter functions for all slots of the
;;; papierorgel. the orgeltarget can be specified either as
;;; orgelnummer or as keyword.
;;;
;;; Examples:
;;;
;;; getter functions:
;;;
;;; (level :orgel01 2)
;;; (level 1 2)
;;; (base-freq :orgel02)
;;; (base-freq 2)
;;;
;;; setter functions:
;;; 
;;; (setf (level :orgel01 2) 0.3)
;;; (setf (level 1 2) 0.3)
;;; (setf (base-freq :orgel02) 271)
;;; (setf (base-freq 2) 271)
;;;
;;; **********************************************************************
;;; Copyright (c) 2022 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>
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

(in-package :cl-orgelctl)

(defun orgel-access-fn (target)
  "retrieve function object by keyword of its name."
  (symbol-function (intern (string-upcase (format nil "orgel-~a" target)) :cl-orgelctl)))

;;; (orgel-access-fn :level) -> #<function orgel-level>

(defun orgel-slot-name (target)
  "convert keyword to symbol"
  (read-from-string (format nil "orgel-~a" target)))

;;; (orgel-slot-name :level) -> orgel-level 

;;; utility shorthand access fns for the organ slots in *curr-state*

(declaim (inline get-orgelidx))

(defmacro define-orgel-fader-access-fn (target)
  `(progn
     (defun ,(intern (string-upcase (format nil "~a" target))) (orgelnummer idx)
       ,(format nil "access function for the ~a slot with index <idx>
of orgel at <orgelnummer> in *curr-state*." target)
       (let ((orgelidx (gethash orgelnummer *orgeltargets*)))
         (aref (,(intern (string-upcase (format nil "orgel-~a" target)) :cl-orgelctl)
                (aref *curr-state* orgelidx))
               (1- idx))))
     (defsetf ,(intern (string-upcase (format nil "~a" target))) (orgelnummer idx) (value)
       ,(format nil "access function for the ~a slot with index <idx>
of orgel at <orgelnummer> in *curr-state*." target)
       `(orgel-ctl ,orgelnummer `(,,,target ,,idx) ,value))))

(defmacro define-all-orgel-fader-access-fns (targets)
  `(progn
     ,@(loop
         for target in (eval targets)
         collect (list 'define-orgel-fader-access-fn target))))

(define-all-orgel-fader-access-fns *orgel-fader-targets*)

;; (define-orgel-fader-access-fn :level)
;; (define-orgel-fader-access-fn :gain)
;; (define-orgel-fader-access-fn :delay)
;; (define-orgel-fader-access-fn :q)
;; (define-orgel-fader-access-fn :osc-level)

(defmacro define-orgel-global-access-fn (target)
  `(progn
     (defun ,(intern (string-upcase (format nil "~a" target))) (orgelnummer)
       ,(format nil "access function for the ~a slot of orgel at <orgelnummer> in *curr-state*." target)
       (let ((orgelidx (gethash orgelnummer *orgeltargets*)))
         (,(intern (string-upcase (format nil "orgel-~a" target)) :cl-orgelctl)
          (aref *curr-state* orgelidx))))
     (defsetf ,(intern (string-upcase (format nil "~a" target))) (orgelnummer) (value)
       `(orgel-ctl ,orgelnummer ,,target ,value))))

(defmacro define-all-orgel-global-access-fns (targets)
  `(progn
     ,@(loop
         for target in (eval targets)
         collect (list 'define-orgel-global-access-fn target))))

;;; for the following target list we need to remove phase as this
;;; symbol is used and we call the access function "ophase":

(define-all-orgel-global-access-fns (remove :phase *orgel-global-targets*))

(defun ophase (orgelnummer)
"access function for the phase slot with index <idx>
of orgel at <orgelnummer> in *curr-state*."
  (let ((orgelidx (gethash orgelnummer *orgeltargets*)))
    (orgel-phase (aref *curr-state* orgelidx))))

;;; another special case, as mlevel isn't part of a preset and
;;; therefore not stored in *curr-state*

(defun mlevel (orgelnummer idx)
"access function for the mlevel slot with index <idx>
of orgel at <orgelnummer> in *curr-state*."
  (let ((orgelidx (gethash orgelnummer *orgeltargets*)))
    (aref (aref *orgel-mlevel* orgelidx) (1- idx))))

