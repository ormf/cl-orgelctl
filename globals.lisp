;;; 
;;; globals.lisp
;;;
;;; global variables are defined here.
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

(defparameter *debug* t
   "flag for printing debugging info.")

(defparameter *num-orgel* 10
  "total num of organs")

(defparameter *base-freqs*
  '(27.5 32.401794 38.49546 46.19711 56.132587 69.28748 87.30706 113.156204
    152.76933 220.0)
  "all base frequencies of the orgel.")

(defparameter *orgel-freqs*
  (sort
   (loop
     for base-freq in *base-freqs*
     for orgeltarget from 1
     append (loop
              for partial from 1 to 16
              collect (list (* base-freq partial)
                            (ftom (* base-freq partial))
                            orgeltarget partial)))
   #'<
   :key #'first)
  "all available frequencies in orgel. The entries contain frequency,
keynum, orgelno and partialno.")

(defparameter *orgel-max-freq* (caar (last *orgel-freqs*)))
(defparameter *orgel-min-freq* (caar *orgel-freqs*))

(defparameter *orgel-presets-file*
  (asdf:system-relative-pathname :cl-orgelctl "presets/orgel-presets.lisp"))

(defparameter *route-presets-file*
    (asdf:system-relative-pathname :cl-orgelctl "presets/route-presets.lisp"))

(defconstant +notch+ 1)
(defconstant +bandp+ 0)
(defconstant +phase+ 1)
(defconstant +invert+ -1)

;;; the current state of all orgel vars:

(defparameter *curr-state*
  (make-array
   *num-orgel*
   :element-type 'orgel
   :initial-contents (loop for i below *num-orgel* collect (make-orgel)))
  "State of all faders of the orgel on the pd side.")

(defparameter *orgel-mlevel*
  (make-array *num-orgel*
              :element-type 'simple-array
              :initial-contents
              (loop
                for i below *num-orgel*
                collect (make-array 16 :element-type 'float
                                       :initial-contents (loop for x below 16 collect 0.0))))
  "all volume levels currently measured in pd (permanently updated).")

(defparameter *orgeltargets* (make-hash-table)
  "lookup of orgelname (as keyword) or orgelnumber (starting from 1 to
zerobased index.")

(defparameter *global-targets* nil)
(defparameter *global-amps* nil)

;;; setup of *orgeltargets*

(dotimes (i *num-orgel*)
  (setf (gethash (read-from-string (format nil ":orgel~2,'0d" (1+ i)))
                 *orgeltargets*)
        i
        (gethash (1+ i) *orgeltargets*)
        i))

(defparameter *orgel-global-targets*
  '(:base-freq :phase :bias-pos :bias-bw :bias-type :main :min-amp :max-amp
    :ramp-up :ramp-down :exp-base))

(defparameter *orgel-fader-targets*
  '(:level :bias-level :delay :q :gain :osc-level :bias-level))

(defparameter *orgel-measure-targets*
  '(:mlevel))

(defparameter *midi-targets*
  '())

(defparameter *orgel-nr-lookup* nil
  "property list of orgel names and their number.")

(defparameter *orgel-name-lookup* #()
    "vector associating orgel numbers with their name.")

(setf *orgel-nr-lookup*
      (loop for idx below *num-orgel*
            for name = (read-from-string (format nil ":orgel~2,'0d" (1+ idx)))
            append (list name (1+ idx)))

      *orgel-name-lookup*
      (coerce (cons nil (loop
                          for (name idx) on *orgel-nr-lookup* by #'cddr
                          collect name))
              'vector))
