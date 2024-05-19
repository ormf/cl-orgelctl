;;; 
;;; utils.lisp
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

(defun my-make-symbol (string)
  "will return a symbol without the leading hash as the common-lisp function does."
  (read-from-string (format nil ":~a" string)))

(defun ndb-slider->amp (ndb &key (min -40) (max 0))
  (if (zerop ndb)
      0
      (ou:db->amp (n-lin ndb min max))))

(defun amp->ndb-slider (amp &key (min -40) (max 0))
  (if (zerop amp)
      0
      (ou:lin-n (ou:amp->db amp) min max)))

(defun r-elt (seq)
  (elt seq (random (length seq))))

(defun reset-orgel-global ()
  (loop for orgel from 1 to 6 do
    (progn
      (orgel-ctl-global orgel :ramp-up 249)
      (orgel-ctl-global orgel :ramp-down 249)
      (orgel-ctl-global orgel :exp-base 0.3)
      (orgel-ctl-global orgel :min-amp 0)
      (orgel-ctl-global orgel :max-amp 1)
      (orgel-ctl-global orgel :phase 1)
      (orgel-ctl-global orgel :base-freq 117))))

;;; (reset-orgel-global)

(defun orgel-slot-fn (slot)
  "get the access function of slot keyword."
  (symbol-function (read-from-string (format nil "orgel-~a" slot))))

;;; (orgel-slot-fn :level)

(defun orgel-nr (key)
  (getf *orgel-nr-lookup* key))

(defun orgel-name (orgelno)
  (aref *orgel-name-lookup* orgelno))

(defun clip (x min max)
  "clip x to the interval [min max]"
  (min max (max x min)))


(defun all-faders-off (&optional target orgeln)
  (let ((targets (cond ((keywordp target) (list target))
                       ((null target) *orgel-fader-targets*)
                       (t target))))
    (dolist (target targets)
      (dolist (orgel-nr (or orgeln (range 1 (1+ *orgelcount*))))
        (dotimes (fader 16)
          (orgel-ctl-fader orgel-nr target (1+ fader) 0.0))))))

(defun set-faders (orgel target fn)
  "set all faders of <target> at orgel <orgelno> to the values
determined by fn, called on all partials."
  (let ((orgel-bias (bias-pos (1+ (gethash orgel *orgeltargets*)))))
    (loop
      for fader from 1 to 16
      for x from 0 by 1/15
      do (orgel-ctl-fader orgel target fader (funcall fn x)))))

(defun set-global-faders (targets fn)
  "set faders of <targets> to the values determined by fn, called on all
targets with the idx of the target as arg."
  (if targets
      (loop
        for target in targets
        for x from 0 by (if (> (length targets) 1) (/ (1- (length targets))) 1)
        do (case (length target)
             (2 (orgel-ctl (orgel-name (second target))
                           (first target)
                           (float (funcall fn x) 1.0)))
             (3 (orgel-ctl-fader (orgel-name (second target))
                                 (first target) (third target)
                                 (float (funcall fn x) 1.0)))))))

(defun apply-notch (bias-type fn)
  "return a function by composing fn with an inverter of values with
respect to the range [0..1] (0->1, 1->0, 0.5->0.5, 0.2-0.8) if
(= bias-type 1), otherwise don't invert."
  (if (= bias-type 1)
      (lambda (x) (+ 1 (* -1 (funcall fn x))))
      fn))

(defun permute (fn permutation)
  "permute a fader idxs (1-16) according to permutation."
  (let* ((len (length permutation))
         (permutatio (make-array len :element-type 'fixnum))
         (n-1 (1- len)))
    (loop
      for n from 1
      for idx across permutation
      do (setf (aref permutatio (1- idx)) n))
    (lambda (x) (funcall fn (/ (1- (aref permutatio (round (* x n-1)))) n-1)))))

#|
(let ((permutation #(1 16 2 15 3 14 4 13 5 12 6 11 7 10 8 9)))
  (loop for x below 16 collect (aref permutation x)))
|#

(defmacro n-exp (x min max)
  (let ((quot (if (zerop min) 0 (/ max min))))
    `(if (zerop ,x)
         ,min
         (* ,min (expt ,quot ,x)))))

(defun n-lin (x min max)
  (let ((diff (- max min)))
    (+ min (* diff x))))

(defun recalc-bw (bw num-faders)
  (n-lin bw 0.5 num-faders))

(defun n-recalc-bw (bw num-faders)
  (n-lin bw (/ 0.5 num-faders) 1))

(defun recalc-bias-pos (pos num-faders)
  (n-lin pos 1 num-faders))

#|
(defun bias-cos (bias-pos bw &key targets (levels #(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)))
  "return a function which calculates the bias level for a slider [1-16]
with given center freq and bw. bias-pos and bw are normalized. bw is
the distance between the bias-pos and the -6 dB points left/right of
the bias-pos. At 15/15.5 <bw<1 the faders are interpolated between the
faders at bw 15/15.5 and max level of all faders at bw 1."
  (let* ((num-faders (if targets (length targets) 16))
         (real-bw (recalc-bw bw num-faders))
         (fader-interp (- (clip real-bw (1- num-faders) num-faders) (1- num-faders))))
    (lambda (x) (* (aref levels (1- x))
              (+ fader-interp
                 (* (- 1 fader-interp)
                    (+
                     0.5
                     (* 0.5
                        (cos
                         (clip (/ (* pi 1/2 (- x (recalc-bias-pos bias-pos num-faders)))
                                  real-bw)
                               (* -1 pi) pi))))))))))
|#

(defun bias-wippe (bias-pos bw &key (levels #(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)))
  (lambda (x) (let* ((real-bw (+ (* 18/33 (1- (n-lin bw 0.5 15.5))) 7))
                (val1 (clip (+ 1 (* (1- x) (/ -1 real-bw))) 0 1))
                (val2 (clip (+ 1 (* (- 16 x) (/ -1 real-bw))) 0 1))
                (interp (/ (1- (recalc-bias-pos bias-pos)) 15)))
           (* (aref levels (1- x)) (+ (* (- 1 interp) val1) (* interp val2))))))

(defun bias-db-linear (bias-pos bw &key (levels #(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)))
  (lambda (x) (let* ((real-bw (+ (* 18/33 (1- (n-lin bw 0.5 15.5))) 7))
                (val1 (clip (+ 1 (* (1- x) (/ -1 real-bw))) 0 1))
                (val2 (clip (+ 1 (* (- 16 x) (/ -1 real-bw))) 0 1))
                (interp (/ (1- (recalc-bias-pos bias-pos)) 15)))
           (* (aref levels (1- x)) (+ (* (- 1 interp) val1) (* interp val2))))))

(defun calc-bw-interp (bw pivot)
  (if (< bw pivot) 0
      (/ (- bw pivot) (- 1 pivot))))

(defun bias-cos (bias-pos bw &key targets (levels #(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)))
  "return a function which calculates the bias level for a slider
[1-(length targets)] with given center freq and bw. bias-pos and bw
are normalized. bw is the distance between the bias-pos and the -6 dB
points left/right of the bias-pos. At 15/15.5<bw<1 the values of the
faders are interpolated between the faders at bw 15/15.5 and 1."
  (let* ((num-faders (if targets (length targets) 16))
         (real-bw (n-recalc-bw bw num-faders))
         (fader-interp (calc-bw-interp real-bw (/ (1- num-faders) num-faders))))
    (lambda (x) (* (aref levels (round (n-lin x 0 (1- num-faders))))
              (+ fader-interp
                 (* (- 1 fader-interp)
                    (+
                     0.5
                     (* 0.5
                        (cos
                         (clip (/ (* pi 1/2 (- x bias-pos)) real-bw)
                               (* -1 pi) pi))))))))))

(defun bias-wippe (bias-pos bw &key targets (levels #(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)))
  (lambda (n-x) (let* ((num-targets (if targets (length targets) 16))
                  (x (1+ (round (* n-x (1- num-targets)))))
                  (real-bw (+ (* 18/33 (1- (n-lin bw 0.5 15.5))) 7))
                  (val1 (clip (+ 1 (* (1- x) (/ -1 real-bw))) 0 1))
                  (val2 (clip (+ 1 (* (- 16 x) (/ -1 real-bw))) 0 1))
                  (interp (/ (1- (recalc-bias-pos bias-pos num-targets)) 15)))
             (* (aref levels (1- x)) (+ (* (- 1 interp) val1) (* interp val2))))))

(defun bias-db-linear (bias-pos bw &key targets (levels #(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)))
  (lambda (n-x) (let* ((num-targets (if targets (length targets) 16))
                  (x (1+ (round (* n-x (1- num-targets)))))
                  (real-bw (+ (* 18/33 (1- (n-lin bw 0.5 15.5))) 7))
                  (val1 (clip (+ 1 (* (1- x) (/ -1 real-bw))) 0 1))
                  (val2 (clip (+ 1 (* (- 16 x) (/ -1 real-bw))) 0 1))
                  (interp (/ (1- (recalc-bias-pos bias-pos num-targets)) 15)))
             (* (aref levels (1- x)) (+ (* (- 1 interp) val1) (* interp val2))))))


(defun bias-wippe-db (bias-pos bw &key targets)
  (lambda (n-x) (let* ((num-targets (if targets (length targets) 16))
                  (x (1+ (round (* n-x (1- num-targets)))))
                  (real-bw (+ (* 9/15 (1- (recalc-bw bw num-targets))) 7))
                  (val1 (clip (+ 1 (* (1- x) (/ -1 real-bw))) 0 1))
                  (val2 (clip (+ 1 (* (- 16 x) (/ -1 real-bw))) 0 1))
                  (interp (/ (1- (recalc-bias-pos bias-pos num-targets)) 15)))
             (* 127 (+ (* (- 1 interp) val1) (* interp val2))))))

(defun clear-routes ()
  (digest-routes nil))

(defun switch-targets (new &key (old '*global-targets*) (trigger nil))
  "set *global-targets* to new, zeroeing all faders of old, which are
not contained in new."
;;;  (break "~a ~a"  (symbol-value old) new)
  (let* ((last (symbol-value old))
         (to-remove (remove-if (lambda (x) (member x new :test #'equal)) last)))
    (loop
      for (target orgelno faderno value)
        in to-remove
      do (progn
           (orgel-ctl-fader (orgel-name orgelno) target faderno 0)))
    (setf (symbol-value old) new)
    (if trigger (mapcar #'funcall (get-trig-fns trigger)))))

(defun get-trig-fns (expr)
  "get the functions triggered by receiving a new value of an orgel
fader. expr has to be a quoted expression of an orgelfader, which
triggers a recalculation, e.g. '(bias-pos 1)."
  (case (length expr)
    (2 (slot-value (aref *osc-responder-registry* (1- (second expr))) (first expr)))
    (3 (aref (slot-value (aref *osc-responder-registry* (1- (second expr))) (first expr)) (third expr)))))


#|
(defparameter *base-freqs*
  '(27.5 35.321407 45.367332 58.270473 74.84345
    96.13003 123.470825 158.58774 203.69244 261.62555))

(sort '(220.0 152.76933 113.156204 87.30706 69.28748 56.132587 46.19711 38.49546
          32.401794 27.5)
        #'<)
|#

(defun find-orgel-partial (freq &key (orgel-registry *orgel-freqs*))
  "for a given freq find the closest partial given the orgel-registry.
Return the deviation from the desired keynum in midifloats."
  (cond
    ((<= *orgel-min-freq* freq *orgel-max-freq*)
     (loop for (entry1 entry2) on orgel-registry
           for f1 = (first entry1)
           for f2 = (first entry2)
           until (<= f1 freq f2)
           finally (return
                     (if (< (- freq f1)
                            (- f2 freq))
                         (values entry1 (- (second entry1) (ftom freq)))
                         (values entry2 (- (second entry2) (ftom freq)))))))
    ((< freq *orgel-min-freq*) (let ((entry (first orgel-registry)))
                                (values entry (- (second entry) (ftom freq)))) )
    (t (let ((entry (first (last orgel-registry))))
         (values entry (- (second entry) (ftom freq)))) )))

(defun find-fader (freq-amp &key (fader 'level))
  "given a list (<freq> <amp>) and a fader type construct a list to be
sent using orgel-ctl-fader."
  (destructuring-bind (freq amp) freq-amp
    (destructuring-bind (&optional freq keynum orgeltarget partial)
        (find-orgel-partial freq)
      (declare (ignore keynum))
      (if freq (list freq (list fader orgeltarget partial) amp)))))

(defun transpose (seqs &key (initial-element nil))
  "transpose seqs and return them as a list of vectors. The number of
seqs returned equals the number of the first element of seqs.  If an
element of seqs is shorter then the length of the first element, the
element gets nil padded at the end."
  (labels ((inner (seq accum)
             (if (null seq)
                 (mapcar #'reverse accum)
                 (inner (cdr seq) (mapcar #'cons (first seq) accum)))))
    (inner seqs (or initial-element (loop for i below (length (first seqs))
                                            collect nil)))))

(defun find-orgel-fader-amps (seq &key (fader 'level))
  (if seq
      (let ((fader-seq (mapcar (lambda (x) (find-fader x :fader fader)) seq)))
        (transpose
         (mapcar #'cdr
                 (remove-duplicates
                  (sort (remove nil fader-seq)
                        (lambda (elem1 elem2)
                          (destructuring-bind (freq1 target1 amp1) elem1
                            (declare (ignore target1))
                            (destructuring-bind (freq2 target2 amp2) elem2
                              (declare (ignore target2))
                              (or (< freq1 freq2)
                                  (and (= freq1 freq2) (< amp1 amp2)))))))
                  :key #'first
                  :from-end t))))))

(defun oscillator-mute ()
  (loop for orgelidx from 1 to *orgelcount*
        for orgeltarget = (make-keyword (format nil "orgel~2,'0d" orgelidx))
        do (loop for partial from 1 to 16
                 do (orgel-ctl-fader orgeltarget :osc-level partial 0.0))))

(defun set-orgel-freqs (base-freqs preset-no)
  (setf *base-freqs* base-freqs)
  (setf *orgel-freqs*
        (sort
         (loop
           for base-freq in base-freqs
           for orgeltarget from 1
           append (loop
                    for partial from 1 to 16
                    collect (list (* base-freq partial)
                                  (ftom (* base-freq partial))
                                  orgeltarget partial)))
         #'<
         :key #'first))
  (setf *orgel-max-freq* (caar (last *orgel-freqs*)))
  (setf *orgel-min-freq* (caar *orgel-freqs*))
  (loop for f in base-freqs
        for orgelidx from 1
        for orgeltarget = (make-keyword (format nil "orgel~2,'0d" orgelidx))
        do (progn
             (orgel-ctl orgeltarget :base-freq f)
             (orgel-ctl orgeltarget :min-amp 0)
             (orgel-ctl orgeltarget :max-amp 1)
             (orgel-ctl orgeltarget :ramp-up 239)
             (orgel-ctl orgeltarget :ramp-down 239)
             (orgel-ctl orgeltarget :exp-base 0.8)
             (orgel-ctl orgeltarget :phase 1)
             ))
  (copy-orgel-preset *curr-state* (aref *orgel-presets* preset-no))
  (save-orgel-presets))

(defun wellenlaenge (freq &key (schallgeschwindigkeit 343.2))
  (/ schallgeschwindigkeit freq 2))

(defun send-plist (seq)
  "send the list of partials to set the *global-targets* parameter using osc."
  (let ((osc-msgs
          (loop for s in seq
                collect
                (list* "/plist-ctl/fader" "sfff"
                       (format nil "~a" (first s)) (mapcar #'float (cdr s))))))
    (apply #'incudine.osc:bundle
           *org-oscout* 0
           `(("/plist-ctl/start" "")
             ,@osc-msgs
             ("/plist-ctl/stop" "")))))


#|
(destructuring-bind (targets amps)
    (find-orgel-fader-amps
     '((311.3 0.5) (412.2 0.3)
       (1230.5 0.1) (3410.8 0.191)
       (311.3 0.2) (412.2 0.247)
       (1230.5 0.4) (3410.8 0.193)
       (311.3 0.321) (412.2 0.312)
       (1230.5 0.125) (3410.8 0.71)
       (311.3 0.521) (412.2 0.25)
       (1230.5 0.413) (3410.8 0.31))
     :fader 'osc-level)
  (setf *global-targets* targets)
  (setf *global-amps* (apply #'vector amps)))

|#


#|
                                        
;;; examples:                           
                                        
;;; preparation for use with ats-cuda:  
;;;                                     
;;; given a list of freqs and their amps return two vectors 
;;; containing the orgel faders <(level orgelno partial)> and their 
;;; respective amps. The vectors are reduced by removing duplicate faders 
;;; (the loudest one is kept).          
                                        
(find-orgel-fader-amps                  
'((311.3 0.5) (412.2 0.3)               
(1230.5 0.1) (3410.8 0.191)             
(311.3 0.2) (412.2 0.247)               
(1230.5 0.4) (3410.8 0.193)             
(311.3 0.321) (412.2 0.312)             
(1230.5 0.125) (3410.8 0.71)            
(311.3 0.521) (412.2 0.25)              
(1230.5 0.413) (3410.8 0.31)))          
                                        
                                        
                                        
;;; set *global-targets*:               
                                        
(setf *global-targets*                  
'((level 1 1 1) (level 1 2 1) (level 2 1 1) (level 2 2 1) (level 1 3 1) 
(level 1 5 1) (level 1 4 1) (level 2 11 1) (level 2 13 1) (level 2 3 1) 
(level 1 15 1) (level 1 12 1) (level 2 7 1) (level 1 6 1) (level 2 5 1) (level 1 11 1)))
                                        
;;; digest the route (:bias-pos, :bias-bw and _bias-type trigger 
;;; recalculation of the *global-targets*): 
                                        
(digest-routes                          
'(:orgel01                              
(:global                                
((apply-notch :bias-type                
(bias-cos :bias-pos :bias-bw :targets   
*global-targets*))                      
*global-targets*))))                    
                                        
;;; change *global-targets* and trigger recalculation with new 
;;; targets, after old targets not contained in new targets have been set 
;;; to 0:                               
                                        
(switch-targets '((level 3 1 1) (level 1 2 1) (level 1 1 1) (level 2 5 1) (level 1 3 1) 
(level 1 5 1) (level 1 4 1) (level 2 11 1) (level 2 13 1) (level 2 3 1) 
(level 3 15 1) (level 2 12 1) (level 2 7 1) (level 1 6 1) (level 2 5 1) (level 1 11 1)) 
:trigger '(bias-pos 1))                 
                                        
;;; change *global-targets* back to previous values: 
                                        
(switch-targets '((level 1 1 1) (level 1 2 1) (level 2 1 1) (level 2 2 1) (level 1 3 1) 
(level 1 5 1) (level 1 4 1) (level 2 11 1) (level 2 13 1) (level 2 3 1) 
(level 1 15 1) (level 1 12 1) (level 2 7 1) (level 1 6 1) (level 2 5 1) (level 1 11 1)) 
:trigger '(bias-pos 1))                 
                                        
|#
