;;;; package.lisp

(defpackage #:cl-orgelctl
  (:use #:cl #:orm-utils #:cellctl #:cl-orgel-gui)

;;   (:shadowing-import-from :cl-orgel-gui #:orgel #:make-orgel #:orgel-ramp-up #:orgel-ramp-down #:orgel-exp-base #:orgel-base-freq #:orgel-min-amp #:orgel-max-amp #:orgel-phase #:orgel-bias-type #:orgel-level #:orgel-delay #:orgel-q #:orgel-gain #:orgel-osc-level
;; ;;; #:orgel-meters
;;                           #:orgel-main #:orgel-bias-bw #:orgel-bias-pos #:start-orgel-gui)

  (:export #:ndb-slider->amp #:amp->ndb-slider)
  )
