;;_____________________________________________________________________________
;;
;;;; CEL is a polynomial-time Classifier for the DL EL+ 
;;;; (EL + GCI + RI + RR + Bot + ABox)
;;_____________________________________________________________________________
;;
;;;; Author: Dr. Boontawee Suntisrivaraporn [Meng]
;;;; Prof. Dr. Franz Baader, Prof. Dr. Carsten Lutz
;;;; Copyright (C) 2005-2009, Authors and the UNIVERSITY OF DRESDEN
;;;; Tested runtime system: Allegro CL on Linux
;;;; Last Modified: 2009-03-12
;;;; Note the T/C in LICENSE.txt
;;_____________________________________________________________________________


(in-package cl-user)

;; :build to generate a binary executable of CEL
;; :complie to complie CEL to fasl codes
;; :fastload to load pre-compiled fast loaded codes of CEL
(defparameter *make-method*
;;    ':fastload)
;;    ':complie)
    ':build)
    

(defconstant app-name "cel")
(defconstant app-path "../bin/")
(defconstant cel-sources '("cel"
			   "module"
			   "system"			   
			   "core"
			   "mina"
			   "hst"
			   "repository"
			   "text"
			   "interface"
			   "utilities"
			   "classifier"
			   "owlapi"
			   ))

(defun compile-cel (file-name)
  (compile-file (concatenate 'string 
		  file-name ".cl")
		:output-file 
		(concatenate 'string 
		  file-name ".clo")))

(defmacro all-cel-object-code ()
  `(mapcar #'(lambda (s)
	       (concatenate 'string s ".clo"))
	   ',cel-sources))


(defun make (&optional make-method)
  "all-in-one function to load all dependencies in to the interpreter, compile each source code, and then build an application from fast-loaded compiled code."
  
  (cond
   ((eq make-method ':fastload)
    (dolist (source cel-sources)
      (load (concatenate 'string
	      source ".clo"))))    
   
   ((eq make-method ':compile)
    (dolist (source cel-sources)
      (load (concatenate 'string
	      source ".cl")))      
    (dolist (source cel-sources)
      (compile-cel source)))
  
   ((eq make-method ':build)
    (dolist (source cel-sources)
      (load (concatenate 'string
	      source ".cl")))      
    (dolist (source cel-sources)
      (compile-cel source))
    
    (generate-application app-name
			  app-path
			  (append '(:aserve :list2)
				  (all-cel-object-code))
			  ;;:runtime-bundle t
			  :lisp-heap-size "250m"
			  :ignore-command-line-arguments t
			  :allow-existing-directory t
			  :include-compiler nil))
   ))

(make :compile)
(make :build) ;; comment out when debugging
(make :fastload)


(eval-when (:load-toplevel :execute)
  (global-reset))