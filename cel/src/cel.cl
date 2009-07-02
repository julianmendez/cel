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
;;;; Last Modified: 2009-03-26
;;;; Note the T/C in LICENSE.txt
;;_____________________________________________________________________________

(in-package cl-user)

;; The module :aserve is required for the DIG server and should be included when the application is built
(eval-when (:load-toplevel :execute)
  (require :aserve)
  (require :sock))


(defpackage cel
  (:use cl-user
	common-lisp
        ))
(defpackage cel-system
  (:use cl-user
        common-lisp)  
  )

(defpackage cel-dig 
  (:use excl 
	common-lisp
	net.aserve 
	net.html.generator
	net.uri
	net.xml.parser	
	)
  (:export startup-server
	   shutdown-server
	   ))


(in-package cel-system)

;;_____________________________________________________________

(setq excl:*restart-init-function* '(lambda () (startup-cel)))
(setq excl:*print-startup-message* nil)
;;_____________________________________________________________

(defvar *cel-mode* :interactive
  "Valid modes are :interactive :batch :dig :owlapi")
(defparameter *enable-primitive-tbox-optimization* nil)
(defparameter *enable-progress-bar* t)
(defparameter *classification-mode**** 2
  "Mode of classification: 
- 0 = Implication sets
- 1 = Hasse diagram
- 2 = Activatedly computation of implication sets")

;;_____________________________________________________________
(defconstant *cel-keywords*
    '(;; concept and role constructs
      top
      bottom
      some 
      and 
      compose 
      ;; ontological (tbox) axioms
      define-primitive-concept ;;defprimconcept
      define-concept ;;defconcept
      implies concept-inclusion
      equivalent ;;concept-equivalent      
      disjoint
      define-primitive-role ;;defprimrole
      role-inclusion
      role-equivalent
      reflexive transitive
      domain range
      ;; abox assertional axioms
      define-primitive-individual
      instance related 
      same-individuals
      different-individuals
      ;; queries about concepts
      concept? 
      all-concepts
      all-unsatisfiable-concepts
      satisfiable? concept-satisfiable?
      subsumes? concept-subsumes? 
      implies? concept-implies? 
      equivalent? concept-equivalent?
      parents
      children
      ancestors super-concepts
      descendants sub-concepts
      equivalents
      ;; module extraction
      extract-module
      extract-c-module
      ;; justification by axiom pinpointing
      extract-mina
      extract-all-minas
      ;; enhanced queries
      ?implies
      ?subsumes
      ;; queries about roles
      role?
      all-roles
      role-subsumes?
      role-implies?
      transitive?
      reflexive?
      parent-roles
      child-roles
      super-roles
      sub-roles
      ;; queries about individuals
      individual? 
      all-individuals
      instance?
      individual-direct-types
      individual-types
      concept-instances       
      ;; operation on ontology
      default
      create-tbox create-ontology
      release-tbox release-ontology
      release-all-tboxes release-all-ontologies
      restore-tbox restore-ontology
      repository
      ;; operation on current ontology
      add-axiom add-axioms
      clear-tbox clear-ontology
      load-tbox load-ontology
      classify-tbox classify-ontology
      detail-ontology
      ;; queries about current ontology
      tbox-prepared? ontology-prepared?
      tbox-classified? ontology-classified?
      tbox-consistent? 
      abox-consistent?
      ontology-consistent?
      ;; operation on incremental ontology TTBox
      activate-ttbox 
      deactivate-ttbox
      clear-ttbox 
      classify-ttbox
      commit-ttbox
      ;; queries about incremental ontology
      ttbox-active? 
      ttbox-cleared?
      ttbox-prepared?
      ttbox-classified?
      ;; result handling command
      output-subsumption ;; obsolete
      output-imp-sets ;; succinct
      output-supers ;; complete and comparable ***
      output-synonyms ;; obsolete
      output-hierarchy
      output-taxonomy
      ;; dig server
      startup-dig-server
      shutdown-dig-server
      ;; utilities and information   
      start
      version
      build
      el+
      help
      global-reset
      quit
      ))

(defconstant *cel-response-codes*    
    '(;; concept and role forming errors
      :named-concept
      :malformed-concept
      :named-role
      :malformed-role
      ;; general input typing error
      :input-type-error
      ;; macro
      :macro-syntax-error
      :unsupported
      ;; tbox errors
      :tbox-state-error
      ))

(defconstant *cel-version* "1.0")
;;_____________________________________________________________

(eval-when (:load-toplevel :execute)
  (defun export-all-cel-global-names ()

    (export *cel-keywords* 'cel-system)
    (make-cel-package)
    (make-cel-package 'cel))
  
  (defun make-cel-package (&optional (user-package 'cl-user))
    (loop for sym being the external-symbols of (find-package 'cel-system) do
          (let ((cel-sym (find-symbol (symbol-name sym) user-package)))
            (when cel-sym
              (unintern cel-sym user-package)))
          (import sym user-package)))
  )

;; ************** please do not modify this function ***************

(eval-when (:load-toplevel :execute)
  (export-all-cel-global-names)  
  
  (defun start-profiling-time (benchmark)
    (prof:with-profiling (:type :time)
      (start benchmark)))
  
  (defun in-cel ()
    (in-package cel-system))  
  
  (defun startup-cel ()				
    (cel-system::greeting-screen)
    ;; start right in CEL package
    (tpl:setq-default *package* (find-package 'cel))
    (setq tpl::*saved-package* (find-package 'cel))
    ;; no annoying GC message, but the GC is automatically done.
    (setq excl:*global-gc-behavior* :auto)
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    (handle-command-line)))
;;_____________________________________________________________________________
;;_____________________________________________________________________________


(defun global-reset ()  
  "Anything that should be invoked at the very beginning...HERE!!!"  
  ;; initialize the repository and the current ontology
  (setq *ontology-repository* (make-hash-table :test 'equal))
  (setq *ontology* 
    (setf (cel-ontology default) (create-cel-ontology default)))  
  )
;;_____________________________________________________________________________

(defun handle-command-line ()

  (global-reset)
  
  (let* ((args (sys:command-line-arguments :application t))
	 (length (length args))
	 arg l c oS oT oH q tmp)
    
    (loop for i from 3 to (- length 1) do
	  
	  (setq arg (nth i args))
	  
	  (cond 
	   ((or (string= arg "-h")
		(string= arg "-help"))
	    
	    (display-command-line-help)
	    (excl:exit))
	   
	   ((or (string= arg "-l")
		(string= arg "-loadOntology"))
	    
	    (setq l (when (< (+ i 1) length) 
		      (nth (incf i) args)))
	    (when (or (null l)
		      (eq #\- (aref l 0)))
	      (setq l nil)
	      (msg "What ontology do you want to load!?~%")))
	      
	   ((or (string= arg "-c")
		(string= arg "-classifyOntology"))
	    
	    (setq c t))
	   
	   ((string= arg "-outputSupers")
	    
	    (setq tmp (when (< (+ i 1) length)
			(nth (+ i 1) args)))
	    (if (or (null tmp)
		    (eq #\- (aref tmp 0)))
		(setq oS t)		
	      (setq oS (nth (incf i) args))))
	   
	   ((string= arg "-outputTaxonomy")
	   
	    (setq tmp (when (< (+ i 1) length)
			(nth (+ i 1) args)))
	    (if (or (null tmp)
		    (eq #\- (aref tmp 0)))
		(setq oT t)		
	      (setq oT (nth (incf i) args))))
	   
	   ((string= arg "-outputHierarchy")

	    (setq tmp (when (< (+ i 1) length)
			(nth (+ i 1) args)))
	    (if (or (null tmp)
		    (eq #\- (aref tmp 0)))
		(setq oH t)		
	      (setq oH (nth (incf i) args))))
	   
	   ((string= arg "-digServer")

	    ;; CEL is running in :owlapi mode
	    (setq *cel-mode* :dig)

	    ;; << put the DIG server startup function here >>
	    ;;(err "DIG feature is not yet available!"))
	    
	    (cel-dig:startup-server 
	     :port (when (< (+ i 1) (length args))	
		     (parse-integer (nth (+ i 1) args) 
				    :junk-allowed t)))
	    ;; forever loop until kill signal
	    )
	   
	   ((string= arg "-owlapiServer")
	    
	    ;; CEL is running in :owlapi mode
	    (setq *cel-mode* :owlapi)
	    
	    (cond
	     ((>= (+ i 2) (length args))
	      (err "The -owlapiServer option requires remote-host and remote-port parameters~%")
	      (excl:exit))
	     
	     (t
	      (let ((remote-host (nth (+ i 1) args))
		    (remote-port (parse-integer (nth (+ i 2) args)
						:junk-allowed t))
		    (logging? (nth (+ i 3) args)))
		(when (null remote-port)
		  (err "Value of remote-port must be integer~%")
		  (excl:exit))
		
		(msg "You request to start CEL with an OWLAPI connection to ~A:~A"
		     remote-host 
		     remote-port)
		(in-package cel-system)
		(when (string= logging? "logging")
		  (msg "OWLAPI logging is enabled")
		  (setq *owlapi-verbose-msg* t))
		(startup-owlapi-listener remote-host remote-port)
		))
	     ))
	      
	   
	   ((or (string= arg "-q")
		(string= arg "-quit"))
	    
	    (setq q t)))
	  
	  )
    
    (when (and l c q)
      (setq *cel-mode* :batch))
    
    (if l 
	(progn (msg "You want to load an ontology ~S," l)
	       (if c 
		   (msg "and classify it.~%")		 
		 (msg "but don't want to classify it.~%")))
      (if c 
	  (msg "You didn't give me an ontology to classify!~%")))
    
    (clear-tbox-f)    
    
    (when l
      (msg "Loading and preprocessing the ontology from file ~S..." l)
      (load-tbox-f l)
      (msg "...done in~a." (milisec-to-string (ont-preprocessing-time)))
      (when c
	(msg "Classifying the ontology...")
	(classify-tbox-f)
	(msg "...done in~A." (milisec-to-string (ont-classification-time)))
	(when oS
	  (msg "Outputting classification results [supers]...")
	  (output-supers 
	   :file-name (if (eq oS t)
			  (concatenate 'string l ".supers")
			oS))
	  (format t "done!")
	  )
	(when oT
	  (msg "Outputting classification results [taxonomy]...")
	  (output-taxonomy 
	   :file-name (if (eq oT t)
			  (concatenate 'string l ".taxonomy")
			oT))
	  (format t "done!")
	  )
	(when oH
	  (msg "Outputting classification results [hierarchy]...")
	  (output-hierarchy 
	   :file-name (if (eq oH t)
			  (concatenate 'string l ".hierarchy")
			oH))
	  (format t "done!")
	  )
	))
    (when q (excl:exit))
    ))


;;(eval-when (:execute)
;;  (global-initialization))
