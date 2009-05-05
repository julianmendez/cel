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
;;;; Last Modified: Tue Jan 27 2009
;;;; Note the T/C in LICENSE.txt
;;_____________________________________________________________________________


(in-package cel-system)


(defvar *ontology-repository* (make-hash-table :test 'equal) ;; maybe 'eq
  "Mapping of ontology URIs to system ontology state snapshots.
- a URI can be a file path, Internet URL or vertual URI. These are interned strings, i.e., symbols of form |http://www.snomed.org/snomed-ct.owl|
- an ontology state snapshot is (presumably) an instance of struct #<ontology> with all neccessary information and datastructures to store it back to the CEL system.")

(defmacro cel-ontology (uri)
  `(gethash ,uri *ontology-repository*))
(defmacro cel-default-ontology ()
  `(gethash default *ontology-repository*))


(defun duplicate-cel-ontology (uri &key new-uri
					(make-current t))
  "Make an exact copy of uri-designated ontology and set as current when requested"
  (unless (cel-ontology uri)
    (err "The URI \"~A\" does not designate any ontology!")
    (return-from duplicate-cel-ontology nil))
  
  (setq new-uri (gen-uri))
  
  (let* ((*ontology* (copy-ontology (cel-ontology uri))))
    ;; (setf (ont-uri) new-uri) -- URI is read-only
    (setf (cel-ontology new-uri) *ontology*))
  
  (when make-current
    ;; restore the URI-specified, newly-created ontology
    (restore-cel-ontology new-uri))
  )
  

(defun create-cel-ontology (&optional uri 
			    &key (make-current t)
				 (if-exists 'do-not-replace))
  "Create a new set of data structures for a new ontology (i.e. value of type ontology) and then set it as the current ontology (i.e. current value of the variable *ontology* and so on). Of course, the old ontology must be kept in the *ontology-repository* for future reference"
  (when (cel-ontology uri)
    (msg "The requested URI is already in use.")
    (if (eq if-exists 'do-not-replace)
	(return-from create-cel-ontology)      
      (msg "The existing ontology with URI \"~A\" will be deleted!"
	   uri)))
  
  ;; generate a unique uri if not given
  (unless uri
    (setq uri (gen-uri)))
  
  (let ((*ontology* (make-ontology :uri uri)))
    ;;(load-all-pointers)
    ;; keep the new ontology in the repository
    (setf (cel-ontology uri) *ontology*)
    ;; initialize the built-in top and bottom concepts
    (vector-push (make-bot-concept-struct) (ods-cname-array))
    (vector-push (make-top-concept-struct) (ods-cname-array))
    (setf (system-cname 'top) *top*)
    (setf (system-cname 'bottom) *bot*)
    (initialize-hasse)
    )
        
  (when make-current
    ;; restore the URI-specified, newly-created ontology
    (restore-cel-ontology uri)    
    ))

(defun release-cel-ontology (&optional uri)
  (cond
   ;; the default ontolgy can never be released
   ((or (eq uri default)
	(eq uri 'default)
	(and (null uri)
	     (eq (ont-uri) default)))
    (msg "The default ontology cannot be released."))
   ;; t is given, release all ontologies
   ((eq uri t)
    (release-all-cel-ontologies))    
   ;; otherwise, release it from the repository
   (uri
    (when (eq (ont-uri) uri)
      (restore-default-cel-ontology))
    (remhash uri *ontology-repository*))
   ;; if no uri is given, release the current one, then set the default as current
   (t
    (remhash (ont-uri) *ontology-repository*)
    (restore-default-cel-ontology))
   )
  ;;------------------------------------------------
  (excl::gc)
  *ontology*)

(defun release-all-cel-ontologies ()
  (let ((default-ont (cel-ontology default)))
    (setq *ontology-repository* (make-hash-table :test 'equal))
     ;;------------------------------------------------
    (excl::gc)
    (setf *ontology*
      (setf (cel-ontology default) (or default-ont
				       (create-cel-ontology default))))
    ))


(defun clear-cel-ontology (&optional uri)
  (setq uri (or uri 
		(ont-uri)))
  (when (eq uri 'default)
    (setq uri default))
      
  (cond  
   ;; treat the current ontology
   ((eq (ont-uri) uri)
    (remhash uri *ontology-repository*)
    (create-cel-ontology uri
			 :make-current t))    
   ;; treat existing one in the repository
   ((cel-ontology uri)
    (remhash uri *ontology-repository*)
    (create-cel-ontology uri 
			 :make-current nil)))  
  ;;------------------------------------------------
  (excl::gc)
  *ontology*)

(defun clear-default-cel-ontology ()
  (clear-cel-ontology default))

(defun save-cel-ontology ()
  ;;(setf (cel-ontology uri) (copy-ontology *ontology*))
  )

(defun restore-cel-ontology (uri)
  (cond
   ((or (eq uri default)
	(eq uri 'default))
    (restore-default-cel-ontology))
   (t
    (setf *ontology* (or (cel-ontology uri)
			 *ontology*)))))

(defun restore-default-cel-ontology ()
  (setf *ontology* (cel-ontology default)))


(defun gen-uri (&key from-file)
  "Generate a unique symbol as URI for a CEL ontology"
  (cond
   (from-file
    (intern (concatenate 'string
	      "urn:cel-ontology:file:/"
	      (string-downcase from-file))))
   (t
    (intern (concatenate 'string
	      "urn:cel-ontology:id-"
	      (string-downcase (symbol-name (gensym))))))
   ))



;;_________________________________________________________________________
;;_________________________________________________________________________

(defun repository (&key (stream t))
  "List all active CEL ontologies with some useful properties"
  (format stream "~%================ CEL Ontology Repository ================")

  (loop as key being the hash-key of *ontology-repository* do
	(let ((current *ontology*)
	      (*ontology* (gethash key *ontology-repository*)))
	  (format stream "~%~%~A~A" 
		  (if (eq current *ontology*)
		      "*"
		    "")
		  *ontology*)
	  (unless (ont-state? :cleared)
	    (format stream "~%       +[#concepts=~D; #roles=~D; #individuals=~D; #axioms=~D]"
		    (- (length (ods-cname-array))
		       (length (ont-individuals)))			    
		    (length (ods-rname-array))
		    (length (ont-individuals))
		    (+ (ont-n-pcdefs)
		       (ont-n-cdefs)
		       (ont-n-gcis)
		       (ont-n-ris)
		       (ont-n-cas)
		       (ont-n-ras))
		    ))))
  (format stream "~%~%=========================================================")
  )
;;_________________________________________________________________________

(defun detail-ontology (&key uri
			     (stream t))
  "to delineate to stream the ontology specified by URI in detail. If no uri is given, the current ontoogy is considered"
  (when (stringp uri)
    (setq uri (intern uri)))
  
  (cond
   ((null uri)
    (detail-current-ontology stream))
   ((null (cel-ontology uri))
    (err "The given URI does not exist!"))
   (t
    (let ((*ontology* (cel-ontology uri)))
      (detail-current-ontology stream)))
   ))
;;_________________________________________________________________________

(defun detail-current-ontology (&optional (stream t))
  
  (declare (ignore uri))  
  
  (case (ont-state)
    ;; ======== nothing more to show than just the fact that it's cleared
    ((:cleared)
      ;;:i-cleared :i-prepared :i-classified :i-taxonomized)
     (format stream "~A" *ontology*))
    ;; ======== told information is shown
    ((:prepared)
     (format stream "Ontology[
  URI = \"~A\";
  state = PREPARED (pre-classification queries can be posted);
  #concepts = ~D (including TOP and BOTTOM);
  #roles = ~D;
  #individuals = ~D;
  DL dialect = EL+~A;
  general TBox? = ~A;
  TBox has multiple definitions? = ~A;
  TBox has complex role inclusions? = ~A;
  
  TBox comprises~A~A~A~A; while, ABox~A.  It takes~A to pre-process the ontology.  
]"
     (ont-uri)
     (- (length (ods-cname-array)) (length (ont-individuals)))
     (length (ods-rname-array))
     (length (ont-individuals))
     (if (ont-rr-no-ri)
	 "[rr] (with range restrictions)"
       "[ri] (without range restrictions)")
     (if (ont-tbox-general?) 'yes 'no)
     (if (ont-has-mult-def?) 'yes 'no)
     (if (ont-has-cri?) 'yes 'no)
     (if (zerop (ont-n-pcdefs))
	 ""
       (format nil " ~D primitive concept definitions" (ont-n-pcdefs)))
     (if (zerop (ont-n-cdefs))
	 ""
       (format nil " ~D (full) concept definitions" (ont-n-cdefs)))
     (if (zerop (ont-n-gcis))
	 ""
       (format nil " ~D GCIs" (ont-n-gcis)))
     (if (zerop (ont-n-ris))
	 ""
       (format nil " ~D role inclusions" (ont-n-ris)))	  	  
     (if (zerop (+ (ont-n-cas) (ont-n-ras)))
	 " is not present in this ontology"
       (format nil 
	       " comprises ~D concept assertions and ~D role assertions"
	       (ont-n-cas)
	       (ont-n-ras)))	      	   	  
     
     (milisec-to-string (ont-preprocessing-time))	  
     ))
    ;; ======== told information is shown
    ((:classified :taxonomized
      :i-cleared :i-prepared :i-classified :i-taxonomized)
     (format stream "Ontology[
  URI = ~S;
  state = ~A (post-classification queries can be posted);
  #concepts = ~D (including TOP and BOTTOM);
  #roles = ~D;
  #individuals = ~D;
  DL dialect = EL+~A;
  general TBox? = ~A;
  TBox has multiple definitions? = ~A;
  TBox has complex role inclusions? = ~A;
  
  TBox comprises~A~A~A~A; while, ABox~A.  It takes~D to pre-process and~D to classify~A the ontology.  Finally, the ontology is~A.
]"
     (ont-uri)
     (ont-state)
     (- (length (ods-cname-array)) (length (ont-individuals)))
     (length (ods-rname-array))
     (length (ont-individuals))
     (if (ont-rr-no-ri)
	 "[rr] (with range restrictions)"
       "[ri] (without range restrictions)")
     (if (ont-tbox-general?) 'yes 'no)
     (if (ont-has-mult-def?) 'yes 'no)
     (if (ont-has-cri?) 'yes 'no)
     (if (zerop (ont-n-pcdefs))
	 ""
       (format nil " ~D primitive concept definitions" (ont-n-pcdefs)))
     (if (zerop (ont-n-cdefs))
	 ""
       (format nil " ~D (full) concept definitions" (ont-n-cdefs)))
     (if (zerop (ont-n-gcis))
	 ""
       (format nil " ~D GCIs" (ont-n-gcis)))
     (if (zerop (ont-n-ris))
	 ""
       (format nil " ~D role inclusions" (ont-n-ris)))	  	  
     (if (zerop (+ (ont-n-cas) (ont-n-ras)))
	 " is not present in this ontology"
       (format nil 
	       " comprises ~D concept assertions and ~D role assertions"
	       (ont-n-cas)
	       (ont-n-ras)))	      	   	  
     
     (milisec-to-string (ont-preprocessing-time))
     (milisec-to-string (ont-classification-time))
     (if (ont-individuals)
	 "&reify"
       "")
     (if (ont-abox-consistent?)
	 " coherent"
       (format nil " incoherent with inconsistencies found ~A"
	       (if (ont-tbox-consistent?)
		   "already in TBox"
		 "in ABox")))
     
     ))
    ))
;;_____________________________________________________________________________