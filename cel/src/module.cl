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


(defstruct (module-data-structure)
  "Struct wrapping all essential data structures for module extraction"
  (axioms nil) ;; list of all original (told) axioms in the ontology
  (prepared? nil) ;; set to t when module extraction is prepared  
  ;;------------------------------------------------  
  (axiom-sig-array ;; Mapping of internal index (GCI or RI) to axiom-sig struct
   (make-array 1000 
	       :element-type 'axiom-sig
	       :adjustable t
	       :fill-pointer 0)
   :type array)
  (active-axioms-table ;; Mapping of symbol to its active axioms (i.e. the symbol occurs on the left-hand side of the axioms
   (make-hash-table :test 'eql)
   :type hash-table)
  )


(defstruct (axiom-sig
	    (:print-function (lambda (ax stream k)
                               (declare (ignore k))
                               (format  stream "ax[~S]"
					(axiom-sig-original ax)))))
  (original nil)
  (l-sig nil)
  (r-sig nil)
  )

(defmacro odsm-axioms ()
  `(module-data-structure-axioms (ontology-ds-m *ontology*)))
(defmacro odsm-prepared? ()
  `(module-data-structure-prepared? (ontology-ds-m *ontology*)))
(defmacro odsm-axiom-sig-array ()
  `(module-data-structure-axiom-sig-array (ontology-ds-m *ontology*)))
(defmacro odsm-active-axioms-table ()
  `(module-data-structure-active-axioms-table (ontology-ds-m *ontology*)))

(defmacro odsm- ()
  `(module-data-structure- (ontology-ds-m *ontology*)))

(defmacro ax-original (ax)
  `(axiom-sig-original (aref (odsm-axiom-sig-array) ,ax)))
(defmacro ax-l-sig (ax)
  `(axiom-sig-l-sig (aref (odsm-axiom-sig-array) ,ax)))
(defmacro ax-r-sig (ax)
  `(axiom-sig-r-sig (aref (odsm-axiom-sig-array) ,ax)))

(defmacro active-axioms (x)
  `(gethash ,x (odsm-active-axioms-table)))


;;_____________________________________________________________________________
;;_____________________________________________________________________________


(defun init-module-extraction--- ()
  "OBSOLETE"
  
  )


(defun add-axiom (axiom l-sig r-sig)
  "Add to (odsm-axiom-sig-array) and return its reference index"  
  (vector-push-extend (make-axiom-sig :original axiom
				      :l-sig l-sig
				      :r-sig r-sig)
		      (odsm-axiom-sig-array)
		      *cname-array-enlarge-factor*)    
  )

(defun sig-of (x)
  "Retrieve a set of all role and concept names occurring in x"
  (if (listp x)
      (let (x-sig)
	(dolist (y (cdr x))
	  (setq x-sig (union x-sig (sig-of y) :test 'eql)))
	x-sig)
    (list x)))

(defun cn-of (x)
  "Retrieve a set of all role and concept names occurring in x"
  (if (listp x)
      (let (x-sig)
	(dolist (y (cdr x))
	  (setq x-sig (union x-sig (cn-of y) :test 'eql)))
	x-sig)
    (if (q-is-concept-f x)
	(list x))
    ))
  

(defun prepare-user-axiom (axiom)
  (case (car axiom)
    ((define-primitive-concept implies role-inclusion)
     (let* ((l-sig (sig-of (cadr axiom)))
	    (r-sig (sig-of (caddr axiom)))
	    (ax (add-axiom axiom l-sig r-sig)))
       (dolist (x l-sig)
	 (push ax (active-axioms x)))
       ))
    ((define-concept equivalent)
     (let* ((l-sig (sig-of (cadr axiom)))
	    (r-sig (sig-of (caddr axiom)))
	    (ax1 (add-axiom axiom l-sig r-sig))
	    (ax2 (add-axiom axiom r-sig l-sig)))
       (dolist (x l-sig)
	 (push ax1 (active-axioms x)))
       (dolist (x r-sig)
	 (push ax2 (active-axioms x)))       
       ))
    ;; all symbols occur on the left and only bot on the right
    ((disjoint)
     (let* ((ax-sig (sig-of axiom))
	    (ax (add-axiom axiom ax-sig nil))
	    )
       (dolist (x ax-sig)
	 (push ax (active-axioms x)))
       ))
    ((transitive)
     (push (add-axiom axiom 
		      (cdr axiom) 
		      (cdr axiom))
	   (active-axioms (cadr axiom)))
     )
    ((reflexive)
     (push (add-axiom axiom 
		      nil
		      (cdr axiom))
	   (active-axioms nil))
     )
    ((define-primitive-role) ;; only parent is treated: s is parent of r
     (let ((r (cadr axiom))
	   (s (cadddr axiom)))
       (push (add-axiom axiom 
			(list r)
			(list s))
	     (active-axioms r))
       ))
    ((domain range)
     (let ((r (cadr axiom))
	   (c-sig (sig-of (caddr axiom))))
       (push (add-axiom axiom
			(list r)
			c-sig)
	     (active-axioms r))
       ))
    (t 
     (err "No support yet for axiom ~A" axiom)
     )
    ))
	        

(defun prepare-for-module-extraction (&optional new-axioms)
  "Read and prepare relevant data structures for extracting reachability-based modules"         
  (msg "Preparing for module extraction...")
  
  (dolist (axiom (or new-axioms
		     (odsm-axioms)))
    (prepare-user-axiom axiom)    
    )
  (format t "Done!")
  
  (setf (odsm-prepared?) t)
  )
;;_____________________________________________________________________________

(defmacro is-subset (l1 l2 &key (test 'eql))
  `(every #'(lambda (x)
	      (member x ,l2 :test ',test))
	  ,l1))


(defun original-axioms (ax-table)
  "Map each of ax-list to the original axiom, no duplications"
  (let (axioms)    
    (loop as ax being the hash-key of ax-table do
	  (pushnew (ax-original ax) axioms :test 'eq))
    axioms))



(defmacro get-hash-keys (hash)
  `(let (keys)
     (loop as key being the hash-key of ,hash do
	   (push key keys))
     keys))

;;_____________________________________________________________________________


;;_____________________________________________________________________________

(defvar *m-table* (make-hash-table :test 'eql))

(defun extract-all-c-modules-naively (&optional (collection (all-concepts)))
  ;;(setq *m-table* (make-hash-table :test 'eql))
  ;;(let ((*m-table* (make-hash-table :test 'eql)))
  (dolist (c collection)
    (unless (gethash c *m-table*)
      (setf (gethash c *m-table*)
	(extract-c-module-f c)))
    )
  *m-table*
  )
;;_____________________________________________________________________________

(defun extract-c-module-f (c &optional out)
  (unless (q-is-concept-f c)
    (err "A concept name is expected; ~S is given."
	 c)
    (return-from extract-c-module-f :named-concept))
  
  (extract-module-f (list c) out))
;;_____________________________________________________________________________


(defun extract-module-f (sig &optional out)
  "Extract the reachability-based module for sig (instead of only C) in the current ontology. The optional parameter out specifies how and where to output the extracted module: nil as a returned value, t printed to console, filename to disk"
  
  (when (ont-state? :cleared)
    (return-from extract-module-f :tbox-state-error))
  (unless (listp sig)
    (return-from extract-module-f :input-type-error))
  
  (unless (odsm-prepared?)
    (prepare-for-module-extraction))
  
  
  (let ((module (make-hash-table :test 'eq))
	(module-sig (make-hash-table :test 'eql))
	(queue-flag (make-hash-table :test 'eq))
	(queue nil)
	)
    
    (dolist (ax (active-axioms nil))
      (setf (gethash ax queue-flag) t))
    
    (dolist (x sig)    
      (setf (gethash x module-sig) t)
      (dolist (ax (active-axioms x))
	(setf (gethash ax queue-flag) t))
      )    
    (setf queue (get-hash-keys queue-flag))
      
    
    (do ((ax (pop queue)
	     (pop queue)))
	((null ax) module) ;; when empty, return the module
                  
      (setf (gethash ax queue-flag) nil)

      ;; test if module doesn't already contain ax
      ;;  and if (ax-l-sig ax) is a subset of module-sig
      (when (and (not (gethash ax module))        
		 (every #'(lambda (x)
			    (gethash x module-sig))
			(ax-l-sig ax)))
	;; add ax to the module
	(setf (gethash ax module) t)
		
	(dolist (x (ax-r-sig ax))
	  ;; take only x that has not been reachable before
	  (unless (gethash x module-sig)
	    (setf (gethash x module-sig) t)
	      
	    (dolist (new-ax (active-axioms x))
	      (unless (gethash new-ax queue-flag)
		(setf (gethash new-ax queue-flag) t)
		(push new-ax queue)))
	    ))	
	)
      )  
    
    ;; reverse-map the axioms and return the module
    
    
    (let ((original-axioms-flag (make-hash-table :test 'eq))
	  (original-axioms nil)
	  ) ;; m-sig)
      (loop as x being the hash-key of module do
	    (setf (gethash (ax-original x) original-axioms-flag) t))
      ;;(loop as x being the hash-key of original-axioms-flag do
	;;    (push x original-axioms))
      (setq original-axioms (get-hash-keys original-axioms-flag))
      
      
      ;; output the module depending on the parameter out
      (cond
       ((eq out nil)
	original-axioms)
       ((eq out t)
	(format t "~S"
		original-axioms)
	T)
       (t ;; out is a file name to which module is outputted
	(with-open-file (output-stream out
			 :direction :output 
			 :if-exists :supersede)
	  (format output-stream "~S"
		  original-axioms))
	T))      
      ))
  )



;;_____________________________________________________________________________
;;
;; Cycle detecter ... applied to unfoldable TBox
;;_____________________________________________________________________________

(defun q-cyclic-unfoldable-tbox-f (&key (force nil))
  "Return t if the tbox is unfoldable and contains cycles (terminological or nested existential)"
  
  ;; return when it is not an unfoldable tbox
  (when (and (not force)
	     (or (ont-tbox-general?)
		 (ont-has-mult-def?)))
    (return-from q-cyclic-unfoldable-tbox-f :tbox-not-unfoldable))
  
  (let ((visited-cnames (make-hash-table :test 'eql)))
    (declare (special visited-cnames))
    
    (loop
      for c from 2 to (- (length (ods-cname-array)) 1)
	do 	  
	  (when (detect-cyclic-dependency c)
	    (return-from q-cyclic-unfoldable-tbox-f t))
	  ))
  nil)

(defun detect-cyclic-dependency (c &key (start-cn (user-cname c))
					(cnames-in-def (cn-of (c-definition c))))

  (declare (special visited-cnames))

  ;; if c has been visited
  (when (gethash c visited-cnames)
    (return-from detect-cyclic-dependency))
    
  (setf (gethash c visited-cnames) t)
  ;;(print c)
  ;; if a cycle is detected
  (when (member start-cn cnames-in-def :test 'eql)
    (return-from detect-cyclic-dependency t))
   
  ;; if not at this level, go deeper
  (dolist (x cnames-in-def)
    (when (detect-cyclic-dependency (system-cname x) :start-cn start-cn)
      (return-from detect-cyclic-dependency t)))
  
  nil)




;;_____________________________________________________________________________
;;_____________________________________________________________________________


(defun extract-all-modules-experiments (ont stat-file
					&optional module time size 
						  (total-time 0))
  
  (clear-tbox)
  (load-tbox ont)
  (prepare-for-module-extraction)  
  
  (with-open-file (stream stat-file
		   :direction :output
		   :if-exists :supersede)
    
    (format stream "concept~At-module~As-module"
	      #\Tab
	      #\Tab 
	      )
    
    
  (dolist (c (all-concepts))
        
    (setq t0 (get-internal-run-time))	
    (setq module (extract-c-module-f c))
    (setq time (float (/ (- (get-internal-run-time) t0)
			 internal-time-units-per-second)))
    
    (incf total-time time)
    (setq size (length module))

    (format stream "~%~S~A~S~A~S"
	    c #\Tab
	    time #\Tab 
	    size )	    
    (finish-output stream)
    
    ))
  total-time
  )



(defun sample-concept-signature (n &optional 
				   (max (- (cdr (ods-system-cname-range)) 1))
				   (count 0)
				   )
  
  (let ((sampled-c-table (make-hash-table :test 'eql))
	(sampled-c-list nil))
  
    (do ((c	 
	  (+ (random max) 2)
	  (+ (random max) 2)
	  ))
	((eq n count))
      
      (unless (gethash c sampled-c-table)
	(incf count)
	(setf (gethash c sampled-c-table) t)
	)
      )
    
    (loop as c being the hash-key of sampled-c-table do
	  (push (user-cname c) sampled-c-list)
	  )
    sampled-c-list
    ))



(defun extract-s-modules-experiments (ont stat-file
				      &optional module time size 
						(total-time 0)
						(total-size 0))  
  (clear-tbox)
  (load-tbox ont)
  (prepare-for-module-extraction)  
  
  (with-open-file (stream stat-file
		   :direction :output
		   :if-exists :supersede)
    
    (format stream "sig-size~At-module~As-module"
	      #\Tab
	      #\Tab 
	      )
    
    
    (dotimes (n 30) ;; size of sig ranges from 10 to 1000
      
      (setq total-time 0)
      (setq total-size 0)
      
      (dotimes (i 10)  ;; perform n repetitions
	
	(setq t0 (get-internal-run-time))	
	(setq module (extract-module-f (sample-concept-signature (* (+ n 1) 2000))))
	(setq time (float (/ (- (get-internal-run-time) t0)
			 internal-time-units-per-second)))
	(incf total-time time)
	(incf total-size (length module))
	
	)
      
      (format stream "~%~S~A~,2:F~A~,2:F"
	    (* (+ n 1) 2000) #\Tab
	    (/ total-time 10) #\Tab 
	    (/ total-size 10))
      
      )    
    ))      