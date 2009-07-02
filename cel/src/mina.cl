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
;;;; Last Modified: 2009-01-27
;;;; Note the T/C in LICENSE.txt
;;_____________________________________________________________________________


(in-package cel-system)



(defun extract-mina-f (c d &optional out)
  "Extract a minimal axiom set (MinA) for the subsumption C <= D"
  
  (unless (q-complex-concept-subsumes-f d c)
    ;; if the subsumption does not hold
    (msg "The subsumption \"~S [= ~S\" does not hold!"
	 c d)
    (return-from extract-mina-f nil))
    
  
  (let ((mina (minimize-module-logarithmically c d))) 
    ;;(minimize-module-linearly c d)
    ;; output the module depending on the parameter out
    (cond
     ((eq out nil)
      mina)
     ((eq out t)
      (format t "~S"
	      mina)
      T)
     (t ;; out is a file name to which module is outputted
      (with-open-file (output-stream out
		       :direction :output 
		       :if-exists :supersede)
	(format output-stream "~S"
		mina))
      T))
    ))
;;_____________________________________________________________________________


(defun extract-mina-linearly-f (c d)
  "Brute-force method by minimizing the entire ontology linearly"
  (cond
   ((eq (q-complex-concept-subsumes-f d c) t)
    ;; if the subsumption holds
    (minimize-module-linearly c d (odsm-axioms)))
   (t
    ;; if it doesnt hold, mina is irrelavant!!!
    (msg "The subsumption \"~S [= ~S\" does not hold!"
	 c d)
    nil))
  )

(defun extract-mina-logarithmically-f (c d)
  "Improved method by minimizing the entire ontology logarithmically"
  (cond
   ((eq (q-complex-concept-subsumes-f d c) t)
    ;; if the subsumption holds
    (minimize-module-logarithmically c d (odsm-axioms)))
   (t
    ;; if it doesnt hold, mina is irrelavant!!!
    (msg "The subsumption \"~S [= ~S\" does not hold!"
	 c d)
    nil))
  )
;;_____________________________________________________________________________


(defun extract-2-minas-f (c d)
  "Extract a minimal axiom set (MinA) for the subsumption C <= D, then extract another (different) one, if any"
  (cond
   ((eq (q-complex-concept-subsumes-f d c) t)
    ;; if the subsumption holds
    ;;(minimize-module-linearly c d)
    (let* ((module (extract-c-module-f c))	  
	   (1-mina 
	    (minimize-module-logarithmically c d (copy-list module)))
	   (2-mina 
	    (extract-next-mina-f c d module 1-mina))
	   )
      
      (if (eq 2-mina :none)
	  (format t "~%The only MinA for \"~S [= ~S\" contains ~A axioms:~%  ~S"
		  c d 
		  (length 1-mina)
		  1-mina)
	(format t "~%There are at least two MinAs for \"~S [= ~S\"; one contains ~A axioms:~%  ~S~%and another contains ~A axioms:~%  ~S"
		c d 
		(length 1-mina)
		1-mina
		(length 2-mina)
		2-mina)
	)))
   (t
    ;; if it doesnt hold, mina is irrelavant!!!
    (msg "The subsumption \"~S [= ~S\" does not hold!"
	 c d)
    nil
    ))
  )
;;_____________________________________________________________________________

(defun minimize-module-linearly (c d &optional 
				     (module (extract-c-module-f c)))
  "Extract the reachability-based module for C, then by using the naive, linear black-box algorithm minimize the module to a MinA"
  
  (let ((mina nil)
	(current-ont-uri (ont-uri)))
    ;; a temp ontology is active within this scope        
    (create-ontology)
    ;; go through all axioms in the module, one by one, linearly
    (do ((ax (pop module)
	     (pop module)))
	((null ax)
	 ;; when the do-loop is done,
	 t)

      ;; load axioms under consideration
      (handler-case 
	  (dolist (ax (append mina module))
	    (eval (copy-tree ax)))
	(error ()
	  (err "An error occurs while loading module axioms!"))
	)
      ;; normalize these axioms
      (normalize)
      ;; text the subsumption (in lazy mode)
      ;; if it doesnt hold, means ax is vital, so keep it
      (unless (q-complex-concept-subsumes-f d c)
	(push ax mina))

      ;; clear the ontology
      (clear-ontology)      
      )
    ;; once finished, release the temp ontology
    (release-ontology)
    (restore-cel-ontology current-ont-uri)
    ;; return the minimized module, which is a MinA
    mina
    ))
;;_____________________________________________________________________________
   
(defun minimize-module-logarithmically (c d &optional 
					    (module (extract-c-module-f c)))
  "Extract the reachability-based module for C, then by using the logarithmically black-box algorithm minimize the module to a MinA"
  (unless (cel-blackbox-sub-test d c :tbox module)
    (return-from minimize-module-logarithmically nil))

  ;; start the recursive binary search
  (split-and-check-r d c ;; the goal subsumption
		     nil ;; support is nil initially
		     ;; start with the module of C to reduce search space
		     module)
  )

(defun split-and-check-r (d c support set)
  "This is an implementation of the best-case logarithmic algorithm (optimal minimal) proposed in Fig 2 of Checking Safety paper. The test p in the paper is equivalent to testing the subsumption C [= D here"
  (cond
   ((<= (list-length set) 1)
    set)
   (t
    (let* ((set-r (halve-list set))
	   (set-l set))
      (cond
       ((cel-blackbox-sub-test d c :tbox (append support set-l))
	(split-and-check-r d c support set-l))
       ((cel-blackbox-sub-test d c :tbox (append support set-r))
	(split-and-check-r d c support set-r))
       (t
	;; the worst case, where we require some elements from both halves
	(let* ((set-l-min 
		(split-and-check-r d c (append support set-r) set-l))
	       (set-r-min 
		(split-and-check-r d c (append support set-l-min) set-r)))
	  (append set-l-min set-r-min)))
       )))
   ))
;;_____________________________________________________________________________

(defun extract-next-mina-f (c d module 1-mina)
  "Check if there exists a second MinA, and, if any, return one. This function assumes 1-mina to be a subset of module and that axioms in both set are pointer-shared"
  (dolist (dispensed-ax 1-mina) ;; try to do away with an axiom from 1-mina
    (let ((module- (set-difference module (list dispensed-ax)
				   :test 'eq)))
      (when (cel-blackbox-sub-test d c :tbox module-)
	(return-from extract-next-mina-f
	  (split-and-check-r d c nil module-)))))
  :none)
;;_____________________________________________________________________________

(defmacro blackbox-subsumes? (cn1 cn2 &key tbox)
  "to test the subsumption CN2 [= CN1 w.r.t. the TBox"
  `(cel-blackbox-sub-test ',cn1 ',cn2 :tbox ',tbox))
(defmacro blackbox-implies? (cn1 cn2 &key tbox)
  "to test the subsumption CN1 [= CN2 w.r.t. the TBox"
  `(cel-blackbox-sub-test ',cn2 ',cn1 :tbox ',tbox))

(defun cel-blackbox-sub-test (d c &key tbox
				       (current-ont-uri (ont-uri))
				       (yes? nil)
				       (t0 (get-internal-run-time)))
  "Test subsumption C [= D w.r.t. the TBox using the CEL reasoner in the blackbox manner. The function shall not alter existing ontologies and their classifications; this is done by exploiting ontology repository feature, i.e., creating a new, temporary tbox for this computation and releasing it later"
  ;; a temp ontology is active within this scope        
  (create-ontology)  
  ;; load all axioms in tbox
  (handler-case 
      (dolist (ax tbox)
	(eval (copy-tree ax)))
    (error ()
      (err "An error occurs while loading axioms!"))
    )
  ;; load all axioms in tbox
;;;  (let ((tbox-copy (copy-tree tbox)))
;;;    ;; request allocation be made on stack (not heap)
;;;    (declare (dynamic-extent tbox-copy))
;;;    (handler-case 
;;;	(dolist (ax tbox-copy)
;;;	  (eval ax))
;;;      (error ()
;;;	(err "An error occurs while loading axioms!"))
;;;      ))
  ;; normalize these axioms
  (normalize)
  ;; test the subsumption (in lazy mode)  
  (setq yes? (q-complex-concept-subsumes-f d c))
  ;; clear the ontology
  (clear-ontology)
  ;; release the temp ontology, and restore the current one back
  (release-ontology)
  (restore-cel-ontology current-ont-uri)
  
  ;; collect some statistic info here (TO BE REMOVED IN THE RELEASE VERSION)
  (incf bb-subs-count)
  (incf bb-subs-time (/ (- (get-internal-run-time) t0)
			internal-time-units-per-second))
  ;; return the answer
  
  yes?)
;;_____________________________________________________________________________
  
    
(defun halve-list (list)
  "Split the list by half, list-r and list-l. This function is DESTRUCTIVE. Outputs: the function returns list-r, while list is altered to list-l. When the list has odd number of elements, list-r always has an extra element"
  (let* ((size (list-length list))
	 (pre-mid (nthcdr (1- (floor size 2)) ;; the mid loc - 1
			  list))
	 (list-r (cdr pre-mid))
	 )
    (setf (cdr pre-mid) nil) ;; cutting list just after pre-mid
    list-r))


;;_____________________________________________________________________________
;;_____________________________________________________________________________

