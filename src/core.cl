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

;; This file contains the implementation of the core algorithm; there are at the time of writing 4 varients:
;; (1) standard, batch process (classification)
;; (2) goal-directed process (lazy subsumption)
;; (3) batch process of ttbox (incremental classification)
;; (4) goal-directed process of ttbox (lazy subsumption and complex subsumption queries)




(defun print-s ()
  (print-hash (ods-s-table)))
(defun print-s2 ()
  (print-hash (ods2-s-table)))
(defun print-o ()
  (print-hash (ods-o)))
(defun print-o2 ()
  (print-hash (ods2-o)))
(defun print-r2 ()
  (print-hash (ods2-r-succ)))


;;____________________________________________________________________________
;;
;; [I] BATCH CLASSIFICATION 
;;----------------------------------------------------------------------------
;; Preconditions:: main tbox is initialized and prepared, state is :prepared
;; Procedure:: complete implication sets, one by one in the ordering of system cnames
;; Consequences:: implication sets are completed, state is :classified, all positive cnames (plus some reachable negative cnames) are activated
;;____________________________________________________________________________


(defun act-complete-all-imp-sets (&optional (t0 (get-internal-run-time)))
  "Complete all implication sets one by one (only those user-defined)"
  
  (init-s-r-structures)
	
  (init-queue-s *top*)
    
  (loop
      for c
      from 2
      to (cdr (ods-system-cname-range)) do
	;;(when (cname-struct c)
	
	;; definitional ordering
	(ordered-act-complete-imp-set c)
	;; reverse definitional ordering
	;;(reverse-ordered-act-complete-imp-set c)
	
	;; randomly
	;;(act-complete-imp-set c)
	
	)
  
  (setf (ont-classification-time) (/ (- (get-internal-run-time) t0)
				      internal-time-units-per-second))
  )
;;_____________________________________________________________________________

(defun get-all-known-subsumers- (c)
  "Processing in definitional order gives us more known subsumers for later-processed concepts. These are gathered from told subsumption and their S sets. This function is used to initialize S and queue when activate the concept c and when all told subsumers of c have been processed."
  (let ((subsumers)
	(subsumers-flag (make-hash-table :test 'eq)))
    (dolist (x (c-told-subsumers c))
      (dolist (y (s-labels x))
	(setf (gethash y subsumers-flag) t)))
    
    (loop as key being each hash-key of subsumers-flag do	  
	  (push key subsumers))
    (cons c subsumers))
  )

(defun get-all-known-subsumers (c)
  "Processing in definitional order gives us more known subsumers for later-processed concepts. These are gathered from told subsumption and their S sets. This function is used to initialize S and queue when activate the concept c and when all told subsumers of c have been processed."
  (let ((subsumers))
    (dolist (x (c-told-subsumers c))
      (setq subsumers (union subsumers
			     (s-labels x)
			     :test 'eq))
      )    
    (cons c subsumers))
  )


;;_____________________________________________________________________________
(defun ordered-act-complete-imp-set (c)
  "Definitional ordering (based on told subsumers)"
  (unless (activation-flag c)
    (dolist (d (c-told-subsumers c))
      (ordered-act-complete-imp-set d))
    (act-complete-imp-set c)))
;;_____________________________________________________________________________
(defun reverse-ordered-act-complete-imp-set (c)
  "Reverse definitional ordering (based on told subsumees)"
  (unless (activation-flag c)
    (dolist (d (c-told-subsumees c))
      (reverse-ordered-act-complete-imp-set d))
    (act-complete-imp-set c)))
 ;;_____________________________________________________________________________  
(defun act-complete-imp-set (c)
  "Complete the implication set for c. Only S and queue of c are initialized, and existential reachable (posssibly new) cnames are activated during processing as required"
  
 ;;(activate c)
  ;;(activate-w-told c (cons c (c-told-subsumers c)))
  (activate-w-told c (get-all-known-subsumers c))
  
  (do ((pair
	(q-next-queue)
	(q-next-queue)))
      ;; if all have been processed, return t as success
      ((null pair) t)
    ;; begin do    
    (let ((s-cn (car pair))
	  (queue (cdr pair)))
      ;; process the current cname with all its queue entries
      (dolist (x queue)
	
	;; DISABLING OPTIMIZATION
;;;	(act-process s-cn x)
	
	;; ENABLING OPTIMIZATION
	(if (ont-tbox-primitive?)
	    (act-process-no-existentials s-cn x)
	  (act-process s-cn x))
	
	)))

  (verbose "*")
  ;;(incf *total-classified-cnames*)
  )
;;_____________________________________________________________________________

(defun act-process (a x)
  "Process the application of Completion Rules corresponding to a and x in an activation a
pproach"
  ;;(format t "~a" x)

  ;; there are three cases for x; we do case distinction as follows:
  ;; (1) conjuctive condition - consequence pair: ((A1...An) . B)
  (when (and (listp x)
             (listp (car x)))

    ;; if the consequence (cdr x) of this conditional is alredy in S(A)
    ;; no need to consider this conditional
;;    (if (check-label (cdr x) a)
;;      (return-from act-process))
    
    ;; if one of the conjuncts are not satisfied in a, do nothing
    ;; otherwise, set x to be the consequence
    (if (check-labels (car x) a)
	(setq x (cdr x))
      (return-from act-process)))
  
;;;    (if (dolist (conj (car x) t)
;;;          (unless (check-label conj a)
;;;            (return-from act-process)))
;;;        (setq x (cdr x))))

  ;; (2) exists restriction: (r . B)
  (when (listp x)
    (let ((r (car x))
          (b (cdr x)))
      ;; if the edge already exists; dont do again
      (if (check-edge a r b)
          (return-from act-process))

      ;; if a implies (some r *bot*) == *bot*, meaning that a is unsatisfiable
      (when (is-unsat b)
        (process-unsat-cname a)
        (return-from act-process))

      (activate b)
      (process-new-edges a r b)
      (return-from act-process)))

  ;; (3) the bottom or unsat cname
  (when (is-unsat x)
    (process-unsat-cname a)
    (return-from act-process))

  ;; (4) concept name: B
  ;; if the label already exists; dont do again
  (if (check-label x a)
      (return-from act-process))

  (q-enqueue a (o-nexts x))
  (add-label x a)

  ;;(cache-subs x a)
  ;; modify the queue of all predecessors of A
  (loop ;; for each system rname
      for r from 0 to (- (length (ods-rname-array)) 1)
      do (let ((new-items (o-nexts (cons r x))))
           ;; only non-empty set should be added to queues
           (when new-items
             ;; a is also an r-predecessor, if r is reflexive
             ;; note that the following when-clause can be replaced with
             ;; (q-enqueue a (o-nexts-reflexive x)) outside the loop, but
             ;; I believe this is more optimized.
             (when (r-reflexive r)
               (q-enqueue a new-items))
             ;; apply this to each predecessor B
             (dolist (b (r-preds a r))
               (q-enqueue b new-items)))
           ))
  )
;;____________________________________________________________________________

;; EXPERIMENTAL WITH OPTIMIZATION FOR PRIMITIVE TBOX
(defun act-process-no-existentials (a x)
  "Process the application of Completion Rules corresponding to a and x in an activation a
pproach"
  ;;(format t "~a" x)

  ;; there are three cases for x; we do case distinction as follows:
  ;; (1) conjuctive condition - consequence pair: ((A1...An) . B)
  (when (and (listp x)
             (listp (car x)))
    ;; if one of the conjuncts are not satisfied in a, do nothing
    ;; otherwise, set x to be the consequence
    (if (check-labels (car x) a)
	(setq x (cdr x))
      (return-from act-process-no-existentials)))

  ;; (2) exists restriction: (r . B)
  (when (listp x)    
    (return-from act-process-no-existentials))

  ;; (3) the bottom or unsat cname
  (when (is-unsat x)
    (process-unsat-cname a)
    (return-from act-process-no-existentials))

  ;; (4) concept name: B
  ;; if the label already exists; dont do again
  (if (check-label x a)
      (return-from act-process-no-existentials))

  (q-enqueue a (o-nexts x))
  (add-label x a)

  )
;;____________________________________________________________________________


;;____________________________________________________________________________
;;
;; [II] TTBOX BATCH CLASSIFICATION WITH PRECLASSIFIED TBOX
;; a varient of [I] which takes into account both sets of data structures and which initializes the queues by properly taking known logical conseqeunces from TBox
;;----------------------------------------------------------------------------
;; Preconditions:: main tbox is classified, temp tbox is prepared, state is :i-prepared
;; Procedure:: complete implication sets, one by one in the ordering of system cnames including cnames in TBox (if axioms in TTBox activate them AGAIN)
;; Consequences:: implication sets are completed wrt both TBoxes, state is :i-classified, all positive cnames (plus some reachable negative cnames) are 2-activated
;;____________________________________________________________________________

(defun act-complete-all-imp-sets-2 ()
  "Complete all implication sets one by one (only those user-defined)"  
  ;; old r s information has to be kept and reused
  ;;(init-s-r-structures)

  ;; re-activate all the concepts w.r.t. ttbox, starting with the top concept
  (activate-2 *top*)
    
  (loop
      for c
      from 2
      to (cdr (ods-system-cname-range)) do
	;;(when (cname-struct c)
	(act-complete-imp-set-2 c))  
  )
;;____________________________________________________________________________

(defun act-complete-imp-set-2 (c)
  "Complete the implication set for c. Only S and queue of c are initialized, and existential reachable (posssibly new) cnames are activated during processing as required"
  (activate-2 c)
  
  ;;(format t "~A " c)
  
  (do ((pair
	(q-next-queue)
	(q-next-queue)))
      ;; if all have been processed, return t as success
      ((null pair) t)
    ;; begin do    
    (let ((s-cn (car pair))
	  (queue (cdr pair)))
      ;; process the current cname with all its queue entries
      (dolist (x queue)
	(act-process-2 s-cn x))))

  (verbose "*")
  ;;(incf *total-classified-cnames*)
  )
;;____________________________________________________________________________

(defun act-process-2 (a x)
  "Process the application of Completion Rules corresponding to a and x in an activation approach"
  ;; there are three cases for x; we do case distinction as follows:
  ;; (1) conjuctive condition - consequence pair: ((A1...An) . B)
  (when (and (listp x)
	     (listp (car x)))
    
    ;; if the consequence (cdr x) of this conditional is alredy in S(A)
    ;; no need to consider this conditional
;;    (if (check-label (cdr x) a)
;;	(return-from act-process-2))
    
    ;; if one of the conjuncts are not satisfied in a, do nothing
    ;; otherwise, set x to be the consequence
    (if (dolist (conj (car x) t)	   
	  (unless (check-label-both conj a)
	    (return-from act-process-2)))
	(setq x (cdr x))))
  
  ;; (2) exists restriction: (r . B)
  (when (listp x)
    (let ((r (car x))
	  (b (cdr x)))
      ;; if the edge already exists; dont do again
      (if (check-edge-both a r b)
	  (return-from act-process-2))
      
      ;; if a implies (some r *bot*) == *bot*, meaning that a is unsatisfiable
      (when (is-unsat-both b)
	(process-unsat-cname-2 a)
	(return-from act-process-2))
      
      (activate-2 b)
      (process-new-edges-2 a r b)
      (return-from act-process-2)))
  
  ;; (3) the bottom or unsat cname
  (when (is-unsat-both x)
    (process-unsat-cname-2 a)    
    (return-from act-process-2))
  
  
  ;; (4) concept name: B
  ;; if the label already exists; dont do again
  (if (check-label-both x a)
      (return-from act-process-2))       
  
  (q-enqueue a (o-nexts-both x))
  (add-label-2 x a)
  
  ;;(cache-subs x a)
  ;; modify the queue of all predecessors of A
  (loop ;; for each system rname 
      for r from 0 to (- (length (ods-rname-array)) 1)
      do (let ((new-items (o-nexts-both (cons r x))))
	   ;; only non-empty set should be added to queues
	   (when new-items
	     ;; a is also an r-predecessor, if r is reflexive
	     ;; note that the following when-clause can be replaced with
	     ;; (q-enqueue a (o-nexts-reflexive x)) outside the loop, but
	     ;; I believe this is more optimized.
	     (when (r-reflexive r)
	       (q-enqueue a new-items))
	     ;; apply this to each predecessor B
	     (dolist (b (r-preds-both a r))
	       (q-enqueue b new-items)))
	   ))
  )
;;____________________________________________________________________________



;;____________________________________________________________________________
;;____________________________________________________________________________
;;;;
;;;; Goal-directed varient for single subsumption test
;;;; Compute like act-process but up to the point where positive subsumption
;;;; can be answered.
;;____________________________________________________________________________
;;____________________________________________________________________________

(defmacro subs (b a)
  `(test-subs (system-cname ',b)
	      (system-cname ',a)))


(defun test-subs (s-cn-b s-cn-a)
  "Compute pairwise subsumption, ie s-cn-a <=? s-cn-b, in lazy manner and cache intermediate subsumption results (Goal-directed version of act-complete-all-imp-sets. Initialize only queue and S of s-cn and process as usual. Existential reachable cnames are activated. It returns when subsumption in question is verified or when the queues are empty.)"
  
  (clear-queue-s) ;; to be removed when not benchmarking
  
  ;; lazily initialize S and queue: only for s-cn-a and top
  (init-queue-s *top*)
  (init-queue-s s-cn-a)
  
  ;;==========================  
  (do ((pair
	(q-next-queue)
	(q-next-queue)))
      ;; if all have been processed, return nil as a <= b does not hold
      ((null pair) nil)
    ;; begin do    
    (let ((s-cn (car pair))
	  (queue (cdr pair))
	  (positive nil))
      ;; process the current cname with all its queue entries
      (dolist (x queue)
	;;(print (cons s-cn x))
	;; ENABLING OPTIMIZATION
	(if (ont-tbox-primitive?)
	    (when (goal-directed-process-no-existentials s-cn x s-cn-a s-cn-b)
	      (setq positive t))
	  (when (goal-directed-process s-cn x s-cn-a s-cn-b)
	    (setq positive t))
	  )
	
	)
	
      (when positive
	(return-from test-subs t))))
  nil)
;;_____________________________________________________________________________

(defun goal-directed-process (a x s-cn-a s-cn-b)
  "Process the application of Completion Rules corresponding to a and x in an activation approach. Moreover, whenever the targeted subsumption is known to hold, return positive result instantly; otherwise, complete this implication set"
  ;;(msg "process ~a with ~a; targeting ~a <= ~a" a x s-cn-a s-cn-b)
  
  ;; there are three cases for x; we do case distinction as follows:
  ;; (1) conjuctive condition - consequence pair: ((A1...An) . B)
  (if (and (listp x)
	   (listp (car x)))
      ;; if one of the conjuncts are not satisfied in a, do nothing
      ;; otherwise, set x to be the consequence
      (if (dolist (conj (car x) t)	   
	    (unless (check-label conj a)
	      (return-from goal-directed-process)))
	  (setq x (cdr x))))
  
  ;; (2) exists restriction: (r . B)
  (when (listp x)
    (let ((r (car x))
	  (b (cdr x)))
      ;; if the edge already exists; dont do again
      (when (check-edge a r b)
	(return-from goal-directed-process))
	
      ;; if a implies (some r *bot*) == *bot*, meaning that a is unsatisfiable
      (when (is-unsat b)
	(process-unsat-cname a)
	(return-from goal-directed-process))
	
	
      (activate b)
      (process-new-edges a r b)
      (return-from goal-directed-process)))
  
  ;; (3) the bottom or unsat cname
  (when (is-unsat x)
    (process-unsat-cname a)    
    ;; if s-cn-a = a = *bot*, then subs holds
    (return-from goal-directed-process (eq a s-cn-a)))

  
  ;; (4) concept name: B
  ;; if the label already exists; dont do again
  (if (check-label x a)
      (return-from goal-directed-process))       
  
  (q-enqueue a (o-nexts x))
  (add-label x a)
  ;;(cache-subs x a)
  ;; modify the queue of all predecessors of A
  (loop ;; for each system rname 
      for r from 0 to (- (length *rname-array*) 1)
      do (let ((new-items (o-nexts (cons r x))))
	   ;; apply this to each predecessor B
	   ;; but, only non-empty set should be added to queues
	   (when new-items
	     (dolist (b (r-preds a r))
	       (q-enqueue b new-items)))
	   ))

  ;; check targeted pairwise subsumption
  (if (and (eq s-cn-a a)
	   (eq s-cn-b x))
      ;; return t as the subsumption in question is verified
      (return-from goal-directed-process t))
  
  nil)
;;_____________________________________________________________________________



(defun goal-directed-process-no-existentials (a x s-cn-a s-cn-b)
  "Process the application of Completion Rules corresponding to a and x in an activation approach. Moreover, whenever the targeted subsumption is known to hold, return positive result instantly; otherwise, complete this implication set"
  ;;(msg "process ~a with ~a; targeting ~a <= ~a" a x s-cn-a s-cn-b)
  
  ;; there are three cases for x; we do case distinction as follows:
  ;; (1) conjuctive condition - consequence pair: ((A1...An) . B)
  (if (and (listp x)
	   (listp (car x)))
      ;; if one of the conjuncts are not satisfied in a, do nothing
      ;; otherwise, set x to be the consequence
      (if (dolist (conj (car x) t)	   
	    (unless (check-label conj a)
	      (return-from goal-directed-process-no-existentials)))
	  (setq x (cdr x))))
  
  ;; (2) exists restriction: (r . B)
  (when (listp x)    
    (return-from goal-directed-process-no-existentials))
  
  ;; (3) the bottom or unsat cname
  (when (is-unsat x)
    (process-unsat-cname a)    
    ;; if s-cn-a = a = *bot*, then subs holds
    (return-from goal-directed-process-no-existentials (eq a s-cn-a)))
  
  ;; (4) concept name: B
  ;; if the label already exists; dont do again
  (if (check-label x a)
      (return-from goal-directed-process-no-existentials))       
  
  (q-enqueue a (o-nexts x))
  (add-label x a)
    
  ;; check targeted pairwise subsumption
  (if (and (eq s-cn-a a)
	   (eq s-cn-b x))
      ;; return t as the subsumption in question is verified
      (return-from goal-directed-process-no-existentials t))
  
  nil)
;;_____________________________________________________________________________





;;_____________________________________________________________________________
;;_____________________________________________________________________________
;;;;
;;;; An adaptation from the activation approach to compute a particular 
;;;; subsumption relationship and to return as soon as known.
;;;; This version exploits 2 TBoxes (permanent and temporary) to achieve 
;;;; subsumption queries between two complex concept descriptions
;;;; Idea is simple: add two GCIs and test subsumption between cnames
;;;; Realization is hard: need to maintain seperate (2 sets) of data structures
;;;; and they must be deleted afterward, TBox state and logical consequences 
;;;; must be rolled back
;;_____________________________________________________________________________
;;_____________________________________________________________________________

(defmacro ?implies (c1 c2)
  "to query implication between two (complex) concepts prior or after classification, e.g. 
(?implies (some r (and A B))
	  (and (some r A) (some r B)))"
  `(q-complex-concept-subsumes-f ',c2 ',c1))
(defmacro ?subsumes (c2 c1)
  "to query subsumption between two (complex) concepts prior or after classification"
  `(q-complex-concept-subsumes-f ',c2 ',c1))


(defun q-complex-concept-subsumes-f (c2 c1 &optional yes?)
  ;; test whether c1 implies c2
  "Introduce (at least; maybe more from normalization) two new names, add GCIs to the temp TBox, viz., (implies a1 c1) and (implies c2 a2). Then the ttbox is classified against the old classification information, and the subsumption in question -- now turned to be (implies? a1 a2) -- is tested and returned."
  
  ;; Ontology state handling & wellform checking stuffs
  (cond
   ((ont-state? :cleared)
    (msg "The operation needs ontology to be normalized...")
    (normalize)
    (setf (ont-state) :prepared)
    )
   ((ont-ttbox-active?)
    (msg "Supplemental axioms exist; please `commit-ttbox' or `deactivate-ttbox' before retry this query.")
    (return-from q-complex-concept-subsumes-f ':ont-state-error)
    )
   ((not (el-concept c1))
    ;; c1 must be an EL concept
    (err "Syntax: \"~A\" is not a well-formed EL concept" c1)
    (return-from q-complex-concept-subsumes-f ':malformed-concept)
    )
   ((not (el-concept c2))
    ;; c2 must be an EL concept
    (err "Syntax: \"~A\" is not a well-formed EL concept" c2)
    (return-from q-complex-concept-subsumes-f ':malformed-concept)
    )
   )
  
  (setq c1 (and-expansion c1))
  (setq c2 (and-expansion c2))   
     
  (cond
   ;; =====================>        
   ((equal c1 c2)
    (setq yes? t)
    )
   ;; =====================>     
   ((or (listp c1)
	(listp c2))
    (let ((tbox-classified? (ont-state? (:classified :taxonomized))))
      ;; activate ttbox to host additional constraints
      (activate-ttbox-f)
      (msg "TTBox activated for complex subsumption query")
      ;; one of the operand is a complex concept, ttbox must be used
      (let ((a1 (get-subconcept-name c1 :side 'l)) ;; revise sides
	    (a2 (get-subconcept-name c2 :side 'r)))
	;; add two GCI constraints for a1 and a2     
	(normalize-left a1 c1)
	(normalize-left c2 a2)
	;; normalize the rhs of those axioms that have been processed on lhs
	(dolist (axiom (ods-gci-list2) t)
	  (normalize-right-w-range (cdr axiom)
				   (car axiom)))
	(setf (ods-gci-list2) nil)      
	;; test subsumption between the newly introduced cnames      
	(setq yes? (if tbox-classified? ;; old state, of course
		       (test-subs-2-tbox-classified   a2 a1)
		     (test-subs-2-tbox-not-classified a2 a1)))
	)
      ;; reset to its previous state, removing all consequences caused from ttbox    
      (deactivate-ttbox-f)
      ))
   ;; =====================>
   (t       
    (let ((s-c1 (system-cname (synonym-of c1)))
	  (s-c2 (system-cname (synonym-of c2))))            
      (cond
       ((or (null s-c1)
	    (null s-c2))
	(setq yes? nil)
	)
       ((or (eq s-c1 *bot*)
	    (eq s-c2 *top*)
	    (eq s-c1 s-c2))
	(setq yes? t)
	)
       ((ont-state? (:classified :taxonomized))
	(setq yes? (if (check-label s-c2 s-c1)
		       t nil))
	)
       (t ;; reasoning is needed, use lazy subsupmtion without caching
	;;(activate-ttbox-f)
	;;(msg "TTBox activated for preclassified subsumption query")
	;;(setq yes? (test-subs-2-tbox-not-classified s-c2 s-c1))
	
	(setq yes? (test-subs s-c2 s-c1))
	;;(deactivate-ttbox-f)
	))
      )))  
  yes?)
;;_____________________________________________________________________________

(defun test-subs-2-tbox-classified (s-cn-b s-cn-a)
  
  (msg "Subsumption test with TBox already classified")
   
  (activate-2 *top*)
  (activate-2 s-cn-a)
  
  ;;==========================  
  (do ((pair
	(q-next-queue)
	(q-next-queue)))
      ;; if all have been processed, return nil as a <= b does not hold
      ((null pair) nil)
    ;; begin do    
    (let ((s-cn (car pair))
	  (queue (cdr pair)))
      ;; process the current cname with all its queue entries
      (dolist (x queue)
	;; if targeted subs is found, return t and reset the queue
	(when (goal-directed-process-2 s-cn x s-cn-a s-cn-b t)  
	  ;; clear queue
	  (setf (ods-queue) (make-queue-struct))
	  (return-from test-subs-2-tbox-classified t)
	  ))))
  (setf (ods-queue) (make-queue-struct))
  (return-from test-subs-2-tbox-classified nil)
  )
;;_____________________________________________________________________________

(defun test-subs-2-tbox-not-classified (s-cn-b s-cn-a)
  
  ;;(msg "Subsumption test prior to classification of TBox")
  
  (clear-queue-s)
  ;;(init-s-r-structures)
  
  (activate-both *top*)
  (activate-both s-cn-a)
  
  ;;==========================  
  (do ((pair
	(q-next-queue)
	(q-next-queue)))
      ;; if all have been processed, return nil as a <= b does not hold
      ((null pair) nil)
    ;; begin do    
    (let ((s-cn (car pair))
	  (queue (cdr pair)))
      ;; process the current cname with all its queue entries
      (dolist (x queue)
	;; if targeted subs is found, return t and reset the queue
	(when (goal-directed-process-2 s-cn x s-cn-a s-cn-b nil)  
	  ;; clear queue
	  (setf (ods-queue) (make-queue-struct))
	  (return-from test-subs-2-tbox-not-classified t)
	  ))))
  (setf (ods-queue) (make-queue-struct))
  (return-from test-subs-2-tbox-not-classified nil)
  )
;;_____________________________________________________________________________

(defun goal-directed-process-2 (a x s-cn-a s-cn-b tbox-classified?)
  "Process the application of Completion Rules corresponding to a and x in an activation approach. Moreover, whenever the targeted subsumption is known to hold, return positive result instantly; otherwise, complete this implication set"
  
  ;;(msg "process ~a with ~a; targetting ~a <= ~a" a x s-cn-a s-cn-b)
  
  ;; there are three cases for x; we do case distinction as follows:
  ;; (1) conjuctive condition - consequence pair: ((A1...An) . B)
  (if (and (listp x)
	   (listp (car x)))
      ;; if one of the conjuncts are not satisfied in a, do nothing
      ;; otherwise, set x to be the consequence
      (if (dolist (conj (car x) t)	   
	    (unless (check-label-both conj a)
	      (return-from goal-directed-process-2)))
	  (setq x (cdr x))))
  
  ;; (2) exists restriction: (r . B)
  (when (listp x)
    (let ((r (car x))
	  (b (cdr x)))
      ;; if the edge already exists; dont do again
      (when (check-edge-both a r b)
	(return-from goal-directed-process-2))
	
      ;; if a implies (some r *bot*) == *bot*, meaning that a is unsatisfiable
      (when (is-unsat-both b)
	(process-unsat-cname-2 a)
	(return-from goal-directed-process-2))
	
      ;; supposed to be (activate-2) if pre-classified
      (if tbox-classified?
	  (activate-2 b)
	(activate-both b))
      
      (process-new-edges-2 a r b)
      (return-from goal-directed-process-2)))
  
  ;; (3) the bottom or unsat cname
  (when (is-unsat-both  x)
    (process-unsat-cname-2 a)
    ;; if s-cn-a = a = *bot*, then subs holds
    (return-from goal-directed-process-2 (eq a s-cn-a))) 

  
  ;; (4) concept name: B
  ;; if the label already exists; dont do again
  (if (check-label-both x a)
      (return-from goal-directed-process-2))       
  
  (q-enqueue a (o-nexts-both x))
  (add-label-2 x a)
  ;;(cache-subs x a)
  ;; modify the queue of all predecessors of A
  (loop ;; for each system rname 
      for r from 0 to (- (length (ods-rname-array)) 1)
      do (let ((new-items (o-nexts-both (cons r x))))
	   ;; only non-empty set should be added to queues
	   (when new-items
	     ;; a is also an r-predecessor, if r is reflexive
	     (when (r-reflexive r)
	       (q-enqueue a new-items))
	     ;; apply this to each predecessor B
	     (dolist (b (r-preds-both a r))
	       (q-enqueue b new-items)))
	   ))

  ;; check targeted pairwise subsumption
  (if (and (eq s-cn-a a)
	   (eq s-cn-b x))
      ;; return t as the subsumption in question is verified
      (return-from goal-directed-process-2 t))
  
  nil)
;;_____________________________________________________________________________