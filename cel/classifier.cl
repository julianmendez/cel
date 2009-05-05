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

(defvar *array-marker* nil)
(defvar *bottom-search-avoidable* nil)

(defun init-classifier ()
  (make-a-marker)
  (setq *bottom-search-avoidable* (not (or (zerop (ont-n-pcdefs))
					   (ont-tbox-general?)
					   (ont-has-mult-def?)))))

(defmacro make-a-marker ()
  `(setq *array-marker* (make-array (+ 1 (cdr (ods-system-cname-range))))))
(defmacro a-marked (s-cn)
  `(aref *array-marker* ,s-cn))
;;_____________________________________________________________________________

(defun mark-ancestors (x marker)
  "Mark as marker of c all ancesters x in the hierarchy"
  (setf (c-marked x) marker)
  (dolist (y (c-parents x))
    (unless (eq (c-marked y) marker)
      (mark-ancestors y marker))))
;;_____________________________________________________________________________

(defun mark-descendants (x marker)
  "Mark as marker of c all descendants x in the hierarchy"
  (setf (c-marked x) marker)
  (dolist (y (c-children x))
    (unless (eq (c-marked y) marker)    
      (mark-descendants y marker))))
;;_____________________________________________________________________________

(defun mark-told-subsumers-and-ancestors (c subsumer)
  "Mark as subsumer of c all told subsumers and their ancestors in the hierarchy"
  (dolist (x (c-told-subsumers c))
    (unless (eq (c-marked x) subsumer)
      (when (c-classified x)	       
	(mark-ancestors x subsumer))
      (mark-told-subsumers-and-ancestors x subsumer))))
;;_____________________________________________________________________________

(defun mark-cached-subsumers-and-ancestors (c subsumer)
  "Mark as subsumer of c all cached subsumers and their ancestors in the hierarchy"
  (dolist (x (s-labels c))
    (unless (or (is-new-cname x)
		(eq (c-marked x) subsumer))
      (mark-ancestors x subsumer))))
;;_____________________________________________________________________________

(defun mark-told-subsumees-and-descendants (c subsumee)
  "Mark as subsumee of c all told subsumees and their descendants in the hierarchy"
  (dolist (x (c-told-subsumees c))
    (when (c-classified x)
      (mark-descendants x subsumee))
    (mark-told-subsumees-and-descendants x subsumee)))
;;_____________________________________________________________________________


(defun mark-cached-subsumees-and-descendants (c subsumee)
  "Mark as subsumee of c all cached subsumees and their descendants in the hierarchy"
  (dolist (x (c-cached-subsumees c))
    (when (and (not (is-new-cname x))
	       (c-classified x))
      (mark-descendants x subsumee))))
;;_____________________________________________________________________________

(defun mark-descendants-as-candidate (c candidate)
  (unless (eq (c-marked-as-candidate c) candidate)
    (setf (c-marked-as-candidate c) candidate)
    (dolist (child (c-children c))
      (mark-descendants-as-candidate child candidate))))
;;_____________________________________________________________________________

(defun mark-marked-descendants-as-candidate (x 
					     marker 
					     candidate 
					     &optional (visited (gensym)))
  (setf (c-marked x) visited)
  (when (eq (c-marked-as-candidate x) marker)
    (setf (c-marked-as-candidate x) candidate))
  (dolist (y (c-children x))
    (unless (eq (c-marked y) visited)
      (mark-marked-descendants-as-candidate y marker candidate visited))))
;;_____________________________________________________________________________

(defun mark-bottom-search-candidates (parents candidate)
  "Mark as candidate the successors of all parents"
  (case (length parents)
    ((1) 
     (dolist (c (c-children (car parents)))
       (mark-descendants-as-candidate c candidate)))
    ((2)
     (let ((marker (gensym)))
       (dolist (c (c-children (car parents)))
	 (mark-descendants-as-candidate c marker))
       (dolist (c (c-children (cadr parents)))
	 (mark-marked-descendants-as-candidate c marker candidate))))
    (t
     (let ((new-marker (gensym))
	   (old-marker nil))
       (dolist (c (c-children (cadr parents)))
	 (mark-descendants-as-candidate c new-marker))
       (dolist (p (cddr parents))
	 (setq old-marker new-marker)
	 (setq new-marker (gensym))
	 (dolist (c (c-children p))	   
	   (mark-marked-descendants-as-candidate c old-marker new-marker)))
       (dolist (c (c-children (car parents)))
	 (mark-marked-descendants-as-candidate c new-marker candidate))))))
;;_____________________________________________________________________________

(defun enhanced-top-subs (y c subsumer non-subsumer right-reachable)
  "Return t if c <= y by exploiting previously computed results. If it was never computed, call the lazy-subsumption procedure test-subs"
  (cond
   ;; using previously tested subsumption results
   ((eq (c-marked y) subsumer) t)
   ((eq (c-marked y) non-subsumer) nil)
   ;; using gci reachability knowledge
   ;;((not (eq (c-marked y) right-reachable)) nil)
   ;; using negative results for parent concepts
   ((and (every #'(lambda (z) (enhanced-top-subs z c 
						 subsumer 
						 non-subsumer
						 right-reachable))
		(c-parents y))
	 (test-subs y c))
    (setf (c-marked y) subsumer)
    t)
   (t
    (setf (c-marked y) non-subsumer)
    nil)))
;;_____________________________________________________________________________

(defun top-search (c x &optional (subsumer (gensym))
				 (non-subsumer (gensym))
				 (right-reachable nil)
				 (visited (gensym)))
  "Search top-down for direct parents of the concept c"
  (setf (c-visited x) visited)
  (let ((pos-succ (remove-if-not #'(lambda (y) 
				     (enhanced-top-subs y c 
							subsumer 
							non-subsumer
							right-reachable))
				 (c-children x))))
    (if (null pos-succ)
	(return-from top-search (list x))    
      (mapcan #'(lambda (y) 
		  (unless (eq (c-visited y) visited)
		    (top-search c y 
				subsumer 
				non-subsumer 
				right-reachable 
				visited)))
	      pos-succ))))
;;_____________________________________________________________________________

(defun enhanced-bottom-subs (c y subsumee non-subsumee candidate left-reachable)
  "Return t if y <= c by exploiting previously computed results. If it was never computed, call the lazy-subsumption procedure test-subs"
  (cond
   ;; using top-search knowledge
   ((not (eq (c-marked-as-candidate y) candidate)) nil)
   ;; using previously tested subsumption results
   ((eq (c-marked y) subsumee) t)
   ((eq (c-marked y) non-subsumee) nil)
   ;; using gci reachability knowledge
   ;;((not (eq (c-marked y) left-reachable)) nil)
   ;; using negative results for lower concepts
   ((and (every #'(lambda (z) 
		    (enhanced-bottom-subs c z 
					  subsumee 
					  non-subsumee 
					  candidate
					  left-reachable))
		(c-children y))
	 (test-subs c y))
    (setf (c-marked y) subsumee)
    t)
   (t
    (setf (c-marked y) non-subsumee)
    nil)))
;;_____________________________________________________________________________

(defun bottom-search (c x candidate &optional (subsumee (gensym))
					      (non-subsumee (gensym))
					      (left-reachable nil)
					      (visited (gensym)))
  "Search bottom-up for direct children of the concept c"
  (setf (c-visited x) visited)
  (let ((pos-pred (remove-if-not 
		   #'(lambda (y) 
		       (enhanced-bottom-subs c y 
					     subsumee
					     non-subsumee
					     candidate
					     left-reachable))
		   (c-parents x))))
    (if (null pos-pred)
	(return-from bottom-search (list x))    
      (mapcan #'(lambda (y) 
		  (unless (eq (c-visited y) visited)
		    (bottom-search c y 
				   candidate 
				   subsumee 
				   non-subsumee 
				   left-reachable
				   visited)))
	      pos-pred))))
;;_____________________________________________________________________________

(defun classify-all-cnames-in-phierarchy ()
  "Classify cnames in phierarchy in definitional order"
  (dolist (c (ods-phierarchy-cname-list))
    (ordered-classify-cname-in-phierarchy c))
  
  ;; complete the open bottom of the pHierarchy
  (let ((bot-parents nil)
	(bot-list (list *bot*)))
    (dolist (c (ods-phierarchy-cname-list))
      (unless (c-children c)
	(setf (c-children c) bot-list)
	(push c bot-parents)))
    (setf (c-parents *bot*) bot-parents))
  )
;;_____________________________________________________________________________

(defun ordered-classify-cname-in-phierarchy (c &optional (subs-path nil))
  "Classify cnames in phierarchy in definitional order"
  (when (member c subs-path
		:test 'eq)
    ;; all cnames from the head to c in subs-path are in a cycle, 
    ;; so they are all equivalent!
    (setf (ont-tbox-cyclic?) t)
    (return-from ordered-classify-cname-in-phierarchy))
  (unless (c-classified c)
    (dolist (x (c-told-subsumers c))
      (ordered-classify-cname-in-phierarchy x (cons c subs-path)))
    (classify-cname-in-phierarchy c)))
;;_____________________________________________________________________________

(defun classify-cname-in-phierarchy (c)
  "Classify c which is in pHierarchy. This is done by looking at its told subsumers, ie. they are candidates for parents, and bottom is the only child"
  (let ((candidates (adjoin *top* (c-told-subsumers c)
			    :test 'eq))
	(marker (gensym))
	(parents nil))
    (dolist (x candidates)
      (dolist (y (c-parents x))
	(mark-ancestors y marker)))    
    (setq parents (remove-if #'(lambda (x)
				 (eq (c-marked x) marker))
			     candidates))
    (insert-classified-cname c parents)
    (unless (zerop *profiling-mode*)
      (format *profile-out-stream*
	      "~&(cname-pre-classified ~S~&  :parents ~S"
	      (user-cname c)
	      (user-cnames parents))))
  (incf (ods-total-classified-cnames))
  (verbose "^")
  (setf (c-classified c) t))
;;_____________________________________________________________________________

(defun ordered-classify-cname-with-cycles (c &optional (subs-path nil))
  "Classify concept names in definitional order, ie told subsumers of c are classified before c. Terminological cycles can be detected at this phase. If this is the case, special care for cycles has to be ensured: concepts in a cycle are equivalent, classify one of them and add as equivalent concepts the others"
  (let ((cycle (member c subs-path
		       :test 'eq)))
    (case (length cycle)
      ((0) ;; no cycle
       (dolist (x (c-told-subsumers c))
	 (ordered-classify-cname x (cons c subs-path)))
       (classify-cname c))
      ((1) ;; direct cycle, ie A <= (and A C) === A <= C
       (return-from ordered-classify-cname-with-cycles))
      (t ;; terminological cycle  
       (setf (ont-tbox-cyclic?) t)
       
       ))))
;;_____________________________________________________________________________

(defun ordered-classify-cname (c &optional (subs-path nil))
  "Classify concept names in definitional order, ie told subsumers of c are classified before c. Terminological cycles can be detected at this phase"
  (when (member c subs-path
		:test 'eq)
    ;; all cnames from the head to c in subs-path are in a cycle, 
    ;; so they are all equivalent!
    (setf (ont-tbox-cyclic?) t)
    (return-from ordered-classify-cname))
  (unless (c-classified c)
    (dolist (x (c-told-subsumers c))
      (ordered-classify-cname x (cons c subs-path)))
    (classify-cname c)))
;;_____________________________________________________________________________

(defun classify-cname (c)
  "Classify cname c by means of top-search and bottom-search"
  (let ((parents '())
	(children '())
	(subsumer (gensym))
	(non-subsumer (gensym))
	(right-reachable (gensym))
	;; profiling
	(start-time (get-internal-run-time))
	(start-subs (ods-total-subs-tests))
	(top-search-subs-tests 0)
	(start-s-counter (ods-total-s-counter))
	(hasse-msg nil))
    ;; pre-marking
    ;;(mark-right-reachable-and-ancestors c right-reachable)
    ;;(mark-told-subsumers-and-ancestors c subsumer)
    ;;(mark-cached-subsumers-and-ancestors c subsumer)
    (setf (c-marked *top*) subsumer)
    (setq parents (top-search c *top* 
			      subsumer 
			      non-subsumer
			      right-reachable))
    (setq top-search-subs-tests (- (ods-total-subs-tests)
				   start-subs))
    (cond
     ;; check if c is an equivalent of the only parent
     ((and (= (length parents) 1)
	   (test-subs c (car parents)))
      (let ((c-e (car parents))
	    (c-u (c-user-name c)))
	(setf (c-equivalent c) c-e) ;; c-e is a representative of c
	(pushnew c (c-equivalent c-e)) ;; c-e has c in its equivalent set
	(setf (system-cname c-u) c-e)
	(push c-u (c-equivalent-user-names c-e))
	(unless (zerop *profiling-mode*)
	  (setq hasse-msg (format nil  ":equivalent ~S"
				  (c-user-name c-e))))))
     ;; otherwise, compute bottom-search
     (t
      (if (and *bottom-search-avoidable*
	       (eq (c-status c) 'primitive-not-ph))
	  ;; the only children for primitive cnames are the bottom-concept
	  (setq children (list *bot*))
	;; otherwise, bottom-search cannot be avoided
	(let ((candidate (gensym))
	      (subsumee (gensym))
	      (non-subsumee (gensym))
	      (left-reachable (gensym)))
	  ;; pre-marking
	  (mark-bottom-search-candidates parents candidate)
	  ;;(mark-left-reachable-and-descendants c left-reachable)
	  ;;(mark-cached-subsumees-and-descendants c subsumee)
	  ;;(mark-told-subsumees-and-descendants c subsumee)
	  (setf (c-marked *bot*) subsumee)
	  (setq children (bottom-search c *bot*
					candidate
					subsumee
					non-subsumee
					left-reachable))))
      (unless (zerop *profiling-mode*)
	(setq hasse-msg
	  (format nil ":parents ~S :children ~S"
		  (user-cnames parents)
		  (user-cnames children))))
      ;; update the Hasse diagram
      (insert-classified-cname c parents children)))
    ;; profile statistical infos into *profile-out-stream*
    (unless (zerop *profiling-mode*)
      (format *profile-out-stream*
	      "~&(cname-classified ~S~&  ~A~&  :subs-tests ~5:D :top-subs-tests ~5:D :s-counter ~5:D :cpu-time ~8Fs)"
	      (user-cname c)
	      hasse-msg
	      (- (ods-total-subs-tests) start-subs)
	      top-search-subs-tests
	      (- (ods-total-s-counter) start-s-counter)
	      (/ (- (get-internal-run-time) start-time)
		 internal-time-units-per-second))))
  ;; c has now been classified, remove its cache and mark as classified
  ;;(remove-cache c)
  (incf (ods-total-classified-cnames))
  (verbose "*")
  (setf (c-classified c) t))
;;_____________________________________________________________________________

(defun insert-classified-cname (c parents &optional children)
  "Add c to the subsumption hierarchy by adding <-edge to parents and >-edge to children and remove all edge between parents and children. Note: no children implicitly means only *bot*"
  (setf (c-parents c) parents)
  (setf (c-children c) children)
  (dolist (parent parents)
    (setf (c-children parent) 
      (cons c (set-difference (c-children parent) children
			      :test 'eq))))
  (dolist (child children)
    (setf (c-parents child)
      (cons c (set-difference (c-parents child) parents
			      :test 'eq)))))

(defun insert-classified-cname2 (c parents 
				 &optional (children (list *bot*))
					   (parent-marker (gensym))
					   (child-marker (gensym)))
  "Add c to the subsumption hierarchy by adding <-edge to parents and >-edge to children and remove all edge between parents and children"
  (setf (c-parents c) parents)
  (setf (c-children c) children)

  (dolist (parent parents)
    (setf (a-marked parent) parent-marker))
  
  (dolist (child children)
    (setf (a-marked child) child-marker)  
    (setf (c-parents child)
      (cons c (remove-if #'(lambda (x) 
			     (eq (a-marked x) parent-marker))
			 (c-parents child)))))
  
  (dolist (parent parents)
    (setf (c-children parent)
      (cons c (remove-if #'(lambda (x) 
			     (eq (a-marked x) child-marker))
			 (c-children parent))))))

(defun insert-classified-cname3 (c parents &optional (children (list *bot*)))
  "Add c to the subsumption hierarchy by adding <-edge to parents and >-edge to children and remove all edge between parents and children"
  (setf (c-parents c) parents)
  (setf (c-children c) children)

  (make-a-marker)
  (dolist (parent parents)
    (setf (a-marked parent) t))
  (dolist (child children)
    (setf (c-parents child)
      (cons c (remove-if #'(lambda (x) 
			     (a-marked x))
			 (c-parents child)))))
  (make-a-marker)    
  (dolist (child children)
    (setf (a-marked child) t))
  (dolist (parent parents)
    (setf (c-children parent)
      (cons c (remove-if #'(lambda (x) 
			     (a-marked x))
			 (c-children parent))))))
;;_____________________________________________________________________________

(defun classify-all-cnames (&optional (t0 (get-internal-run-time)))
  "Classify all concept names"    
  ;; initialize global variables used in the classifier
  (init-classifier)
  ;; pre-activate the top-concept, it is a consequence of all other concepts
  (init-queue-s *top*)    
  ;; if TBox is definitive, ie. no GCIs, no multiple definitions
  ;; we can use pre-classification (pHierarchy)
  (unless (or (ont-tbox-general?)
	      (ont-has-mult-def?))
    ;; after this call, cnames's status can be defined, phierarchy, or primitive-not-ph
    (mark-phierarchy-cnames))

  ;; set the output stream for profiling
  (setq *profile-out-stream* 
    (case *profiling-mode*
      ((0) nil)
      ((1) t)
      ((2) (open (if *profile-file-name* 
		     *profile-file-name*
		   (concatenate 'string 
		     (ont-uri) ".profile"))
		 :direction :output
		 :if-exists :supersede))))  
  (unless (zerop *profiling-mode*)
    (format *profile-out-stream*
	    ";; CEL classification profile of the TBox: ~A"
	    (ont-uri)))

  ;; optimization technique from Dmitry's paper
  ;; classify in definitional order those primitive hierarchy concepts (CD concepts)
  (classify-all-cnames-in-phierarchy)
  
  (setf (ont-phierarchy-time) (/ (- (get-internal-run-time) t0)
				  internal-time-units-per-second))
  
;;;  (msg "...pHierarchy built in ~:Fs. Continue with standard classification..."
;;;       (ont-phierarchy-time))
   
  ;; classify all cnames in definitional order  
  (loop
      for c from 2 to (- (length (ods-cname-array)) 1)
      do (ordered-classify-cname c))
  
  (setf (ont-classification-time) (/ (- (get-internal-run-time) t0)
				      internal-time-units-per-second))
  
  (unless (zerop *profiling-mode*)
    (format *profile-out-stream*
	    "~&(tbox-classified ~&  :total-subs-tests ~:D ~&  :total-s-counter ~:D ~&  :total-classification-time  ~:Fs)"
	    (ods-total-subs-tests)
	    (ods-total-s-counter)
	    (ont-classification-time)
	    ))
  ;; close file stream in case of mode 2
  (when (eq *profiling-mode* 2)
    (close *profile-out-stream*))
  
  t)
;;_____________________________________________________________________________

(defun initialize-hasse (&optional top-bottom-link)
  (loop
      for c from 2 to (- (length (ods-cname-array)) 1)
      do (let ((struct (cname-struct c)))
	   (setf (concept-classified struct) nil)
	   (setf (concept-parents struct) nil)
	   (setf (concept-children struct) nil)
	   (setf (concept-equivalent struct) nil)
	   (setf (concept-equivalent-user-names struct) nil)))
  
  (let ((top-struct (cname-struct *top*))
	(bot-struct (cname-struct *bot*)))
    (setf (concept-children top-struct) (if top-bottom-link `(,*bot*) nil)
	  (concept-parents top-struct) nil
	  (concept-equivalent top-struct) nil
	  (concept-equivalent-user-names top-struct) nil)
    (setf (concept-children bot-struct) nil
	  (concept-parents bot-struct) (if top-bottom-link `(,*top*) nil)
	  (concept-equivalent bot-struct) nil
	  (concept-equivalent-user-names bot-struct) nil)))    
;;_____________________________________________________________________________

(defun re-classify-all-cnames ()
  "Re-classify all concept names"
  (initialize-hasse)  
  (classify-all-cnames))
;;_____________________________________________________________________________

(defun hasse-subs (c1 c2)
  (hasse-offspring c1 c2))

(defun hasse-offspring (c x &optional (offspring (gensym)))
  "Check if x is an offspring of c"
  (unless (eq (c-marked c) offspring)
    (setf (c-marked c) offspring)  
    (or (eq c x)
	(some #'(lambda (y) (hasse-offspring y x offspring))
	      (c-children c)))))
;;_____________________________________________________________________________

(defun hasse-ancestors (c &optional (visited (gensym)))
  "Return all ancestors of c"  
  (setf (c-visited c) visited)
  (cons c (append (c-equivalent c)
		  (mapcan #'(lambda (x)
			      (unless (eq (c-visited x) visited)
				(hasse-ancestors x visited)))
			  (c-parents c)))))
;;_____________________________________________________________________________

(defun hasse-descendants (c &optional (visited (gensym)))
  "Return all descendants of c in the hasse diagram. This set is computed by descending down the hierarchy and mark all visited nodes."  
  (setf (c-visited c) visited)
  (cons c (append (c-equivalent c)
		  (mapcan #'(lambda (x)
			      (unless (eq (c-visited x) visited)
				(hasse-descendants x visited)))
			  (c-children c)))))
;;_____________________________________________________________________________


(defun hasse-concept-descendants-new (c)
  "Direct version: return user cnames"
  (let ((visit-flag (make-hash-table :test 'eq))
	(descendants nil))
    (hasse-concept-descendants-new-r c visit-flag)
    
    (maphash #'(lambda (key value)
		 (declare (ignore value))
		 (setq descendants (append (synonyms (user-cname key))
					   descendants)))		 
	     visit-flag)
    descendants))
  
(defun hasse-concept-descendants-new-r 
    (c &optional (visit-flag (make-hash-table
				:test 'eq)))
  "Mark in visit-flag all concepts reachable from c down the hierarchy"  
  (setf (gethash c visit-flag) t)
  (dolist (x (c-equivalent c))
    (setf (gethash x visit-flag) t))
  (dolist (x (c-children c))
    (unless (or (gethash x visit-flag)
		(c-individual? x))
      (hasse-concept-descendants-new-r x visit-flag))))
;;_____________________________________________________________________________

(defun hasse-concept-descendants (c &optional (visited (gensym)))
  "A varient of hasse-descendants which visits only concept nodes"  
  (setf (c-visited c) visited)
  (cons c (append (remove-if #'(lambda (x)
				 (c-individual? x))
			     (c-equivalent c))
		  (mapcan #'(lambda (x)
			      (cond
			       ;; if we reach an ind, don't go down the path
			       ;; rather jump to the bottom
			       ((c-individual? x)
				(unless (eq (c-visited 0) visited)
				  (hasse-concept-descendants 0 visited)))
			       ;; unvisited concept node: traverse it!
			       ((not (eq (c-visited x) visited))
				(hasse-concept-descendants x visited))
			       (t
				nil)))
			  (c-children c)))))
;;_____________________________________________________________________________

(defun hasse-individual-descendants (c &optional (visited (gensym)))
  "A varient of hasse-descendants which collects only individual nodes"  
  (setf (c-visited c) visited)
  (if (c-individual? c)
      (cons c (mapcan #'(lambda (x)
			  (unless (eq (c-visited x) visited)
			    (hasse-individual-descendants x visited)))
		      (c-children c)))
    (mapcan #'(lambda (x)
		(unless (eq (c-visited x) visited)
		  (hasse-individual-descendants x visited)))
	    (c-children c))))
;;_____________________________________________________________________________


(defun depth (c d)
  (cond ((not (integerp (c-marked c)))
	 (setf (c-marked c) d)
	 (dolist (child (c-children c))
	   (depth child (+ d 1))))
	((> d (c-marked c))
	 (setf (c-marked c) d))))


(defun print-equivalences ()
  (loop for i from 2 to (- (length (ods-cname-array)) 1) do
	(when (c-equivalent-user-names i)
	  (format t "~%~S is equivalent to ~S"
		  (user-cname i)
		  (c-equivalent-user-names i)))))



;;_____________________________________________________________________________
;;_____________________________________________________________________________
;;_____________________________________________________________________________

(defun request-hasse ()
  (when (ont-state? :classified)
    (classify-all-cnames-using-imp-sets)
    (setf (ont-state) :taxonomized)))


(defun classify-all-cnames-using-imp-sets ()
  "Classify all cnames using precomputed (completed) implication sets"
  (initialize-hasse)
  
  ;; classify all user defined cnames one by one using
  ;; - the information from the completed implication sets as told subsumers
  ;; - the completely defined (pHierarchy) technique to avoid (top&bottom search)
  (loop
      for c from 2 to (- (length (ods-cname-array)) 1)
      do (unless (c-classified c)
	   (ordered-classify-cname-using-imp-sets c)))
  
  ;; complete the open bottom of the pHierarchy
  (let ((bot-parents nil)
	(bot-list (list *bot*)))
    (loop
	for c from 1 to (- (length (ods-cname-array)) 1)
	do (unless (c-children c)
	     ;; do only when c has open bottom; has no children
	     (when (listp (c-equivalent c))
	       ;; do only when c is in the hierarchy
	       (setf (c-children c) bot-list)
	       (push c bot-parents))))
    (setf (c-parents *bot*) bot-parents))  
  )
;;_____________________________________________________________________________

(defun ordered-classify-cname-using-imp-sets (c)
  "Classify cnames using implication sets in definitional order"
  (when (eq (s-labels c) *bot*)
    ;; when c is unsatisfiable, ie. equivalent to bottom
    ;; here, bottom is its representative
    (let ((c-user-name (c-user-name c)))
      (setf (c-classified c) t
	    (c-parents c) nil
	    (c-children c) nil)
      (setf (c-equivalent c) *bot*)
      (push c (c-equivalent *bot*))
      (setf (system-cname c-user-name) *bot*)
      (push c-user-name (c-equivalent-user-names *bot*)))
    (return-from ordered-classify-cname-using-imp-sets))
  
  (let ((all-subsumers (remove-if #'(lambda (x) (or (eq c x)
						    (< x 0)))
				  (union (s-labels c)
					 (s-labels *top*)
					 :test 'eq)))
	(candidates nil))
    (dolist (x all-subsumers)
      
      (cond
       ;; x has previously been classified
       ((c-classified x)
	(when (listp (c-equivalent x))
	  ;; add it only when it's in the hierarchy
	  (push x candidates)))
       
       ;; -- when not, classify it first --
       
       ;; if x and c are equivalent
       ((check-label c x)
	;; both subsume each other--equivalent
	;; here, c is to be classifed and a representative of x
	(let ((x-user-name (c-user-name x)))
	  (setf (c-classified x) t
		(c-parents x) nil
		(c-children x) nil)
	  (setf (c-equivalent x) c) ;; c is a representative of x
	  (push x (c-equivalent c)) ;; c has x in its equivalent set
	  (setf (system-cname x-user-name) c)
	  (push x-user-name (c-equivalent-user-names c))))
        
       ;; if x is strict subsumer of c, then classify it
       (t
	(ordered-classify-cname-using-imp-sets x)
	(push x candidates))))
;;    (msg "classifying ~A with subsumers ~A and candidates ~A"
;;	 c all-subsumers candidates)
    (classify-cname-using-imp-sets c candidates)))
;;_____________________________________________________________________________

(defun classify-cname-using-imp-sets (c candidates)
  "Classify c with the help of its implication set. This is done by looking at its precomputed subsumers, ie. they are candidates for parents, and bottom is the only child (given the classification in definitional order)"
  (let ((marker (gensym))
	(parents nil))
    
    (dolist (x candidates)
      (dolist (y (c-parents x))
	(mark-ancestors y marker)))
    ;; c itself cannot be parent
    ;;(setf (c-marked c) marker)
    (setq parents (remove-if #'(lambda (x)
				 (eq (c-marked x) marker))
			     candidates))
    (insert-classified-cname c parents))
;;;    (unless (zerop *profiling-mode*)
;;;      (format *profile-out-stream*
;;;	      "~&(cname-pre-classified ~S~&  :parents ~S"
;;;	      (user-cname c)
;;;	      (user-cnames parents))))
  (incf (ods-total-classified-cnames))
  ;;(verbose "^")
  ;;(msg "^ ~S classified" c)
  (setf (c-classified c) t))
;;_____________________________________________________________________________

