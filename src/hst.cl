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



(defvar *hst-path-labels* nil) ;; HS
(defvar *hst-node-labels* nil) ;; S


(defparameter *hst-debug* nil)


(defmacro justs () `(length *hst-node-labels*))
(defmacro paths () `(length *hst-path-labels*))



(defun extract-all-minas-f (c d &optional (out t))
   "Extract ALL minimal axiom sets (MinAs) for the subsumption C <= D"
  
  (unless (q-complex-concept-subsumes-f d c)
    ;; if the subsumption does not hold
    (msg "The subsumption \"~S [= ~S\" does not hold!"
	 c d)
    (return-from extract-all-minas-f nil))
    
  ;; ==================================================
  (setq *hst-path-labels* nil) ;; HS
  (setq *hst-node-labels* nil) ;; S
  
  (setq m-time 0) ;; module extraction time
  (setq m-size 0) ;; module size
  (setq hst-search-time 0) ;; excluding substest
  (setq bb-subs-time 0) ;; overall subs test time
  (setq bb-subs-count 0) ;; number of subs test
  ;; ==================================================
    

  (let* (module just t0)
	 
    (setq t0 (get-internal-run-time))  
    (setq module (extract-c-module-f c))
    (setq m-time (/ (- (get-internal-run-time) t0)
		    internal-time-units-per-second))
    (setq m-size (length module))
    
    
    (setq just (minimize-module-logarithmically c d (copy-list module)))
    
    ;;(setq just (minimize-module-linearly c d module))
        
    ;; ==================================================
    (push just *hst-node-labels*)

    (setq bb-subs-time 0) ;; overall subs test time
    (setq bb-subs-count 0) ;; number of subs test
    (setq t0 (get-internal-run-time))      
    
    (let ((node-id 0))
    (dolist (ax just)
      (incf node-id)
      (hst-search c d 
		  (set-difference module (list ax))
		  (list ax) ;; first paths are singleton
		  (list node-id) ;; node id		       
		  )
      ))

    
    (setq hst-search-time (/ (- (get-internal-run-time) t0)
			     internal-time-units-per-second))

    ;; ==================================================

    
    ;; output the module depending on the parameter out
    (cond
     ((eq out nil)
      *hst-node-labels*)
      ;;(length just))
     ;;just)
     ((eq out t)
      (print-all-minas t c d)
      T)
     (t ;; out is a file name to which module is outputted
      (with-open-file (output-stream out
		       :direction :output 
		       :if-exists :supersede)
	(print-all-minas output-stream c d)
	)
      T))
    ))
;;_____________________________________________________________________________

(defun print-all-minas (stream c d)
  
  (format stream "~%(;; There are ~S MinAs for ~S [= ~S~%"
	  (length *hst-node-labels*)
	  c d)
  ;; flush out each of the MinAs with an empty line between them
  (dolist (just *hst-node-labels*)
    (format stream "~%  ~S~%"
	    just))
  (format stream "~%) ;; End of all MinAs")
  )


(defun op-1 (new-path old-path)
  (when (subsetp old-path new-path :test 'eq)
    (return-from op-1 t))
    
  (do ((p (cdr old-path)
	  (cdr p)))
      ((null p) nil)
    ;; do for every prefix-path of old-path
    (when (and (= (length p) (length new-path))
	       (subsetp p new-path :test 'eq)
	       (subsetp new-path p :test 'eq))
      (return-from op-1 t)))  
  )

(defun hst-search (c d ont path node-id
		   &optional (new-just nil)
			     (just-reuse nil)
			     (branch-id 0)
			     )
  ;; ======================    
  (when (>= (justs) 10)
    (return-from hst-search))
  ;; =======================
  
  
			      
  (hst-debug "~%~a " node-id)
  
  (when (some #'(lambda (h) (op-1 path h))
	      *hst-path-labels*)
    (hst-debug "X")
    (return-from hst-search))
      
   
   (cond 
    ((some #'(lambda (just) 
	       (when (and path 
			  (not (intersection path just)))
		 (setq new-just just)
		 (setq just-reuse t)
		 t
		 ))
	   *hst-node-labels*)
     )
     
    (t
     ;;(print (length ont))
     (setq new-just (minimize-module-logarithmically c d 
						     (copy-list ont)))
     ;;(print (length ont))
     ;;(format t "~%single-mina --> ~a" (if new-just t nil))
     )
    )
   
   ;;(format t "~%removing ~a --> ~a" ax new-just)
   
   (cond
    (new-just
     (unless just-reuse
       (hst-debug "MinA")
       (pushnew new-just *hst-node-labels* 
		:test #'(lambda (x y)
			  (and (= (length x) (length y))
			       (subsetp x y :test 'eq)
			       (subsetp y x :test 'eq))
			  )
		))       
     (dolist (new-ax new-just)
       (incf branch-id)
       (hst-search c d 
		   (set-difference ont (list new-ax))
		   (cons new-ax path)
		   (cons branch-id node-id))
       ))
    
    (t
     (when path
       (hst-debug "/")
       (pushnew path *hst-path-labels* :test 'equal)))
       ;;(pushnew path *hst-path-labels* :test 'equiv)))      
    )
   )
;;_____________________________________________________________________________



(defun output-all-minas (&key (file-name nil))
  (if file-name
      (with-open-file (output-stream file-name		   
		       :direction :output 
		       :if-exists :supersede)
	(output-all-minas-f output-stream))
    (output-all-minas-f t)))
    
    
(defun output-all-minas-f (output-stream)
  (format output-stream 
	  "( ;; ~a MinAs ~%" 
	  (length *hst-node-labels*))
  (dolist (mina *hst-node-labels*)      
    (format output-stream 
	    "~% ;; size ~a~% ~s~%" 
	    (length mina)
	    mina))
  (format output-stream
	  "~%)")
  )
;;_____________________________________________________________________________


(defun hst-debug (string &rest values)
  (when *hst-debug*
    (apply #'format t 
	   string values)))

(defun equiv (list1 list2)
  (and (subsetp list1 list2 :test 'eq)
       (subsetp list2 list1 :test 'eq)))

(defun subsetp-hash-eq (list1 list2)
  (let ((list2-hash (make-hash-table :test 'eq)))
    (dolist (item list2)
      (setf (gethash item list2-hash) t))
    (dolist (item list1)
      (unless (gethash item list2-hash)
	(return-from subsetp-hash-eq nil))
      )
    t))




;;_____________________________________________________________________________
;;;
;;;  GLOBAL VARIABLES USED FOR BENCHMARKING HST-MOB
;;_____________________________________________________________________________


(defvar m-time 0) ;; module extraction time
(defvar m-size 0) ;; module size
(defvar hst-search-time 0) ;; including substest (- hst-search-time bb-subs-time)
(defvar bb-subs-time 0) ;; overall subs test time
(defvar bb-subs-count 0) ;; number of subs test

(defun output-hst-mob-stat (o-stream)
  (format o-stream
	  ;; m-time m-size hst-seart-time-- bb-subs-count bb-subs-time
	  "~,2:F~A~D~A~,2:F~A~D~A~,2:F"	  
	  m-time #\Tab
	  m-size #\Tab
	  (- hst-search-time bb-subs-time) #\Tab
	  bb-subs-count #\Tab
	  bb-subs-time #\Tab
	  )
  )


(defun output-all-justs-stat (o-stream &optional (hst-data-list *hst-node-labels*))
  (let ((min 400000)
	(mean 0)
	(max 0)
	(size 0)
	(all-just-intersect 0)
	(all-just-union 0)
	(ax-hash (make-hash-table :test 'eq))
	(marker (gensym))
	marker2
	)
    
    (setq min (setq max (setq mean (length (car hst-data-list)))))
    
    (dolist (ax (car hst-data-list))
      (setf (gethash ax ax-hash)
	marker))
    
    (dolist (just (cdr hst-data-list))

      (setq size (length just))
      (setq max (max max size))
      (setq min (min min size))
      (incf mean size)
      
      (setq marker2 (gensym))

      (dolist (ax just)
	
	(if (eq (gethash ax ax-hash) marker)	    
	    (setf (gethash ax ax-hash)
	      marker2)
	  (setf (gethash ax ax-hash)
	    t))
	)
      
      (setq marker marker2)
      )
	
    (setq mean (/ mean (length hst-data-list)))
    
    
    (loop as key being the hash-key of ax-hash do
	  (incf all-just-union)
	  (when (eq (gethash key ax-hash) marker)
	    (incf all-just-intersect))
	  )
	  
  
    (format o-stream
	    ;; number min mean max intersection union
	    "~D~A~D~A~,2:F~A~D~A~D~A~D" 
	    (length hst-data-list) #\Tab
	    min #\Tab
	    mean #\Tab
	    max #\Tab
	    all-just-intersect #\Tab
	    all-just-union #\Tab
	    )
    
    (finish-output o-stream)
    
    ))


;;_____________________________________________________________________________
;;_____________________________________________________________________________


(defun hst-mob-minas-experiments (ont supers-file 
			     &optional (stat-file (concatenate 'string		  
						    supers-file
						    ".mob-stat")))
  "massive experiments of the hst-mob-mina algorithm. does the following: (1) load ont (2) sample a number of concepts / take all their subsumers (3) extract all the minas using gob techniques (4) output statistics for those with more than one mina"
  
  
  ;; load the ontology if specified; otherwise, reuse
  (when ont
    (format t "~%Preparing the ontology...")
    (clear-tbox)
    (load-tbox ont)
    (format t "done!")
    )
  
  
  ;; read in the randomed samples concepts and their subsumers
  (with-open-file (i-stream supers-file
		   :direction :input)
    (with-open-file (o-stream stat-file
		     :direction :output 
		     :if-exists :supersede)

      ;; combination of there stat sets 1 + 5 + 6 + 6  (17 Tabs)
      (format o-stream "subsumption~At-module~As-module~At-hst-search~A#bb-subs-tests~At-bb-subs-tests~A#justs~Amin~Amean~Amax~A#common-axioms~A#all-axioms~A#diags~Amin~Amean~Amax~A#common-axioms~A#all-axioms~%"
	      #\Tab #\Tab #\Tab #\Tab
	      #\Tab #\Tab #\Tab #\Tab
	      #\Tab #\Tab #\Tab #\Tab
	      #\Tab #\Tab #\Tab #\Tab
	      #\Tab
	      )

      (let (c c-subsumers)
      
      (do ((statement
	    (read i-stream nil i-stream)
	    (read i-stream nil i-stream)))
	  ((eq statement i-stream) t)

	  (setq c (cadr statement))
	  
	  (when ;;t 
	      ;;(gethash (system-cname c) *sampled-c-table*)
	      (gethash c *sampled-c-table*)
	    
	    (setq c-subsumers (set-difference (caddr statement)
					      (list top c)))	  
	    
	    (dolist (d c-subsumers)	      
	      
	      (format t "-")
	    	      
	      (extract-all-minas-f c d)
	      
	      (when (cdr *hst-node-labels*)
		(format t "*")
		(format o-stream "~%(~S [= ~S)~A"
			c d #\Tab)
		(output-hst-mob-stat o-stream)
		(format o-stream "~A" #\Tab)
		(output-all-justs-stat o-stream)
		(format o-stream "~A" #\Tab)
		(output-all-justs-stat o-stream *hst-path-labels*)				
		
		))
	    )))
      ))
  )