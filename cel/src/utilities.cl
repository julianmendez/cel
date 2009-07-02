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


(defconstant tbox-paths '("../tboxes/"
			  "~/ontologies/test-suite/"
			  "~/ontologies/tboxes/"
			  "~/ontologies/go/"
			  "~/ontologies/snomed/"
			  "~/ontologies/galen/"))

(defvar *verbosity* 2)
;;_____________________________________________________________________________

(defun version ()
  "Display the version of the CEL reasoner"
  (format t "~A" *cel-version*))

(defun build (&key (internal-reference-build-id 6)
		   (internal-reference-build-date "January 15, 2008"))
  "Display the build of the CEL reasoner"
  (format t "CEL ~A; Build #~A on ~A"
	  *cel-version*
	  internal-reference-build-id
	  internal-reference-build-date))

(defun greeting-screen ()
  (format t *greeting-text*))
;;_____________________________________________________________________________

(defun display-command-line-help ()
  (format t *command-line-help-text*))
;;_____________________________________________________________________________

(defun el+ (&key role-inclusion range-restriction)
  "Some words on the underlying description logics EL+"
  (cond
   ((and role-inclusion range-restriction)
    (format t "~%CEL does not allow to use both range restrictions and complex role inclusion at the same time in the same ontology. If an attempt to use both is made, CEL will signal an error and the ontology will be illegal. However, specific form of role inclusions can be used in the presence of range restrictions. See (el+ :range-restriction t) for more.~%"))
   (role-inclusion
    (format t *el+ri*))
   (range-restriction
    (format t *el+rr*))
   (t
    (format t *el+*))
   ))

(defun benchmark (&optional (file-name nil)
		  &key (ontologies '("go.tbox"
				     "galen.cel"
				     "snomed-cdef.cel"
				     "snomed.cel"))
		       (mode 2))
  (if file-name
      (with-open-file (output-stream file-name		   
		       :direction :output 
		       :if-exists :supersede)
	(benchmark-f output-stream ontologies mode))
    (benchmark-f t ontologies mode)))

(defun benchmark-f (output-stream ontologies mode)
  (dolist (ont ontologies)
    (start ont
	   :mode mode
	   :verbosity nil
	   :profiling nil)
    (format output-stream
	    "~%(cel-benchmark ~S
  :classification-mode ~D
  :total-runtime ~:F
  :runtime-string ~S
  :is-tbox-consistent? ~A
  :no-pcdef-axioms ~D
  :no-cdefs-axioms ~D
  :no-gci-axioms ~D
  :no-ri-axioms ~D)"			    
	    (ont-uri)
	    mode
	    (+ (ont-preprocessing-time)
	       (ont-classification-time))
	    (string-trim '(#\Space) 
			 (milisec-to-string (+ (ont-preprocessing-time)
					       (ont-classification-time))))
	    (ont-tbox-consistent?)
	    (ont-n-pcdefs)
	    (ont-n-cdefs)
	    (ont-n-gcis)
	    (ont-n-ris))    
    (force-output output-stream)
    (msg "Classification of ~A completed." ont)))
  
      

(defun start (file-name &key (verbosity 2)
			     (profiling nil)
			     (uri default))
  "to clear, load, preprocess and finally classify the input ontology, all in one function."

;;;":verbosity - the level of information printed out on the screen. 
;;;             0 = nothing, 
;;;             >=1 = error messages, 
;;;             >=2 = normal messages, 
;;;             >=10 = unimportant and often annoying messages. 
;;;             t will be regarded as infinite level, and others are as 0.
;;;:profiling - mode of profiling. 
;;;             0 or nil = mode 0, ie. no profiling
;;;             1 or t = mode 1, ie. profiling on screen
;;;             2 = mode 2, ie. profiling on file using default file name
;;;             string velue = mode 2 with user-specified file name"
  
  (setq *verbosity*
    (cond
     ((eq verbosity t) 100)     
     ((numberp verbosity) verbosity)
     (t 0)))
  (case profiling 
    ((0 nil)
     (setq *profiling-mode* 0))
    ((1 t)
     (setq *profiling-mode* 1))
    ((2)
     (setq *profiling-mode* 2)
     (setq *profile-file-name* nil))
    (t
     (setq *profiling-mode* 2)
     (setq *profile-file-name* profiling)))	
  
  (when (stringp uri)
    (setq uri (intern uri)))  
  (case uri
    ;; when the default uri is used
    ((default |urn:cel-ontology:default|)
     (clear-default-cel-ontology)
     (restore-cel-ontology default))
    ;; when a new ontology is requested with system designated uri
    ((t)
     (create-tbox-f (gen-uri :from-file file-name)))
    ;; when a uri is given explicitly
    (t
     (create-tbox-f uri)))
  
  (msg "The ontology's URI is \"~A\"."
       (ont-uri))
  ;; Preprocessing......
  (msg "Loading and preprocessing the ontology from file \"~a\"..." 
       file-name)
    
  (unless (load-tbox-f file-name)
    (return-from start))
  
  (msg "...done in~a." (milisec-to-string (ont-preprocessing-time)))
    
  ;; Classifying......
  (msg "Classifying the ontology...")
    
  (classify-tbox-f)
  
  (msg "...done in~A.~%" (milisec-to-string (ont-classification-time)))
  
  (when (eq *verbosity* 3)
    (print *ontology*))
  t)
;;_____________________________________________________________________________

(defmacro help (&optional command)
  (if (and command
	   (member command *cel-keywords*))
      (help-command command)
    (help-text)))
;;_____________________________________________________________________________

(defun help-text ()
  (format t "~%CEL is an interactive, file-based Description Logic reasoner for EL+ with DIG interface support. The underpinning logic EL+ contains a selected set of essentially useful ontology constructors, such as, general concept inclusions, complex role inclusions, domain and range restrictions, disjointness assertions, and a few more. The following are keywords and (operational and assertional) commands used to interact with the CEL system:")
  (dolist (item *cel-keywords*)
    (format t "~%    ~a" item))
  (format t "~%Use (help <command>) for a brief documentation! For more complete explanations and examples, pleae consult the CEL user manual.")
  (format t "~%If things do not work properly as they should or if you have suggestions or any questions, feel free to get in touch with us (meng@tcs.inf.tu-dresden.de).")
  )
;;_____________________________________________________________________________

(defun help-command (command)
  (cond
   ((documentation command 'function)
    (format t (documentation command 'function))
    t)
   ((documentation command 'variable)
    (format t (documentation command 'variable))
    t)
   (t
    (err "Sorry, no documentation is given")
    nil)
   ))
;;_____________________________________________________________________________

(defun print-hash (hash-table &optional n (i 0))
  "to print out to the console key-value pairs in the hash-table, with n the max number of entr
ies printed"

  (format t "~%~a" hash-table)
  (loop as key being the hash-key of hash-table do
        (when (and n (>= i n))
          (return-from print-hash))
        (format t "~%key: ~S value: ~S" key (gethash key hash-table))
        (incf i))
  )


(defun output-hash (hash-table o-file)  
  (with-open-file (o-stream o-file
		   :direction :output 
		   :if-exists :supersede)
    (loop as key being the hash-key of hash-table do
	  (format o-stream  "(key-value ~S ~S)~%" key (gethash key hash-table)))
    ))

(defun input-hash (i-file &optional (hash-table (make-hash-table :test 'eql)))
  (with-open-file (i-stream i-file 
		   :direction :input
		   :if-does-not-exist nil)
    (do ((item
	  (read i-stream nil i-stream)
	  (read i-stream nil i-stream)))
	((eq item i-stream) t)
      (setf (gethash (cadr item) hash-table)
	(caddr item))
      ))
  hash-table)
    
    
(defun print-hash-- (hash-table)
  "to print out to the console key-value pairs in the hash-table"
  (format t "~%~a" hash-table)
  (maphash #'(lambda (key value) (format t "~%key: ~a value: ~a" key value))
           hash-table))
;;_____________________________________________________________________________

(defun msg (string &rest values)
  "to print out a message to the console"
  (when (>= *verbosity* 2)
    (apply #'format t                       
	   (concatenate 'string "~%;; " string)
	   values)))
;;_____________________________________________________________________________

(defun err (string &rest values)
  "to print out an error message to the console"
  (when (>= *verbosity* 1)
    (apply #'format t 
	   (concatenate 'string "~%;;;; " string)
	   values)))
;;_____________________________________________________________________________

(defmacro verbose (v)
  "when *verbosity* level is 10 or higher, to flush a verbosity out to the console screen, otherwise, do nothing"
  `(when (>= *verbosity* 10)
     (princ ,v))
  t)
;;_____________________________________________________________________________

(defun search-tbox-file (file-name)
  "to search for and return the full path of file-name. the folders and orders are specified in the list tbox-paths."
  (if (probe-file file-name)
      (return-from search-tbox-file file-name)
    (dolist (path tbox-paths nil)
      (let ((fn (concatenate 'string path file-name)))
	(if (probe-file fn)
	    (return-from search-tbox-file fn))))))
;;_____________________________________________________________________________



;;_____________________________________________________________________________
;;_____________________________________________________________________________
;; non approval code

(defun reverse-hash (hash-table)
  "assuming that hash-table contains only atomic keys and value elements"
  (let ((reverse-hash-table (make-hash-table)))
    (declare (special reverse-hash-table))
    (maphash #'(lambda (key value)
		 (declare (special reverse-hash-table))
		 (dolist (v value)
		   (pushnew key 
			    (gethash v reverse-hash-table))))
	     hash-table)
    reverse-hash-table))

(defun out-hierarchy (out-file hash-table &optional (depth 5))
  "hash-table must have atomic keys and lists of atomic elements as values"
  (with-open-file (out out-file
                   :direction :output
                   :if-exists :supersede)
    (out-hierarchy-r out hash-table 'top depth depth)))

(defun out-hierarchy-r (out-stream hash-table root max depth)
  (if (= depth 1)
      (format out-stream
	      "~%~a(~a ~a)~%"
	      (make-string max :initial-element #\Space)
	      root
	      (gethash root hash-table))
    (progn
      (format out-stream
	      "~%~a(~a "
	      (make-string (- max depth) :initial-element #\Space)
	      root)
      (dolist (child (gethash root hash-table))
	(out-hierarchy-r out-stream hash-table child max (- depth 1)))
      (format out-stream
	      ")"))))
    


(defun out-hash-table (out-file hash-table &optional (keyword 'key-value))
  (with-open-file (out out-file
                   :direction :output
                   :if-exists :supersede)
    (declare (special out))

    (maphash #'(lambda (key value)
                 (declare (special out))

                 (format out
                         "~% ~a, ~a"
                         ;;keyword
			 ;;#\Tab
                         key
			 ;;#\Tab
                         value))
             hash-table)))

(defun out-gci-tbox (out-file gci-tbox &optional (keyword 'implies))
  (with-open-file (out out-file
                   :direction :output
                   :if-exists :supersede)
    (declare (special out))

    (maphash #'(lambda (key value)
                 (declare (special out))

                 (dolist (v value)
                   (if (and (listp v)
                            (not (listp (cdr v))))
                       ;; dotted pair
                       (format out "~%(~a ~a ~a)"
                               keyword
                               (list 'and key (car v))
                               (cdr v))
                     ;; simply left & right components
                     (format out  "~%(~a ~a ~a)"
                             keyword
                             key
                             v))))
             gci-tbox)))

(defun milisec-to-string (sec)
  (let ((m 0)
	(h 0))
    (when (= sec 0)
      (return-from milisec-to-string " less than 10 miliseconds"))
    (when (>= sec 60)
      (multiple-value-setq (m sec) (floor sec 60))
      (when (>= m 60)
	(multiple-value-setq (h m) (floor m 60))))
    (concatenate 'string
      (when (> h 0) (format nil " ~:D hour~:P" h))
      (when (> m 0) (format nil " ~:D minute~:P" m))
      (when (> sec 0) (format nil " ~:F second~:P" sec)))))


;;_____________________________________________________________________________


(defun count-frequency (data-file freq-file 
			&optional (freq-table (make-hash-table :test 'equal)))
  (with-open-file (d-stream data-file
		   :direction :input
		   :if-does-not-exist nil)
    (with-open-file (f-stream freq-file
		     :direction :output
		     :if-exists :supersede)
      
      (do ((line
	    (read-line d-stream nil d-stream)
	    (read-line d-stream nil d-stream)))
	  ((eq line d-stream) t)
	    
	(incf (gethash line freq-table 0))
	)
      
      (format f-stream "item~Afrequency" #\Tab)
      (loop as key being the hash-key of freq-table do
	    
	    (format f-stream "~%~A~A~D" 
		    key 
		    #\Tab
		    (gethash key freq-table)))
      )))



(defun compare-taxonomies (tax-file &key (ignore-bottom t)
					  tax-file2)
  (when tax-file2
    (msg "Under development!")
    (return-from compare-taxonomies))
  
  (with-open-file (input-stream tax-file 
		   :direction :input
		   :if-does-not-exist nil)    
    (when (null input-stream)
      (err "File (~a) does not exist!" tax-file)
      (return-from compare-taxonomies nil))
    
    (do ((statement 
          (read input-stream nil input-stream)
          (read input-stream nil input-stream)))
        ((eq statement input-stream) t)
      
      (unless (and ignore-bottom
		   (eq 'bottom (cadr statement)))
	(handler-case (unless (eval statement)
			(return-from compare-taxonomies statement))
	  (error ()
	    (err "Unrecognized axiom: ~S" statement)))
	)
      ))
  t)

(defmacro taxonomy (c &key parents children equivalent)
  `(taxonomy-f ',c ',parents ',children ',equivalent))

(defun taxonomy-f (c &optional parents children equivalent)
  
  (when equivalent
    ;; check if c and equivalent are equivalent in CEL
    (if (q-concept-equivalent-f c equivalent)
	(return-from taxonomy-f t)
      (return-from taxonomy-f nil)
      ))
  
  (let* ((s-c (system-cname (synonym-of c)))
	 c-parents c-children 
	 missing-parents surplus-parents
	 missing-children surplus-children)

    (unless s-c
      (msg "CEL does not recognize the concept name ~S."
	   c)
      (return-from taxonomy-f nil))
    
    (request-hasse)
    
    (setq c-parents (user-cnames (c-parents s-c)))
    (setq c-children (user-cnames (c-children s-c)))
	 
    (setq missing-parents (set-difference parents c-parents :test 'eql))
    (when missing-parents
      (msg "CEL's missing parents for ~S are ~A"
	   c (format-diff-set missing-parents))
      ;;(return-from taxonomy-f nil)
      )
    
    (setq surplus-parents (set-difference c-parents parents :test 'eql))
    (when surplus-parents
      (msg "CEL's surplus parents for ~S are ~A"
	   c (format-diff-set surplus-parents))
      ;;(return-from taxonomy-f nil)
      )
    
    (setq missing-children (set-difference children c-children :test 'eql))
    (when missing-children
      (msg "CEL's missing children for ~S are ~A"
	   c (format-diff-set missing-children))
      ;;(return-from taxonomy-f nil)
      )
    
    (setq surplus-children (set-difference c-children children :test 'eql))
    (when surplus-children
      (msg "CEL's surplus children for ~S are ~A"
	   c (format-diff-set surplus-children))
      ;;(return-from taxonomy-f nil)
      ) 
    
    t))


(defun format-diff-set (set &optional (max-length 10))
  (if (> (length set) max-length)
      (format nil "totally ~A items" (length set))
    (format nil "~S" set)))



(setq *mrd* (make-hash-table :test 'eq))
(defmacro c-max-rdepth (c)
  `(gethash ,c *mrd*))


(setq *ucs* (make-hash-table :test 'eq))
(defmacro c-unfolded-size (c)
  `(gethash ,c *ucs*))


(defun max-unfolded-concept-size (&optional (max 0) largest-c)
  (loop for c from (car (ods-system-cname-range)) to (cdr (ods-system-cname-range)) do
	(let ((size-of-c (unfolded-size-of c)))
	  (when (> size-of-c max)
	    (setq largest-c c)
	    (setq max size-of-c))))
  (print largest-c)
  max)

(defun unfolded-size-of (c &optional path)
  "Calulate the size of c when all defined cnames are unfolded by their definitions; path is a list of cnames along the definitional orders"
  (when (c-unfolded-size c)
    (return-from unfolded-size-of (c-unfolded-size c)))
  
  (when (member c path :test 'eq)
    (msg "Cyclic dependency along the path: ~S"
	 (cons c path))
    (return-from unfolded-size-of :cyclic))
  
  (push c path)
  
  (let ((size-of-c 0))    
    ;; retrieve all subconcepts occurring in c's definition
    ;; if none, it's primitive
;;;    (unless (o-nexts c)
;;;      (return-from unfolded-size-of
;;;	(setf (c-unfolded-size c) 1)))
    
    (dolist (x (o-nexts c))
      (cond
     ;; existential
       ((and (listp x)
	     (not (listp (car x))))
	(let* (;; (r (car x))
	       (b (cdr x))
	       (size-of-b (unfolded-size-of b path)))
	  (when (eq size-of-b :cyclic)	    
	    (return-from unfolded-size-of
	      (setf (c-unfolded-size c) 0)))
	  (incf size-of-c (+ size-of-b 1))	  
	  ))
       ;; atomic
       ((not (listp x))
	(let ((size-of-x (unfolded-size-of x path)))
	  (when (eq size-of-x :cyclic)	    
	    (return-from unfolded-size-of
	      (setf (c-unfolded-size c) 0)))
	  (incf size-of-c size-of-x)
	  ))
       ))
        
    (setf (c-unfolded-size c) (if (zerop size-of-c)
				  1
				size-of-c))
    
    ))


(defun max-role-depth (&optional (max 0))
  (loop for c from 0 to (cdr (ods-system-cname-range)) do
	(let ((max-from-c (max-role-depth-from c)))
	  (when (> max-from-c max)
	    (setq max max-from-c))))
  max)


(defun max-role-depth-from (c &optional path)
  "Calculate the max role depth starting from the concept c; path is a list of cnames along the role successors relationship (existentials)"
  (when (c-max-rdepth c)
    (return-from max-role-depth-from (c-max-rdepth c)))    
  
  (when (member c path :test 'eq)
    (msg "Cyclic dependency along the path: ~S"
	 (cons c path))
    (return-from max-role-depth-from :cyclic))
  
  (push c path)

  ;; retrieve all r-successors d of c for all r in the ontology
  (let (successors
	(succ-max 0))
    (loop for r from 0 to (- (length (ods-rname-array)) 1) do ;; for each system rname	
	  (setq successors (union successors
				  (r-succs c r)
				  :test 'eq))
	  )
    ;; if c has no successors, it has 0 role depth
    (unless successors
      (return-from max-role-depth-from
	(setf (c-max-rdepth c) 0)))

    ;; otherwise, it has depth 1 + max of its successors' depths
    (dolist (x successors)
      (let ((x-max (max-role-depth-from x path)))
	(when (eq x-max :cyclic)
	  (setf (c-max-rdepth c) 0)
	  (return-from max-role-depth-from 0))
	(when (> x-max succ-max)
	  (setq succ-max x-max))))
    
    (setf (c-max-rdepth c) (+ succ-max 1))
    
    
;;;    (setf (c-max-rdepth c)
;;;      (+ (apply 'max (cons 0 (mapcar 'max-role-depth-from successors)))
;;;	 1))
    ))




(defun archive-internal-s-structures (file-name)
  )








(defun rewrite-krss-ontology (in-file out-file)

  (let ((pdef-table (make-hash-table :test 'eq))
	(fdef-table (make-hash-table :test 'eq))
	type lhs rhs
	)

    (with-open-file (in in-file
		     :direction :input
		     :if-does-not-exist nil)
      (with-open-file (out out-file
		       :direction :output
		       :if-exists :supersede)
		
	(do ((axiom 
	      (read in nil in)
	      (read in nil in)))
	    ((eq axiom in) t)

	  (setq type (car axiom))
	  (setq lhs (cadr axiom))
	  (setq rhs (caddr axiom))
	  
	  (cond
	   ((or (eq type 'define-primitive-concept)
		(and (eq type 'implies)
		     (not (listp lhs))))
	    (push rhs
		  (gethash lhs pdef-table)))
	   
	    
	   ((or (eq type 'define-concept)
		(and (eq type 'equivalent)
		     (not (listp lhs))))
	    (push rhs
		  (gethash lhs fdef-table)))
	   
	   (t
	    (format out "~%~S" axiom))
	   )
	  
	  )
	
	(loop as key being the hash-key of fdef-table do
	      
	      (setq rhs (gethash key fdef-table))	      
	      ;; output the first one as fdef
	      (format out "~%(DEFINE-CONCEPT ~S ~S)"
		      key
		      (car rhs))
	      ;; next is formulated as equivalent
	      (dolist (next (cdr rhs))
		(format out "~%(EQUIVALENT ~S ~S)"
			key
			next))
	      )
	
	(loop as key being the hash-key of pdef-table do
	
	      (setq rhs (gethash key pdef-table))
	      
	      ;; check if rhs contain top
	      (when (cdr rhs)
		(setq rhs (set-difference rhs '(top) :test 'eql))
		)
	      
	      (cond
	       ;; if fdef already exists, is is GCI
	       ((gethash key fdef-table)
		(format out "~%(IMPLIES ~S ~S)"
			key
			(if (cdr rhs)
			    (and-expansion (cons 'and rhs))
			  (car rhs))
			))
	       
	       ;; otherwise, a single pdef
	       (t
		(format out "~%(DEFINE-PRIMITIVE-CONCEPT ~S ~S)"
			key
			(if (cdr rhs)
			    (and-expansion (cons 'and rhs))
			  (car rhs))
			))
	       
	       ))
	))
    ))
	       