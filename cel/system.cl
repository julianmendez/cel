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


;; temporary global variables to collect cpu time results
(defvar *t0* 0)
(defvar *t* 0)


(defconstant *system-version* 9)

(defconstant *max-cnames* 536000000
  "About a half billion limited by the max dimension of an array")
(defconstant *max-rnames* 1000000)
;;_____________________________________________________________________________

;; the notion of tbox here is quite weird since we mix name space of individuals and concepts. in other words, we can't really distinguish internally between tbox and abox. internal individuals are just concepts with flag (c-individual?) set to true. we will have to use this when output results to the user.
;; some notions, however, need separation, such as, abox-consistency. this implies tbox-consistency but the vice versa.
;; DECISION: change the name of this struct and consequently all its accessing functions, macrots to ontology (ont, for short). this naturally comprises both tbox and abox
(defstruct (ontology
	    (:print-function 
	     (lambda (ont stream k)
	       (declare (ignore k))
	       (format stream "Ontology[uri=\"~A\"; state=~A]"
		       (ontology-uri ont)
		       (symbol-name (ontology-state ont))
		       ))))
  "Data structure for storing tbox properties"
  (uri (error "ontology URI must be specified!")
       :read-only t) ;; set to the interned string of ontology URI
  (state :cleared
	 :type symbol) ;; cleared 0, prepared 10, classified 20, taxonomized 30, illegal -1 (which can be resolved only by clear-tbox)
  
  
  (ttbox-active? nil) ;; set to t when a temporary TBox is in use and active
  (tbox-consistent? t) ;; set to nil if tbox is contradictory
  (abox-consistent? t) ;; set to nil icancelf abox is inconsistent  
  (tbox-cyclic? nil) ;; set to t if tbox,abox contains cycle
  (tbox-general? nil) ;; set to t if tbox uses GCIs
  (tbox-primitive? t) ;; set to nil if tbox has a full definition or GCI
  (has-mult-def? nil) ;; set to t if tbox has multiple definitions
  (has-cri? nil) ;; set to t if tbox has a complex role incluison, not expressible as reflexive or transitive role.
  
  (individuals nil) ;; the list of all individuals (disguised as concepts)  
  (reflexive-roles nil) ;; the list of all reflexive roles, incl. implicit ones
  (transitive-roles nil) ;; the list of all transitive roles
  
  (rr-no-ri nil) ;; set to t if tbox has range restrictions but no complex role inclusions -- still tractable
  (ri-no-rr nil) ;; set to t if tbox has complex role inclusion but no range restrictions -- still tractable
  ;; if both hold, it's no longer tractable, and thus tbox must be rejected
  (roles-w-range) ;; the list of all roles with declared range
  (roles-w-domain) ;; the list of all roles with declared domain
  ;; note that their subroles will have restriction as well -- must be taken into account
  (n-pcdefs 0) ;; #primitive concept definitions
  (n-cdefs 0) ;; #full concept definitions
  (n-gcis 0) ;; #general concept inclusions
  (n-ris 0) ;; #role inclusion axioms (incl. all kind of role axoms)
  (n-cas 0) ;; #concept assertions (involving individuals)
  (n-ras 0) ;; #role assertions (involving individuals)
  (preprocessing-time 0)
  (phierarchy-time 0)
  (classification-time 0)
  
  (ds (make-internal-data-structure))
  (ds-2 (make-internal-data-structure-2))
  (ds-m (make-module-data-structure))
  ) ;; end of defstruct
;;_____________________________________________________________________________

(defstruct (concept ;; system cname
            (:print-function (lambda (s-cn stream k)
                               (declare (ignore k))
                               (format  stream "c[~A]"
                                       (concept-user-name s-cn)))))
  "Data structure for a system cname"
  (parents nil)
  (children nil)
  (status nil) ;; unkown (nil), primitive, defined, phierarchy
  (individual? nil)
  (definition nil)
  (equivalent nil)
  (marked nil)
  (visited nil)
  (marked-as-candidate nil)
  (classified nil)
  (told-subsumers nil)
  (told-subsumees nil)
  (user-name nil)
  (equivalent-user-names nil))
;;_____________________________________________________________________________

(defstruct (role ;; system rname
            (:print-function (lambda (s-rn stream k)
                               (declare (ignore k))
                               (format stream "r[~A]" 
				       (role-user-name s-rn)))))
  "Data structure for a system rname"
  (parents nil)
  (ancestors nil)
  (descendants nil)
  (transitive nil)
  (reflexive nil)
  (domain nil)
  (range nil)
  (range-cname nil)
  (inclusions1 nil) ;; the role r is in r o s => t, in the list is (s . t)
  (inclusions2 nil) ;; the role r is in s o r => t, in the list is (s . t)
  (user-name nil))
;;_____________________________________________________________________________


(defstruct (queue-struct
	    (:print-function (lambda (q stream k)
			       (declare (ignore k))
			       (format stream "q[P=~D; ~D active queues]"
				       (queue-struct-priority q)
				       (q-total-active-queues q)))))	    
  "Data structure for queues and non-empty queue handler"
  ;;(active-flag (make-hash-table :test 'eq))
  (priority nil)
  (active-list nil)
  (active-list-tail nil)
  (entry-table (make-hash-table :test 'eq)))
;;_____________________________________________________________________________

(defstruct (internal-data-structure)
  "Struct wrapping all ontology-own internal variables such as *cname-table*, *o*, *s-table* etc. All those variables are now OBSOLETE and accessing macros have to be modified accordingly by referring to the variable of this type instead."
  ;;------------------------------------------------
  (cname-table ;; Mapping of user cnames to system cnames (fixnums)
   (make-hash-table :test 'equal)
   :type hash-table)
  (cname-array ;; Mapping of system cnames (fixnums) to user cnames
   (make-array 1000
	       :element-type 'concept
	       :adjustable t
	       :fill-pointer 0)
   :type array)
  ;;------------------------------------------------
  (system-cname-range ;; Pair of the fixnum range used as cnames: bottom (0), top (1), user cnames (+), and new system cnames (-)
   (cons 0 1)
   :type cons)
  (backup-system-cname-range ;; Pair of the fixnum range used as cnames: bottom (0), top (1), user cnames (+), and new system cnames (-)
   (cons 0 1)
   :type cons)
  (backup-rname-array-fill-pointer ;; The last index of permanent rname (those in the committed TBox)
   nil)
  ;;------------------------------------------------
  (rname-table ;; Mapping of user rnames to system rnames (fixnums)
   (make-hash-table :test 'equal)
   :type hash-table)
  (rname-array ;; Mapping of system rnames (fixnums) to user rnames
   (make-array 100
	       :element-type 'role
	       :adjustable t
	       :fill-pointer 0)
   :type array)
  ;;------------------------------------------------
  (o ;; Mapping of concepts to their consequeces built from the normalized tbox
   (make-hash-table :test 'equal) 
   :type hash-table)
  ;; This is a representation of GCI-normalized general tbox using a hashtable:
  ;; Key:   a concept, occuring on the left-hand side, of the form
  ;;        A or (SOME R A)
  ;; Value: a list of algorithm-optimized GCIs in the following forms:
  ;;        if Key is A: (SOME R B), 
  ;;                     B, 
  ;;                     (B . C) : this is equivalent to (DEFGCI (AND A B) C)
  ;;        if Key is (SOME R A): B.
  ;; Note that the key-testing function is EQUAL
  ;;------------------------------------------------
  (queue
   (make-queue-struct :priority 1)
   :type queue-struct)
  (s-table ;; Mapping of system cnames to sets of inferred subsumers
   (make-hash-table :test 'eq)
   :type hash-table)  
  (r-pred ;; Mapping of the role relation R, predecessor part
   (make-hash-table :test 'equal)
   :type hash-table)
  (r-succ ;; Mapping of the role relation R, successor part
   (make-hash-table :test 'equal)
   :type hash-table)
  ;;------------------------------------------------
  (synonym-table ;; Mapping of cnames to their synonyms (union-find structure); the representatives have value as the list of all synonyms
   (make-hash-table :test 'equal)
   :type hash-table)
  (synonym-list ;; List of synonymic cnames that have a representative
   nil
   :type cons)
  ;;------------------------------------------------
  (gci-list1 ;; List of input GCIs which must be normalized in NF1
   nil
   :type cons)
  (gci-list2 ;; List of GCIs in NF1 which must be normalized further in NF2
   nil
   :type cons)
  (subconcept-table ;; Mapping of subconcepts occurring in the tbox to new cnames with flags (fixnums) indicating the location of them: left (-1) r (1) or b (0)
   (make-hash-table :test 'equal)
   :type hash-table)
  (defined-cname-list ;; List of (cn status definition) to be added to the system in normalize (new version of pass-2)
   nil
   :type cons)
  (phierarchy-cname-list ;; List of cnames in primitive hierarchy (completly defined)
   nil
   :type cons)
  (activation-flag ;; Mapping of cnames to t (nil) if they are (not) activated
   (make-hash-table :test 'eq)
   :type hash-table)
  (cached-subsumer-table ;; Mapping of unclassified cnames to their cached subsumers--OBSOLETE
   (make-hash-table :test 'eq))
  (cached-subsumee-table ;; Mapping of unclassified cnames to their cached subsumees--OBSOLETE
   (make-hash-table :test 'eq))
  ;;------------------------------------------------
  (s-counter 0)
  (total-s-counter 0)
  (total-subs-tests 0)
  (total-positive-subs-tests 0)
  (total-cache-accesses 0)
  (total-cache-hits 0)
  (total-classified-cnames 0)
    
  ) ;; end of defstruct
;;_____________________________________________________________________________

(defstruct (internal-data-structure-2)
  (o ;; Mapping of concepts to their consequeces built from the normalized tbox
   (make-hash-table :test 'equal) 
   :type hash-table)
  (s-table ;; Mapping of system cnames to sets of inferred subsumers
   (make-hash-table :test 'eq)
   :type hash-table)  
  (r-pred ;; Mapping of the role relation R, predecessor part
   (make-hash-table :test 'equal)
   :type hash-table)
  (r-succ ;; Mapping of the role relation R, successor part
   (make-hash-table :test 'equal)
   :type hash-table)
  (subconcept-table ;; Mapping of subconcepts occurring in the tbox to new cnames with flags (fixnums) indicating the location of them: left (-1) r (1) or b (0)
   (make-hash-table :test 'equal)
   :type hash-table)
  (activation-flag ;; Mapping of cnames to t (nil) if they are (not) activated
   (make-hash-table :test 'eq)
   :type hash-table)
  ;;------------------------------------------------  
  (total-s-counter 0)
  )

(defconstant default '|urn:cel-ontology:default|
  "Reference URI to the default CEL ontology")

(defvar *ontology* (make-ontology :uri default)
  "The current pointer to active ontology")
(defvar *backup-ontology* nil)

(defvar *internal-data-structure* nil
  "Pointer to (ontology-data-structure) slot")
(defvar *internal-data-structure-2* nil
  "Pointer to (ontology-data-structure-2) slot")



(defvar *s-marker* (make-hash-table :test 'eq))
(defmacro make-s-marker ()
  `(setq *s-marker* (make-hash-table :test 'eq)))
(defmacro s-marked (s-cn)
  `(gethash ,s-cn *s-marker*))

;;; to remove
;;;(defun s-mark-all-nodes (x &optional (marked (gensym)))
;;;  (unless (eq (s-marked x) marked)
;;;    (setf (s-marked x) marked)
;;;    (dolist (y (s-nodes x))
;;;      (s-mark-all-nodes y marked))))
(defun s-mark-all-labels (x &optional (marked (gensym)))
  (unless (eq (s-marked x) marked)
    (setf (s-marked x) marked)
    (dolist (y (s-labels x))
      (s-mark-all-labels y marked))))



(defmacro is-default-ontology? ()
  "Check if the current ontology is the default ontology"
  `(eq (ont-uri) default))

(defconstant *top* 1)
(defconstant *top-concept-symbol* 'top)
(defmacro make-top-concept-struct ()
  `(make-concept :user-name 'top
		 :status 'phierarchy
		 :classified t))
(defconstant *bot* 0)
(defconstant *bot-concept-symbol* 'bottom)
(defmacro make-bot-concept-struct ()
  `(make-concept :user-name 'bottom
		 :status 'phierarchy
		 :classified t))


(defparameter *cname-array-enlarge-factor* 20000
  "The array *cname-array* is enlarged by this factor each time")

(defparameter *rname-array-enlarge-factor* 100
  "The array *rname-array* is enlarged by this factor each time")

(defparameter *profiling-mode* 0
  "Mode of profiling: no profile (0), on screen (1), onto file (2)")

(defvar *profile-file-name* "./cel.log"
  "File name for profiling. If not specified but the mode is set to 2, *tbox-file-name* with an extension 'profile' will be used")

(defvar *profile-out-stream* nil
  "Stream of opened prifiling file")


(defmacro ont-state? (state)
  "to check if the ontology is in state or one of the states given"
  `(cond
    ((listp ',state)
     (if (member (ont-state) ',state
		 :test 'eq)
	 t
       nil))
    (t
     (eq (ont-state) ',state))))

;;_____________________________________________________________________________
;;
;; Macro definitions for accessing global variables 
;;_____________________________________________________________________________

;; Macros to access fields in *ontology* object
(defmacro ont-uri ()
  `(ontology-uri *ontology*))
(defmacro ont-state ()
  `(ontology-state *ontology*))

(defmacro ont-ttbox-active? ()
  `(ontology-ttbox-active? *ontology*))

(defmacro ont-tbox-consistent? ()
  `(ontology-tbox-consistent? *ontology*))
(defmacro ont-abox-consistent? ()
  `(ontology-abox-consistent? *ontology*))
(defmacro ont-tbox-cyclic? ()
  `(ontology-tbox-cyclic? *ontology*))
(defmacro ont-tbox-general? ()
  `(ontology-tbox-general? *ontology*))
(defmacro ont-tbox-primitive? ()
  `(ontology-tbox-primitive? *ontology*))
(defmacro ont-has-mult-def? ()
  `(ontology-has-mult-def? *ontology*))
(defmacro ont-has-cri? ()
  `(ontology-has-cri? *ontology*))

(defmacro ont-individuals ()
  `(ontology-individuals *ontology*))
(defmacro ont-transitive-roles ()
  `(ontology-transitive-roles *ontology*))
(defmacro ont-reflexive-roles ()
  `(ontology-reflexive-roles *ontology*))
(defmacro ont-rr-no-ri ()
  `(ontology-rr-no-ri *ontology*))
(defmacro ont-ri-no-rr ()
  `(ontology-ri-no-rr *ontology*))
(defmacro ont-roles-w-range ()
  `(ontology-roles-w-range *ontology*))
(defmacro ont-roles-w-domain ()
  `(ontology-roles-w-domain *ontology*))

(defmacro ont-n-pcdefs ()
  `(ontology-n-pcdefs *ontology*))
(defmacro ont-n-cdefs ()
  `(ontology-n-cdefs *ontology*))
(defmacro ont-n-gcis ()
  `(ontology-n-gcis *ontology*))
(defmacro ont-n-ris ()
  `(ontology-n-ris *ontology*))
(defmacro ont-n-cas ()
  `(ontology-n-cas *ontology*))
(defmacro ont-n-ras ()
  `(ontology-n-ras *ontology*))

(defmacro ont-preprocessing-time ()
  `(ontology-preprocessing-time *ontology*))
(defmacro ont-phierarchy-time ()
  `(ontology-phierarchy-time *ontology*))
(defmacro ont-classification-time ()
  `(ontology-classification-time *ontology*))

;;_____________________________________________________________________________

(defmacro ods-cname-table ()
  `(internal-data-structure-cname-table (ontology-ds *ontology*)))
(defmacro ods-cname-array ()
  `(internal-data-structure-cname-array (ontology-ds *ontology*)))

(defmacro ods-system-cname-range ()
  `(internal-data-structure-system-cname-range (ontology-ds *ontology*)))
(defmacro ods-backup-system-cname-range ()
  `(internal-data-structure-backup-system-cname-range
    (ontology-ds *ontology*)))
(defmacro ods-backup-rname-array-fill-pointer ()
  `(internal-data-structure-backup-rname-array-fill-pointer 
    (ontology-ds *ontology*)))


(defmacro ods-rname-table ()
  `(internal-data-structure-rname-table (ontology-ds *ontology*)))
(defmacro ods-rname-array ()
  `(internal-data-structure-rname-array (ontology-ds *ontology*)))

(defmacro ods-o ()
  `(internal-data-structure-o (ontology-ds *ontology*)))

(defmacro ods-queue ()
  `(internal-data-structure-queue (ontology-ds *ontology*)))
(defmacro ods-s-table ()
  `(internal-data-structure-s-table (ontology-ds *ontology*)))
(defmacro ods-r-pred ()
  `(internal-data-structure-r-pred (ontology-ds *ontology*)))
(defmacro ods-r-succ ()
  `(internal-data-structure-r-succ (ontology-ds *ontology*)))

(defmacro ods-synonym-table ()
  `(internal-data-structure-synonym-table (ontology-ds *ontology*)))
(defmacro ods-synonym-list ()
  `(internal-data-structure-synonym-list (ontology-ds *ontology*)))

(defmacro ods-gci-list1 ()
  `(internal-data-structure-gci-list1 (ontology-ds *ontology*)))
(defmacro ods-gci-list2 ()
  `(internal-data-structure-gci-list2 (ontology-ds *ontology*)))
(defmacro ods-subconcept-table ()
  `(internal-data-structure-subconcept-table (ontology-ds *ontology*)))
(defmacro ods-defined-cname-list ()
  `(internal-data-structure-defined-cname-list (ontology-ds *ontology*)))
(defmacro ods-phierarchy-cname-list ()
  `(internal-data-structure-phierarchy-cname-list (ontology-ds *ontology*)))
(defmacro ods-activation-flag ()
  `(internal-data-structure-activation-flag (ontology-ds *ontology*)))
(defmacro ods-cached-subsumer-table ()
  `(internal-data-structure-cached-subsumer-table (ontology-ds *ontology*)))
(defmacro ods-cached-subsumee-table ()
  `(internal-data-structure-cached-subsumee-table (ontology-ds *ontology*)))

(defmacro ods-s-counter ()
  `(internal-data-structure-s-counter (ontology-ds *ontology*)))
(defmacro ods-total-s-counter ()
  `(internal-data-structure-total-s-counter (ontology-ds *ontology*)))
(defmacro ods-total-subs-tests ()
  `(internal-data-structure-total-subs-tests (ontology-ds *ontology*)))
(defmacro ods-total-positive-subs-tests ()
  `(internal-data-structure-total-positive-subs-tests (ontology-ds *ontology*)))
(defmacro ods-total-cache-accesses ()
  `(internal-data-structure-total-cache-accesses (ontology-ds *ontology*)))
(defmacro ods-total-cache-hits ()
  `(internal-data-structure-total-cache-hits (ontology-ds *ontology*)))
(defmacro ods-total-classified-cnames ()
  `(internal-data-structure-total-classified-cnames (ontology-ds *ontology*)))
;;_____________________________________________________________________________

(defmacro ods2-o ()
  `(internal-data-structure-2-o (ontology-ds-2 *ontology*)))
(defmacro ods2-s-table ()
  `(internal-data-structure-2-s-table (ontology-ds-2 *ontology*)))
(defmacro ods2-r-pred ()
  `(internal-data-structure-2-r-pred (ontology-ds-2 *ontology*)))
(defmacro ods2-r-succ ()
  `(internal-data-structure-2-r-succ (ontology-ds-2 *ontology*)))
(defmacro ods2-subconcept-table ()
  `(internal-data-structure-2-subconcept-table 
    (ontology-ds-2 *ontology*)))
(defmacro ods2-activation-flag ()
  `(internal-data-structure-2-activation-flag 
    (ontology-ds-2 *ontology*)))
(defmacro ods2-total-s-counter ()
  `(internal-data-structure-2-total-s-counter (ontology-ds-2 *ontology*)))
;;_____________________________________________________________________________

(defmacro ods- ()
  `(internal-data-structure- (ontology-ds *ontology*)))

;;_____________________________________________________________________________


(defmacro system-cname (cn)
  `(gethash ,cn (ods-cname-table)))
(defmacro system-cnames (cn-list)
  `(mapcar #'(lambda (cn) (system-cname cn))
	   ,cn-list))

(defmacro user-cname (s-cn)
  `(concept-user-name (aref (ods-cname-array) ,s-cn)))
(defmacro user-cnames (s-cn-list)
  `(mapcar #'(lambda (s-cn) (user-cname s-cn))
	   ,s-cn-list))
(defun user-cname-safe (s-cn)
  (if (< s-cn 0)
      s-cn
    (user-cname s-cn)))
(defmacro user-cnames-safe (s-cn-list)
  `(mapcar #'(lambda (s-cn) (user-cname-safe s-cn))
	   ,s-cn-list))

(defmacro cname-struct (s-cn)
  "To access concept structure by system cname"
  `(aref (ods-cname-array) ,s-cn))
(defmacro system-cname-struct (cn)
  "To access concept structure by user cname"
  `(aref (ods-cname-array) (gethash ,cn (ods-cname-table))))

;; Macros to access fields in concept structure by system cname
(defmacro c-individual? (s-cn)
  `(concept-individual? (cname-struct ,s-cn)))
(defmacro c-parents (s-cn)
  `(concept-parents (cname-struct ,s-cn)))
(defmacro c-children (s-cn)
  `(concept-children (cname-struct ,s-cn)))
(defmacro c-equivalent (s-cn)
  `(concept-equivalent (cname-struct ,s-cn)))
(defmacro c-marked (s-cn)
  `(concept-marked (cname-struct ,s-cn)))
(defmacro c-marked-as-candidate (s-cn)
  `(concept-marked-as-candidate (cname-struct ,s-cn)))
(defmacro c-visited (s-cn)
  `(concept-visited (cname-struct ,s-cn)))
(defmacro c-status (s-cn)
  `(concept-status (cname-struct ,s-cn)))
(defmacro c-definition (s-cn)
  `(concept-definition (cname-struct ,s-cn)))
(defmacro c-classified (s-cn)
  `(concept-classified (cname-struct ,s-cn)))
(defmacro c-told-subsumers (s-cn)
  `(concept-told-subsumers (cname-struct ,s-cn)))
(defmacro c-told-subsumees (s-cn)
  `(concept-told-subsumees (cname-struct ,s-cn)))
(defmacro c-user-name (s-cn)
  `(concept-user-name (cname-struct ,s-cn)))
(defmacro c-equivalent-user-names (s-cn)
  `(concept-equivalent-user-names (cname-struct ,s-cn)))

(defmacro c-cached-subsumers (s-cn)
  `(gethash ,s-cn (ods-cached-subsumer-table)))
(defmacro c-cached-subsumees (s-cn)
  `(gethash ,s-cn (ods-cached-subsumee-table)))
;;_____________________________________________________________________________

(defmacro system-rname (rn)
  `(gethash ,rn (ods-rname-table)))
(defmacro system-rnames (rn-list)
  `(mapcar #'(lambda (rn) (system-rname rn))
	   ,rn-list))

(defmacro user-rname (s-rn)
  `(role-user-name (aref (ods-rname-array) ,s-rn)))
(defmacro user-rnames (s-rn-list)
  `(mapcar #'(lambda (s-rn) (user-cname s-rn))
	   ,s-rn-list))

(defmacro rname-struct (s-rn)
  `(aref (ods-rname-array) ,s-rn))
(defmacro system-rname-struct (rn)
  `(aref (ods-rname-array) (gethash ,rn (ods-rname-table))))

;; Macros to access fields in role structure by system rname
(defmacro r-parents (s-rn)
  `(role-parents (rname-struct ,s-rn)))
(defmacro r-ancestors (s-rn)
  `(role-ancestors (rname-struct ,s-rn)))
(defmacro r-descendants (s-rn)
  `(role-descendants (rname-struct ,s-rn)))
(defmacro r-reflexive (s-rn)
  `(role-reflexive (rname-struct ,s-rn)))
(defmacro r-transitive (s-rn)
  `(role-transitive (rname-struct ,s-rn)))
(defmacro r-domain (s-rn)
  `(role-domain (rname-struct ,s-rn)))
(defmacro r-range (s-rn)
  `(role-range (rname-struct ,s-rn)))
(defmacro r-range-cname (s-rn)
  `(role-range-cname (rname-struct ,s-rn)))
(defmacro r-inclusions1 (s-rn)
  `(role-inclusions1 (rname-struct ,s-rn)))
(defmacro r-inclusions2 (s-rn)
  `(role-inclusions2 (rname-struct ,s-rn)))
(defmacro r-user-name (s-rn)
  `(role-user-name (rname-struct ,s-rn)))

;;_____________________________________________________________________________

;;;(defmacro q-make-queue ()
;;;  `(let ((q (list nil))) (setf (car q) q)))

(defmacro q-priority (queue-struct)
  `(queue-struct-priority ,queue-struct))
(defmacro q-active-list (queue-struct)
  `(queue-struct-active-list ,queue-struct))
(defmacro q-active-list-tail (queue-struct)
  `(queue-struct-active-list-tail ,queue-struct))
(defmacro q-entry-table (queue-struct)
  `(queue-struct-entry-table ,queue-struct))
(defmacro q-entry (q queue-struct)
  `(gethash ,q (queue-struct-entry-table ,queue-struct)))
(defmacro q-elements (q queue-struct)
  `(cdr (q-entry ,q ,queue-struct)))
(defmacro q-empty-p (q queue-struct)
  `(null (q-elements ,q ,queue-struct)))
(defmacro q-length (q queue-struct)
  `(length (q-elements ,q ,queue-struct)))
(defmacro q-total-active-queues (queue-struct)
  `(length (q-active-list ,queue-struct)))

(defun q-activate (q queue-struct)
  (let ((new-tail (list q)))
    (if (q-active-list queue-struct)
	(setf (cdr (q-active-list-tail queue-struct)) new-tail
	      (q-active-list-tail queue-struct) new-tail)
      (setf (q-active-list queue-struct) new-tail
	    (q-active-list-tail queue-struct) new-tail))))
(defun q-activate-as-stack (q queue-struct)
  (push q (q-active-list queue-struct)))
  
(defmacro q-next-queue ()
  "Return a cons of the next active cname and its queue"
  `(q-next-queue-no-priority))
;;  `(q-next-queue-priority))

(defmacro q-enqueue (q new-items)
  "Union the queue q with new-items"
  `(q-enqueue-no-priority ,q ,new-items))
;;  `(q-enqueue-priority ,q ,new-items)) 
;;_____________________________________________________________________________


(defun q-next-queue-no-priority ()
  "Return a cons of the next active cname and its queue"
  (let ((q (pop (q-active-list (ods-queue)))))
    (when q
      (let ((elements (q-elements q (ods-queue))))
	(remhash q (q-entry-table (ods-queue)))	
	(cons q elements)))))

(defun q-enqueue-no-priority (q new-items)
  (when new-items
    (let ((entry (q-entry q (ods-queue))))
      (if entry	  
	  (setf (cdr entry) (union (cdr entry) new-items :test 'eq))
	(progn (q-activate q (ods-queue))
	       (setf (q-entry q (ods-queue)) (cons nil new-items)))))))
;;_____________________________________________________________________________
;;_____________________________________________________________________________

(defmacro push-gci-list1 (lhs rhs)
  "Add input GCI (lhs <= rhs) to (ods-gci-list1) for later normalization"
  `(push (cons ,lhs ,rhs) (ods-gci-list1)))
(defmacro push-gci-list2 (lhs rhs)
  "Add lhs-normalized GCI (lhs <= rhs) to (ods-gci-list2) for later normalization"
  `(push (cons ,lhs ,rhs) (ods-gci-list2)))
(defmacro o-nexts (key)
  `(gethash ,key (ods-o)))
(defmacro o-nexts-2 (key)
  `(gethash ,key (ods2-o)))
(defmacro o-nexts-both (key)
  "Take the two together, assuming that both are mutually disjoint by initialization"
  `(append (gethash ,key (ods2-o))
	   (gethash ,key (ods-o))))

(defmacro r-preds (s-cn s-rn)
  `(gethash (cons ,s-cn ,s-rn) (ods-r-pred)))
(defmacro r-preds-2 (s-cn s-rn)
  `(gethash (cons ,s-cn ,s-rn) (ods2-r-pred)))
(defmacro r-preds-both (s-cn s-rn)
  `(append (gethash (cons ,s-cn ,s-rn) (ods2-r-pred))
	   (gethash (cons ,s-cn ,s-rn) (ods-r-pred))))

(defmacro r-succs (s-cn s-rn)
  `(gethash (cons ,s-cn ,s-rn) (ods-r-succ)))
(defmacro r-succs-2 (s-cn s-rn)
  `(gethash (cons ,s-cn ,s-rn) (ods2-r-succ)))
(defmacro r-succs-both (s-cn s-rn)
  `(append (gethash (cons ,s-cn ,s-rn) (ods2-r-succ))
	   (gethash (cons ,s-cn ,s-rn) (ods-r-succ))))

(defmacro check-edge (s-cn-src s-rn s-cn-snk)
  `(let ((pre-list (gethash (cons ,s-cn-snk ,s-rn)
			    (ods-r-pred))))
     (if (or (member ,s-cn-src pre-list
		     :test 'eq)
	     (member *top* pre-list
		     :test 'eq))
	 t nil)))
(defmacro check-edge-2 (s-cn-src s-rn s-cn-snk)
  `(let ((pre-list (gethash (cons ,s-cn-snk ,s-rn)
			    (ods2-r-pred))))
     (if (or (member ,s-cn-src pre-list
		     :test 'eq)
	     (member *top* pre-list
		     :test 'eq))
	 t nil)))
(defun check-edge-both (s-cn-src s-rn s-cn-snk)
  (or (check-edge s-cn-src s-rn s-cn-snk)
      (check-edge-2 s-cn-src s-rn s-cn-snk)))
;;_____________________________________________________________________________

(defmacro s-labels (s-cn)
  `(gethash ,s-cn (ods-s-table)))
(defmacro s-labels-2 (s-cn)
  `(gethash ,s-cn (ods2-s-table)))
(defun s-labels-both (s-cn)
  (let ((s1 (s-labels s-cn))
	(s2 (s-labels-2 s-cn)))
    (cond
     ((or (eq s2 *bot*)
	  (eq s1 *bot*))
      *bot*)
     (t
      (append (gethash s-cn (ods2-s-table))
	      (gethash s-cn (ods-s-table))))
     )))

(defmacro s-top-labels ()
  `(gethash *top* (ods-s-table)))
(defmacro s-top-labels-2 ()
  `(gethash *top* (ods2-s-table)))
(defmacro s-top-labels-both ()
  `(append (gethash *top* (ods2-s-table))
	   (gethash *top* (ods-s-table))))

(defmacro s-bottom-labels ()
  `(gethash *bot* (ods-s-table)))
(defmacro s-bottom-labels-2 ()
  `(gethash *bot* *s-table-2*))
(defmacro s-bottom-labels-both ()
  `(append (gethash *bot* (ods2-s-table))
	   (gethash *bot* (ods-s-table))))


;;_____________________________________________________________________________

(defmacro check-label (s-cn-l s-cn-n)
  `(check-label-original ,s-cn-l ,s-cn-n))
;;_____________________________________________________________________________

(defmacro check-labels (label-list s-cn-n)
  `(check-labels-naively ,label-list ,s-cn-n))

(defun check-labels-by-marking (label-list s-cn-n 
				&optional (marker t)
					  (s-labels (s-labels s-cn-n)))
  (when (or (eq s-cn-n *bot*) ;; the bottom
	    (eq s-labels *bot*)) ;; an unsatisfiable cname		
    (return-from check-labels-by-marking t))
  (make-s-marker)
  (dolist (x s-labels)
    (setf (s-marked x) marker))
  (every #'(lambda (x) (s-marked x))
	 label-list)
  )

(defmacro set-if-greater (var val)
  "Set var to val if greater that its current value"
  `(when (> ,val ,var)
     (setq ,var ,val)))
(defmacro set-if-smaller (var val)
  "Set var to val if smaller that its current value"
  `(when (< ,val ,var)
     (setq ,var ,val)))

(defun check-labels-naively (label-list s-cn-n)
  ;;(set-if-greater *biggest-left-conjunction* (length label-list))
  (dolist (x label-list t)
    (unless (check-label x s-cn-n)
      (return-from check-labels-naively nil))))
;;_____________________________________________________________________________

;;;(defun check-label-partial (s-cn-l s-cn-n &optional (visited (gensym)))
;;;  "This is an experiment with incomplete S datastructure that is intended to save space. Mostly, this approach does not work due to time"
;;;  (unless (eq (s-marked s-cn-n) visited)
;;;    (setf (s-marked s-cn-n) visited)
;;;    
;;;    (let ((s-candidates (s-labels s-cn-n)))
;;;      (or (eq s-cn-n s-cn-l)
;;;	  
;;;	  (eq s-cn-n *bot*) ;; the bottom
;;;	  (eq s-candidates *bot*) ;; an unsatisfiable cname
;;;	  ;; check recursively if the label appears in ss or ss's ancestors
;;;	  (some #'(lambda (x) (check-label-pdag s-cn-l x visited))		  
;;;		s-candidates)
;;;	  )
;;;      )))
;;_____________________________________________________________________________

(defun check-label-original (s-cn-l s-cn-n)
  (let ((ss (s-labels s-cn-n)))
    (or (eq s-cn-n *bot*) ;; the bottom
	(eq ss *bot*) ;; an unsatisfiable cname, ie. equivalent to the bottom
	(member s-cn-l ss 
		:test 'eq)
	(member s-cn-l (s-top-labels)
		:test 'eq))))
(defun check-label-2 (s-cn-l s-cn-n)
  (let ((ss (s-labels-2 s-cn-n)))
    (or (eq s-cn-n *bot*) ;; the bottom
	(eq ss *bot*) ;; an unsatisfiable cname, ie. equivalent to the bottom
	(member s-cn-l ss 
		:test 'eq)
	(member s-cn-l (s-top-labels-2)
		:test 'eq))))
(defun check-label-both (s-cn-l s-cn-n)
  (or (check-label s-cn-l s-cn-n)
      (check-label-2 s-cn-l s-cn-n)))

(defmacro is-unsat (s-cn)
  `(or (eq *bot* ,s-cn)
       (eq *bot* (s-labels ,s-cn))))
(defmacro is-unsat-2 (s-cn)
  `(or (eq *bot* ,s-cn)
       (eq *bot* (s-labels-2 ,s-cn))))
(defmacro is-unsat-both (s-cn)
  `(or (eq *bot* ,s-cn)
       (eq *bot* (s-labels ,s-cn))
       (eq *bot* (s-labels-2 ,s-cn))))

(defmacro is-new-cname (s-cn)
  `(and (integerp ,s-cn)
	(< ,s-cn 0)))

(defmacro n-system-cnames ()  
  `(let ((range-pair (ods-system-cname-range)))
     (+ 1 (- (cdr range-pair)
	     (car range-pair)))))
(defmacro n-backup-system-cnames ()  
  `(let ((range-pair (ods-backup-system-cname-range)))
     (+ 1 (- (cdr range-pair)
	     (car range-pair)))))

(defmacro subconcept (c)
  `(gethash ,c (ods-subconcept-table)))
(defmacro subconcept-2 (c)
  `(gethash ,c (ods2-subconcept-table)))

(defmacro activation-flag (s-cn)
  `(gethash ,s-cn (ods-activation-flag)))
(defmacro activation-flag-2 (s-cn)
  `(gethash ,s-cn (ods2-activation-flag)))
(defmacro activation-flag-both (s-cn)
  `(or (activation-flag ,s-cn)
       (activation-flag-2 ,s-cn)))


(defmacro copy-top-level-ontology-- ()
  (make-ontology :uri (ont-uri)
		 :state (ont-state)
		 :tbox-consistent? (ont-tbox-consistent?)
		 :abox-consistent? (ont-abox-consistent?)
		 :tbox-cyclic? (ont-tbox-cyclic?)
		 :tbox-general? (ont-tbox-general?)
		 :tbox-primitive? (ont-tbox-primitive?)
		 :has-mult-def? (ont-has-mult-def?)
		 :has-cri? (ont-has-cri?)
		 :individuals (copy-list (ont-individuals))
		 :reflexive-roles (copy-list (ont-reflexive-roles))
		 :transitive-roles (copy-list (ont-transitive-roles))
		 :rr-no-ri (ont-rr-no-ri)
		 :ri-no-rr (ont-ri-no-rr)
		 :roles-w-range (copy-list (ont-roles-w-range))
		 :roles-w-domain (copy-list (ont-roles-w-domain))
		 :n-pcdefs (ont-n-pcdefs)
		 :n-cdefs (ont-n-cdefs)
		 :n-gcis (ont-n-gcis)
		 :n-ris (ont-n-ris)
		 :n-cas (ont-n-cas)
		 :n-ras (ont-n-ras)
		 :preprocessing-time (ont-preprocessing-time)
		 :phierarchy-time (ont-phierarchy-time)
		 :classification-time (ont-classification-time)
		 ))

(defun copy-top-level-ontology ()
  (let ((new-ontology (copy-ontology *ontology*)))
    (setf (ont-individuals) (copy-list (ont-individuals)))
    (setf (ont-reflexive-roles) (copy-list (ont-reflexive-roles)))
    (setf (ont-transitive-roles) (copy-list (ont-transitive-roles)))
    (setf (ont-roles-w-range) (copy-list (ont-roles-w-range)))
    (setf (ont-roles-w-domain) (copy-list (ont-roles-w-domain)))
    
    new-ontology))
	

;;_____________________________________________________________________________
;;
;; Functions repository 
;;_____________________________________________________________________________

;;_____________________________________________________________________________
;;
;; Temporary TBox a.k.a. TTBox management functions
;;_____________________________________________________________________________

(defun init-ttbox ()
  "Initialize the value of data structure set 2 which is used for TTBox"
  (setf (ontology-ds-2 *ontology*)
    (make-internal-data-structure-2))
  (setf *backup-ontology* nil))

(defun activate-ttbox-f ()
  "Reset values of all ttbox-related variables to their default"
  (clrhash (ods2-o))
  (clrhash (ods2-s-table))
  (clrhash (ods2-r-pred))
  (clrhash (ods2-r-succ))
  (clrhash (ods2-subconcept-table))  
  (clrhash (ods2-activation-flag))
  ;; back up top-level information
  (setq *backup-ontology* (copy-top-level-ontology))
  ;; marking the old ranges -- system concept and role names on arrays  
  (setf (ods-backup-system-cname-range) (cons (car (ods-system-cname-range))
					      (cdr (ods-system-cname-range))))
  (setf (ods-backup-rname-array-fill-pointer) (length (ods-rname-array)))
  ;; set ttbox as active
  (setf (ont-ttbox-active?) t)
  (setf (ont-state) :i-cleared)
  *ontology*)
  
(defun deactivate-ttbox-f ()
  "Roll back to the previous state without axioms from the temp TBox"  
  (clrhash (ods2-o))
  (clrhash (ods2-s-table))
  (clrhash (ods2-r-pred))
  (clrhash (ods2-r-succ))
  (clrhash (ods2-subconcept-table))  
  (clrhash (ods2-activation-flag))
  (setf (ods2-total-s-counter) 0)
  ;; restore top-level information
  (when *backup-ontology*
    (setq *ontology* *backup-ontology*)
    ;; pointer in the repository table has to be updated
    (setf (cel-ontology (ont-uri))
      *ontology*)
    (setq *backup-ontology* nil))
  
  ;; recovering the old concept ranges
  (loop for i 
      from (+ (cdr (ods-backup-system-cname-range)) 1)
      to   (cdr (ods-system-cname-range)) do
	(remhash (c-user-name i) (ods-cname-table))
	(setf (cname-struct i) nil)
	)
  ;; shrink the array to the original size
  (setf (fill-pointer (ods-cname-array)) 
    (+ (cdr (ods-backup-system-cname-range)) 1))
  ;; recover the old range
  (setf (ods-system-cname-range) (ods-backup-system-cname-range))
  
  ;; recovering the old role ranges
  (loop for i
      from (ods-backup-rname-array-fill-pointer)
      to   (- (length (ods-rname-array)) 1) do
	(remhash (r-user-name i) (ods-rname-table))
	(setf (rname-struct i) nil)
	)
  ;; shrink the array to the original size
  (setf (fill-pointer (ods-rname-array)) 
    (ods-backup-rname-array-fill-pointer))
  
  ;;(setf (ont-ttbox-active?) nil)
  ;;(setf (ont-state) :i-cleared)
  *ontology*)
;;_____________________________________________________________________________

(defun clear-ttbox-f ()
  (unless (ont-state? (:i-prepared :i-classified :i-taxonomized))
    (return-from clear-ttbox-f :tbox-state-error))
  
  (deactivate-ttbox-f)
  (activate-ttbox-f))
;;_____________________________________________________________________________

(defun merge-hash (mergee merger &key (test 'eq))
  "Merge hashtable merger to hashtable mergee by taking union of the corresponding values (using :test function) and set as value of the key to mergee."
  (maphash #'(lambda (key value)
	       (setf (gethash key mergee) (union value
						 (gethash key mergee)
						 :test test)))
	   merger))
;;_____________________________________________________________________________

(defun commit-ttbox-f ()
  "Commit change by merging all necessary data structures from the set 2 to set 1 the deactivate the temp TBox"
  (unless (ont-state? (:i-classified :i-taxonomized))
    (return-from commit-ttbox-f ':tbox-state-error))    
  
  ;; Merge modifications found in ttbox to tbox
  (merge-hash (ods-o) (ods2-o) 
	      :test 'equal)
  (merge-hash (ods-s-table) (ods2-s-table))
  (merge-hash (ods-r-pred) (ods2-r-pred))
  (merge-hash (ods-r-succ) (ods2-r-succ))
  (incf (ods-total-s-counter)
	(ods2-total-s-counter))	
  ;;(merge-hash (ods-subconcept)
  ;;(merge-hash (ods-activation-flag)
  
  ;; Clear set 2 data structure
  (clrhash (ods2-o))
  (clrhash (ods2-s-table))
  (clrhash (ods2-r-pred))
  (clrhash (ods2-r-succ))
  (clrhash (ods2-subconcept-table))  
  (clrhash (ods2-activation-flag))  
  (setf (ods2-total-s-counter) 0)
  
  (setf (ont-ttbox-active?) nil)
  (setf (ont-state) :classified)
  )
;;_____________________________________________________________________________

(defun classify-ttbox-f (&optional commit?)
  (when (ont-state? :i-cleared)
    (normalize-ttbox))
  (unless (ont-state? :i-prepared)
    (return-from classify-ttbox-f :tbox-state-error))
  
  (act-complete-all-imp-sets-2)  
  (setf (ont-state) :i-classified)
  
  (when commit?
    (commit-ttbox-f))
  
  *ontology*)
;;_____________________________________________________________________________

(defun init-s-r-structures ()
  ;; initialize (ods-s-table), this is used in place of the array
  (setf (ods-s-table) (make-hash-table :test 'eq))
  ;; initialize (ods-r-...) structures  
  (setf (ods-r-pred) (make-hash-table :test 'equal))
  (setf (ods-r-succ) (make-hash-table :test 'equal))
  )
;;_____________________________________________________________________________

(defun add-cname-later (cn &key (status nil) (definition nil))
  (push (cons cn (cons status definition))
	(ods-defined-cname-list)))
;;_____________________________________________________________________________

(defun add-synonymic-cnames ()
  "Add all representative synonyms, ie., cnames that are supposed to be in the range of *system-cname* array. Some synonymic (equivalent) classes might not connect with other system names. We add these here."
  (maphash #'(lambda (key value)
	       (when (listp value)
		 ;; key is a representative synonym
		 (add-cname key)))	       	   
	   (ods-synonym-table))
  )
;;_____________________________________________________________________________

(defun add-cname (cn &key (status nil) 
			  (definition nil) 
			  (individual? nil)
			  (concept? nil))
  "Add a user cname cn to the system if not done yet"
  (setq cn (synonym-of cn))
  (cond
   ((or (is-new-cname cn)
	(eq cn top)
	(eq cn bottom))
    (when individual?
      (err "Punning: TOP and BOTTOM are predefined concepts and cannot be used as individuals!")))
   ((system-cname cn)
    (let ((s-cn (system-cname cn)))
      (cond
       ;; if it's surely an individual    
       ((and (c-individual? s-cn)
	     (not concept?))
	(setf (c-definition s-cn) (or definition 'top)))
       ;; if it's a concept
       ((and (not (c-individual? s-cn))
	     (not individual?))
	(cond
	 ((eq status nil)
	  t)
	 ((and (eq status 'primitive)
	       (eq (c-status s-cn) 'primitive))
	  ;; two primitive definitions can be absorbed
	  (setf (c-definition s-cn) (list 'and
					  (c-definition s-cn)
					  definition)))
	 (t
	  ;; otherwise, multiple definitions
	  (setf (ont-has-mult-def?) t)
	  (setf (c-status s-cn) nil)))
	)
       ;; otherwise, punning occurs. this's not allowed
       (t
	(err "Punning: ~A occurs as both a concept and an individual name. This is currently not allowed." cn)))))
   (t 
    ;; unless cn is new, top, bottom, or it is already there is the system, 
    ;; create a system name for it    
    (let ((s-cn (vector-push-extend (make-concept :user-name cn
						  :status status
						  :definition definition
						  :individual? individual?)
				    (ods-cname-array)
				    *cname-array-enlarge-factor*)))
      (setf (system-cname cn) s-cn)
      (setf (cdr (ods-system-cname-range)) s-cn)))))
;;_____________________________________________________________________________

(defun add-rname (rn &key (reflexive nil)
			  (transitive nil)
			  (s-rn nil))
  "Add a user rname rn to the system if not done yet"
  (setq s-rn (system-rname rn))    
  
  (cond
   (s-rn
    ;; if rn is already added, update transitivity
    (when transitive
      ;;(pushnew rn (ont-transitive-roles))
      (setf (r-transitive s-rn) t))
    (when reflexive
      (pushnew rn (ont-reflexive-roles))
      (setf (r-reflexive s-rn) t)))
   (t
    ;; otherwise, create and add it
    (setf (system-rname rn)
      (vector-push-extend (make-role :user-name rn
				     :reflexive reflexive
				     :transitive transitive)
			  (ods-rname-array) 
			  *rname-array-enlarge-factor*))
    (when reflexive
      (pushnew rn (ont-reflexive-roles)))
    )))
;;_____________________________________________________________________________

(defun gen-new-cname ()
  "Generate a new system cname and return"
  (decf (car (ods-system-cname-range))))
;;_____________________________________________________________________________

(defun synonym-of (cn)
  "Find and return the synonym representative of cn; nil if cn has no synonym"
  (let ((cn-value (gethash cn (ods-synonym-table))))
    (if (listp cn-value)
	cn
      (synonym-of cn-value))))
;;_____________________________________________________________________________

(defun synonyms (cn)
  "Find and return the set of all synonyms of cn; nil if cn has no synonym"
  (let ((cn-value (gethash cn (ods-synonym-table))))
    (if (listp cn-value)
	(cons cn cn-value)
      (synonyms cn-value))))
;;_____________________________________________________________________________

(defun add-synonym (cn1 cn2)
  "Add a pair of synonym"
  (setq cn1 (synonym-of cn1))
  (setq cn2 (synonym-of cn2))
  
  (if (eq cn1 cn2)
      (return-from add-synonym))

  ;; cn1 and cn2 must be unified, ie. keep one remove the other
  ;; if the removed one has been added to the system, flag it!  
  
  (if (> (length (gethash cn1 (ods-synonym-table)))
	 (length (gethash cn2 (ods-synonym-table))))
      (synonym-class-union cn2 cn1)
    (synonym-class-union cn1 cn2)))
;;_____________________________________________________________________________

(defun synonym-class-union (x y)
  "Union x and y by keeping y as the representative"
  ;; if x has been added to the system, remove it!!!
;;;  (let ((s-x (system-cname x)))
;;;    (when s-x
;;;      (remhash x (ods-cname-table))
;;;      (setf (cname-struct s-x) nil)))    
  
  (push x (ods-synonym-list))
  ;; update the union-find structure
  (setf (gethash y (ods-synonym-table)) (cons x (union (gethash x (ods-synonym-table))
						   (gethash y (ods-synonym-table))
						   :test 'eq)))
  (setf (gethash x (ods-synonym-table)) y))
;;_____________________________________________________________________________

(defun set-labels (s-cn-n labels)
  "Set a set of new labels into S of node"
  (setf (s-labels s-cn-n) labels)
  (incf (ods-total-s-counter) (length labels)))
;;_____________________________________________________________________________

(defun add-label (s-cn-l s-cn-n)
  "Add a new label into S of node"
  (incf (ods-total-s-counter))
  (push s-cn-l (s-labels s-cn-n)))
(defun add-label-2 (s-cn-l s-cn-n)
  "Add a new label into S of node"
  (incf (ods2-total-s-counter))
  (push s-cn-l (s-labels-2 s-cn-n)))
;;_____________________________________________________________________________

(defun add-edge (s-cn-src s-rn s-cn-snk)
  "Add a new edge (pair of cnames) into R of s-rn"
  (setf (r-preds s-cn-snk s-rn) (cons s-cn-src 
				      (r-preds s-cn-snk s-rn)))	  
  (setf (r-succs s-cn-src s-rn) (cons s-cn-snk 
				      (r-succs s-cn-src s-rn))))
(defun add-edge-2 (s-cn-src s-rn s-cn-snk)
  "Add a new edge (pair of cnames) into R of s-rn"
  (push s-cn-src (r-preds-2 s-cn-snk s-rn))
  (push s-cn-snk (r-succs-2 s-cn-src s-rn)))
;;_____________________________________________________________________________

(defun process-new-edges (s-cn-a s-rn-r s-cn-b)
  "Add all new edges resulting from this, and update queues of aftected cnames"
  (if (check-edge s-cn-a s-rn-r s-cn-b)
      (return-from process-new-edges))

  (dolist (s-rn-s (adjoin s-rn-r 
			  (r-ancestors s-rn-r)
			  :test 'eq))
    (add-edge s-cn-a 
	      s-rn-s 
	      s-cn-b)
    (update-queue s-cn-a 
		  s-rn-s 
		  s-cn-b)
    
    (dolist (pair (r-inclusions1 s-rn-s))
      (let ((s-rn-t (car pair))
	    (s-rn-u (cdr pair)))
	(dolist (s-cn-bb (r-succs s-cn-b s-rn-t))
	  (process-new-edges s-cn-a
			     s-rn-u
			     s-cn-bb))))
    
    (dolist (pair (r-inclusions2 s-rn-s))
      (let ((s-rn-t (car pair))
	    (s-rn-u (cdr pair)))
	(dolist (s-cn-aa (r-preds s-cn-a s-rn-t))
	  (process-new-edges s-cn-aa 
			     s-rn-u 
			     s-cn-b))))
    ))

(defun process-new-edges-2 (s-cn-a s-rn-r s-cn-b)
  "Add all new edges resulting from this, and update queues of aftected cnames"
  (if (check-edge-both s-cn-a s-rn-r s-cn-b)
      (return-from process-new-edges-2))

  (dolist (s-rn-s (adjoin s-rn-r 
			  (r-ancestors s-rn-r)
			  :test 'eq))
    (add-edge-2 s-cn-a 
		s-rn-s 
		s-cn-b)
    (update-queue-both s-cn-a 
		       s-rn-s 
		       s-cn-b)
    
    (dolist (pair (r-inclusions1 s-rn-s))
      (let ((s-rn-t (car pair))
	    (s-rn-u (cdr pair)))
	(dolist (s-cn-bb (r-succs-both s-cn-b s-rn-t))
	  (process-new-edges-2 s-cn-a
			       s-rn-u
			       s-cn-bb))))
    
    (dolist (pair (r-inclusions2 s-rn-s))
      (let ((s-rn-t (car pair))
	    (s-rn-u (cdr pair)))
	(dolist (s-cn-aa (r-preds-both s-cn-a s-rn-t))
	  (process-new-edges-2 s-cn-aa 
			       s-rn-u 
			       s-cn-b))))
    ))
;;_____________________________________________________________________________

;;; to remove
;;;(defun propagate-predecessors-descendants (a x)
;;;  "x is a new label for a. For each r, immediate consequences of (some r x) must be enqueued to all r-predecessors b and b's descendants of a"
;;;  
;;;  ;;(msg "propagate ~a from ~a to its predessors' descendants"
;;;  ;;     (user-cname x) (user-cname a))
;;;  
;;;  
;;;  (loop ;; for each system rname 
;;;      for r from 0 to (- (length (ods-rname-array)) 1)
;;;      do (let ((new-items (o-nexts (cons r x))))
;;;	   ;; only non-empty set should be added to queues
;;;	   (when new-items
;;;	     ;; a is also an r-predecessor, if r is reflexive
;;;	     ;; note that the following when-clause can be replaced with
;;;	     ;; (q-enqueue a (o-nexts-reflexive x)) outside the loop, but
;;;	     ;; I believe this is more optimized.
;;;	     (when (r-reflexive r)
;;;	       (q-enqueue a new-items))
;;;	     ;; apply this to each predecessor B
;;;	     (dolist (b (r-preds a r))
;;;	       ;;(q-enqueue b new-items)))
;;;	       ;; instead, queues for all b's descendants are updated
;;;	       (sweep-descendants-update-queues b new-items)))
;;;	   ))
;;;  )

;;; to remove
;;;(defun sweep-descendants-update-queues (b new-items 
;;;					&optional (visited (gensym)))
;;;  (unless (eq (s-marked b) visited)
;;;    (setf (s-marked b) visited)
;;;    (q-enqueue b new-items)
;;;    (dolist (x (s-nodes b))
;;;      (sweep-descendants-update-queues x new-items visited))))
  
;;_____________________________________________________________________________

;;; call system union
(defmacro my-union- (s1 s2 &key (test 'eql))
  ;;(incf *total-union-calls*)
  `(union ,s1 ,s2 :test ,test))

;;; My fast(er) operations on sets
(defun my-union (s1 s2 &key (test 'eql))
  ;;(incf *total-union-calls*)
  (declare (ignore test))
  
  (cond
   ((> (length s1) (length s2))
    (my-union1 s1 s2))
   (t
    (my-union1 s2 s1))))
      
(defmacro my-union1 (base suplements)
  `(let ((union (copy-list ,base)))
     (dolist (s ,suplements)
       (unless (member s ,base :test (if (listp s)
					 'my-equal
				       'eq))
	 (push s union)))
     union))

(defun my-equal (x y)
  "Compare x and y, given that 
- x is a list and y is unknown
- list here means just a dotted-pair of symbols or pointers to lists"
  (and (listp y)
       (eq (car x) (car y))
       (eq (cdr x) (cdr y))))

(defun my-union--(s1 s2 &key (test 'eq))
  (if (> (length s1) (length s2))
      (union (copy-list s2) s1 :test test)
    (union (copy-list s1) s2 :test test)))

(defun my-set-difference (s1 s2 &key (test 'eq))
  (if (> (length s1) (length s2))
      (nset-difference (copy-list s2) s1 :test test)
    (nset-difference (copy-list s1) s2 :test test)))
;;_____________________________________________________________________________

(defun update-queue (s-cn-a s-rn s-cn-b)
  "Update queue of s-cn-a all consequences from having added an edge s-rn to s-cn-b"
  (let ((new-items '()))
    (dolist (x (s-labels s-cn-b))
      (setq new-items (union new-items
			     (o-nexts (cons s-rn x))
			     :test 'eq)))
    (dolist (x (s-top-labels))
      (setq new-items (union new-items
			     (o-nexts (cons s-rn x))
			     :test 'eq)))
    (q-enqueue s-cn-a new-items)))

(defun update-queue-both (s-cn-a s-rn s-cn-b)
  "Update queue of s-cn-a all consequences from having added an edge s-rn to s-cn-b"
  (let ((new-items '()))
    (dolist (x (s-labels-both s-cn-b))
      (setq new-items (union new-items
			     (o-nexts-both (cons s-rn x))
			     :test 'eq)))
    (dolist (x (s-top-labels-both))
      (setq new-items (union new-items
			     (o-nexts-both (cons s-rn x))
			     :test 'eq)))
    (q-enqueue s-cn-a new-items)))
;;_____________________________________________________________________________

;;;(defun enqueue (s-cn new-items)
;;;  "Append new-items to the queue of s-cn"
;;;  (when new-items
;;;     
;;;    (setf (queue s-cn) (append (queue s-cn) new-items))
;;;;;    (setf (queue s-cn) (union (queue s-cn) new-items 
;;;;;			      :test 'equal))
;;;    
;;;    (unless (queue-flag s-cn)
;;;      ;; if queue of s-cn is not set active, do so
;;;      (setf (queue-flag s-cn) t)
;;;      ;; stack behavior
;;;      (push s-cn *active-queue-list*)
;;;      ;; queue behavior
;;;      ;;(setq *active-queue-list* (nconc *active-queue-list* 
;;;	;;			       (list s-cn)))
;;;      )))
;;_____________________________________________________________________________

(defun has-bottom (c)
  "Check if the bottom occurs in the concept c"
  (cond
   ((eq c bottom) t)
   ((listp c)
    (let ((func (car c))
	  (args (cdr c)))
      (cond
       ((eq func 'and)
	(some #'has-bottom args))
       ((eq func 'some)
	(has-bottom (cadr args)))
       (t ;; other DL concept constructs are not accepted in CEL
        nil)
       )))
   (t ;; top or a concept name
    nil)))
;;_____________________________________________________________________________

(defun el-concept (c)
  "Check if the concept c is well-formed"
  (cond
   ((listp c)
    (let ((func (car c))
	  (args (cdr c)))
      (cond
       ((eq func 'and)
	(every #'el-concept args))
       ((eq func 'some)
	(and (eq (length args) 2)
	     (not (listp (car args)))
	     (el-concept (cadr args))))
       (t ;; other DL concept constructs are not accepted in CEL
        nil)
       )))           
   (t ;; atomic concept is always well-formed
    t
    )))
;;_____________________________________________________________________________

(defun and-expansion (c)
  "Flaten top-level conjunction in c and return"
  (if (and (listp c)
           (eq 'and (car c)))
      (let ((conjuncts (union (expand-and (cadr c))
			      (expand-and-list (cddr c))
			      :test 'equal)))
	(if (= 1 (length conjuncts))
	    (car conjuncts)            
	  (cons 'and conjuncts)))
    c))
;;_____________________________________________________________________________

(defun expand-and (term)
  (if (and (listp term)
           (eq 'and (car term)))
      (expand-and-list (cdr term))
    (list term)))
;;_____________________________________________________________________________

(defun expand-and-list (term-list)
  (if (null term-list)
      nil
    (union (expand-and (car term-list))
	   (expand-and-list (cdr term-list))
	   :test 'equal)))
;;_____________________________________________________________________________

(defun set-subconcept-name (cn concept)
  "A (fully) defined concept name is reused as a new name for normalization"
  (if (ont-ttbox-active?)
      (setf (subconcept-2 concept)
	(cons cn 'b))
    (setf (subconcept concept)
      (cons cn 'b)))
  cn
  )
;;_____________________________________________________________________________

(defun get-subconcept-name (concept &key (side 'l)
					 (generate t))
  "Return the associated name for the subconcept in question, if any. If not, generate a new one and return. Flag indicating the location side of the subconcept has to be updated."
  ;; to be programmed: do not generate but only retrieve the name if any
  (declare (ignore generate))
  
  (let ((name-flag (or (and (ont-ttbox-active?)
			    (subconcept-2 concept))
		       (subconcept concept))))
    (cond
     (name-flag
      (if (or (and (eq side 'l)
		   (eq (cdr name-flag) 'r))
	      (and (eq side 'r)
		   (eq (cdr name-flag) 'l)))
	  ;; if ttbox, create a new name-flag in *subconcept-table-2*
	  ;; if not, update the existing one
	  (if (ont-ttbox-active?)
	      (setf (subconcept-2 concept)
		(cons (car name-flag) 'b))		    
	    (setf (cdr name-flag) 'b)))
      (return-from get-subconcept-name (car name-flag)))
      
     (t
      (let ((new-name (gen-new-cname)))
	(if (ont-ttbox-active?)
	    (setf (subconcept-2 concept)
	      (cons new-name side))
	  (setf (subconcept concept)
	    (cons new-name side)))	  	
	(return-from get-subconcept-name new-name)))
     )))
;;_____________________________________________________________________________

;;_____________________________________________________________________________
;;
;; Preprocessing procedures including normalization 
;;_____________________________________________________________________________

(defun eval-tbox-axioms (input-file 
			 &optional (start-time (get-internal-run-time))
				   (axioms nil))
  "Read and evaluate each axiom, then preprocess"  
  ;; open the INPUT-FILE in read mode
  (with-open-file (input-stream input-file 
		   :direction :input
		   :if-does-not-exist nil)    
    (when (null input-stream)
      (err "File (~a) does not exist!" input-file)
      (return-from eval-tbox-axioms nil))    
    ;; (setf (ont-uri) input-file) === uri was set before
    ;; read each Lisp statement one by one, until the end of file (EOF).
    ;; each statement is to evaluated accordingly
    (do ((statement 
          (read input-stream nil input-stream)
          (read input-stream nil input-stream)))
        ((eq statement input-stream) t)
      
      (push (copy-tree statement) axioms)
      
      (handler-case (eval statement)	
	(error ()
	  (pop axioms)
	  (err "Unrecognized axiom: ~S" statement))	
	)
      ))  
  ;; keep all original axioms for module extraction and/or pinpointing
  (setf (odsm-axioms) axioms)
  ;; normalize the ontology and initialize o-next data structure
  (normalize)
  ;;(print *ontology*)
  (setf (ont-preprocessing-time) (/ (- (get-internal-run-time) start-time)
				     internal-time-units-per-second))
  T)
;;_____________________________________________________________________________

(defun propagate-reflexivity (&optional (ref-roles (ont-reflexive-roles))
					(new-ref-roles nil))
  (dolist (r ref-roles)       
    
    (let ((s-r (system-rname r)))
      
      (pushnew r (ont-reflexive-roles)
	       :test 'eq)
      (setf (r-reflexive s-r) t)
      
      ;; super-roles
      (dolist (s-s (r-ancestors s-r))
	(unless (r-reflexive s-s)
	  (pushnew (user-rname s-s) new-ref-roles)))
      
      ;; role inclusion r o s <= u
      (dolist (pair-s-u (r-inclusions1 s-r))	
	(let ((s (car pair-s-u))
	      (u (cdr pair-s-u)))
	  ;; reflexivity on both r and s implies reflexivity on u
	  (when (and (r-reflexive s)
		     (not (r-reflexive u)))
	    (pushnew (user-rname u) new-ref-roles
		     :test 'eq))
	  ;; r o s <= u implies to s <= u, given reflexivity on r
	  ;; note: the cri cant be removed	   
	  (add-rh s u :system-rname? t)
	  ))
	  	  
      ;; role inclusion s o r <= u
      (dolist (pair-s-u (r-inclusions2 s-r))
	(let ((s (car pair-s-u))
	      (u (cdr pair-s-u)))
	  ;; reflexivity on both r and s implies reflexivity on u
	  (when (and (r-reflexive s)
		     (not (r-reflexive u)))
	    (pushnew (user-rname u) new-ref-roles
		     :test 'eq))
	  ;; s o r <= u implies to s <= u, given reflexivity on r
	  ;; note: the cri cant be removed
	  (add-rh s u :system-rname? t)
	  ))
      
;;;      ;; role inclusion r o s <= u
;;;      (dolist (pair-s-u (r-inclusions1 s-r))
;;;	(let ((s (car pair-s-u))
;;;	      (u (cdr pair-s-u)))
;;;	  (when (and (r-reflexive s)
;;;		     (not (r-reflexive u)))
;;;	    (pushnew (user-rname u) new-ref-roles))))
;;;      ;; above, if s is not reflexive but r is, this cri boils down to a role hierarchy s <= u.   similary to the case below!!!!
;;;      
;;;      ;; role inclusion s o r <= u
;;;      (dolist (pair-s-u (r-inclusions2 s-r))
;;;	(let ((s (car pair-s-u))
;;;	      (u (cdr pair-s-u)))
;;;	  (when (and (r-reflexive s)
;;;		     (not (r-reflexive u)))
;;;	    (pushnew (user-rname u) new-ref-roles))))
      
      ))
  
  (when new-ref-roles
    (propagate-reflexivity new-ref-roles)))
;;_____________________________________________________________________________


(defun normalize ()
  "This is the two-phase normalization: the lhs and rhs normalization. After this procedure terminates, we get the (ods-o) mapping completely initialized.

Note: this version is aware of role range axiom, i.e. the range of r will be inserted as a conjunct of all r-nested concepts"
  
  ;; EL++ with complex role inclusion and range restriction on roles are in general not tractable (even undecidable). we rule out this case by rejecting any ontology having both features at the( same time.
  (cond
   ((and (ont-has-cri?)
	 (ont-roles-w-range))
    (err "Tractability: complex role inclusions and range restrictions have been detected in this ontology. This is not allowed since it violates the tractability barrier of EL. The TBox is now reset!")
    (clear-tbox-f)
    (return-from normalize ':unsupported))
   ((ont-has-cri?)
    (setf (ont-ri-no-rr) t))
   ((ont-roles-w-range)
    (setf (ont-rr-no-ri) t))
   )       

  ;; pre-compile all reflexive roles -- both explicit and (trivially but complete) implicit. r is reflexive (added to (ont-reflexive-roles) and (r-reflexive r) is t) iff ref(r), s <= r and s is reflexive, or s o t <= r and s,r are both reflexive
  (propagate-reflexivity)

  
  ;;; REMARK: the domain and range encoding below must be done in regard with all reflexive roles, including inferred ones. Therefore, this must be executed only after reflexivity has been properly progagated (as already done above this comment).
  
  ;; domain can be expressed with a GCI and an existential restriction. however, if the role in question is known to be reflexive, it's optimized by dropping the existential restriction as the domain holds virtually everywhere!
  (dolist (s (ont-roles-w-domain))
    (setq s (system-rname s))
    (dolist (r (cons s (r-descendants s)))
      (push-gci-list1 (if (r-reflexive r)
			  top 
			`(some ,(user-rname r) top))
		      (r-domain s))))
  ;; similarly, range on a reflexive role is equivalent to the top concept
  (dolist (s (ont-roles-w-range))
    (setq s (system-rname s))
    (dolist (r (cons s (r-descendants s)))
      (when (r-reflexive r)
	(push-gci-list1 top (r-range-cname s)))))
  
  ;; normalize the lhs of all the input gci axioms
  (dolist (axiom (ods-gci-list1) t)
    (normalize-left (car axiom)
		    (cdr axiom)))    
  (setf (ods-gci-list1) nil)

  ;; add -- or update status of -- (primitively) defined cnames to the system, after the first pass has done. this is required since a concept may appear for the first time at the beginning of the ontology without information of its status, and this status information might be provided later on.
  (dolist (c (ods-defined-cname-list))
    (add-cname (car c) 
	       :status (cadr c)
	       :definition (cddr c)
	       :concept? t))
  (setf (ods-defined-cname-list) nil)
  
  ;; normalize the rhs of those axioms that have been processed on lhs
  (dolist (axiom (ods-gci-list2) t)    
    (normalize-right-w-range (cdr axiom)
			     (car axiom)))
  (setf (ods-gci-list2) nil)

  ;; add synonymic cnames that are representative of a synonymic class and probably have not been added before.
  (add-synonymic-cnames)
  
  ;; clear all unused data structures
  ;;(clrhash (ods-subconcept-table))
  ;;(setf (ods-subconcept-table) nil)
  
  (when (or (ont-tbox-general?)
	    (ont-has-mult-def?)
	    (> (ont-n-cdefs) 0))
    (setf (ont-tbox-primitive?) nil))     
  
  (setf (ont-state) :prepared)
  t)
;;_____________________________________________________________________________


(defun normalize-ttbox ()
  "Simply normalize the left and then right of all incremental GCIs.
Issues: (1) add-cname-later is not considered here as there is problem with retraction of cname status (A may turned to be defined, whereas previously primitive) (2) synonymic cnames are also not considered due to the same reason
Solutions: (1) do this when TTBox is committed (2) Aaarch!"
  ;; normalize the lhs of all the input gci axioms
  (dolist (axiom (ods-gci-list1) t)
    (normalize-left (car axiom)
		    (cdr axiom)))    
  (setf (ods-gci-list1) nil)
  
  ;; normalize the rhs of those axioms that have been processed on lhs
  (dolist (axiom (ods-gci-list2) t)    
    (normalize-right-w-range (cdr axiom)
			     (car axiom)))
  (setf (ods-gci-list2) nil)
  
  (setf (ont-state) :i-prepared)
  t)
;;_____________________________________________________________________________
;;_____________________________________________________________________________

(defun mark-phierarchy-cnames ()
  (loop for cn from 2 to (- (length (ods-cname-array)) 1) do
	(mark-phierarchy-cname cn)))

(defun mark-phierarchy-cname (cn)
  (cond
   ;; not known to be declared primitive or defined, assumed phierarchy
   ((not (c-status cn))
    (setf (c-status cn) 'phierarchy)
    (push cn (ods-phierarchy-cname-list))
    t)
   ((eq (c-status cn) 'phierarchy) t)
   ((eq (c-status cn) 'defined) nil)
   ((eq (c-status cn) 'primitive-not-ph) nil)
   ((eq (c-status cn) 'primitive)
    (let ((def (c-definition cn)))
      (cond
       ;; cn has definition: cn <= x
       ((and (not (listp def))
	     (mark-phierarchy-cname (system-cname def)))
	(setf (c-status cn) 'phierarchy)
	(push cn (ods-phierarchy-cname-list))
	t)
       ;; cn has definition: cn <= (and x1 x2 ...)
       ((and (listp def)
	     (eq (car def) 'and)
	     (every #'(lambda (x)
			(and (not (listp x))
			     (mark-phierarchy-cname (system-cname x))))
		    (cdr def)))
	(setf (c-status cn) 'phierarchy)
	(push cn (ods-phierarchy-cname-list))
	t)
       (t 
	(setf (c-status cn) 'primitive-not-ph)
	nil))))))

;;_____________________________________________________________________________
;;_____________________________________________________________________________


;;_____________________________________________________________________________

(defun add-rh (rn-r rn-s &key system-rname?)
  "Add a role hierarchy rn-r <= rn-s to the system"
  ;;(setf (ont-has-cri?) t)
  (unless system-rname?
    (setq rn-r (system-rname rn-r))
    (setq rn-s (system-rname rn-s)))
  
  (when (or (eq rn-r rn-s) ;; they are identical or synonymic roles
	    (member rn-s (r-ancestors rn-r) ;; known relationship
		    :test 'eq))
    (return-from add-rh))
  
  (let ((s-subsumers (adjoin rn-s (r-ancestors rn-s)
			     :test 'eq))
	(r-subsumees (adjoin rn-r (r-descendants rn-r)
			     :test 'eq)))
    (dolist (rr r-subsumees)
      (setf (r-ancestors rr)
	(union (r-ancestors rr) s-subsumers
	       :test 'eq)))
    (dolist (ss s-subsumers)
      (setf (r-descendants ss)
	(union (r-descendants ss) r-subsumees
	       :test 'eq)))
    ))
;;_____________________________________________________________________________

(defun add-ri (rn-r rn-s rn-t)
  "Add a role inclusion rn-r o rn-s <= rn-t to the system"
  (cond
   ((and (eq rn-r rn-s)
	 (eq rn-r rn-t))
    ;; this is transitivity
    (pushnew rn-r (ont-transitive-roles))
    (add-rname rn-r :transitive t))
   (t
    ;; this is a complex role inclusion, e.g., right-identity rule  
    (setf (ont-has-cri?) t)))
  
  (setq rn-r (system-rname rn-r))
  (setq rn-s (system-rname rn-s))
  (setq rn-t (system-rname rn-t))
  
  (pushnew (cons rn-s rn-t)
	   (role-inclusions1 (rname-struct rn-r))
	   :test 'equal)
  (pushnew (cons rn-r rn-t)
	   (role-inclusions2 (rname-struct rn-s))
	   :test 'equal))
;;_____________________________________________________________________________

(defun normalize-left (left &optional right)
  "Normalize the left part of a GCI and keep it in (ods-gci-list1) for 2nd pass"
  (cond
   ((has-bottom left)
    (return-from normalize-left))
   ((listp left)
    (cond
     ;; ========> NF2 & NF3: LEFT = (AND CC1 CC2 ...)
     ;; n-ary conjunction
     ((eq (car left) 'and)        
      (do ((i 1 (incf i))	     
	   (n (length left)))
	  ((= i n))
	;; for each conjunct, do the following:	  
	(let ((c (nth i left)))	    
	  (if (listp c)     
	      (setf (nth i left) (normalize-left c))))))	
     ;; ========> NF4: LEFT = (SOME R C?)
     ((eq (car left) 'some)
      (let ((c (caddr left)))
	(if (listp c) ;; LEFT = (SOME R (...))
	    (setf (caddr left) (normalize-left c)))))
     ;; ========> Unrecognized
     (t
;;;      (err "The construct (~a) is not recognized. This assertion is skipped."
;;;	   (car left))
;;;      (return-from normalize-left))
      )))
   (t
    ;;  ==========> LEFT = C
    ;; do nothing as it is in normal form already
    ))
  
  (if (null right)
      (setq right (get-subconcept-name left :side 'l)))
	    	    	     
  (push-gci-list2 left right)  
  
  (return-from normalize-left right))
;;_____________________________________________________________________________

(defun normalize-right (right &optional left)
  "Normalize the right part of a GCI and update the mapping (ods-o)"
  (cond
   ((has-bottom right)
    ;; ===========> RIGHT = the bottom
    ;; add to (ods-o) mapping as it is in normal form
    (add-o-entry left bottom))
   ((listp right)
    (cond
     ;; ========> RIGHT = (AND C1 C2 ...)
     ((eq (car right) 'and)
      (if left
	  (dolist (conjunct (cdr right)) ;; split rhs conjunction
	    (normalize-right conjunct left))
	(progn
	  (do ((i 1 (incf i))	     
	       (n (length right)))
	      ((= i n))		 
	    ;; for each conjunct, do the following:
	    (let ((c (nth i right)))		
	      (if (listp c)
		  (setf (nth i right) (normalize-right c)))))
	  
	  (setq left (get-subconcept-name right))
	  (dolist (conjunct (cdr right)) ;; split rhs conjunction
	    (normalize-right conjunct left))
	  (return-from normalize-right left))))
     ;; ========> RIGHT = (SOME R C?)
     ((eq (car right) 'some)
      (let ((c (caddr right)))	  
	(if (listp c) ;; RIGHT = (SOME R (...))
	    (setf (caddr right) (normalize-right c)))	  
	;; RIGHT = (SOME R C)
	(if (null left)
	    (setq left (get-subconcept-name right :side 'r)))	  
	;; add to (ods-o) mapping as it is in normal form
	(add-o-entry left right)	
	(return-from normalize-right left)))
     ;; ========> Unrecognized
     (t
;;;	(err "The construct (~a) is not recognized. This assertion is skipped."
;;;	     (car right))
;;;	(return-from normalize-right))
      )))
   (t
    ;; ===========> RIGHT = C
    ;; add to (ods-o) mapping as it is in normal form
    (add-o-entry left right))
   ))
;;_____________________________________________________________________________  


(defun normalize-right-w-range (right &optional left)
  "Normalize the right part of a GCI and update the mapping (ods-o)"
  (cond
   ((has-bottom right)
    ;; ===========> RIGHT = the bottom
    ;; add to (ods-o) mapping as it is in normal form
    (add-o-entry left bottom))
   ((listp right)
    (cond
     ;; ========> RIGHT = (AND C1 C2 ...)
     ((eq (car right) 'and)
      (if left
	  (dolist (conjunct (cdr right)) ;; split rhs conjunction
	    (normalize-right-w-range conjunct left))
	(progn
	  (do ((i 1 (incf i))	     
	       (n (length right)))
	      ((= i n))		 
	    ;; for each conjunct, do the following:
	    (let ((c (nth i right)))		
	      (if (listp c)
		  (setf (nth i right) (normalize-right-w-range c)))))
	  
	  (setq left (get-subconcept-name right))
	  (dolist (conjunct (cdr right)) ;; split rhs conjunction
	    (normalize-right-w-range conjunct left))
	  (return-from normalize-right-w-range left))))
     ;; ========> RIGHT = (SOME R C?)
     ((eq (car right) 'some)
      (let ((r (system-rname (cadr right)))
	    (c (caddr right))
	    (range-basket nil))
	
	(cond
	 ;; if r is defined and potentially has range restrictions
	 (r
	  ;; collect range cnames for this existential restriction, taking into account all its supe-roles with range restrictions
	  (dolist (s (cons r (r-ancestors r)))
	    (when (r-range-cname s)
	      (push (r-range-cname s) range-basket)))
	  
;;;	  (dolist (s (ont-roles-w-range))
;;;	    (when (or (eq r s)
;;;		      (member r (r-descendants s)
;;;			      :test 'eq))
;;;	      (push (r-range-cname s) range-basket)))
	  	  
	  ;; replace left <= some r c with left <= some r (and c cnames)
	  (cond

	   ;;====== emulating GCI: a <= (some r b) for a role assertion r(a,b)
	   ((and (not (listp c))
		 (> (or (system-cname c) 0) 1)
		 (c-individual? (system-cname c)))
	    (add-o-entry left right)
	    (normalize-right-w-range (cons 'and range-basket) c))
	   
	   ;;====== normal GCI: A <= (some r C)
	   
	   ;; c is a conjunction
	   ((and (listp c)
		 (eq (car c) 'and))
	    (setf (caddr right)
	      (normalize-right-w-range (append c range-basket))))
	   
	   ;; c has no 'and as its head, but we have ranges to add
	   (range-basket
	    (setf (caddr right)
	      (normalize-right-w-range (cons 'and
					     (cons c range-basket)))))
	   
	   ;; range-basket is empty, but c is still complex and needs normalizatio
	   ((listp c)
	    (setf (caddr right)
	      (normalize-right-w-range c)))
	   
	   ;; else; c is named, and we don't need to do anything
	   ))

	 ;; if r is not yet defined, it doesnot appear in an RI
	 (t
	  (when (listp c)
	    (setf (caddr right) (normalize-right-w-range c)))))
	
	;; in case of nested rhs normalization, generate a new cname for the lhs and later return it to the upper-level
	(unless left
	  (setq left (get-subconcept-name right :side 'r)))
	
	;; add to (ods-o) mapping as it is in normal form
	(add-o-entry left right)	
	(return-from normalize-right-w-range left)))
     ;; ========> Unrecognized
     (t
;;;	(err "The construct (~a) is not recognized. This assertion is skipped."
;;;	     (car right))
;;;	(return-from normalize-right-w-range))
      )))
   (t
    ;; ===========> RIGHT = C
    ;; add to (ods-o) mapping as it is in normal form
    (add-o-entry left right))
   ))
;;_____________________________________________________________________________  

(defun pushnew-o (newvalue key)
  "Push newvalue to the set of immediate consequences of key in (ods-o) only when anew, otherwise ignore"
  ;;(return-from pushnew-o)
  
  (if (ont-ttbox-active?)
      (pushnew-o-2 newvalue key)
    (pushnew newvalue (gethash key (ods-o))
	     :test 'equal)))

(defun pushnew-o-2 (newvalue key)
  "Push newvalue to a list of key in (ods2-o) when anew (compared to the permanent *o* as well, otherwise ignore"
  (unless (member newvalue (gethash key (ods-o))
		  :test 'equal)
    (pushnew newvalue (gethash key (ods2-o))
	     :test 'equal)))
;;_____________________________________________________________________________  

(defun add-o-entry (left right)
  "After normalization, GCIs in normal form are to be recorded in our data structure, ie. (ODS-O). Auxilariry data structures for heuristic queries (lazy approach) can also be updated here!!!"
  ;; ========= Process rhs
  (cond
   ;; RIGHT = (some r A)
   ((listp right)    
    (let ((r (cadr right))
	  (a (synonym-of (caddr right))))
      (add-rname r)
      (add-cname a)
      (setq r (system-rname r))	  
      (setq a (if (is-new-cname a)
		  a
		(system-cname a)))
      (setq right (cons r a))))
   ;; RIGHT = A
   (t 
    (setq right (synonym-of right))
    (add-cname right)
    (setq right (if (is-new-cname right)
		    right
		  (system-cname right)))))
  
  ;; ========= Process lhs
  (cond
   ((listp left)
    (cond
     ;; LEFT = (and A1...An)
     ((eq (car left) 'and)
      (let ((a-list nil))      
	;; filter out all synonyms and keep only their representatives
	(dolist (a (cdr left))
	  (setq a (synonym-of a))
	  (add-cname a)
	  (setq a (if (is-new-cname a)
		      a
		    (system-cname a)))
	  (pushnew a a-list))
	(if (null (cdr a-list))
	    ;; if a-list is a singleton, we store (a . right)
	    (pushnew-o right (car a-list))
	  ;; this case we store ((a1...an) . right)
	  (let ((o-entry (cons a-list right)))
	    (dolist (a a-list)
	      (pushnew-o o-entry a))))
	;; update gci reachability
;;;	(let ((r-c (if (listp right) 
;;;		       (cdr right)
;;;		     right)))
;;;	  (dolist (a a-list)
;;;	    (pushnew r-c (c-gci-rhs-cnames a))
;;;	    (pushnew a (c-gci-lhs-cnames r-c))))
	))
     ;; LEFT = (SOME R A)
     (t
      (let ((r (cadr left))
	    (a (synonym-of (caddr left))))
	;; add role name on lhs and concept name
	(add-rname r)
	(add-cname a)
	(setq r (system-rname r))
	(setq a (if (is-new-cname a)
		    a
		  (system-cname a)))
	;; store ((r . a) . right)
	(pushnew-o right (cons r a))
	;; update gci reachability
;;;	(let ((r-c (if (listp right) 
;;;		       (cdr right)
;;;		     right)))
;;;	  (pushnew r-c (c-gci-rhs-cnames a))
;;;	  (pushnew a (c-gci-lhs-cnames r-c)))
	))      
     ))
   ;; LEFT = A
   (t
    (setq left (synonym-of left))
    (add-cname left)
    (setq left (if (is-new-cname left)
		   left
		 (system-cname left)))
    ;; store (left . right)
    (pushnew-o right left)
    ;; update told subsumption if both sides are atomic
    (unless (or (listp right)
		(is-new-cname right)
		(is-new-cname left))
      (pushnew right (c-told-subsumers left) :test 'eq)
      (pushnew left (c-told-subsumees right) :test 'eq))
    ;; update gci reachability
;;;    (let ((r-c (if (listp right) 
;;;		   (cdr right)
;;;		 right)))
;;;      (pushnew r-c (c-gci-rhs-cnames left))
;;;      (pushnew left (c-gci-lhs-cnames r-c)))
    )))
;;_____________________________________________________________________________

;;_____________________________________________________________________________
;;
;; Classification and pairwise subsumption procedures 
;;_____________________________________________________________________________

(defun o-nexts-reflexive (s-cn &optional items)
  "Return union of (o-nexts (some r s-cn)) where r is reflexive. This contains all initial consequences for s-cn drawn from reflexivity"
  (setq items nil)
  
  (dolist (r (ont-reflexive-roles))
    (setq items (union items
		       (o-nexts (cons (system-rname r) s-cn))
		       :test 'equal)))
  items)
(defun o-nexts-reflexive-2 (s-cn &optional items)
  "Return union of (o-nexts-2 (some r s-cn)) (i.e. wrt ttbox) where r is reflexive. This contains all initial consequences for s-cn drawn from reflexivity"
  (setq items nil)
  
  (dolist (r (ont-reflexive-roles))
    (setq items (union items
		       (o-nexts-2 (cons (system-rname r) s-cn))
		       :test 'equal)))
  items)


(defun clear-queue-s ()
  "Clear all queues and S and set tbox state back to 'prepared"
  ;; clear obsolete entries
  ;;(setf (ods-s-counter) 0)
  (init-s-r-structures)
  (setf (ods-queue) (make-queue-struct))
  (clrhash (ods-activation-flag))
  (setf (ont-state) :prepared))

(defun init-queue-s (&optional (s-cn nil))
  "Initialize S and queue of s-cn if specified, otherwise, all system cnames"
  (if s-cn
      ;; -> only s-cn
      (activate s-cn)
    ;; -> all system cnames
    (loop 
	for s-cn 
	from (car (ods-system-cname-range))
	to (cdr (ods-system-cname-range)) do 
	  (add-label s-cn s-cn)
	  (q-enqueue s-cn (union (o-nexts s-cn)
				 (o-nexts-reflexive s-cn)
				 :test 'equal)))))
;;_____________________________________________________________________________

(defun activate-w-told (s-cn told-subsumers)
  "Activate s-cn by initializing its queue and S"
  (unless (activation-flag s-cn)
    (setf (activation-flag s-cn) t)
    
    ;; instead of adding itself only, we add all told subsumers
    (set-labels s-cn told-subsumers)
    
    ;; update queue accordingly
    (let ((new-q-items))
      (dolist (x told-subsumers)
	(setq new-q-items (union new-q-items
				 (o-nexts x)
				 :test 'eq))
	(setq new-q-items (union new-q-items		      
				 (o-nexts-reflexive x)
				 :test 'eq))
	)
      (q-enqueue s-cn new-q-items))
    ))

(defun activate (s-cn)
  "Activate s-cn by initializing its queue and S"
  (unless (activation-flag s-cn)
    (setf (activation-flag s-cn) t)
    (add-label s-cn s-cn)
    (q-enqueue s-cn (union (o-nexts s-cn)
			   (o-nexts-reflexive s-cn)
			   :test 'equal))))
;;_____________________________________________________________________________
(defun activate-both (s-cn)
  "Activate s-cn by initializing its queue and S
This is used when 2. data structures are to be populated"
  (unless (activation-flag-both s-cn)
    (setf (activation-flag-2 s-cn) t)
    (add-label-2 s-cn s-cn)
    (q-enqueue s-cn (union (o-nexts-both s-cn)
			   (union (o-nexts-reflexive s-cn)
				  (o-nexts-reflexive-2 s-cn)
				  :test 'equal)
			   :test 'equal))))
;;_____________________________________________________________________________
(defun activate-2 (s-cn)
  "Activate s-cn by initializing its queue and S
This is a varient used when 1. data structures have been fully populated by a full classification and now the information is reused in the incremental classification which in turn populates 2. data structures only"
  (unless (activation-flag-2 s-cn) ;; OR ...-flag-2??
    (setf (activation-flag-2 s-cn) t)
    (cond
     ;; s-cn is in the main TBox and was activated once while classifying it
     ((activation-flag s-cn)
      (let ((new-items nil))
	;; immediate consequences from existing s entries against ttbox
	(dolist (x (s-labels s-cn))
	  (setq new-items (union (o-nexts-reflexive-2 x)
				 (union (o-nexts-2 x)
					new-items
					:test 'equal)
				 :test 'equal)))
	
	
	(setq *t0* (get-internal-run-time))
	
	;; immediate consequences from existing r tuples against ttbox
	;; could be optimized by running through lhs of o-2
;;;	(loop 
;;;	    for r from 0 to (- (length (ods-rname-array)) 1)
;;;	    do (dolist (b (r-succs s-cn r))
;;;		 (dolist (x (s-labels b))
;;;		   (setq new-items (union (o-nexts-2 (cons r x))
;;;					  new-items
;;;					  :test 'equal)))))
	
	
	;; do for each o2 entries of the form (r . X)
	(loop as key being the hash-key of (ods2-o) do
	      (when (listp key)
		(dolist (b (r-succs s-cn (car key)))
		  (when (member (cdr key) (s-labels b) :test 'eq)
		    (setq new-items (union (o-nexts-2 key)
					   new-items
					   :test 'equal))
		    )))
	      )
	
	(incf *t* (float (/ (- (get-internal-run-time) *t0*)
			    internal-time-units-per-second))
	      )
	
	;; initialize the queue with new-items
	(when new-items
	  (q-enqueue s-cn new-items)))
      )
     ;; s-cn is new from the ttbox
     (t
      (add-label-2 s-cn s-cn)
      (q-enqueue s-cn (union (o-nexts-2 s-cn)
			     (o-nexts-reflexive-2 s-cn)
			     :test 'equal))
      )
     )))
;;_____________________________________________________________________________

(defun cache-subs (s-cn-b s-cn-a)
  "Cache known subsumption s-cn-a <= s-cn-b for unclassified concept names"
  (when (or (is-new-cname s-cn-a)
	    (not (c-classified s-cn-a)))
    (pushnew s-cn-b (c-cached-subsumers s-cn-a)))
  (when (or (is-new-cname s-cn-b)
	    (not (c-classified s-cn-b)))
    (pushnew s-cn-a (c-cached-subsumees s-cn-b))))
;;_____________________________________________________________________________

(defun remove-cache (s-cn)
  "Remove the subsumption cache for s-cn"
  (remhash s-cn (ods-cached-subsumer-table))
  (remhash s-cn (ods-cached-subsumee-table)))
;;_____________________________________________________________________________

(defun process-unsat-cname (s-cn)
  "Mark s-cn as unsatisfiable (equivalent to the bottom) and add it to the (s-labels 0), do the same for all its predecessors"
  ;; when it is not yet marked unsatisfiable (0 = *bot*)
  (when (listp (s-labels s-cn))
    ;; check if the tbox is inconsistent (top implies bottom). if tbox is inconsistent, so is abox.
    (when (member s-cn (s-top-labels))	  
      (setf (ont-tbox-consistent?) nil)
      (setf (ont-abox-consistent?) nil)    
      (add-label *bot* *top*)
      (add-label *top* *bot*)      
      (return-from process-unsat-cname))

    ;; also check if abox is inconsistent (an individual implies bottom)
    (when (and (> s-cn 1)
	       (c-individual? s-cn))
      (setf (ont-abox-consistent?) nil)
      ;;(add-label *bot* *top*)
      ;;(add-label *top* *bot*)      
      ;;(return-from process-unsat-cname)
      )
    
    ;; else inconsistency is just local, ie. unsatisfiability
    (setf (s-labels s-cn) *bot*)
    (add-label s-cn *bot*)
    (loop 
	for r from 0 to (- (length (ods-rname-array)) 1) do
	  (dolist (pred (r-preds s-cn r))	    
	    (process-unsat-cname pred)))))
;;_____________________________________________________________________________

(defun process-unsat-cname-2 (s-cn)
  "Mark s-cn as unsatisfiable (equivalent to the bottom) and add it to the (s-labels 0), do the same for all its predecessors"
  ;; when it is not yet marked unsatisfiable (0 = *bot*)
  (when (listp (s-labels-both s-cn))
    ;; check if the tbox is inconsistent (top implies bottom). if tbox is inconsistent, so is abox.
    (when (member s-cn (s-top-labels-both))	  
      (setf (ont-tbox-consistent?) nil)
      (setf (ont-abox-consistent?) nil)    
      (add-label-2 *bot* *top*)
      (add-label-2 *top* *bot*)      
      (return-from process-unsat-cname-2))

    ;; also check if abox is inconsistent (an individual implies bottom)
    (when (and (> s-cn 1)
	       (c-individual? s-cn))
      (setf (ont-abox-consistent?) nil)
      ;;(add-label *bot* *top*)
      ;;(add-label *top* *bot*)      
      ;;(return-from process-unsat-cname)
      )
    
    ;; else inconsistency is just local, ie. unsatisfiability
    (setf (s-labels-2 s-cn) *bot*)
    (add-label-2 s-cn *bot*)
    (loop 
	for r from 0 to (- (length *rname-array*) 1) do
	  (dolist (pred (r-preds-both s-cn r))	    
	    (process-unsat-cname pred)))))
;;_____________________________________________________________________________













;;_____________________________________________________________________________
;;
;; Declaration of all pointers to ontology data structures (fast referencing)
;;_____________________________________________________________________________

(defvar *cname-table* (make-hash-table :test 'equal))
(defvar *cname-array* nil)
(defvar *system-cname-range* nil)
(defvar *backup-system-cname-range* nil)
(defvar *backup-rname-array-fill-pointer* nil)
(defvar *rname-table* nil)
(defvar *rname-array* nil)
(defvar *o* nil)
(defvar *queue* nil)
(defvar *s-table* nil)
(defvar *r-pred* nil)
(defvar *r-succ* nil)
(defvar *synonym-table* nil)
(defvar *synonym-list* nil)
(defvar *gci-list1* nil)
(defvar *gci-list2* nil)
(defvar *subconcept-table* nil)
(defvar *defined-cname-list* nil)
(defvar *phierarchy-cname-list* nil)
(defvar *activation-flag* nil)
(defvar *cached-subsumer-table* nil)
(defvar *cached-subsumee-table* nil)
(defvar *s-counter* nil)
(defvar *total-s-counter* nil)
(defvar *total-subs-tests* nil)
(defvar *total-positive-subs-tests* nil)
(defvar *total-cache-accesses* nil)
(defvar *total-cache-hits* nil)
(defvar *total-classified-cnames* nil)
(defvar *2-o* nil)
(defvar *2-s-table* nil)
(defvar *2-r-pred* nil)
(defvar *2-r-succ* nil)
(defvar *2-subconcept-table* nil)
(defvar *2-activation-flag* nil)
(defvar *2-total-s-counter* nil)

(defun save-all-pointers ()
  "Set slots in current *ontology* to current values of respective pointers"
  
  )

(defun load-all-pointers--- ()
  "Set pointers to respective slots in current *ontology*"
  (setq *cname-table* 
    (internal-data-structure-cname-table (ontology-ds *ontology*)))
  (setq *cname-array* 
    (internal-data-structure-cname-array (ontology-ds *ontology*)))
  (setq *system-cname-range* 
    (internal-data-structure-system-cname-range (ontology-ds *ontology*)))
  (setq *backup-system-cname-range* 
    (internal-data-structure-backup-system-cname-range (ontology-ds *ontology*)))
  (setq *backup-rname-array-fill-pointer* 
    (internal-data-structure-backup-rname-array-fill-pointer (ontology-ds *ontology*)))
  (setq *rname-table* 
    (internal-data-structure-rname-table (ontology-ds *ontology*)))
  (setq *rname-array* 
    (internal-data-structure-rname-array (ontology-ds *ontology*)))
  (setq *o* 
    (internal-data-structure-o (ontology-ds *ontology*)))
  (setq *queue* 
    (internal-data-structure-queue (ontology-ds *ontology*)))
  (setq *s-table* 
    (internal-data-structure-s-table (ontology-ds *ontology*)))
  (setq *r-pred* 
    (internal-data-structure-r-pred (ontology-ds *ontology*)))
  (setq *r-succ* 
    (internal-data-structure-r-succ (ontology-ds *ontology*)))
  (setq *synonym-table* 
    (internal-data-structure-synonym-table (ontology-ds *ontology*)))
  (setq *synonym-list* 
    (internal-data-structure-synonym-list (ontology-ds *ontology*)))
  (setq *gci-list1* 
    (internal-data-structure-gci-list1 (ontology-ds *ontology*)))
  (setq *gci-list2* 
    (internal-data-structure-gci-list2 (ontology-ds *ontology*)))
  (setq *subconcept-table* 
    (internal-data-structure-subconcept-table (ontology-ds *ontology*)))
  (setq *defined-cname-list* 
    (internal-data-structure-defined-cname-list (ontology-ds *ontology*)))
  (setq *phierarchy-cname-list* 
    (internal-data-structure-phierarchy-cname-list (ontology-ds *ontology*)))
  (setq *activation-flag* 
    (internal-data-structure-activation-flag (ontology-ds *ontology*)))
  (setq *cached-subsumer-table* 
    (internal-data-structure-cached-subsumer-table (ontology-ds *ontology*)))
  (setq *cached-subsumee-table* 
    (internal-data-structure-cached-subsumee-table (ontology-ds *ontology*)))
  (setq *s-counter* 
    (internal-data-structure-s-counter (ontology-ds *ontology*)))
  (setq *total-s-counter* 
    (internal-data-structure-total-s-counter (ontology-ds *ontology*)))
  (setq *total-subs-tests* 
    (internal-data-structure-total-subs-tests (ontology-ds *ontology*)))
  (setq *total-positive-subs-tests* 
    (internal-data-structure-total-positive-subs-tests (ontology-ds *ontology*)))
  (setq *total-cache-accesses* 
    (internal-data-structure-total-cache-accesses (ontology-ds *ontology*)))
  (setq *total-cache-hits* 
    (internal-data-structure-total-cache-hits (ontology-ds *ontology*)))
  (setq *total-classified-cnames* 
    (internal-data-structure-total-classified-cnames (ontology-ds *ontology*)))
  (setq *2-o* 
    (internal-data-structure-2-o (ontology-ds-2 *ontology*)))
  (setq *2-s-table* 
    (internal-data-structure-2-s-table (ontology-ds-2 *ontology*)))
  (setq *2-r-pred* 
    (internal-data-structure-2-r-pred (ontology-ds-2 *ontology*)))
  (setq *2-r-succ* 
    (internal-data-structure-2-r-succ (ontology-ds-2 *ontology*)))
  (setq *2-subconcept-table* 
    (internal-data-structure-2-subconcept-table (ontology-ds-2 *ontology*)))
  (setq *2-activation-flag* 
    (internal-data-structure-2-activation-flag (ontology-ds-2 *ontology*)))
  (setq *2-total-s-counter* 
    (internal-data-structure-2-total-s-counter (ontology-ds-2 *ontology*)))
  
  )


;;;(defmacro ods-cname-table () '*cname-table*)
;;;(defmacro ods-cname-array () '*cname-array*)
;;;(defmacro ods-system-cname-range () '*system-cname-range*)
;;;(defmacro ods-backup-system-cname-range () '*backup-system-cname-range*)
;;;(defmacro ods-backup-rname-array-fill-pointer () '*backup-rname-array-fill-pointer*)
;;;(defmacro ods-rname-table () '*rname-table*)
;;;(defmacro ods-rname-array () '*rname-array*)
;;;(defmacro ods-o () '*o*)
;;;(defmacro ods-queue () '*queue*)
;;;(defmacro ods-s-table () '*s-table*)
;;;(defmacro ods-r-pred () '*r-pred*)
;;;(defmacro ods-r-succ () '*r-succ*)
;;;(defmacro ods-synonym-table () '*synonym-table*)
;;;(defmacro ods-synonym-list () '*synonym-list*)
;;;(defmacro ods-gci-list1 () '*gci-list1*)
;;;(defmacro ods-gci-list2 () '*gci-list2*)
;;;(defmacro ods-subconcept-table () '*subconcept-table*)
;;;(defmacro ods-defined-cname-list () '*defined-cname-list*)
;;;(defmacro ods-phierarchy-cname-list () '*phierarchy-cname-list*)
;;;(defmacro ods-activation-flag () '*activation-flag*)
;;;(defmacro ods-cached-subsumer-table () '*cached-subsumer-table*)
;;;(defmacro ods-cached-subsumee-table () '*cached-subsumee-table*)
;;;(defmacro ods-s-counter () '*s-counter*)
;;;(defmacro ods-total-s-counter () '*total-s-counter*)
;;;(defmacro ods-total-subs-tests () '*total-subs-tests*)
;;;(defmacro ods-total-positive-subs-tests () '*total-positive-subs-tests*)
;;;(defmacro ods-total-cache-accesses () '*total-cache-accesses*)
;;;(defmacro ods-total-cache-hits () '*total-cache-hits*)
;;;(defmacro ods-total-classified-cnames () '*total-classified-cnames*)
;;;(defmacro ods2-o () '*2-o*)
;;;(defmacro ods2-s-table () '*2-s-table*)
;;;(defmacro ods2-r-pred () '*2-r-pred*)
;;;(defmacro ods2-r-succ () '*2-r-succ*)
;;;(defmacro ods2-subconcept-table () '*2-subconcept-table*)
;;;(defmacro ods2-activation-flag () '*2-activation-flag*)
;;;(defmacro ods2-total-s-counter () '*2-total-s-counter*)


