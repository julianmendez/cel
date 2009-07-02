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
;;;; Last Modified: 2009-07-02
;;;; Note the T/C in LICENSE.txt
;;_____________________________________________________________________________


(in-package cel-system)

;;; ************** KRSS CONSTANT INTERFACE **************
(defconstant top 'TOP
  "the predefined 'top' concept")
(defconstant bot 'BOTTOM
  "the predefined 'bottom' concept (not yet available)")
(defconstant bottom 'BOTTOM
  "the predefined 'bottom' concept (not yet available)")
(defconstant and 'AND
  "the conjunction construct, eg., (AND Parent Man)")
(defconstant some 'SOME
  "the existential construct, eg., (SOME has-child Human)")
(defconstant compose 'COMPOSE
  "the role composition construct, eg., (COMPOSE cont-in part-of)")
;;;(defconstant :top top)
;;;(defconstant :and and)
;;;(defconstant :some some)
;;;(defconstant :compose compose)

;;; ************** KRSS MACRO INTERFACE **************

(defmacro define-concept (cn c)
  "to assert a concept definition, eg., 
(define-concept Man
                (AND Human (SOME has-gender Male)))"
  `(define-concept-f ',cn ',c))
;;_____________________________________________________________________________

(defmacro define-primitive-concept (cn &optional (c top))
  "to assert a primitive concept definition, eg., 
(define-primitive-concept Mother 
                          (SOME has-child Human))"
  `(define-primitive-concept-f ',cn ',c))
;;_____________________________________________________________________________

(defmacro implies (c1 c2)
  "to assert a general concept inclusion axiom, eg., 
(implies (SOME has-sister (SOME has-child Girl)) 
	 (AND Uncle (SOME has-niece TOP)))"
  `(implies-f ',c1 ',c2))
(defmacro concept-inclusion (c1 c2)
  "to assert a general concept inclusion axiom, eg., 
(concept-inclusion (SOME has-sister (SOME has-child Girl)) 
	           (AND Uncle (SOME has-niece TOP)))"
  `(implies-f ',c1 ',c2))
;;_____________________________________________________________________________

(defmacro equivalent (c1 c2 &rest c-rest)
  "to assert a concept equivalence, eg., 
(equivalent Kangaroo Masupial Macropods)
(equivalent (AND Man (SOME has-child Human))
	    (AND Parent Male))"
  `(equivalent-f (cons ',c1 (cons ',c2 ',c-rest))))
;;_____________________________________________________________________________

(defmacro disjoint (c1 c2 &rest c-rest)
  "to assert a disjoint axiom, eg., 
(disjoint Male Female Unisex)
(disjoint Child (some has-child Human)"
  `(disjoint-f (cons ',c1 (cons ',c2 ',c-rest))))
;;_____________________________________________________________________________

(defmacro domain (rn domain)
  "to assert a domain restriction, eg.,
(domain has-child Human)"
  `(domain-f ',rn ',domain))
;;_____________________________________________________________________________

(defmacro range (rn range)
  "to assert a range restriction, eg.,
(range has-daughter (and Female Human))"
  `(range-f ',rn ',range))
;;_____________________________________________________________________________

(defmacro reflexive (rn)
  "to assert reflexivity on role, eg., (reflexive part-of)"
  `(reflexive-f ',rn))
;;_____________________________________________________________________________

(defmacro transitive (rn)
  "to assert transitivity on role, eg., (transitive proper-part-of)"
  `(transitive-f ',rn))
;;_____________________________________________________________________________

(defmacro define-primitive-role (rn &key 
				    (reflexive nil)
				    (transitive nil) 
				    (parents nil) 
				    (parent top)
				    (domain top)
				    (range top)
				    (right-identity nil)
				    (left-identity nil))
  "to assert a primitive role definition, eg., 
(define-primitive-role has-daughter :parent has-child),
(define-primitive-role part-of :transitive t), and
(define-primitive-role contained-in :right-identity part-of)"
  `(define-primitive-role-f ',rn 
       :reflexive ',reflexive
       :transitive ',transitive 
       :parents ',parents 
       :parent ',parent  
       :domain ',domain
       :range ',range
       :right-identity ',right-identity
       :left-identity ',left-identity))
;;_____________________________________________________________________________

(defmacro role-inclusion (role-composite role-name)
  "to assert a simplified form of complex role inclusion of the form:
(role-inclusion (compose RN1 RN2) RN3) with RNi possibly distinct role names; this can be used to, eg., express a right-identity rule:
(role-inclusion (compose contained-in part-of) contained-in)"
  `(role-inclusion-f ',role-composite ',role-name))
;;_____________________________________________________________________________

(defmacro role-equivalent (rn1 rn2 &rest r-rest)
  "to assert an equivalent of two role names, which is essentially a pair of role hierarchy assertions in both direction. Eg.,
(role-equivalent composed-of has-component)"
  `(role-equivalent-f (cons ',rn1 (cons ',rn2 ',r-rest))))
;;_____________________________________________________________________________  

(defmacro define-primitive-individual (ind &key type)
  "to declare an individual possibly with the class it belongs to, e.g.,
(define-primitive-individual THAILAND :type TropicalCountry)"
  `(define-primitive-individual-f ',ind ',type))
;;_____________________________________________________________________________  

(defmacro same-individuals (ind1 ind2 &rest ind-rest)
  "to assert that two (or more) individuals refer to the same domain object, e.g.,
(instance EL-PROJECT DFG-BA112211-1)
(instance MENG SUNTISRIVARAPORN BOONTAWEE)"
  `(same-individuals-f (cons ',ind1 (cons ',ind2 ',ind-rest))))
;;_____________________________________________________________________________  

(defmacro different-individuals (ind1 ind2 &rest ind-rest)
  "to assert that two (or more) individuals refer to different domain objects, e.g.,
(instance EL-PROJECT TONES-PROJECT)
(instance BAADER CARSTEN MENG JULIAN)"
  `(different-individuals-f (cons ',ind1 (cons ',ind2 ',ind-rest))))
;;_____________________________________________________________________________  

(defmacro instance (ind c)
  "to assert an instance of a concept, e.g., 
(instance JOHN Man)"
  `(instance-f ',ind ',c))
;;_____________________________________________________________________________  

(defmacro related (ind1 ind2 rn)
  "to assert relationship between two individuals (role assertion), e.g., 
(related JOHN ANNA loves)"
  `(related-f ',ind1 ',ind2 ',rn))
;;_____________________________________________________________________________  


;;_____________________________________________________________________________
;;
;;  MACROS FOR OPERATION ON AND QUERIES ABOUT ONTOLOGY (TBOX,ABOX)
;;_____________________________________________________________________________


(defmacro create-tbox (&optional uri)
  "to create a new ontology"
  `(create-tbox-f ',uri))
(defmacro create-ontology (&optional uri)
  "to create a new ontology"
  `(create-tbox-f ',uri))
;;_____________________________________________________________________________

(defmacro clear-tbox (&optional uri)
  "to clear the specified TBox, default to the current"
  `(clear-tbox-f ',uri))
(defmacro clear-ontology (&optional uri)
  "to clear the specified TBox, default to the current"
  `(clear-tbox-f ',uri))
;;_____________________________________________________________________________

(defmacro release-tbox (&optional uri)
  "to release the ontology designated by a given URI"
  `(release-tbox-f ',uri))
(defmacro release-ontology (&optional uri)
  "to release the ontology designated by a given URI"
  `(release-tbox-f ',uri))
;;_____________________________________________________________________________

(defmacro release-all-tboxes ()
  "to release all ontologies in the repository"
  `(release-all-cel-ontologies))
(defmacro release-all-ontologies ()
  "to release all ontologies in the repository"
  `(release-all-cel-ontologies))
;;_____________________________________________________________________________

(defmacro restore-tbox (uri)
  "to restore the ontology designated by a given URI"
  `(restore-tbox-f ',uri))
(defmacro restore-ontology (uri)
  "to restore the ontology designated by a given URI"
  `(restore-tbox-f ',uri))
;;_____________________________________________________________________________

(defmacro duplicate-tbox (uri)
  "to duplicate the ontology designated by a given URI"
  `(duplicate-tbox-f ',uri))
(defmacro duplicate-ontology (uri)
  "to duplicate the ontology designated by a given URI"
  `(duplicate-tbox-f ',uri))
;;_____________________________________________________________________________

(defmacro load-tbox (file-name &key clear)
  "to load and preprocess an input TBox, eg. (load-tbox \"med.tbox\")"
  `(load-tbox-f ,file-name ,clear))
(defmacro load-ontology (file-name &key clear)
  "to load and preprocess an input TBox, eg. (load-tbox \"med.tbox\")"
  `(load-tbox-f ,file-name ,clear))
;;_____________________________________________________________________________

(defmacro add-axiom (axiom)
  "to add a new axiom into the current ontology, eg. (add-axiom (implies A B))"
  `(add-axiom-f ',axiom))
(defmacro add-axioms (axioms)
  "to add a list of new axioms into the current ontology"
  `(add-axioms-f ',axioms))
;;_____________________________________________________________________________

(defmacro classify-tbox ()
  "to classify the current TBox"
  `(classify-tbox-f))
(defmacro classify-ontology ()
  "to classify the current TBox"
  `(classify-tbox-f))
;;_____________________________________________________________________________

(defmacro reclassify-tbox ()
  "to reclassify the current TBox"
  `(reclassify-tbox-f))
(defmacro reclassify-ontology ()
  "to reclassify the current TBox"
  `(reclassify-tbox-f))
;;_____________________________________________________________________________

(defmacro tbox-prepared? ()
  "to query if the current TBox is prepared and ready"
  `(q-tbox-prepared-f))
(defmacro ontology-prepared? ()
  "to query if the current TBox is been prepared and ready"
  `(q-tbox-prepared-f))
;;_____________________________________________________________________________

(defmacro tbox-classified? ()
  "to query if the current TBox has been classified"
  `(q-tbox-classified-f))
(defmacro ontology-classified? ()
  "to query if the current TBox has been classified"
  `(q-tbox-classified-f))
;;_____________________________________________________________________________

(defmacro tbox-consistent? ()
  "to query if the current TBox is consistent, ie. it has a model"
  `(q-tbox-consistent-f))
(defmacro ontology-consistent? ()
  "to query if the current TBox is consistent, ie. it has a model"
  `(and (q-tbox-consistent-f)
	(q-abox-consistent-f)))	
;;_____________________________________________________________________________

(defmacro abox-consistent? ()
  "to query if the current ABox is consistent, ie. it has a common model together with the TBox"
  `(q-abox-consistent-f))
;;_____________________________________________________________________________

;;_____________________________________________________________________________
;;
;;  MACROS FOR QUERIES ABOUT TTBox (Temporary & Incremental)
;;_____________________________________________________________________________

(defmacro activate-ttbox ()
  "to activate the incremental TBox (thus enable incremental classification)"
  `(if (ont-state? (:classified :taxonomized))
       (activate-ttbox-f)
     :tbox-state-error))
(defmacro deactivate-ttbox ()
  "to deactivate the incremental TBox (thus disable incremental classification)"
  `(deactivate-ttbox-f))
(defmacro clear-ttbox ()
  "to clear the incremental TBox"
  `(clear-ttbox-f))
(defmacro classify-ttbox (&key commit)
  "to classify the incremental TBox against the main TBox"
  `(classify-ttbox-f ,commit))
(defmacro commit-ttbox ()
  "to commit the incremental TBox as part of the main TBox"
  `(commit-ttbox-f))

(defmacro ttbox-active? ()
  "to query if the incremental TBox is active"
  `(q-ttbox-active-f))
(defmacro ttbox-cleared? ()
  "to query if the incremental TBox is cleared"
  `(q-ttbox-cleared-f))
(defmacro ttbox-prepared? ()
  "to query if the incremental TBox is prepared and ready"
  `(q-ttbox-prepared-f))
(defmacro ttbox-classified? ()
  "to query if the incremental TBox is classified against the main TBox"
  `(q-ttbox-classified-f))

;;_____________________________________________________________________________
;;
;;  MACROS FOR QUERIES ABOUT ROLES
;;_____________________________________________________________________________

(defmacro concept? (cn)
  "to query if the input CN is a concept name"
  `(q-is-concept-f ',cn))
;;_____________________________________________________________________________

(defmacro all-concepts ()
  "to retrieve all concept names occurring in the TBox"
  `(q-all-concepts-f))
;;_____________________________________________________________________________

(defmacro all-unsatisfiable-concepts ()
  "to retrieve all unsatisfiable concept names, i.e., cannot have any instances"
  `(q-all-unsatisfiable-concepts-f))
;;_____________________________________________________________________________

(defmacro concept-satisfiable? (cn)
  "to query if a concept name is satisfiable, ie. it has an instance"
  `(q-concept-satisfiable-f ',cn))
(defmacro satisfiable? (cn)
  "to query if a concept name is satisfiable, ie. it has an instance"
  `(q-concept-satisfiable-f ',cn))
;;_____________________________________________________________________________

(defmacro concept-subsumes? (cn1 cn2)
  "to query subsumption relationship between two concept names"
  `(q-concept-subsumes-f ',cn1 ',cn2))
(defmacro subsumes? (cn1 cn2)
  "to query subsumption relationship between two concept names"
  `(q-concept-subsumes-f ',cn1 ',cn2))
;;_____________________________________________________________________________

(defmacro concept-implies? (cn1 cn2)
  "to query implication relationship between two concept names"
  `(q-concept-implies-f ',cn1 ',cn2))
(defmacro implies? (cn1 cn2)
  "to query implication relationship between two concept names"
  `(q-concept-implies-f ',cn1 ',cn2))
;;_____________________________________________________________________________

(defmacro concept-equivalent? (cn1 cn2)
  "to query equivalence relationship between two concept names"  
  `(q-concept-equivalent-f ',cn1 ',cn2))
(defmacro equivalent? (cn1 cn2)
  "to query equivalence relationship between two concept names"  
  `(q-concept-equivalent-f ',cn1 ',cn2))
;;_____________________________________________________________________________

(defmacro concept-disjoint? (cn1 cn2)
  "to query disjointness between two concept names"  
  `(q-concept-disjoint-f ',cn1 ',cn2))
(defmacro disjoint? (cn1 cn2)
  "to query disjointness between two concept names"  
  `(q-concept-disjoint-f ',cn1 ',cn2))
;;_____________________________________________________________________________

;;_____________________________________________________________________________

(defmacro parents (cn &key (make-eclass nil))
  "to get all parent concepts of the input concept name CN"
  `(q-parents-f ',cn :make-eclass ',make-eclass))
;;_____________________________________________________________________________

(defmacro children (cn &key (make-eclass nil))
  "to get all child concepts of the input concept name CN"
 `(q-children-f ',cn :make-eclass ',make-eclass))
;;_____________________________________________________________________________

(defmacro ancestors (cn &key (make-eclass nil))
  "to get all concept names subsuming the input concept name CN"
  `(q-ancestors-f ',cn :make-eclass ',make-eclass))
(defmacro super-concepts (cn &key (make-eclass nil))
  "to get all concept names subsuming the input concept name CN"
  `(q-ancestors-f ',cn :make-eclass ',make-eclass))
;;_____________________________________________________________________________

(defmacro descendants (cn &key (make-eclass nil))
  "to get all concept names being subsumed by the input concept name CN"
  `(q-descendants-f ',cn :make-eclass ',make-eclass))
(defmacro sub-concepts (cn &key (make-eclass nil))
  "to get all concept names being subsumed by the input concept name CN"
  `(q-descendants-f ',cn :make-eclass ',make-eclass))
;;_____________________________________________________________________________

(defmacro equivalents (cn &key (unknown-as-primitive nil))
  "to get all equivalent concepts (incl. synonyms) of the input concept name CN"
  `(q-equivalents-f ',cn ',unknown-as-primitive))
;;_____________________________________________________________________________

(defmacro extract-module (sig &key (out nil))
  "to extract the reachability-based module for the signature SIG, e.g.
(extract-module (Disease has-location Heart))"
  `(extract-module-f ',sig ,out))

(defmacro extract-c-module (cn &key (out nil))
  "to extract the reachability-based module for the concept name CN, e.g.
(extract-c-module Pericarditis)"
  `(extract-c-module-f ',cn ,out))
;;_____________________________________________________________________________

(defmacro extract-mina (cn1 cn2 &key (out nil))
  "to extract a MinA (minimal axiom set) that has the subsumption CN1 [= CN2, given that the subsumption holds w.r.t. the ontology. E.g., to get a justification for \"Pericarditis [= HeartDisease\", enter:
(extract-mina Pericarditis HeartDisease)"
  `(extract-mina-f ',cn1 ',cn2 ,out))
;;_____________________________________________________________________________

(defmacro extract-all-minas (cn1 cn2 &key (out t))
  "to extract a MinA (minimal axiom set) that has the subsumption CN1 [= CN2, given that the subsumption holds w.r.t. the ontology. E.g., to get a justification for \"Pericarditis [= HeartDisease\", enter:
(extract-mina Pericarditis HeartDisease)"
  `(extract-all-minas-f ',cn1 ',cn2 ,out))
;;_____________________________________________________________________________




;;_____________________________________________________________________________
;;
;;  MACROS FOR QUERIES ABOUT ROLES
;;_____________________________________________________________________________

(defmacro role? (rn)
  "to query if the input RN is a role name"
  `(q-is-role-f ',rn))
;;_____________________________________________________________________________

(defmacro all-roles ()
  "to retrieve all role names occurring in the TBox"
  `(q-all-roles-f))
;;_____________________________________________________________________________

(defmacro role-subsumes? (rn1 rn2)
  "to query subsumption relationship between two role names"
  `(q-role-subsumes-f ',rn1 ',rn2))
;;_____________________________________________________________________________

(defmacro role-equivalent? (rn1 rn2)
  "to query equivalent relationship between two role names"
  `(q-role-equivalent-f ',rn1 ',rn2))
;;_____________________________________________________________________________

(defmacro role-implies? (rn1 rn2)
  "to query implication relationship between two role names"  
  `(q-role-implies-f ',rn1 ',rn2))
;;_____________________________________________________________________________

(defmacro transitive? (rn)
  "to query the transitivity of the input role name RN"
  `(q-transitive-f ',rn))
;;_____________________________________________________________________________

(defmacro reflexive? (rn)
  "to query the reflexivity of the input role name RN"
  `(q-reflexive-f ',rn))
;;_____________________________________________________________________________

(defmacro parent-roles (rn)
  "to get all parent roles of the input concept name RN"
  `(q-parent-roles-f ',rn))
;;_____________________________________________________________________________

(defmacro child-roles (rn)
  "to get all child roles of the input concept name RN"
 `(q-child-roles-f ',rn))
;;_____________________________________________________________________________

(defmacro super-roles (rn)
  "to get all role names subsuming the input role name RN"
  `(q-super-roles-f ',rn))
;;_____________________________________________________________________________

(defmacro sub-roles (rn)
  "to get all role names being subsumed by the input role name RN"
  `(q-sub-roles-f ',rn))
;;_____________________________________________________________________________

;;_____________________________________________________________________________
;;
;;  MACROS FOR QUERIES ABOUT INDIVIDUALS
;;_____________________________________________________________________________

(defmacro individual? (ind)
  "to query if the name is an individual name"
  `(q-is-individual-f ',ind))
;;_____________________________________________________________________________

(defmacro all-individuals ()
  "to retrieve all individual names occurring in the ABox"
  `(q-all-individuals-f))
;;_____________________________________________________________________________

(defmacro all-unsatisfiable-individuals ()
  "to retrieve all unsatisfiable individuals, i.e., having no types"
  `(q-all-unsatisfiable-individuals-f))
;;_____________________________________________________________________________

(defmacro instance? (ind cn)
  "to query if the individual is an instance of the concept name"
  `(q-instance-f ',ind ',cn))
;;_____________________________________________________________________________

(defmacro individual-direct-types (ind  &key (make-eclass nil))
  "to query the direct (i.e., most specific) types of the individual"
  `(q-individual-direct-types-f ',ind :make-eclass ',make-eclass))
;;_____________________________________________________________________________

(defmacro individual-types (ind &key (make-eclass nil))
  "to query the types of the individual"
  `(q-individual-types-f ',ind :make-eclass ',make-eclass))
;;_____________________________________________________________________________

(defmacro concept-direct-instances (cn)
  "to query all direct instances belonging to the concept name"
  `(q-concept-direct-instances-f ',cn))
;;_____________________________________________________________________________

(defmacro concept-instances (cn)
  "to query all instances belonging to the concept name"
  `(q-concept-instances-f ',cn))
;;_____________________________________________________________________________



(defun output-subsumption (&key (file-name nil)
				(keyword "subsumes"))
  "to print out all subsumption relationships either to file or console in the form:
(subsumes CN1 CN2) with CN1 and CN2 concept names occurring in the TBox"
  (cond
   ((not (q-tbox-classified-f))
    (err "TBox has to be first classified"))
;;;   ((eq *classification-mode* 1)
;;;    (err "This feature is not available. Use 'output-taxonomy'!"))
   (t
    (msg "This feature is obsolete!")
    (if file-name
        (with-open-file (output-stream file-name		   
		         :direction :output 
		         :if-exists :supersede)
           (format output-stream ";; Trivial subsumption relationships involving top and bottom are omitted")
           (output-subsumption-f output-stream keyword))
       (output-subsumption-f t keyword)))))
;;_____________________________________________________________________________

(defun output-imp-sets (&key (file-name nil)
			     (keyword "imp-set"))
  "to print out the implication sets either to file or console in the form:
(imp-set CN S) where CN is a concept name occurring in the TBox and S is its computed implication set"
  (cond
   ((not (q-tbox-classified-f))
    (err "TBox has to be first classified"))
;;;   ((eq *classification-mode* 1)
;;;    (err "This feature is not available. Use 'output-supers'"))
   (t
    (msg "For complete sets (but less succinct), use 'output-supers'!")
    (if file-name
      (with-open-file (output-stream file-name		   
		       :direction :output 
		       :if-exists :supersede)        
	(output-imp-sets-f output-stream keyword))
    (output-imp-sets-f t keyword)))))
;;_____________________________________________________________________________

(defun output-supers (&key (file-name nil)
			   (keyword "supers"))
  "to print out the sets of *all* super concepts either to file or console in the form: (supers CN S) where CN is a concept name occurring in the TBox (including those synonyms) and S is its computed super concepts"
  (cond
   ((not (q-tbox-classified-f))
    (err "TBox has to be first classified"))
   (t
    (if file-name
      (with-open-file (output-stream file-name		   
		       :direction :output 
		       :if-exists :supersede)        
	(output-supers-f output-stream keyword))
      (output-supers-f t keyword)))))
;;_____________________________________________________________________________

(defmacro output-hierarchy (&key (file-name nil) 
				 (root top)
				 (depth nil)
				 (realization nil))
  `(output-hierarchy-f ,file-name ,(system-cname root) ,depth ,realization))
;;_____________________________________________________________________________

(defun output-taxonomy (&key (file-name nil)
			     (keyword "taxonomy"))
  "to print out the hasse diagram either to file or console in the form:
(taxonomy CN :parents PS :children CS) or (taxonomy CN :equivalent CN0) where CN and CN0 are concept names occurring in the TBox and PS and CS are sets of parent and child concept names, respectively [available only in *classification-mode* 1]"
  (request-hasse)

  (cond
   ((not (q-tbox-classified-f))
    (err "TBox has to be first classified"))
;;;   ((not (eq *classification-mode* 1))
;;;    (err "This feature is available only after classification in mode 1"))
   (t ;; otherwise, ready to give output
    (if file-name
      (with-open-file (output-stream file-name		   
		       :direction :output 
		       :if-exists :supersede)
	(output-taxonomy-f output-stream keyword))
    (output-taxonomy-f t keyword)))))
;;_____________________________________________________________________________
 
(defun output-synonyms (&key (file-name nil)
			     (keyword "synonyms"))
  "to print out the equivalence classes of unified concept names and their representative either to file or console in the form:
(synonyms CN SS) with SS an equivalence classes and CN its representative"
  (if file-name
      (with-open-file (output-stream file-name		   
		       :direction :output 
		       :if-exists :supersede)
	(output-synonyms-f output-stream keyword))
    (output-synonyms-f t keyword)))
;;_____________________________________________________________________________

(defmacro quit ()
  "to exit CEL interactive interface"
  (cel-dig:shutdown-server)
  (excl:exit))
;;_____________________________________________________________________________



;;; ************** KRSS FUNCTION INTERFACE **************

(defun define-concept-f (cn c)
  (unless (ont-state? (:cleared :i-cleared))
    (return-from define-concept-f ':tbox-state-error))
  
  (cond   
   ((or (listp cn)
	(eq cn top)
	(eq cn bottom))
    ;; cn must be a (defined) concept name
    (err "Syntax: lhs of define-concepts must be a concept name")
    (return-from define-concept-f ':named-concept))
   ((not (el-concept c))
    ;; c must be an EL concept
    (err "Syntax: \"~A\" is not a well-formed EL concept" c)
    (return-from define-concept-f ':malformed-concept))
   ((not (listp c))
    ;; when c is atomic
    (add-synonym cn c))
   (t
    ;; when the axiom is well-formed
    (incf (ont-n-cdefs))
    (setq c (and-expansion c))
    (add-cname-later cn 
	       :status 'defined
	       :definition (copy-tree c))
    ;; Optimization of using a defined name during normalization
    ;; To be disabled for SNOMED CT
    ;;(set-subconcept-name cn c)
    (push-gci-list1 cn (copy-tree c))
    (push-gci-list1 c cn)))
  (verbose "C"))
;;_____________________________________________________________________________

(defun define-primitive-concept-f (cn c) 
  (unless (ont-state? (:cleared :i-cleared))
    (return-from define-primitive-concept-f ':tbox-state-error))

  (cond
   ((or (listp cn)
	(eq cn top)
	(eq cn bottom))
    ;; cn must be a (defined) concept name
    (err "Syntax: lhs of define-primitive-concept must be a concept name")
    (return-from define-primitive-concept-f ':named-concept))
   ((not (el-concept c))
    ;; c must be an EL concept
    (err "Syntax: \"~A\" is not a well-formed EL concept" c)
    (return-from define-primitive-concept-f ':malformed-concept))
   (t
    ;; when the axiom is well-formed
    (incf (ont-n-pcdefs))
    (setq c (and-expansion c))
    (add-cname-later cn 
		     :status 'primitive
		     :definition (copy-tree c))
    (push-gci-list1 cn (if c
			   c
			 'top))))
  (verbose "P"))
;;_____________________________________________________________________________

(defun implies-f (c1 c2)
  (unless (ont-state? (:cleared :i-cleared))
    (return-from implies-f ':tbox-state-error))
  
  (cond
   ((not (el-concept c1))
    ;; c1 must be an EL concept
    (err "Syntax: \"~A\" is not a well-formed EL concept" c1)
    (return-from implies-f ':malformed-concept))
   ((not (el-concept c2))
    ;; c2 must be an EL concept
    (err "Syntax: \"~A\" is not a well-formed EL concept" c2)
    (return-from implies-f ':malformed-concept))   
   ((listp c1)
    ;; when this is a real GCI
    (incf (ont-n-gcis))    
    (setf (ont-tbox-general?) t)
    (push-gci-list1 (and-expansion c1)
		    (and-expansion c2)))
   (t
    ;; when this can be treated as a PCDef
    (incf (ont-n-pcdefs))
    (setq c2 (and-expansion c2))
    (add-cname-later c1
		     :status 'primitive
		     :definition (copy-tree c2))
    (push-gci-list1 c1 c2)))
  (verbose "I"))
;;_____________________________________________________________________________

(defun equivalent-f (c-list &optional (n (length c-list) ))
  (unless (ont-state? (:cleared :i-cleared))
    (return-from equivalent-f ':tbox-state-error))
  
  (cond
   ((< (length c-list) 2)
    (err "Syntax: equivalent expects at least 2 arguments")
    (return-from equivalent-f :malformed-axiom))
   
;;;   ((= (length c-list) 2)
;;;    (binary-equivalent-f (car c-list)
;;;			 (cadr c-list)))

   (t
    (dolist (c c-list)
      (unless (el-concept c)
      (err "Syntax: \"~A\" is not a well-formed EL concept" c)
      (return-from equivalent-f ':malformed-concept)))      

    ;; 0th element is equivalent to every other
    (loop for i from 1 to (- n 1) do
	  (binary-equivalent-f (nth i c-list)
			       (nth 0 c-list))
	  )
    ))
  (verbose "E")
  )
  
(defun binary-equivalent-f (c1 c2)
  
  (cond
   ((not (el-concept c1))
    ;; c1 must be an EL concept
    (err "Syntax: \"~A\" is not a well-formed EL concept" c1)
    (return-from binary-equivalent-f ':malformed-concept))
   ((not (el-concept c2))
    ;; c2 must be an EL concept
    (err "Syntax: \"~A\" is not a well-formed EL concept" c2)
    (return-from binary-equivalent-f ':malformed-concept))
   ((and (listp c1) (listp c2))
    ;; both c1 and c2 are complex     
    (incf (ont-n-gcis) 2)  
    (setf (ont-tbox-general?) t)
    (setq c1 (and-expansion c1))
    (setq c2 (and-expansion c2))
    (push-gci-list1 (copy-tree c1)
		    (copy-tree c2))
    (push-gci-list1 c2 c1))
   ;; c1 is complex, but c2 is not
   ((listp c1)
    (incf (ont-n-cdefs))
    (setq c1 (and-expansion c1))
    (add-cname-later c2 
		     :status 'defined
		     :definition (copy-tree c1))
    (push-gci-list1 c2 (copy-tree c1))
    (push-gci-list1 c1 c2))
   ;; c1 is not complex, but c2 is      
   ((listp c2)        
    (incf (ont-n-cdefs))
    (setq c2 (and-expansion c2))
    (add-cname-later c1
		     :status 'defined
		     :definition (copy-tree c2))
    (push-gci-list1 c1 (copy-tree c2))
    (push-gci-list1 c2 c1))
   ;; neither c1 nor c2 is complex
   ((or (eq c1 top) (eq c2 top) (eq c1 bottom) (eq c2 bottom))
    (push-gci-list1 c1 c2)
    (push-gci-list1 c2 c1))
   (t
    (add-synonym c1 c2)))
  (verbose "E")
  )
;;_____________________________________________________________________________

(defun disjoint-f (c-list &optional (n (length c-list)) )
  (unless (ont-state? (:cleared :i-cleared))
    (return-from disjoint-f ':tbox-state-error)) 
  
  (dolist (c c-list)
    (unless (el-concept c)
      (err "Syntax: \"~A\" is not a well-formed EL concept" c)
      (return-from disjoint-f ':malformed-concept)))
  
  (loop for i from 0 to (- n 2) do
	(loop for j from (+ i 1) to (- n 1) do
	      (incf (ont-n-gcis))
	      (push-gci-list1 `(and ,(copy-tree (nth i c-list))
				    ,(copy-tree (nth j c-list)))
			      bottom)
	      ))
  (verbose "D"))
;;_____________________________________________________________________________

(defun domain-f (rn domain)
  (unless (ont-state? :cleared)
    (return-from domain-f :tbox-state-error)) 
  
  (unless (el-concept domain)
    (err "Syntax: the domain \"~A\" is not a well-formed EL concept" domain)
    (return-from domain-f :malformed-concept))
  ;; process role domain
  (unless (eq domain 'top)
    (add-rname rn)
    (setq domain (and-expansion domain))
    (incf (ont-n-gcis))    
    (setf (ont-tbox-general?) t)
    (pushnew rn (ont-roles-w-domain)
	     :test 'eq)
    ;; keep the domain (krss)
    (setf (r-domain (system-rname rn)) domain))
  )
;;_____________________________________________________________________________

(defun range-f (rn range)
  (unless (ont-state? :cleared)
    (return-from range-f :tbox-state-error))   
  
  (unless (el-concept range)
    (err "Syntax: the domain \"~A\" is not a well-formed EL concept" range)
    (return-from range-f :malformed-concept))  
  ;; process role range
  (unless (eq range 'top)
    (add-rname rn)
    (setq range (and-expansion range))
    (pushnew rn (ont-roles-w-range)
	     :test 'eq)
    ;; keep the range (krss)
    (setf (r-range (system-rname rn)) range)
    
    (let ((range-cn (get-subconcept-name range :side 'b)))
      ;; create a new cname for range and save it
      (setf (r-range-cname (system-rname rn)) range-cn)
      (push-gci-list1 range-cn (copy-tree range))
      (push-gci-list1 (copy-tree range) range-cn)))
  )
;;_____________________________________________________________________________

(defun reflexive-f (rn)
  (unless (ont-state? :cleared)
    (return-from reflexive-f ':tbox-state-error))     
  
  (add-rname rn :reflexive t)
  )
;;_____________________________________________________________________________

(defun transitive-f (rn)
  (unless (ont-state? :cleared)
    (return-from transitive-f ':tbox-state-error))   
  
  (add-rname rn :transitive t)  
  (add-ri rn rn rn)
  )
;;_____________________________________________________________________________

(defun define-primitive-role-f (rn &key (reflexive nil)
					(transitive nil) 
					(parents nil) 
					(parent nil)
					(domain nil)
					(range nil)
					(right-identity nil)
					(left-identity nil))
  (unless (ont-state? :cleared)
    (return-from define-primitive-role-f ':tbox-state-error))
  
  (when (or (and rn (listp rn))
	    (and parent (listp parent))
	    (and right-identity (listp right-identity))
	    (and left-identity (listp left-identity)))
    (err "Syntax: role name must be atomic")
    (print `(,parent ,right-identity ,left-identity))
    (return-from define-primitive-role-f ':named-role))
  (if (listp parents)
      (dolist (p parents)
	(when (listp p)
	  (err "Syntax: role name must be atomic")
	  (return-from define-primitive-role-f ':named-role)))
    (progn
      (err "Syntax: parents is expected to be a list of role names")
      (return-from define-primitive-role-f ':macro-syntax-error)))
  
  (incf (ont-n-ris))
  
  (progn
    (add-rname rn 
	       :reflexive reflexive
	       :transitive transitive)
    ;; process role transitivity
    (when transitive
      (add-ri rn rn rn))
    ;; process right-indentity
    (when right-identity
      (add-rname right-identity)
      (add-ri rn right-identity rn))
    ;; process left-indentity
    (when left-identity
      (add-rname left-identity)
      (add-ri left-identity rn rn))
    ;; process role hierarchy
    (when (and parent
	       (not (eql parent 'top)))
      (pushnew parent parents
	       :test 'eq))
    (dolist (each-parent parents)
      (add-rname each-parent)
      (add-rh rn each-parent))    
    ;; assert domain and range restrictions
    (when domain
      (domain-f rn domain))
    (when range
      (range-f rn range))
    )
  (verbose "R"))
;;_____________________________________________________________________________

(defun role-inclusion-f (rc rn)
  (unless (ont-state? :cleared)
    (return-from role-inclusion-f ':tbox-state-error)) 
  (when (listp rn)
    (err "Syntax: rhs of role-implies must be atomic")
    (return-from role-inclusion-f ':named-role))
  
  (incf (ont-n-ris))

  (cond
   ((not (listp rc))
    ;; role hierarchy
    (add-rname rc)
    (add-rname rn)
    (add-rh rc rn))
   ((and (= 3 (length rc))
	 (eq (car rc) 'compose))
    ;; role inclusion is expected now
    (let ((r (cadr rc))
	  (s (caddr rc)))
      (add-rname r)
      (add-rname s)
      (add-rname rn)
      (add-ri r s rn)))
   (t    
    (err "Syntax: lhs of role-implies must be atomic or of the form (compose rn1 rn2)")
    (return-from role-inclusion-f ':malformed-role)
    ))
  (verbose "R"))
;;_____________________________________________________________________________

(defun role-equivalent-f (r-list &optional (n (length r-list) ))
  (unless (ont-state? :cleared)
    (return-from role-equivalent-f ':tbox-state-error)) 
  
  (cond
   ((< (length r-list) 2)
    (err "Syntax: role-equivalent expects at least 2 arguments")
    (return-from role-equivalent-f :malformed-axiom))
   
   (t
    (dolist (rn r-list)
      (when (listp rn)
	(err "Syntax: \"~A\" is not an admissible role name" rn)
	(return-from role-equivalent-f ':malformed-role))
      )
    
    ;; adding all role names
    (loop for i from 0 to (- n 1) do
	  (add-rname (nth i r-list)))
    ;; adding RH axioms forming a loop
    (incf (ont-n-ris) n)
    (loop for i from 0 to (- n 2) do
	  (add-rh (nth i r-list) 
		  (nth (+ i 1) r-list)
		  ))
    (add-rh (nth (- n 1) r-list)
	    (nth 0 r-list))    
    ))
  (verbose "R")
  )

(defun binary-role-equivalent-f (rn1 rn2)
  (unless (ont-state? :cleared)
    (return-from binary-role-equivalent-f ':tbox-state-error))   
  
  (when (listp rn1)
    (err "Syntax: \"~A\" is not an admissible role name" rn1)
    (return-from binary-role-equivalent-f ':named-role))
  (when (listp rn2)
    (err "Syntax: \"~A\" is not an admissible role name" rn2)
    (return-from binary-role-equivalent-f ':named-role))
  
  (incf (ont-n-ris) 2)
  
  (add-rname rn1)
  (add-rname rn2)
  (add-rh rn1 rn2)
  (add-rh rn2 rn1)
  
  (verbose "R"))
;;_____________________________________________________________________________

(defun same-individuals-f (ind-list &optional (n (length ind-list)) )
  (unless (ont-state? (:cleared :i-cleared))
    (return-from same-individuals-f ':tbox-state-error))
  
  (cond
   ((< (length ind-list) 2)
    (err "Syntax: same-individuals expects at least 2 arguments")
    (return-from same-individuals-f :malformed-axiom))
   
   (t
    (dolist (ind ind-list)
      (when (listp ind)
	(err "Syntax: \"~A\" is not an admissible individual" ind)
	(return-from same-individuals-f :malformed-individual)))

    ;;(dolist (ind ind-list)
    ;;  (add-cname-later ind
    ;;		       :individual? t))

    (incf (ont-n-cas) n)
    ;; 0th element is equivalent to every other
    (loop for i from 1 to (- n 1) do
	  (add-ind-synonym (nth i ind-list)
			   (nth 0 ind-list)))
    ))
  (verbose "A")
  )
;;_____________________________________________________________________________

(defun different-individuals-f (ind-list &optional (n (length ind-list)) )
  (unless (ont-state? (:cleared :i-cleared))
    (return-from different-individuals-f ':tbox-state-error))
  
  (cond
   ((< (length ind-list) 2)
    (err "Syntax: different-individuals expects at least 2 arguments")
    (return-from different-individuals-f :malformed-axiom))
   
   (t
    (dolist (ind ind-list)
      (when (listp ind)
	(err "Syntax: \"~A\" is not an admissible individual" ind)
	(return-from different-individuals-f :malformed-individual)))

    (dolist (ind ind-list)
      (add-cname ind
		 :individual? t))

    (loop for i from 0 to (- n 2) do
	  (loop for j from (+ i 1) to (- n 1) do
		(incf (ont-n-cas))
		(push-gci-list1 `(and ,(nth i ind-list)
				      ,(nth j ind-list))
				bottom)
		))
    ))
  (verbose "A")
  )
;;_____________________________________________________________________________

(defun define-primitive-individual-f (ind c)
  (instance-f ind c))

(defun instance-f (ind c)
  (unless (ont-state? (:cleared :i-cleared))
    (return-from instance-f ':tbox-state-error))
  
  (cond
   ((or (listp ind)
	(eq ind top)
	(eq ind bottom))
    ;; ind must be an individual name
    (err "Syntax: an individual must not be complex nor used as concept name")
    (return-from instance-f ':named-concept))
   ((not (el-concept c))
    ;; c must be an EL concept
    (err "Syntax: \"~A\" is not a well-formed EL concept" c)
    (return-from instance-f ':malformed-concept))
   (t
    ;; alright!!
    (setq c (and-expansion c))
    (add-cname ind 
	       :status 'primitive
	       :definition (copy-tree c)
	       :individual? t)
    (push-gci-list1 ind (if c
			    c
			  'top))
    (pushnew ind (ont-individuals)
	     :test 'eq)
    (incf (ont-n-cas))))
  
  (verbose "A"))
;;_____________________________________________________________________________

(defun related-f (ind1 ind2 rn)
  (unless (ont-state? (:cleared :i-cleared))
    (return-from related-f ':tbox-state-error))
  
  (cond
   ((or (listp ind1)
	(eq ind1 top)
	(eq ind1 bottom)
	(listp ind2)
	(eq ind2 top)
	(eq ind2 bottom))		
    ;; ind must be an individual name
    (err "Syntax: an individual must not be complex nor used as concept name")
    (return-from related-f ':named-concept))
   ((listp rn)
    ;; rn must be a role name
    (err "Syntax: individuals can be related only by role names")
    (return-from related-f ':named-concept))
   (t
    ;; alright!!!      
    (pushnew ind1 (ont-individuals)
	     :test 'eq)
    (pushnew ind2 (ont-individuals)
	     :test 'eq)  
    (add-cname ind1 
	       :status 'primitive
	       :individual? t)
    (add-cname ind2
	       :status 'primitive
	       :individual? t)
    (push-gci-list1 ind1 `(some ,rn ,ind2))
    (incf (ont-n-ras))))
   
  (verbose "A"))
;;_____________________________________________________________________________

(defun create-tbox-f (&optional uri)
  "to create a new TBox"
  (when (stringp uri)
    (setq uri (intern uri)))
  (create-cel-ontology uri :make-current t)
  *ontology*)
;;_____________________________________________________________________________

(defun release-tbox-f (&optional uri)
  "to release the specified TBox"
  (when (stringp uri)
    (setq uri (intern uri)))
  (release-cel-ontology uri)
  *ontology*)
;;_____________________________________________________________________________

(defun restore-tbox-f (uri)
  "to restore the specified TBox"
  (when (stringp uri)
    (setq uri (intern uri)))
  (restore-cel-ontology uri)
  *ontology*)
;;_____________________________________________________________________________

(defun duplicate-tbox-f (uri)
  "to duplicate the specified TBox"
  (when (stringp uri)
    (setq uri (intern uri)))
  (duplicate-cel-ontology uri :make-current t)
  *ontology*)
;;_____________________________________________________________________________

(defun clear-tbox-f (&optional uri)
  "to clear the current TBox"
  (when (stringp uri)
    (setq uri (intern uri)))
  (clear-cel-ontology uri)
  *ontology*)
;;_____________________________________________________________________________

(defun load-tbox-f (file-name &optional clear?)
  "to load and preprocess an input TBox"
  (when clear?
    (clear-tbox-f))
  (unless (ont-state? :cleared)
    (return-from load-tbox-f ':tbox-state-error))
  
  (let ((fn (search-tbox-file file-name)))
    (if fn
	(progn	  
	  ;;(init-variables)
	  (unless (eval-tbox-axioms fn)
	    (return-from load-tbox-f))
	  (setf (ont-state) :prepared))
      (progn
	(err "File \"~a\" does not exist!" file-name)
	(return-from load-tbox-f))))
  *ontology*)
;;_____________________________________________________________________________

(defun classify-tbox-f ()
  "to classify the current TBox"
  (when (ont-state? :cleared)
    (normalize))
  (unless (ont-state? :prepared)
    (return-from classify-tbox-f :tbox-state-error))
    
  (act-complete-all-imp-sets)
  (setf (ont-state) :classified)
  *ontology*)
;;_____________________________________________________________________________

(defun add-axiom-f (ax &key (make-copy t))
  "Properly add a new axiom to the system"
  ;; Directly evaluating new-axiom also works w.r.t. standard reasoning,
  ;; but CEL will lose track of the syntactical form of the axiom
  (unless (ont-state? (:cleared :i-cleared))
    (return-from add-axiom-f ':tbox-state-error))
  
  (handler-case (eval (if make-copy
			  (copy-tree ax)
			ax))
    (error ()
      (err "Unrecognized axiom: ~S" ax)
      (return-from add-axiom-f nil)
      ))  
  (push ax (odsm-axioms))
  t)
;;_____________________________________________________________________________

(defun add-axioms-f (ax-list &key (make-copy t))
  "Properly add new axioms to the system"
  ;; Directly evaluating new-axiom also works w.r.t. standard reasoning,
  ;; but CEL will lose track of the syntactical form of the axiom
  (unless (ont-state? (:cleared :i-cleared))
    (return-from add-axioms-f ':tbox-state-error))  
  
  (dolist (ax ax-list)
    (add-axiom-f ax :make-copy make-copy))
  t)
;;_____________________________________________________________________________

(defun reclassify-tbox-f ()
  "to reclassify the current TBox"
  (unless (ont-state? (:classified :taxonomized))    
    (return-from reclassify-tbox-f :tbox-state-error))
  (clear-queue-s)
  (act-complete-all-imp-sets)
  (setf (ont-state) :classified)
  *ontology*)
;;_____________________________________________________________________________

(defun q-tbox-cleared-f ()
  (ont-state? :cleared))
;;_____________________________________________________________________________

(defun q-tbox-prepared-f ()
  (ont-state? :prepared))
;;_____________________________________________________________________________

(defun q-tbox-classified-f ()
  (ont-state? (:classified :taxonomized :i-classified :i-taxonomized)))
;;_____________________________________________________________________________

(defun q-ttbox-active-f ()
  (ont-state? (:i-cleared :i-prepared :i-classified :i-taxonomized)))
;;_____________________________________________________________________________
  
(defun q-ttbox-cleared-f ()
  (ont-state? :i-cleared))
;;_____________________________________________________________________________

(defun q-ttbox-prepared-f ()
  (ont-state? :i-prepared))
;;_____________________________________________________________________________

(defun q-ttbox-classified-f ()
  (ont-state? (:i-classified :i-taxonomized)))
;;_____________________________________________________________________________

(defun q-tbox-consistent-f ()
  (ont-tbox-consistent?))
;;_____________________________________________________________________________

(defun q-abox-consistent-f ()
  (ont-abox-consistent?))
;;_____________________________________________________________________________

(defun q-is-concept-f (cn)
  (let ((s-cn (system-cname (synonym-of cn))))
    (and (numberp s-cn)
	 (>= s-cn 0)
	 (not (c-individual? s-cn)))))
;;_____________________________________________________________________________

(defun q-is-individual-f (cn)
  (let ((s-cn (system-cname (synonym-of cn))))
    (and (> s-cn 1)
	 (c-individual? s-cn))))
;;_____________________________________________________________________________

(defun q-is-role-f (rn)
  (integerp (system-rname rn)))
;;_____________________________________________________________________________

(defun q-all-concepts-f ()
  "Attempt to optimize retrieval of abundant concepts"
    
  (let ((basket nil))
    (loop for i from 0 to (cdr (ods-system-cname-range)) do
	  (unless (c-individual? i)
	    (setq basket (append (synonyms (user-cname i))
				 basket))))
    basket))
;;_____________________________________________________________________________

(defun q-all-concept-eclasses-f ()
  "Return the list of all eclasses of all cnames"
  (request-hasse)
  (let ((eclass-set nil))
    (loop for i from 0 to (cdr (ods-system-cname-range)) do
	  (unless (c-individual? i)
	    
	    (let ((eclass nil))
	      (when (listp (c-equivalent i))
		(dolist (y (cons i (c-equivalent i)))
		  (setq eclass (append (synonyms (user-cname y))
				       eclass)))
		(push eclass eclass-set)
		)
	      )))
    eclass-set))
;;_____________________________________________________________________________

(defun q-all-individuals-f ()
  (ont-individuals))
;;_____________________________________________________________________________

(defun q-all-unsatisfiable-concepts-f ()
  (let ((basket nil))
    (dolist (i (s-labels-both 0))
      (when (and (> i 1)
		 (not (c-individual? i)))
	(setq basket (append basket
			     (synonyms (user-cname i))))))
    basket))
;;_____________________________________________________________________________

(defun q-all-unsatisfiable-individuals-f ()
  (let ((basket nil))
    (dolist (i (s-labels-both 0))
      (when (and (> i 1)
		 (c-individual? i))
	(setq basket (append basket
			     (synonyms (user-cname i))))))
    basket))
;;_____________________________________________________________________________

(defun q-all-roles--f ()
  (ods-rname-array))
(defun q-all-roles-f ()
  (let ((basket nil))
    (loop for i from 0 to (- (length (ods-rname-array)) 1) do
	  (push (user-rname i) basket))
    basket))
;;_____________________________________________________________________________  
(defun q-concept-satisfiable-p-- (x)
  (declare (ignore x))
  t)
(defun q-concept-satisfiable-f (x)
  (setq x (system-cname x))
  
  ;; when the concept is unknown, then it is satisfiable
  (when (null x)
    (return-from q-concept-satisfiable-f t))    
  (if (or (eq x *bot*)
	  (eq (s-labels-both x) *bot*))
      nil
    t))
;;_____________________________________________________________________________  

(defun q-concept-subsumes-f (x y)
  (let ((s-x (system-cname (synonym-of x)))
	(s-y (system-cname (synonym-of y))))
    ;; return nil for unknown concepts
    (unless (and s-x s-y)
      (return-from q-concept-subsumes-f nil))
    ;; top (bottom) concept subsumes (is subsumed by) all concepts    
    (when (or (eq s-x *top*)
	      (eq s-y *bot*)
	      (eq s-x s-y))
      (return-from q-concept-subsumes-f t))
    ;; non-concepts cannot be quiried
    (unless s-x 
      (err "~a is not a concept name" x)
      (return-from q-concept-subsumes-f :named-concept))
    (unless s-y 
      (err "~a is not a concept name" y)
      (return-from q-concept-subsumes-f :named-concept))
    
    (let ((state (ont-state)))
      ;; use the old state in case of these two
      (when (ont-state? (:i-cleared :i-prepared))
	(setq state (ontology-state *backup-ontology*)))
      
      (case state
	((:cleared :prepared)
	 ;; if not classified but already prepared, compute pairwise subsumption
	 (err "TBox is not classified. Please use `?implies' or `?subsumes' for complex and preclassified subsumption queries!")
	 (return-from q-concept-subsumes-f :tbox-state-error))	
	((:classified :taxonomized :i-classified :i-taxonomized)
	 ;; if tbox is classified, look up for the answer depending on the mode
	 (if (check-label-both s-x s-y)
	     t nil))
	(t
	 ;; otherwise, return nil
	 (err "Fatal internal state error. Please reset by `clear-tbox'")
	 (return-from q-concept-subsumes-f :tbox-state-error))
	))))
;;_____________________________________________________________________________

(defun q-concept-implies-f (x y)
  (q-concept-subsumes-f y x))
;;_____________________________________________________________________________

(defun q-concept-equivalent-f (x y)
  (and (q-concept-subsumes-f x y)
       (q-concept-subsumes-f y x)))
;;_____________________________________________________________________________

(defun q-concept-disjoint-f (x y)
  (declare (ignore x y))
  ':unsupported)
;;_____________________________________________________________________________

(defun q-role-subsumes-f (rn1 rn2)
  (when (eq rn1 rn2)
    (return-from q-role-subsumes-f t))
  
  (setq rn1 (system-rname rn1))  
  (setq rn2 (system-rname rn2))  
  (unless (and rn1 rn2)
    (return-from q-role-subsumes-f nil))  
  
  (if (member rn1 (r-ancestors rn2))
      T))
;;_____________________________________________________________________________

(defun q-role-implies-f (rn1 rn2)
  (q-role-subsumes-f rn2 rn1))
;;_____________________________________________________________________________

(defun q-role-equivalent-f (rn1 rn2)
  (and (q-role-subsumes-f rn2 rn1)
       (q-role-subsumes-f rn1 rn2)))
;;_____________________________________________________________________________

(defun q-transitive-f (rn)
  (setq rn (system-rname rn))  
  (unless rn
    (return-from q-transitive-f nil))
  
  (r-transitive rn))
;;_____________________________________________________________________________

(defun q-reflexive-f (rn)
  (setq rn (system-rname rn))  
  (unless rn
    (return-from q-reflexive-f nil))
  
  (r-reflexive rn))
;;_____________________________________________________________________________

(defun q-parents-f (cn &key (make-eclass nil))
  (setq cn (system-cname (synonym-of cn)))
  
  (when (or (not cn)
	    (c-individual? cn))
    (return-from q-parents-f (and make-eclass
				  '((top)) )))
  
  ;; this requires DAG
  (request-hasse)        

  ;; optimized retrieval, as the parent-set for bottom tends to be giantic
  ;; also, we need to deal with intermediate individual nodes
  (when (eq *bot* cn)
    (return-from q-parents-f 
      (if make-eclass
	  (q-bottom-parents-eclass-set-f)
	(q-bottom-parents-flat-set-f)
	)))  
  
  (cond
   
   (make-eclass
    (let ((s-cnames (c-parents cn))
	  (parent-set nil))
      (dolist (s-cn s-cnames)
	(let ((synonym-set nil))
	  (dolist (x (cons s-cn (c-equivalent s-cn)))
	    (setq synonym-set (append (synonyms (user-cname x))
				      synonym-set)))
	  (push synonym-set parent-set)))
      parent-set))
   
   (t
    (let ((s-cnames (c-parents cn))
	  (parent-set nil))
      (dolist (s-cn s-cnames)
	(dolist (x (cons s-cn (c-equivalent s-cn)))
	    (setq parent-set (append (synonyms (user-cname x))
				     parent-set))))
      parent-set))
   ))
;;_____________________________________________________________________________

(defun q-bottom-parents-eclass-set-f ()
  
  (let ((s-cnames (c-parents *bot*))
	(parent-set nil))
    (dolist (s-cn s-cnames)
      (cond
       ;; if s-cn is an individual, leap up one step
       ((c-individual? s-cn)	  
	(let ((synonym-set nil)
	      (ind-types (c-parents s-cn)))
	  
	  (dolist (ind-type ind-types)
	    (when (every #'(lambda (x)
			     (c-individual? x))
			 (c-children ind-type))
	      (dolist (x (cons ind-type (c-equivalent ind-type)))
		(setq synonym-set (append (synonyms (user-cname x))
					  synonym-set)))))
	  (push synonym-set parent-set)
	  ))	  
       ;; ordinary concept
       (t
	  (let ((synonym-set nil))
	    (dolist (x (cons s-cn (c-equivalent s-cn)))
	      (setq synonym-set (append (synonyms (user-cname x))
					synonym-set)))
	    (push synonym-set parent-set)
	    ))
       ))
    parent-set
    ))

(defun q-bottom-parents-flat-set-f ()
  (let ((s-cnames (c-parents *bot*))
	(parent-set nil))
    (dolist (s-cn s-cnames)
      (cond
       ;; if s-cn is an individual, leap up one step
       ((c-individual? s-cn)	  
	
	(dolist (ind-type (c-parents s-cn))
	  (when (every #'(lambda (x)
			   (c-individual? x))
		       (c-children ind-type))
	    (dolist (x (cons ind-type (c-equivalent ind-type)))
	      (setq parent-set (append (synonyms (user-cname x))
				       parent-set)))))
	)	 
       ;; ordinary concept
       (t
	(dolist (x (cons s-cn (c-equivalent s-cn)))
	  (setq parent-set (append (synonyms (user-cname x))
				   parent-set)))
	)))
    parent-set
    ))
;;_____________________________________________________________________________

(defun q-children-f (cn &key (make-eclass nil))
  (setq cn (system-cname (synonym-of cn)))
  
  (when (or (not cn)
	    (c-individual? cn))
    (return-from q-children-f (and make-eclass
				   '((bottom)) )))
  
  ;; this requires DAG
  (request-hasse)  
  
  (cond
   
   (make-eclass
    (let ((s-cnames (c-children cn))
	  (child-set nil))
      (dolist (s-cn s-cnames)
	(when (not (c-individual? s-cn))
	  (let ((synonym-set nil))
	    (dolist (x (cons s-cn 
			     (c-equivalent s-cn)))
	      (setq synonym-set (append (synonyms (user-cname x))
					synonym-set)))
	    (push synonym-set child-set))))
      (or child-set
	  '((bottom)))
      ))
   (t
    (let ((s-cnames (c-children cn))
	  (child-set nil))
      (dolist (s-cn s-cnames)
	(when (not (c-individual? s-cn))
	  (dolist (x (cons s-cn
			   (c-equivalent s-cn)))
	    (setq child-set (append (synonyms (user-cname x))
				    child-set)))))
      (or child-set
	  '(bottom))))
    ))
;;_____________________________________________________________________________


(defun mark-all-told-subsumers-of (s-cn t-marker
				   &key (mark-itself t))
  "Mark all told subsumers of s-cn in the hashtable t-marker"
  (when mark-itself 
    (setf (gethash s-cn t-marker) t))
  (dolist (x (c-told-subsumers s-cn))
    (unless (gethash x t-marker)
      (mark-all-told-subsumers-of x t-marker))))
;;_____________________________________________________________________________


(defun q-told-ancestors-f (cn)
  "Given a concept name cn, the funtion returns subsuming concept names told explicitly in the ontology, i.e. transitive closure of told subsumers"
  
  (setq cn (system-cname (synonym-of cn)))
  
  (when (or (not cn)
	    (c-individual? cn)
	    (eq cn *top*)
	    (eq cn *bot*))
    (return-from q-told-ancestors-f :not-concept-name))
  
  (let ((told-table (make-hash-table :test 'eq))
	(told-list nil)
	(u-cnames nil))
	
    (mark-all-told-subsumers-of cn told-table
				:mark-itself nil)
    (setq told-list (get-hash-keys told-table))
    (dolist (s-cn told-list)
      (setq u-cnames (append (synonyms (user-cname s-cn))
			     u-cnames)))
    
    u-cnames
    ))
;;_____________________________________________________________________________
    
(defun q-inferred-ancestors-f (cn)
  "Given a concept name cn, the funtion returns subsuming strictly inferred concept names subsuming cn, i.e. all ancestors except for told and trivial ones"
  
  (setq cn (system-cname (synonym-of cn)))
  
  (when (or (not cn)
	    (c-individual? cn)
	    (eq cn *top*)
	    (eq cn *bot*))
    (return-from q-inferred-ancestors-f :not-concept-name))
  
  (let ((ignore-table (make-hash-table :test 'eq))
	(cn-supers (s-labels-both cn))
	(top-supers (s-top-labels))
	(u-cnames nil))

    (when (eq cn-supers *bot*)
      ;; all concepts are ancestors of an unsat concept
      (return-from q-inferred-ancestors-f
	(q-all-concepts-f)))
    
    (mark-all-told-subsumers-of cn ignore-table
				:mark-itself t)
    (setf (gethash *top* ignore-table) t)
    
    (dolist (s-cn (union cn-supers
			 top-supers
			 :test 'eq))
      (when (and (> s-cn 0)
		 (not (gethash s-cn ignore-table)))		       
	(setq u-cnames (append (synonyms (user-cname s-cn))
			       u-cnames))))
    u-cnames))
;;_____________________________________________________________________________

(defun q-ancestors-f (cn &key (make-eclass nil))
  "Given a concept name cn, the function returns *all* concept names subsuming it"
  (setq cn (system-cname (synonym-of cn)))
  
  (when (or (not cn)
	    (c-individual? cn))
    (return-from q-ancestors-f (and make-eclass
				    '((top)) )))  
  (cond
   (make-eclass
    (if (zerop cn)
	(q-all-concept-eclasses-f)
      (q-ancestors-eclass-set-f cn)))
   (t
    (if (zerop cn)
	(q-all-concepts-f)
      (q-ancestors-flat-set-f cn)))
   ))

(defun q-ancestors-flat-set-f (s-cn)
  "Given a (valid) system cname, return a flat set of user cnames subsuming it"
  (let ((cn-supers (s-labels-both s-cn))
	(top-supers (s-top-labels-both))
	(u-cnames nil))
    (when (eq cn-supers *bot*)
      ;; all concepts are ancestors of an unsat concept
      (return-from q-ancestors-flat-set-f
	(q-all-concepts-f)))
    
    (dolist (x (union cn-supers top-supers
		      :test 'eq))
      (when (> x 0)
	(setq u-cnames (append (synonyms (user-cname x))
			       u-cnames))))
    u-cnames))

(defun q-ancestors-eclass-set-f (s-cn)
  "Given a (valid) system cname, return an eclass set of user cnames subsuming it"
  (let ((cn-supers (s-labels-both s-cn))
	(top-supers (s-top-labels-both))
	(eclass-set nil))
    (when (eq cn-supers *bot*)
      ;; all concepts are ancestors of an unsat concept
      (return-from q-ancestors-eclass-set-f
	(q-all-concept-eclasses-f)))
    
    (dolist (x (union cn-supers top-supers
		      :test 'eq))
      (when (> x 0)
	(let ((eclass nil))
	  (when (listp (c-equivalent x))
	    (dolist (y (cons x (c-equivalent x)))
	      (setq eclass (append (synonyms (user-cname y))
				   eclass)))
	    (push eclass eclass-set)
	    )
	  )))
    eclass-set))

(defun q-ancestors-f-backup (cn)
  "Given a concept name cn, the function returns *all* concept names subsuming it"
  (setq cn (system-cname (synonym-of cn)))
  
  ;; ============================ to check
  (when (or (not cn)
	    (c-individual? cn))
    (return-from q-ancestors-f-backup nil))
  (when (zerop cn)
    ;; all concepts are ancestors of bottom (or unsatisfiable concepts)
    (return-from q-ancestors-f-backup
      (q-all-concepts-f)))
  
  (let ((cn-supers (s-labels-both cn))
	(top-supers (s-top-labels))
	(u-cnames nil))
    (when (eq cn-supers *bot*)
      ;; all concepts are ancestors of an unsat concept
      (return-from q-ancestors-f-backup
	(q-all-concepts-f)))
    
    (dolist (s-cn (union cn-supers
			 top-supers
			 :test 'eq))
      (when (> s-cn 0)
	(setq u-cnames (append (synonyms (user-cname s-cn))
			       u-cnames))))
    u-cnames))
;;_____________________________________________________________________________


(defun q-descendants-f (cn &key (make-eclass nil))
  "Given a concept name cn, the function returns *all* concept names subsuming it"
  (setq cn (system-cname (synonym-of cn)))
  
  (when (or (not cn)
	    (c-individual? cn))
    (return-from q-descendants-f (and make-eclass
				      '((bottom)) )))  
  (cond
   (make-eclass
    (if (= cn 1)
	(q-all-concept-eclasses-f)
      (q-descendants-eclass-set-f cn)))
   (t
    (if (= cn 1)
	(q-all-concepts-f)
      (q-descendants-flat-set-f cn)))
   ))

(defun q-descendants-flat-set-f (cn)
  (let ((descendants-table (make-hash-table :test 'eq))
	(descendants nil))    
    (q-descendants-flat-set-f-r cn descendants-table)
    (loop as key being the hash-key of descendants-table do
	  (setq descendants (append (synonyms (user-cname key))
				    descendants)))
    descendants
    ))
      
(defun q-descendants-flat-set-f-r (cn table)
  (unless (or (gethash cn table)
	      (c-individual? cn))
    (setf (gethash cn table) t)    
    (dolist (x (c-equivalent cn))
      (setf (gethash x table) t))
    (dolist (y (c-children cn))
      (q-descendants-flat-set-f-r y table))
    ))

(defun q-descendants-eclass-set-f (cn)
  (let ((descendants-table (make-hash-table :test 'eq))
	(eclass-set nil))
    (declare (special descendants-table))
    (declare (special eclass-set))
    
    (q-descendants-eclass-set-f-r cn)
    eclass-set
    ))
(defun q-descendants-eclass-set-f-r (cn)
  (declare (special descendants-table))
  (declare (special eclass-set))
  
  (unless (or (gethash cn descendants-table)
	      (c-individual? cn))
    ;;(setf (gethash cn table) t)
    (let ((eclass nil))
      (dolist (x (cons cn (c-equivalent cn)))
	(setf (gethash x descendants-table) t)
	(setq eclass (append (synonyms (user-cname x))
			     eclass)))
      (push eclass eclass-set)
      )
    
    (dolist (y (c-children cn))
      (q-descendants-eclass-set-f-r y))
    ))

    
    

(defun q-descendants-f-backup (cn &key (make-eclass nil))  
  (when (eq cn top)
    ;; all concepts are descendants of top
    (return-from q-descendants-f-backup
      (q-all-concepts-f)))
  
  (setq cn (system-cname (synonym-of cn)))  
  (when (or (not cn)
	    (c-individual? cn))
    (return-from q-descendants-f-backup (and make-eclass
				      '((bottom)) )))
  
  ;; this requires DAG
  (request-hasse)  
  
  ;; OPTIMIZED: compute all descendants in user cname format directly
  (hasse-concept-descendants-new cn))  
;;_____________________________________________________________________________

(defun q-equivalents-f (cn &optional (unknown-as-primitive nil)
				     (the-cn cn))
  (setq cn (system-cname (synonym-of cn)))
  
  (when (or (not cn)
	    (c-individual? cn))
    (return-from q-equivalents-f (and unknown-as-primitive
				      (list the-cn))))
  
  ;; this requires DAG
  (request-hasse)  
  
  (let ((s-cnames (cons cn (c-equivalent cn)))
	(u-cnames nil))
    (dolist (s-cn s-cnames)
      (setq u-cnames (append (synonyms (user-cname s-cn))
			     u-cnames)))
    u-cnames
    ))
;;_____________________________________________________________________________

(defun q-individual-direct-types-f (cn &key (make-eclass nil))
  "Comparable to q-parents-f"
  (setq cn (system-cname (synonym-of cn)))
  
  (unless (and cn
	       (c-individual? cn))
    (return-from q-individual-direct-types-f (and make-eclass 
						  '((top)) )))
  
  ;; this requires DAG
  (request-hasse)  
  
  (cond
   
   (make-eclass
    (let ((s-cnames (c-parents cn))
	  (parent-set nil))
      (dolist (s-cn s-cnames)	
	(let ((synonym-set nil))
	  (dolist (x (cons s-cn 
			   (c-equivalent s-cn)))
	    (setq synonym-set (append (synonyms (user-cname x))
				      synonym-set)))	  
	  (push synonym-set parent-set)))
      parent-set))
   
   (t
    (let ((s-cnames (c-parents cn))
	  (parent-set nil))
      (dolist (s-cn s-cnames)
	(dolist (x (cons s-cn 
			 (c-equivalent s-cn)))
	  (setq parent-set (append (synonyms (user-cname x))
				   parent-set))))
      parent-set))
    ))
;;_____________________________________________________________________________

(defun q-individual-types-f (cn &key (make-eclass nil))
  "Comparable to q-ancestors-f"
  (setq cn (system-cname (synonym-of cn)))
  
  (when (or (not cn)
	    (not (c-individual? cn)))   
    (return-from q-individual-types-f (and make-eclass
					   '((top)) )))  
  (cond
   (make-eclass    
    (q-individual-types-eclass-set-f cn))
   (t    
    (q-individual-types-flat-set-f cn))
   ))

(defun q-individual-types-flat-set-f (s-cn)
  "Given a (valid) system individual cname, return a flat set of user cnames that are its types"  
  (let ((cn-supers (s-labels-both s-cn))
	(top-supers (s-top-labels-both))
	(u-cnames nil))
    (when (eq cn-supers *bot*)
      ;; all concepts are ancestors of an unsat concept
      (return-from q-individual-types-flat-set-f
	(q-all-concepts-f)))
    
    (dolist (x (union cn-supers
		      top-supers
		      :test 'eq))
      (when (and (> x 0)
		 (not (= x s-cn))) ;; cn itself is individual, not its type
	(setq u-cnames (append (synonyms (user-cname x))
			       u-cnames))))
    u-cnames))

(defun q-individual-types-eclass-set-f (s-cn)
  "Given a (valid) system individual cname, return an eclass set of user cnames that are its types"
  (let ((cn-supers (s-labels-both s-cn))
	(top-supers (s-top-labels-both))
	(eclass-set nil))
    (when (eq cn-supers *bot*)
      ;; all concepts are ancestors of an unsat concept
      (return-from q-individual-types-eclass-set-f
	(q-all-concept-eclasses-f)))
    
    (dolist (x (union cn-supers top-supers
		      :test 'eq))
      (when (and (> x 0)
		 (not (= x s-cn)))
	(let ((eclass nil))
	  (when (listp (c-equivalent x))
	    (dolist (y (cons x (c-equivalent x)))
	      (setq eclass (append (synonyms (user-cname y))
				   eclass)))
	    (push eclass eclass-set)
	    )
	  )))
    eclass-set))
;;_____________________________________________________________________________

(defun q-instance-f (ind cn)
  "Similar to checking subsumption ind =>? cn"
  (let ((s-ind (system-cname (synonym-of ind)))
	(s-cn (system-cname (synonym-of cn))))
    
    (cond
     ;; return nil for unknown concepts
     ((not (and s-ind s-cn))
      (return-from q-instance-f nil))
     ;; ind and cn are not of the right type
     ((not (c-individual? s-ind))
      (err "~a is not an individual" ind)
      (return-from q-instance-f ':individual))
     ((c-individual? s-cn)
      (err "~a is not a concept name" cn)
      (return-from q-instance-f ':named-concept))            
     ;; bottom does not have any instances
     ((eq s-cn *bot*)
      (return-from q-instance-f nil))
     ;; every individual is an instance of top
     ((eq s-cn *top*)
      (return-from q-instance-f t))
     )     
       
    (case (ont-state)
      ((:classified :taxonomized)
       ;; if tbox is classified, look up for the answer depending on the mode
       (if (check-label-both s-cn s-ind)
	   t nil))
      ((:prepared)
       ;; if not classified but already prepared, compute pairwise subsumption
       (err "Pre-classified subsumption test is obsolute. Please classify the ontology first and then invoke this function again.")
       (return-from q-instance-f ':tbox-state-error))
       ;;(test-subs s-x s-y))
      (t
       ;; otherwise, return  state error
       (err "No TBox has been loaded")
       (return-from q-instance-f ':tbox-state-error))
      )))
;;_____________________________________________________________________________

(defun q-concept-direct-instances-f (cn)
  "Comparable to q-children-f, filtering only individual cnames"
  (setq cn (system-cname (synonym-of cn)))  
  
  (when (or (not cn)
	    (c-individual? cn))
    (return-from q-concept-direct-instances-f))
  
  (request-hasse)  
  
  (let ((s-cnames (c-children cn))
	(instances nil))
    (dolist (s-cn s-cnames)
      (when (c-individual? s-cn)
	(dolist (x (cons s-cn
			 (c-equivalent s-cn)))
	  (setq instances (append (synonyms (user-cname x))
				  instances)))))
    instances
    ))
;;_____________________________________________________________________________

(defun q-concept-instances-f (cn)
  "Comparable to q-descendants-f, filtering only individual cnames"
  (when (eq cn top)
    ;; all individuals are instance of top
    (return-from q-concept-instances-f
      (q-all-individuals-f)))
  
  (setq cn (system-cname (synonym-of cn)))  
  (when (or (not cn)
	    (c-individual? cn))    
    (return-from q-concept-instances-f))
  
  ;; this requires DAG
  (request-hasse)  
  
  (let ((s-cnames (hasse-individual-descendants cn))
	(u-cnames nil))
    (dolist (s-cn s-cnames)
      (setq u-cnames (append (synonyms (user-cname s-cn))
			     u-cnames)))
    u-cnames))
;;_____________________________________________________________________________

(defun q-parent-roles-f (rn)
  (declare (ignore rn))
  ':unsupported)
;;_____________________________________________________________________________

(defun q-child-roles-f (rn)
  (declare (ignore rn))
  ':unsupported)
;;_____________________________________________________________________________

(defun q-super-roles-f (rn)
  (setq rn (system-rname rn))
  (unless rn
    (return-from q-super-roles-f nil))  
  
  (let ((s-rnames (r-ancestors rn))
	(u-rnames nil))
    (dolist (s-rn s-rnames)
      (pushnew (r-user-name s-rn) u-rnames
	       :test 'eq))
    u-rnames))
;;_____________________________________________________________________________

(defun q-sub-roles-f (rn)
  (setq rn (system-rname rn))
  (unless rn
    (return-from q-sub-roles-f nil))
  
  (let ((s-rnames (r-descendants rn))
	(u-rnames nil))
    (dolist (s-rn s-rnames)
      (pushnew (r-user-name s-rn) u-rnames
	       :test 'eq))
    u-rnames))
;;_____________________________________________________________________________


(defun imp-set-f (cn)
  "to retrieve the succinct subsumption set of cn, ie.
(imp-set-f top) = valid concepts
(imp-set-f bottom) = unsat concepts
(imp-set-f sat-cname) = set of non-valid subsumers
(imp-set-f unsat-cname) = nil"
 
  (setq cn (synonym-of cn))
  (let ((s-cnames (s-labels-both (system-cname cn)))
	(u-cnames nil))
    (cond
     ((eq s-cnames 0) nil)
     ((eq s-cnames nil) (list cn))
     (t      
      (dolist (s-cn s-cnames)
	(when (>= s-cn 0)
	  (let ((u-cn (user-cname s-cn)))
	    (push u-cn u-cnames))))
      u-cnames))))
;;_____________________________________________________________________________

(defun output-subsumption-f (stream keyword)
  (declare (special stream))
  (declare (special keyword))
    
  (loop ;; for each system cname incl. top
      for i 
      from 2
      to (cdr (ods-system-cname-range))
      do (let* ((cn (user-cname i))
		(imp-set (imp-set-f cn)))
	   (dolist (subsumer imp-set)
	     (unless (eq cn subsumer)
	       (format stream
		       "~%(~a ~s ~s)"
		       keyword
		       subsumer
		       cn))))
	 ))
;;_____________________________________________________________________________

(defun output-imp-sets-f (stream keyword)
  (format stream ";; For succinctness, we output only subsumption sets only satisfiable concepts, as unsatisfiable concepts have all concepts as their subsumers. Moreover, to reduce redundancy, we split valid concepts (ie. equivalent to top) out of each subsumption set.")
  
  (format stream "~%(~a ~s)" 
	  "unsat-concepts" 
	  (imp-set-f bottom))
  (format stream "~%(~a ~s" 
	  "valid-concepts" 
	  (imp-set-f top))
  
  (loop ;; for each system cname incl. top
      for i 
      from 2
      to (cdr (ods-system-cname-range))
      do (let* ((cn (user-cname i))
		(imp-set (imp-set-f cn)))
	   (when imp-set
	     (format stream
		     "~%(~a ~s ~s)"
		     keyword
		     cn
		     imp-set)))))   
;;_____________________________________________________________________________

(defun out-active-imp-sets ()
  (maphash #'(lambda (key values)
	       (declare (ignore values))
	       (if (> key 0)
		   (let* ((cn (user-cname key))
			  (imp-set (imp-set-f cn)))
		     (if imp-set (format t
					 "~%(~a ~s ~s)"
					 "imp-set"
					 cn
					 imp-set))))	       )
	   (ods-activation-flag)))
;;_____________________________________________________________________________

(defun output-supers-f (stream keyword)
  (loop ;; for each system cname incl. bottom and top
      for i
      from 0
      to (cdr (ods-system-cname-range))
      do (let* ((cn (user-cname i))
		(supers (q-ancestors-f cn)))
	   (dolist (c (synonyms cn))
	     (format stream
		     "~%(~A ~S~%        ~S)"
		     keyword
		     c
		     supers)))))

(defun output-supers-inverse-f (stream keyword)
  (loop ;; for each system cname incl. bottom and top
      for i
      from (cdr (ods-system-cname-range))
      downto 2
      do (let* ((cn (user-cname i))
		(supers (q-ancestors-f cn)))
	   (dolist (c (synonyms cn))
	     (format stream
		     "~%(~A ~S~%        ~S)"
		     keyword
		     c
		     supers)))))
;;_____________________________________________________________________________

(defun output-synonyms-f (stream keyword)
  (declare (special stream))
  (declare (special keyword))
  
  ;; key is a representant and value is all equivalent concepts
  (maphash #'(lambda (key value)
               (declare (special stream))
	       (declare (special keyword))
	       (if (listp value)
		   (format stream
			   "~%(~a ~s ~s)"
			   keyword
			   key
			   value)))
	   (ods-synonym-table)))
;;_____________________________________________________________________________

(defun output-hierarchy-f (&optional (file-name nil) system-root depth realization)
  "to print out the taxomony of the TBox either to file or console [available only in *classification-mode* 1]"
  (request-hasse)
  
  (cond  
   ((not (q-tbox-classified-f))
    (err "TBox has to be first classified!")
    (return-from output-hierarchy-f :tbox-state-error))
   ((q-ttbox-active-f)
    (err "TTBox has to be committed/deactivated in order to proceed!")
    (return-from output-hierarchy-f :tbox-state-error))
;;;   ((not (eq *classification-mode* 1))
;;;    (err "This feature is available only after classification in mode 1"))
   (t ;; otherwise, ready to give output
    (let ((unsat-cns (all-unsatisfiable-concepts))
	  (unsat-inds (all-unsatisfiable-individuals)))
      (cond
       (file-name
	(with-open-file (output-stream file-name		   
			 :direction :output 
			 :if-exists :supersede)
	  (output-hierarchy-fr-new output-stream 
			       (or system-root 1)
			       depth
			       realization)
	  ;; bottom-most in the hierarchy is the bottom conncept and all unsat ones
	  (when unsat-cns
	    (format output-stream "~%  !:~A =~S"
		    bottom 
		    unsat-cns))
	  ;; unsatisfiable individuals
	  (when unsat-inds
	    (format output-stream "~% !!!: ~A"
		    (mapcar #'(lambda (x)
				(format nil "{~S}" x))
			    unsat-inds)))
	  ))
       (t	  
	(output-hierarchy-fr-new t 
				 (or system-root 1)
				 depth
				 realization)
	;; bottom-most in the hierarchy is the bottom conncept and all unsat ones
	(when unsat-cns
	  (format t "~%~%  -:~A =~S"	 
		  bottom 
		  unsat-cns))
	;; unsatisfiable individuals
	(when unsat-inds
	  (format t "~%  ~~:~A"
		  (mapcar #'(lambda (x)
			      (format nil "{~S}" x))
			  unsat-inds)))
	))))))
;;_____________________________________________________________________________

(defun output-hierarchy-fr-new (&optional (stream t)
					  (c *top*)
					  (depth nil)
					  (realization nil)
					  (indent 0) (spacing 2))
  ;; we process this node, only when it's a concept node or individual when we want realization
  ;;(print realization)  
  (when (<= c 0)
    (return-from output-hierarchy-fr-new))
  
  (cond
   ((c-individual? c)
    (unless realization
      (return-from output-hierarchy-fr-new))
    
    (let* ((ind (user-cname c))
	   (ind-equivalents (remove-if #'(lambda (x) (eq x ind))
				       (ind-synonyms ind)))
	   )
      (format stream "~%~3D:~A{~S}"
	      (/ indent spacing)
	      (make-string indent :initial-element #\ )
	      ind)
      (when ind-equivalents
	(format stream " ={~S}"
		ind-equivalents))
      ))
   
   (t ;; when c is an actual concept name
    (let* ((c-user-name (user-cname c))
	   (equivalents (union (c-equivalent-user-names c)
			       (remove-if #'(lambda (x) (eq x c-user-name))
					  (synonyms c-user-name))
			       :test 'eq)))
      (format stream "~%~3D:~A~S"
	      (/ indent spacing)
	      (make-string indent :initial-element #\ )
	      c-user-name)
      (when equivalents
	(format stream " =~S"
		equivalents))
      ))
   )
   
  ;; depth handling
  (cond
   ((null depth)
    (dolist (x (c-children c))
      (output-hierarchy-fr-new stream x nil realization (+ indent spacing))))
   
   ((not (zerop depth))
    (decf depth)
    (dolist (x (c-children c))
      (output-hierarchy-fr-new stream x depth realization (+ indent spacing))))
   ))


(defun output-hierarchy-fr (&optional (stream t)
				      (c *top*)
				      (depth nil)
				      (realization nil)
				      (indent 0) (spacing 2))
  ;; we process this node, only when it's a concept node or individual when we want realization
  ;;(print realization)
  (when (and (> c 0)
	     (or (not (c-individual? c))
		 realization))
    
    (let* ((c-user-name (user-cname c))
	   (equivalents (union (c-equivalent-user-names c)
			       (remove-if #'(lambda (x) (eq x c-user-name))
					  (synonyms c-user-name))
			       :test 'eq)))
      (if (c-individual? c) 	  	  
	  (format stream "~%~3D:~A{~S}"
		  (/ indent spacing)
		  (make-string indent :initial-element #\ )
		  c-user-name)
	(format stream "~%~3D:~A~S"
		(/ indent spacing)
		(make-string indent :initial-element #\ )
		c-user-name))
      (when equivalents
	(format stream " =~S"
		equivalents))
      
      ;; depth handling
      (cond
       ((null depth)
	(dolist (x (c-children c))
	  (output-hierarchy-fr stream x nil realization (+ indent spacing))))
       ((not (zerop depth))
	(decf depth)
	(dolist (x (c-children c))
	  (output-hierarchy-fr stream x depth realization (+ indent spacing))))
       ))       
    ))
;;_____________________________________________________________________________

(defun output-taxonomy-f (stream keyword)
  (loop ;; for each system cname incl. bottom and top
      for i 
      from 0
      to (cdr (ods-system-cname-range))
      do (let* ((cn (user-cname i))
		(equivalent (c-equivalent i))
		(parents (c-parents i))
		(children (c-children i))
		(synonyms (remove-if #'(lambda (x) (eq x cn))
				     (synonyms cn))))	   
	   (cond
	    ;; c is in the taxonomy
	    ((listp equivalent)
	     (format stream
		     "~%(~A ~S ~%       :parents ~S ~%       :children ~S)"
		     keyword
		     cn
		     (user-cnames parents)
		     (user-cnames children))
	     (dolist (synonym synonyms)
	       (format stream
		       "~%(~A ~S :equivalent ~S)"
		       keyword
		       synonym     
		       cn)))    
	    ;; c is equivalent to equivalent
	    (t
	     (format stream
		     "~%(~A ~S :equivalent ~S)"
		     keyword
		     cn		     
		     (user-cname equivalent))
	     (dolist (synonym synonyms)
	       (format stream
		       "~%(~A ~S :equivalent ~S)"
		       keyword
		       synonym     
		       (user-cname equivalent))))))))
;;_____________________________________________________________________________

(defun output-taxonomy-p-- (stream keyword 
			  &optional (c *top*)
				    (marker (gensym)))
  (setf (c-visited c) marker)
  (let* ((cn (user-cname c))
	 (equivalent (c-equivalent c))
	 (parents (c-parents c))
	 (children (c-children c))
	 (synonyms (set-difference (synonyms cn)
				   (list cn))))
    ;;(format t "~S ~S ~S ~S ~S ~S" c cn equivalent parents children synonyms)
    (cond
     ;; c is in the taxonomy
     ((listp equivalent)
      (format stream
	      "~%(~A ~S ~%       :parents ~S ~%       :children ~S)"
	      keyword
	      cn
	      (user-cnames parents)
	      (user-cnames children))
      (dolist (synonym synonyms)
	(format stream
		"~%(~A ~S :equivalent ~S)"
		keyword
		synonym     
		cn))
      (dolist (child children)
	(unless (eq (c-visited child) marker)
	  (output-taxonomy-p-- stream keyword child marker))))
     ;; c is equivalent to equivalent
     (t
      (format stream
	      "~%(~A ~S :equivalent ~S)"
	      keyword
	      cn		     
	      (user-cname equivalent))
      (dolist (synonym synonyms)
	(format stream
		"~%(~A ~S :equivalent ~S)"
		keyword
		synonym     
		(user-cname equivalent)))))))
	     
  
;;_____________________________________________________________________________

