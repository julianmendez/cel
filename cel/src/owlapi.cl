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
;;;; Last Modified: 2009-03-12
;;;; Note the T/C in LICENSE.txt
;;_____________________________________________________________________________


(in-package cel-system)

;;_____________________________________________________________________________
;;_____________________________________________________________________________

(defvar *remote-host* nil)
(defvar *remote-port* nil)
;; the socket&stream for comunication with Java
;; of type socket-stream-internet-active
(defvar *socket-stream* nil)
(defvar *response* nil)

(defparameter *allow-cel-system-functions* nil)
;; a flag for debuggin message: t mean print
(defparameter *owlapi-verbose-msg* nil)

;; return values are on of the forms defined by *return-value-types*
;; e.g. (:void), (:error :owl-reasoner-exception)
;;      (:return :true), (:return ((C D) (E F)))
;; the following datatype correspondance is employed
;;   list -> Set
;;   dotted list -> Map
;;   symbol -> OWL elementary objects like OWLClass, OWLObjectProperty
(defconstant *return-value-types*
    '(:return ;; stadard return value
      :error ;; exception
      :void ;; nothing to return
      :unknown ;; unknown return value
      ))

(defconstant *allowed-exceptions* 
    '(:owl-reasoner-exception
      :unsupported-reasoner-operation-exception      
      ))
;;_____________________________________________________________________________



(defun startup-owlapi-listener (remote-host remote-port)
  (setq *cel-mode* :owlapi)
  (setq *remote-host* remote-host)
  (setq *remote-port* remote-port)

  (when *owlapi-verbose-msg*
    (msg "Establishing an OWLAPI connection..."))
  (if (open-owlapi-socket)
      (when *owlapi-verbose-msg*
	(format t "Connected to ~A:~A via local port ~A!"
		remote-host remote-port
		(socket:local-port *socket-stream*)))
    (progn 
      (when *owlapi-verbose-msg*
	(format t "Unable to connect!"))
      (owlapi.dispose))
    )
  
  (do ((query
	(read *socket-stream* nil *socket-stream*)
	(read *socket-stream* nil *socket-stream*)))      
      ((eq query *socket-stream*) t)
    
    (cond
     ;; if the query operand has the OWLAPI prefix
     ;; or any at all in case of *allow-cel-system-functions*
     ((or *allow-cel-system-functions*
	  (and (> (length (symbol-name (car query))) 
		  7)
	       (string= (subseq (symbol-name (car query)) 0 7) 
			"OWLAPI.")))
      
      (when *owlapi-verbose-msg*
	(msg "Receiving and processing a request: ~A" (car query)))
      (handler-case (setq *response*
		      (eval query))
	(error (condition)
	  (cond
	   ((typep condition 'undefined-function)
	    (when *owlapi-verbose-msg*
	      (err "Unsupported OWLAPI feature: ~S" (car query)))
	    (setq *response* '(:error :unsupported-reasoner-operation-exception))
	    )
	   (t
	    (when *owlapi-verbose-msg*
	      (err "Unknown error occured while processing the query:~%     ~S" query))
	    (setq *response* '(:error :owl-reasoner-exception))
	    )
	   )))
      )
     ;; if the query operand is not prefixed by OWLAPI
     (t
      (when *owlapi-verbose-msg*
	(msg "Received operation not permitted: ~A" (car query)))
      (setq *response* '(:error :unsupported-reasoner-operation-exception))
      ))
    
    (when *owlapi-verbose-msg*
      (msg "Sending a response..."))
    (send-owlapi-response)
    (when *owlapi-verbose-msg*
      (format t "Done!"))
    )  
  (when *owlapi-verbose-msg*
    (msg "Connection to OWLAPI was broken"))
  (owlapi.dispose)  
  )
;;_____________________________________________________________________________

(defun send-owlapi-response (&optional response)
  "send the response to OWLAPI by writing to the socket stream"
  (when response
    (setq *response* response))
  (setq *response* (if (and *response*
			    (listp *response*))
		       *response*
		     (list :unknown *response*)))
  
  (let ((str (format nil "(~S ~S)" 
		     (car *response*)
		     (cadr *response*))))
    (when *owlapi-verbose-msg*
      (print str))
    (format *socket-stream* "~A" str)  
    (finish-output *socket-stream*)
    ))  
;;_____________________________________________________________________________

(defun open-owlapi-socket (&optional remote-host remote-port)
  "open a (client) socket that connects to the specified remote address"
  (setq *remote-host* (or remote-host 
			  *remote-host*))
  (setq *remote-port* (or remote-port
			  *remote-port*))

  (handler-case (setq *socket-stream* 
		  (socket:make-socket :address-family :internet 
				      :type :stream			 
				      :remote-host *remote-host* 
				      :remote-port *remote-port*))
    (error ()      
      (err "Error while opening a socket! Make sure that host:port is ready to accept a connection.")
      (return-from open-owlapi-socket nil))
    )
  ;; set *socket-stream* to be interactive, 
  ;; so that flusing always work instantly
  (setf (interactive-stream-p *socket-stream*) t)
  )
;;_____________________________________________________________________________

(defun close-owlapi-socket ()
  (when *socket-stream*
    (close *socket-stream*))
  )

;;_____________________________________________________________________________
;;
;;  CEL implementation of OWLAPI's OWLReasoner interface
;;_____________________________________________________________________________

(defmacro owlapi.clear-ontologies ()
  "public void clearOntologies ()"
  `(owlapi.clear-ontologies-f))
(defun owlapi.clear-ontologies-f ()
  ;; either of these functions should be used
  (release-all-cel-ontologies)
  (clear-tbox-f)
  '(:void))
;;_____________________________________________________________________________

(defmacro owlapi.unload-ontologies (list-of-list-of-axioms)
  "public void unloadOntologies (Set<OWLOntology>)"
  `(owlapi.unload-ontologies-f ',list-of-list-of-axioms))
(defun owlapi.unload-ontologies-f (list-of-list-of-axioms)  
  ;; multiple ontologies are just combined with a big union
  (declare (ignore list-of-list-of-axioms))
  '(:error :unsupported-reasoner-operation-exception))
;;_____________________________________________________________________________

(defmacro owlapi.load-ontologies (list-of-list-of-axioms)
  "public void loadOntologies (Set<OWLOntology>)"
  `(owlapi.load-ontologies-f ',list-of-list-of-axioms))
(defun owlapi.load-ontologies-f (list-of-list-of-axioms)  
  ;; multiple ontologies are just combined with a big union
  (dolist (list-of-axioms list-of-list-of-axioms)
    (when (eq (add-axioms-f list-of-axioms) :tbox-state-error)
      (return-from owlapi.load-ontologies-f
	'(:error :owl-reasoner-exception))
      ))
  '(:void))
;;_____________________________________________________________________________

(defmacro owlapi.load-ontology (list-of-axioms)
  "public void loadOntology (OWLOntology)"
  `(owlapi.load-ontology-f ',list-of-axioms))
(defun owlapi.load-ontology-f (list-of-axioms)  
  (cond
   ((eq (add-axioms-f list-of-axioms) :tbox-state-error)
    '(:error :owl-reasoner-exception))
   (t
    '(:void))
   ))
;;_____________________________________________________________________________

(defmacro owlapi.dispose ()
  "public void dispose ()"
  `(owlapi.dispose-f))
(defun owlapi.dispose-f ()
  (cond
   ((eq *cel-mode* :owlapi)
    (send-owlapi-response '(:owlapi-return :void))
    (close-owlapi-socket)
    (msg "OWLAPI connection has been closed properly.")
    (msg "Terminating the CEL reasoner...~%")
    (excl:exit))
   (t
    (release-all-cel-ontologies-f)
    (msg "No OWLAPI connection to be closed!")
    )))
;;_____________________________________________________________________________

(defmacro owlapi.classify ()
  "public void classify ()"
  `(owlapi.classify-f))
(defun owlapi.classify-f ()
  (classify-tbox-f)
  (request-hasse)
  (when *owlapi-verbose-msg*
    (output-hierarchy))
  '(:void))
;;_____________________________________________________________________________

(defmacro owlapi.realise ()
  "public void realise ()"
  `(owlapi.realise-f))
(defun owlapi.realise-f ()
  (classify-tbox-f)
  (request-hasse)
  '(:void))
;;_____________________________________________________________________________

(defmacro owlapi.is-classified ()
  "public boolean isClassified ()"
  `(owlapi.is-classified-f))
(defun owlapi.is-classified-f ()
  (list :return
	(if (q-tbox-classified-f) 
	    t
	  nil))
  )
;;_____________________________________________________________________________

(defmacro owlapi.is-realised ()
  "public boolean isRealised ()"
  `(owlapi.is-realised-f))
(defun owlapi.is-realised-f ()
  (list :return
	(if (ont-state? :taxonomized)
	    t
	  nil))
  )
;;_____________________________________________________________________________

(defmacro owlapi.is-consistent (&optional dummy-list-of-axioms)
  "public boolean isConsistent ()"
  `(owlapi.is-consistent-f))
(defun owlapi.is-consistent-f ()
  (list :return
	(if (and (q-tbox-consistent-f)
		 (q-abox-consistent-f))
	    t
	  nil))
  )
;;_____________________________________________________________________________

(defmacro owlapi.get-ancestor-classes (cname)
  "public Set<Set<OWLClass>> getAncestorClasses (OWLDescription)"
  `(owlapi.get-ancestor-classes-f ',cname))
(defun owlapi.get-ancestor-classes-f (cname)
  (list :return
	(q-ancestors-f cname :make-eclass t))
  )
;;_____________________________________________________________________________

(defmacro owlapi.get-descendant-classes (cname)
  "public Set<Set<OWLClass>> getDescendantClasses (OWLDescription)"
  `(owlapi.get-descendant-classes-f ',cname))
(defun owlapi.get-descendant-classes-f (cname)
  (list :return
	(q-descendants-f cname :make-eclass t))
  )
;;_____________________________________________________________________________

(defmacro owlapi.get-super-classes (cname)
  "public Set<Set<OWLClass>> getSuperClasses (OWLDescription)"
  `(owlapi.get-super-classes-f ',cname))
(defun owlapi.get-super-classes-f (cname)
  (list :return
	(q-parents-f cname :make-eclass t))
  )
;;_____________________________________________________________________________

(defmacro owlapi.get-sub-classes (cname)
  "public Set<Set<OWLClass>> getSubClasses (OWLDescription)"
  `(owlapi.get-sub-classes-f ',cname))
(defun owlapi.get-sub-classes-f (cname)
  (list :return
	(q-children-f cname :make-eclass t))
  )
;;_____________________________________________________________________________

(defmacro owlapi.is-sub-class-of (cname1 cname2)
  "public boolean isSubClassOf (OWLDescription OWLDescription)"
  `(owlapi.is-sub-class-of-f ',cname1 ',cname2))
(defun owlapi.is-sub-class-of-f (cname1 cname2)
  (list :return
	(if (q-concept-implies-f cname1 cname2)
	    t
	  nil))
  )
;;_____________________________________________________________________________

(defmacro owlapi.get-equivalent-classes (cname)
  "public Set<OWLClass> getEquivalentClasses (OWLDescription)"
  `(owlapi.get-equivalent-classes-f ',cname))
(defun owlapi.get-equivalent-classes-f (cname)
  (list :return
	(q-equivalents-f cname))
  )
;;_____________________________________________________________________________

(defmacro owlapi.is-equivalent-class (cname1 cname2)
  "public boolean isEquivalentClass (OWLDescription OWLDescription)"
  `(owlapi.is-equivalent-class-f ',cname1 ',cname2))
(defun owlapi.is-equivalent-class-f (cname1 cname2)
  (list :return
	(if (q-concept-equivalent-f cname1 cname2)
	    t
	  nil))
  )
;;_____________________________________________________________________________
  
(defmacro owlapi.get-inconsistent-classes ()
  "public Set<OWLClass> getInconsistentClasses ()"
  `(owlapi.get-inconsistent-classes-f))
(defun owlapi.get-inconsistent-classes-f ()
  (list :return
	(q-all-unsatisfiable-concepts-f))
  )
;;_____________________________________________________________________________
      
(defmacro owlapi.is-satisfiable (cname)
  "public boolean isSatisfiable (OWLDescription)"
  `(owlapi.is-satisfiable-f ',cname))
(defun owlapi.is-satisfiable-f (cname)
  (list :return
	(if (q-concept-satisfiable-f cname)
	    t
	  nil))
  )
;;_____________________________________________________________________________

(defmacro owlapi.get-individuals (cname direct?)
  "public Set<OWLIndividuals> getIndividuals (OWLDescription boolean)"
  `(owlapi.get-individuals-f ',cname ',direct?))
(defun owlapi.get-individuals-f (cname direct?)
  (list :return
	(if direct?
	    (q-concept-direct-instances-f cname)
	  (q-concept-instances-f cname))
	)
  )
;;_____________________________________________________________________________
 
(defmacro owlapi.get-types (ind direct?)
  "public Set<Set<OWLClass>> getTypes (OWLIndividual boolean)"
  `(owlapi.get-types-f ',ind ',direct?))
(defun owlapi.get-types-f (ind direct?)
  (list :return
	(if direct?
	    (q-individual-direct-types-f ind :make-eclass t)
	  (q-individual-types-f ind :make-eclass t))
	)
  )
;;_____________________________________________________________________________
 
(defmacro owlapi.has-type (ind cname)
  "public bool hasType (OWLIndividual OWLDescription)"
  `(owlapi.has-type-f ',ind ',cname))
(defun owlapi.has-type-f (ind cname)
  (list :return
	(if (q-instance-f ind cname)
	    t
	  nil))
  )
;;_____________________________________________________________________________










;;_____________________________________________________________________________
;;_____________________________________________________________________________
;;_____________________________________________________________________________


(defun test-server (socket stream)

  (require :sock)
  (unintern 'socket)
  (use-package 'acl-socket)
  
  (setq socket (make-socket :connect :passive :local-port 9934))
  (setq stream (accept-connection socket))
  
  (format stream "(classify)")
  (finish-output stream)
  (format stream "(dispose)")
  (finish-output stream)
  
  (close socket)
  
  )



(defun re-accept ()
  (close stream)
  (setq stream (accept-connection socket))
  )

(defun test-query (q-message)  
  (format stream q-message)
  (finish-output stream)
  (read stream nil :end-of-file)
  )