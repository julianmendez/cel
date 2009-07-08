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
;;;; Last Modified: 2009-07-08
;;;; Note the T/C in LICENSE.txt
;;_____________________________________________________________________________

(in-package cel-system)


(defconstant *command-line-help-text*
    "
Usage: cel [options]

If no options are given, CEL starts up and prompts for interactive commands.
The following options can be used:
   -loadOntology file  <-l>    Load and preprocess ontology from file
   -classifyOntology <-c>      Classify the ontology
   -outputSupers [file]        Output the sets of all super-classes
   -outputTaxonomy [file]      Output the direct sub- and super-classes
   -outputHierarchy [file]     Output the hierarchy as an indented tree
   -digServer [port]           Start CEL as a backend DIG reasoner
   -owlapiServer host port     Start CEL as a backend OWLAPI-like reasoner
   -quit <-q>                  Exit; Don't enter the interactive interface
   -help <-h>                  Display this help message
  
Examples:
 $ cel -loadOntology med.tbox -classifyOntology -outputTaxonomy med.dag -quit
 To load and classify the ontology \"med.tbox\", and to output the DAG to \"med.dag\"
 $ cel -loadOntology gene.tbox
 To start the interactive CEL interface with the initial ontology \"gene.tbox\" loaded and preprocessed
")


(defconstant *greeting-text*    
    (format nil 	    
	    "
   * * * * * * * * * * *  CEL Version ~A  * * * * * * * * * * * *
   *                                                               *
   *  CEL: [C]lassifier for the Description Logic [E][L]+          *
   *  Supported description logic: EL+  (el+)                      *
   *  Copyright (C) 2005-2009: B. Suntisrivaraporn and TU Dresden. *
   *  CEL comes with ABSOLUTELY NO WARRANTY; use at your own risk. *
   *  This is free software for research and evaluation purposes.  *
   *  Commercial use is prohibited; please contact the author.     *
   *                                                               *
   *  Try (help) to see what CEL can do!                           *
   *                                                               *
   * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *~%~%"
	    *cel-version*))


(defconstant *el+*
    (format nil
	    "
The description logic (DL) EL is a small concept language which has two essential concept constructs, namely, concept conjunctions (`and') and existential restrictions (`some'). The name EL+ embraces the expressivity of this base concept language and far beyond. In terms of the concept language, EL+ allows you to also use the top concept (`TOP') and the bottom concept (`BOTTOM') which are very useful in practice when combined with so-called general concept inclusions or GCIs.

Besides the concept language, EL+ covers the notion of ontology language which is used to formulate definitions on and/or restrictions on concepts, properties of and relationship among roles, and maybe also facts about individuals. An EL+ ontology consists of two parts, TBox and ABox. It allows to use a general TBox which is a set of primitive concept definition (`define-primitive-concept'), full concept definition (`define-concept'), general concept inclusions (`implies'), concept disjointness axioms (`disjoint'), domain and range restrictions on roles (`define-primitive-roles'), role hierarchy axioms, role transitivity, as well as, reflexivity declarations, left- and right-identity rules. An ABox is used to describe assertional knowledge about individuals, consisting of concept assertions (`instance') and role assertion (`related'). CEL does not require the two parts of an ontology to be seperated, nor does it assume that an ABox exist at all. CEL, however, does not allow name punning between concept and individual name spaces. 

It is worth noting that using all the expressivity available in CEL will lead to computational undecidability. For this reason, we define two fragments of EL+, for both of which polytime reasoning is available. For information of these two varients, type (el+ :role-inclusion t) and (el+ :range-restriction t).

Reasoning services CEL provides include TBox classification and ABox realization. These allow us to cast a number of standard queries. Amongst others are `parents', `children', `ancestors', `descendants', `satisfiable?', `individual-direct-types', and `concept-instances'.

For details of these functional interfacing macros, use the interactive help command or refer to the CEL user manual.
"
	    ))

(defconstant *el+ri*
    (format nil
	    "
Without range restriction, you can use complex role inclusions in any arbitary forms, including right- and left- identity rules.
"
	    ))

(defconstant *el+rr*
    (format nil
	    "
If you need expressivity range restriction, you need to sacrifice complex role inclusions. This, however, does not exclude role hierarchy axioms, reflexive and transitive roles.
"
	    ))