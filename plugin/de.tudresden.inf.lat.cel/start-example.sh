#!/bin/bash

#
# This is an example of how to start the CEL Plug-in from the command line.
# In this example, the CEL Plug-in classifies the file "ontology.owl" and stores
# the resulting output in the file "inferredOntology.xml".
#


java -Xmx1g -cp .:../cel-jars/lib/org.semanticweb.owl/jars/owlapi-bin.jar\
:../cel-jars/lib/de.tudresden.inf.lat.jsexp/jars/jsexp-0.1.0.jar\
:dist/bundle/de.tudresden.inf.lat.cel.jar\
  de.tudresden.inf.lat.cel.protege.ConsoleStarter ontology.owl inferredOntology.xml

