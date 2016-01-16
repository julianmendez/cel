/*
 * Copyright 2009 Julian Mendez
 *
 *
 * This file is part of CEL Plug-in.
 *
 * CEL Plug-in is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * CEL Plug-in is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with CEL Plug-in.  If not, see <http://www.gnu.org/licenses/>.
 *
 */

package de.tudresden.inf.lat.cel.protege;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.io.OWLRendererException;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyCreationException;
import org.semanticweb.owlapi.model.OWLOntologyManager;

import de.tudresden.inf.lat.cel.owlapi.CelReasoner;
import de.tudresden.inf.lat.cel.owlapi.OWLReasonerXMLOutput;

/**
 * This class starts the CEL Plug-in from a console.
 *
 * @author Julian Mendez
 *
 */
public class ConsoleStarter {

	private static final Logger logger = Logger.getLogger("de.tudresden.inf.lat.cel");

	/**
	 * Starts a classifier instance from the command line.
	 *
	 * @param args
	 *            a list containing the command line parameters, they are first
	 *            parameter: input file (required), second parameter: output
	 *            file (required), third parameter: log level (optional)
	 * @throws OWLOntologyCreationException
	 *             if the ontology could not be created
	 * @throws OWLRendererException
	 *             if a renderer error occurs
	 * @throws FileNotFoundException
	 *             if a file was not found
	 */
	public static void main(String[] args)
			throws OWLRendererException, OWLOntologyCreationException, FileNotFoundException {
		boolean helpNeeded = true;
		ConsoleStarter instance = new ConsoleStarter();
		if (args.length > 1) {
			helpNeeded = false;
			Level logLevel = Level.FINE;
			if (args.length > 2) {
				logLevel = Level.parse(args[2]);
			}
			instance.start(new File(args[0]), new File(args[1]), logLevel);
			instance.stop();
		}
		if (helpNeeded) {
			System.out.println(instance.minihelp);
		}
	}

	/** A very small help about how to start a new instance. */
	private final String minihelp = "\nUsage:\njava -cp .:<list of jars> " + this.getClass().getCanonicalName()
			+ " <input ontology file name> <inferred data file name> [<log level>]\n";

	private CelReasoner reasoner = null;

	public ConsoleStarter() {
	}

	public String getMiniHelp() {
		return this.minihelp;
	}

	/**
	 * Executes the classifier on a given ontology.
	 *
	 * @param ontologyFile
	 *            ontology file to be classified
	 * @param inferredFile
	 *            file to write the inferred data
	 * @param logLevel
	 *            log level
	 * @throws OWLOntologyCreationException
	 *             if the ontology could not be created
	 * @throws OWLRendererException
	 *             if a renderer error occurs
	 * @throws FileNotFoundException
	 *             if a file was not found
	 */
	public void start(File ontologyFile, File inferredFile, Level logLevel)
			throws OWLOntologyCreationException, OWLRendererException, FileNotFoundException {
		OWLOntologyManager manager = OWLManager.createOWLOntologyManager();
		OWLOntology ontology = manager.loadOntologyFromOntologyDocument(ontologyFile);

		logger.setLevel(logLevel);
		this.reasoner = new CelReasoner(ontology);
		this.reasoner.precomputeInferences();

		OWLReasonerXMLOutput xmlDoc = new OWLReasonerXMLOutput(this.reasoner);
		xmlDoc.toXML(new FileOutputStream(inferredFile));
	}

	public void stop() {
		this.reasoner.dispose();
	}

}
