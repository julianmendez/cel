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

import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.net.URI;
import java.util.HashSet;
import java.util.Set;

import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.TransformerException;

import org.semanticweb.owl.apibinding.OWLManager;
import org.semanticweb.owl.inference.OWLReasonerException;
import org.semanticweb.owl.model.OWLOntology;
import org.semanticweb.owl.model.OWLOntologyCreationException;
import org.semanticweb.owl.model.OWLOntologyManager;
import org.w3c.dom.DOMException;

import de.tudresden.inf.lat.cel.owlapi.CelReasoner;
import de.tudresden.inf.lat.cel.owlapi.OWLReasonerXMLOutput;

/**
 * This class starts the CEL Plug-in from a console.
 * 
 * @author Julian Mendez
 * 
 */
public class ConsoleStarter {

	public static void main(String[] args) throws OWLReasonerException,
			OWLOntologyCreationException, SecurityException, IOException,
			DOMException, ParserConfigurationException, TransformerException {
		boolean helpNeeded = true;
		ConsoleStarter instance = new ConsoleStarter();
		if (args.length > 0) {
			helpNeeded = false;
			if (args.length > 1) {
				instance.setOutput(new FileOutputStream(args[1]));
			}
			instance.start(args[0]);
			instance.stop();
		}
		if (helpNeeded) {
			System.out.println(instance.getMiniHelp());
		}
	}

	private String minihelp = "\nUsage:\njava -cp .:<list of jars> "
			+ this.getClass().getCanonicalName()
			+ " <input ontology> [<output file name>]\n";

	private OutputStream output = System.out;
	private CelReasoner reasoner = null;

	public ConsoleStarter() {
	}

	public String getMiniHelp() {
		return this.minihelp;
	}

	public OutputStream getOutput() {
		return this.output;
	}

	/**
	 * @throws IOException
	 * @throws SecurityException
	 */
	public void initialise() throws SecurityException, IOException {
	}

	public void setOutput(OutputStream out) {
		this.output = out;
	}

	public void start(String ontologyFilename) throws OWLReasonerException,
			OWLOntologyCreationException, SecurityException, IOException,
			DOMException, ParserConfigurationException, TransformerException {
		initialise();

		OWLOntologyManager manager = OWLManager.createOWLOntologyManager();
		URI physicalURI = URI.create("file:" + ontologyFilename);
		OWLOntology ontology = manager.loadOntologyFromPhysicalURI(physicalURI);

		this.reasoner = new CelReasoner(manager);

		Set<OWLOntology> ontologySet = new HashSet<OWLOntology>();
		ontologySet.add(ontology);
		this.reasoner.loadOntologies(ontologySet);
		this.reasoner.classify();

		OWLReasonerXMLOutput xmlDoc = new OWLReasonerXMLOutput(this.reasoner,
				manager.getOWLDataFactory().getOWLNothing(), manager
						.getOWLDataFactory().getOWLThing());
		xmlDoc.toXML(output);
	}

	public void stop() throws OWLReasonerException {
		this.reasoner.dispose();
	}
}
