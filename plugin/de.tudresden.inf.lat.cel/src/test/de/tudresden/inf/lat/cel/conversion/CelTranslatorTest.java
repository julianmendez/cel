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

package de.tudresden.inf.lat.cel.conversion;

import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.Set;
import java.util.TreeSet;

import junit.framework.TestCase;

import org.semanticweb.owl.apibinding.OWLManager;
import org.semanticweb.owl.model.OWLDataFactory;
import org.semanticweb.owl.model.OWLDescription;
import org.semanticweb.owl.model.OWLObjectProperty;
import org.semanticweb.owl.model.OWLOntologyManager;

import de.tudresden.inf.lat.jsexp.Sexp;
import de.tudresden.inf.lat.jsexp.SexpFactory;
import de.tudresden.inf.lat.jsexp.SexpParserException;

/**
 * This class tests the translation of OWL objects into S-expressions.
 * 
 * @author Julian Mendez
 */
public class CelTranslatorTest extends TestCase {

	public void testAnd() throws CelTranslatorException, URISyntaxException,
			SexpParserException, IOException {
		CelTranslator translator = new CelTranslator();
		OWLOntologyManager ontologyManager = OWLManager
				.createOWLOntologyManager();
		OWLDataFactory dataFactory = ontologyManager.getOWLDataFactory();
		OWLDescription desc1 = dataFactory.getOWLClass(new URI("Friendly"));
		OWLDescription desc2 = dataFactory.getOWLClass(new URI("Intelligent"));
		OWLDescription desc3 = dataFactory.getOWLClass(new URI("Person"));
		Set<OWLDescription> set = new TreeSet<OWLDescription>();
		set.add(desc1);
		set.add(desc2);
		set.add(desc3);
		OWLDescription description = dataFactory
				.getOWLObjectIntersectionOf(set);
		Sexp translated = translator.translate(description);
		Sexp expected = SexpFactory
				.parse("(and |Friendly| |Intelligent| |Person|)");
		assertEquals(expected, translated);
	}

	public void testNestedConcepts() throws CelTranslatorException,
			URISyntaxException, SexpParserException, IOException {
		CelTranslator translator = new CelTranslator();
		OWLOntologyManager ontologyManager = OWLManager
				.createOWLOntologyManager();
		OWLDataFactory dataFactory = ontologyManager.getOWLDataFactory();
		OWLDescription desc1 = dataFactory.getOWLClass(new URI("Person"));
		OWLObjectProperty role = dataFactory.getOWLObjectProperty(new URI(
				"has-child"));
		OWLDescription desc2_1_1 = dataFactory.getOWLClass(new URI("Friendly"));
		OWLDescription desc2_1_2 = dataFactory.getOWLClass(new URI(
				"Intelligent"));
		OWLDescription desc2_1_3 = dataFactory.getOWLClass(new URI("Person"));
		Set<OWLDescription> set2_1 = new TreeSet<OWLDescription>();
		set2_1.add(desc2_1_1);
		set2_1.add(desc2_1_2);
		set2_1.add(desc2_1_3);
		OWLDescription desc2_1 = dataFactory.getOWLObjectIntersectionOf(set2_1);

		OWLDescription desc2 = dataFactory.getOWLObjectSomeRestriction(role,
				desc2_1);
		Set<OWLDescription> set = new TreeSet<OWLDescription>();
		set.add(desc1);
		set.add(desc2);
		OWLDescription description = dataFactory
				.getOWLObjectIntersectionOf(set);
		Sexp translated = translator.translate(description);
		Sexp expected = SexpFactory
				.parse("(and |Person| (some |has-child| (and |Friendly| |Intelligent| |Person|)))");
		assertEquals(expected, translated);
	}

	public void testSome() throws URISyntaxException, CelTranslatorException,
			SexpParserException, IOException {
		CelTranslator translator = new CelTranslator();
		OWLOntologyManager ontologyManager = OWLManager
				.createOWLOntologyManager();
		OWLDataFactory dataFactory = ontologyManager.getOWLDataFactory();
		OWLObjectProperty role = dataFactory.getOWLObjectProperty(new URI(
				"has-child"));
		OWLDescription desc1 = dataFactory.getOWLClass(new URI("Person"));
		OWLDescription description = dataFactory.getOWLObjectSomeRestriction(
				role, desc1);
		Sexp translated = translator.translate(description);
		Sexp expected = SexpFactory.parse("(some |has-child| |Person|)");
		assertEquals(expected, translated);
	}

	public void testTopAndBottom() throws CelTranslatorException,
			URISyntaxException {
		CelTranslator translator = new CelTranslator();
		OWLOntologyManager ontologyManager = OWLManager
				.createOWLOntologyManager();
		OWLDataFactory dataFactory = ontologyManager.getOWLDataFactory();
		OWLDescription description = dataFactory.getOWLThing();
		Sexp translated = translator.translate(description);
		Sexp expected = SexpFactory.newAtomicSexp("top");
		assertEquals(expected, translated);
		description = dataFactory.getOWLNothing();
		translated = translator.translate(description);
		expected = SexpFactory.newAtomicSexp("bottom");
		assertEquals(expected, translated);
	}
}
