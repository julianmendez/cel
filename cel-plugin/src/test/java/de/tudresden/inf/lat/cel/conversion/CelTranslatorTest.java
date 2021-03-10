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
import java.net.URISyntaxException;
import java.util.Set;
import java.util.TreeSet;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLClassExpression;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLObjectProperty;
import org.semanticweb.owlapi.model.OWLOntologyManager;

import de.tudresden.inf.lat.jsexp.Sexp;
import de.tudresden.inf.lat.jsexp.SexpFactory;
import de.tudresden.inf.lat.jsexp.SexpParserException;

/**
 * This class tests the translation of OWL objects into S-expressions.
 * 
 * @author Julian Mendez
 */
public class CelTranslatorTest {

	@Test
	public void testAnd() throws CelTranslatorException, URISyntaxException, SexpParserException, IOException {
		CelTranslator translator = new CelTranslator();
		OWLOntologyManager ontologyManager = OWLManager.createOWLOntologyManager();
		OWLDataFactory dataFactory = ontologyManager.getOWLDataFactory();
		OWLClassExpression desc1 = dataFactory.getOWLClass(IRI.create("Friendly"));
		OWLClassExpression desc2 = dataFactory.getOWLClass(IRI.create("Intelligent"));
		OWLClassExpression desc3 = dataFactory.getOWLClass(IRI.create("Person"));
		Set<OWLClassExpression> set = new TreeSet<>();
		set.add(desc1);
		set.add(desc2);
		set.add(desc3);
		OWLClassExpression description = dataFactory.getOWLObjectIntersectionOf(set);
		Sexp translated = translator.translate(description);
		Sexp expected = SexpFactory.parse("(and |Friendly| |Intelligent| |Person|)");
		Assertions.assertEquals(expected, translated);
	}

	@Test
	public void testNestedConcepts()
			throws CelTranslatorException, URISyntaxException, SexpParserException, IOException {
		CelTranslator translator = new CelTranslator();
		OWLOntologyManager ontologyManager = OWLManager.createOWLOntologyManager();
		OWLDataFactory dataFactory = ontologyManager.getOWLDataFactory();
		OWLClassExpression desc1 = dataFactory.getOWLClass(IRI.create("Person"));
		OWLObjectProperty role = dataFactory.getOWLObjectProperty(IRI.create("has-child"));
		OWLClassExpression desc2_1_1 = dataFactory.getOWLClass(IRI.create("Friendly"));
		OWLClassExpression desc2_1_2 = dataFactory.getOWLClass(IRI.create("Intelligent"));
		OWLClassExpression desc2_1_3 = dataFactory.getOWLClass(IRI.create("Person"));
		Set<OWLClassExpression> set2_1 = new TreeSet<>();
		set2_1.add(desc2_1_1);
		set2_1.add(desc2_1_2);
		set2_1.add(desc2_1_3);
		OWLClassExpression desc2_1 = dataFactory.getOWLObjectIntersectionOf(set2_1);

		OWLClassExpression desc2 = dataFactory.getOWLObjectSomeValuesFrom(role, desc2_1);
		Set<OWLClassExpression> set = new TreeSet<>();
		set.add(desc1);
		set.add(desc2);
		OWLClassExpression description = dataFactory.getOWLObjectIntersectionOf(set);
		Sexp translated = translator.translate(description);
		Sexp expected = SexpFactory.parse("(and |Person| (some |has-child| (and |Friendly| |Intelligent| |Person|)))");
		Assertions.assertEquals(expected, translated);
	}

	@Test
	public void testSome() throws URISyntaxException, CelTranslatorException, SexpParserException, IOException {
		CelTranslator translator = new CelTranslator();
		OWLOntologyManager ontologyManager = OWLManager.createOWLOntologyManager();
		OWLDataFactory dataFactory = ontologyManager.getOWLDataFactory();
		OWLObjectProperty role = dataFactory.getOWLObjectProperty(IRI.create("has-child"));
		OWLClassExpression desc1 = dataFactory.getOWLClass(IRI.create("Person"));
		OWLClassExpression description = dataFactory.getOWLObjectSomeValuesFrom(role, desc1);
		Sexp translated = translator.translate(description);
		Sexp expected = SexpFactory.parse("(some |has-child| |Person|)");
		Assertions.assertEquals(expected, translated);
	}

	@Test
	public void testTopAndBottom() throws CelTranslatorException, URISyntaxException {
		CelTranslator translator = new CelTranslator();
		OWLOntologyManager ontologyManager = OWLManager.createOWLOntologyManager();
		OWLDataFactory dataFactory = ontologyManager.getOWLDataFactory();
		OWLClassExpression description = dataFactory.getOWLThing();
		Sexp translated = translator.translate(description);
		Sexp expected = SexpFactory.newAtomicSexp("top");
		Assertions.assertEquals(expected, translated);
		description = dataFactory.getOWLNothing();
		translated = translator.translate(description);
		expected = SexpFactory.newAtomicSexp("bottom");
		Assertions.assertEquals(expected, translated);
	}

}
