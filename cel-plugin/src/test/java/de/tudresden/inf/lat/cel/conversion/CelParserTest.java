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

/**
 * This class tests the parsing of OWL objects from S-expressions.
 * 
 * @author Julian Mendez
 */
public class CelParserTest {

	@Test
	public void testVal() throws CelParserException, URISyntaxException {
		OWLOntologyManager ontologyManager = OWLManager.createOWLOntologyManager();
		OWLDataFactory dataFactory = ontologyManager.getOWLDataFactory();
		String concept = "(and Human (some has-child Human))";
		CelParser parser = new CelParser();
		OWLClassExpression desc1 = dataFactory.getOWLClass(IRI.create("Human"));
		OWLObjectProperty role2_1 = dataFactory.getOWLObjectProperty(IRI.create("has-child"));
		OWLClassExpression desc2_2 = dataFactory.getOWLClass(IRI.create("Human"));
		OWLClassExpression desc2 = dataFactory.getOWLObjectSomeValuesFrom(role2_1, desc2_2);
		Set<OWLClassExpression> set = new TreeSet<>();
		set.add(desc1);
		set.add(desc2);
		OWLClassExpression expected = dataFactory.getOWLObjectIntersectionOf(set);
		OWLClassExpression parsed = parser.parse(concept, dataFactory);
		Assertions.assertEquals(expected.toString(), parsed.toString());
	}

}
