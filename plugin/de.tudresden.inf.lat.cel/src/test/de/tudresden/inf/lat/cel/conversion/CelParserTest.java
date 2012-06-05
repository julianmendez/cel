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

/**
 * This class tests the parsing of OWL objects from S-expressions.
 * 
 * @author Julian Mendez
 */
public class CelParserTest extends TestCase {

	public void testVal() throws CelParserException, URISyntaxException {
		OWLOntologyManager ontologyManager = OWLManager
				.createOWLOntologyManager();
		OWLDataFactory dataFactory = ontologyManager.getOWLDataFactory();
		String concept = "(and Human (some has-child Human))";
		CelParser parser = new CelParser();
		OWLDescription desc1 = dataFactory.getOWLClass(new URI("Human"));
		OWLObjectProperty role2_1 = dataFactory.getOWLObjectProperty(new URI(
				"has-child"));
		OWLDescription desc2_2 = dataFactory.getOWLClass(new URI("Human"));
		OWLDescription desc2 = dataFactory.getOWLObjectSomeRestriction(role2_1,
				desc2_2);
		Set<OWLDescription> set = new TreeSet<OWLDescription>();
		set.add(desc1);
		set.add(desc2);
		OWLDescription expected = dataFactory.getOWLObjectIntersectionOf(set);
		OWLDescription parsed = parser.parse(concept, dataFactory);
		assertEquals(expected.toString(), parsed.toString());
	}
}
