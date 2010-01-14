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

package de.tudresden.inf.lat.cel.owlapi;

import java.util.Collections;
import java.util.Set;
import java.util.TreeSet;

import org.semanticweb.owl.inference.OWLReasoner;
import org.semanticweb.owl.inference.OWLReasonerException;
import org.semanticweb.owl.model.OWLObjectProperty;


/**
 * 
 * @author Julian Mendez
 */
public class XMLGraphOWLObjectProperty extends XMLGraph<OWLObjectProperty> {

	protected static final String xmlMainTag = "properties";

	private Set<OWLObjectProperty> elements = null;
	private OWLReasoner reasoner = null;

	public XMLGraphOWLObjectProperty(OWLReasoner owlReasoner) {
		this.reasoner = owlReasoner;
		if (owlReasoner != null && owlReasoner.getLoadedOntologies().size() > 0) {
			this.elements = new TreeSet<OWLObjectProperty>();
			this.elements.addAll(owlReasoner.getLoadedOntologies().iterator()
					.next().getObjectPropertiesInSignature());
		} else {
			throw new IllegalArgumentException(
					"The reasoner does not have any ontology.");
		}
	}

	@Override
	public Set<OWLObjectProperty> getElements() {
		return Collections.unmodifiableSet(this.elements);
	}

	@Override
	public Set<OWLObjectProperty> getEquivalentElements(OWLObjectProperty orig)
			throws OWLReasonerException {
		return this.reasoner.getEquivalentProperties(orig);
	}

	@Override
	public String getMainTag() {
		return xmlMainTag;
	}

	@Override
	public Set<Set<OWLObjectProperty>> getSubElements(OWLObjectProperty orig)
			throws OWLReasonerException {
		return this.reasoner.getSubProperties(orig);
	}
}
