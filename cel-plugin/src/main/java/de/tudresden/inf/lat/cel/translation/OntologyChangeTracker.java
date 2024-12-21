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

package de.tudresden.inf.lat.cel.translation;

import java.util.List;
import java.util.logging.Logger;

import org.semanticweb.owlapi.model.OWLException;
import org.semanticweb.owlapi.model.OWLOntologyChange;
import org.semanticweb.owlapi.model.OWLOntologyChangeListener;

/**
 * This class is a listener that keeps track about if a particular ontology has
 * changed.
 * 
 * @author Julian Mendez
 */
class OntologyChangeTracker implements OWLOntologyChangeListener {

	private static final Logger logger = Logger.getLogger(OntologyChangeTracker.class.getName());

	/** Tells whether the ontology has changed. */
	private boolean ontologyChanged = true;

	public OntologyChangeTracker() {
	}

	/**
	 * @return true if the ontology has changed.
	 */
	public boolean getOntologyChanged() {
		return this.ontologyChanged;
	}

	/**
	 * This is invoked when the ontology has changed.
	 */
	@Override
	public void ontologiesChanged(List<? extends OWLOntologyChange> arg0) {
		logger.fine("The ontologies have changed.");
		this.ontologyChanged = true;
	}

	/**
	 * Forces a state: (true) changed or (false) unchanged.
	 * 
	 * @param status
	 *            new value for the tracker.
	 */
	public void setOntologyChanged(boolean status) {
		this.ontologyChanged = status;
	}
}
