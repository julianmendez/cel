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

import java.util.logging.Level;
import java.util.logging.Logger;

import org.protege.editor.owl.model.inference.ProtegeOWLReasonerFactoryAdapter;
import org.semanticweb.owl.inference.OWLReasoner;
import org.semanticweb.owl.model.OWLOntologyManager;

import de.tudresden.inf.lat.cel.owlapi.CelReasoner;

/**
 * This is the main class invoked by Protege.
 * 
 * @author Julian Mendez
 */
public class CelReasonerFactory extends ProtegeOWLReasonerFactoryAdapter {

	private static final Logger logger = Logger
			.getLogger("de.tudresden.inf.lat.cel");

	/**
	 * This constructor is invoked when the CEL reasoner is selected in Protege.
	 */
	public OWLReasoner createReasoner(OWLOntologyManager owlOntologyManager) {
		OWLReasoner ret = new CelReasoner(owlOntologyManager);
		return ret;
	}

	public void dispose() throws Exception {
	}

	/**
	 * This function is invoked when protege starts its execution. The logger
	 * and logging level is defined here.
	 * 
	 * @see org.protege.editor.core.plugin.ProtegePluginInstance#initialise()
	 */
	public void initialise() throws Exception {
		logger.setLevel(Level.INFO);
	}

	/**
	 * This is value is hard coded to true.
	 * 
	 * @return always true
	 */
	public boolean requiresExplicitClassification() {
		return true;
	}
}
