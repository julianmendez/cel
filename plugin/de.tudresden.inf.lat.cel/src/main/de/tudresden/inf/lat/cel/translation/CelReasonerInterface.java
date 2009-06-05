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

import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import org.semanticweb.owl.inference.OWLReasonerException;
import org.semanticweb.owl.model.OWLClass;
import org.semanticweb.owl.model.OWLDescription;
import org.semanticweb.owl.model.OWLIndividual;
import org.semanticweb.owl.model.OWLObjectProperty;
import org.semanticweb.owl.model.OWLObjectPropertyExpression;
import org.semanticweb.owl.model.OWLOntology;
import org.semanticweb.owl.model.OWLOntologyManager;

import de.tudresden.inf.lat.cel.connection.CelConnectionException;
import de.tudresden.inf.lat.cel.connection.CelSocketManager;
import de.tudresden.inf.lat.cel.conversion.CelParser;
import de.tudresden.inf.lat.cel.conversion.CelParserException;
import de.tudresden.inf.lat.cel.conversion.CelTranslator;
import de.tudresden.inf.lat.cel.conversion.CelTranslatorException;
import de.tudresden.inf.lat.cel.conversion.LispKeyword;
import de.tudresden.inf.lat.jsexp.Sexp;
import de.tudresden.inf.lat.jsexp.SexpFactory;

/**
 * This class provides all the implemented methods for using the CEL reasoner.
 * 
 * @author Julian Mendez
 */
public class CelReasonerInterface {

	// private static final Logger logger = Logger.getAnonymousLogger();

	private static final String nothing = "Nothing";
	private static final String thing = "Thing";
	private OntologyChangeTracker changeTracker = new OntologyChangeTracker();
	private Set<OWLOntology> ontologies = new HashSet<OWLOntology>();
	private OWLOntologyManager ontologyManager = null;
	private CelParser parser = new CelParser();
	private CelSocketManager socketManager = new CelSocketManager();
	private CelTranslator translator = new CelTranslator();

	public CelReasonerInterface(OWLOntologyManager manager) {
		this.ontologyManager = manager;
		this.ontologyManager.addOntologyChangeListener(this.changeTracker);
	}

	public void classify() throws CelReasonerException {
		synchronizedIfChanged();
		Sexp message = SexpFactory.newNonAtomicSexp();
		message.add(SexpFactory.newAtomicSexp(CelOwlApiKeyword.keyClassify));
		send(message, "Classifying ontology ...");
	}

	/**
	 * The command only deletes the current ontology.
	 * 
	 * @throws OWLReasonerException
	 */
	public void clearOntologies() throws CelReasonerException {
		Sexp message = SexpFactory.newNonAtomicSexp();
		message.add(SexpFactory
				.newAtomicSexp(CelOwlApiKeyword.keyClearOntologies));
		send(message, "Clearing ontologies ...");
		this.ontologies = new HashSet<OWLOntology>();
	}

	public void dispose() {
		if (getSocketManager() != null) {
			Sexp message = SexpFactory.newNonAtomicSexp();
			message.add(SexpFactory.newAtomicSexp(CelOwlApiKeyword.keyDispose));
			getSocketManager().stopExecution(message);
		}
	}

	public Set<Set<OWLClass>> getAncestorClasses(OWLDescription description)
			throws CelReasonerException {
		return getSetOfSetOfClasses(CelOwlApiKeyword.keyGetAncestorClasses,
				description);
	}

	public Set<Set<OWLObjectProperty>> getAncestorProperties(
			OWLObjectProperty arg0) throws NotImplementedOperationException {
		throw new NotImplementedOperationException();
	}

	public Set<Set<OWLClass>> getDescendantClasses(OWLDescription description)
			throws CelReasonerException {
		return getSetOfSetOfClasses(CelOwlApiKeyword.keyGetDescendantClasses,
				description);
	}

	public Set<Set<OWLObjectProperty>> getDescendantProperties(
			OWLObjectProperty arg0) throws NotImplementedOperationException {
		throw new NotImplementedOperationException();
	}

	public Set<Set<OWLDescription>> getDomains(OWLObjectProperty property)
			throws CelReasonerException {
		Set<Set<OWLDescription>> ret = null;
		Sexp message = SexpFactory.newNonAtomicSexp();
		message.add(SexpFactory.newAtomicSexp(CelOwlApiKeyword.keyGetDomains));
		message.add(getTranslator().translate(property));
		Sexp response = send(message);
		try {
			ret = getParser().parseSetOfSetOfDescriptions(response,
					getOWLOntologyManager().getOWLDataFactory());
		} catch (CelParserException e) {
			throw new CelReasonerException(e);
		}
		return ret;
	}

	public Set<OWLClass> getEquivalentClasses(OWLDescription description)
			throws CelReasonerException {
		Set<OWLClass> ret = null;
		Sexp message = SexpFactory.newNonAtomicSexp();
		try {
			message.add(SexpFactory
					.newAtomicSexp(CelOwlApiKeyword.keyGetEquivalentClasses));
			message.add(getTranslator().translate(description));
		} catch (CelTranslatorException e) {
			throw new CelReasonerException(e);
		}
		Sexp response = send(message);
		ret = getParser().parseSetOfClasses(response,
				getOWLOntologyManager().getOWLDataFactory());
		return ret;
	}

	public Set<OWLObjectProperty> getEquivalentProperties(
			OWLObjectProperty property) throws NotImplementedOperationException {
		throw new NotImplementedOperationException();
	}

	/**
	 * @return the set of inconsistent classes
	 * @throws CelReasonerException
	 */
	public Set<OWLClass> getInconsistentClasses() throws CelReasonerException {
		Set<OWLClass> ret = new HashSet<OWLClass>();
		Sexp message = SexpFactory.newNonAtomicSexp();
		message.add(SexpFactory
				.newAtomicSexp(CelOwlApiKeyword.keyGetInconsistentClasses));
		Sexp expr = send(message);
		if (expr.toString().equalsIgnoreCase(LispKeyword.lispNil)) {
			expr = SexpFactory.newNonAtomicSexp();
		}
		ret = getParser().parseSetOfClasses(expr,
				getOWLOntologyManager().getOWLDataFactory());
		return ret;
	}

	public Set<OWLIndividual> getIndividuals(OWLDescription description,
			boolean arg) throws CelReasonerException {
		Set<OWLIndividual> ret = new HashSet<OWLIndividual>();
		Sexp message = SexpFactory.newNonAtomicSexp();
		message.add(SexpFactory
				.newAtomicSexp(CelOwlApiKeyword.keyGetIndividuals));
		try {
			message.add(getTranslator().translate(description));
		} catch (CelTranslatorException e) {
			throw new CelReasonerException(e);
		}
		message.add(getTranslator().translate(arg));
		Sexp expr = send(message);
		if (expr.toString().equalsIgnoreCase(LispKeyword.lispNil)) {
			expr = SexpFactory.newNonAtomicSexp();
		}
		ret = getParser().parseSetOfIndividuals(expr,
				getOWLOntologyManager().getOWLDataFactory());
		return ret;
	}

	public Set<OWLOntology> getLoadedOntologies() {
		return this.ontologies;
	}

	public Map<OWLObjectProperty, Set<OWLIndividual>> getObjectPropertyRelationships(
			OWLIndividual individual) throws NotImplementedOperationException {
		throw new NotImplementedOperationException();
	}

	protected Set<OWLOntology> getOntologies() {
		return this.ontologies;
	}

	public OWLOntologyManager getOWLOntologyManager() {
		return this.ontologyManager;
	}

	public CelParser getParser() {
		return this.parser;
	}

	public Set<OWLDescription> getRanges(OWLObjectProperty property)
			throws CelReasonerException {
		Set<OWLDescription> ret = null;
		Sexp message = SexpFactory.newNonAtomicSexp();
		message.add(SexpFactory.newAtomicSexp(CelOwlApiKeyword.keyGetRanges));
		message.add(getTranslator().translate(property));
		Sexp response = send(message);
		try {
			ret = getParser().parseSetOfDescriptions(response,
					getOWLOntologyManager().getOWLDataFactory());
		} catch (CelParserException e) {
			throw new CelReasonerException(e);
		}
		return ret;
	}

	public Set<OWLIndividual> getRelatedIndividuals(OWLIndividual individual,
			OWLObjectPropertyExpression property) throws CelReasonerException {
		Sexp message = SexpFactory.newNonAtomicSexp();
		message.add(SexpFactory
				.newAtomicSexp(CelOwlApiKeyword.keyGetRelatedIndividuals));
		message.add(getTranslator().translate(individual));
		message.add(getTranslator().translate(property));
		Sexp response = send(message);
		Set<OWLIndividual> ret = getParser().parseSetOfIndividuals(response,
				getOWLOntologyManager().getOWLDataFactory());
		return ret;
	}

	protected Set<OWLObjectProperty> getSetOfProperties(String command,
			OWLDescription description) throws CelReasonerException {
		Sexp message = SexpFactory.newNonAtomicSexp();
		try {
			message.add(SexpFactory.newAtomicSexp(command));
			message.add(getTranslator().translate(description));
		} catch (CelTranslatorException e) {
			throw new CelReasonerException(e);
		}
		Sexp response = send(message);
		Set<OWLObjectProperty> ret = getParser().parseSetOfProperties(response,
				getOWLOntologyManager().getOWLDataFactory());
		return ret;
	}

	protected Set<Set<OWLClass>> getSetOfSetOfClasses(String command,
			OWLDescription description) throws CelReasonerException {
		Sexp message = SexpFactory.newNonAtomicSexp();
		try {
			message.add(SexpFactory.newAtomicSexp(command));
			message.add(getTranslator().translate(description));
		} catch (CelTranslatorException e) {
			throw new CelReasonerException(e);
		}
		Sexp response = send(message);
		Set<Set<OWLClass>> ret = getParser().parseSetOfSetOfClasses(response,
				getOWLOntologyManager().getOWLDataFactory());
		return ret;
	}

	protected Set<Set<OWLObjectProperty>> getSetOfSetOfProperties(
			String command, OWLObjectProperty property)
			throws CelReasonerException {
		Sexp message = SexpFactory.newNonAtomicSexp();
		message.add(SexpFactory.newAtomicSexp(command));
		message.add(getTranslator().translate(property));
		Sexp response = send(message);
		Set<Set<OWLObjectProperty>> ret = getParser()
				.parseSetOfSetOfProperties(response,
						getOWLOntologyManager().getOWLDataFactory());
		return ret;
	}

	protected CelSocketManager getSocketManager() {
		return this.socketManager;
	}

	public Set<Set<OWLClass>> getSubClasses(OWLDescription description)
			throws CelReasonerException {
		Set<Set<OWLClass>> ret = null;
		ret = getSetOfSetOfClasses(CelOwlApiKeyword.keyGetSubClasses,
				description);
		return ret;
	}

	public Set<Set<OWLObjectProperty>> getSubProperties(
			OWLObjectProperty property) throws CelReasonerException {
		return getSetOfSetOfProperties(CelOwlApiKeyword.keyGetSubProperties,
				property);
	}

	public Set<Set<OWLClass>> getSuperClasses(OWLDescription description)
			throws CelReasonerException {
		return getSetOfSetOfClasses(CelOwlApiKeyword.keyGetSuperClasses,
				description);
	}

	public Set<Set<OWLObjectProperty>> getSuperProperties(
			OWLObjectProperty property) throws CelReasonerException {
		return getSetOfSetOfProperties(CelOwlApiKeyword.keyGetSuperProperties,
				property);
	}

	public CelTranslator getTranslator() {
		return this.translator;
	}

	public Set<Set<OWLClass>> getTypes(OWLIndividual individual, boolean arg1)
			throws NotImplementedOperationException {
		throw new NotImplementedOperationException();
	}

	public boolean hasObjectPropertyRelationship(OWLIndividual individual0,
			OWLObjectPropertyExpression property, OWLIndividual individual1)
			throws CelReasonerException {
		Sexp message = SexpFactory.newNonAtomicSexp();
		message
				.add(SexpFactory
						.newAtomicSexp(CelOwlApiKeyword.keyHasObjectPropertyRelationship));
		message.add(getTranslator().translate(individual0));
		message.add(getTranslator().translate(property));
		message.add(getTranslator().translate(individual1));
		Sexp response = send(message);
		boolean ret = getParser().parseBoolean(response);
		return ret;
	}

	public boolean hasTypes(OWLIndividual individual,
			OWLDescription description, boolean arg2)
			throws NotImplementedOperationException {
		throw new NotImplementedOperationException();
	}

	/**
	 * @return true if the ontology is classified
	 * @throws CelReasonerException
	 */
	public boolean isClassified() throws CelReasonerException {
		Sexp message = SexpFactory.newNonAtomicSexp();
		message
				.add(SexpFactory
						.newAtomicSexp(CelOwlApiKeyword.keyIsClassified));
		Sexp response = send(message);
		boolean ret = getParser().parseBoolean(response);
		return ret;
	}

	/**
	 * NOTE: this function does not use the given ontology.
	 * 
	 * @return true if the current ontology is consistent
	 * @throws CelReasonerException
	 */
	public boolean isConsistent(OWLOntology ontology)
			throws CelReasonerException {
		Sexp message = SexpFactory.newNonAtomicSexp();
		message
				.add(SexpFactory
						.newAtomicSexp(CelOwlApiKeyword.keyIsConsistent));
		Sexp response = send(message);
		boolean ret = getParser().parseBoolean(response);
		return ret;
	}

	/**
	 * @return true if it is defined
	 */
	public boolean isDefined(OWLClass cls) {
		boolean ret = false;
		if (cls.toString().equals(thing) || cls.toString().equals(nothing)) {
			ret = true;
		} else {
			for (OWLOntology currentOntology : getOntologies()) {
				ret = ret || (currentOntology != null)
						&& currentOntology.containsClassReference(cls.getURI());
			}
		}
		return ret;
	}

	/**
	 * @return true if it is defined
	 */
	public boolean isDefined(OWLIndividual individual) {
		boolean ret = false;
		for (OWLOntology currentOntology : getOntologies()) {
			ret = ret
					|| (currentOntology != null && currentOntology
							.containsIndividualReference(individual.getURI()));
		}

		return ret;
	}

	/**
	 * @return true if it is defined
	 */
	public boolean isDefined(OWLObjectProperty property) {
		boolean ret = false;
		for (OWLOntology currentOntology : getOntologies()) {
			ret = ret
					|| (currentOntology != null && currentOntology
							.containsObjectPropertyReference(property.getURI()));
		}
		return ret;
	}

	/**
	 * @return true if the two classes are equivalent
	 * @throws CelReasonerException
	 */
	public boolean isEquivalentClass(OWLDescription description0,
			OWLDescription description1) throws CelReasonerException {
		Sexp message = SexpFactory.newNonAtomicSexp();
		try {
			message.add(SexpFactory
					.newAtomicSexp(CelOwlApiKeyword.keyClearOntologies));
			message.add(getTranslator().translate(description0));
			message.add(getTranslator().translate(description1));
		} catch (CelTranslatorException e) {
			throw new CelReasonerException(e);
		}
		Sexp response = send(message);
		boolean ret = getParser().parseBoolean(response);
		return ret;
	}

	/**
	 * @return true if the two classes are equivalent
	 * @throws CelReasonerException
	 */
	public boolean isRealised() throws CelReasonerException {
		Sexp message = SexpFactory.newNonAtomicSexp();
		message.add(SexpFactory.newAtomicSexp(CelOwlApiKeyword.keyIsRealised));
		Sexp response = send(message);
		boolean ret = getParser().parseBoolean(response);
		return ret;
	}

	public boolean isReflexive(OWLObjectProperty property)
			throws CelReasonerException {
		Sexp message = SexpFactory.newNonAtomicSexp();
		message.add(SexpFactory.newAtomicSexp(CelOwlApiKeyword.keyIsReflexive));
		message.add(getTranslator().translate(property));
		Sexp response = send(message);
		boolean ret = getParser().parseBoolean(response);
		return ret;
	}

	/**
	 * @return true if the description is satisfiable
	 * @throws CelReasonerException
	 */
	public boolean isSatisfiable(OWLDescription description)
			throws CelReasonerException {
		boolean ret = false;
		Sexp message = SexpFactory.newNonAtomicSexp();
		try {
			message.add(SexpFactory
					.newAtomicSexp(CelOwlApiKeyword.keyIsSatisfiable));
			message.add(getTranslator().translate(description));
		} catch (CelTranslatorException e) {
			throw new CelReasonerException(e);
		}
		Sexp response = send(message);
		ret = getParser().parseBoolean(response);
		return ret;
	}

	/**
	 * @return true if the first description is a subclass of the second
	 *         description
	 * @throws CelReasonerException
	 */
	public boolean isSubClassOf(OWLDescription description0,
			OWLDescription description1) throws CelReasonerException {
		Sexp message = SexpFactory.newNonAtomicSexp();
		try {
			message.add(SexpFactory
					.newAtomicSexp(CelOwlApiKeyword.keyIsSubClassOf));
			message.add(getTranslator().translate(description0));
			message.add(getTranslator().translate(description1));
		} catch (CelTranslatorException e) {
			throw new CelReasonerException(e);
		}
		Sexp response = send(message);
		boolean ret = getParser().parseBoolean(response);
		return ret;
	}

	public boolean isTransitive(OWLObjectProperty property)
			throws CelReasonerException {
		Sexp message = SexpFactory.newNonAtomicSexp();
		message
				.add(SexpFactory
						.newAtomicSexp(CelOwlApiKeyword.keyIsTransitive));
		message.add(getTranslator().translate(property));
		Sexp response = send(message);
		boolean ret = getParser().parseBoolean(response);
		return ret;
	}

	/**
	 * Loads the ontology.
	 * 
	 * @throws CelReasonerException
	 */
	public void loadOntologies(Set<OWLOntology> setOfOntologies)
			throws CelReasonerException {
		this.ontologies = setOfOntologies;
		synchronizedIfChanged();
	}

	/**
	 * @throws CelReasonerException
	 */
	public void realise() throws CelReasonerException {
		Sexp message = SexpFactory.newNonAtomicSexp();
		message.add(SexpFactory.newAtomicSexp(CelOwlApiKeyword.keyRealise));
		send(message);
	}

	protected Sexp send(Sexp message) throws CelReasonerException {
		Sexp ret = null;
		try {
			ret = getSocketManager().send(message);
		} catch (CelConnectionException e) {
			throw new CelReasonerException(e);
		}
		return ret;
	}

	protected Sexp send(Sexp message, String title) throws CelReasonerException {
		SwingProgressMonitor monitor = new SwingProgressMonitor();
		monitor.setStarted();
		monitor.setMessage(title);
		monitor.increment();
		getSocketManager().setProgressMonitor(monitor);
		Sexp ret = send(message);
		monitor.setFinished();
		return ret;
	}

	public void synchronizedIfChanged() throws CelReasonerException {
		if (this.changeTracker.getOntologyChanged()) {
			Sexp message = SexpFactory.newNonAtomicSexp();
			message.add(SexpFactory
					.newAtomicSexp(CelOwlApiKeyword.keyClearOntologies));
			send(message, "Clearing ontologies ...");
			message = SexpFactory.newNonAtomicSexp();
			message.add(SexpFactory
					.newAtomicSexp(CelOwlApiKeyword.keyLoadOntologies));
			Sexp temp = SexpFactory.newNonAtomicSexp();
			try {
				for (OWLOntology currentOntology : getOntologies()) {
					temp.add(getTranslator().translate(currentOntology));
				}
			} catch (CelTranslatorException e) {
				throw new CelReasonerException(e);
			}
			message.add(temp);
			send(message, "Loading ontologies ...");
			this.changeTracker.setOntologyChanged(false);
		}
	}

	/**
	 * This function is not implemented yet.
	 * 
	 * @throws NotImplementedOperationException
	 */
	public void unloadOntologies(Set<OWLOntology> arg0)
			throws NotImplementedOperationException {
		throw new NotImplementedOperationException();
	}
}
