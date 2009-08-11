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
import java.util.Iterator;
import java.util.Set;
import java.util.logging.Logger;

import org.semanticweb.owl.inference.OWLReasonerException;
import org.semanticweb.owl.model.OWLClass;
import org.semanticweb.owl.model.OWLDescription;
import org.semanticweb.owl.model.OWLIndividual;
import org.semanticweb.owl.model.OWLObjectProperty;
import org.semanticweb.owl.model.OWLOntology;
import org.semanticweb.owl.model.OWLOntologyManager;
import org.semanticweb.owl.util.ProgressMonitor;

import de.tudresden.inf.lat.cel.connection.CelConnectionException;
import de.tudresden.inf.lat.cel.connection.CelSocketManager;
import de.tudresden.inf.lat.cel.conversion.CelParser;
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

	private static final Logger logger = Logger
			.getLogger(CelReasonerInterface.class.getName());

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

	public CelReasonerInterface(OWLOntologyManager manager,
			ProgressMonitor progressMonitor) {
		this(manager);
		getSocketManager().setProgressMonitor(progressMonitor);
	}

	protected void assertSupportedDescription(OWLDescription description)
			throws CelReasonerException {
		try {
			getTranslator().translate(description);
		} catch (CelTranslatorException e) {
			throw new CelReasonerException(e);
		}
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
		setOntologies(new HashSet<OWLOntology>());
	}

	/**
	 * Converts a NIL into a (), and a (NIL) into a (()).
	 * 
	 * @param expr
	 * @return the () list if NIL is found, and (()) if (NIL) is found.
	 */
	protected Sexp convertNil(Sexp expr) {
		Sexp nilExpr = SexpFactory.newAtomicSexp(LispKeyword.lispNil);
		Sexp listOfNilExpr = SexpFactory.newNonAtomicSexp();
		listOfNilExpr.add(nilExpr);

		Sexp ret = expr;
		if (ret.toString().equalsIgnoreCase(nilExpr.toString())) {
			ret = SexpFactory.newNonAtomicSexp();
		} else if (ret.toString().equalsIgnoreCase(listOfNilExpr.toString())) {
			ret = SexpFactory.newNonAtomicSexp();
			ret.add(SexpFactory.newNonAtomicSexp());
		}
		return ret;
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
			OWLObjectProperty property) throws CelReasonerException {
		return makeEquivalentClasses(getFlattenedAncestorProperties(property));
	}

	public Set<Set<OWLClass>> getDescendantClasses(OWLDescription description)
			throws CelReasonerException {
		return getSetOfSetOfClasses(CelOwlApiKeyword.keyGetDescendantClasses,
				description);
	}

	public Set<Set<OWLObjectProperty>> getDescendantProperties(
			OWLObjectProperty property) throws CelReasonerException {
		return makeEquivalentClasses(getFlattenedDescendantProperties(property));
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
		Sexp response = sendAndConvert(message);
		ret = getParser().parseSetOfClasses(response,
				getOWLOntologyManager().getOWLDataFactory());
		return ret;
	}

	public Set<OWLObjectProperty> getEquivalentProperties(
			OWLObjectProperty property) throws CelReasonerException {
		Set<OWLObjectProperty> ret = null;
		Sexp message = SexpFactory.newNonAtomicSexp();
		message.add(SexpFactory
				.newAtomicSexp(CelOwlApiKeyword.keyGetEquivalentProperties));
		message.add(getTranslator().translate(property));
		Sexp response = sendAndConvert(message);
		ret = getParser().parseSetOfProperties(response,
				getOWLOntologyManager().getOWLDataFactory());
		return ret;
	}

	protected Set<OWLObjectProperty> getFlattenedAncestorProperties(
			OWLObjectProperty property) throws CelReasonerException {
		return getSetOfProperties(
				CelOwlApiKeyword.keyGetFlattenedAncestorProperties, property);
	}

	protected Set<OWLObjectProperty> getFlattenedDescendantProperties(
			OWLObjectProperty property) throws CelReasonerException {
		return getSetOfProperties(
				CelOwlApiKeyword.keyGetFlattenedDescendantProperties, property);
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
		Sexp expr = sendAndConvert(message);
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
		Sexp expr = sendAndConvert(message);
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

	protected Set<OWLOntology> getOntologies() {
		return this.ontologies;
	}

	public OWLOntologyManager getOWLOntologyManager() {
		return this.ontologyManager;
	}

	public CelParser getParser() {
		return this.parser;
	}

	public ProgressMonitor getProgressMonitor() {
		return this.socketManager.getProgressMonitor();
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
		Sexp response = sendAndConvert(message);
		Set<OWLObjectProperty> ret = getParser().parseSetOfProperties(response,
				getOWLOntologyManager().getOWLDataFactory());
		return ret;
	}

	protected Set<OWLObjectProperty> getSetOfProperties(String command,
			OWLObjectProperty property) throws CelReasonerException {
		Sexp message = SexpFactory.newNonAtomicSexp();
		message.add(SexpFactory.newAtomicSexp(command));
		message.add(getTranslator().translate(property));
		Sexp response = sendAndConvert(message);
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
		Sexp response = sendAndConvert(message);
		Set<Set<OWLClass>> ret = getParser().parseSetOfSetOfClasses(response,
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
			OWLObjectProperty property) throws OWLReasonerException {
		ReachabilityGraph<OWLObjectProperty> graph = new ReachabilityGraph<OWLObjectProperty>();
		Set<OWLObjectProperty> reachableVertices = getFlattenedDescendantProperties(property);
		graph.addReachable(property, reachableVertices);
		for (OWLObjectProperty vertex : reachableVertices) {
			graph
					.addReachable(vertex,
							getFlattenedDescendantProperties(vertex));
		}
		return makeEquivalentClasses(graph.getDirectSuccessors(property));
	}

	public Set<Set<OWLClass>> getSuperClasses(OWLDescription description)
			throws CelReasonerException {
		return getSetOfSetOfClasses(CelOwlApiKeyword.keyGetSuperClasses,
				description);
	}

	public Set<Set<OWLObjectProperty>> getSuperProperties(
			OWLObjectProperty property) throws OWLReasonerException {
		ReachabilityGraph<OWLObjectProperty> graph = new ReachabilityGraph<OWLObjectProperty>();
		Set<OWLObjectProperty> reachableVertices = getFlattenedAncestorProperties(property);
		graph.addReachable(property, reachableVertices);
		for (OWLObjectProperty vertex : reachableVertices) {
			graph.addReachable(vertex, getFlattenedAncestorProperties(vertex));
		}
		return makeEquivalentClasses(graph.getDirectSuccessors(property));
	}

	public CelTranslator getTranslator() {
		return this.translator;
	}

	public Set<Set<OWLClass>> getTypes(OWLIndividual individual, boolean direct)
			throws CelReasonerException {
		Sexp message = SexpFactory.newNonAtomicSexp();
		message.add(SexpFactory.newAtomicSexp(CelOwlApiKeyword.keyGetTypes));
		message.add(getTranslator().translate(individual));
		message.add(getTranslator().translate(direct));
		Sexp response = sendAndConvert(message);
		Set<Set<OWLClass>> ret = getParser().parseSetOfSetOfClasses(response,
				getOWLOntologyManager().getOWLDataFactory());
		return ret;
	}

	public boolean hasTypes(OWLIndividual individual,
			OWLDescription description, boolean direct)
			throws CelReasonerException {
		assertSupportedDescription(description);
		boolean found = false;
		Set<Set<OWLClass>> typeSet = getTypes(individual, direct);
		for (Iterator<Set<OWLClass>> it = typeSet.iterator(); !found
				&& it.hasNext();) {
			Set<OWLClass> currentSet = it.next();
			found = found || currentSet.contains(description);
		}
		return found;
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
					.newAtomicSexp(CelOwlApiKeyword.keyIsEquivalentClass));
			message.add(getTranslator().translate(description0));
			message.add(getTranslator().translate(description1));
		} catch (CelTranslatorException e) {
			throw new CelReasonerException(e);
		}
		Sexp response = send(message);
		boolean ret = getParser().parseBoolean(response);
		return ret;
	}

	public boolean isRealised() throws CelReasonerException {
		Sexp message = SexpFactory.newNonAtomicSexp();
		message.add(SexpFactory.newAtomicSexp(CelOwlApiKeyword.keyIsRealised));
		Sexp response = send(message);
		boolean ret = getParser().parseBoolean(response);
		return ret;
	}

	/**
	 * @param property
	 * @return true if the property is reflexive
	 * @throws CelReasonerException
	 */
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

	/**
	 * @param property
	 * @return true if the property is transitive
	 * @throws CelReasonerException
	 */
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
		// OWLImportsDeclaration declaration
		// getOWLOntologyManager().getOntology(declaration.getImportedOntologyURI()).getAxioms());

		setOntologies(setOfOntologies);
		synchronizedIfChanged();
	}

	protected Set<Set<OWLObjectProperty>> makeEquivalentClasses(
			Set<OWLObjectProperty> flattenedSet) throws CelReasonerException {
		Set<Set<OWLObjectProperty>> ret = new HashSet<Set<OWLObjectProperty>>();
		Set<OWLObjectProperty> visited = new HashSet<OWLObjectProperty>();
		for (OWLObjectProperty property : flattenedSet) {
			if (!visited.contains(property)) {
				Set<OWLObjectProperty> equivalentProperties = getEquivalentProperties(property);
				ret.add(equivalentProperties);
				visited.addAll(equivalentProperties);
			}
		}
		return ret;
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
		logger.fine(title);
		if (getProgressMonitor() != null) {
			getProgressMonitor().setStarted();
			getProgressMonitor().setMessage(title);
		}
		Sexp ret = send(message);
		if (getProgressMonitor() != null) {
			getProgressMonitor().setFinished();
		}
		return ret;
	}

	protected Sexp sendAndConvert(Sexp message) throws CelReasonerException {
		return convertNil(send(message));
	}

	protected void setOntologies(Set<OWLOntology> setOfOntologies) {
		this.ontologies = setOfOntologies;
		this.changeTracker.setOntologyChanged(true);
	}

	public void setProgressMonitor(ProgressMonitor progressMonitor) {
		getSocketManager().setProgressMonitor(progressMonitor);
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
			Sexp axiomSet = SexpFactory.newNonAtomicSexp();
			try {
				for (OWLOntology currentOntology : getOntologies()) {
					axiomSet.add(getTranslator().translate(currentOntology,
							getOWLOntologyManager()));
				}
			} catch (CelTranslatorException e) {
				throw new CelReasonerException(e);
			}
			message.add(axiomSet);
			send(message, "Loading ontologies ...");
			this.changeTracker.setOntologyChanged(false);
		}
	}
}
