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
import java.util.Set;
import java.util.StringTokenizer;
import java.util.logging.Logger;

import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLClassExpression;
import org.semanticweb.owlapi.model.OWLNamedIndividual;
import org.semanticweb.owlapi.model.OWLObjectProperty;
import org.semanticweb.owlapi.model.OWLObjectPropertyExpression;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyManager;
import org.semanticweb.owlapi.reasoner.Node;
import org.semanticweb.owlapi.reasoner.NodeSet;
import org.semanticweb.owlapi.reasoner.ReasonerProgressMonitor;
import org.semanticweb.owlapi.reasoner.impl.NodeFactory;
import org.semanticweb.owlapi.reasoner.impl.OWLClassNodeSet;
import org.semanticweb.owlapi.reasoner.impl.OWLNamedIndividualNodeSet;
import org.semanticweb.owlapi.reasoner.impl.OWLObjectPropertyNodeSet;
import org.semanticweb.owlapi.util.Version;

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
	public static final String reasonerName = "CEL";
	private static final String thing = "Thing";
	private OntologyChangeTracker changeTracker = new OntologyChangeTracker();
	private OWLOntology ontology = null;
	private CelParser parser = new CelParser();
	private CelSocketManager socketManager = new CelSocketManager();
	private Long timeOut = Long.MAX_VALUE;
	private CelTranslator translator = new CelTranslator();

	public CelReasonerInterface(OWLOntology ontology) {
		this.ontology = ontology;
		this.ontology.getOWLOntologyManager().addOntologyChangeListener(
				this.changeTracker);
	}

	public CelReasonerInterface(OWLOntology ontology,
			ReasonerProgressMonitor progressMonitor) {
		this(ontology);
		getSocketManager().setProgressMonitor(progressMonitor);
	}

	protected void assertSupportedClassExpression(OWLClassExpression description)
			throws CelReasonerException {
		try {
			getTranslator().translate(description);
		} catch (CelTranslatorException e) {
			throw new CelReasonerException(e);
		}
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

	protected NodeSet<OWLClass> convertToNodeSetOfOWLClass(
			Set<Set<OWLClass>> setOfSets) {
		Set<Node<OWLClass>> nodeSet = new HashSet<Node<OWLClass>>();
		for (Set<OWLClass> elem : setOfSets) {
			nodeSet.add(NodeFactory.getOWLClassNode(elem));
		}
		return new OWLClassNodeSet(nodeSet);
	}

	protected NodeSet<OWLNamedIndividual> convertToNodeSetOfOWLNamedIndividual(
			Set<OWLNamedIndividual> individualSet) {

		Set<Node<OWLNamedIndividual>> nodeSet = new HashSet<Node<OWLNamedIndividual>>();
		for (OWLNamedIndividual individual : individualSet) {
			nodeSet.add(NodeFactory.getOWLNamedIndividualNode(individual));
		}
		return new OWLNamedIndividualNodeSet(nodeSet);
	}

	protected NodeSet<OWLObjectProperty> convertToNodeSetOfOWLObjectProperty(
			Set<Set<OWLObjectProperty>> setOfSets) {
		Set<Node<OWLObjectProperty>> nodeSet = new HashSet<Node<OWLObjectProperty>>();
		for (Set<OWLObjectProperty> elem : setOfSets) {
			nodeSet.add(NodeFactory.getOWLObjectPropertyNode(elem));
		}
		return new OWLObjectPropertyNodeSet(nodeSet);
	}

	public void dispose() {
		if (getSocketManager() != null) {
			Sexp message = SexpFactory.newNonAtomicSexp();
			message.add(SexpFactory.newAtomicSexp(CelOwlApiKeyword.keyDispose));
			getSocketManager().stopExecution(message);
		}
	}

	public Node<OWLClass> getBottomClassNode() {
		return getEquivalentClasses(getOWLNothing());
	}

	public Node<OWLObjectProperty> getBottomObjectPropertyNode() {
		return getEquivalentObjectProperties(getOWLBottomObjectProperty());
	}

	public Node<OWLClass> getEquivalentClasses(OWLClassExpression description)
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
		return NodeFactory.getOWLClassNode(ret);
	}

	public Node<OWLObjectProperty> getEquivalentObjectProperties(
			OWLObjectPropertyExpression propertyExpression)
			throws CelReasonerException {
		Set<OWLObjectProperty> ret = null;
		Sexp message = SexpFactory.newNonAtomicSexp();
		message.add(SexpFactory
				.newAtomicSexp(CelOwlApiKeyword.keyGetEquivalentProperties));
		message.add(getTranslator().translate(
				propertyExpression.asOWLObjectProperty()));
		Sexp response = sendAndConvert(message);
		ret = getParser().parseSetOfProperties(response,
				getOWLOntologyManager().getOWLDataFactory());
		return NodeFactory.getOWLObjectPropertyNode(ret);
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

	public NodeSet<OWLNamedIndividual> getInstances(
			OWLClassExpression description, boolean direct)
			throws CelReasonerException {
		Set<OWLNamedIndividual> ret = new HashSet<OWLNamedIndividual>();
		Sexp message = SexpFactory.newNonAtomicSexp();
		message.add(SexpFactory
				.newAtomicSexp(CelOwlApiKeyword.keyGetIndividuals));
		try {
			message.add(getTranslator().translate(description));
		} catch (CelTranslatorException e) {
			throw new CelReasonerException(e);
		}
		message.add(getTranslator().translate(direct));
		Sexp expr = sendAndConvert(message);
		if (expr.toString().equalsIgnoreCase(LispKeyword.lispNil)) {
			expr = SexpFactory.newNonAtomicSexp();
		}
		ret = getParser().parseSetOfIndividuals(expr,
				getOWLOntologyManager().getOWLDataFactory());
		return convertToNodeSetOfOWLNamedIndividual(ret);
	}

	public OWLObjectProperty getOWLBottomObjectProperty() {
		return getRootOntology().getOWLOntologyManager().getOWLDataFactory()
				.getOWLBottomObjectProperty();
	}

	public OWLClass getOWLNothing() {
		return getRootOntology().getOWLOntologyManager().getOWLDataFactory()
				.getOWLNothing();
	}

	public OWLOntologyManager getOWLOntologyManager() {
		return this.ontology.getOWLOntologyManager();
	}

	public OWLClass getOWLThing() {
		return getRootOntology().getOWLOntologyManager().getOWLDataFactory()
				.getOWLThing();
	}

	public OWLObjectProperty getOWLTopObjectProperty() {
		return getRootOntology().getOWLOntologyManager().getOWLDataFactory()
				.getOWLTopObjectProperty();
	}

	public CelParser getParser() {
		return this.parser;
	}

	public ReasonerProgressMonitor getProgressMonitor() {
		return this.socketManager.getProgressMonitor();
	}

	public String getReasonerName() {
		return reasonerName;
	}

	public Version getReasonerVersion() {
		Version ret = new Version(0, 0, 0, 0);
		String versionId = CelReasonerInterface.class.getPackage()
				.getImplementationVersion();
		if (versionId != null) {
			StringTokenizer stok = new StringTokenizer(versionId, ".");
			int major = stok.hasMoreTokens() ? Integer.parseInt(stok
					.nextToken()) : 0;
			int minor = stok.hasMoreTokens() ? Integer.parseInt(stok
					.nextToken()) : 0;
			int patch = stok.hasMoreTokens() ? Integer.parseInt(stok
					.nextToken()) : 0;
			int build = stok.hasMoreTokens() ? Integer.parseInt(stok
					.nextToken()) : 0;
			ret = new Version(major, minor, patch, build);
		}
		return ret;
	}

	public OWLOntology getRootOntology() {
		return this.ontology;
	}

	protected Set<OWLObjectProperty> getSetOfProperties(String command,
			OWLClassExpression description) throws CelReasonerException {
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
			OWLClassExpression description) throws CelReasonerException {
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

	public NodeSet<OWLClass> getSubClasses(OWLClassExpression classExpression,
			boolean direct) {
		NodeSet<OWLClass> ret = null;
		if (direct) {
			ret = convertToNodeSetOfOWLClass(getSetOfSetOfClasses(
					CelOwlApiKeyword.keyGetSubClasses, classExpression));
		} else {
			ret = convertToNodeSetOfOWLClass(getSetOfSetOfClasses(
					CelOwlApiKeyword.keyGetDescendantClasses, classExpression));
		}
		return ret;
	}

	public NodeSet<OWLObjectProperty> getSubObjectProperties(
			OWLObjectPropertyExpression propertyExpression, boolean direct) {
		NodeSet<OWLObjectProperty> ret = null;
		if (direct) {
			ReachabilityGraph<OWLObjectProperty> graph = new ReachabilityGraph<OWLObjectProperty>();
			Set<OWLObjectProperty> reachableVertices = getFlattenedDescendantProperties(propertyExpression
					.asOWLObjectProperty());
			graph.addReachable(propertyExpression.asOWLObjectProperty(),
					reachableVertices);
			for (OWLObjectProperty vertex : reachableVertices) {
				graph.addReachable(vertex,
						getFlattenedDescendantProperties(vertex));
			}
			ret = convertToNodeSetOfOWLObjectProperty(makeEquivalentClasses(graph
					.getDirectSuccessors(propertyExpression
							.asOWLObjectProperty())));
		} else {
			ret = convertToNodeSetOfOWLObjectProperty(makeEquivalentClasses(getFlattenedDescendantProperties(propertyExpression
					.asOWLObjectProperty())));
		}
		return ret;
	}

	public NodeSet<OWLClass> getSuperClasses(
			OWLClassExpression classExpression, boolean direct) {
		NodeSet<OWLClass> ret = null;
		if (direct) {
			ret = convertToNodeSetOfOWLClass(getSetOfSetOfClasses(
					CelOwlApiKeyword.keyGetSuperClasses, classExpression));
		} else {
			ret = convertToNodeSetOfOWLClass(getSetOfSetOfClasses(
					CelOwlApiKeyword.keyGetAncestorClasses, classExpression));
		}
		return ret;
	}

	public NodeSet<OWLObjectProperty> getSuperObjectProperties(
			OWLObjectPropertyExpression propertyExpression, boolean direct) {
		NodeSet<OWLObjectProperty> ret = null;
		if (direct) {
			ReachabilityGraph<OWLObjectProperty> graph = new ReachabilityGraph<OWLObjectProperty>();
			Set<OWLObjectProperty> reachableVertices = getFlattenedAncestorProperties(propertyExpression
					.asOWLObjectProperty());
			graph.addReachable(propertyExpression.asOWLObjectProperty(),
					reachableVertices);
			for (OWLObjectProperty vertex : reachableVertices) {
				graph.addReachable(vertex,
						getFlattenedAncestorProperties(vertex));
			}
			ret = convertToNodeSetOfOWLObjectProperty(makeEquivalentClasses(graph
					.getDirectSuccessors(propertyExpression
							.asOWLObjectProperty())));
		} else {
			ret = convertToNodeSetOfOWLObjectProperty(makeEquivalentClasses(getFlattenedAncestorProperties(propertyExpression
					.asOWLObjectProperty())));
		}

		return ret;
	}

	public long getTimeOut() {
		return this.timeOut;
	}

	public Node<OWLClass> getTopClassNode() {
		return getEquivalentClasses(getOWLThing());
	}

	public Node<OWLObjectProperty> getTopObjectPropertyNode() {
		return getEquivalentObjectProperties(getOWLTopObjectProperty());
	}

	public CelTranslator getTranslator() {
		return this.translator;
	}

	public NodeSet<OWLClass> getTypes(OWLNamedIndividual individual,
			boolean direct) throws CelReasonerException {
		Sexp message = SexpFactory.newNonAtomicSexp();
		message.add(SexpFactory.newAtomicSexp(CelOwlApiKeyword.keyGetTypes));
		message.add(getTranslator().translate(individual));
		message.add(getTranslator().translate(direct));
		Sexp response = sendAndConvert(message);
		Set<Set<OWLClass>> ret = getParser().parseSetOfSetOfClasses(response,
				getOWLOntologyManager().getOWLDataFactory());
		return convertToNodeSetOfOWLClass(ret);
	}

	/**
	 * @return the set of inconsistent classes
	 * @throws CelReasonerException
	 */
	public Node<OWLClass> getUnsatisfiableClasses() throws CelReasonerException {
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
		return NodeFactory.getOWLClassNode(ret);
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
	 * @return true if the current ontology is consistent
	 * @throws CelReasonerException
	 */
	public boolean isConsistent() throws CelReasonerException {
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
			ret = this.ontology != null
					&& this.ontology.containsClassInSignature(cls.getIRI());
		}
		return ret;
	}

	/**
	 * @return true if it is defined
	 */
	public boolean isDefined(OWLNamedIndividual individual) {
		return this.ontology != null
				&& this.ontology.containsIndividualInSignature(individual
						.getIRI());

	}

	/**
	 * @return true if it is defined
	 */
	public boolean isDefined(OWLObjectProperty property) {
		return this.ontology != null
				&& this.ontology.containsObjectPropertyInSignature(property
						.getIRI());
	}

	/**
	 * @return true if the two classes are equivalent
	 * @throws CelReasonerException
	 */
	public boolean isEquivalentClass(OWLClassExpression description0,
			OWLClassExpression description1) throws CelReasonerException {
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
	public boolean isSatisfiable(OWLClassExpression description)
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
	public boolean isSubClassOf(OWLClassExpression description0,
			OWLClassExpression description1) throws CelReasonerException {
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
		setOntologies(setOfOntologies);
		synchronizedIfChanged();
	}

	protected Set<Set<OWLObjectProperty>> makeEquivalentClasses(
			Set<OWLObjectProperty> flattenedSet) throws CelReasonerException {
		Set<Set<OWLObjectProperty>> ret = new HashSet<Set<OWLObjectProperty>>();
		Set<OWLObjectProperty> visited = new HashSet<OWLObjectProperty>();
		for (OWLObjectProperty property : flattenedSet) {
			if (!visited.contains(property)) {
				Set<OWLObjectProperty> equivalentProperties = getEquivalentObjectProperties(
						property).getEntities();
				ret.add(equivalentProperties);
				visited.addAll(equivalentProperties);
			}
		}
		return ret;
	}

	public void prepareReasoner() throws CelReasonerException {
		synchronizedIfChanged();
		Sexp message = SexpFactory.newNonAtomicSexp();
		message.add(SexpFactory.newAtomicSexp(CelOwlApiKeyword.keyClassify));
		send(message, "Classifying ontology ...");
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
			getProgressMonitor().reasonerTaskStarted(title);
		}
		Sexp ret = send(message);
		if (getProgressMonitor() != null) {
			getProgressMonitor().reasonerTaskStopped();
		}
		return ret;
	}

	protected Sexp sendAndConvert(Sexp message) throws CelReasonerException {
		return convertNil(send(message));
	}

	protected void setOntologies(Set<OWLOntology> setOfOntologies) {
		this.ontology = setOfOntologies.iterator().next();
		this.changeTracker.setOntologyChanged(true);
	}

	public void setProgressMonitor(ReasonerProgressMonitor progressMonitor) {
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
				axiomSet.add(getTranslator().translate(this.ontology,
						getOWLOntologyManager()));
			} catch (CelTranslatorException e) {
				throw new CelReasonerException(e);
			}
			message.add(axiomSet);
			send(message, "Loading ontologies ...");
			this.changeTracker.setOntologyChanged(false);
		}
	}
}
