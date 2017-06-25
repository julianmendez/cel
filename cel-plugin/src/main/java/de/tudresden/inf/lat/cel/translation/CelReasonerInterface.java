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

import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.StringTokenizer;
import java.util.logging.Logger;

import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLAxiom;
import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLClassExpression;
import org.semanticweb.owlapi.model.OWLNamedIndividual;
import org.semanticweb.owlapi.model.OWLObjectProperty;
import org.semanticweb.owlapi.model.OWLObjectPropertyExpression;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyChange;
import org.semanticweb.owlapi.model.OWLOntologyManager;
import org.semanticweb.owlapi.reasoner.InferenceType;
import org.semanticweb.owlapi.reasoner.Node;
import org.semanticweb.owlapi.reasoner.NodeSet;
import org.semanticweb.owlapi.reasoner.OWLReasonerConfiguration;
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
import de.tudresden.inf.lat.util.map.OptMap;
import de.tudresden.inf.lat.util.map.OptMapImpl;

/**
 * This class provides all the implemented methods for using the CEL reasoner.
 *
 * @author Julian Mendez
 */
public class CelReasonerInterface {

	private static final String auxClassPrefix = "http://lat.inf.tu-dresden.de/systems/cel/auxClass";
	private static final Logger logger = Logger.getLogger(CelReasonerInterface.class.getName());
	private static final String nothing = "Nothing";
	public static final String reasonerName = "CEL";
	private static final String thing = "Thing";

	private int auxClassCount = 0;
	private final OptMap<OWLClassExpression, OWLClass> auxClassInvMap = new OptMapImpl<>(new HashMap<>());
	private final OptMap<OWLClass, OWLClassExpression> auxClassMap = new OptMapImpl<>(new HashMap<>());
	private final OntologyChangeTracker changeTracker = new OntologyChangeTracker();
	private final OntologyEntailmentChecker entailmentChecker = new OntologyEntailmentChecker(this);
	private OWLOntology ontology = null;
	private final CelParser parser = new CelParser();
	private final CelSocketManager socketManager = new CelSocketManager();
	private final Long timeOut = Long.MAX_VALUE;
	private final CelTranslator translator = new CelTranslator();

	public CelReasonerInterface(OWLOntology ontology) {
		this.ontology = ontology;
		this.ontology.getOWLOntologyManager().addOntologyChangeListener(this.changeTracker);
	}

	public CelReasonerInterface(OWLOntology ontology, OWLReasonerConfiguration config) {
		this(ontology);
		getSocketManager().setReasonerConfiguration(config);
	}

	protected void assertSupportedClassExpression(OWLClassExpression expression) throws CelReasonerException {
		try {
			getTranslator().translate(expression);
		} catch (CelTranslatorException e) {
			throw new CelReasonerException(e);
		}
	}

	/**
	 * Converts a NIL into a (), and a (NIL) into a (()).
	 *
	 * @param expr
	 *            S-expression
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

	protected NodeSet<OWLClass> convertToNodeSetOfOWLClass(Set<Set<OWLClass>> setOfSets) {
		Set<Node<OWLClass>> nodeSet = new HashSet<>();
		setOfSets.forEach(elem -> {
			nodeSet.add(NodeFactory.getOWLClassNode(elem));
		});
		return new OWLClassNodeSet(nodeSet);
	}

	protected NodeSet<OWLNamedIndividual> convertToNodeSetOfOWLNamedIndividual(Set<OWLNamedIndividual> individualSet) {

		Set<Node<OWLNamedIndividual>> nodeSet = new HashSet<>();
		individualSet.forEach(individual -> {
			nodeSet.add(NodeFactory.getOWLNamedIndividualNode(individual));
		});
		return new OWLNamedIndividualNodeSet(nodeSet);
	}

	protected NodeSet<OWLObjectPropertyExpression> convertToNodeSetOfOWLObjectPropertyExpression(
			Set<Set<OWLObjectPropertyExpression>> setOfSets) {
		Set<Node<OWLObjectPropertyExpression>> nodeSet = new HashSet<>();
		setOfSets.forEach(elem -> {
			nodeSet.add(NodeFactory.getOWLObjectPropertyNode(elem));
		});
		return new OWLObjectPropertyNodeSet(nodeSet);
	}

	protected Set<OWLObjectPropertyExpression> convertToOWLObjectPropertyExpression(Set<OWLObjectProperty> set) {
		Set<OWLObjectPropertyExpression> ret = new HashSet<>();
		ret.addAll(set);
		return ret;
	}

	private OWLClass createAuxiliaryClass() {
		IRI iri = IRI.create(auxClassPrefix + this.auxClassCount);
		this.auxClassCount++;
		OWLClass ret = getRootOntology().getOWLOntologyManager().getOWLDataFactory().getOWLClass(iri);
		return ret;
	}

	public void dispose() {
		if (getSocketManager() != null) {
			Sexp message = SexpFactory.newNonAtomicSexp();
			message.add(SexpFactory.newAtomicSexp(CelOwlApiKeyword.keyDispose));
			getSocketManager().stopExecution(message);
		}
	}

	protected OWLClass flattenClassExpression(OWLClassExpression ce) {
		OWLClass ret = null;
		if (ce instanceof OWLClass) {
			ret = (OWLClass) ce;
		} else {
			Optional<OWLClass> optClass = this.auxClassInvMap.get(ce);
			if (optClass.isPresent()) {
				ret = optClass.get();
			} else {
				ret = createAuxiliaryClass();
				this.auxClassMap.put(ret, ce);
				this.auxClassInvMap.put(ce, ret);
				Set<OWLClassExpression> classExpressions = new HashSet<>();
				classExpressions.add(ce);
				classExpressions.add(ret);
				OWLAxiom newAxiom = getRootOntology().getOWLOntologyManager().getOWLDataFactory()
						.getOWLEquivalentClassesAxiom(classExpressions);
				getRootOntology().getOWLOntologyManager().addAxiom(getRootOntology(), newAxiom);
			}
		}
		return ret;
	}

	public Node<OWLClass> getBottomClassNode() {
		return getEquivalentClasses(getOWLNothing());
	}

	public Node<OWLObjectPropertyExpression> getBottomObjectPropertyNode() {
		return getEquivalentObjectProperties(getOWLBottomObjectProperty());
	}

	public Node<OWLClass> getEquivalentClasses(OWLClassExpression expression) throws CelReasonerException {
		Set<OWLClass> ret = null;
		Sexp message = SexpFactory.newNonAtomicSexp();
		try {
			message.add(SexpFactory.newAtomicSexp(CelOwlApiKeyword.keyGetEquivalentClasses));
			message.add(getTranslator().translate(expression));
		} catch (CelTranslatorException e) {
			throw new CelReasonerException(e);
		}
		Sexp response = sendAndConvert(message);
		ret = getParser().parseSetOfClasses(response, getOWLOntologyManager().getOWLDataFactory());
		return NodeFactory.getOWLClassNode(ret);
	}

	public Node<OWLObjectPropertyExpression> getEquivalentObjectProperties(
			OWLObjectPropertyExpression propertyExpression) throws CelReasonerException {
		Set<OWLObjectPropertyExpression> ret = null;
		Sexp message = SexpFactory.newNonAtomicSexp();
		message.add(SexpFactory.newAtomicSexp(CelOwlApiKeyword.keyGetEquivalentProperties));
		message.add(getTranslator().translate(propertyExpression.asOWLObjectProperty()));
		Sexp response = sendAndConvert(message);
		ret = convertToOWLObjectPropertyExpression(
				getParser().parseSetOfProperties(response, getOWLOntologyManager().getOWLDataFactory()));
		return NodeFactory.getOWLObjectPropertyNode(ret);
	}

	protected Set<OWLObjectProperty> getFlattenedAncestorProperties(OWLObjectProperty property)
			throws CelReasonerException {
		return getSetOfProperties(CelOwlApiKeyword.keyGetFlattenedAncestorProperties, property);
	}

	protected Set<OWLObjectProperty> getFlattenedDescendantProperties(OWLObjectProperty property)
			throws CelReasonerException {
		return getSetOfProperties(CelOwlApiKeyword.keyGetFlattenedDescendantProperties, property);
	}

	public NodeSet<OWLNamedIndividual> getInstances(OWLClassExpression expression, boolean direct)
			throws CelReasonerException {
		Set<OWLNamedIndividual> ret = new HashSet<>();
		Sexp message = SexpFactory.newNonAtomicSexp();
		message.add(SexpFactory.newAtomicSexp(CelOwlApiKeyword.keyGetIndividuals));
		try {
			message.add(getTranslator().translate(expression));
		} catch (CelTranslatorException e) {
			throw new CelReasonerException(e);
		}
		message.add(getTranslator().translate(direct));
		Sexp expr = sendAndConvert(message);
		if (expr.toString().equalsIgnoreCase(LispKeyword.lispNil)) {
			expr = SexpFactory.newNonAtomicSexp();
		}
		ret = getParser().parseSetOfIndividuals(expr, getOWLOntologyManager().getOWLDataFactory());
		return convertToNodeSetOfOWLNamedIndividual(ret);
	}

	public OWLObjectProperty getOWLBottomObjectProperty() {
		return getRootOntology().getOWLOntologyManager().getOWLDataFactory().getOWLBottomObjectProperty();
	}

	public OWLClass getOWLNothing() {
		return getRootOntology().getOWLOntologyManager().getOWLDataFactory().getOWLNothing();
	}

	public OWLOntologyManager getOWLOntologyManager() {
		return this.ontology.getOWLOntologyManager();
	}

	public OWLClass getOWLThing() {
		return getRootOntology().getOWLOntologyManager().getOWLDataFactory().getOWLThing();
	}

	public OWLObjectProperty getOWLTopObjectProperty() {
		return getRootOntology().getOWLOntologyManager().getOWLDataFactory().getOWLTopObjectProperty();
	}

	public CelParser getParser() {
		return this.parser;
	}

	public Set<OWLAxiom> getPendingAxiomAdditions() {
		return Collections.emptySet();
	}

	public Set<OWLAxiom> getPendingAxiomRemovals() {
		return Collections.emptySet();
	}

	public List<OWLOntologyChange> getPendingChanges() {
		return Collections.emptyList();
	}

	public Set<InferenceType> getPrecomputableInferenceTypes() {
		return Collections.singleton(InferenceType.CLASS_HIERARCHY);
	}

	public OWLReasonerConfiguration getReasonerConfiguration() {
		return this.socketManager.getReasonerConfiguration();
	}

	public String getReasonerName() {
		return reasonerName;
	}

	public Version getReasonerVersion() {
		Version ret = new Version(0, 0, 0, 0);
		String versionId = CelReasonerInterface.class.getPackage().getImplementationVersion();
		if (versionId != null) {
			StringTokenizer stok = new StringTokenizer(versionId, ".");
			int major = stok.hasMoreTokens() ? Integer.parseInt(stok.nextToken()) : 0;
			int minor = stok.hasMoreTokens() ? Integer.parseInt(stok.nextToken()) : 0;
			int patch = stok.hasMoreTokens() ? Integer.parseInt(stok.nextToken()) : 0;
			int build = stok.hasMoreTokens() ? Integer.parseInt(stok.nextToken()) : 0;
			ret = new Version(major, minor, patch, build);
		}
		return ret;
	}

	public OWLOntology getRootOntology() {
		return this.ontology;
	}

	protected Set<OWLObjectProperty> getSetOfProperties(String command, OWLClassExpression expression)
			throws CelReasonerException {
		Sexp message = SexpFactory.newNonAtomicSexp();
		try {
			message.add(SexpFactory.newAtomicSexp(command));
			message.add(getTranslator().translate(expression));
		} catch (CelTranslatorException e) {
			throw new CelReasonerException(e);
		}
		Sexp response = sendAndConvert(message);
		Set<OWLObjectProperty> ret = getParser().parseSetOfProperties(response,
				getOWLOntologyManager().getOWLDataFactory());
		return ret;
	}

	protected Set<OWLObjectProperty> getSetOfProperties(String command, OWLObjectProperty property)
			throws CelReasonerException {
		Sexp message = SexpFactory.newNonAtomicSexp();
		message.add(SexpFactory.newAtomicSexp(command));
		message.add(getTranslator().translate(property));
		Sexp response = sendAndConvert(message);
		Set<OWLObjectProperty> ret = getParser().parseSetOfProperties(response,
				getOWLOntologyManager().getOWLDataFactory());
		return ret;
	}

	protected Set<Set<OWLClass>> getSetOfSetOfClasses(String command, OWLClassExpression expression)
			throws CelReasonerException {
		Sexp message = SexpFactory.newNonAtomicSexp();
		try {
			message.add(SexpFactory.newAtomicSexp(command));
			message.add(getTranslator().translate(expression));
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

	public NodeSet<OWLClass> getSubClasses(OWLClassExpression classExpression, boolean direct) {
		NodeSet<OWLClass> ret = null;
		if (direct) {
			ret = convertToNodeSetOfOWLClass(getSetOfSetOfClasses(CelOwlApiKeyword.keyGetSubClasses, classExpression));
		} else {
			ret = convertToNodeSetOfOWLClass(
					getSetOfSetOfClasses(CelOwlApiKeyword.keyGetDescendantClasses, classExpression));
		}
		return ret;
	}

	public NodeSet<OWLObjectPropertyExpression> getSubObjectProperties(OWLObjectPropertyExpression propertyExpression,
			boolean direct) {
		NodeSet<OWLObjectPropertyExpression> ret = null;
		if (direct) {
			ReachabilityGraph<OWLObjectProperty> graph = new ReachabilityGraph<OWLObjectProperty>();
			Set<OWLObjectProperty> reachableVertices = getFlattenedDescendantProperties(
					propertyExpression.asOWLObjectProperty());
			graph.addReachable(propertyExpression.asOWLObjectProperty(), reachableVertices);
			reachableVertices.forEach(vertex -> {
				graph.addReachable(vertex, getFlattenedDescendantProperties(vertex));
			});
			ret = convertToNodeSetOfOWLObjectPropertyExpression(
					makeEquivalentClasses(convertToOWLObjectPropertyExpression(
							graph.getDirectSuccessors(propertyExpression.asOWLObjectProperty()))));
		} else {
			ret = convertToNodeSetOfOWLObjectPropertyExpression(
					makeEquivalentClasses(convertToOWLObjectPropertyExpression(
							getFlattenedDescendantProperties(propertyExpression.asOWLObjectProperty()))));
		}
		return ret;
	}

	public NodeSet<OWLClass> getSuperClasses(OWLClassExpression classExpression, boolean direct) {
		NodeSet<OWLClass> ret = null;
		if (direct) {
			ret = convertToNodeSetOfOWLClass(
					getSetOfSetOfClasses(CelOwlApiKeyword.keyGetSuperClasses, classExpression));
		} else {
			ret = convertToNodeSetOfOWLClass(
					getSetOfSetOfClasses(CelOwlApiKeyword.keyGetAncestorClasses, classExpression));
		}
		return ret;
	}

	public NodeSet<OWLObjectPropertyExpression> getSuperObjectProperties(OWLObjectPropertyExpression propertyExpression,
			boolean direct) {
		NodeSet<OWLObjectPropertyExpression> ret = null;
		if (direct) {
			ReachabilityGraph<OWLObjectProperty> graph = new ReachabilityGraph<OWLObjectProperty>();
			Set<OWLObjectProperty> reachableVertices = getFlattenedAncestorProperties(
					propertyExpression.asOWLObjectProperty());
			graph.addReachable(propertyExpression.asOWLObjectProperty(), reachableVertices);
			reachableVertices.forEach(vertex -> {
				graph.addReachable(vertex, getFlattenedAncestorProperties(vertex));
			});
			ret = convertToNodeSetOfOWLObjectPropertyExpression(
					makeEquivalentClasses(convertToOWLObjectPropertyExpression(
							graph.getDirectSuccessors(propertyExpression.asOWLObjectProperty()))));
		} else {
			ret = convertToNodeSetOfOWLObjectPropertyExpression(
					makeEquivalentClasses(convertToOWLObjectPropertyExpression(
							getFlattenedAncestorProperties(propertyExpression.asOWLObjectProperty()))));
		}

		return ret;
	}

	public long getTimeOut() {
		return this.timeOut;
	}

	public Node<OWLClass> getTopClassNode() {
		return getEquivalentClasses(getOWLThing());
	}

	public Node<OWLObjectPropertyExpression> getTopObjectPropertyNode() {
		return getEquivalentObjectProperties(getOWLTopObjectProperty());
	}

	public CelTranslator getTranslator() {
		return this.translator;
	}

	public NodeSet<OWLClass> getTypes(OWLNamedIndividual individual, boolean direct) throws CelReasonerException {
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
	 *             if a CEL reasoner error occurs
	 */
	public Node<OWLClass> getUnsatisfiableClasses() throws CelReasonerException {
		Set<OWLClass> ret = new HashSet<>();
		Sexp message = SexpFactory.newNonAtomicSexp();
		message.add(SexpFactory.newAtomicSexp(CelOwlApiKeyword.keyGetInconsistentClasses));
		Sexp expr = sendAndConvert(message);
		if (expr.toString().equalsIgnoreCase(LispKeyword.lispNil)) {
			expr = SexpFactory.newNonAtomicSexp();
		}
		ret = getParser().parseSetOfClasses(expr, getOWLOntologyManager().getOWLDataFactory());
		return NodeFactory.getOWLClassNode(ret);
	}

	/**
	 * @return true if the ontology is classified
	 * @throws CelReasonerException
	 *             if a CEL reasoner error occurs
	 */
	public boolean isClassified() throws CelReasonerException {
		Sexp message = SexpFactory.newNonAtomicSexp();
		message.add(SexpFactory.newAtomicSexp(CelOwlApiKeyword.keyIsClassified));
		Sexp response = send(message);
		boolean ret = getParser().parseBoolean(response);
		return ret;
	}

	/**
	 * @return true if the current ontology is consistent
	 * @throws CelReasonerException
	 *             if a CEL reasoner error occurs
	 */
	public boolean isConsistent() throws CelReasonerException {
		Sexp message = SexpFactory.newNonAtomicSexp();
		message.add(SexpFactory.newAtomicSexp(CelOwlApiKeyword.keyIsConsistent));
		Sexp response = send(message);
		boolean ret = getParser().parseBoolean(response);
		return ret;
	}

	/**
	 * @param cls
	 *            OWL class
	 * @return true if it is defined
	 */
	public boolean isDefined(OWLClass cls) {
		boolean ret = false;
		if (cls.toString().equals(thing) || cls.toString().equals(nothing)) {
			ret = true;
		} else {
			ret = (this.ontology != null) && this.ontology.containsClassInSignature(cls.getIRI());
		}
		return ret;
	}

	/**
	 * @param individual
	 *            OWL named individual
	 * @return true if it is defined
	 */
	public boolean isDefined(OWLNamedIndividual individual) {
		return (this.ontology != null) && this.ontology.containsIndividualInSignature(individual.getIRI());

	}

	/**
	 * @param property
	 *            OWL object property
	 * @return true if it is defined
	 */
	public boolean isDefined(OWLObjectProperty property) {
		return (this.ontology != null) && this.ontology.containsObjectPropertyInSignature(property.getIRI());
	}

	public boolean isEntailed(Set<OWLAxiom> axioms) {
		boolean ret = true;
		for (Iterator<OWLAxiom> it = axioms.iterator(); ret && it.hasNext();) {
			OWLAxiom axiom = it.next();
			ret = ret && axiom.accept(this.entailmentChecker);
		}
		return ret;
	}

	/**
	 * @param expression0
	 *            OWL class expression
	 * @param expression1
	 *            OWL class expression
	 * @return true if the two classes are equivalent
	 * @throws CelReasonerException
	 *             if a CEL reasoner error occurs
	 */
	public boolean isEquivalentClass(OWLClassExpression expression0, OWLClassExpression expression1)
			throws CelReasonerException {
		Sexp message = SexpFactory.newNonAtomicSexp();
		try {
			message.add(SexpFactory.newAtomicSexp(CelOwlApiKeyword.keyIsEquivalentClass));
			message.add(getTranslator().translate(expression0));
			message.add(getTranslator().translate(expression1));
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
	 *            OWL object property
	 * @return true if the property is reflexive
	 * @throws CelReasonerException
	 *             if a CEL reasoner error occurs
	 */
	public boolean isReflexive(OWLObjectProperty property) throws CelReasonerException {
		Sexp message = SexpFactory.newNonAtomicSexp();
		message.add(SexpFactory.newAtomicSexp(CelOwlApiKeyword.keyIsReflexive));
		message.add(getTranslator().translate(property));
		Sexp response = send(message);
		boolean ret = getParser().parseBoolean(response);
		return ret;
	}

	/**
	 * @param expression
	 *            OWL class expression
	 * @return true if the expression is satisfiable
	 * @throws CelReasonerException
	 *             if a CEL reasoner error occurs
	 */
	public boolean isSatisfiable(OWLClassExpression expression) throws CelReasonerException {
		boolean ret = false;
		Sexp message = SexpFactory.newNonAtomicSexp();
		try {
			message.add(SexpFactory.newAtomicSexp(CelOwlApiKeyword.keyIsSatisfiable));
			message.add(getTranslator().translate(expression));
		} catch (CelTranslatorException e) {
			throw new CelReasonerException(e);
		}
		Sexp response = send(message);
		ret = getParser().parseBoolean(response);
		return ret;
	}

	/**
	 * @param expression0
	 *            OWL class expression
	 * @param expression1
	 *            OWL class expression
	 * @return true if the first expression is a subclass of the second
	 *         expression
	 * @throws CelReasonerException
	 *             if a CEL reasoner error occurs
	 */
	public boolean isSubClassOf(OWLClassExpression expression0, OWLClassExpression expression1)
			throws CelReasonerException {
		Sexp message = SexpFactory.newNonAtomicSexp();
		try {
			message.add(SexpFactory.newAtomicSexp(CelOwlApiKeyword.keyIsSubClassOf));
			message.add(getTranslator().translate(expression0));
			message.add(getTranslator().translate(expression1));
		} catch (CelTranslatorException e) {
			throw new CelReasonerException(e);
		}
		Sexp response = send(message);
		boolean ret = getParser().parseBoolean(response);
		return ret;
	}

	/**
	 * @param property
	 *            OWL object property
	 * @return true if the property is transitive
	 * @throws CelReasonerException
	 *             if a CEL reasoner error occurs
	 */
	public boolean isTransitive(OWLObjectProperty property) throws CelReasonerException {
		Sexp message = SexpFactory.newNonAtomicSexp();
		message.add(SexpFactory.newAtomicSexp(CelOwlApiKeyword.keyIsTransitive));
		message.add(getTranslator().translate(property));
		Sexp response = send(message);
		boolean ret = getParser().parseBoolean(response);
		return ret;
	}

	/**
	 * Loads the ontology.
	 *
	 * @param setOfOntologies
	 *            set of OWL ontologies
	 *
	 * @throws CelReasonerException
	 *             if a CEL reasoner error occurs
	 */
	public void loadOntologies(Set<OWLOntology> setOfOntologies) throws CelReasonerException {
		setOntologies(setOfOntologies);
		synchronizedIfChanged();
	}

	protected Set<Set<OWLObjectPropertyExpression>> makeEquivalentClasses(Set<OWLObjectPropertyExpression> flattenedSet)
			throws CelReasonerException {
		Set<Set<OWLObjectPropertyExpression>> ret = new HashSet<>();
		Set<OWLObjectPropertyExpression> visited = new HashSet<>();
		flattenedSet //
				.stream() //
				.filter(property -> !visited.contains(property)) //
				.forEach(property -> {
					Set<OWLObjectPropertyExpression> equivalentProperties = getEquivalentObjectProperties(property)
							.getEntities();
					ret.add(equivalentProperties);
					visited.addAll(equivalentProperties);
				});
		return ret;
	}

	public void precomputeInferences() throws CelReasonerException {
		synchronizedIfChanged();
		Sexp message = SexpFactory.newNonAtomicSexp();
		message.add(SexpFactory.newAtomicSexp(CelOwlApiKeyword.keyClassify));
		send(message, "Classifying ontology ...");
	}

	/**
	 * @throws CelReasonerException
	 *             if a CEL reasoner error occurs
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
		if ((getReasonerConfiguration() != null) && (getReasonerConfiguration().getProgressMonitor() != null)) {
			getReasonerConfiguration().getProgressMonitor().reasonerTaskStarted(title);
		}
		Sexp ret = send(message);
		if ((getReasonerConfiguration() != null) && (getReasonerConfiguration().getProgressMonitor() != null)) {
			getReasonerConfiguration().getProgressMonitor().reasonerTaskStopped();
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

	public void setProgressMonitor(OWLReasonerConfiguration config) {
		getSocketManager().setReasonerConfiguration(config);
	}

	public void synchronizedIfChanged() throws CelReasonerException {
		if (this.changeTracker.getOntologyChanged()) {
			Sexp message = SexpFactory.newNonAtomicSexp();
			message.add(SexpFactory.newAtomicSexp(CelOwlApiKeyword.keyClearOntologies));
			send(message, "Clearing ontologies ...");
			message = SexpFactory.newNonAtomicSexp();
			message.add(SexpFactory.newAtomicSexp(CelOwlApiKeyword.keyLoadOntologies));
			Sexp axiomSet = SexpFactory.newNonAtomicSexp();
			try {
				axiomSet.add(getTranslator().translate(this.ontology, getOWLOntologyManager()));
			} catch (CelTranslatorException e) {
				throw new CelReasonerException(e);
			}
			message.add(axiomSet);
			send(message, "Loading ontologies ...");
			this.changeTracker.setOntologyChanged(false);
		}
	}

}
