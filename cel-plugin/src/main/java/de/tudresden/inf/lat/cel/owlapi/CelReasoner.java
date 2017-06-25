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
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.semanticweb.owlapi.model.AxiomType;
import org.semanticweb.owlapi.model.OWLAxiom;
import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLClassExpression;
import org.semanticweb.owlapi.model.OWLDataProperty;
import org.semanticweb.owlapi.model.OWLDataPropertyExpression;
import org.semanticweb.owlapi.model.OWLLiteral;
import org.semanticweb.owlapi.model.OWLNamedIndividual;
import org.semanticweb.owlapi.model.OWLObjectPropertyExpression;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyChange;
import org.semanticweb.owlapi.reasoner.AxiomNotInProfileException;
import org.semanticweb.owlapi.reasoner.BufferingMode;
import org.semanticweb.owlapi.reasoner.ClassExpressionNotInProfileException;
import org.semanticweb.owlapi.reasoner.FreshEntitiesException;
import org.semanticweb.owlapi.reasoner.FreshEntityPolicy;
import org.semanticweb.owlapi.reasoner.InconsistentOntologyException;
import org.semanticweb.owlapi.reasoner.IndividualNodeSetPolicy;
import org.semanticweb.owlapi.reasoner.InferenceType;
import org.semanticweb.owlapi.reasoner.Node;
import org.semanticweb.owlapi.reasoner.NodeSet;
import org.semanticweb.owlapi.reasoner.OWLReasoner;
import org.semanticweb.owlapi.reasoner.OWLReasonerConfiguration;
import org.semanticweb.owlapi.reasoner.ReasonerInterruptedException;
import org.semanticweb.owlapi.reasoner.TimeOutException;
import org.semanticweb.owlapi.reasoner.UnsupportedEntailmentTypeException;
import org.semanticweb.owlapi.util.Version;

import de.tudresden.inf.lat.cel.translation.CelReasonerInterface;

/**
 * This class is the connection with the OWL API. It separates the functions
 * that are implemented from the functions that are not, throwing an exception
 * for the unimplemented ones.
 * 
 * @author Julian Mendez
 */
public class CelReasoner implements OWLReasoner {

	private static final Logger logger = Logger.getLogger(CelReasoner.class.getName());
	private CelReasonerInterface celInterface = null;
	private Date instanceStart = new Date();
	private final Set<AxiomType<?>> supportedAxiomTypes;

	public CelReasoner(OWLOntology ontology) {
		this.instanceStart = new Date();
		this.celInterface = new CelReasonerInterface(ontology);
		this.supportedAxiomTypes = getSupportedTypes();
	}

	public CelReasoner(OWLOntology ontology, OWLReasonerConfiguration config) {
		this.instanceStart = new Date();
		this.celInterface = new CelReasonerInterface(ontology, config);
		this.supportedAxiomTypes = getSupportedTypes();
	}

	@Override
	public void dispose() {
		logger.finer("dispose()");
		getCelInterface().dispose();
	}

	@Override
	public void finalize() throws Throwable {
		logger.finer("finalize()");
		this.dispose();
		super.finalize();
	}

	@Override
	public void flush() {
		logger.finer("flush()");
		// does nothing
	}

	@Override
	public Node<OWLClass> getBottomClassNode() {
		logger.finer("getBottomClassNode()");
		return getCelInterface().getBottomClassNode();
	}

	@Override
	public Node<OWLDataProperty> getBottomDataPropertyNode() {
		logger.finer("getBottomDataPropertyNode()");
		throw new UnsupportedReasonerOperationInCelException();
	}

	@Override
	public Node<OWLObjectPropertyExpression> getBottomObjectPropertyNode() {
		logger.finer("getBottomObjectPropertyNode()");
		return getCelInterface().getBottomObjectPropertyNode();
	}

	@Override
	public BufferingMode getBufferingMode() {
		logger.finer("getBufferingMode()");
		return BufferingMode.NON_BUFFERING;
	}

	protected CelReasonerInterface getCelInterface() {
		return this.celInterface;
	}

	@Override
	public NodeSet<OWLClass> getDataPropertyDomains(OWLDataProperty property, boolean direct)
			throws InconsistentOntologyException, FreshEntitiesException, ReasonerInterruptedException,
			TimeOutException {
		logger.finer("getDataPropertyDomains(" + property + ", " + direct + ")");
		throw new UnsupportedReasonerOperationInCelException();
	}

	@Override
	public Set<OWLLiteral> getDataPropertyValues(OWLNamedIndividual individual, OWLDataProperty property)
			throws InconsistentOntologyException, FreshEntitiesException, ReasonerInterruptedException,
			TimeOutException {
		logger.finer("getDataPropertyValues(" + individual + ", " + property + ")");
		throw new UnsupportedReasonerOperationInCelException();
	}

	@Override
	public NodeSet<OWLNamedIndividual> getDifferentIndividuals(OWLNamedIndividual individual)
			throws InconsistentOntologyException, FreshEntitiesException, ReasonerInterruptedException,
			TimeOutException {
		logger.finer("getDifferentIndividuals(" + individual + ")");
		throw new UnsupportedReasonerOperationInCelException();
	}

	@Override
	public NodeSet<OWLClass> getDisjointClasses(OWLClassExpression classExpression) {
		logger.finer("getDisjointClasses(" + classExpression + ")");
		throw new UnsupportedReasonerOperationInCelException();
	}

	@Override
	public NodeSet<OWLDataProperty> getDisjointDataProperties(OWLDataPropertyExpression property)
			throws InconsistentOntologyException, FreshEntitiesException, ReasonerInterruptedException,
			TimeOutException {
		logger.finer("getDisjointDataProperties(" + property + ")");
		throw new UnsupportedReasonerOperationInCelException();
	}

	@Override
	public NodeSet<OWLObjectPropertyExpression> getDisjointObjectProperties(
			OWLObjectPropertyExpression objectPropertyExpression) throws InconsistentOntologyException,
			FreshEntitiesException, ReasonerInterruptedException, TimeOutException {
		logger.finer("getDisjointObjectProperties(" + objectPropertyExpression + ")");
		throw new UnsupportedReasonerOperationInCelException();
	}

	@Override
	public Node<OWLClass> getEquivalentClasses(OWLClassExpression classExpression) {
		logger.finer("getEquivalentClasses(" + classExpression + ")");
		return getCelInterface().getEquivalentClasses(classExpression);
	}

	@Override
	public Node<OWLDataProperty> getEquivalentDataProperties(OWLDataProperty property)
			throws InconsistentOntologyException, FreshEntitiesException, ReasonerInterruptedException,
			TimeOutException {
		logger.finer("getEquivalentDataProperties(" + property + ")");
		throw new UnsupportedReasonerOperationInCelException();
	}

	@Override
	public Node<OWLObjectPropertyExpression> getEquivalentObjectProperties(
			OWLObjectPropertyExpression objectPropertyExpression) throws InconsistentOntologyException,
			FreshEntitiesException, ReasonerInterruptedException, TimeOutException {
		logger.finer("getEquivalentObjectProperties(" + objectPropertyExpression + ")");
		return getCelInterface().getEquivalentObjectProperties(objectPropertyExpression);
	}

	@Override
	public FreshEntityPolicy getFreshEntityPolicy() {
		logger.finer("getFreshEntityPolicy()");
		throw new UnsupportedReasonerOperationInCelException();
	}

	@Override
	public IndividualNodeSetPolicy getIndividualNodeSetPolicy() {
		logger.finer("getIndividualNodeSetPolicy()");
		throw new UnsupportedReasonerOperationInCelException();
	}

	@Override
	public NodeSet<OWLNamedIndividual> getInstances(OWLClassExpression classExpression, boolean direct)
			throws InconsistentOntologyException, ClassExpressionNotInProfileException, FreshEntitiesException,
			ReasonerInterruptedException, TimeOutException {
		logger.finer("getInstances(" + classExpression + ", " + direct + ")");
		return getCelInterface().getInstances(classExpression, direct);
	}

	@Override
	public Node<OWLObjectPropertyExpression> getInverseObjectProperties(
			OWLObjectPropertyExpression objectPropertyExpression) throws InconsistentOntologyException,
			FreshEntitiesException, ReasonerInterruptedException, TimeOutException {
		logger.finer("getInverseObjectProperties(" + objectPropertyExpression + ")");
		throw new UnsupportedReasonerOperationInCelException();
	}

	@Override
	public NodeSet<OWLClass> getObjectPropertyDomains(OWLObjectPropertyExpression objectPropertyExpression,
			boolean direct) throws InconsistentOntologyException, FreshEntitiesException, ReasonerInterruptedException,
			TimeOutException {
		logger.finer("getObjectPropertyDomains(" + objectPropertyExpression + ", " + direct + ")");
		throw new UnsupportedReasonerOperationInCelException();
	}

	@Override
	public NodeSet<OWLClass> getObjectPropertyRanges(OWLObjectPropertyExpression objectPropertyExpression,
			boolean direct) throws InconsistentOntologyException, FreshEntitiesException, ReasonerInterruptedException,
			TimeOutException {
		logger.finer("getObjectPropertyRanges(" + objectPropertyExpression + ", " + direct + ")");
		throw new UnsupportedReasonerOperationInCelException();
	}

	@Override
	public NodeSet<OWLNamedIndividual> getObjectPropertyValues(OWLNamedIndividual individual,
			OWLObjectPropertyExpression objectPropertyExpression) throws InconsistentOntologyException,
			FreshEntitiesException, ReasonerInterruptedException, TimeOutException {
		logger.finer("getObjectPropertyValues(" + individual + ", " + objectPropertyExpression + ")");
		throw new UnsupportedReasonerOperationInCelException();
	}

	@Override
	public Set<OWLAxiom> getPendingAxiomAdditions() {
		logger.finer("getPendingAxiomAdditions()");
		return getCelInterface().getPendingAxiomAdditions();
	}

	@Override
	public Set<OWLAxiom> getPendingAxiomRemovals() {
		logger.finer("getPendingAxiomRemovals()");
		return getCelInterface().getPendingAxiomRemovals();
	}

	@Override
	public List<OWLOntologyChange> getPendingChanges() {
		logger.finer("getPendingChanges()");
		return getCelInterface().getPendingChanges();
	}

	@Override
	public Set<InferenceType> getPrecomputableInferenceTypes() {
		logger.finer("getPrecomputableInferenceTypes()");
		return getCelInterface().getPrecomputableInferenceTypes();
	}

	public OWLReasonerConfiguration getReasonerConfiguration() {
		return getCelInterface().getReasonerConfiguration();
	}

	@Override
	public String getReasonerName() {
		logger.finer("getReasonerName()");
		return getCelInterface().getReasonerName();
	}

	@Override
	public Version getReasonerVersion() {
		logger.finer("getReasonerVersion()");
		return getCelInterface().getReasonerVersion();
	}

	@Override
	public OWLOntology getRootOntology() {
		logger.finer("getRootOntology()");
		return getCelInterface().getRootOntology();
	}

	@Override
	public Node<OWLNamedIndividual> getSameIndividuals(OWLNamedIndividual individual)
			throws InconsistentOntologyException, FreshEntitiesException, ReasonerInterruptedException,
			TimeOutException {
		logger.finer("getSameIndividuals(" + individual + ")");
		throw new UnsupportedReasonerOperationInCelException();
	}

	@Override
	public NodeSet<OWLClass> getSubClasses(OWLClassExpression classExpression, boolean direct) {
		logger.finer("getSubClasses(" + classExpression + ", " + direct + ")");
		return getCelInterface().getSubClasses(classExpression, direct);
	}

	@Override
	public NodeSet<OWLDataProperty> getSubDataProperties(OWLDataProperty property, boolean direct)
			throws InconsistentOntologyException, FreshEntitiesException, ReasonerInterruptedException,
			TimeOutException {
		logger.finer("getSubDataProperties(" + property + ", " + direct + ")");
		throw new UnsupportedReasonerOperationInCelException();
	}

	@Override
	public NodeSet<OWLObjectPropertyExpression> getSubObjectProperties(
			OWLObjectPropertyExpression objectPropertyExpression, boolean direct) throws InconsistentOntologyException,
			FreshEntitiesException, ReasonerInterruptedException, TimeOutException {
		logger.finer("getSubObjectProperties(" + objectPropertyExpression + ", " + direct + ")");
		return getCelInterface().getSubObjectProperties(objectPropertyExpression, direct);
	}

	@Override
	public NodeSet<OWLClass> getSuperClasses(OWLClassExpression classExpression, boolean direct)
			throws InconsistentOntologyException, ClassExpressionNotInProfileException, FreshEntitiesException,
			ReasonerInterruptedException, TimeOutException {
		logger.finer("getSuperClasses(" + classExpression + ", " + direct + ")");
		return getCelInterface().getSuperClasses(classExpression, direct);
	}

	@Override
	public NodeSet<OWLDataProperty> getSuperDataProperties(OWLDataProperty dataProperty, boolean direct)
			throws InconsistentOntologyException, FreshEntitiesException, ReasonerInterruptedException,
			TimeOutException {
		logger.finer("getSuperDataProperties(" + dataProperty + ", " + direct + ")");
		throw new UnsupportedReasonerOperationInCelException();
	}

	@Override
	public NodeSet<OWLObjectPropertyExpression> getSuperObjectProperties(
			OWLObjectPropertyExpression objectPropertyExpression, boolean direct) throws InconsistentOntologyException,
			FreshEntitiesException, ReasonerInterruptedException, TimeOutException {
		logger.finer("getSuperObjectProperties(" + objectPropertyExpression + ", " + direct + ")");
		return getCelInterface().getSuperObjectProperties(objectPropertyExpression, direct);
	}

	private Set<AxiomType<?>> getSupportedTypes() {
		Set<AxiomType<?>> ret = new HashSet<>();
		ret.add(AxiomType.EQUIVALENT_CLASSES);
		ret.add(AxiomType.SUBCLASS_OF);
		ret.add(AxiomType.SUB_OBJECT_PROPERTY);
		return Collections.unmodifiableSet(ret);
	}

	@Override
	public long getTimeOut() {
		logger.finer("getTimeOut()");
		return getCelInterface().getTimeOut();
	}

	@Override
	public Node<OWLClass> getTopClassNode() {
		logger.finer("getTopClassNode()");
		return getCelInterface().getTopClassNode();
	}

	@Override
	public Node<OWLDataProperty> getTopDataPropertyNode() {
		logger.finer("getTopDataPropertyNode()");
		throw new UnsupportedReasonerOperationInCelException();
	}

	@Override
	public Node<OWLObjectPropertyExpression> getTopObjectPropertyNode() {
		logger.finer("getTopObjectPropertyNode()");
		return getCelInterface().getTopObjectPropertyNode();
	}

	@Override
	public NodeSet<OWLClass> getTypes(OWLNamedIndividual individual, boolean direct)
			throws InconsistentOntologyException, FreshEntitiesException, ReasonerInterruptedException,
			TimeOutException {
		logger.finer("getTypes(" + individual + ", " + direct + ")");
		return getCelInterface().getTypes(individual, direct);
	}

	@Override
	public Node<OWLClass> getUnsatisfiableClasses() throws ReasonerInterruptedException, TimeOutException {
		logger.finer("getUnsatisfiableClasses()");
		return getCelInterface().getUnsatisfiableClasses();
	}

	@Override
	public void interrupt() {
		logger.finer("interrupt()");
		throw new UnsupportedReasonerOperationInCelException();
	}

	@Override
	public boolean isConsistent() throws ReasonerInterruptedException, TimeOutException {
		logger.finer("isConsistent()");
		return getCelInterface().isConsistent();
	}

	@Override
	public boolean isEntailed(OWLAxiom axiom) throws ReasonerInterruptedException, UnsupportedEntailmentTypeException,
			TimeOutException, AxiomNotInProfileException, FreshEntitiesException {
		logger.finer("isEntailed((OWLAxiom) " + axiom + ")");
		boolean ret = getCelInterface().isEntailed(Collections.singleton(axiom));
		logger.finer("" + ret);
		return ret;
	}

	@Override
	public boolean isEntailed(Set<? extends OWLAxiom> axiomSet) throws ReasonerInterruptedException,
			UnsupportedEntailmentTypeException, TimeOutException, AxiomNotInProfileException, FreshEntitiesException {
		logger.finer("isEntailed((Set<? extends OWLAxiom>) " + axiomSet + ")");
		Set<OWLAxiom> set = new HashSet<>();
		axiomSet.forEach(axiom -> {
			set.add(axiom);
		});
		boolean ret = getCelInterface().isEntailed(set);
		logger.finer("" + ret);
		return ret;
	}

	@Override
	public boolean isEntailmentCheckingSupported(AxiomType<?> axiomType) {
		logger.finer("isEntailmentCheckingSupported(" + axiomType + ")");
		boolean ret = this.supportedAxiomTypes.contains(axiomType);
		logger.finer("" + ret);
		return ret;

	}

	@Override
	public boolean isPrecomputed(InferenceType inferenceType) {
		logger.finer("isPrecomputed(" + inferenceType + ")");
		throw new UnsupportedReasonerOperationInCelException();
	}

	@Override
	public boolean isSatisfiable(OWLClassExpression classExpression) {
		logger.finer("isSatisfiable(" + classExpression + ")");
		return getCelInterface().isSatisfiable(classExpression);
	}

	@Override
	public void precomputeInferences(InferenceType... inferenceTypes) {
		logger.finer("precomputeInferences(" + inferenceTypes + ")");
		Date start = new Date();
		getCelInterface().precomputeInferences();
		Date end = new Date();
		if (logger.isLoggable(Level.CONFIG)) {
			System.out.println(" (" + (end.getTime() - this.instanceStart.getTime()) + " ms) " + "\tCEL classified in  "
					+ (end.getTime() - start.getTime()) + " ms ");
		}
	}

}
