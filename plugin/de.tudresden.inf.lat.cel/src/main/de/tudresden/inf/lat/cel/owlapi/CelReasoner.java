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

import java.util.Map;
import java.util.Set;
import java.util.logging.Logger;

import org.semanticweb.owl.inference.MonitorableOWLReasoner;
import org.semanticweb.owl.inference.OWLReasoner;
import org.semanticweb.owl.inference.OWLReasonerException;
import org.semanticweb.owl.model.OWLClass;
import org.semanticweb.owl.model.OWLConstant;
import org.semanticweb.owl.model.OWLDataProperty;
import org.semanticweb.owl.model.OWLDataPropertyExpression;
import org.semanticweb.owl.model.OWLDataRange;
import org.semanticweb.owl.model.OWLDescription;
import org.semanticweb.owl.model.OWLEntity;
import org.semanticweb.owl.model.OWLIndividual;
import org.semanticweb.owl.model.OWLObjectProperty;
import org.semanticweb.owl.model.OWLObjectPropertyExpression;
import org.semanticweb.owl.model.OWLOntology;
import org.semanticweb.owl.model.OWLOntologyManager;
import org.semanticweb.owl.util.ProgressMonitor;

import de.tudresden.inf.lat.cel.translation.CelReasonerInterface;

/**
 * This class is the connection with the OWL API. It filters the functions that
 * are implemented from the functions that are not, throwing an exception for
 * the unimplemented ones.
 * 
 * @author Julian Mendez
 */
public class CelReasoner implements OWLReasoner, MonitorableOWLReasoner {

	private static final Logger logger = Logger.getAnonymousLogger();

	private CelReasonerInterface celInterface = null;
	private ProgressMonitor monitor = null;

	public CelReasoner(OWLOntologyManager manager) {
		this.celInterface = new CelReasonerInterface(manager);
	}

	public void classify() throws OWLReasonerException {
		logger.fine("(called)");
		getCelInterface().classify();
	}

	public void clearOntologies() throws OWLReasonerException {
		logger.fine("(called)");
		getCelInterface().clearOntologies();
	}

	public void dispose() throws OWLReasonerException {
		logger.fine("(called)");
		getCelInterface().dispose();
	}

	@Override
	public void finalize() throws Throwable {
		logger.fine("(finalize)");
		this.dispose();
		super.finalize();
	}

	public Set<Set<OWLClass>> getAncestorClasses(OWLDescription description)
			throws OWLReasonerException {
		logger.fine("(called)");
		return getCelInterface().getAncestorClasses(description);
	}

	public Set<Set<OWLDataProperty>> getAncestorProperties(
			OWLDataProperty dataProperty) throws OWLReasonerException {
		throw new UnsupportedReasonerOperationInCelException();
	}

	public Set<Set<OWLObjectProperty>> getAncestorProperties(
			OWLObjectProperty property) throws OWLReasonerException {
		logger.fine("(called) " + property.getURI());
		return getCelInterface().getAncestorProperties(property);
	}

	protected CelReasonerInterface getCelInterface() {
		return this.celInterface;
	}

	public OWLEntity getCurrentEntity() {
		return null;
	}

	public Map<OWLDataProperty, Set<OWLConstant>> getDataPropertyRelationships(
			OWLIndividual individual) throws OWLReasonerException {
		throw new UnsupportedReasonerOperationInCelException();
	}

	public Set<Set<OWLClass>> getDescendantClasses(OWLDescription description)
			throws OWLReasonerException {
		logger.fine("(called) " + description.toString());
		return getCelInterface().getDescendantClasses(description);
	}

	public Set<Set<OWLDataProperty>> getDescendantProperties(
			OWLDataProperty dataProperty) throws OWLReasonerException {
		throw new UnsupportedReasonerOperationInCelException();
	}

	public Set<Set<OWLObjectProperty>> getDescendantProperties(
			OWLObjectProperty property) throws OWLReasonerException {
		logger.fine("(called)" + property.getURI());
		return getCelInterface().getDescendantProperties(property);
	}

	public Set<Set<OWLDescription>> getDomains(OWLDataProperty dataProperty)
			throws OWLReasonerException {
		throw new UnsupportedReasonerOperationInCelException();
	}

	public Set<Set<OWLDescription>> getDomains(OWLObjectProperty property)
			throws OWLReasonerException {
		logger.fine("(called) " + property.getURI());
		return getCelInterface().getDomains(property);
	}

	public Set<OWLClass> getEquivalentClasses(OWLDescription description)
			throws OWLReasonerException {
		logger.fine("(called) " + description);
		return getCelInterface().getEquivalentClasses(description);
	}

	public Set<OWLDataProperty> getEquivalentProperties(
			OWLDataProperty dataProperty) throws OWLReasonerException {
		throw new UnsupportedReasonerOperationInCelException();
	}

	public Set<OWLObjectProperty> getEquivalentProperties(
			OWLObjectProperty property) throws OWLReasonerException {
		logger.fine("(called) " + property.getURI());
		return getCelInterface().getEquivalentProperties(property);
	}

	public Set<OWLClass> getInconsistentClasses() throws OWLReasonerException {
		logger.fine("(called)");
		return getCelInterface().getInconsistentClasses();
	}

	public Set<OWLIndividual> getIndividuals(OWLDescription description,
			boolean arg1) throws OWLReasonerException {
		logger.fine("(called) " + description);
		return getCelInterface().getIndividuals(description, arg1);
	}

	public Set<Set<OWLObjectProperty>> getInverseProperties(
			OWLObjectProperty property) throws OWLReasonerException {
		throw new UnsupportedReasonerOperationInCelException();
	}

	public Set<OWLOntology> getLoadedOntologies() {
		logger.fine("(called)");
		return getCelInterface().getLoadedOntologies();
	}

	public Map<OWLObjectProperty, Set<OWLIndividual>> getObjectPropertyRelationships(
			OWLIndividual individual) throws OWLReasonerException {
		logger.fine("(called) " + individual.getURI());
		return getCelInterface().getObjectPropertyRelationships(individual);
	}

	public OWLOntologyManager getOWLOntologyManager() {
		return getCelInterface().getOWLOntologyManager();
	}

	public ProgressMonitor getProgressMonitor() {
		return this.monitor;
	}

	public Set<OWLDataRange> getRanges(OWLDataProperty dataProperty)
			throws OWLReasonerException {
		throw new UnsupportedReasonerOperationInCelException();
	}

	public Set<OWLDescription> getRanges(OWLObjectProperty property)
			throws OWLReasonerException {
		logger.fine("(called)");
		return getCelInterface().getRanges(property);
	}

	public Set<OWLIndividual> getRelatedIndividuals(OWLIndividual individual,
			OWLObjectPropertyExpression propertyExpression)
			throws OWLReasonerException {
		logger.fine("(called) " + individual.getURI() + " "
				+ propertyExpression);
		return getCelInterface().getRelatedIndividuals(individual,
				propertyExpression);
	}

	public Set<OWLConstant> getRelatedValues(OWLIndividual individual,
			OWLDataPropertyExpression dataProperty) throws OWLReasonerException {
		throw new UnsupportedReasonerOperationInCelException();
	}

	public Set<Set<OWLClass>> getSubClasses(OWLDescription description)
			throws OWLReasonerException {
		logger.fine("(called) " + description);
		return getCelInterface().getSubClasses(description);
	}

	public Set<Set<OWLDataProperty>> getSubProperties(
			OWLDataProperty dataProperty) throws OWLReasonerException {
		throw new UnsupportedReasonerOperationInCelException();
	}

	public Set<Set<OWLObjectProperty>> getSubProperties(
			OWLObjectProperty property) throws OWLReasonerException {
		logger.fine("(called) " + property.getURI());
		return getCelInterface().getSubProperties(property);
	}

	public Set<Set<OWLClass>> getSuperClasses(OWLDescription description)
			throws OWLReasonerException {
		logger.fine("(called) " + description);
		return getCelInterface().getSuperClasses(description);
	}

	public Set<Set<OWLDataProperty>> getSuperProperties(
			OWLDataProperty dataProperty) throws OWLReasonerException {
		throw new UnsupportedReasonerOperationInCelException();
	}

	public Set<Set<OWLObjectProperty>> getSuperProperties(
			OWLObjectProperty property) throws OWLReasonerException {
		logger.fine("(called) " + property.getURI());
		return getCelInterface().getSuperProperties(property);
	}

	public Set<Set<OWLClass>> getTypes(OWLIndividual individual, boolean arg1)
			throws OWLReasonerException {
		logger.fine("(called)");
		return getCelInterface().getTypes(individual, arg1);
	}

	public boolean hasDataPropertyRelationship(OWLIndividual individual,
			OWLDataPropertyExpression dataPropertyExpression, OWLConstant arg2)
			throws OWLReasonerException {
		throw new UnsupportedReasonerOperationInCelException();
	}

	public boolean hasObjectPropertyRelationship(OWLIndividual individual0,
			OWLObjectPropertyExpression property, OWLIndividual individual1)
			throws OWLReasonerException {
		logger.fine("(called) " + individual0.getURI() + " " + property + " "
				+ individual1.getURI());
		return getCelInterface().hasObjectPropertyRelationship(individual0,
				property, individual1);
	}

	public boolean hasType(OWLIndividual individual,
			OWLDescription description, boolean arg2)
			throws OWLReasonerException {
		logger.fine("(called) " + individual.getURI() + " " + description);
		return getCelInterface().hasTypes(individual, description, arg2);
	}

	public boolean isAntiSymmetric(OWLObjectProperty property)
			throws OWLReasonerException {
		throw new UnsupportedReasonerOperationInCelException();
	}

	public boolean isClassified() throws OWLReasonerException {
		boolean ret = getCelInterface().isClassified();
		logger.fine("(called) result: " + ret);
		return ret;
	}

	public boolean isConsistent(OWLOntology ontology)
			throws OWLReasonerException {
		logger.fine("(called)");
		return getCelInterface().isConsistent(ontology);
	}

	public boolean isDefined(OWLClass cls) throws OWLReasonerException {
		boolean ret = getCelInterface().isDefined(cls);
		logger.fine("(called) OWLClass " + cls.getURI() + " " + "result: "
				+ ret);
		return ret;
	}

	public boolean isDefined(OWLDataProperty property)
			throws OWLReasonerException {
		throw new UnsupportedReasonerOperationInCelException();
	}

	public boolean isDefined(OWLIndividual individual)
			throws OWLReasonerException {
		logger.fine("(called) OWLIndividual " + individual.getURI());
		return getCelInterface().isDefined(individual);
	}

	public boolean isDefined(OWLObjectProperty property)
			throws OWLReasonerException {
		logger.fine("(called) OWLObjectProperty " + property.getURI());
		return getCelInterface().isDefined(property);
	}

	public boolean isEquivalentClass(OWLDescription description0,
			OWLDescription description1) throws OWLReasonerException {
		logger.fine("(called) " + description0 + " " + description1);
		return getCelInterface().isEquivalentClass(description0, description1);
	}

	public boolean isFunctional(OWLDataProperty dataProperty)
			throws OWLReasonerException {
		throw new UnsupportedReasonerOperationInCelException();
	}

	public boolean isFunctional(OWLObjectProperty property)
			throws OWLReasonerException {
		throw new UnsupportedReasonerOperationInCelException();
	}

	public boolean isInverseFunctional(OWLObjectProperty property)
			throws OWLReasonerException {
		throw new UnsupportedReasonerOperationInCelException();
	}

	public boolean isIrreflexive(OWLObjectProperty property)
			throws OWLReasonerException {
		throw new UnsupportedReasonerOperationInCelException();
	}

	public boolean isRealised() throws OWLReasonerException {
		logger.fine("(called)");
		return getCelInterface().isRealised();
	}

	public boolean isReflexive(OWLObjectProperty property)
			throws OWLReasonerException {
		logger.fine("(called) " + property.getURI());
		return getCelInterface().isReflexive(property);
	}

	public boolean isSatisfiable(OWLDescription description)
			throws OWLReasonerException {
		logger.fine("(called) " + description);
		return getCelInterface().isSatisfiable(description);
	}

	public boolean isSubClassOf(OWLDescription description0,
			OWLDescription description1) throws OWLReasonerException {
		logger.fine("(called) " + description0 + " " + description1);
		return getCelInterface().isSubClassOf(description0, description1);
	}

	public boolean isSymmetric(OWLObjectProperty property)
			throws OWLReasonerException {
		throw new UnsupportedReasonerOperationInCelException();
	}

	public boolean isTransitive(OWLObjectProperty property)
			throws OWLReasonerException {
		logger.fine("(called) " + property.getURI());
		return getCelInterface().isTransitive(property);
	}

	public void loadOntologies(Set<OWLOntology> ontologySet)
			throws OWLReasonerException {
		logger.fine("(called)");
		getCelInterface().loadOntologies(ontologySet);
	}

	public void realise() throws OWLReasonerException {
		logger.fine("(called)");
		getCelInterface().realise();
	}

	public void setProgressMonitor(ProgressMonitor pMonitor) {
		this.monitor = pMonitor;
	}

	public void unloadOntologies(Set<OWLOntology> ontologySet)
			throws OWLReasonerException {
		logger.fine("(called)");
		getCelInterface().unloadOntologies(ontologySet);
	}
}
