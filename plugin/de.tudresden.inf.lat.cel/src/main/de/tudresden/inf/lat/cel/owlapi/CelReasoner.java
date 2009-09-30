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

import java.util.Date;
import java.util.Map;
import java.util.Set;
import java.util.logging.Level;
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

	private static final Logger logger = Logger.getLogger(CelReasoner.class
			.getName());

	private CelReasonerInterface celInterface = null;
	private Date instanceStart = null;

	public CelReasoner(OWLOntologyManager manager) {
		this.instanceStart = new Date();
		this.celInterface = new CelReasonerInterface(manager);
	}

	public CelReasoner(OWLOntologyManager manager,
			ProgressMonitor progressMonitor) {
		this.instanceStart = new Date();
		this.celInterface = new CelReasonerInterface(manager, progressMonitor);
	}

	@Override
	public void classify() throws OWLReasonerException {
		logger.fine("(called)");
		Date start = new Date();
		getCelInterface().classify();
		Date end = new Date();
		if (logger.isLoggable(Level.CONFIG)) {
			System.out.println(" ("
					+ (end.getTime() - this.instanceStart.getTime()) + " ms) "
					+ "\tCEL classified in  "
					+ (end.getTime() - start.getTime()) + " ms ");
		}
	}

	@Override
	public void clearOntologies() throws OWLReasonerException {
		logger.fine("(called)");
		Date start = new Date();
		getCelInterface().clearOntologies();
		Date end = new Date();
		if (logger.isLoggable(Level.CONFIG)) {
			System.out.println(" ("
					+ (end.getTime() - this.instanceStart.getTime()) + " ms) "
					+ "\tCEL cleared in     "
					+ (end.getTime() - start.getTime()) + " ms ");
		}
	}

	@Override
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

	@Override
	public Set<Set<OWLClass>> getAncestorClasses(OWLDescription description)
			throws OWLReasonerException {
		logger.fine("(called)");
		return getCelInterface().getAncestorClasses(description);
	}

	@Override
	public Set<Set<OWLDataProperty>> getAncestorProperties(
			OWLDataProperty dataProperty) throws OWLReasonerException {
		throw new UnsupportedReasonerOperationInCelException();
	}

	@Override
	public Set<Set<OWLObjectProperty>> getAncestorProperties(
			OWLObjectProperty property) throws OWLReasonerException {
		logger.fine("(called) " + property.getURI());
		return getCelInterface().getAncestorProperties(property);
	}

	protected CelReasonerInterface getCelInterface() {
		return this.celInterface;
	}

	@Override
	public OWLEntity getCurrentEntity() {
		return null;
	}

	@Override
	public Map<OWLDataProperty, Set<OWLConstant>> getDataPropertyRelationships(
			OWLIndividual individual) throws OWLReasonerException {
		throw new UnsupportedReasonerOperationInCelException();
	}

	@Override
	public Set<Set<OWLClass>> getDescendantClasses(OWLDescription description)
			throws OWLReasonerException {
		logger.fine("(called) " + description.toString());
		return getCelInterface().getDescendantClasses(description);
	}

	@Override
	public Set<Set<OWLDataProperty>> getDescendantProperties(
			OWLDataProperty dataProperty) throws OWLReasonerException {
		throw new UnsupportedReasonerOperationInCelException();
	}

	@Override
	public Set<Set<OWLObjectProperty>> getDescendantProperties(
			OWLObjectProperty property) throws OWLReasonerException {
		logger.fine("(called)" + property.getURI());
		return getCelInterface().getDescendantProperties(property);
	}

	@Override
	public Set<Set<OWLDescription>> getDomains(OWLDataProperty dataProperty)
			throws OWLReasonerException {
		throw new UnsupportedReasonerOperationInCelException();
	}

	@Override
	public Set<Set<OWLDescription>> getDomains(OWLObjectProperty property)
			throws OWLReasonerException {
		throw new UnsupportedReasonerOperationInCelException();
	}

	@Override
	public Set<OWLClass> getEquivalentClasses(OWLDescription description)
			throws OWLReasonerException {
		logger.fine("(called) " + description);
		return getCelInterface().getEquivalentClasses(description);
	}

	@Override
	public Set<OWLDataProperty> getEquivalentProperties(
			OWLDataProperty dataProperty) throws OWLReasonerException {
		throw new UnsupportedReasonerOperationInCelException();
	}

	@Override
	public Set<OWLObjectProperty> getEquivalentProperties(
			OWLObjectProperty property) throws OWLReasonerException {
		logger.fine("(called) " + property.getURI());
		return getCelInterface().getEquivalentProperties(property);
	}

	@Override
	public Set<OWLClass> getInconsistentClasses() throws OWLReasonerException {
		logger.fine("(called)");
		return getCelInterface().getInconsistentClasses();
	}

	@Override
	public Set<OWLIndividual> getIndividuals(OWLDescription description,
			boolean arg1) throws OWLReasonerException {
		logger.fine("(called) " + description);
		return getCelInterface().getIndividuals(description, arg1);
	}

	@Override
	public Set<Set<OWLObjectProperty>> getInverseProperties(
			OWLObjectProperty property) throws OWLReasonerException {
		throw new UnsupportedReasonerOperationInCelException();
	}

	@Override
	public Set<OWLOntology> getLoadedOntologies() {
		logger.fine("(called)");
		return getCelInterface().getLoadedOntologies();
	}

	@Override
	public Map<OWLObjectProperty, Set<OWLIndividual>> getObjectPropertyRelationships(
			OWLIndividual individual) throws OWLReasonerException {
		throw new UnsupportedReasonerOperationInCelException();
	}

	public OWLOntologyManager getOWLOntologyManager() {
		return getCelInterface().getOWLOntologyManager();
	}

	public ProgressMonitor getProgressMonitor() {
		return getCelInterface().getProgressMonitor();
	}

	@Override
	public Set<OWLDataRange> getRanges(OWLDataProperty dataProperty)
			throws OWLReasonerException {
		throw new UnsupportedReasonerOperationInCelException();
	}

	@Override
	public Set<OWLDescription> getRanges(OWLObjectProperty property)
			throws OWLReasonerException {
		throw new UnsupportedReasonerOperationInCelException();
	}

	@Override
	public Set<OWLIndividual> getRelatedIndividuals(OWLIndividual individual,
			OWLObjectPropertyExpression propertyExpression)
			throws OWLReasonerException {
		throw new UnsupportedReasonerOperationInCelException();
	}

	@Override
	public Set<OWLConstant> getRelatedValues(OWLIndividual individual,
			OWLDataPropertyExpression dataProperty) throws OWLReasonerException {
		throw new UnsupportedReasonerOperationInCelException();
	}

	@Override
	public Set<Set<OWLClass>> getSubClasses(OWLDescription description)
			throws OWLReasonerException {
		logger.fine("(called) " + description);
		return getCelInterface().getSubClasses(description);
	}

	@Override
	public Set<Set<OWLDataProperty>> getSubProperties(
			OWLDataProperty dataProperty) throws OWLReasonerException {
		throw new UnsupportedReasonerOperationInCelException();
	}

	@Override
	public Set<Set<OWLObjectProperty>> getSubProperties(
			OWLObjectProperty property) throws OWLReasonerException {
		logger.fine("(called) " + property);
		return getCelInterface().getSubProperties(property);
	}

	@Override
	public Set<Set<OWLClass>> getSuperClasses(OWLDescription description)
			throws OWLReasonerException {
		logger.fine("(called) " + description);
		return getCelInterface().getSuperClasses(description);
	}

	@Override
	public Set<Set<OWLDataProperty>> getSuperProperties(
			OWLDataProperty dataProperty) throws OWLReasonerException {
		throw new UnsupportedReasonerOperationInCelException();
	}

	@Override
	public Set<Set<OWLObjectProperty>> getSuperProperties(
			OWLObjectProperty property) throws OWLReasonerException {
		logger.fine("(called) " + property);
		return getCelInterface().getSuperProperties(property);
	}

	@Override
	public Set<Set<OWLClass>> getTypes(OWLIndividual individual, boolean arg1)
			throws OWLReasonerException {
		logger.fine("(called)");
		return getCelInterface().getTypes(individual, arg1);
	}

	@Override
	public boolean hasDataPropertyRelationship(OWLIndividual individual,
			OWLDataPropertyExpression dataPropertyExpression, OWLConstant arg2)
			throws OWLReasonerException {
		throw new UnsupportedReasonerOperationInCelException();
	}

	@Override
	public boolean hasObjectPropertyRelationship(OWLIndividual individual0,
			OWLObjectPropertyExpression property, OWLIndividual individual1)
			throws OWLReasonerException {
		throw new UnsupportedReasonerOperationInCelException();
	}

	@Override
	public boolean hasType(OWLIndividual individual,
			OWLDescription description, boolean arg2)
			throws OWLReasonerException {
		logger.fine("(called) " + individual.getURI() + " " + description);
		return getCelInterface().hasTypes(individual, description, arg2);
	}

	@Override
	public boolean isAntiSymmetric(OWLObjectProperty property)
			throws OWLReasonerException {
		throw new UnsupportedReasonerOperationInCelException();
	}

	@Override
	public boolean isClassified() throws OWLReasonerException {
		boolean ret = getCelInterface().isClassified();
		logger.fine("(called) result: " + ret);
		return ret;
	}

	@Override
	public boolean isConsistent(OWLOntology ontology)
			throws OWLReasonerException {
		logger.fine("(called)");
		return getCelInterface().isConsistent(ontology);
	}

	@Override
	public boolean isDefined(OWLClass cls) throws OWLReasonerException {
		boolean ret = getCelInterface().isDefined(cls);
		logger.fine("(called) OWLClass " + cls.getURI() + " " + "result: "
				+ ret);
		return ret;
	}

	@Override
	public boolean isDefined(OWLDataProperty property)
			throws OWLReasonerException {
		throw new UnsupportedReasonerOperationInCelException();
	}

	@Override
	public boolean isDefined(OWLIndividual individual)
			throws OWLReasonerException {
		logger.fine("(called) OWLIndividual " + individual.getURI());
		return getCelInterface().isDefined(individual);
	}

	@Override
	public boolean isDefined(OWLObjectProperty property)
			throws OWLReasonerException {
		logger.fine("(called) OWLObjectProperty " + property.getURI());
		return getCelInterface().isDefined(property);
	}

	@Override
	public boolean isEquivalentClass(OWLDescription description0,
			OWLDescription description1) throws OWLReasonerException {
		logger.fine("(called) " + description0 + " " + description1);
		return getCelInterface().isEquivalentClass(description0, description1);
	}

	@Override
	public boolean isFunctional(OWLDataProperty dataProperty)
			throws OWLReasonerException {
		throw new UnsupportedReasonerOperationInCelException();
	}

	@Override
	public boolean isFunctional(OWLObjectProperty property)
			throws OWLReasonerException {
		throw new UnsupportedReasonerOperationInCelException();
	}

	@Override
	public boolean isInverseFunctional(OWLObjectProperty property)
			throws OWLReasonerException {
		throw new UnsupportedReasonerOperationInCelException();
	}

	@Override
	public boolean isIrreflexive(OWLObjectProperty property)
			throws OWLReasonerException {
		throw new UnsupportedReasonerOperationInCelException();
	}

	@Override
	public boolean isRealised() throws OWLReasonerException {
		logger.fine("(called)");
		return getCelInterface().isRealised();
	}

	@Override
	public boolean isReflexive(OWLObjectProperty property)
			throws OWLReasonerException {
		logger.fine("(called) " + property.getURI());
		return getCelInterface().isReflexive(property);
	}

	@Override
	public boolean isSatisfiable(OWLDescription description)
			throws OWLReasonerException {
		logger.fine("(called) " + description);
		return getCelInterface().isSatisfiable(description);
	}

	@Override
	public boolean isSubClassOf(OWLDescription description0,
			OWLDescription description1) throws OWLReasonerException {
		logger.fine("(called) " + description0 + " " + description1);
		return getCelInterface().isSubClassOf(description0, description1);
	}

	@Override
	public boolean isSymmetric(OWLObjectProperty property)
			throws OWLReasonerException {
		throw new UnsupportedReasonerOperationInCelException();
	}

	@Override
	public boolean isTransitive(OWLObjectProperty property)
			throws OWLReasonerException {
		logger.fine("(called) " + property.getURI());
		return getCelInterface().isTransitive(property);
	}

	@Override
	public void loadOntologies(Set<OWLOntology> ontologySet)
			throws OWLReasonerException {
		logger.fine("(called)");
		Date start = new Date();
		getCelInterface().loadOntologies(ontologySet);
		Date end = new Date();
		if (logger.isLoggable(Level.CONFIG)) {
			System.out.println(" ("
					+ (end.getTime() - this.instanceStart.getTime()) + " ms) "
					+ "\tCEL loaded in      "
					+ (end.getTime() - start.getTime()) + " ms ");
		}
	}

	@Override
	public void realise() throws OWLReasonerException {
		logger.fine("(called)");
		getCelInterface().realise();
	}

	@Override
	public void setProgressMonitor(ProgressMonitor progressMonitor) {
		getCelInterface().setProgressMonitor(progressMonitor);
	}

	@Override
	public void unloadOntologies(Set<OWLOntology> ontologySet)
			throws OWLReasonerException {
		throw new UnsupportedReasonerOperationInCelException();
	}
}
