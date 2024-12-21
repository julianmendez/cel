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

import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.io.Writer;
import java.util.Set;
import java.util.TreeSet;

import org.semanticweb.owlapi.io.OWLRendererException;
import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLEntity;
import org.semanticweb.owlapi.model.OWLNamedIndividual;
import org.semanticweb.owlapi.model.OWLObjectProperty;
import org.semanticweb.owlapi.model.OWLObjectPropertyExpression;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.owlxml.renderer.OWLXMLWriter;
import org.semanticweb.owlapi.reasoner.OWLReasoner;
import org.semanticweb.owlapi.vocab.OWLXMLVocabulary;

/**
 * This class makes an XML representation of the inferred data by an
 * OWLReasoner.
 * 
 * @author Julian Mendez
 */
public class OWLReasonerXMLOutput {

	private OWLReasoner reasoner = null;
	private OWLXMLWriter writer = null;

	public OWLReasonerXMLOutput(OWLReasoner reasoner) {
		this.reasoner = reasoner;
	}

	private void render() {
		Set<OWLClass> classSet = new TreeSet<>();
		classSet.addAll(this.reasoner.getRootOntology().getClassesInSignature());
		Set<OWLObjectProperty> propertySet = new TreeSet<>();
		propertySet.addAll(this.reasoner.getRootOntology().getObjectPropertiesInSignature());
		Set<OWLNamedIndividual> individualSet = new TreeSet<>();
		individualSet.addAll(this.reasoner.getRootOntology().getIndividualsInSignature());

		renderDeclaration(classSet);
		renderDeclaration(propertySet);
		renderDeclaration(individualSet);

		Set<OWLClass> classesToVisit = new TreeSet<>();
		classesToVisit.addAll(classSet);
		while (!classesToVisit.isEmpty()) {
			OWLClass cls = classesToVisit.iterator().next();
			classesToVisit.remove(cls);
			Set<OWLClass> equivClasses = this.reasoner.getEquivalentClasses(cls).getEntities();
			if (equivClasses.size() > 1) {
				renderEquivalentClasses(equivClasses);
			}
			classesToVisit.removeAll(equivClasses);
		}

		Set<OWLObjectProperty> propertiesToVisit = new TreeSet<>();
		propertiesToVisit.addAll(propertySet);
		while (!propertiesToVisit.isEmpty()) {
			OWLObjectProperty property = propertiesToVisit.iterator().next();
			propertiesToVisit.remove(property);
			Set<OWLObjectPropertyExpression> equivProperties = this.reasoner.getEquivalentObjectProperties(property)
					.getEntities();
			if (equivProperties.size() > 1) {
				renderEquivalentObjectPropertyExpressions(equivProperties);
			}
			propertiesToVisit.removeAll(equivProperties);
		}

		classSet.forEach(subClass -> {
			Set<OWLClass> superClasses = new TreeSet<>();
			superClasses.addAll(this.reasoner.getSuperClasses(subClass, true).getFlattened());
			superClasses.forEach(superClass -> {
				renderSubClassOf(subClass, superClass);
			});
		});

		propertySet.forEach(subProperty -> {
			Set<OWLObjectPropertyExpression> superProperties = new TreeSet<>();
			superProperties.addAll(this.reasoner.getSuperObjectProperties(subProperty, true).getFlattened());
			superProperties.forEach(superProperty -> {
				renderSubObjectPropertyOf(subProperty.asOWLObjectProperty(), superProperty.asOWLObjectProperty());
			});
		});

		classSet.forEach(cls -> {
			Set<OWLNamedIndividual> instances = new TreeSet<>();
			instances.addAll(this.reasoner.getInstances(cls, true).getFlattened());
			instances.forEach(individual -> {
				renderClassAssertion(cls, individual);
			});
		});

		propertySet.forEach(property -> {
			individualSet.forEach(individual -> {
				Set<OWLNamedIndividual> propertyValues = new TreeSet<>();
				propertyValues.addAll(this.reasoner.getObjectPropertyValues(individual, property.asOWLObjectProperty())
						.getFlattened());
				propertyValues.forEach(otherIndividual -> {
					renderObjectPropertyAssertion(property, individual, otherIndividual);
				});
			});
		});
	}

	private void renderClassAssertion(OWLClass cls, OWLNamedIndividual individual) {
		this.writer.writeStartElement(OWLXMLVocabulary.CLASS_ASSERTION);
		renderEntity(cls);
		renderEntity(individual);
		this.writer.writeEndElement();

	}

	private void renderDeclaration(Set<? extends OWLEntity> entities) {
		entities.forEach(elem -> {
			this.writer.writeStartElement(OWLXMLVocabulary.DECLARATION);
			renderEntity(elem);
			this.writer.writeEndElement();
		});
	}

	private void renderEntity(OWLEntity entity) {
		if (entity instanceof OWLClass) {
			this.writer.writeStartElement(OWLXMLVocabulary.CLASS);
		} else if (entity instanceof OWLObjectProperty) {
			this.writer.writeStartElement(OWLXMLVocabulary.OBJECT_PROPERTY);
		} else if (entity instanceof OWLNamedIndividual) {
			this.writer.writeStartElement(OWLXMLVocabulary.NAMED_INDIVIDUAL);
		} else {
			throw new IllegalStateException("Entity cannot be rendered : '" + entity + "'.");
		}
		this.writer.writeIRIAttribute(entity.getIRI());
		this.writer.writeEndElement();
	}

	private void renderEntitySet(Set<? extends OWLEntity> entitySet) {
		Set<OWLEntity> set = new TreeSet<>();
		set.addAll(entitySet);
		set.forEach(entity -> {
			renderEntity(entity);
		});
	}

	private void renderEquivalentClasses(Set<OWLClass> classSet) {
		this.writer.writeStartElement(OWLXMLVocabulary.EQUIVALENT_CLASSES);
		renderEntitySet(classSet);
		this.writer.writeEndElement();
	}

	private void renderEquivalentObjectPropertyExpressions(Set<OWLObjectPropertyExpression> propertySet) {
		this.writer.writeStartElement(OWLXMLVocabulary.EQUIVALENT_OBJECT_PROPERTIES);
		Set<OWLObjectPropertyExpression> set = new TreeSet<>();
		set.addAll(propertySet);
		set.forEach(propertyExpression -> {
			renderEntity(propertyExpression.asOWLObjectProperty());
		});
		this.writer.writeEndElement();
	}

	private void renderObjectPropertyAssertion(OWLObjectProperty property, OWLNamedIndividual subject,
			OWLNamedIndividual object) {
		this.writer.writeStartElement(OWLXMLVocabulary.OBJECT_PROPERTY_ASSERTION);
		renderEntity(property);
		renderEntity(subject);
		renderEntity(object);
		this.writer.writeEndElement();
	}

	private void renderSubClassOf(OWLClass subClass, OWLClass superClass) {
		this.writer.writeStartElement(OWLXMLVocabulary.SUB_CLASS_OF);
		renderEntity(subClass);
		renderEntity(superClass);
		this.writer.writeEndElement();
	}

	private void renderSubObjectPropertyOf(OWLObjectProperty subProperty, OWLObjectProperty superProperty) {
		this.writer.writeStartElement(OWLXMLVocabulary.SUB_OBJECT_PROPERTY_OF);
		renderEntity(subProperty);
		renderEntity(superProperty);
		this.writer.writeEndElement();
	}

	public void toXML(OutputStream out) throws OWLRendererException {
		OWLOntology ontology = this.reasoner.getRootOntology();
		Writer writer = new OutputStreamWriter(out);
		PrintWriter printWriter = new PrintWriter(writer);
		this.writer = new OWLXMLWriter(printWriter, ontology);
		this.writer.startDocument(ontology);
		render();
		this.writer.endDocument();
	}

}
