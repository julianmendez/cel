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

import org.semanticweb.owlapi.model.OWLAnnotationAssertionAxiom;
import org.semanticweb.owlapi.model.OWLAnnotationPropertyDomainAxiom;
import org.semanticweb.owlapi.model.OWLAnnotationPropertyRangeAxiom;
import org.semanticweb.owlapi.model.OWLAsymmetricObjectPropertyAxiom;
import org.semanticweb.owlapi.model.OWLAxiomVisitorEx;
import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLClassAssertionAxiom;
import org.semanticweb.owlapi.model.OWLClassExpression;
import org.semanticweb.owlapi.model.OWLDataPropertyAssertionAxiom;
import org.semanticweb.owlapi.model.OWLDataPropertyDomainAxiom;
import org.semanticweb.owlapi.model.OWLDataPropertyRangeAxiom;
import org.semanticweb.owlapi.model.OWLDatatypeDefinitionAxiom;
import org.semanticweb.owlapi.model.OWLDeclarationAxiom;
import org.semanticweb.owlapi.model.OWLDifferentIndividualsAxiom;
import org.semanticweb.owlapi.model.OWLDisjointClassesAxiom;
import org.semanticweb.owlapi.model.OWLDisjointDataPropertiesAxiom;
import org.semanticweb.owlapi.model.OWLDisjointObjectPropertiesAxiom;
import org.semanticweb.owlapi.model.OWLDisjointUnionAxiom;
import org.semanticweb.owlapi.model.OWLEquivalentClassesAxiom;
import org.semanticweb.owlapi.model.OWLEquivalentDataPropertiesAxiom;
import org.semanticweb.owlapi.model.OWLEquivalentObjectPropertiesAxiom;
import org.semanticweb.owlapi.model.OWLFunctionalDataPropertyAxiom;
import org.semanticweb.owlapi.model.OWLFunctionalObjectPropertyAxiom;
import org.semanticweb.owlapi.model.OWLHasKeyAxiom;
import org.semanticweb.owlapi.model.OWLInverseFunctionalObjectPropertyAxiom;
import org.semanticweb.owlapi.model.OWLInverseObjectPropertiesAxiom;
import org.semanticweb.owlapi.model.OWLIrreflexiveObjectPropertyAxiom;
import org.semanticweb.owlapi.model.OWLNegativeDataPropertyAssertionAxiom;
import org.semanticweb.owlapi.model.OWLNegativeObjectPropertyAssertionAxiom;
import org.semanticweb.owlapi.model.OWLObjectProperty;
import org.semanticweb.owlapi.model.OWLObjectPropertyAssertionAxiom;
import org.semanticweb.owlapi.model.OWLObjectPropertyDomainAxiom;
import org.semanticweb.owlapi.model.OWLObjectPropertyRangeAxiom;
import org.semanticweb.owlapi.model.OWLReflexiveObjectPropertyAxiom;
import org.semanticweb.owlapi.model.OWLSameIndividualAxiom;
import org.semanticweb.owlapi.model.OWLSubAnnotationPropertyOfAxiom;
import org.semanticweb.owlapi.model.OWLSubClassOfAxiom;
import org.semanticweb.owlapi.model.OWLSubDataPropertyOfAxiom;
import org.semanticweb.owlapi.model.OWLSubObjectPropertyOfAxiom;
import org.semanticweb.owlapi.model.OWLSubPropertyChainOfAxiom;
import org.semanticweb.owlapi.model.OWLSymmetricObjectPropertyAxiom;
import org.semanticweb.owlapi.model.OWLTransitiveObjectPropertyAxiom;
import org.semanticweb.owlapi.model.SWRLRule;

/**
 * An object of this class checks ontology entailment.
 * 
 * @author Julian Mendez
 */
public class OntologyEntailmentChecker implements OWLAxiomVisitorEx<Boolean> {

	private static final String errorMsg = "Unsupported entailment with axiom:";
	private final CelReasonerInterface reasoner;

	/**
	 * Constructs a new ontology entailment checker.
	 * 
	 * @param reasoner
	 *            reasoner
	 */
	public OntologyEntailmentChecker(CelReasonerInterface reasoner) {
		this.reasoner = reasoner;
	}

	public CelReasonerInterface getReasoner() {
		return this.reasoner;
	}

	@Override
	public Boolean visit(OWLAnnotationAssertionAxiom axiom) {
		throw new UnsupportedQueryException(errorMsg + axiom);
	}

	@Override
	public Boolean visit(OWLAnnotationPropertyDomainAxiom axiom) {
		throw new UnsupportedQueryException(errorMsg + axiom);
	}

	@Override
	public Boolean visit(OWLAnnotationPropertyRangeAxiom axiom) {
		throw new UnsupportedQueryException(errorMsg + axiom);
	}

	@Override
	public Boolean visit(OWLAsymmetricObjectPropertyAxiom axiom) {
		throw new UnsupportedQueryException(errorMsg + axiom);
	}

	@Override
	public Boolean visit(OWLClassAssertionAxiom axiom) {
		throw new UnsupportedQueryException(errorMsg + axiom);
	}

	@Override
	public Boolean visit(OWLDataPropertyAssertionAxiom axiom) {
		throw new UnsupportedQueryException(errorMsg + axiom);
	}

	@Override
	public Boolean visit(OWLDataPropertyDomainAxiom axiom) {
		throw new UnsupportedQueryException(errorMsg + axiom);
	}

	@Override
	public Boolean visit(OWLDataPropertyRangeAxiom axiom) {
		throw new UnsupportedQueryException(errorMsg + axiom);
	}

	@Override
	public Boolean visit(OWLDatatypeDefinitionAxiom axiom) {
		throw new UnsupportedQueryException(errorMsg + axiom);
	}

	@Override
	public Boolean visit(OWLDeclarationAxiom axiom) {
		throw new UnsupportedQueryException(errorMsg + axiom);
	}

	@Override
	public Boolean visit(OWLDifferentIndividualsAxiom axiom) {
		throw new UnsupportedQueryException(errorMsg + axiom);
	}

	@Override
	public Boolean visit(OWLDisjointClassesAxiom axiom) {
		throw new UnsupportedQueryException(errorMsg + axiom);
	}

	@Override
	public Boolean visit(OWLDisjointDataPropertiesAxiom axiom) {
		throw new UnsupportedQueryException(errorMsg + axiom);
	}

	@Override
	public Boolean visit(OWLDisjointObjectPropertiesAxiom axiom) {
		throw new UnsupportedQueryException(errorMsg + axiom);
	}

	@Override
	public Boolean visit(OWLDisjointUnionAxiom axiom) {
		throw new UnsupportedQueryException(errorMsg + axiom);
	}

	@Override
	public Boolean visit(OWLEquivalentClassesAxiom axiom) {
		boolean ret = true;
		Set<OWLClassExpression> set = axiom.getClassExpressions();
		if (!set.isEmpty()) {
			OWLClass representative = getReasoner().flattenClassExpression(set.iterator().next());
			Set<OWLClass> classSet = new HashSet<>();
			set.forEach(classExpr -> {
				classSet.add(getReasoner().flattenClassExpression(classExpr));
			});
			getReasoner().precomputeInferences();
			for (Iterator<OWLClass> it = classSet.iterator(); ret && it.hasNext();) {
				OWLClass currentClass = it.next();
				ret = ret && getReasoner().isEquivalentClass(representative, currentClass);

			}
		}
		return ret;
	}

	@Override
	public Boolean visit(OWLEquivalentDataPropertiesAxiom axiom) {
		throw new UnsupportedQueryException(errorMsg + axiom);
	}

	@Override
	public Boolean visit(OWLEquivalentObjectPropertiesAxiom axiom) {
		throw new UnsupportedQueryException(errorMsg + axiom);
	}

	@Override
	public Boolean visit(OWLFunctionalDataPropertyAxiom axiom) {
		throw new UnsupportedQueryException(errorMsg + axiom);
	}

	@Override
	public Boolean visit(OWLFunctionalObjectPropertyAxiom axiom) {
		throw new UnsupportedQueryException(errorMsg + axiom);
	}

	@Override
	public Boolean visit(OWLHasKeyAxiom axiom) {
		throw new UnsupportedQueryException(errorMsg + axiom);
	}

	@Override
	public Boolean visit(OWLInverseFunctionalObjectPropertyAxiom axiom) {
		throw new UnsupportedQueryException(errorMsg + axiom);
	}

	@Override
	public Boolean visit(OWLInverseObjectPropertiesAxiom axiom) {
		throw new UnsupportedQueryException(errorMsg + axiom);
	}

	@Override
	public Boolean visit(OWLIrreflexiveObjectPropertyAxiom axiom) {
		throw new UnsupportedQueryException(errorMsg + axiom);
	}

	@Override
	public Boolean visit(OWLNegativeDataPropertyAssertionAxiom axiom) {
		throw new UnsupportedQueryException(errorMsg + axiom);
	}

	@Override
	public Boolean visit(OWLNegativeObjectPropertyAssertionAxiom axiom) {
		throw new UnsupportedQueryException(errorMsg + axiom);
	}

	@Override
	public Boolean visit(OWLObjectPropertyAssertionAxiom axiom) {
		throw new UnsupportedQueryException(errorMsg + axiom);
	}

	@Override
	public Boolean visit(OWLObjectPropertyDomainAxiom axiom) {
		throw new UnsupportedQueryException(errorMsg + axiom);
	}

	@Override
	public Boolean visit(OWLObjectPropertyRangeAxiom axiom) {
		throw new UnsupportedQueryException(errorMsg + axiom);
	}

	@Override
	public Boolean visit(OWLReflexiveObjectPropertyAxiom axiom) {
		throw new UnsupportedQueryException(errorMsg + axiom);
	}

	@Override
	public Boolean visit(OWLSameIndividualAxiom axiom) {
		throw new UnsupportedQueryException(errorMsg + axiom);
	}

	@Override
	public Boolean visit(OWLSubAnnotationPropertyOfAxiom axiom) {
		throw new UnsupportedQueryException(errorMsg + axiom);
	}

	@Override
	public Boolean visit(OWLSubClassOfAxiom axiom) {
		OWLClassExpression subClassExpr = axiom.getSubClass();
		OWLClassExpression superClassExpr = axiom.getSuperClass();
		OWLClass subClass = getReasoner().flattenClassExpression(subClassExpr);
		OWLClass superClass = getReasoner().flattenClassExpression(superClassExpr);
		getReasoner().precomputeInferences();
		boolean ret = getReasoner().isSubClassOf(subClass, superClass);
		return ret;
	}

	@Override
	public Boolean visit(OWLSubDataPropertyOfAxiom axiom) {
		throw new UnsupportedQueryException(errorMsg + axiom);
	}

	@Override
	public Boolean visit(OWLSubObjectPropertyOfAxiom axiom) {
		OWLObjectProperty subObjectProp = null;
		OWLObjectProperty superObjectProp = null;
		try {
			subObjectProp = axiom.getSubProperty().asOWLObjectProperty();
			superObjectProp = axiom.getSuperProperty().asOWLObjectProperty();
		} catch (Exception e) {
			throw new UnsupportedQueryException(errorMsg + axiom);
		}
		boolean ret = getReasoner().getSuperObjectProperties(subObjectProp, false).containsEntity(superObjectProp);
		return ret;
	}

	@Override
	public Boolean visit(OWLSubPropertyChainOfAxiom axiom) {
		throw new UnsupportedQueryException(errorMsg + axiom);
	}

	@Override
	public Boolean visit(OWLSymmetricObjectPropertyAxiom axiom) {
		throw new UnsupportedQueryException(errorMsg + axiom);
	}

	@Override
	public Boolean visit(OWLTransitiveObjectPropertyAxiom axiom) {
		throw new UnsupportedQueryException(errorMsg + axiom);
	}

	@Override
	public Boolean visit(SWRLRule rule) {
		throw new UnsupportedQueryException(errorMsg + rule);
	}

}
