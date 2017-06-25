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

package de.tudresden.inf.lat.cel.conversion;

import java.util.List;
import java.util.Set;

import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLAnnotationAssertionAxiom;
import org.semanticweb.owlapi.model.OWLAxiom;
import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLClassAssertionAxiom;
import org.semanticweb.owlapi.model.OWLClassExpression;
import org.semanticweb.owlapi.model.OWLDeclarationAxiom;
import org.semanticweb.owlapi.model.OWLDifferentIndividualsAxiom;
import org.semanticweb.owlapi.model.OWLDisjointClassesAxiom;
import org.semanticweb.owlapi.model.OWLEquivalentClassesAxiom;
import org.semanticweb.owlapi.model.OWLEquivalentObjectPropertiesAxiom;
import org.semanticweb.owlapi.model.OWLNamedIndividual;
import org.semanticweb.owlapi.model.OWLObjectIntersectionOf;
import org.semanticweb.owlapi.model.OWLObjectProperty;
import org.semanticweb.owlapi.model.OWLObjectPropertyAssertionAxiom;
import org.semanticweb.owlapi.model.OWLObjectPropertyDomainAxiom;
import org.semanticweb.owlapi.model.OWLObjectPropertyExpression;
import org.semanticweb.owlapi.model.OWLObjectPropertyRangeAxiom;
import org.semanticweb.owlapi.model.OWLObjectSomeValuesFrom;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyManager;
import org.semanticweb.owlapi.model.OWLReflexiveObjectPropertyAxiom;
import org.semanticweb.owlapi.model.OWLSameIndividualAxiom;
import org.semanticweb.owlapi.model.OWLSubClassOfAxiom;
import org.semanticweb.owlapi.model.OWLSubObjectPropertyOfAxiom;
import org.semanticweb.owlapi.model.OWLSubPropertyChainOfAxiom;
import org.semanticweb.owlapi.model.OWLTransitiveObjectPropertyAxiom;

import de.tudresden.inf.lat.jsexp.Sexp;
import de.tudresden.inf.lat.jsexp.SexpFactory;

/**
 * This class provides methods to convert OWL objects into S-expressions.
 * 
 * @author Julian Mendez
 */
public class CelTranslator {

	public CelTranslator() {
	}

	protected Sexp createAtomicSymbol(IRI iri) {
		return createAtomicSymbol(iri.toString());
	}

	protected Sexp createAtomicSymbol(String symbol) {
		Sexp ret = SexpFactory.newAtomicSexp("" + LispKeyword.lispVBar + symbol + LispKeyword.lispVBar);
		return ret;
	}

	protected boolean isIgnoredAxiom(OWLAxiom axiom) {
		boolean ret = (axiom instanceof OWLDeclarationAxiom) || (axiom instanceof OWLAnnotationAssertionAxiom);
		return ret;
	}

	public Sexp translate(boolean boolval) {
		return boolval ? SexpFactory.newAtomicSexp(LispKeyword.lispTrue)
				: SexpFactory.newAtomicSexp(LispKeyword.lispFalse);
	}

	/**
	 * Axiom translation to S-expression.
	 * 
	 * @param axiom
	 *            an axiom that should not be ignored according to
	 *            isIgnoredAxiom.
	 * @return corresponding S-expression
	 * @throws CelTranslatorException
	 *             if the translation was not found.
	 */
	public Sexp translate(OWLAxiom axiom) throws CelTranslatorException {
		Sexp ret = null;
		if (axiom instanceof OWLClassAssertionAxiom) {
			ret = translateAxiom((OWLClassAssertionAxiom) axiom);
		} else if (axiom instanceof OWLDifferentIndividualsAxiom) {
			ret = translateAxiom((OWLDifferentIndividualsAxiom) axiom);
		} else if (axiom instanceof OWLDisjointClassesAxiom) {
			ret = translateAxiom((OWLDisjointClassesAxiom) axiom);
		} else if (axiom instanceof OWLEquivalentClassesAxiom) {
			ret = translateAxiom((OWLEquivalentClassesAxiom) axiom);
		} else if (axiom instanceof OWLEquivalentObjectPropertiesAxiom) {
			ret = translateAxiom((OWLEquivalentObjectPropertiesAxiom) axiom);
		} else if (axiom instanceof OWLObjectPropertyAssertionAxiom) {
			ret = translateAxiom((OWLObjectPropertyAssertionAxiom) axiom);
		} else if (axiom instanceof OWLSubPropertyChainOfAxiom) {
			ret = translateAxiom((OWLSubPropertyChainOfAxiom) axiom);
		} else if (axiom instanceof OWLObjectPropertyDomainAxiom) {
			ret = translateAxiom((OWLObjectPropertyDomainAxiom) axiom);
		} else if (axiom instanceof OWLObjectPropertyRangeAxiom) {
			ret = translateAxiom((OWLObjectPropertyRangeAxiom) axiom);
		} else if (axiom instanceof OWLSubObjectPropertyOfAxiom) {
			ret = translateAxiom((OWLSubObjectPropertyOfAxiom) axiom);
		} else if (axiom instanceof OWLReflexiveObjectPropertyAxiom) {
			ret = translateAxiom((OWLReflexiveObjectPropertyAxiom) axiom);
		} else if (axiom instanceof OWLSameIndividualAxiom) {
			ret = translateAxiom((OWLSameIndividualAxiom) axiom);
		} else if (axiom instanceof OWLSubClassOfAxiom) {
			ret = translateAxiom((OWLSubClassOfAxiom) axiom);
		} else if (axiom instanceof OWLTransitiveObjectPropertyAxiom) {
			ret = translateAxiom((OWLTransitiveObjectPropertyAxiom) axiom);
		}
		if (ret == null) {
			throw new CelTranslatorException("OWLAxiom '" + axiom.toString() + "' is not supported.");
		}
		return ret;
	}

	public Sexp translate(OWLClassExpression description) throws CelTranslatorException {
		Sexp ret = null;
		if (description.isOWLThing()) {
			ret = SexpFactory.newAtomicSexp(CelKeyword.keyTop);
		} else if (description.isOWLNothing()) {
			ret = SexpFactory.newAtomicSexp(CelKeyword.keyBottom);
		} else if (description instanceof OWLObjectIntersectionOf) {
			ret = translateDescription((OWLObjectIntersectionOf) description);
		} else if (description instanceof OWLObjectSomeValuesFrom) {
			ret = translateDescription((OWLObjectSomeValuesFrom) description);
		} else if (description instanceof OWLClass) {
			ret = translateDescription((OWLClass) description);
		}
		if (ret == null) {
			throw new CelTranslatorException("OWLClassExpression '" + description.toString() + "' is not supported.");
		}
		return ret;
	}

	public Sexp translate(OWLNamedIndividual individual) {
		Sexp ret = createAtomicSymbol(individual.getIRI());
		return ret;
	}

	public Sexp translate(OWLObjectPropertyExpression property) {
		Sexp ret = translateProperty(property.asOWLObjectProperty());
		return ret;
	}

	public Sexp translate(OWLOntology ontology, OWLOntologyManager ontologyManager) throws CelTranslatorException {
		Sexp ret = SexpFactory.newNonAtomicSexp();
		Set<OWLAxiom> axioms = ontology.getAxioms();
		axioms.forEach(axiom -> {
			if (!isIgnoredAxiom(axiom)) {
				ret.add(translate(axiom));
			}
		});
		return ret;
	}

	protected Sexp translateAxiom(OWLClassAssertionAxiom axiom) throws CelTranslatorException {
		Sexp ret = SexpFactory.newNonAtomicSexp();
		ret.add(SexpFactory.newAtomicSexp(CelKeyword.keyInstance));
		ret.add(translate(axiom.getIndividual().asOWLNamedIndividual()));
		ret.add(translate(axiom.getClassExpression()));
		return ret;
	}

	protected Sexp translateAxiom(OWLDifferentIndividualsAxiom axiom) throws CelTranslatorException {
		Sexp ret = SexpFactory.newNonAtomicSexp();
		ret.add(SexpFactory.newAtomicSexp(CelKeyword.keyDifferentIndividuals));
		axiom.getIndividuals().forEach(elem -> {
			ret.add(translate(elem.asOWLNamedIndividual()));
		});
		return ret;
	}

	protected Sexp translateAxiom(OWLDisjointClassesAxiom axiom) throws CelTranslatorException {
		Sexp ret = SexpFactory.newNonAtomicSexp();
		ret.add(SexpFactory.newAtomicSexp(CelKeyword.keyDisjoint));
		axiom.getClassExpressions().forEach(elem -> {
			ret.add(translate(elem));
		});
		return ret;
	}

	protected Sexp translateAxiom(OWLEquivalentClassesAxiom axiom) throws CelTranslatorException {
		Sexp ret = SexpFactory.newNonAtomicSexp();
		ret.add(SexpFactory.newAtomicSexp(CelKeyword.keyEquivalent));
		axiom.getClassExpressions().forEach(desc -> {
			Sexp expr = translate(desc);
			ret.add(expr);
		});
		return ret;
	}

	protected Sexp translateAxiom(OWLEquivalentObjectPropertiesAxiom axiom) throws CelTranslatorException {
		Sexp ret = SexpFactory.newNonAtomicSexp();
		ret.add(SexpFactory.newAtomicSexp(CelKeyword.keyRoleEquivalent));
		axiom.getProperties().forEach(desc -> {
			Sexp expr = translate(desc);
			ret.add(expr);
		});
		return ret;
	}

	protected Sexp translateAxiom(OWLObjectPropertyAssertionAxiom axiom) throws CelTranslatorException {
		Sexp ret = SexpFactory.newNonAtomicSexp();
		ret.add(SexpFactory.newAtomicSexp(CelKeyword.keyRelated));
		ret.add(translate(axiom.getSubject().asOWLNamedIndividual()));
		ret.add(translate(axiom.getObject().asOWLNamedIndividual()));
		ret.add(translate(axiom.getProperty()));
		return ret;
	}

	protected Sexp translateAxiom(OWLObjectPropertyDomainAxiom axiom) throws CelTranslatorException {
		Sexp ret = SexpFactory.newNonAtomicSexp();
		ret.add(SexpFactory.newAtomicSexp(CelKeyword.keyDomain));
		ret.add(translate(axiom.getProperty()));
		ret.add(translate(axiom.getDomain()));
		return ret;
	}

	protected Sexp translateAxiom(OWLObjectPropertyRangeAxiom axiom) throws CelTranslatorException {
		Sexp ret = SexpFactory.newNonAtomicSexp();
		ret.add(SexpFactory.newAtomicSexp(CelKeyword.keyRange));
		ret.add(translate(axiom.getProperty()));
		ret.add(translate(axiom.getRange()));
		return ret;
	}

	protected Sexp translateAxiom(OWLReflexiveObjectPropertyAxiom axiom) throws CelTranslatorException {
		Sexp ret = SexpFactory.newNonAtomicSexp();
		ret.add(SexpFactory.newAtomicSexp(CelKeyword.keyReflexive));
		ret.add(translate(axiom.getProperty()));
		return ret;
	}

	protected Sexp translateAxiom(OWLSameIndividualAxiom axiom) throws CelTranslatorException {
		Sexp ret = SexpFactory.newNonAtomicSexp();
		ret.add(SexpFactory.newAtomicSexp(CelKeyword.keySameIndividuals));
		axiom.getIndividuals().forEach(elem -> {
			ret.add(translate(elem.asOWLNamedIndividual()));
		});
		return ret;
	}

	protected Sexp translateAxiom(OWLSubClassOfAxiom axiom) throws CelTranslatorException {
		Sexp ret = SexpFactory.newNonAtomicSexp();
		ret.add(SexpFactory.newAtomicSexp(CelKeyword.keyImplies));
		ret.add(translate(axiom.getSubClass()));
		ret.add(translate(axiom.getSuperClass()));
		return ret;
	}

	protected Sexp translateAxiom(OWLSubObjectPropertyOfAxiom axiom) throws CelTranslatorException {
		Sexp ret = SexpFactory.newNonAtomicSexp();
		ret.add(SexpFactory.newAtomicSexp(CelKeyword.keyRoleInclusion));
		ret.add(translate(axiom.getSubProperty()));
		ret.add(translate(axiom.getSuperProperty()));
		return ret;
	}

	protected Sexp translateAxiom(OWLSubPropertyChainOfAxiom axiom) throws CelTranslatorException {
		List<OWLObjectPropertyExpression> propertyList = axiom.getPropertyChain();
		Sexp translatedList = SexpFactory.newNonAtomicSexp();
		translatedList.add(SexpFactory.newAtomicSexp(CelKeyword.keyCompose));
		propertyList.forEach(expr -> {
			translatedList.add(translate(expr));
		});
		Sexp ret = SexpFactory.newNonAtomicSexp();
		ret.add(SexpFactory.newAtomicSexp(CelKeyword.keyRoleInclusion));
		ret.add(translatedList);
		ret.add(translate(axiom.getSuperProperty()));
		return ret;
	}

	protected Sexp translateAxiom(OWLTransitiveObjectPropertyAxiom axiom) throws CelTranslatorException {
		Sexp ret = SexpFactory.newNonAtomicSexp();
		ret.add(SexpFactory.newAtomicSexp(CelKeyword.keyTransitive));
		ret.add(translate(axiom.getProperty()));
		return ret;
	}

	protected Sexp translateDescription(OWLClass cl) {
		Sexp ret = createAtomicSymbol(cl.getIRI());
		return ret;
	}

	protected Sexp translateDescription(OWLObjectIntersectionOf description) throws CelTranslatorException {
		Sexp ret = SexpFactory.newNonAtomicSexp();
		ret.add(SexpFactory.newAtomicSexp(CelKeyword.keyAnd));
		description.getOperands().forEach(elem -> {
			ret.add(translate(elem));
		});
		return ret;
	}

	protected Sexp translateDescription(OWLObjectSomeValuesFrom description) throws CelTranslatorException {
		Sexp ret = SexpFactory.newNonAtomicSexp();
		ret.add(SexpFactory.newAtomicSexp(CelKeyword.keySome));
		ret.add(translate(description.getProperty()));
		ret.add(translate(description.getFiller()));
		return ret;
	}

	protected Sexp translateProperty(OWLObjectProperty property) {
		Sexp ret = createAtomicSymbol(property.getIRI());
		return ret;
	}

}
