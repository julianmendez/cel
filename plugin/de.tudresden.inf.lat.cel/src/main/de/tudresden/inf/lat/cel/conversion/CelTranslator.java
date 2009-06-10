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

import java.net.URI;
import java.util.List;
import java.util.Set;

import org.semanticweb.owl.model.OWLAxiom;
import org.semanticweb.owl.model.OWLAxiomAnnotationAxiom;
import org.semanticweb.owl.model.OWLClass;
import org.semanticweb.owl.model.OWLClassAssertionAxiom;
import org.semanticweb.owl.model.OWLDeclarationAxiom;
import org.semanticweb.owl.model.OWLDeprecatedClassAxiom;
import org.semanticweb.owl.model.OWLDeprecatedObjectPropertyAxiom;
import org.semanticweb.owl.model.OWLDescription;
import org.semanticweb.owl.model.OWLDifferentIndividualsAxiom;
import org.semanticweb.owl.model.OWLDisjointClassesAxiom;
import org.semanticweb.owl.model.OWLEntityAnnotationAxiom;
import org.semanticweb.owl.model.OWLEquivalentClassesAxiom;
import org.semanticweb.owl.model.OWLEquivalentObjectPropertiesAxiom;
import org.semanticweb.owl.model.OWLImportsDeclaration;
import org.semanticweb.owl.model.OWLIndividual;
import org.semanticweb.owl.model.OWLObjectIntersectionOf;
import org.semanticweb.owl.model.OWLObjectProperty;
import org.semanticweb.owl.model.OWLObjectPropertyAssertionAxiom;
import org.semanticweb.owl.model.OWLObjectPropertyChainSubPropertyAxiom;
import org.semanticweb.owl.model.OWLObjectPropertyDomainAxiom;
import org.semanticweb.owl.model.OWLObjectPropertyExpression;
import org.semanticweb.owl.model.OWLObjectPropertyRangeAxiom;
import org.semanticweb.owl.model.OWLObjectSomeRestriction;
import org.semanticweb.owl.model.OWLObjectSubPropertyAxiom;
import org.semanticweb.owl.model.OWLOntology;
import org.semanticweb.owl.model.OWLOntologyAnnotationAxiom;
import org.semanticweb.owl.model.OWLReflexiveObjectPropertyAxiom;
import org.semanticweb.owl.model.OWLSameIndividualsAxiom;
import org.semanticweb.owl.model.OWLSubClassAxiom;
import org.semanticweb.owl.model.OWLTransitiveObjectPropertyAxiom;

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

	protected Sexp createAtomicSymbol(String symbol) {
		Sexp ret = SexpFactory.newAtomicSexp("" + LispKeyword.lispVBar + symbol
				+ LispKeyword.lispVBar);
		return ret;
	}

	protected Sexp createAtomicSymbol(URI uri) {
		return createAtomicSymbol(uri.toString());
	}

	protected boolean isIgnoredAxiom(OWLAxiom axiom) {
		boolean ret = (axiom instanceof OWLAxiomAnnotationAxiom)
				|| (axiom instanceof OWLDeclarationAxiom)
				|| (axiom instanceof OWLDeprecatedClassAxiom)
				|| (axiom instanceof OWLDeprecatedObjectPropertyAxiom)
				|| (axiom instanceof OWLEntityAnnotationAxiom)
				|| (axiom instanceof OWLImportsDeclaration)
				|| (axiom instanceof OWLOntologyAnnotationAxiom);
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
		} else if (axiom instanceof OWLObjectPropertyChainSubPropertyAxiom) {
			ret = translateAxiom((OWLObjectPropertyChainSubPropertyAxiom) axiom);
		} else if (axiom instanceof OWLObjectPropertyDomainAxiom) {
			ret = translateAxiom((OWLObjectPropertyDomainAxiom) axiom);
		} else if (axiom instanceof OWLObjectPropertyRangeAxiom) {
			ret = translateAxiom((OWLObjectPropertyRangeAxiom) axiom);
		} else if (axiom instanceof OWLObjectSubPropertyAxiom) {
			ret = translateAxiom((OWLObjectSubPropertyAxiom) axiom);
		} else if (axiom instanceof OWLReflexiveObjectPropertyAxiom) {
			ret = translateAxiom((OWLReflexiveObjectPropertyAxiom) axiom);
		} else if (axiom instanceof OWLSameIndividualsAxiom) {
			ret = translateAxiom((OWLSameIndividualsAxiom) axiom);
		} else if (axiom instanceof OWLSubClassAxiom) {
			ret = translateAxiom((OWLSubClassAxiom) axiom);
		} else if (axiom instanceof OWLTransitiveObjectPropertyAxiom) {
			ret = translateAxiom((OWLTransitiveObjectPropertyAxiom) axiom);
		}
		if (ret == null) {
			throw new CelTranslatorException("OWLAxiom '" + axiom.toString()
					+ "' was not recognized.");
		}
		return ret;
	}

	public Sexp translate(OWLDescription description)
			throws CelTranslatorException {
		Sexp ret = null;
		if (description.isOWLThing()) {
			ret = SexpFactory.newAtomicSexp(CelKeyword.keyTop);
		} else if (description.isOWLNothing()) {
			ret = SexpFactory.newAtomicSexp(CelKeyword.keyBottom);
		} else if (description instanceof OWLObjectIntersectionOf) {
			ret = translateDescription((OWLObjectIntersectionOf) description);
		} else if (description instanceof OWLObjectSomeRestriction) {
			ret = translateDescription((OWLObjectSomeRestriction) description);
		} else if (description instanceof OWLClass) {
			ret = translateDescription((OWLClass) description);
		}
		if (ret == null) {
			throw new CelTranslatorException("OWLDescription '"
					+ description.toString() + "' was not recognized.");
		}
		return ret;
	}

	public Sexp translate(OWLIndividual individual) {
		Sexp ret = createAtomicSymbol(individual.getURI());
		return ret;
	}

	public Sexp translate(OWLObjectPropertyExpression property) {
		Sexp ret = translateProperty(property.asOWLObjectProperty());
		return ret;
	}

	public Sexp translate(OWLOntology ontology) throws CelTranslatorException {
		Sexp ret = SexpFactory.newNonAtomicSexp();
		Set<OWLAxiom> axioms = ontology.getAxioms();
		for (OWLAxiom axiom : axioms) {
			if (!isIgnoredAxiom(axiom)) {
				ret.add(translate(axiom));
			}
		}
		return ret;
	}

	protected Sexp translateAxiom(OWLClassAssertionAxiom axiom)
			throws CelTranslatorException {
		Sexp ret = SexpFactory.newNonAtomicSexp();
		ret.add(SexpFactory.newAtomicSexp(CelKeyword.keyInstance));
		ret.add(translate(axiom.getIndividual()));
		ret.add(translate(axiom.getDescription()));
		return ret;
	}

	protected Sexp translateAxiom(OWLDifferentIndividualsAxiom axiom)
			throws CelTranslatorException {
		Sexp ret = SexpFactory.newNonAtomicSexp();
		ret.add(SexpFactory.newAtomicSexp(CelKeyword.keyDifferentIndividuals));
		for (OWLIndividual elem : axiom.getIndividuals()) {
			ret.add(translate(elem));
		}
		return ret;
	}

	protected Sexp translateAxiom(OWLDisjointClassesAxiom axiom)
			throws CelTranslatorException {
		Sexp ret = SexpFactory.newNonAtomicSexp();
		ret.add(SexpFactory.newAtomicSexp(CelKeyword.keyDisjoint));
		for (OWLDescription elem : axiom.getDescriptions()) {
			ret.add(translate(elem));
		}
		return ret;
	}

	protected Sexp translateAxiom(OWLEquivalentClassesAxiom axiom)
			throws CelTranslatorException {
		Sexp ret = SexpFactory.newNonAtomicSexp();
		ret.add(SexpFactory.newAtomicSexp(CelKeyword.keyEquivalent));
		for (OWLDescription desc : axiom.getDescriptions()) {
			Sexp expr = translate(desc);
			ret.add(expr);
		}
		return ret;
	}

	protected Sexp translateAxiom(OWLEquivalentObjectPropertiesAxiom axiom)
			throws CelTranslatorException {
		Sexp ret = SexpFactory.newNonAtomicSexp();
		ret.add(SexpFactory.newAtomicSexp(CelKeyword.keyRoleEquivalent));
		for (OWLObjectPropertyExpression desc : axiom.getProperties()) {
			Sexp expr = translate(desc);
			ret.add(expr);
		}
		return ret;
	}

	protected Sexp translateAxiom(OWLObjectPropertyAssertionAxiom axiom)
			throws CelTranslatorException {
		Sexp ret = SexpFactory.newNonAtomicSexp();
		ret.add(SexpFactory.newAtomicSexp(CelKeyword.keyRelated));
		ret.add(translate(axiom.getSubject()));
		ret.add(translate(axiom.getObject()));
		ret.add(translate(axiom.getProperty()));
		return ret;
	}

	protected Sexp translateAxiom(OWLObjectPropertyChainSubPropertyAxiom axiom)
			throws CelTranslatorException {
		List<OWLObjectPropertyExpression> propertyList = axiom
				.getPropertyChain();
		Sexp translatedList = SexpFactory.newNonAtomicSexp();
		translatedList.add(SexpFactory.newAtomicSexp(CelKeyword.keyCompose));
		for (OWLObjectPropertyExpression expr : propertyList) {
			translatedList.add(translate(expr));
		}
		Sexp ret = SexpFactory.newNonAtomicSexp();
		ret.add(SexpFactory.newAtomicSexp(CelKeyword.keyRoleInclusion));
		ret.add(translatedList);
		ret.add(translate(axiom.getSuperProperty()));
		return ret;
	}

	protected Sexp translateAxiom(OWLObjectPropertyDomainAxiom axiom)
			throws CelTranslatorException {
		Sexp ret = SexpFactory.newNonAtomicSexp();
		ret.add(SexpFactory.newAtomicSexp(CelKeyword.keyDomain));
		ret.add(translate(axiom.getProperty()));
		ret.add(translate(axiom.getDomain()));
		return ret;
	}

	protected Sexp translateAxiom(OWLObjectPropertyRangeAxiom axiom)
			throws CelTranslatorException {
		Sexp ret = SexpFactory.newNonAtomicSexp();
		ret.add(SexpFactory.newAtomicSexp(CelKeyword.keyRange));
		ret.add(translate(axiom.getProperty()));
		ret.add(translate(axiom.getRange()));
		return ret;
	}

	protected Sexp translateAxiom(OWLObjectSubPropertyAxiom axiom)
			throws CelTranslatorException {
		Sexp ret = SexpFactory.newNonAtomicSexp();
		ret.add(SexpFactory.newAtomicSexp(CelKeyword.keyRoleInclusion));
		ret.add(translate(axiom.getSubProperty()));
		ret.add(translate(axiom.getSuperProperty()));
		return ret;
	}

	protected Sexp translateAxiom(OWLReflexiveObjectPropertyAxiom axiom)
			throws CelTranslatorException {
		Sexp ret = SexpFactory.newNonAtomicSexp();
		ret.add(SexpFactory.newAtomicSexp(CelKeyword.keyReflexive));
		ret.add(translate(axiom.getProperty()));
		return ret;
	}

	protected Sexp translateAxiom(OWLSameIndividualsAxiom axiom)
			throws CelTranslatorException {
		Sexp ret = SexpFactory.newNonAtomicSexp();
		ret.add(SexpFactory.newAtomicSexp(CelKeyword.keySameIndividuals));
		for (OWLIndividual elem : axiom.getIndividuals()) {
			ret.add(translate(elem));
		}
		return ret;
	}

	protected Sexp translateAxiom(OWLSubClassAxiom axiom)
			throws CelTranslatorException {
		Sexp ret = SexpFactory.newNonAtomicSexp();
		ret.add(SexpFactory.newAtomicSexp(CelKeyword.keyImplies));
		ret.add(translate(axiom.getSubClass()));
		ret.add(translate(axiom.getSuperClass()));
		return ret;
	}

	protected Sexp translateAxiom(OWLTransitiveObjectPropertyAxiom axiom)
			throws CelTranslatorException {
		Sexp ret = SexpFactory.newNonAtomicSexp();
		ret.add(SexpFactory.newAtomicSexp(CelKeyword.keyTransitive));
		ret.add(translate(axiom.getProperty()));
		return ret;
	}

	protected Sexp translateDescription(OWLClass cl) {
		Sexp ret = createAtomicSymbol(cl.getURI());
		return ret;
	}

	protected Sexp translateDescription(OWLObjectIntersectionOf description)
			throws CelTranslatorException {
		Sexp ret = SexpFactory.newNonAtomicSexp();
		ret.add(SexpFactory.newAtomicSexp(CelKeyword.keyAnd));
		for (OWLDescription elem : description.getOperands()) {
			ret.add(translate(elem));
		}
		return ret;
	}

	protected Sexp translateDescription(OWLObjectSomeRestriction description)
			throws CelTranslatorException {
		Sexp ret = SexpFactory.newNonAtomicSexp();
		ret.add(SexpFactory.newAtomicSexp(CelKeyword.keySome));
		ret.add(translate(description.getProperty()));
		ret.add(translate(description.getFiller()));
		return ret;
	}

	protected Sexp translateProperty(OWLObjectProperty property) {
		Sexp ret = createAtomicSymbol(property.getURI());
		return ret;
	}
}
