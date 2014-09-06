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

import java.io.BufferedInputStream;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLClassExpression;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLNamedIndividual;
import org.semanticweb.owlapi.model.OWLObjectProperty;
import org.semanticweb.owlapi.model.OWLObjectPropertyExpression;

import de.tudresden.inf.lat.jsexp.Sexp;
import de.tudresden.inf.lat.jsexp.SexpFactory;
import de.tudresden.inf.lat.jsexp.SexpParserException;

/**
 * This class has the methods to parse CEL S-expressions and return OWL objects.
 *
 * @author Julian Mendez
 */
public class CelParser {

	public CelParser() {
	}

	public OWLClassExpression parse(String str, OWLDataFactory dataFactory)
			throws CelParserException {
		OWLClassExpression ret = null;
		BufferedInputStream in = new BufferedInputStream(
				new ByteArrayInputStream(str.getBytes()));
		try {
			Sexp expr = SexpFactory.parse(in);
			ret = parseDescription(expr, dataFactory);
		} catch (SexpParserException e) {
			throw new CelParserException(e);
		} catch (IOException e) {
			throw new CelParserException(e);
		}
		return ret;
	}

	/**
	 * Note: This function returns false for every S-expression which is not
	 * shown as 't' or 'T'.
	 *
	 * @param expr
	 *            S-expression
	 * @return true if the S-expression is 't' or 'T'
	 */
	public boolean parseBoolean(Sexp expr) {
		boolean ret = expr.toString().equalsIgnoreCase(LispKeyword.lispTrue);
		return ret;
	}

	protected OWLClass parseClass(Sexp expr, OWLDataFactory dataFactory) {
		OWLClass ret = null;
		Sexp cleanExpr = removeVbars(expr);
		ret = tryToParseBottom(cleanExpr, dataFactory);
		if (ret == null) {
			ret = tryToParseTop(cleanExpr, dataFactory);
		}
		if (ret == null) {
			ret = dataFactory.getOWLClass(IRI.create(cleanExpr.toString()));
		}
		return ret;
	}

	protected OWLClassExpression parseDescription(Sexp expr,
			OWLDataFactory dataFactory) throws CelParserException {
		OWLClassExpression ret = null;
		if (expr.isAtomic()) {
			ret = parseClass(expr, dataFactory);
		} else {
			ret = tryToParseSome(expr, dataFactory);
			if (ret == null) {
				ret = tryToParseAnd(expr, dataFactory);
			}
			if (ret == null) {
				throw new CelParserException("'" + expr.toString()
						+ "' was not parsed.");
			}
		}
		return ret;
	}

	public Set<OWLClass> parseSetOfClasses(Sexp expr, OWLDataFactory dataFactory) {
		Set<OWLClass> ret = new HashSet<OWLClass>();
		for (Sexp elem : expr) {
			ret.add(parseClass(elem, dataFactory));
		}
		return ret;
	}

	public Set<OWLClassExpression> parseSetOfDescriptions(Sexp expr,
			OWLDataFactory dataFactory) throws CelParserException {
		Set<OWLClassExpression> ret = new HashSet<OWLClassExpression>();
		for (Sexp elem : expr) {
			ret.add(parseDescription(elem, dataFactory));
		}
		return ret;
	}

	public Set<OWLNamedIndividual> parseSetOfIndividuals(Sexp expr,
			OWLDataFactory dataFactory) {
		Set<OWLNamedIndividual> ret = new HashSet<OWLNamedIndividual>();
		for (Sexp elem : expr) {
			Sexp cleanexpr = removeVbars(elem);
			ret.add(dataFactory.getOWLNamedIndividual(IRI.create(cleanexpr
					.toString())));
		}
		return ret;
	}

	public Set<OWLObjectProperty> parseSetOfProperties(Sexp expr,
			OWLDataFactory dataFactory) {
		Set<OWLObjectProperty> ret = new HashSet<OWLObjectProperty>();
		for (Sexp elem : expr) {
			Sexp cleanexpr = removeVbars(elem);
			ret.add(dataFactory.getOWLObjectProperty(IRI.create(cleanexpr
					.toString())));
		}
		return ret;
	}

	public Set<Set<OWLClass>> parseSetOfSetOfClasses(Sexp expr,
			OWLDataFactory dataFactory) {
		Set<Set<OWLClass>> ret = new HashSet<Set<OWLClass>>();
		for (Sexp subexpr : expr) {
			Set<OWLClass> part = new HashSet<OWLClass>();
			for (Sexp elem : subexpr) {
				part.add(parseClass(elem, dataFactory));
			}
			ret.add(part);
		}
		return ret;
	}

	public Set<Set<OWLClassExpression>> parseSetOfSetOfDescriptions(Sexp expr,
			OWLDataFactory dataFactory) throws CelParserException {
		Set<Set<OWLClassExpression>> ret = new HashSet<Set<OWLClassExpression>>();
		for (Sexp subexpr : expr) {
			Set<OWLClassExpression> part = new HashSet<OWLClassExpression>();
			for (Sexp elem : subexpr) {
				part.add(parseDescription(elem, dataFactory));
			}
			ret.add(part);
		}
		return ret;
	}

	public Set<Set<OWLObjectProperty>> parseSetOfSetOfProperties(Sexp expr,
			OWLDataFactory dataFactory) {
		Set<Set<OWLObjectProperty>> ret = new HashSet<Set<OWLObjectProperty>>();
		for (Sexp subexpr : expr) {
			Set<OWLObjectProperty> part = new HashSet<OWLObjectProperty>();
			for (Sexp elem : subexpr) {
				Sexp cleanexpr = removeVbars(elem);
				part.add(dataFactory.getOWLObjectProperty(IRI.create(cleanexpr
						.toString())));
			}
			ret.add(part);
		}
		return ret;
	}

	protected Sexp removeVbars(Sexp symbolWithVbars) {
		Sexp ret = symbolWithVbars;
		if (symbolWithVbars.isAtomic()) {
			String text = symbolWithVbars.toString();
			if ((text.length() >= 2)
					&& text.startsWith("" + LispKeyword.lispVBar)
					&& text.endsWith("" + LispKeyword.lispVBar)) {
				ret = SexpFactory.newAtomicSexp(text.substring(1,
						text.length() - 1));
			}
		}
		return ret;
	}

	protected OWLClassExpression tryToParseAnd(Sexp expr,
			OWLDataFactory dataFactory) throws CelParserException {
		OWLClassExpression ret = null;
		if (!expr.isAtomic()) {
			Iterator<Sexp> it = expr.iterator();
			Sexp current = it.next();
			if (current.isAtomic()
					&& current.toString().equalsIgnoreCase(CelKeyword.keyAnd)) {
				Set<OWLClassExpression> set = new HashSet<OWLClassExpression>();
				while (it.hasNext()) {
					current = it.next();
					set.add(parseDescription(current, dataFactory));
				}
				ret = dataFactory.getOWLObjectIntersectionOf(set);
			}
		}
		return ret;
	}

	protected OWLClass tryToParseBottom(Sexp expr, OWLDataFactory dataFactory) {
		OWLClass ret = null;
		if (expr.isAtomic()
				&& expr.toString().equalsIgnoreCase(CelKeyword.keyBottom)) {
			ret = dataFactory.getOWLNothing();
		}
		return ret;
	}

	protected OWLClassExpression tryToParseSome(Sexp expr,
			OWLDataFactory dataFactory) throws CelParserException {
		OWLClassExpression ret = null;
		if (!expr.isAtomic()) {
			Iterator<Sexp> it = expr.iterator();
			Sexp current = it.next();
			if (current.isAtomic()
					&& current.toString().equalsIgnoreCase(CelKeyword.keySome)) {
				Sexp role = it.next();
				if (!role.isAtomic()) {
					throw new CelParserException("'" + role.toString() + "'"
							+ "was found but only simple roles are accepted.");
				}
				Sexp cleanexpr = removeVbars(role);
				OWLObjectPropertyExpression re = dataFactory
						.getOWLObjectProperty(IRI.create(cleanexpr.toString()));
				Sexp concept = it.next();
				if (it.hasNext()) {
					throw new CelParserException("'" + expr
							+ "' 'some' has too many parameters.");
				}
				OWLClassExpression ce = parseDescription(concept, dataFactory);
				ret = dataFactory.getOWLObjectSomeValuesFrom(re, ce);
			}
		}
		return ret;
	}

	protected OWLClass tryToParseTop(Sexp expr, OWLDataFactory dataFactory) {
		OWLClass ret = null;
		if (expr.isAtomic()
				&& expr.toString().equalsIgnoreCase(CelKeyword.keyTop)) {
			ret = dataFactory.getOWLThing();
		}
		return ret;
	}
}
