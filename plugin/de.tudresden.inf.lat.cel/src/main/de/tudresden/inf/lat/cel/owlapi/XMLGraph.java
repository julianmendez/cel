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

import java.util.Set;
import java.util.TreeSet;

import org.semanticweb.owl.inference.OWLReasonerException;
import org.w3c.dom.DOMException;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

/**
 * An XMLGraph is an abstract hierarchical graph that has an normalized XML
 * representation. Different instances can be easily compared using this XML
 * representation.
 * 
 * @author Julian Mendez
 * 
 * @param <T>
 */
public abstract class XMLGraph<T> {

	protected static final String xmlAttSize = "size";
	protected static final String xmlAttValue = "value";
	protected static final String xmlTagElemSet = "elementset";
	protected static final String xmlTagEquiv = "equivalentset";
	protected static final String xmlTagEquivSet = "equivalent";
	protected static final String xmlTagItem = "item";
	protected static final String xmlTagParent = "parent";
	protected static final String xmlTagParentSet = "parentset";

	protected String addElem(String elementName, String parameter) {
		return "  <!ELEMENT " + elementName + " " + parameter + ">\n";
	}

	protected String addReqAtt(String elementName, String attribute) {
		return "  <!ATTLIST " + elementName + " " + attribute
				+ " CDATA #REQUIRED>\n";
	}

	protected Set<T> flatten(Set<Set<T>> setOfSets) {
		Set<T> ret = new TreeSet<T>();
		for (Set<T> elemSet : setOfSets) {
			ret.addAll(elemSet);
		}
		return ret;
	}

	public String getDTDFirstLine() {
		return addElem(getMainTag(), "(" + xmlTagElemSet + "," + xmlTagEquiv
				+ "," + xmlTagParentSet + ")");
	}

	public String getDTDMainContent() {
		return addElem(xmlTagElemSet, "(" + xmlTagItem + "*)")
				+ addReqAtt(xmlTagElemSet, xmlAttSize)
				+ addElem(xmlTagEquiv, "(" + xmlTagEquivSet + "*)")
				+ addReqAtt(xmlTagEquiv, xmlAttSize)
				+ addElem(xmlTagParentSet, "(" + xmlTagParent + "*)")
				+ addReqAtt(xmlTagParentSet, xmlAttSize)
				+ addElem(xmlTagEquivSet, "(" + xmlTagItem + "*)")
				+ addReqAtt(xmlTagEquivSet, xmlAttSize)
				+ addReqAtt(xmlTagEquivSet, xmlAttValue)
				+ addElem(xmlTagParent, "(" + xmlTagItem + "*)")
				+ addReqAtt(xmlTagParent, xmlAttSize)
				+ addReqAtt(xmlTagParent, xmlAttValue)
				+ addElem(xmlTagItem, "EMPTY")
				+ addReqAtt(xmlTagItem, xmlAttValue);
	}

	public abstract Set<T> getElements();

	protected abstract Set<T> getEquivalentElements(T elem)
			throws OWLReasonerException;

	protected Set<T> getFlattenSubElements(T elem) throws OWLReasonerException {
		return flatten(getSubElements(elem));
	}

	public abstract String getMainTag();

	protected abstract Set<Set<T>> getSubElements(T elem)
			throws OWLReasonerException;

	protected Element listElements(Document document) {
		return listSet(document, getElements(), xmlTagElemSet, null);
	}

	protected Element listParents(Document document)
			throws OWLReasonerException {
		Element ret = document.createElement(xmlTagParentSet);
		Set<T> elements = getElements();
		ret.setAttribute(xmlAttSize, "" + elements.size());
		for (T elem : elements) {
			Element newNode = listSet(document, getFlattenSubElements(elem),
					xmlTagParent, elem.toString());
			ret.appendChild(newNode);
		}
		return ret;
	}

	protected Element listSet(Document document, Set<T> set, String tag,
			String value) {
		Element ret = document.createElement(tag);

		ret.setAttribute(xmlAttSize, "" + set.size());
		if (value != null) {
			ret.setAttribute(xmlAttValue, "" + value);
		}
		for (T elem : set) {
			Element newNode = document.createElement(xmlTagItem);
			newNode.setAttribute(xmlAttValue, elem.toString());
			ret.appendChild(newNode);
		}
		return ret;
	}

	protected Element listSiblings(Document document)
			throws OWLReasonerException {
		Element ret = document.createElement(xmlTagEquiv);
		Set<T> elements = getElements();
		ret.setAttribute(xmlAttSize, "" + elements.size());
		for (T elem : elements) {
			Element newNode = listSet(document, getEquivalentElements(elem),
					xmlTagEquivSet, elem.toString());
			ret.appendChild(newNode);
		}
		return ret;
	}

	public Element toXML(Document doc) throws DOMException,
			OWLReasonerException {
		Element ret = doc.createElement(getMainTag());
		ret.appendChild(listElements(doc));
		ret.appendChild(listSiblings(doc));
		ret.appendChild(listParents(doc));
		return ret;
	}
}
