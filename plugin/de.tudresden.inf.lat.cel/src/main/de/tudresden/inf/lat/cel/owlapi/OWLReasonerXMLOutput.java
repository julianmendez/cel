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

import java.io.IOException;
import java.io.OutputStream;
import java.util.Properties;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Result;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.semanticweb.owl.inference.OWLReasoner;
import org.semanticweb.owl.inference.OWLReasonerException;
import org.semanticweb.owl.model.OWLClass;
import org.w3c.dom.DOMException;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

/**
 * This class makes an XML representation of the inferred data in an
 * OWLReasoner.
 * 
 * @author Julian Mendez
 */
public class OWLReasonerXMLOutput {

	protected static final String INDENT_AMOUNT = "{http://xml.apache.org/xslt}indent-amount";
	protected static final String xmlClasses = "classes";
	protected static final String xmlDeclaration = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n";
	protected static final String xmlInferredData = "inferreddata";
	protected static final String xmlProperties = "properties";
	private XMLGraphOWLClass classGraph = null;
	private String docType = null;
	private XMLGraphOWLObjectProperty propertyGraph = null;

	public OWLReasonerXMLOutput(OWLReasoner reasoner, OWLClass nothing,
			OWLClass thing) {
		this.propertyGraph = new XMLGraphOWLObjectProperty(reasoner);
		this.classGraph = new XMLGraphOWLClass(reasoner, nothing, thing);
		this.docType = "<!DOCTYPE "
				+ xmlInferredData
				+ " [\n"
				+ classGraph.addElem(xmlInferredData, "(" + xmlProperties + ","
						+ xmlClasses + ")") + propertyGraph.getDTDFirstLine()
				+ classGraph.getDTDFirstLine() + classGraph.getDTDMainContent()
				+ "]>\n";
	}

	protected Transformer configureTransformer(Transformer transformer) {
		Properties props = new Properties();
		props.put(OutputKeys.METHOD, "xml");
		props.put(OutputKeys.VERSION, "1.0");
		props.put(OutputKeys.ENCODING, "UTF-8");
		props.put(OutputKeys.OMIT_XML_DECLARATION, "yes");
		props.put(OutputKeys.INDENT, "yes");
		props.put(INDENT_AMOUNT, "2");
		transformer.setOutputProperties(props);
		return transformer;
	}

	public String getDocType() {
		return this.docType;
	}

	public void toXML(OutputStream out) throws ParserConfigurationException,
			DOMException, OWLReasonerException, IOException,
			TransformerException {

		DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
		DocumentBuilder db = dbf.newDocumentBuilder();
		Document doc = db.newDocument();
		Element mainNode = doc.createElement(xmlInferredData);
		mainNode.appendChild(this.propertyGraph.toXML(doc));
		mainNode.appendChild(this.classGraph.toXML(doc));

		TransformerFactory tfac = TransformerFactory.newInstance();
		Transformer transformer = tfac.newTransformer();
		configureTransformer(transformer);
		doc.appendChild(mainNode);
		doc.normalizeDocument();
		Source xmlSource = new DOMSource(doc);
		out.write(xmlDeclaration.getBytes());
		out.write(getDocType().getBytes());
		Result outputTarget = new StreamResult(out);
		transformer.transform(xmlSource, outputTarget);
	}
}
