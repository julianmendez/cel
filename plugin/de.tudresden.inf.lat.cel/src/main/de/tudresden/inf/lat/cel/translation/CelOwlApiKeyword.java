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

/**
 * This interface contains all the keywords used by OWL API in CEL.
 * 
 * @author Julian Mendez
 */
public interface CelOwlApiKeyword {
	public static final String keyClassify = "owlapi.classify";
	public static final String keyClearOntologies = "owlapi.clear-ontologies";
	public static final String keyDispose = "owlapi.dispose";
	public static final String keyGetAncestorClasses = "owlapi.get-ancestor-classes";
	public static final String keyGetAncestorProperties = "owlapi.get-ancestor-properties";
	public static final String keyGetDataPropertyRelationships = "owlapi.get-data-property-relationships";
	public static final String keyGetDescendantClasses = "owlapi.get-descendant-classes";
	public static final String keyGetDescendantProperties = "owlapi.get-descendant-properties";
	public static final String keyGetDomains = "owlapi.get-domains";
	public static final String keyGetEquivalentClasses = "owlapi.get-equivalent-classes";
	public static final String keyGetEquivalentProperties = "owlapi.get-equivalent-properties";
	public static final String keyGetInconsistentClasses = "owlapi.get-inconsistent-classes";
	public static final String keyGetIndividuals = "owlapi.get-individuals";
	public static final String keyGetInverseProperties = "owlapi.get-inverse-properties";
	public static final String keyGetLoadedOntologies = "owlapi.get-loaded-ontologies";
	public static final String keyGetObjectPropertyRelationships = "owlapi.get-object-property-relationships";
	public static final String keyGetRanges = "owlapi.get-ranges";
	public static final String keyGetRelatedIndividuals = "owlapi.get-related-individuals";
	public static final String keyGetRelatedValues = "owlapi.get-related-values";
	public static final String keyGetSubClasses = "owlapi.get-sub-classes";
	public static final String keyGetSubProperties = "owlapi.get-sub-properties";
	public static final String keyGetSuperClasses = "owlapi.get-super-classes";
	public static final String keyGetSuperProperties = "owlapi.get-super-properties";
	public static final String keyGetTypes = "owlapi.get-types";
	public static final String keyHasDataPropertyRelationship = "owlapi.has-data-property-relationship";
	public static final String keyHasObjectPropertyRelationship = "owlapi.has-object-property-relationship";
	public static final String keyHasType = "owlapi.has-type";
	public static final String keyIsAntiSymmetric = "owlapi.is-anti-symmetric";
	public static final String keyIsClassified = "owlapi.is-classified";
	public static final String keyIsConsistent = "owlapi.is-consistent";
	public static final String keyIsDefined = "owlapi.is-defined";
	public static final String keyIsEquivalentClass = "owlapi.is-equivalent-class";
	public static final String keyIsFunctional = "owlapi.is-functional";
	public static final String keyIsInverseFunctional = "owlapi.is-inverse-functional";
	public static final String keyIsIrreflexive = "owlapi.is-irreflexive";
	public static final String keyIsRealised = "owlapi.is-realised";
	public static final String keyIsReflexive = "owlapi.is-reflexive";
	public static final String keyIsSatisfiable = "owlapi.is-satisfiable";
	public static final String keyIsSubClassOf = "owlapi.is-sub-class-of";
	public static final String keyIsSymmetric = "owlapi.is-symmetric";
	public static final String keyIsTransitive = "owlapi.is-transitive";
	public static final String keyLoadOntologies = "owlapi.load-ontologies";
	public static final String keyRealise = "owlapi.realise";
	public static final String keyUnloadOntologies = "owlapi.unload-ontologies";
}
