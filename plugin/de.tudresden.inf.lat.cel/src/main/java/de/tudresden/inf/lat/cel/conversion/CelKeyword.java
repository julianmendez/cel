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

/**
 * This interface has all the keywords of commands supported by CEL.
 * 
 * @author Julian Mendez
 */
public interface CelKeyword {
	public static final String keyAboxConsistentP = "abox-consistent?";
	public static final String keyActivateTtbox = "activate-ttbox";
	public static final String keyAllConcepts = "all-concepts";
	public static final String keyAllIndividuals = "all-individuals";
	public static final String keyAllRoles = "all-roles";
	public static final String keyAllUnsatisfiableConcepts = "all-unsatisfiable-concepts";
	public static final String keyAncestors = "ancestors";
	public static final String keyAnd = "and";
	public static final String keyBottom = "bottom";
	public static final String keyBuild = "build";
	public static final String keyChildren = "children";
	public static final String keyChildRoles = "child-roles";
	public static final String keyClassifyOntology = "classify-ontology";
	public static final String keyClassifyTbox = "classify-tbox";
	public static final String keyClassifyTtbox = "classify-ttbox";
	public static final String keyClearOntology = "clear-ontology";
	public static final String keyClearTbox = "clear-tbox";
	public static final String keyClearTtbox = "clear-ttbox";
	public static final String keyCommitTtbox = "commit-ttbox";
	public static final String keyCompose = "compose";
	public static final String keyConcept_equivalentP = "concept-equivalent?";
	public static final String keyConceptImpliesP = "concept-implies?";
	public static final String keyConceptInclusion = "concept-inclusion";
	public static final String keyConceptInstances = "concept-instances";
	public static final String keyConceptP = "concept?";
	public static final String keyConceptSatisfiableP = "concept-satisfiable?";
	public static final String keyConceptSubsumesP = "concept-subsumes?";
	public static final String keyCreateOntology = "create-ontology";
	public static final String keyCreateTbox = "create-tbox";
	public static final String keyDeactivateTtbox = "deactivate-ttbox";
	public static final String keyDefault = "default";
	public static final String keyDefineConcept = "define-concept";
	public static final String keyDefinePrimitiveConcept = "define-primitive-concept";
	public static final String keyDefinePrimitiveIndividual = "define-primitive-individual";
	public static final String keyDefinePrimitiveRole = "define-primitive-role";
	public static final String keyDescendants = "descendants";
	public static final String keyDetailOntology = "detail-ontology";
	public static final String keyDifferentIndividuals = "different-individuals";
	public static final String keyDisjoint = "disjoint";
	public static final String keyDomain = "domain";
	public static final String keyEl = "el+";
	public static final String keyEquivalent = "equivalent";
	public static final String keyEquivalentP = "equivalent?";
	public static final String keyEquivalents = "equivalents";
	public static final String keyGlobalReset = "global-reset";
	public static final String keyHelp = "help";
	public static final String keyImplies = "implies";
	public static final String keyImpliesP = "implies?";
	public static final String keyIndividualDirectTypes = "individual-direct-types";
	public static final String keyIndividualP = "individual?";
	public static final String keyIndividualTypes = "individual-types";
	public static final String keyInstance = "instance";
	public static final String keyInstanceP = "instance?";
	public static final String keyLoadOntology = "load-ontology";
	public static final String keyLoadTbox = "load-tbox";
	public static final String keyOntologyClassifiedP = "ontology-classified?";
	public static final String keyOntologyConsistentP = "ontology-consistent?";
	public static final String keyOntologyPreparedP = "ontology-prepared?";
	public static final String keyOutputHierarchy = "output-hierarchy";
	public static final String keyOutputImpSets = "output-imp-sets";
	public static final String keyOutputSubsumption = "output-subsumption";
	public static final String keyOutputSupers = "output-supers";
	public static final String keyOutputSynonyms = "output-synonyms";
	public static final String keyOutputTaxonomy = "output-taxonomy";
	public static final String keyParentRoles = "parent-roles";
	public static final String keyParents = "parents";
	public static final String keyPImplies = "?implies";
	public static final String keyPSubsumes = "?subsumes";
	public static final String keyQuit = "quit";
	public static final String keyRange = "range";
	public static final String keyReflexive = "reflexive";
	public static final String keyReflexiveP = "reflexive?";
	public static final String keyRelated = "related";
	public static final String keyReleaseAllOntologies = "release-all-ontologies";
	public static final String keyReleaseAllTboxes = "release-all-tboxes";
	public static final String keyReleaseOntology = "release-ontology";
	public static final String keyReleaseTbox = "release-tbox";
	public static final String keyRepository = "repository";
	public static final String keyRestoreOntology = "restore-ontology";
	public static final String keyRestoreTbox = "restore-tbox";
	public static final String keyRoleEquivalent = "role-equivalent";
	public static final String keyRoleImpliesP = "role-implies?";
	public static final String keyRoleInclusion = "role-inclusion";
	public static final String keyRoleP = "role?";
	public static final String keyRoleSubsumesP = "role-subsumes?";
	public static final String keySameIndividuals = "same-individuals";
	public static final String keySatisfiableP = "satisfiable?";
	public static final String keyShutdownDigServer = "shutdown-dig-server";
	public static final String keySome = "some";
	public static final String keyStart = "start";
	public static final String keyStartupDigServer = "startup-dig-server";
	public static final String keySubConcepts = "sub-concepts";
	public static final String keySubRoles = "sub-roles";
	public static final String keySubsumesP = "subsumes?";
	public static final String keySuperConcepts = "super-concepts";
	public static final String keySuperRoles = "super-roles";
	public static final String keyTboxClassifiedP = "tbox-classified?";
	public static final String keyTboxConsistentP = "tbox-consistent?";
	public static final String keyTboxPreparedP = "tbox-prepared?";
	public static final String keyTop = "top";
	public static final String keyTransitive = "transitive";
	public static final String keyTransitiveP = "transitive?";
	public static final String keyTtboxActiveP = "ttbox-active?";
	public static final String keyTtboxClassifiedP = "ttbox-classified?";
	public static final String keyTtboxClearedP = "ttbox-cleared?";
	public static final String keyTtboxPreparedP = "ttbox-prepared?";
	public static final String keyVersion = "version";
}
