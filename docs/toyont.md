## Toy Ontologies Demoing CEL's Features

- **LAT**: A toy ontology that features most, if not all, expressive means available in the description logic EL+ which is the underlying logic of the [CEL system](https://tu-dresden.de/ing/informatik/thi/lat/forschung/software/cel).
  It logically defines some basic notions (concepts and roles) revolving a university research group and has a small extensional component (ABox) inspired by the DL research group of [Prof. Franz Baader](https://tu-dresden.de/ing/informatik/thi/lat).
  This ontology has intendedly been modelled to be inconsistent.
  Despite the relatively inexpressive DL dialect and the size of the ontology, the sources of inconsistencies are not so trivial to be pinpointed.
  Can you find them?
  ([KRSS](https://sourceforge.net/projects/latitude/files/cel/ontologies/lat.cl), OWL)
- **ENDOCARDITIS**: A toy medical ontology showing a typical use of right-identity rules, in conjunction with the anatomical part-of and spatial location roles.
  In the example, endocarditis 'is a' heart disease, since it 'has location' endocardium which is 'part of' the heart.
  ([KRSS](https://sourceforge.net/projects/latitude/files/cel/ontologies/endocarditis.cl), OWL)
- **NO-SEP**: A small example showing a re-engineered extract of SNOMED CT dispensing with SEP-triplets.
  Taken from Fig.2 of the paper ["Replacing SEP-Triplets in SNOMED CT using Tractable Description Logic Operators"](https://tu-dresden.de/ing/informatik/thi/lat/forschung/veroeffentlichungen#latpub:SunBaaSchSpa-AIME-07).
  ([KRSS](https://sourceforge.net/projects/latitude/files/cel/ontologies/no-sep.cl), OWL)
- **PART-WHOLE**: A toy ontology illustrating the use of reflexivity and transitivity assertions on roles. In particular, part-of MUST be declared transitive so that the classification system can take care of transitivity reasoning.
  Part and whole of some entity could be referred to by the new role, part-whole-of, which is designed to replace the needs for S-nodes in the SEP-triplet modelling technique.
  ([KRSS](https://sourceforge.net/projects/latitude/files/cel/ontologies/part-whole.cl), OWL)
- **KANGAROO**: A toy ontology about maternal kangaroos who have a pouch and a cub.
  But, why can't they exist, i.e., the concept is unsatisfiable?
  ([KRSS](https://sourceforge.net/projects/latitude/files/cel/ontologies/kangaroo.cl), OWL)
- **FOREST**: A toy ontology hopefully giving an idea of using the left-identity rule or left absorption. After all, an ideal forest should have both the color green and the color brown, shouldn't it?
  ([KRSS](https://sourceforge.net/projects/latitude/files/cel/ontologies/forest.cl), OWL)


## Life Science Ontologies that CEL Can Classify

- **GO**: [The Gene Ontology](http://geneontology.org/) is a controlled vocabulary that describes gene and gene product attributes.
  ([KRSS](https://sourceforge.net/projects/latitude/files/cel/ontologies/go.cel.zip), [OWL](https://sourceforge.net/projects/latitude/files/cel/ontologies/go.cel.zip))
- **NCI**: A large ontology about classification of cancers, developed by [National Cancer Institute](https://www.cancer.gov/).
  Though containing several domain and range restrictions, the structure of this ontology is very simple.
  CEL 1.0 and later can classify this ontology.
  ([KRSS](https://sourceforge.net/projects/latitude/files/cel/ontologies/nciOntology.cel.zip), [OWL](https://sourceforge.net/projects/latitude/files/cel/ontologies/nciOntology.owl))
- **NOT-GALEN**: A stripped-down version of NotGalen with no role functionality, for use as a benchmark for the CEL system. NotGalen has been widely used for benchmarking several standard DL reasoners.
  ([KRSS](https://sourceforge.net/projects/latitude/files/cel/ontologies/not-galen.cel), [OWL](https://sourceforge.net/projects/latitude/files/cel/ontologies/not-galen.owl))
- **FULL-GALEN**: the full Galen medical ontology (again, excluding role inverses and functionalities).
  ([KRSS](https://sourceforge.net/projects/latitude/files/cel/ontologies/full-galen.cel.zip), [OWL](https://sourceforge.net/projects/latitude/files/cel/ontologies/full-galen.owl.zip))
- **SNOMED CT**: the Systematized Nomenclature of Medicine, Clinical Terms, is a large standardized clinical terminology adopted by health care sectors in several countries.
  More info on [IHTSDO's web site](https://www.snomed.org/).


