# [CEL](https://julianmendez.github.io/cel/)

[![build](https://github.com/julianmendez/cel/workflows/Java%20CI/badge.svg)](https://github.com/julianmendez/cel/actions)

CEL is a lightweight Description Logic reasoner for large-scale biomedical ontologies. The CEL Plug-ing uses the [OWL API](https://owlcs.github.io/owlapi/) and lets CEL be used as a plug-in for [Protege](https://protege.stanford.edu/).

Description logics (DLs) are an important family of formalisms for reasoning about ontologies. CEL is the first reasoner for the Description Logic EL+, supporting as its main reasoning task the computation of the subsumption hierarchy induced by EL+ ontologies. The Description Logic EL+ forms the core expressive features of [OWL 2 EL Profile](https://www.w3.org/TR/owl2-profiles/#OWL_2_EL_2) -- which is a tractable fragment of the new Web Ontology Language OWL 2. The most distinguishing feature of CEL is that, unlike other modern DL reasoners, it implements a polynomial-time algorithm. The supported Description Logic EL+ offers a selected set of expressive means that are tailored toward the formulation of medical and biological ontologies.

From version 1.0, CEL also supports supplemental reasoning features like incremental classification, modularization and axiom pinpointing. Moreover, the OWL API wrapper for CEL has eventually become available, so now you can use CEL as the backend reasoner from within Protege!

Documentation: [CEL home page](https://tu-dresden.de/ing/informatik/thi/lat/forschung/software/cel)


## Authors

CEL: [Boontawee Suntisrivaraporn](https://meng234.blogspot.com)

CEL Plug-in: [Julian Mendez](https://julianmendez.github.io)


## Licenses

CEL: [Apache 2.0](https://www.apache.org/licenses/LICENSE-2.0.txt)

CEL Plug-in: [LGPL 3.0](https://www.gnu.org/licenses/lgpl-3.0.txt)


## CEL [Downloads](https://julianmendez.github.io/cel/downloads.html)


## CEL [Source Code](https://github.com/julianmendez/cel)


## CEL [Release Notes](https://julianmendez.github.io/cel/RELEASE-NOTES.html)


## Introduction to CEL

Description logics (DLs) are an important family of formalisms for reasoning about ontologies. CEL is the first reasoner for the description logic EL+, supporting as its main reasoning task the computation of the subsumption hierarchy induced by EL+ ontologies. The most distinguishing feature of CEL is that, unlike other modern DL reasoners, it implements a polynomial-time algorithm. The supported description logic EL+ offers a selected set of expressive means that are tailored towards the formulation of medical and biological ontologies.

**CEL is an [OWL 2 EL](https://www.w3.org/tr/owl2-profiles/#OWL_2_EL) [reasoner](https://www.w3.org/2007/OWL/wiki/Implementations) (albeit currently with some limitations).**

To get some idea about what logical aspects can be expressed in CEL ontologies, look at our [toy ontologies](https://julianmendez.github.io/cel/toyont.html) page.


## The CEL System

CEL comes with a very simple interactive interface that provides users with all essential functionalities, including interactive help system. Developing CEL is ongoing work. We aim at pushing its expressive power to the logic EL++, with which one can express, among other things, disjoint concepts, concrete domains, and nominals.

NEWS: A beta version of CEL 1.0 has now been released! A few minor bugs have been fixed and some interesting features have been integrated. New features include role domain and range constraints, role reflxivity, ABox, more powerful subsumption queries (no longer limited to named concepts), and a weak form of incremental classification. Refer to the CEL manual for the details.

Build 6 of CEL 1.0 includes a prototype implementation of the algorithm for extracting the reachability-based modules (equivalent to the minimal locality-based modules modulo EL+). To modularize the ontology w.r.t. a concept or a signature, use the commands `(extract-c-module C)` or `(extract-module S)`, respectively. For the details on this modularization approach and its promising experimental results on NCI, NOT-GALEN, FULL-GALEN, and SNOMED CT, take a look at this [technical report](https://lat.inf.tu-dresden.de/research/reports/2007/Sun-07-LTCS.pdf).


## Downloading CEL

CEL's sources are open and can be downloaded from its [repository](https://github.com/julianmendez/cel) (formerly at [Google Code](https://code.google.com/p/cel/)).

The **CEL plug-in** allows using CEL as a [Protégé](https://protege.stanford.edu/) plug-in, using the [OWL API](https://owlcs.github.io/owlapi/). This is a jar (a Java library) that can be downloaded from [the download site](https://sourceforge.net/projects/latitude/files/cel/). The installation is just copying the jar into the plugins directory. The **current releases** contain a version of CEL compiled for Linux (32-bit), and therefore they **run only on Linux systems**. It was compiled with [Allegro Common Lisp](https://franz.com/products/allegro-common-lisp/), and the sources can be compiled for other platforms.


## Using CEL

The CEL [manual](https://sourceforge.net/projects/latitude/files/cel/cel/cel-manual.pdf) details how to use the interactive interface. Here are a few example ontologies, [med.tbox](https://sourceforge.net/projects/latitude/files/cel/ontologies/med.tbox) and [forest.tbox](https://sourceforge.net/projects/latitude/files/cel/ontologies/forest.tbox), to help get an idea of what can be expressed in CEL. More toy ontologies can be found on the [repository](https://julianmendez.github.io/cel/toyont.html) page. CEL can work as a backend reasoner for a graphical ontology editor, such as Protégé. The following are the possible command line options and a couple of examples:

```
Usage: cel [options]

If no options are given, CEL starts up and prompts for interactive commands.
The following options can be used:
   -loadOntology file  <-l>    Load and preprocess ontology from file
   -classifyOntology <-c>      Classify the ontology
   -outputSupers [file]        Output the sets of all super-classes
   -outputTaxonomy [file]      Output the direct sub- and super-classes
   -outputHierarchy [file]     Output the hierarchy as an indented tree
   -quit <-q>                  Exit; Don't enter the interactive interface
   -help <-h>                  Display this help message

Examples:
$ cel -loadOntology med.tbox -classifyOntology -outputTaxonomy med.dag -quit
To load and classify the ontology "med.tbox", and to output the DAG to "med.dag"
$ cel -loadOntology gene.tbox
To start the interactive CEL interface with the initial ontology "gene.tbox"
loaded and preprocessed
```


## Publications on/related to CEL

F. Baader, C. Lutz, and B. Suntisrivaraporn. CEL—A Polynomial-time Reasoner for Life Science Ontologies. In U. Furbach and N. Shankar, editors, Proceedings of the 3rd International Joint Conference on Automated Reasoning (IJCAR'06), volume 4130 of Lecture Notes in Artificial Intelligence, pages 287–291. Springer-Verlag, 2006.
[Bibtex entry](https://lat.inf.tu-dresden.de/research/papers/2006/BaaLutSun-IJCAR-06.bib)  [Abstract](https://lat.inf.tu-dresden.de/research/papers-2006.html#BaaLutSun-IJCAR-06)  [Paper (PS)](https://lat.inf.tu-dresden.de/research/papers/2006/BaaLutSun-IJCAR-06.ps.gz)  [Paper (PDF)](https://lat.inf.tu-dresden.de/research/papers/2006/BaaLutSun-IJCAR-06.pdf)  [©Springer-Verlag](https://www.springer.de/comp/lncs/index.html)

F. Baader, C. Lutz, and B. Suntisrivaraporn. **Efficient Reasoning in EL+**. In Proceedings of the 2006 International Workshop on Description Logics (DL2006), CEUR-WS, 2006.
[Bibtex entry](https://lat.inf.tu-dresden.de/research/papers/2006/BaaLutSun-DL-06.bib)  [Abstract](https://lat.inf.tu-dresden.de/research/papers-2006.html#BaaLutSun-DL-06)  [Paper (PS)](https://lat.inf.tu-dresden.de/research/papers/2006/BaaLutSun-DL-06.ps.gz)  [Paper (PDF)](https://lat.inf.tu-dresden.de/research/papers/2006/BaaLutSun-DL-06.pdf)

F. Baader, C. Lutz, and B. Suntisrivaraporn. **Is Tractable Reasoning in Extensions of the Description Logic EL Useful in Practice?**. In Proceedings of the Methods for Modalities Workshop (M4M-05), Berlin, Germany, 2005.
[Bibtex entry](https://lat.inf.tu-dresden.de/research/papers/2005/BaaLutSun-M4M-05.bib)  [Abstract](https://lat.inf.tu-dresden.de/research/papers-2005.html#BaaLutSun-M4M-05)  [Paper (PS)](https://lat.inf.tu-dresden.de/research/papers/2005/BaaLutSun-M4M-05.ps.gs)  [Paper (PDF)](https://lat.inf.tu-dresden.de/research/papers/2005/BaaLutSun-M4M-05.pdf)

F. Baader, S. Brandt, and C. Lutz. **Pushing the EL Envelope**. In Proceedings of the Nineteenth International Joint Conference on Artificial Intelligence IJCAI-05, Edinburgh, UK, 2005. Morgan-Kaufmann Publishers.
[Bibtex entry](https://lat.inf.tu-dresden.de/research/papers/2005/BaaderBrandtLutz-IJCAI-05.bib)  [Abstract](https://lat.inf.tu-dresden.de/research/papers-2005.html#BaaderBrandtLutz-IJCAI-05)  [Paper (PDF)](https://lat.inf.tu-dresden.de/research/papers/2005/BaaderBrandtLutz-IJCAI-05.pdf)

Boontawee Suntisrivaraporn, Franz Baader, Stefan Schulz, and Kent Spackman. **Replacing SEP-Triplets in SNOMED CT using Tractable Description Logic Operators**. In Jim Hunter, Riccardo Bellazzi, Ameen Abu-Hanna, editor, Proceedings of the 11th Conference on Artificial Intelligence in Medicine (AIME'07), Lecture Notes in Computer Science. Springer-Verlag, 2007.
[Bibtex entry](https://lat.inf.tu-dresden.de/research/papers/2007/SunBaaSchSpa-AIME-07.bib)  [Abstract](https://lat.inf.tu-dresden.de/research/papers-2007.html#SunBaaSchSpa-AIME-07)  [Paper (PS)](https://lat.inf.tu-dresden.de/research/papers/2007/SunBaaSchSpa-AIME-07.ps)  [Paper (PDF)](https://lat.inf.tu-dresden.de/research/papers/2007/SunBaaSchSpa-AIME-07.pdf)  [©Springer-Verlag](https://www.springer.de/comp/lncs/index.html)

Franz Baader, Rafael Peñaloza, and Boontawee Suntisrivaraporn. **Pinpointing in the Description Logic EL+**. In Proceedings of the 30th German Conference on Artificial Intelligence (KI2007), LNAI, Osnabrück, Germany, 2007. Springer.
[Bibtex entry](https://lat.inf.tu-dresden.de/research/papers/2007/BaaPenSun-KI07.bib)  [Abstract](https://lat.inf.tu-dresden.de/research/papers-2007.html#BaaPenSun-KI07)  [Paper (PDF)](https://lat.inf.tu-dresden.de/research/papers/2007/BaaPenSun-KI07.pdf)  [©Springer-Verlag](https://www.springer.de/comp/lncs/index.html)

Franz Baader, Carsten Lutz, and Boontawee Suntisrivaraporn. **Is Tractable Reasoning in Extensions of the Description Logic EL Useful in Practice?**. In Journal of Logic, Language and Information, Special Issue on Method for Modality (M4M), 2007. To appear.
[Bibtex entry](https://lat.inf.tu-dresden.de/research/papers/2007/BaaLutSun-JoLLI-07.bib)  [Abstract](https://lat.inf.tu-dresden.de/research/papers-2007.html#BaaLutSun-JoLLI-07)  [Paper (PS)](https://lat.inf.tu-dresden.de/research/papers/2007/BaaLutSun-JoLLI-07.ps.gz)  [Paper (PDF)](https://lat.inf.tu-dresden.de/research/papers/2007/BaaLutSun-JoLLI-07.pdf)

Boontawee Suntisrivaraporn. **Module Extraction and Incremental Classification: A Pragmatic Approach for EL+ Ontologies**. In Sean Bechhofer, Manfred Hauswirth, Joerg Hoffmann, and Manolis Koubarakis, editors, Proceedings of the 5th European Semantic Web Conference (ESWC'08), Lecture Notes in Computer Science. Springer-Verlag, 2008.
[Bibtex entry](https://lat.inf.tu-dresden.de/research/papers/2008/Sun-ESWC-08.bib)  [Abstract](https://lat.inf.tu-dresden.de/research/papers-2008.html#Sun-ESWC-08)  [Paper (PDF)](https://lat.inf.tu-dresden.de/research/papers/2008/Sun-ESWC-08.pdf)  [©Springer-Verlag](https://www.springer.de/comp/lncs/index.html)

Boontawee Suntisrivaraporn. **Empirical evaluation of reasoning in lightweight DLs on life science ontologies**. In Proceedings of the 2nd Mahasarakham International Workshop on AI (MIWAI'08), 2008.
[Bibtex entry](https://lat.inf.tu-dresden.de/research/papers/2008/Sun-MIWAI-08.bib)   [Abstract](https://lat.inf.tu-dresden.de/systems/cel/papers-2008.html#Sun-MIWAI-08)  [Paper (PDF)](https://lat.inf.tu-dresden.de/research/papers/2008/Sun-MIWAI-08.pdf)

Stefan Schulz, Boontawee Suntisrivaraporn, Franz Baader, and Martin Boeker. **SNOMED reaching its adolescence: Ontologists' and logicians' health check**. International Journal of Medical Informatics, 78(Supplement 1):S86–S94, 2009.
[Bibtex entry](https://lat.inf.tu-dresden.de/research/papers/2009/SchEtAl-JMI-09.bib)   [Abstract](https://lat.inf.tu-dresden.de/systems/cel/papers-2009.html#SchEtAl-JMI-09)   [Paper (PDF)](https://lat.inf.tu-dresden.de/research/papers/2009/SchEtAl-JMI-09.pdf)


## Contacts

Any questions or bug reports are truly welcome, please feel free to contact us by sending an email to the authors.


