
Introduction
-------------

> "A case could be made to the effect that the typical history of a concept, whether it be 'chemical element', 'atom', 'the unconscious' or whatever, involves the initial emergence of the concept as a vague idea, followed by its gradual clarification as the theory in which it plays a part takes a more precise and coherent form" -- A.F. Chalmers from *What is This Thing Called Science*

Cohorts are fundamental to epidemiology. Loosely, a cohort is simply a group of study subjects sharing a common set of characteristics. The Asclepias 
project proposes a more rigorous definition. Using purely algebraic terms, Asclepias defines a mapping from a population of subjects who have events occurring in time to a cohort (or set of cohorts) of observational units with a collection of features -- relevant information (or "variables") drawn from events. The benefits of this formalism are manifest in the challenges of creating analyzable data from disparate data sources based on scientists' description of who should be in a cohort and what information a cohort should contain.

The extraction and transformation of source data such as insurance claims, electronic medical records, or case report forms into analysis-ready cohorts consumes an inordinate amount of resources in a research workflow. Bottlenecks in the process may include:

* computationally-intensive transformations of source data into a common data model;
* a data model (e.g. relational databases) that does not align with how scientists reason or describe the cohort;
* error-prone translations of a scientist's plan by programmers when creating analytic data from the common model.

In terms of human resources, the translation step is often the most consuming. A scientist describes a cohort in natural, often imprecise, language. No matter how articulate or structured the description, the translation to code is prone to misinterpretations by programmers. This step requires frequent clarification between the scientists and programmers. The canonical model for this process involves teams of scientists writing data specifications in Microsoft Word documents which are then translated by SAS programmers into analytic datasets. This step is often coded by two or more programmers to ensure quality. More recently, proprietary solutions such as [aetion](https://www.aetion.com/platform) and open source projects such as [ConceptQL](https://github.com/outcomesinsights/conceptql) and [ODHSI](https://www.ohdsi.org/)'s [ATLAS](http://www.ohdsi.org/web/atlas/#/home) program [OMOP](https://www.ohdsi.org/data-standardization/the-common-data-model/)'s Common Data Model have emerged.

Tools such as ConceptQL and the OMOP Common Data Model offer language abstractions targeted for describing cohort algorithms. However, these abstractions are still tightly bound to the general programming language, SQL in these cases, in which the abstraction has been implemented. Asclepias takes a different, more formative approach. Asclepias defines a cohort in terms of [algebraic data types](https://en.wikipedia.org/wiki/Algebraic_data_type) and functions that operate on these data types. From this algebra and its supporting DSL, Asclepias provides a rigorous framework for reasoning about, composing, and computing a cohort.

The ultimate goal of Asclepias is to provide scientists with a domain specific language (DSL) to define cohorts without the expensive translation process. To that end, the Asclepias team will design an algebra of events in terms of temporal logic that maps closely to how scientists think about defining cohorts and its algorithms. This algebra will be based upon in part upon existing research, such as Allen's Interval Algebra [@allen1983]. At the same time, the Asclepias team will implement the algebra as an embedded DSL (eDSL) in a functional programming language with the goal of creating a compilation target language for an eventual DSL. 


