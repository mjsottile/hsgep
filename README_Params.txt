Setting GEP Parameters
----------------------

Rates:

rateMutate
rate1R
rate2R
rateGR
rateIS
rateRIS
rateGT

Genome:

genomeTerminals: list of characters that are valid entries in a
chromosome for terminals (e.g.: variables in a regression problem).

genomeNonterminals: list of characters that are valid entries in a
chromosome for nonterminals (e.g.: operators in a regression problem).

genomeMaxArity: maximum arity operator in the nonterminal list.
Examples: boolean NOT has arity 1, arithmetic add has arity 2, boolean
IF has arity 3.

genomeNumGenes: number of genes in a chromosome.

genomeHeadLength: number of characters in the head of a gene.  This is
used in combination with the maximum arity parameter to determine the
overall gene length.

genomeGeneConnector: nonterminal used to connect genes in a multi-gene
chromosome to form a single expression tree.

Genetic Operator Parameters:

maxISLen: Maximum length of insertion sequences.

maxRISLen: Maximum length of root insertion sequences.

Population Parameters:

populationSize: Number of individuals in the population per step.

numGenerations: How many generations to run algorithm for if maximum
fitness not achieved.

Fitness and selection:

maxFitness: Fitness of ideal individual.

rouletteExponent: Exponent for weight function used to determine
roulette wheel bin widths.  Current default function is 1/(k^e) where
k is the bin number and e is the exponent.

selectionRange: Range of selection parameter (M) from the GEP paper.
