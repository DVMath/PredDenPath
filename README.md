# PredDenPath

# Repository of Code and Data of study with title "Predicting high dengue incidence in municipalities of Brazil using path signatures"
## Author: Daniel A.M. Villela, Programa de Computacão Científica, Fundacão Oswaldo Cruz

Script path_functions.R contains helper functions

Script pathDengue.R contain the main functions and implements all scenarios tested in the study.
It runs the evaluations, which might take a few hours, and generates the figures.
At this point, the scenarios are tested sequentially, but a newer implementation in the near future should provide parallel 
running distributed in a set of CPU cores.

The flag captura is set to FALSE to use the pre-processed data, given by the RData file.

Usage: Rscript --no-save pathDengue 1 156 

The arguments mean that all configurations with differents values of k and number of weeks will be tested.
