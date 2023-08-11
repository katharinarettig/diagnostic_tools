# Web-based diagnostic tools

This repository contains R codes and files to reproduce a reduced English version of the web-based diagnostic tools available on https://www.lubw.baden-wuerttemberg.de/wasser/diagnosetool-makrozoobenthos and described in Rettig et al. (in review).

These tools are the result of the project 'Development of diagnostic tools for the identification of causal stressors on basis of macroinvertebrates in the context of stream assessment under the Water Framework Directive' funded by the State Agency for the Environment of Baden-Württemberg (LUBW), Germany. 

## data_text
The folder [network_files](https://github.com/katharinarettig/diagnostic_tools/tree/main/data_text/network_files) contains the Bayesian belief networks (BBN) (.net format), on which the diagnostic tools are based on.

To keep the R scripts handy and universal, tool-specific variable names, codings, display values and class boundaries are kept separate from the R codes and stored in tables (.csv format).

The general_diagnosis.csv tables contain, for example, assignments of headings and default values to display the range of probabilities in the diagnostic tools. 

The input_diagnosis.csv tables define the variable names and state classes and the help entries as displayed in the selection bar (input) of the diagnostic tools. They also assign these variables and their states to their equivalents in the underlying BBN. 

The output_diagnosis.csv tables contain similar information about variable names and state classes for the reactive radar plot and hierarchy (output). They also contain the definition of the state class(es) used on the changed probability values of which the output is generated.

## html_files
This file contains information that is displayed within the diagnostic tools. These include, explanations of the tool and its application, interpretation of results, recommended further reading as well as background information on stressors and linked explanations, such as possible management options. The information on stressors, except for headers, is not provided in English but can be easily translated by copy and paste into a common translation program.

## www
This folder contains photos and graphics embedded in the diagnsotic tools.

## Code
The interface between the BBNs and the web-based diagnostic tools is implemented in R (R Core Team, 2021) and RStudio (https://rstudio.com/products/rstudio/). The script enables the import and storage of files as well as the definition of functions to perform calculations with the web-based diagnostic tools.

## app
Shiny (Chang et al., 2019) allows you to create interactive web apps directly from R. This code defines the server and user interface of the diagnostic tools.
<br>
<br>

#### References
Chang, W., Cheng, J., Allaire, J. J., Sievert, C., Schloerke, B., Xie, Y., Allen, J., McPherson, J., Dipert, A., & Borges, B. (2021). Shiny: web application framework for R. https://cran.r-project.org/web/packages/shiny/index.html <br>
R Core Team. (2021). R: A Language and Environment for Statistical Computing [Computer software]. R Foundation for Statistical Computing. Vienna. https://www.R-project.org <br>
Rettig, K., Semmler-Elpers, R., Brettschneider, D. & Feld, C. K. (in review): Of causes and symptoms - using monitoring data and expert knowledge to diagnose causes of stream degradation.
