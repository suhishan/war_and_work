## War and Work: Labor Market impact of the Nepalese Civil War

This repo contains the code necessary to replicate the results of my paper 'War and Work : Labor Market Impacts of the Nepalese Civil War'. It also includes the relevant .tex files necessary to build the paper document.

------------------------------------------------------------------------

The following is the description of files and folders and their respective contents and functions.

### Main Replication R files

1.  *`war_and_work.Rmd`* :This file includes R code for reading NLFS 1 and 2 dataset, cleaning them, selecting required variables, and appending them. It also includes reading and cleaning conflict dataset from UCDP, collapsing the data into district level, and then an overall merge between this conflict dataset and appended NLFS 1 and 2. The file also includes code for treatment assignment.
2.  *`war_and_work_analysis.Rmd`* : This is the main analysis file which produces .tex documents and figures for descriptive statistics, the table for normalized differences, table of covariate trends, propensity score regressions, and tables of Doubly Robust DiD results (apart from the Bayesian analysis portion of the paper which is in `war_and_work_map.Rmd`. The tex files and figures produced are stored in the folder `Analysis files/` . **Note:** Please run this file after running `war_and_work.Rmd` .
3.  `war_and_work_map.Rmd` : This file produces Nepal's map with conflict casualties as presented in the paper. Further, this file includes the code for the *Bayesian* small sample analysis portion of my paper. As with `war_and_work_analysis.Rmd`, any relevant figures or .tex file is stored in the folder `Analysis files/` .
4.  `rough.R` : A normal `R` file intended as a place for testing out code before putting in them the replication files and serves the function of a rough paper in exams and/or rough notes on a napkin.
5.  `war_and_work_district.Rmd` : While originally intended for district-level Bayesian analysis, code in this file is obsolete and is not necessary for replication for the paper.

------------------------------------------------------------------------

### Latex document for the paper

The main working pdf of the paper is named `war_and_work.pdf` , located inside the folder `research_paper/` .

The latex file used to write the research paper is located in `research paper/war_and_work.tex` . The same folder also includes a `.bib` file of all the references used and the afore-mentioned working pdf made from building the latex file.
