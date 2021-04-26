# takingittothenextlevelrepro

This repo contains an R project file for my paper "The inefficacy of superficial similarities for improving instructor-student relationships: Replicating ‘Taking It to the Next Level". This is a replication study of Robinson, C. D., Scott, W., & Gottfried, M. A. (2019). Taking It to the Next Level: A Field Experiment to Improve Instructor-Student Relationships in College. _Aera Open 5_(1):1-15. https://doi.org/10.1177/2332858419839707

The original paper can be found here: https://files.eric.ed.gov/fulltext/EJ1210490.pdf

This repo contains three folders: inputs, outputs, and scripts. Inputs:

    Data: codebook for the original data (data is only available through ICPSR)
    Literature: the original paper and supplemental material

Outputs:

    Exploratory analysis
    Paper: R Markdown, a pdf version, and a complete bibliography

Scripts:

    Import data
    Make data file for Shiny application
    
Shiny:

    Shiny application code    

How to generate the paper
    
    Request original data from ICPSR and place in the data folder
    Open takingittothenextlevelrepro.Rproj in RStudio
    Install libraries using install.packages() if necessary
    Run 01_import_data.R
    Run paper.Rmd
    For the Shiny application: run 02_make_shiny_data.R and then app.R