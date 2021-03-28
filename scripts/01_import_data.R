### Preamble ###
# Purpose: Import reproduction data
# Author: Amy Farrow
# Contact: amy.farrow@mail.utoronto.ca
# Date: 2021-03-21
# Pre-requisites: None
# to-dos:


### Workspace setup ###
library(tidyverse)
library(haven)
library(readstata13)
library(labelled)

### Import data ###

# this import shows the codes for answers, not just numbers
isrdata <- read.dta13("inputs/data/Instructor-Student Relationships Experiment Data_Anonymous.dta")

# save as a csv
write_csv(isrdata, "inputs/data/isrdata.csv")

### Make the codebook ###

# this import shows the variable labels, but the answers are numerical
isrdata2 = read_dta("inputs/data/Instructor-Student Relationships Experiment Data_Anonymous.dta")

codebook <- tibble(variables = names(isrdata),
                   full_names = var_label(isrdata2))
codebook <- data.frame(lapply(codebook, as.character), stringsAsFactors=FALSE)

# save the codebook as csv
write_csv(codebook, "inputs/data/codebook.csv")