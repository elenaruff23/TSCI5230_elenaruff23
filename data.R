#'---
#' title: "TSCI 5050: Introduction to Data Science"
#' author: 'Author One ^1^, Author Two ^1^'
#' abstract: |
#'  | Provide a summary of objectives, study design, setting, participants,
#'  | sample size, predictors, outcome, statistical analysis, results,
#'  | and conclusions.
#' documentclass: article
#' description: 'Manuscript'
#' clean: false
#' self_contained: true
#' number_sections: false
#' keep_md: true
#' fig_caption: true
#' output:
#'  html_document:
#'    toc: true
#'    toc_float: true
#'    code_folding: show
#' ---
#'
#+ init, echo=FALSE, message=FALSE, warning=FALSE
# init ----
# This part does not show up in your rendered report, only in the script,
# because we are using regular comments instead of #' comments
debug <- 0;
knitr::opts_chunk$set(echo=debug>-1, warning=debug>0, message=debug>0, class.output="scroll-20", attr.output='style="max-height: 150px; overflow-y: auto;"');

library(rio);# simple command for importing and exporting
library(pander); # format tables
#library(printr); # set limit on number of lines printed
library(dplyr); #add dplyr library
library(lubridate) #date manipulation
library(stringr) #string manipulation


options(max.print=500);
panderOptions('table.split.table',Inf); panderOptions('table.split.cells',Inf);


data_location <- "~/Downloads/archive/"
list.files(data_location,full.names = TRUE) #identify files in dataset by full path 
dat <- sapply(list.files(data_location,full.names = TRUE),import)


# data ingestion ----
# Your two data frames
conditions <- dat$`/Users/elenaruff/Downloads/archive//conditions.csv`
patients <- dat$`/Users/elenaruff/Downloads/archive//patients.csv`

# First, subset to acute/viral conditions
acute_viral <- conditions %>%
  filter(grepl("acute|viral", DESCRIPTION, ignore.case = TRUE))

# Match patients and calculate age at encounter
# Mutate is how you redefine columns in a data frame - if column exists it will replace, if column does not exist, it will create one
acute_viral_age <- acute_viral %>%
  left_join(
    patients %>% select(Id, BIRTHDATE),
    by = c("PATIENT" = "Id")
  ) %>% 
  mutate(
    START = as.Date(START),
    BIRTHDATE = as.Date(BIRTHDATE),
    age_at_encounter = time_length(
      interval(BIRTHDATE, START),
      unit = "years"
    )
  )




