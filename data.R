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
library(tidyr) # for pivot_wider function
library(purrr) #for map() function


options(max.print=500);
panderOptions('table.split.table',Inf); panderOptions('table.split.cells',Inf);


data_location <- "~/Downloads/archive/"
list.files(data_location,full.names = TRUE) #identify files in dataset by full path 
dat <- sapply(list.files(data_location,full.names = TRUE),import,simplify = FALSE) %>%
  setNames(.,basename(names(.)))


# data ingestion ----
# First, subset to acute/viral conditions
acute_viral <- dat$conditions.csv %>%
  filter(grepl("acute|viral", DESCRIPTION, ignore.case = TRUE))
# AI: Creates a subset containing conditions with "acute" or "viral" in the description.

# Match patients and calculate age at encounter
# Mutate is how you redefine columns in a data frame - if column exists it will replace, if column does not exist, it will create one
acute_viral_age <- acute_viral %>%
  left_join(
    dat$patients.csv %>% select(Id, BIRTHDATE),
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
# AI: Joins patient birthdates to the condition data and calculates age at the time of each encounter.

#determine age (copilot -> reviewed and edited by Eva)
patients <- dat$patients.csv
patients$age_2025 <- 2025 - year(patients$BIRTHDATE) #age of paitents as of Dec 31 2025

#determine age at time of death (copilot -> reviewed and edited by Eva)
age_at_death <- function(birthdate, deathdate) {
  floor(interval(birthdate, deathdate) / years(1))
} #creates the function
patients$age_at_time_death <- age_at_death(patients$BIRTHDATE,patients$DEATHDATE) #calculate the age at time of death
# AI: Defines a reusable function to calculate age at death and applies it to every patient.

#determine general demographics and conditions (copilot -> reviewed and edited by Eva)
#
conditions <- dat$conditions.csv
conditions_description_types <- table(conditions$DESCRIPTION)


# NEW SINCE 9.2.26: Merged acute_viral 
# Add age variables to patients
patients <- patients %>%
  mutate(
    BIRTHDATE = as.Date(BIRTHDATE),
    DEATHDATE = as.Date(DEATHDATE),
    age_2025 = 2025 - year(BIRTHDATE),
    age_at_time_death = floor(
      interval(BIRTHDATE, DEATHDATE) / years(1)
    )
  )

# Add patient information and age variables to conditions
conditions <- conditions %>%
  mutate(
    START = as.Date(START)
  ) %>%
  left_join(
    patients %>%
      select(
        Id,
        BIRTHDATE,
        DEATHDATE,
        age_2025,
        age_at_time_death
      ),
    by = c("PATIENT" = "Id")
  ) %>%
  mutate(
    age_at_encounter = time_length(
      interval(BIRTHDATE, START),
      unit = "years"
    )
  )
#Subset acute & viral
merged_acute_viral <- conditions %>%
  filter(grepl("acute|viral", DESCRIPTION, ignore.case = TRUE))


# Acute Viral Pharyngitis ----
temp <- filter(dat$conditions.csv, DESCRIPTION == "Acute viral pharyngitis (disorder)") %>% 
  mutate(month = floor_date(START, unit = "month")) %>% 
  group_by(month) %>% summarize(count=n())
lm(count~month,temp) 
# AI: Fits a linear model to estimate the trend in monthly case counts.

# Top conditions summaries ----
condition_slopes <- mutate(dat$conditions.csv,month = floor_date(START, unit = "month")) %>% 
  group_by(month,CODE,DESCRIPTION) %>% summarize(count=n()) %>% 
  group_by(CODE,DESCRIPTION) %>% filter(year(month)>=2023 &length(unique(month))>10) %>% 
  summarize(events=lm(count~month)$coefficients[2]) %>% arrange(desc(events))
# AI: Calculates the monthly slope for each condition, keeping conditions with sufficient data since 2023.
#arrange() sorts data from by column specified
plot(condition_slopes$events,type="l")
abline(v=25,col="red")
# 25 is reasonable cutoff for increasing incidence of conditions 
top_condition_slopes <- head(condition_slopes, 25)$CODE


# Code/name mapping 
#codemap <- dat$conditions.csv[c("CODE","DESCRIPTION")] %>% unique() %>% # Removes all the duplicate rows 
#  {setNames(.$DESCRIPTION,.$CODE)} # no longer a dataframe, now a vector with names # curly brackets "protect" the . 

codemap <- dat$conditions.csv[c("CODE","DESCRIPTION")] %>% unique() %>% # Removes all the duplicate rows 
  with(setNames(DESCRIPTION,CODE)) # no longer a dataframe, now a vector with names 
# with() turns 1st argument into an environment for searching ease 
# AI: Creates a named vector that can translate condition codes into their descriptions.


# Code co-occurance ----
# add criteria into one filter() argument using %in% %and% %or% etc. and return a T or F for each column as it does or does not fit the criteria
patientcodes <- filter(dat$conditions.csv,CODE %in% top_condition_slopes) %>% 
  distinct(PATIENT,CODE) %>% 
  mutate(PRESENT = 1) %>% 
  pivot_wider(names_from = CODE, values_from = PRESENT, values_fill = 0) # creates table where each patient is a row, columns are CODE, and PRESENT fills in your table values
#select(-PATIENT) # select() behaves more predictably than .[] or .$ callouts
# AI: Creates a patient-by-condition matrix where 1 indicates that a patient has the condition and 0 indicates that they do not.

encountercodes <- filter(dat$conditions.csv,CODE %in% top_condition_slopes) %>% 
  distinct(ENCOUNTER,CODE) %>% 
  mutate(PRESENT = 1) %>% 
  pivot_wider(names_from = CODE, values_from = PRESENT, values_fill = 0) # creates table where each patient is a row, columns are CODE, and PRESENT fills in your table values
# AI: Creates the equivalent condition-presence matrix at the encounter level.

npatients <- nrow(dat$patients.csv) # total no of patients
nencounters <- nrow(dat$encounters.csv)
codecombos <- combn(top_condition_slopes,2,simplify = FALSE)# creates pairwise comparison vectors including every possible combination of 2 codes
# AI: Generates every possible pair of the selected condition codes.


#Creating function to 
# {} take many expressions to return one value 
fn_lift <- function(xx,codesource = patientcodes,denom = npatients){
  counta <- sum(codesource[[xx[1]]]) 
  countb <- sum(codesource[[xx[2]]])
  expected <- counta*countb/denom
  observed <- sum(codesource[[xx[1]]]*codesource[[xx[2]]])
  out <- if(expected == 0){1} else{observed/expected} #if(the thing you want to replace){what you replace that value with} else{what it returns for everything else}
  data.frame(CND_A = xx[],CND_B = rev(xx[]),LIFT = out) #
  }
# [] makes a list within a list [[]]] calls out one value from a list; can be used for data frames too
# AI: Calculates lift, which compares the observed co-occurrence of two conditions with the co-occurrence expected if they were independent.


patient_lift_matrix <- map(codecombos,fn_lift) %>% list_rbind() %>% 
  mutate(CND_A = codemap[CND_A],CND_B = codemap[CND_B]) %>% 
  xtabs(LIFT~CND_A + CND_B,data=.)
# AI: Calculates lift for every condition pair using patients as the unit of analysis and organizes the results into a matrix.

encounter_lift_matrix <- map(codecombos,fn_lift, codesource = encountercodes, denom = nencounters) %>% list_rbind() %>% 
  mutate(CND_A = codemap[CND_A],CND_B = codemap[CND_B]) %>% 
  xtabs(LIFT~CND_A + CND_B,data=.)
#xtabs cross tabulates a data frame, meaning it takes every combination of values 
# AI: Repeats the lift calculation using encounters rather than patients as the unit of analysis.

heatmap(log1p(patient_lift_matrix), symm = T,scale = "none", col=hcl.colors(50, "RdBu", rev=TRUE))
# AI: Displays patient-level lift values as a heatmap after log transformation.

e_heat <- heatmap(log1p(encounter_lift_matrix), symm = T,scale = "none", col=hcl.colors(50, "RdBu", rev=TRUE))
colnames(encounter_lift_matrix)[e_heat$colInd]
