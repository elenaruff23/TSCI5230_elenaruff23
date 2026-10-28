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
systemic_conditions <- colnames(encounter_lift_matrix)[e_heat$colInd][18:25]
local_conditions <- colnames(encounter_lift_matrix)[e_heat$colInd][-(3:5)] %>% setdiff(systemic_conditions)


# Summarize conditions by encounter ----
encounter_summary<- group_by(dat$conditions.csv,ENCOUNTER,PATIENT) %>% 
  summarize(local=length(intersect(local_conditions,DESCRIPTION))
            ,systemic=length(intersect(systemic_conditions,DESCRIPTION))
            ,first_condition_start=min(START,na.rm = T)
            ,last_condition_start=max(START,na.rm = T)
            ,condition_type = case_when(
              local > 0 & systemic > 0 ~ "both",
              local > 0 ~ "local",
              systemic > 0 ~ "systemic",
              TRUE ~ "neither"
                                                       ))
#first_condition_start is a good proxy for encounter date 
subset(encounter_summary,first_condition_start!=last_condition_start & condition_type!="neither")

#Most encounters have more at least 4 different systemic conditions
subset(encounter_summary,systemic > 0)$systemic %>% hist()

# Most encounters have 1 local condition, none have more than 3
subset(encounter_summary,local > 0)$local %>% hist()

sum(encounter_summary$systemic > 0) #no. encounters with systemic conditions
sum(encounter_summary$local > 0) #no. encounters with local conditions
#no encounters have both local and systemic conditions 
sum((encounter_summary$systemic > 0)*(encounter_summary$local > 0))

#contingency table for systemic and local conditions
with(encounter_summary,table(local,systemic))

encounter_summary$condition_type %>% table() %>% prop.table()


# Summarize conditions by encounter ----
encounter_summary<- group_by(dat$conditions.csv,ENCOUNTER,PATIENT) %>% 
  summarize(local=length(intersect(local_conditions,DESCRIPTION))
            ,systemic=length(intersect(systemic_conditions,DESCRIPTION))
            ,first_condition_start=min(START,na.rm = T)
            ,last_condition_start=max(START,na.rm = T)
            ,condition_type = case_when(
              local > 0 & systemic > 0 ~ "both",
              local > 0 ~ "local",
              systemic > 0 ~ "systemic",
              TRUE ~ "neither"
                                                       ))
#first_condition_start is a good proxy for encounter date 
subset(encounter_summary,first_condition_start!=last_condition_start & condition_type!="neither")

#Most encounters have more at least 4 different systemic conditions
subset(encounter_summary,systemic > 0)$systemic %>% hist()

# Most encounters have 1 local condition, none have more than 3
subset(encounter_summary,local > 0)$local %>% hist()

sum(encounter_summary$systemic > 0) #no. encounters with systemic conditions
sum(encounter_summary$local > 0) #no. encounters with local conditions
#no encounters have both local and systemic conditions 
sum((encounter_summary$systemic > 0)*(encounter_summary$local > 0))

#contingency table for systemic and local conditions
with(encounter_summary,table(local,systemic))

encounter_summary$condition_type %>% table() %>% prop.table()

patient_summary <- group_by(encounter_summary,PATIENT) %>% 
  summarize(n_local = sum(local > 0)
            ,n_systemic = sum(systemic > 0)
            ,date_systemic = if(n_systemic > 0){min(first_condition_start[systemic>0])} else {NA}
            ,date_local = if(n_local > 0){min(first_condition_start[local>0])} else {NA}
            ,condition_order = case_when(
              n_local > 0 & n_systemic == 0 ~ "local only",
              n_systemic > 0 & n_local == 0 ~ "systemic only",
              n_local > 0 & n_systemic > 0 & date_local < date_systemic ~ "local first",
              n_local > 0 & n_systemic > 0 & date_systemic < date_local ~ "systemic first",
              n_local > 0 & n_systemic > 0 & date_local == date_systemic ~ "same date",
              TRUE ~ "neither"
            )
            ,interval = as.numeric(difftime(date_systemic, date_local, units = "days"))
            #,debug = {if(n_local+n_systemic > 0){browser()}
            #  0
            #}
)%>%
  left_join(dat$patients.csv,by=c("PATIENT"="Id"))

#period between local and systemic encounters most frequently between 0-20 days
hist(patient_summary$interval,breaks=1000,xlim = c(0,50))

#for next time: make histograms for each variable in patient_summary
#link patient demographics of interest (such as geographical location - narrow to state-level)


#Missed class 10/7/26 - determine order of conditions per patient 
# summarize patient-level patterns
hist(patient_summary$n_local
     ,breaks=30
     ,main="Number of Local Conditions per Patient"
     ,xlab="Number of Local Conditions")

hist(patient_summary$n_systemic
     ,breaks=30
     ,main="Number of Systemic Conditions per Patient"
     ,xlab="Number of Systemic Conditions")

hist(patient_summary$interval
     ,breaks=100
     ,main="Interval Between Systemic and Local Conditions"
     ,xlab="Interval (days)")

barplot(table(patient_summary$condition_order)
        ,main="Condition Order per Patient"
        ,ylab="Number of Patients"
        ,las=2)


#History of disease spread ----

# earliest local and systemic dates in the dataset
patient_summary %>%
  summarize(date_local=min(date_local,na.rm=T)
            ,date_systemic=min(date_systemic,na.rm=T))

# earliest local and systemic dates by state
group_by(patient_summary,STATE) %>%
  summarize(date_local=min(date_local,na.rm=T)
            ,date_systemic=min(date_systemic,na.rm=T)) %>%
  arrange(date_local,date_systemic)

# earliest local and systemic dates by Texas city
subset(patient_summary,STATE=="Texas" & !is.na(date_local)) %>%
  group_by(CITY) %>%
  summarize(date_local=min(date_local,na.rm=T)
            ,date_systemic=min(date_systemic,na.rm=T)) %>%
  arrange(date_local,date_systemic)


#Create local/systemic patient and encounter lists ----
local_encounters <- subset(encounter_summary,local>0)$ENCOUNTER
systemic_encounters <- subset(encounter_summary,systemic>0)$ENCOUNTER
local_patients <- subset(encounter_summary,local>0)$PATIENT %>% unique()
systemic_patients <- subset(encounter_summary,systemic>0)$PATIENT %>% unique()


#Procedures ----
subset(dat$procedures.csv,ENCOUNTER %in% local_encounters)$DESCRIPTION %>%table() %>% sort()
subset(dat$procedures.csv,ENCOUNTER %in% systemic_encounters)$DESCRIPTION %>%table() %>% sort()

#Observations----
subset(dat$observations.csv,ENCOUNTER %in% local_encounters)$DESCRIPTION %>%table() %>% sort()
subset(dat$observations.csv,ENCOUNTER %in% systemic_encounters)$DESCRIPTION %>%table() %>% sort()

#Number of records within each encounter for local and systemic conditions 
subset(dat$observations.csv,ENCOUNTER %in% local_encounters) %>% nrow()


#Creating a function to count number of rows local and systemic encounters per dataframe in dat
fn_rows_encounters <- function(xx)
{summarize(
  dat[[xx]],
  source = xx,
  rows=n(),
  rows_local = sum(ENCOUNTER %in% local_encounters,na.rm=T),
  rows_systemic = sum(ENCOUNTER %in% systemic_encounters,na.rm=T),
  encounters = length(unique(ENCOUNTER)),
  encounters_local = length(intersect(ENCOUNTER,local_encounters)),
  encounters_systemic = length(intersect(ENCOUNTER,systemic_encounters)),
  rows_per_encounter = rows/encounters, 
  rows_per_encounter_local = rows_local/encounters_local,
  rows_per_encounter_systemic = rows_systemic/encounters_systemic
    )}

names(dat) %>% setdiff(c("encounters.csv","patients.csv")) %>% sapply(fn_rows_encounters,simplify = FALSE) %>% bind_rows()


#Creating a function to count number of rows local and systemic patient phenotypes per dataframe in dat
fn_rows_patients <- function(xx)
{summarize(
  dat[[xx]],
  source = xx,
  rows=n(),
  rows_local = sum(PATIENT %in% local_patients,na.rm=T),
  rows_systemic = sum(PATIENT %in% systemic_patients,na.rm=T),
  patients = length(unique(PATIENT)),
  patients_local = length(intersect(PATIENT,local_patients)),
  patients_systemic = length(intersect(PATIENT,systemic_patients)),
  rows_per_patients = rows/patients, 
  rows_per_patient_local = rows_local/patients_local,
  rows_per_patient_systemic = rows_systemic/patients_systemic
)}

names(dat) %>% setdiff(c("encounters.csv","patients.csv")) %>% sapply(fn_rows_patients,simplify = FALSE) %>% bind_rows()

#standardize dataframes (except patients.csv - lacks same column naming system)
transmute(dat$medications.csv,PATIENT, ENCOUNTER, CODE, DESCRIPTION, source = "medications") %>% filter(!is.na(CODE)) %>% View()
transmute(dat$medications.csv,PATIENT, ENCOUNTER, CODE = REASONCODE, DESCRIPTION = REASONDESCRIPTION, source = "medications_reason") %>% filter(!is.na(CODE)) %>% View()

fn_standardize <- function(xx){
  if("SSN" %in% names(xx)){return(NULL)}
  if("Id" %in% names(xx) & !"ENCOUNTER"%in% names(xx)){xx <- mutate(xx,ENCOUNTER = Id)}
  out <- transmute(xx,PATIENT, ENCOUNTER, CODE = as.character(CODE), DESCRIPTION, subtype = "CODE") %>% filter(!is.na(CODE))
  if(length(intersect(colnames(xx),c("REASONCODE","REASONDESCRIPTION"))) == 2){
    out <- bind_rows(out,transmute(xx,PATIENT, ENCOUNTER, 
                                   CODE = as.character(REASONCODE), DESCRIPTION = REASONDESCRIPTION, subtype = "REASON") %>% filter(!is.na(CODE)))
    }
  #browser(),
  return(out)
}

#Test fn_standardize
#fact_table <- fn_standardize(dat$encounters.csv)

#Apply fn_standardize to all dataframes in dat
fact_table <- sapply(dat,fn_standardize,simplify = FALSE) %>% bind_rows(.id = "type") %>% 
  filter(ENCOUNTER %in% dat$encounters.csv$Id)

#Code from 10/21 (out sick) 
# Determine the number of encounters with a specified code "agitation"
summarise(fact_table, encounter_no = length(unique(ENCOUNTER)),
          encounter_w_code = length(unique(ENCOUNTER[CODE == "24199005"])),
          systemic = length(systemic_encounters),
          systemic_w_code = length(intersect(ENCOUNTER[CODE == "24199005"],systemic_encounters)))

# Create a function to determine number of encounters for each code (medication, allergy, care plan, etc.)
fn_encounter_codes <- function(xx){
  encounter_w_code <- length(unique(fact_table$ENCOUNTER[fact_table$CODE == xx]))
  systemic_w_code <- length(intersect(fact_table$ENCOUNTER[fact_table$CODE == xx],systemic_encounters))
  out <- c(systemic_w_code = systemic_w_code, encounter_w_code = encounter_w_code)
  return(out)
}

# test if fn_encounter_codes works on a test code (agitation)
fn_encounter_codes("24199005")

# Apply fn_encounter_codes to all unique codes from fact_table - Version 1, SLOWER
#encounter_code_tallies <- sapply(unique(fact_table$CODE), fn_encounter_codes, simplify=FALSE) %>% bind_rows

#10/28 
# Create a table of number of encoutners for each code (Generated by Microsoft Copilot Basic) FASTER than encounter_code_tallies
encounter_code_tallies_2 <- fact_table %>%
  group_by(CODE,DESCRIPTION,type,subtype) %>%
  summarise(encounter_w_code = n_distinct(ENCOUNTER),
            systemic_w_code = sum(unique(ENCOUNTER) %in% systemic_encounters),
            local_w_code = sum(unique(ENCOUNTER) %in% local_encounters)
  ) %>% 
  mutate(total_encounters = nrow(dat$encounters.csv),
         total_systemic_encounters = length(systemic_encounters),
         total_local_encounters = length(local_encounters),
         fraction_total_w_code = encounter_w_code/total_encounters,
         fraction_systemic_w_code = systemic_w_code/total_systemic_encounters,
         fraction_local_w_code = local_w_code/total_local_encounters,
         local_no_code = total_local_encounters - local_w_code,
         notlocal_w_code = encounter_w_code - local_w_code,
         notlocal_no_code = total_encounters - encounter_w_code - total_local_encounters + local_w_code,
         ## Haldane correction
         H_local_w_code = local_w_code + 0.5,
         H_local_no_code = local_no_code + 0.5,
         H_notlocal_w_code = notlocal_w_code + 0.5,
         H_notlocal_no_code = notlocal_no_code + 0.5,
         ##Proportions 
         local_code_prevalence = H_local_w_code/(H_local_w_code + H_local_no_code),
         notlocal_code_absence = H_notlocal_no_code/(H_notlocal_w_code + H_notlocal_no_code),
         local_likelihood_ratio = local_code_prevalence/(1 - notlocal_code_absence),
         local_OR = (H_local_w_code * H_notlocal_no_code)/(H_local_no_code * H_notlocal_w_code)
         ) %>% arrange(desc(local_likelihood_ratio))

#Contingency table to identify likelihood ratio of a local encounter vs non local encounter to have any particular code
# |           | Has Code                              | No Code                                                      |
# |-----------|--------------------------------------|-------------------------------------------------------------|
# | Is Local  | local_w_code                         | local_no_code = total_local_encounters - local_w_code       |
# | Not Local | notlocal_w_code = encounter_w_code - local_w_code | notlocal_no_code = total_encounters - encounter_w_code - total_local_encounters + local_w_code |

filter(encounter_code_tallies_2, !DESCRIPTION %in% local_conditions) %>% 
  select(CODE, DESCRIPTION, type, subtype, encounter_w_code, local_w_code, local_likelihood_ratio, local_OR)

#All encounters vs encounter_code_tallies_2 total encounters
# n_distinct(fact_table$ENCOUNTER)
# n_distinct(dat$encounters.csv$Id)
# encounters_differ <- setdiff(fact_table$ENCOUNTER,c(dat$encounters.csv$Id,""))



## Homework Assignment: 
## 1. copy local calculations for systemic encounters 
## 2. tallies group by patient 


