##Natalie Daniels
##Data wrangling for PP
##Oct. 9 2026

##Midterm
##Replicate Figures 1 and 2 from Bartels (2000), “Partisanship and Voting 
##Behavior”, for the years 2006–2024 using the Cumulative CCES Dataset.
##replicating figure 2 in this script

#Figure 2! 

#1. combine variables
ces <- ces %>%
  mutate(
    # Generate a dummy variable for all party identifiers, strong or weak
    identifiers = strong + weak,
    
    # Variable equals "identifiers" for voters (voted==2), missing otherwise
    identvote = if_else(voted == 2, identifiers, NA),
    
    # Variable equals "identifiers" for nonvoters (voted==1), missing otherwise
    identnovote = if_else(voted == 1, identifiers, NA),
    
    # Simple turnout variable: recode voted (1=0 nonvoter, 2=1 voter, else missing)
    turnout = case_when(
      voted == 1 ~ 0,
      voted == 2 ~ 1,
    )
  )

#2. declare and colapse some data
install.packages("srvyr")
library(srvyr)

ces_svy <- ces %>%
  as_survey_design(weights = weight)

figure1_cumul <- ces_svy %>%
  filter(year > 1950) %>%
  group_by(year) %>%
  summarise(
    strong  = survey_mean(strong, na.rm = TRUE, vartype = NULL),
    weak    = survey_mean(weak,   na.rm = TRUE, vartype = NULL),
    leaners = survey_mean(leaners,na.rm = TRUE, vartype = NULL),
    indeps  = survey_mean(indeps, na.rm = TRUE, vartype = NULL),
  )

###find varable 
names(ces)[grepl("turnout|vote|voted", names(ces), ignore.case = TRUE)]
table(ces$voted_turnout_self, useNA = "ifany")

figure2_cumul <- ces_svy %>%
  filter(year > 2006) %>%
  group_by(year, vote) %>%
  summarise(
    identifiers = survey_mean(identifiers, na.rm = TRUE, vartype = NULL)
  )