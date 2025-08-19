library(plyr)
library(purrr)
library(readr)

# read formatted lists

listA_results <- read.csv("listA_results_formatted.csv")
listB_results <- read.csv("listB_results_formatted.csv")
listC_results <- read.csv("listC_results_formatted.csv")
listD_results <- read.csv("listD_results_formatted.csv")


# --------- Data cleaning: attention checks -------------------------

# filter for SPR attention checks (by list, to see if the groups are still balanced)

library(dplyr)
listA_attention_SPR <- filter(listA_results, PennElementType == "Controller-Question" & Label == "SPR-trial-attention")

# count number of correct answers by participant (values in "Reading.time" column for questions), threshold at 18 correct answers

listA_attention_SPR_points <- listA_attention_SPR %>% group_by(PROLIFIC_ID) %>%
  summarise(count_ones = sum(Reading.time == 1, na.rm = TRUE))

View(listA_attention_SPR_points) # all participants passed attention checks


listB_attention_SPR <- filter(listB_results, PennElementType == "Controller-Question" & Label == "SPR-trial-attention")

listB_attention_SPR_points <- listB_attention_SPR %>% group_by(PROLIFIC_ID) %>%
  summarise(count_ones = sum(Reading.time == 1, na.rm = TRUE))

View(listB_attention_SPR_points) # all participants passed attention checks

listC_attention_SPR <- filter(listC_results, PennElementType == "Controller-Question" & Label == "SPR-trial-attention")

listC_attention_SPR_points <- listC_attention_SPR %>% group_by(PROLIFIC_ID) %>%
  summarise(count_ones = sum(Reading.time == 1, na.rm = TRUE))

View(listC_attention_SPR_points) # all participants passed attention checks

listD_attention_SPR <- filter(listD_results, PennElementType == "Controller-Question" & Label == "SPR-trial-attention")

listD_attention_SPR_points <- listD_attention_SPR %>% group_by(PROLIFIC_ID) %>%
  summarise(count_ones = sum(Reading.time == 1, na.rm = TRUE))

View(listD_attention_SPR_points) # all participants passed attention checks


# participant exclusion based on acceptability judgments (by list) --------------------------------------------

# filter for acceptability judgments for control items
A_accept_controls <- filter(listA_results, PennElementName == "AcceptabilityJudgment" & experiment == "control" & Value != "NULL")
B_accept_controls <- filter(listB_results, PennElementName == "AcceptabilityJudgment" & experiment == "control" & Value != "NULL")
C_accept_controls <- filter(listC_results, PennElementName == "AcceptabilityJudgment" & experiment == "control" & Value != "NULL")
D_accept_controls <- filter(listD_results, PennElementName == "AcceptabilityJudgment" & experiment == "control" & Value != "NULL")

#--------------- check for participants to be excluded by the acceptability experiment (by list) ("parameter" column contains difference between "bad" vs. "good" controls)

A_accept_controls_grouped <- A_accept_controls %>% group_by(PROLIFIC_ID) %>% group_by(parameter) %>% select(PROLIFIC_ID, parameter, Value)

# count good and bad controls on the wrong side of the scale for each participant

A_controls_check <- A_accept_controls_grouped %>%
  group_by(PROLIFIC_ID) %>%
  summarise(
    bad_above_4 = sum(parameter == "bad" & Value %in% c("5","6","7"), na.rm = TRUE),
    good_below_4 = sum(parameter == "good" & Value %in% c("1","2","3"), na.rm = TRUE)
  )

View(A_controls_check)

# exclude 1 participant from group A: 6 bad controls rated higher than 4

listA_exclusion <- filter(listA_results, PROLIFIC_ID != "6015d2a02ab6d07215f2cc0a")

# group B:

B_accept_controls_grouped <- B_accept_controls %>% group_by(PROLIFIC_ID) %>% group_by(parameter) %>% select(PROLIFIC_ID, parameter, Value)

B_controls_check <- B_accept_controls_grouped %>%
  group_by(PROLIFIC_ID) %>%
  summarise(
    bad_above_4 = sum(parameter == "bad" & Value %in% c("5","6","7"), na.rm = TRUE),
    good_below_4 = sum(parameter == "good" & Value %in% c("1","2","3"), na.rm = TRUE)
  )

View(B_controls_check)

# exclude 1 participant from group B: 7 bad controls higher than 4

listB_exclusion <- filter(listB_results, PROLIFIC_ID != "5ea5d79c37cb2805c2e6f08e")

# group C:

C_accept_controls_grouped <- C_accept_controls %>% group_by(PROLIFIC_ID) %>% group_by(parameter) %>% select(PROLIFIC_ID, parameter, Value)

C_controls_check <- C_accept_controls_grouped %>%
  group_by(PROLIFIC_ID) %>%
  summarise(
    bad_above_4 = sum(parameter == "bad" & Value %in% c("5","6","7"), na.rm = TRUE),
    good_below_4 = sum(parameter == "good" & Value %in% c("1","2","3"), na.rm = TRUE)
  )

View(C_controls_check)

# exclude 1 participant from group C: 5 bad controls higher than 4	

listC_exclusion <- filter(listC_results, PROLIFIC_ID != "6107ab75c50cfb94fb523279")

# group C:

D_accept_controls_grouped <- D_accept_controls %>% group_by(PROLIFIC_ID) %>% group_by(parameter) %>% select(PROLIFIC_ID, parameter, Value)

D_controls_check <- D_accept_controls_grouped %>%
  group_by(PROLIFIC_ID) %>%
  summarise(
    bad_above_4 = sum(parameter == "bad" & Value %in% c("5","6","7"), na.rm = TRUE),
    good_below_4 = sum(parameter == "good" & Value %in% c("1","2","3"), na.rm = TRUE)
  )

View(D_controls_check)

# exclude 1 participant from group D: 5 bad controls higher than 4

listD_exclusion <- filter(listD_results, PROLIFIC_ID != "60fd55ed59e7795b518f7434")

# combine the 4 lists
results_complete <- rbind(listA_exclusion, listB_exclusion, listC_exclusion, listD_exclusion)
View(results_complete)

# write a new csv file with the complete data

write.csv(results_complete,"/Users/amucha/OneDrive - University of Edinburgh/project_multilingualism/results_native_speakers/results_native_complete.csv", row.names = FALSE)


q()

