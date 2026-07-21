library(plyr)
library(purrr)
library(readr)

# read formatted lists
list1_results <- read.csv("norming1_report_formatted.csv")
list2_results <- read.csv("norming2_report_formatted.csv")

View(list1_results)
View(list2_results)


# participant exclusion based on controls (by list) --------------------------------------------
library(dplyr)

# filter for acceptability judgments for control items
accept_controls_1 <- filter(list1_results, PennElementName == "AcceptabilityJudgment" & experiment == "control" & Value != "NULL")
accept_controls_2 <- filter(list2_results, PennElementName == "AcceptabilityJudgment" & experiment == "control" & Value != "NULL")

View(accept_controls_1)
View(accept_controls_2)

#--------------- check for participants to be excluded by the controls (by list) ("parameter" column contains difference between "bad" vs. "good" controls)

accept_controls_1_grouped <- accept_controls_1 %>% group_by(PROLIFIC_ID) %>% group_by(parameter) %>% select(PROLIFIC_ID, parameter, Value)
View(accept_controls_1_grouped)

# count good and bad controls on the wrong side of the scale for each participant
controls_check_1 <- accept_controls_1_grouped %>%
  group_by(PROLIFIC_ID) %>%
  summarise(
    bad_above_4 = sum(parameter == "bad" & Value %in% c("5","6","7"), na.rm = TRUE),
    good_below_4 = sum(parameter == "good" & Value %in% c("1","2","3"), na.rm = TRUE)
  )

View(controls_check_1)

# exclude 4 participants from group 1: 6/5 bad controls rated higher than 4, 6/5 good controls lower than 4
list1_exclusion <- filter(list1_results, PROLIFIC_ID != "6a03127a82779752ae63ccfa" & 
                            PROLIFIC_ID != "6a0dfd4e2c59a32d8302ff80" &
                            PROLIFIC_ID != "6a07f838da6095b2fc947554" & 
                            PROLIFIC_ID != "6a21fad0ce86b4bb93d7a748")

# also check for incomplete data:
participant_counts1 <- list1_exclusion %>%
  group_by(PROLIFIC_ID) %>%
  summarise(n_rows = n()) %>%
  arrange(n_rows)

View(participant_counts1)

View(list1_exclusion)
n_distinct(list1_exclusion$PROLIFIC_ID)

# group 2:
accept_controls_2_grouped <- accept_controls_2 %>% group_by(PROLIFIC_ID) %>% group_by(parameter) %>% select(PROLIFIC_ID, parameter, Value)
View(accept_controls_2_grouped)

controls_check_2 <- accept_controls_2_grouped %>%
  group_by(PROLIFIC_ID) %>%
  summarise(
    bad_above_4 = sum(parameter == "bad" & Value %in% c("5","6","7"), na.rm = TRUE),
    good_below_4 = sum(parameter == "good" & Value %in% c("1","2","3"), na.rm = TRUE)
  )

View(controls_check_2)

# exclude 4 participants from group 2: 7/5 good controls lower than 4
list2_exclusion <- filter(list2_results, PROLIFIC_ID != "6a08230310d145317709c4e0" & 
                            PROLIFIC_ID != "69810502a84beb62c27c3c8b" &
                            PROLIFIC_ID != "69fdac828dbce0f87440c60c" & 
                            PROLIFIC_ID != "6a23f33c73eda715f1c22e64")

View(list2_exclusion)

n_distinct(list2_exclusion$PROLIFIC_ID) 

# combine the lists
results_norming_complete <- rbind(list1_exclusion, list2_exclusion)
View(results_norming_complete)
n_distinct(results_norming_complete$PROLIFIC_ID) 

q()

