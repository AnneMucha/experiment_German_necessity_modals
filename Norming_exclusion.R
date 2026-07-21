library(plyr)
library(purrr)
library(readr)

# read formatted lists

list1_results <- read.csv("norming1_results_formatted.csv")
list2_results <- read.csv("norming2_results_formatted.csv")

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

# exclude 2 participants from group 1: 6/7 bad controls rated higher than 4
list1_exclusion <- filter(list1_results, PROLIFIC_ID != "69fcc39ebdffca0a95dc1986" & PROLIFIC_ID != "601b1acc67409b421d50f9bc")

# also check for incomplete data:
participant_counts1 <- list1_exclusion %>%
  group_by(PROLIFIC_ID) %>%
  summarise(n_rows = n()) %>%
  arrange(n_rows)

View(participant_counts1)

View(list1_exclusion)

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

# exclude 1 participant from group 2: 6 good controls higher than 4
list2_exclusion <- filter(list2_results, PROLIFIC_ID != "6a0ab30f4426651bf3712046")

View(list2_exclusion)

# combine the 4 lists
results_norming_complete <- rbind(list1_exclusion, list2_exclusion)
View(results_norming_complete)


q()

