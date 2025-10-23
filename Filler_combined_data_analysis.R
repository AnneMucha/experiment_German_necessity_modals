library(readr)
library(dplyr)
library(ggplot2)
library(lme4)

# read and inspect cleaned data frame for L2 speakers
L2_data <- read.csv("L2_data_after_exclusion.csv", encoding = "UTF-8")
View (L2_data)

# add columns for demographic data
library(tidyr)
L2_data_background <- L2_data %>%
  filter(controller_type %in% c("age", "sex", "abroad-choice", "years-choice")) %>%  
  pivot_wider(
    id_cols = ID,
    names_from = controller_type,
    values_from = word_or_rating
  )

# join dfs back together
L2_data_complete <- L2_data %>%
  left_join(L2_data_background, by = "ID")

View(L2_data_complete)


# read and inspect data from native speakers and add demographic info
native_data <- read.csv("results_native_complete.csv", encoding = "UTF-8")
View(native_data)

native_data_background <- native_data %>%
  filter(PennElementName %in% c("age", "sex", "abroad-choice", "dialect-choice", "languages-choice")) %>%  
  pivot_wider(
    id_cols = PROLIFIC_ID,
    names_from = PennElementName,
    values_from = Value
  )

# join dfs back together
native_data_complete <- native_data %>%
  left_join(native_data_background, by = "PROLIFIC_ID")

View(native_data_complete)


# separate rating data
rating_data_L2 <- filter(L2_data_complete, controller_type == "AcceptabilityJudgment" & word_or_rating != "NULL")

rating_data_native <- filter(native_data_complete, PennElementName == "AcceptabilityJudgment" & Value != "NULL")

# read SPR data from csv files generated in data cleaning process
SPR_data_native <- read.csv("SPR_cleaned_native.csv", encoding = "UTF-8")

SPR_data_L2 <- read.csv("SPR_L2_no_threshold.csv", encoding = "UTF-8") 


# select columns and unify column names for the rating data -----------------------------------
library(plyr)
rating_data_native <- plyr::rename(rating_data_native, c("Order.number.of.item" = "order_number", "Label"="trial_type", "Value" = "rating", "PROLIFIC_ID" = "ID",
                                                         "experiment"="item_type", "item"="item_name", "parameter"="flavor"))

rating_data_L2 <- plyr::rename(rating_data_L2, c("word_or_rating" = "rating"))

# add a column for speaker_group
rating_data_native$speaker_group <- "Native speakers"
rating_data_L2$speaker_group <- "L2 speakers"


# select columns for binding --------------------------------------------
library(dplyr)
rating_native_reduced <- select(rating_data_native, c("order_number", "trial_type", "rating", "ID", "group", "item_type", "item_name", "condition", "modal", "flavor", "speaker_group"))

rating_L2_reduced <- select(rating_data_L2, c("order_number", "trial_type", "rating", "ID", "group", "item_type", "item_name", "condition", "modal", "flavor", "speaker_group"))

rating_data_combined <- rbind(rating_native_reduced, rating_L2_reduced)

View(rating_data_combined) 

# transform rating data, filter for filler experiment
rating_data_combined$rating <- as.numeric(as.character(rating_data_combined$rating))
rating_data_combined_filler <- filter(rating_data_combined, item_type == "filler")

View(rating_data_combined_filler)

# turn speaker group into a factor
rating_data_combined_filler$speaker_group <- factor(rating_data_combined_filler$speaker_group, levels = c("Native speakers", "L2 speakers"))

detach("package:plyr", unload = TRUE)  # run if 'summarise' function is masked by plyr
library(dplyr)

# aggregate rating data 
rating_combined_plot_filler <- rating_data_combined_filler %>%                     
  group_by(condition, speaker_group) %>%                           
  summarise(                                             
    M  = mean(rating, na.rm = TRUE),             
    N  = n(),                                            
    SE = sd(rating, na.rm = TRUE) / sqrt(N),
    .groups = "drop"
  )

View(rating_combined_plot_filler)

# plot rating data
plot_combined_ratings_filler2 <- ggplot(rating_combined_plot_filler, aes(x = speaker_group, y = M, group = condition)) +
  geom_line(aes(linetype = condition)) +
  geom_point(aes(shape = condition), size = 3) +
  geom_errorbar(aes(ymin = M - SE, ymax = M + SE), width = 0.2) +
  labs(
    x = "Speaker group",
    y = "Mean Acceptability Rating",
    shape = "condition",
    linetype = "condition"
  ) +
  coord_cartesian(ylim = c(1, 7)) +
  theme_bw(base_size = 18)

plot_combined_ratings_filler2


# model the combined rating data ----------------------------------------------
# define factors
rating_data_combined_filler$rating <- factor(rating_data_combined_filler$rating)
rating_data_combined_filler$condition <- factor(rating_data_combined_filler$condition)

library(ordinal)

# fit model with maximal random effects structure
model_compare_filler <- clmm(rating ~ condition * speaker_group +
                        (1 + condition | ID) +
                        (1 + condition | item_name),
                      data = rating_data_combined_filler)


summary(model_compare_filler) 
confint(model_compare_filler, method="Wald") 

# analyse self-paced reading data ----------------------------------------------

# unify column names
SPR_data_native <- plyr::rename(SPR_data_native, c("Order.number.of.item" = "order_number", "Label"="trial_type", "Value" = "word", "PROLIFIC_ID" = "ID",
                                                   "experiment"="item_type", "item"="item_name", "parameter"="flavor", "Sentence..or.sentence.MD5."="sentence","Reading.time"="RT"))

SPR_data_L2 <- plyr::rename(SPR_data_L2, c("word_or_rating"= "word"))

# add column for speaker group
SPR_data_native$speaker_group <- "Native speakers"
SPR_data_L2$speaker_group <- "L2 speakers"


# select columns for reduced df
SPR_native_reduced <- select(SPR_data_native, c("order_number","trial_type","ID", "group","item_type", "item_name", "condition", "modal", "flavor","region","word", "RT", 
                                                "sentence","mean_rt", "sd_rt_3","word_length", "predicted_RT","residuals", "speaker_group"))

SPR_L2_reduced <- select(SPR_data_L2, c("order_number","trial_type","ID", "group","item_type", "item_name", "condition", "modal", "flavor","region","word", "RT", 
                                        "sentence","mean_rt", "sd_rt_3","word_length", "predicted_RT","residuals", "speaker_group"))

# combine and inspect data from speaker groups
SPR_data_combined <- rbind(SPR_native_reduced, SPR_L2_reduced)

View(SPR_data_combined)

# analyse differences in SPR data----------------------------------------------
# z-transform RTs by participant to remove baseline speed differences 
SPR_data_z <- SPR_data_combined %>%
  group_by(ID) %>%
  mutate(RT_z = as.numeric(scale(RT)))

# make sure that RT_z is a vector
str(SPR_data_z$RT_z)
View(SPR_data_z$RT_z)

# select data from filler experiment
SPR_data_z_filler <- SPR_data_z %>% filter(item_type == "filler")

# plot z-transformed reading times for both speaker groups
str(SPR_data_z_filler)

# define factors
SPR_data_z_filler$speaker_group <- factor(SPR_data_z_filler$speaker_group, levels=c("Native speakers", "L2 speakers"))
SPR_data_z_filler$condition <- factor(SPR_data_z_filler$condition)
SPR_data_z_filler$region <- factor(SPR_data_z_filler$region, levels = c("intro", "subject", "modal", "MF1", "MF2", "verb"))

# aggregate and plot 
agg_data_z_filler <- SPR_data_z_filler %>%
  group_by(region, condition, speaker_group) %>%
  summarise(mean_RT_z = mean(RT_z),
            se = sd(RT_z, na.rm = TRUE) / sqrt(n()),  
            .groups = "drop")

plot_RT_z_groups_filler <- ggplot(agg_data_z_filler, aes(x = region, y = mean_RT_z, group = condition, color = condition)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = mean_RT_z - se, ymax = mean_RT_z + se), width = 0.2) +
  facet_wrap(~ speaker_group) +
  labs(
    title = "Mean z-scored Reading Times Across Regions, Conditions and Groups",
    x = "Region",
    y = "Mean z-scored Reading Time",
    color = "Condition"
  ) 

plot_RT_z_groups_filler

# model z-scored RTs across groups for all critical regions ------------------------
# each selected model includes the maximal random effects structure supported by the data (see Barr et al., 2013; Matuschek et al., 2017 for model reduction techniques) 

# model for region "modal":

# filter data
SPR_data_z_filler_modal <- SPR_data_z_filler %>% filter(region == "modal")

# selected model:
model_SPR_modal_filler <- lmer(RT_z ~ condition * speaker_group +
                                   (1 | ID) +
                                   (1 + condition | item_name),
                                 data = SPR_data_z_filler_modal)


summary(model_SPR_modal_filler) 
confint(model_SPR_modal_filler, method = "Wald") 

# no indication of interaction or condition-effect

# model for region "MF1":

# filter data
SPR_data_z_filler_MF1 <- SPR_data_z_filler %>% filter(region == "MF1")

# selected model:
model_SPR_MF1_filler <- lmer(RT_z ~ condition * speaker_group +
                                 (1 | ID) +
                                 (1 + condition | item_name),
                               data = SPR_data_z_filler_MF1)


summary(model_SPR_MF1_filler) 
confint(model_SPR_MF1_filler, method = "Wald")

# suggests main effect of speaker group (t = 3.537) and condition (t = -2.807)

# model comparison to test for main effect of condition
model_SPR_MF1_filler_noint <- lmer(RT_z ~ condition + speaker_group +
                                       (1 | ID) +
                                       (1 + condition | item_name),
                                     data = SPR_data_z_filler_MF1)

model_SPR_MF1_filler_nocond <- lmer(RT_z ~ speaker_group +
                                     (1 | ID) +
                                     (1 + condition | item_name),
                                   data = SPR_data_z_filler_MF1)

anova(model_SPR_MF1_filler_noint, model_SPR_MF1_filler_nocond) # ** main effect of condition
anova(model_SPR_MF1_filler, model_SPR_MF1_filler_noint) # confirms no interaction

# model for region "MF2":

# filter data
SPR_data_z_filler_MF2 <- SPR_data_z_filler %>% filter(region == "MF2")

# selected model:
model_SPR_MF2_filler <- lmer(RT_z ~ condition * speaker_group +
                               (1 | ID) +
                               (1 | item_name),
                             data = SPR_data_z_filler_MF2)


summary(model_SPR_MF2_filler) 
confint(model_SPR_MF2_filler, method = "Wald") 

# suggests main effect of speaker group (t = 4.788) but not condition (t = -1.292)

# model comparison to test for main effect of condition
model_SPR_MF2_filler_noint <- lmer(RT_z ~ condition + speaker_group +
                                     (1 | ID) +
                                     (1 | item_name),
                                   data = SPR_data_z_filler_MF2)

model_SPR_MF2_filler_nocond <- lmer(RT_z ~ speaker_group +
                                      (1 | ID) +
                                      (1 | item_name),
                                    data = SPR_data_z_filler_MF2)

anova(model_SPR_MF2_filler_noint, model_SPR_MF2_filler_nocond) 
# model comparison gives weak evidence for an effect of condition (p = 0.03873)


# model for region "verb":

# filter data
SPR_data_z_filler_verb <- SPR_data_z_filler %>% filter(region == "verb")

# selected model:
model_SPR_verb_filler <- lmer(RT_z ~ condition * speaker_group +
                               (1 | ID) +
                               (1 + condition | item_name),
                             data = SPR_data_z_filler_verb)


summary(model_SPR_verb_filler) 
confint(model_SPR_verb_filler, method = "Wald") 

# no effects
#-----------------------------------------------------------------------------------------

# model MF1 and MF2 data for speaker groups separately with z-scored RTs (sanity check for models with raw RTs)


# MF1
# native
# filter data
SPR_data_z_filler_MF1_native <- SPR_data_z_filler_MF1 %>% filter(speaker_group == "Native speakers")
View(SPR_data_z_filler_MF1_native)

# selected model:
model_SPR_MF1_filler_native <- lmer(RT_z ~ condition +
                               (1 + condition | ID) +
                               (1 + condition | item_name),
                             data = SPR_data_z_filler_MF1_native)

summary(model_SPR_MF1_filler_native) 
confint(model_SPR_MF1_filler_native, method = "Wald") 

# model comparison

model_SPR_MF1_filler_native_red <- lmer(RT_z ~
                                      (1 + condition | ID) +
                                      (1 + condition | item_name),
                                    data = SPR_data_z_filler_MF1_native)

anova(model_SPR_MF1_filler_native, model_SPR_MF1_filler_native_red) 

# confirms effect of condition in MF1 for native speakers

# L2
# filter data
SPR_data_z_filler_MF1_L2 <- SPR_data_z_filler_MF1 %>% filter(speaker_group == "L2 speakers")
View(SPR_data_z_filler_MF1_L2)

# selected model:
model_SPR_MF1_filler_L2 <- lmer(RT_z ~ condition +
                                      (1 | ID) +
                                      (1 + condition | item_name),
                                    data = SPR_data_z_filler_MF1_L2)

summary(model_SPR_MF1_filler_L2) 
confint(model_SPR_MF1_filler_L2, method = "Wald") 

# model comparison
model_SPR_MF1_filler_L2_red <- lmer(RT_z ~
                                  (1 | ID) +
                                  (1 + condition | item_name),
                                data = SPR_data_z_filler_MF1_L2)

anova(model_SPR_MF1_filler_L2, model_SPR_MF1_filler_L2_red) 

# confirms effect of condition in MF1 for L2 speakers

#-----------------------

# MF 2
# native
# filter data
SPR_data_z_filler_MF2_native <- SPR_data_z_filler_MF2 %>% filter(speaker_group == "Native speakers")
View(SPR_data_z_filler_MF2_native)

# selected model:
model_SPR_MF2_filler_native <- lmer(RT_z ~ condition +
                               (1 | ID) +
                               (1 | item_name),
                             data = SPR_data_z_filler_MF2_native)

summary(model_SPR_MF2_filler_native) 
confint(model_SPR_MF2_filler_native, method = "Wald") 

# model comparison
model_SPR_MF2_filler_native_red <- lmer(RT_z ~
                                      (1 | ID) +
                                      (1 | item_name),
                                    data = SPR_data_z_filler_MF2_native)

anova(model_SPR_MF2_filler_native, model_SPR_MF2_filler_native_red) 

# confirms no effect of condition in MF2

# L2 
SPR_data_z_filler_MF2_L2 <- SPR_data_z_filler_MF2 %>% filter(speaker_group == "L2 speakers")
View(SPR_data_z_filler_MF2_L2)

# selected model (only model w/o random slopes fits):
model_SPR_MF2_filler_L2 <- lmer(RT_z ~ condition +
                                      (1 + condition | ID) +
                                      (1 | item_name),
                                    data = SPR_data_z_filler_MF2_L2)

summary(model_SPR_MF2_filler_L2) 
confint(model_SPR_MF2_filler_L2, method = "Wald") 

# model comparison
model_SPR_MF2_filler_L2_red <- lmer(RT_z ~
                                  (1 + condition | ID) +
                                  (1 | item_name),
                                data = SPR_data_z_filler_MF2_L2)

anova(model_SPR_MF2_filler_L2, model_SPR_MF2_filler_L2_red) 

# confirms no effect of condition in MF2


q()
