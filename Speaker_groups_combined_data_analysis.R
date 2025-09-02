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


# separate rating data ----------------
rating_data_L2 <- filter(L2_data_complete, controller_type == "AcceptabilityJudgment" & word_or_rating != "NULL")

rating_data_native <- filter(native_data_complete, PennElementName == "AcceptabilityJudgment" & Value != "NULL")

# read SPR data from csv files generated in data cleaning process
SPR_data_native <- read.csv("SPR_cleaned_native.csv", encoding = "UTF-8")

SPR_data_L2_threshold <- read.csv("SPR_cleaned_L2.csv", encoding = "UTF-8") # SPR data with threshold at 6000ms

SPR_data_L2 <- read.csv("SPR_L2_no_threshold.csv", encoding = "UTF-8") # SPR data w/o upper threshold (used for data analysis)


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

View(rating_data_combined) # still contains practice items, controls and fillers

# plot for visual comparison of L1 and L2 ratings ------------------------------
rating_data_combined$rating <- as.numeric(as.character(rating_data_combined$rating))
rating_data_combined_main <- filter(rating_data_combined, item_type == "main")

View(rating_data_combined_main)

rating_data_combined_main$speaker_group <- factor(rating_data_combined_main$speaker_group, levels = c("Native speakers", "L2 speakers"))

# aggregate data and plot acceptability ratings-----------------------------------
detach("package:plyr", unload = TRUE)  # if 'summarise' function masked by plyr
library(dplyr)

data_rating_combined_plot <- rating_data_combined_main %>%                     
  group_by(flavor, modal, speaker_group) %>%                           
  summarise(                                             
    M  = mean(rating, na.rm = TRUE),             
    N  = n(),                                            
    SE = sd(rating, na.rm = TRUE) / sqrt(N),
    .groups = "drop"
  )

View(data_rating_combined_plot)


plot_combined_ratings <- ggplot(data_rating_combined_plot, aes(x = flavor, y = M, group = modal)) +
  geom_line(aes(linetype = modal)) +
  geom_point(aes(shape = modal), size = 3) +
  geom_errorbar(aes(ymin = M - SE, ymax = M + SE), width = 0.2) +
  labs(
    x = "Flavours (context)",
    y = "Mean Acceptability Rating",
    shape = "Modal",
    linetype = "Modal"
  ) +
  coord_cartesian(ylim = c(1, 7)) +
  facet_wrap(~ speaker_group) +
  theme_bw(base_size = 16)

plot_combined_ratings


# model the combined rating data ----------------------------------------------
# define factors
rating_data_combined_main$rating <- factor(rating_data_combined_main$rating)
rating_data_combined_main$modal <- factor(rating_data_combined_main$modal)
rating_data_combined_main$flavor <- factor(rating_data_combined_main$flavor)

library(ordinal)

# fit model with maximal random effects structure
model_compare <- clmm(rating ~ modal * flavor * speaker_group +
                        (1 + modal * flavor | ID) +
                        (1 + modal * flavor | item_name),
                      data = rating_data_combined_main)

# Note: this model takes a long time to fit!

summary(model_compare)
confint(model_compare, method="Wald")

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

# select data from main experiment
SPR_data_z_main <- SPR_data_z %>% filter(item_type == "main")

# plot z-transformed reading times for both speaker groups

str(SPR_data_z_main)

# define factors
SPR_data_z_main$speaker_group <- factor(SPR_data_z_main$speaker_group, levels=c("Native speakers", "L2 speakers"))
SPR_data_z_main$condition <- factor(SPR_data_z_main$condition)
SPR_data_z_main$region <- factor(SPR_data_z_main$region, levels = c("intro", "subject", "modal", "MF1", "MF2", "verb"))

# aggregate and plot 
agg_data_z_main <- SPR_data_z_main %>%
  group_by(region, condition, speaker_group) %>%
  summarise(mean_RT_z = mean(RT_z),
            se = sd(RT_z, na.rm = TRUE) / sqrt(n()),  
            .groups = "drop")

plot_RT_z_groups <- ggplot(agg_data_z_main, aes(x = region, y = mean_RT_z, group = condition, color = condition)) +
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

plot_RT_z_groups


# model z-scored RTs across groups for critical regions ------------------------
# each selected model includes the maximal random effects structure supported by the data (see Barr et al., 2013; Matuschek et al., 2017 for model reduction techniques) 

# model for region MF1:

# filter data
SPR_data_z_main_MF1 <- SPR_data_z_main %>% filter(region == "MF1")

# selected model:
model_SPR_MF1_groups <- lmer(RT_z ~ modal * flavor * speaker_group +
                                (1 + flavor | ID) +
                                (1 + flavor | item_name),
                              data = SPR_data_z_main_MF1)


summary(model_SPR_MF1_groups)
confint((model_SPR_MF1_groups7), method = "Wald")

# model indicates no significant interactions in MF1 (three-way-interaction bordering on significant, t = 1.849, CI [-0.02, 0.4])

# investigate three-way with model comparison -----------------
# fit a model w/o speaker group interaction:
model_SPR_MF1_groups_simpl <- lmer(RT_z ~ modal * flavor + speaker_group +
                               (1 + flavor | ID) +
                               (1 + flavor | item_name),
                             data = SPR_data_z_main_MF1)

 anova(model_SPR_MF1_groups, model_SPR_MF1_groups_simpl) # p = 0.08891 

# model for region MF2:

# filter data
SPR_data_z_main_MF2 <- SPR_data_z_main %>% filter(region == "MF2")

# selected model:
model_SPR_groups_MF2 <- lmer(RT_z ~ modal * flavor * speaker_group +
                                (1 | ID) +
                                (1 + flavor | item_name),
                              data = SPR_data_z_main_MF2)



summary(model_SPR_groups_MF2)
confint(model_SPR_groups_MF2, method="Wald")

# the three-way-interaction does not come out as significant, but the context:modal interaction does

# model comparison targeting interaction with speaker group
# fit model:
model_SPR_groups_MF2_simpl <- lmer(RT_z ~ modal * flavor + speaker_group +
                                (1 | ID) +
                                (1 + flavor | item_name),
                              data = SPR_data_z_main_MF2)

summary(model_SPR_groups_MF2_simpl)
anova(model_SPR_groups_MF2,model_SPR_groups_MF2_simpl) # confirms that interaction with speaker group improves model fit


# model comparison targeting interaction between modal and flavor
# fit model:
model_SPR_groups_MF2_noint <- lmer(RT_z ~ modal + flavor + speaker_group +
                                       (1 | ID) +
                                       (1 + flavor | item_name),
                                     data = SPR_data_z_main_MF2)

summary(model_SPR_groups_MF2_noint)
anova(model_SPR_groups_MF2_simpl,model_SPR_groups_MF2_noint) # confirms significantly better model fit with interaction as predictor


# model for region 'verb'

# filter data
SPR_data_z_main_verb <- SPR_data_z_main %>% filter(region == "verb")

# selected model
model_SPR_groups_verb <- lmer(RT_z ~ modal * flavor * speaker_group +
                                (1 + flavor | ID) +
                                (1 + flavor + modal | item_name),
                              data = SPR_data_z_main_verb)


summary(model_SPR_groups_verb)
confint(model_SPR_groups_verb, method="Wald")

# investigate possible 3-way-interaction by model comparison
# fit model w/o speaker group interaction:
model_SPR_groups_verb_simpl <- lmer(RT_z ~ modal * flavor + speaker_group +
                                 (1 + flavor | ID) +
                                 (1 + flavor + modal | item_name),
                               data = SPR_data_z_main_verb)

anova(model_SPR_groups_verb, model_SPR_groups_verb_simpl) # only marginally better model fit with speaker group interaction (p = 0.09542)

# model comparison for interaction between modal and flavor
model_SPR_groups_verb_noint <- lmer(RT_z ~ modal + flavor + speaker_group +
                                      (1 + flavor | ID) +
                                      (1 + flavor + modal | item_name),
                                    data = SPR_data_z_main_verb)

anova(model_SPR_groups_verb_simpl, model_SPR_groups_verb_noint) # no significant improvement of model fit


# using the scaled RTs to model the crucial data within speaker groups --------------------------------------------------------------------

# effect in region "MF2" in native speaker data-----------------------------

# filter and insepct data (region MF2, native speakers)
SPR_data_z_native_MF2 <- filter(SPR_data_z_main_MF2, speaker_group == "Native speakers")
View(SPR_data_z_native_MF2)

# selected model:
model_native_MF2_z <- lmer(RT_z ~ modal * flavor + 
                             (1 | ID) +
                             (1 | item_name),
                           data = SPR_data_z_native_MF2)

summary(model_native_MF2_z) # t = 3.072
confint(model_native_MF2_z, method = "Wald") # CI [0.1, 0.44]

# model comparison targeting the interaction

# fit model w/o interaction
model_native_MF2_z_noint <- lmer(RT_z ~ modal + flavor + 
                                 (1 | ID) +
                                 (1 | item_name),
                                data = SPR_data_z_native_MF2)

anova(model_native_MF2_z_red, model_native_MF2_z_noint) # significant effect confirmed


# check for a possible effect in the last region in the L2 speaker data -----------------
# filter and inspect data (region verb, L2 speakers)
SPR_data_z_L2_verb <- filter(SPR_data_z_main_verb, speaker_group == "L2 speakers")
View(SPR_data_z_L2_verb)

# selected model
model_L2_verb_z <- lmer(RT_z ~ modal*flavor + 
                        (1 | ID) +
                        (1 + modal | item_name),
                        data = SPR_data_z_L2_verb)


summary(model_L2_verb_z)
confint(model_L2_verb_z, method = "Wald")

# model comparison targeting interaction

# fit model w/o interaction
model_L2_verb_z_noint <- lmer(RT_z ~ modal+flavor + 
                              (1 | ID) +
                              (1 + modal | item_name),
                            data = SPR_data_z_L2_verb)

summary(model_L2_verb_z_noint)
confint(model_L2_verb_z_noint, method = "Wald")

anova(model_L2_verb_z, model_L2_verb_z_noint) # confirmed: no sign. effect of interaction on model fit


# get a p-value for a possible main effect of flavor
# fit the model without predictor
model_L2_verb_z_nofl <- lmer(RT_z ~ modal + 
                                (1 | ID) +
                                (1 + modal | item_name),
                              data = SPR_data_z_L2_verb)

anova(model_L2_verb_z_noint, model_L2_verb_z_nofl) # predictor "flavor" does not significantly improve model fit (p = 0.1028)

q()

