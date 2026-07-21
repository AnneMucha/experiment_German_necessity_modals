library(readr)
library(dplyr)
library(ggplot2)
library(lme4)

# 1. include experience as a predictor for the L2 data -------------------
L2_data_exp <- read.csv("L2_data_with_experience.csv", encoding = "UTF-8")

# acceptability data
results_L2_accept <- filter(L2_data_exp, controller_type == "AcceptabilityJudgment" & word_or_rating != "NULL")
View(results_L2_accept)

# filter for the main experiment
L2_main_accept <- filter(results_L2_accept, item_type == "main")
View(L2_main_accept)
str(L2_main_accept)

# turn ratings into numeric values
L2_main_accept$word_or_rating <- as.numeric(as.character(L2_main_accept$word_or_rating))

# sanity check:
mean_ratings <- L2_main_accept %>% group_by(condition) %>% summarize(mean = mean(word_or_rating))
mean_ratings 

str(L2_main_accept)
View(L2_main_accept)

# turn rating into a factor for ordinal model
L2_main_accept$word_or_rating <- factor(L2_main_accept$word_or_rating, level=c("1","2","3","4","5","6","7"), label=c("1","2","3","4","5","6","7"))

# other factors:
L2_main_accept$modal <- factor(L2_main_accept$modal)
L2_main_accept$flavor <- factor(L2_main_accept$flavor)
L2_main_accept$experience <- factor(L2_main_accept$experience, level = c("< 7 years", "7-15 years", "> 15 years"))

# sum-code contrasts
contrasts(L2_main_accept$flavor) <- contr.sum
contrasts(L2_main_accept$modal) <- contr.sum
contrasts(L2_main_accept$experience) <- contr.sum

# fit a model with experience as additional predictor:
library(ordinal)
model_accept_L2_exp_int <- clmm(word_or_rating ~ flavor * modal * experience + 
                                  (1 + flavor * modal | ID) + (1 + flavor * modal | item_name), data = L2_main_accept)

summary(model_accept_L2_exp_int)
confint(model_accept_L2_exp_int, method = "Wald")

# check the order of levels
levels(L2_main_accept$experience)

# estimate means per condition
library(emmeans)
emm <- emmeans(model_accept_L2_exp_int, ~ flavor * modal * experience)

# Test the interaction within each experience group
emmeans(model_accept_L2_exp_int, ~ flavor * modal | experience)

# model comparison with and without meta-interaction:
model_accept_L2_exp_reduced <- clmm(
  word_or_rating ~ modal * flavor + modal * experience + flavor * experience +
    (1 + modal * flavor | ID) +
    (1 + modal * flavor | item_name),
  data = L2_main_accept)


summary(model_accept_L2_exp_reduced)

anova(model_accept_L2_exp_int, model_accept_L2_exp_reduced)

# plot emmeans
plot_data <- as.data.frame(emm)

library(ggplot2)

ggplot(plot_data, aes(x = flavor, y = emmean, color = modal, group = modal)) +
  geom_point(position = position_dodge(width = 0.2), size = 3) +
  geom_line(position = position_dodge(width = 0.2), linewidth = 1) +
  geom_errorbar(aes(ymin = emmean - SE,
                    ymax = emmean + SE),
                width = 0.1,
                position = position_dodge(width = 0.2)) +
  facet_wrap(~ experience) +
  labs(
    x = "Flavours (context)",
    y = "Estimated latent rating",
    color = "Modal"
  ) +
  theme_minimal(base_size = 14)

# simple effects analysis
pairs(
  emmeans(model_accept_L2_exp_int,
          ~ modal | flavor * experience,
          mode = "latent"),
  adjust = "holm"
)

# context difference
pairs(
  emmeans(model_accept_L2_exp_int,
          ~ flavor | experience,
          mode = "latent"),
  adjust = "holm"
)

# run a combined analysis with "native" as experience category (not reported in the paper)
native_data <- read.csv("results_native_complete.csv", encoding = "UTF-8")
View(native_data)

rating_data_native_main <- filter(native_data_complete, PennElementName == "AcceptabilityJudgment" & Value != "NULL" & experiment == "main")
View(rating_data_native_main)

# unify columns names
library(plyr)
rating_data_native_main <- plyr::rename(rating_data_native_main, c("Order.number.of.item" = "order_number", "Label"="trial_type", "Value" = "rating", "PROLIFIC_ID" = "ID",
                                                         "experiment"="item_type", "item"="item_name", "parameter"="flavor"))

L2_main_accept <- plyr::rename(L2_main_accept, c("word_or_rating" = "rating"))
View(L2_main_accept)

# create an "experience" column for native data
rating_data_native_main$experience <- "native"

# select columns for binding --------------------------------------------
library(dplyr)
rating_native_reduced <- select(rating_data_native_main, c("order_number", "trial_type", "rating", "ID", "group", "item_type", "item_name", "condition", "modal", "flavor", "experience"))

rating_L2_reduced <- select(L2_main_accept, c("order_number", "trial_type", "rating", "ID", "group", "item_type", "item_name", "condition", "modal", "flavor", "experience"))

rating_data_combined <- rbind(rating_native_reduced, rating_L2_reduced)

View(rating_data_combined)

# define factors and coding
# other factors:
rating_data_combined$modal <- factor(rating_data_combined$modal)
rating_data_combined$flavor <- factor(rating_data_combined$flavor)
rating_data_combined$rating <- factor(rating_data_combined$rating)
rating_data_combined$experience <- factor(rating_data_combined$experience, level = c("< 7 years", "7-15 years", "> 15 years", "native"))

# sum-code contrasts
contrasts(rating_data_combined$flavor) <- contr.sum
contrasts(rating_data_combined$modal) <- contr.sum
contrasts(rating_data_combined$experience) <- contr.sum

# fit a model with "experience" to the complete data
model_exp_combined <- clmm(rating ~ flavor * modal * experience + 
                             (1 + flavor * modal | ID) + (1 + flavor * modal | item_name), data = rating_data_combined)

summary(model_exp_combined)

# estimate means per condition
library(emmeans)
emm_combined_exp <- emmeans(model_exp_combined , ~ flavor * modal * experience)

# Test the interaction within each experience group
emmeans(model_exp_combined, ~ flavor * modal | experience)

# plot emmeans
plot_data_comb_exp <- as.data.frame(emm_combined_exp)

library(ggplot2)

ggplot(plot_data_comb_exp, aes(x = flavor, y = emmean, color = modal, group = modal)) +
  geom_point(position = position_dodge(width = 0.2), size = 3) +
  geom_line(position = position_dodge(width = 0.2), linewidth = 1) +
  facet_wrap(~ experience) +
  labs(
    x = "Flavor",
    y = "Predicted rating (latent scale)",
    color = "Modal"
  ) +
  theme_minimal(base_size = 14)

# model w/o meta-interaction
model_exp_combined_reduced <- clmm(
  rating ~ modal * flavor + modal * experience + flavor * experience +
    (1 + modal * flavor | ID) +
    (1 + modal * flavor | item_name),
  data = rating_data_combined)

anova(model_exp_combined, model_exp_combined_reduced) 

# see if the "speaker group" interaction remains w/o the least experienced L2 speakers
rating_data_combined_nonewbies <- filter(rating_data_combined, experience != "< 7 years")
View(rating_data_combined_nonewbies)

rating_data_combined_nonewbies <- rating_data_combined_nonewbies %>%
  mutate(speaker_group = if_else(experience == "native", "native", "L2"))

# fit a model with 3-way-interaction:
model_combined_nonewbies <- clmm(rating ~ flavor * modal * speaker_group + 
                             (1 + flavor * modal | ID) + (1 + flavor * modal | item_name), data = rating_data_combined_nonewbies)

summary(model_combined_nonewbies)
confint(model_combined_nonewbies)
# still a significant 3-way-interaction

emm_combined_nonewbies <- emmeans(model_combined_nonewbies, ~ flavor * modal * speaker_group)

# Test the interaction within each experience group
emmeans(emm_combined_nonewbies, ~ flavor * modal | speaker_group)

# plot emmeans
plot_data_comb_nonewbies <- as.data.frame(emm_combined_nonewbies)

ggplot(plot_data_comb_nonewbies, aes(x = flavor, y = emmean, color = modal, group = modal)) +
  geom_point(position = position_dodge(width = 0.2), size = 3) +
  geom_line(position = position_dodge(width = 0.2), linewidth = 1) +
  facet_wrap(~ speaker_group) +
  labs(
    x = "Flavor",
    y = "Predicted rating (latent scale)",
    color = "Modal"
  ) +
  theme_minimal(base_size = 14)

model_combined_nonewbies_reduced <- clmm(rating ~ modal * flavor + modal * speaker_group + flavor * speaker_group + 
                                   (1 + flavor * modal | ID) + (1 + flavor * modal | item_name), data = rating_data_combined_nonewbies)

anova(model_combined_nonewbies, model_combined_nonewbies_reduced)

# experience analysis for SPR data -------------------------------------------------------
# view DF with experience bins
View(L2_data_exp)

# filter for SPR target trials in main exp
L2_SPR_main_exp <- filter(L2_data_exp, controller_type == "DashedSentence" & trial_type != "practice-SPR" & item_type == "main")
View(L2_SPR_main_exp)

# define factors
L2_SPR_main_exp$flavor <- factor(L2_SPR_main_exp$flavor)
L2_SPR_main_exp$modal <- factor(L2_SPR_main_exp$modal)
L2_SPR_main_exp$experience <- factor(L2_SPR_main_exp$experience, level = c("< 7 years", "7-15 years", "> 15 years"))

# define levels for region
L2_SPR_main_exp$region <- factor(L2_SPR_main_exp$index, level = c("1","2", "3", "4", "5", "6"), 
                                label = c("intro","subject", "modal", "MF1", "MF2", "verb"))

View(L2_SPR_main_exp)

#  filter out item "main-18" due to wrong segmentation
L2_SPR_main_exp <- filter(L2_SPR_main_exp, item_name != "main-18")

# sum-code contrasts
contrasts(L2_SPR_main_exp$flavor) <- contr.sum
contrasts(L2_SPR_main_exp$modal) <- contr.sum
contrasts(L2_SPR_main_exp$experience) <- contr.sum

# investigate RT transformations to use for the SPR data ------------------------
# model raw RTs --------------
# filter data
SPR_data_L2_main_MF2 <- L2_SPR_main_exp %>% filter(region == "MF2")
View(SPR_data_L2_main_MF2)
str(SPR_data_L2_main_MF2)

SPR_data_L2_main_MF2$RT <- as.numeric(SPR_data_L2_main_MF2$RT)

# selected model:
model_SPR_L2_MF2_raw <- lmer(RT ~ modal * flavor * experience +
                                   (1 | ID) +
                                   (1 + flavor | item_name),
                                 data = SPR_data_L2_main_MF2)

# 1. Residual histogram
hist(residuals(model_SPR_L2_MF2_raw))

# 2. QQPlot
qqnorm(residuals(model_SPR_L2_MF2_raw))
qqline(residuals(model_SPR_L2_MF2_raw))

# 3. Residuals vs fitted values: Check homoscedasticity
plot(fitted(model_SPR_L2_MF2_raw), residuals(model_SPR_L2_MF2_raw))
abline(h = 0, col = "red")

# log RTs
# selected model:
model_SPR_L2_MF2_log <- lmer(log(RT) ~ modal * flavor * experience +
                               (1 | ID) +
                               (1 + flavor | item_name),
                             data = SPR_data_L2_main_MF2)

# 1. Residual histogram
hist(residuals(model_SPR_L2_MF2_log))
# nicely symmetrical

# 2. QQPlot
qqnorm(residuals(model_SPR_L2_MF2_log))
qqline(residuals(model_SPR_L2_MF2_log))

# 3. Residuals vs fitted values: Check homoscedasticity
plot(fitted(model_SPR_L2_MF2_log), residuals(model_SPR_L2_MF2_log))
abline(h = 0, col = "red")
# definitely better than raw RTs

# model with logRTs

# check the order of exp levels
levels(L2_SPR_main_exp$experience) 

# summary of MF2-model
summary(model_SPR_L2_MF2_log)
confint(model_SPR_L2_MF2_log, method = "Wald")

# model for comparison (w/o meta-interaction):
model_SPR_L2_MF2_simp <- lmer(log(RT) ~ modal * flavor + modal * experience + flavor * experience +
                               (1 | ID) +
                               (1 + flavor | item_name),
                             data = SPR_data_L2_main_MF2)

anova(model_SPR_L2_MF2_log, model_SPR_L2_MF2_simp) 


library(emmeans)

emm_MF2 <- emmeans(model_SPR_L2_MF2_log, ~ flavor * modal * experience)

# Test the interaction within each experience group
emmeans(model_SPR_L2_MF2_log, ~ flavor * modal | experience)

# plot
plot_data_MF2 <- as.data.frame(emm_MF2)

library(ggplot2)

ggplot(plot_data_MF2, aes(x = flavor, y = emmean, color = modal, group = modal)) +
  geom_point(position = position_dodge(width = 0.2), size = 3) +
  geom_line(position = position_dodge(width = 0.2), linewidth = 1) +
  facet_wrap(~ experience) +
  labs(
    x = "Flavor",
    y = "Predicted value (latent scale)",
    color = "Modal"
  ) +
  theme_minimal(base_size = 14)

# test for modal-flavor-interaction
model_SPR_L2_MF2_simp2 <- lmer(log(RT) ~ modal + flavor + modal * experience + flavor * experience +
                                (1 | ID) +
                                (1 + flavor | item_name),
                              data = SPR_data_L2_main_MF2)

summary(model_SPR_L2_MF2_simp2) 

anova(model_SPR_L2_MF2_simp, model_SPR_L2_MF2_simp2)


# region "modal"
SPR_data_L2_main_modal <- L2_SPR_main_exp %>% filter(region == "modal")
View(SPR_data_L2_main_modal)

SPR_data_L2_main_modal$RT <- as.numeric(SPR_data_L2_main_modal$RT)

# selected model:
model_SPR_L2_modal <- lmer(log(RT) ~ modal * flavor * experience +
                               (1 | ID) +
                               (1 + flavor | item_name),
                             data = SPR_data_L2_main_modal)

summary(model_SPR_L2_modal)
confint(model_SPR_L2_modal, method = "Wald") 

# compare models with and and w/o experience as predictor:
model_SPR_L2_modal_simp <- lmer(log(RT) ~ modal * flavor + experience +
                             (1 | ID) +
                             (1 | item_name),
                           data = SPR_data_L2_main_modal)

model_SPR_L2_modal_noexp <- lmer(log(RT) ~ modal * flavor +
                                  (1 | ID) +
                                  (1 | item_name),
                                data = SPR_data_L2_main_modal)

anova(model_SPR_L2_modal_simp, model_SPR_L2_modal_noexp) 

emm_modal <- emmeans(model_SPR_L2_modal, ~ flavor * modal * experience)

# Test the interaction within each experience group
emmeans(model_SPR_L2_modal, ~ flavor * modal | experience)

# plot
plot_data_modal <- as.data.frame(emm_modal)

library(ggplot2)

ggplot(plot_data_modal, aes(x = flavor, y = emmean, color = modal, group = modal)) +
  geom_point(position = position_dodge(width = 0.2), size = 3) +
  geom_line(position = position_dodge(width = 0.2), linewidth = 1) +
  facet_wrap(~ experience) +
  labs(
    x = "Flavor",
    y = "Predicted value (latent scale)",
    color = "Modal"
  ) +
  theme_minimal(base_size = 14)


# region "MF1"
SPR_data_L2_main_MF1 <- L2_SPR_main_exp %>% filter(region == "MF1")
View(SPR_data_L2_main_MF1)

SPR_data_L2_main_MF1$RT <- as.numeric(SPR_data_L2_main_MF1$RT)

# selected model:
model_SPR_L2_MF1 <- lmer(log(RT) ~ modal * flavor * experience +
                             (1 + modal | ID) +
                             (1 | item_name),
                           data = SPR_data_L2_main_MF1, control = lmerControl(optimizer = "bobyqa"))

summary(model_SPR_L2_MF1)
confint(model_SPR_L2_MF1, method = "Wald")

# model for comparison (targeting 3-way-interaction)
model_SPR_L2_MF1_simp <- lmer(log(RT) ~ modal * flavor + modal * experience + flavor * experience +
                           (1 + modal | ID) +
                           (1 | item_name),
                         data = SPR_data_L2_main_MF1, control = lmerControl(optimizer = "bobyqa"))

anova(model_SPR_L2_MF1, model_SPR_L2_MF1_simp) 

# compare with a model without experience:
model_SPR_L2_MF1_exp <- lmer(log(RT) ~ modal * flavor + experience +
                                 (1 + modal | ID) +
                                 (1 | item_name),
                               data = SPR_data_L2_main_MF1)

model_SPR_L2_MF1_noexp <- lmer(log(RT) ~ modal * flavor +
                                (1 + modal | ID) +
                                (1 | item_name),
                              data = SPR_data_L2_main_MF1)

anova(model_SPR_L2_MF1_exp, model_SPR_L2_MF1_noexp) 

emm_MF1 <- emmeans(model_SPR_L2_MF1, ~ flavor * modal * experience)

# plot
plot_data_MF1 <- as.data.frame(emm_MF1)

library(ggplot2)

ggplot(plot_data_MF1, aes(x = flavor, y = emmean, color = modal, group = modal)) +
  geom_point(position = position_dodge(width = 0.2), size = 3) +
  geom_line(position = position_dodge(width = 0.2), linewidth = 1) +
  facet_wrap(~ experience) +
  labs(
    x = "Flavor",
    y = "Predicted value (latent scale)",
    color = "Modal"
  ) +
  theme_minimal(base_size = 14)

# check for a context-flavor-interaction with least experienced group
View(SPR_data_L2_main_MF1_newbies)
SPR_data_L2_main_MF1_newbies <- filter(SPR_data_L2_main_MF1, experience == "< 7 years")

model_newbies <- lmer(log(RT) ~ modal * flavor +
                        (1 + flavor | ID) +
                        (1 | item_name),
                      data = SPR_data_L2_main_MF1_newbies)

summary(model_newbies) 
confint(model_newbies, method = "Wald")

model_newbies_no_int <- lmer(log(RT) ~ modal + flavor +
                        (1 + flavor | ID) +
                        (1 | item_name),
                      data = SPR_data_L2_main_MF1_newbies)

anova(model_newbies_no_int, model_newbies) 

# check for a context-flavor-interaction with most experienced group
View(SPR_data_L2_main_MF1_oldbies)
SPR_data_L2_main_MF1_oldbies <- filter(SPR_data_L2_main_MF1, experience == "> 15 years")

model_oldbies <- lmer(log(RT) ~ modal * flavor +
                        (1 + flavor | ID) +
                        (1 | item_name),
                      data = SPR_data_L2_main_MF1_oldbies)

summary(model_oldbies)
confint(model_oldbies, method = "Wald")

model_oldbies_no_int <- lmer(log(RT) ~ modal + flavor +
                               (1 + flavor | ID) +
                               (1 | item_name),
                             data = SPR_data_L2_main_MF1_oldbies)

anova(model_oldbies_no_int, model_oldbies) 

# check for a context-flavor-interaction with middle group
View(SPR_data_L2_main_MF1_midbies)
SPR_data_L2_main_MF1_midbies <- filter(SPR_data_L2_main_MF1, experience == "7-15 years")

model_midbies <- lmer(log(RT) ~ modal * flavor +
                        (1 + modal | ID) +
                        (1 | item_name),
                      data = SPR_data_L2_main_MF1_midbies)

summary(model_midbies)
confint(model_midbies, method = "Wald")

model_midbies_no_int <- lmer(log(RT) ~ modal + flavor +
                               (1 + modal | ID) +
                               (1 | item_name),
                             data = SPR_data_L2_main_MF1_midbies)

anova(model_midbies_no_int, model_midbies) 

# region "verb"
SPR_data_L2_main_verb <- L2_SPR_main_exp %>% filter(region == "verb")
View(SPR_data_L2_main_verb)

SPR_data_L2_main_verb$RT <- as.numeric(SPR_data_L2_main_verb$RT)

# selected model:
model_SPR_L2_verb <- lmer(log(RT) ~ modal * flavor * experience +
                           (1 + modal | ID) +
                           (1 | item_name),
                         data = SPR_data_L2_main_verb)

summary(model_SPR_L2_verb)
confint(model_SPR_L2_verb, method = "Wald")

# model for comparison
model_SPR_L2_verb_simp <- lmer(log(RT) ~ modal * flavor + modal * experience + flavor * experience +
                                (1 + modal | ID) +
                                (1 | item_name),
                              data = SPR_data_L2_main_verb)

summary(model_SPR_L2_verb_simp)
confint(model_SPR_L2_verb_simp, method = "Wald")
anova(model_SPR_L2_verb, model_SPR_L2_verb_simp) 

# compare models with and  without experience as predictor:
model_SPR_L2_verb_exp <- lmer(log(RT) ~ modal * flavor + experience +
                                  (1 + modal | ID) +
                                  (1 | item_name),
                                data = SPR_data_L2_main_verb)

model_SPR_L2_verb_noexp <- lmer(log(RT) ~ modal * flavor +
                                 (1 + modal | ID) +
                                 (1 | item_name),
                               data = SPR_data_L2_main_verb)

anova(model_SPR_L2_verb_exp, model_SPR_L2_verb_noexp) 

# test interaction between modal and flavor
model_SPR_L2_verb_simp2 <- lmer(log(RT) ~ modal + flavor + modal * experience + flavor * experience +
                                 (1 + modal | ID) +
                                 (1 | item_name),
                               data = SPR_data_L2_main_verb)

anova(model_SPR_L2_verb_simp, model_SPR_L2_verb_simp2) 


summary(model_SPR_L2_verb_noexp)
confint(model_SPR_L2_verb_noexp, method = "Wald")

emm_verb <- emmeans(model_SPR_L2_verb, ~ flavor * modal * experience)

# plot
plot_data_verb <- as.data.frame(emm_verb)


ggplot(plot_data_verb, aes(x = flavor, y = emmean, color = modal, group = modal)) +
  geom_point(position = position_dodge(width = 0.2), size = 3) +
  geom_line(position = position_dodge(width = 0.2), linewidth = 1) +
  facet_wrap(~ experience) +
  labs(
    x = "Flavor",
    y = "Predicted value (latent scale)",
    color = "Modal"
  ) +
  theme_minimal(base_size = 14)

# compare models with and  without flavor as predictor:
model_SPR_L2_verb_flav <- lmer(log(RT) ~ modal * experience + flavor +
                                (1 + modal | ID) +
                                (1 | item_name),
                              data = SPR_data_L2_main_verb)

summary(model_SPR_L2_verb_flav)
confint(model_SPR_L2_verb_flav, method = "Wald")

model_SPR_L2_verb_noflav <- lmer(log(RT) ~ modal * experience +
                                 (1 + modal | ID) +
                                 (1 | item_name),
                               data = SPR_data_L2_main_verb)

anova(model_SPR_L2_verb_flav, model_SPR_L2_verb_noflav)


q()

