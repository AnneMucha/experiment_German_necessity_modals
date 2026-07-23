library(readr)
library(dplyr)
library(ggplot2)
library(lme4)
library(ordinal)

# Main data analysis script

# read and inspect cleaned data frame for L2 speakers
L2_data <- read.csv("L2_data_after_exclusion.csv", encoding = "UTF-8")
View(L2_data)

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

# calculate by-item mean ratings
data_rating_combined_item <- rating_data_combined_main %>%                     
  group_by(speaker_group, flavor, modal, item_name) %>%                           
  summarise(                                             
    M  = mean(rating, na.rm = TRUE),             
    N  = n(),                                            
    SE = sd(rating, na.rm = TRUE) / sqrt(N),
    .groups = "drop"
  )

View(data_rating_combined_item)

# look at teleological items in particular
rating_data_combined_teleo <- filter(rating_data_combined_main, flavor == "non-bouletic" & 
                                       item_name %in% c("main-1","main-2","main-4", "main-5","main-8","main-15","main-18","main-19","main-21","main-29"))

data_rating_teleo_plot <- rating_data_combined_teleo %>%                     
  group_by(modal, speaker_group) %>%                           
  summarise(                                             
    M  = mean(rating, na.rm = TRUE),             
    N  = n(),                                            
    SE = sd(rating, na.rm = TRUE) / sqrt(N),
    .groups = "drop"
  )

View(data_rating_teleo_plot) 

plot_teleo_ratings <- ggplot(data_rating_teleo_plot, aes(x = speaker_group, y = M, group = modal)) +
  geom_line(aes(linetype = modal)) +
  geom_point(aes(shape = modal), size = 3) +
  geom_errorbar(aes(ymin = M - SE, ymax = M + SE), width = 0.2) +
  labs(
    x = "Speaker group",
    y = "Mean Acceptability Rating (teleological contexts)",
    shape = "Modal",
    linetype = "Modal"
  ) +
  coord_cartesian(ylim = c(1, 7)) +
  #facet_wrap(~ speaker_group) +
  theme_bw(base_size = 16)

plot_teleo_ratings

# fit a model ------

# define factors
rating_data_combined_teleo$rating <- factor(rating_data_combined_teleo$rating)
rating_data_combined_teleo$modal <- factor(rating_data_combined_teleo$modal)
rating_data_combined_teleo$flavor <- factor(rating_data_combined_teleo$flavor)

library(ordinal)
# sum-code contrasts
contrasts(rating_data_combined_teleo$modal) <- contr.sum
contrasts(rating_data_combined_teleo$speaker_group) <- contr.sum

# fit a model with maximal random effects structure
model_teleo <- clmm(rating ~ modal * speaker_group +
                        (1 + modal | ID) +
                        (1 + modal | item_name),
                      data = rating_data_combined_teleo)

summary(model_teleo) 
confint(model_teleo, method="Wald") 

# simple effects analysis of modal within speaker group
library(emmeans)
pairs(
  emmeans(model_teleo,
          ~ modal | speaker_group,
          mode = "latent"),
  adjust = "holm"
)
# simple effects analysis of speaker group within modal
pairs(
  emmeans(model_teleo,
          ~ speaker_group | modal,
          mode = "latent"),
  adjust = "holm"
)

# investigate variation between participants ---------------
# native speakers
participant_means_ratings_native <- rating_data_combined_main %>%
  filter(speaker_group == "Native speakers") %>%
  group_by(ID, modal, flavor) %>%
  summarise(mean_rating = mean(rating), .groups = "drop")

View(participant_means_ratings_native)

# spaghetti plot
ggplot(participant_means_ratings_native,
       aes(x = flavor,
           y = mean_rating,
           group = ID)) +
  geom_line(alpha = .3) +
  facet_wrap(~ modal) +
  labs(x = "Flavor (context)",
       y = "Mean ratings by participants (native speakers)")

# inspect participants with lowest ratings 
participant_means_ratings_native %>%
  arrange(mean_rating) %>%
  arrange(desc(modal)) %>%
  head(15)

# same for L2 speakers:
participant_means_ratings_L2 <- rating_data_combined_main %>%
  filter(speaker_group == "L2 speakers") %>%
  group_by(ID, modal, flavor) %>%
  summarise(mean_rating = mean(rating), .groups = "drop")

# spaghetti plot
ggplot(participant_means_ratings_L2,
       aes(x = flavor,
           y = mean_rating,
           group = ID)) +
  geom_line(alpha = .3) +
  facet_wrap(~ modal) +
  labs(x = "Flavor (context)",
       y = "Mean ratings by participants (L2 speakers)")


# model the combined rating data ----------------------------------------------
# define factors
rating_data_combined_main$rating <- factor(rating_data_combined_main$rating)
rating_data_combined_main$modal <- factor(rating_data_combined_main$modal)
rating_data_combined_main$flavor <- factor(rating_data_combined_main$flavor)
rating_data_combined_main$speaker_group <- factor(rating_data_combined_main$speaker_group)

library(ordinal)

# sum-code contrasts
contrasts(rating_data_combined_main$flavor) <- contr.sum
contrasts(rating_data_combined_main$modal) <- contr.sum
contrasts(rating_data_combined_main$speaker_group) <- contr.sum

# fit a model with maximal random effects structure
model_compare <- clmm(rating ~ modal * flavor * speaker_group +
                        (1 + modal * flavor | ID) +
                        (1 + modal * flavor | item_name),
                      data = rating_data_combined_main)

# Note: this model takes a long time to fit!

summary(model_compare)
confint(model_compare, method="Wald")

# Added: reduced model for comparison (removing the 3-way-interaction)
model_reduced <- clmm(
  rating ~ modal * flavor + modal * speaker_group + flavor * speaker_group +
    (1 + modal * flavor | ID) +
    (1 + modal * flavor | item_name),
  data = rating_data_combined_main
)

summary(model_reduced)
anova(model_compare, model_reduced)

# verify modal-flavor-interaction
model_reduced2 <- clmm(
  rating ~ modal + flavor + modal * speaker_group + flavor * speaker_group +
    (1 + modal * flavor | ID) +
    (1 + modal * flavor | item_name),
  data = rating_data_combined_main
)

summary(model_reduced2)

anova(model_reduced, model_reduced2) 

# check interaction between modal and speaker group
model_reduced3 <- clmm(
  rating ~ modal * flavor + modal + speaker_group + flavor * speaker_group +
    (1 + modal * flavor | ID) +
    (1 + modal * flavor | item_name),
  data = rating_data_combined_main
)

summary(model_reduced3)
anova(model_reduced, model_reduced3) 

# testing simple effects:
pairs(
  emmeans(model_compare,
          ~ modal | flavor * speaker_group,
          mode = "latent"),
  adjust = "holm"
)

# testing difference between contexts with "sollen"
pairs(
  emmeans(model_compare,
          ~ flavor | modal * speaker_group,
          mode = "latent"),
  adjust = "holm"
)


# added here: LRT for the rating data in the L2 study --------------------------------------------------
View(rating_data_combined_main)
L2_rating_data <- filter(rating_data_combined_main, speaker_group == "L2 speakers")
View(L2_rating_data)

# fit an ordinal mixed model with maximal effect structure 
model_accept <- clmm(rating ~ flavor * modal + (1 + flavor * modal | ID) + (1 + flavor * modal | item_name), data = L2_rating_data)
summary(model_accept) 
confint(model_accept)

# fit model without interaction
model_reduced <- clmm(rating ~ flavor + modal + (1 + flavor * modal | ID) + (1 + flavor * modal | item_name), data = L2_rating_data)

anova(model_accept, model_reduced)

# also added: participant variation
G <- VarCorr(model_accept)$ID

cell_var <- function(x) {
  drop(t(x) %*% G %*% x)
}

vars <- c(
  "bouletic, muss"      = cell_var(c(1,  1,  1,  1)),
  "bouletic, soll"      = cell_var(c(1,  1, -1, -1)),
  "non-bouletic, muss"  = cell_var(c(1, -1,  1, -1)),
  "non-bouletic, soll"  = cell_var(c(1, -1, -1,  1))
)

data.frame(
  condition = names(vars),
  variance = vars,
  sd = sqrt(vars)
)

# analyse self-paced reading data ---------------------------------------------- ----------------------------------------------

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

# exclude item "main-18" because there is a segmentation error in the non-bouletic-muss condition

SPR_data_combined <- filter(SPR_data_combined, item_name != "main-18")

# summarising mean reading times by region, condition, speaker group
rawRT_summary <- SPR_data_combined %>% filter(item_type == "main" & (! region %in% c("intro", "subject"))) %>%
  mutate(
    speaker_group = factor(speaker_group, levels = c("Native speakers", "L2 speakers")),
    region = factor(region, levels = c("modal", "MF1", "MF2", "verb"))
  ) %>%
  group_by(region,condition, speaker_group) %>%
  summarise(
    mean_RT = mean(RT, na.rm = TRUE),
    n       = sum(!is.na(RT)),   
    sd_RT   = sd(RT, na.rm = TRUE), 
    se_RT   = sd_RT / sqrt(n),             
    .groups = "drop"
  ) %>%
  mutate(
    summary_val = paste0(round(mean_RT, 1), " (", round(sd_RT, 1), ")"),
  ) %>%
  select(speaker_group, region, condition, summary_val) %>%
  pivot_wider(
    names_from = region,
    values_from = summary_val,
    names_sort = FALSE
  )

View(rawRT_summary)

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

# Select a reading time measure based on model assumptions
# using region "MF2" where there is most likely to be a significant effect
View(SPR_data_z_main)

# sum-code contrasts
SPR_data_z_main$flavor <- factor(SPR_data_z_main$flavor)
SPR_data_z_main$modal <- factor(SPR_data_z_main$modal)
SPR_data_z_main$speaker_group <- factor(SPR_data_z_main$speaker_group)

contrasts(SPR_data_z_main$flavor) <- contr.sum
contrasts(SPR_data_z_main$modal) <- contr.sum
contrasts(SPR_data_z_main$speaker_group) <- contr.sum

# model raw RTs --------------
# filter data
SPR_data_z_main_MF2 <- SPR_data_z_main %>% filter(region == "MF2")
View(SPR_data_z_main_MF2)

# selected model:
model_SPR_groups_MF2_raw <- lmer(RT ~ modal * flavor * speaker_group +
                               (1 | ID) +
                               (1 | item_name),
                             data = SPR_data_z_main_MF2)


# 1. Residual histogram
hist(residuals(model_SPR_groups_MF2_raw))

# 2. QQPlot
qqnorm(residuals(model_SPR_groups_MF2_raw))
qqline(residuals(model_SPR_groups_MF2_raw))

# 3. Residuals vs fitted values: Check homoscedasticity
plot(fitted(model_SPR_groups_MF2_raw), residuals(model_SPR_groups_MF2_raw))
abline(h = 0, col = "red")

# or
plot(model_SPR_groups_MF2_raw)

# model log RTs --------------

# selected model (includes a random slope w/o overfit):
model_SPR_groups_MF2_log <- lmer(log(RT) ~ modal * flavor * speaker_group +
                                   (1 | ID) +
                                   (1 + flavor | item_name),
                                 data = SPR_data_z_main_MF2)


# 1. Residual histogram
hist(residuals(model_SPR_groups_MF2_log))
# better, no skew at all

# 2. QQPlot
qqnorm(residuals(model_SPR_groups_MF2_log))
qqline(residuals(model_SPR_groups_MF2_log))
# not perfect, but less deviations at the tails, follows the line better

# 3. Residuals vs fitted values
plot(fitted(model_SPR_groups_MF2_log), residuals(model_SPR_groups_MF2_log))
abline(h = 0, col = "red")
# good: no funnel shape, no increasing spread


# model zRTs --------------

# selected model:
model_SPR_groups_MF2_z <- lmer(RT_z ~ modal * flavor * speaker_group +
                                   (1 | ID) +
                                   (1 | item_name),
                                 data = SPR_data_z_main_MF2)

# 1. Residual histogram
hist(residuals(model_SPR_groups_MF2_z))

# 2. QQPlot
qqline(residuals(model_SPR_groups_MF2_z))
# better than raw, worse than log

# 3. Residuals vs fitted values
plot(fitted(model_SPR_groups_MF2_z), residuals(model_SPR_groups_MF2_z))
abline(h = 0, col = "red")
# better than raw, worse than log

# resRT: model overfits even without varying slopes
# report results with log transformation!

summary(model_SPR_groups_MF2_log) 
confint(model_SPR_groups_MF2_log, method="Wald")

# model comparison targeting interaction with speaker group
# fit model:
model_SPR_groups_MF2_simpl <- lmer(log(RT) ~ modal * flavor + modal * speaker_group + flavor * speaker_group +
                                     (1 | ID) +
                                     (1 + flavor | item_name),
                                   data = SPR_data_z_main_MF2)

summary(model_SPR_groups_MF2_simpl)
anova(model_SPR_groups_MF2_log, model_SPR_groups_MF2_simpl) 
confint(model_SPR_groups_MF2_simpl, method="Wald")

# model comparison to identify modal-flavor-interaction:
model_SPR_groups_MF2_simpl2 <- lmer(log(RT) ~ modal + flavor + modal * speaker_group + flavor * speaker_group +
                                     (1 | ID) +
                                     (1 + flavor | item_name),
                                   data = SPR_data_z_main_MF2)

anova(model_SPR_groups_MF2_simpl,model_SPR_groups_MF2_simpl2) 


# model for region MF1:
# filter data
SPR_data_z_main_MF1 <- SPR_data_z_main %>% filter(region == "MF1")
View(SPR_data_z_main_MF1)
# selected model:
model_SPR_MF1_groups <- lmer(log(RT) ~ modal * flavor * speaker_group +
                                (1 + modal | ID) +
                                (1 | item_name),
                              data = SPR_data_z_main_MF1)


summary(model_SPR_MF1_groups)
confint((model_SPR_MF1_groups), method = "Wald")

# investigate three-way with model comparison -----------------
# fit a model w/o speaker group interaction:
model_SPR_MF1_groups_simpl <- lmer(log(RT) ~ modal * flavor + modal * speaker_group + flavor * speaker_group +
                               (1 + modal | ID) +
                               (1 | item_name),
                             data = SPR_data_z_main_MF1)

 anova(model_SPR_MF1_groups, model_SPR_MF1_groups_simpl) 


# model for region 'verb'
# filter data
SPR_data_z_main_verb <- SPR_data_z_main %>% filter(region == "verb")

# selected model
model_SPR_groups_verb <- lmer(log(RT) ~ modal * flavor * speaker_group +
                                (1 + modal | ID) +
                                (1 + flavor | item_name),
                              data = SPR_data_z_main_verb)


summary(model_SPR_groups_verb)
confint(model_SPR_groups_verb, method="Wald")

# investigate possible 3-way-interaction by model comparison
# fit model w/o speaker group interaction:
model_SPR_groups_verb_simpl <- lmer(log(RT) ~ modal * flavor + modal * speaker_group + flavor * speaker_group +
                                 (1 + modal | ID) +
                                 (1 + flavor | item_name),
                               data = SPR_data_z_main_verb)

anova(model_SPR_groups_verb, model_SPR_groups_verb_simpl) 

# investigate estimated marginal means
library(emmeans)
emm_verb_combined <- emmeans(model_SPR_groups_verb, ~ flavor * modal * speaker_group)

# plot
plot_data_verb_combined <- as.data.frame(emm_verb_combined)

library(ggplot2)
ggplot(plot_data_verb_combined, aes(x = flavor, y = emmean, color = modal, group = modal)) +
  geom_point(position = position_dodge(width = 0.2), size = 3) +
  geom_line(position = position_dodge(width = 0.2), linewidth = 1) +
  facet_wrap(~ speaker_group) +
  labs(
    x = "Flavor",
    y = "Predicted value (latent scale)",
    color = "Modal"
  ) +
  theme_minimal(base_size = 14)

# check for modal-flavor-interaction
model_SPR_groups_verb_simpl2 <- lmer(log(RT) ~ modal + flavor + modal * speaker_group + flavor * speaker_group +
                                      (1 + modal | ID) +
                                      (1 + flavor | item_name),
                                    data = SPR_data_z_main_verb)

summary(model_SPR_groups_verb_simpl2)
anova(model_SPR_groups_verb_simpl, model_SPR_groups_verb_simpl2) 

# using logRTs to model the crucial data within speaker groups --------------------------------------------------------------------

# effect in region "MF2" in native speaker data-----------------------------
# filter and insepct data (region MF2, native speakers)
SPR_data_z_native_MF2 <- filter(SPR_data_z_main_MF2, speaker_group == "Native speakers")
View(SPR_data_z_native_MF2)

# first check again that log RTs provide the best fit
# try raw RTs:
model_native_MF2_raw <- lmer(RT ~ modal * flavor + 
                             (1 | ID) +
                             (1 + flavor | item_name),
                           data = SPR_data_z_native_MF2)

# 1. Residual histogram
hist(residuals(model_native_MF2_raw))
# right skew

# 2. QQPlot: This is usually the key diagnostic for RT models.
qqnorm(residuals(model_native_MF2_raw))
qqline(residuals(model_native_MF2_raw))
# not great: lots of deviations at the right tails

# 3. Residuals vs fitted values: Check homoscedasticity
plot(fitted(model_native_MF2_raw), residuals(model_native_MF2_raw))
abline(h = 0, col = "red")

# or
plot(model_SPR_groups_MF2_raw)
# not great: increasing spread

# try logRTs:
model_native_MF2_log <- lmer(log(RT) ~ modal * flavor + 
                               (1 | ID) +
                               (1 + flavor | item_name),
                             data = SPR_data_z_native_MF2)

# 1. Residual histogram
hist(residuals(model_native_MF2_log))
# good: almost no skew

# 2. QQPlot
qqnorm(residuals(model_native_MF2_log))
qqline(residuals(model_native_MF2_log))
# better: closer to the line, less deviation at the right tail

# 3. Residuals vs fitted values: Check homoscedasticity
plot(fitted(model_native_MF2_log), residuals(model_native_MF2_log))
abline(h = 0, col = "red")
# looks good
# report modeling with logRTs

summary(model_native_MF2_log)
confint(model_native_MF2_log, method = "Wald")


# model comparison targeting the interaction
# fit model w/o interaction
model_native_MF2_noint <- lmer(log(RT) ~ modal + flavor + 
                                 (1 | ID) +
                                 (1 + flavor | item_name),
                                data = SPR_data_z_native_MF2)

anova(model_native_MF2_log, model_native_MF2_noint) 
summary(model_native_MF2_noint)

# also check for main effect of flavor
model_native_MF2_noflav <- lmer(log(RT) ~ modal + 
                                 (1 | ID) +
                                 (1 + flavor | item_name),
                               data = SPR_data_z_native_MF2)

summary(model_native_MF2_noflav)
anova(model_native_MF2_noint, model_native_MF2_noflav) 

# simple effects test 
pairs(
  emmeans(model_native_MF2_log,
          ~ modal | flavor),
  adjust = "holm"
)


# investigate the other regions in the native speaker data:
# filter and inspect data (region modal, native speakers)
SPR_data_z_native_modal <- filter(SPR_data_z_main, speaker_group == "Native speakers" & region == "modal")
View(SPR_data_z_native_modal)

# selected model:
model_native_modal <- lmer(log(RT) ~ modal * flavor + 
                               (1 | ID) +
                               (1 + flavor | item_name),
                             data = SPR_data_z_native_modal)

summary(model_native_modal)
confint(model_native_modal, method = "Wald") 

# filter and inspect data (region MF1, native speakers)
SPR_data_z_native_MF1 <- filter(SPR_data_z_main_MF1, speaker_group == "Native speakers")
View(SPR_data_z_native_MF1)

# selected model:
model_native_MF1 <- lmer(log(RT) ~ modal * flavor + 
                             (1 + modal | ID) +
                             (1 + modal + flavor | item_name),
                           data = SPR_data_z_native_MF1)

summary(model_native_MF1)
confint(model_native_MF1, method = "Wald") 

# filter and inspect data (region verb, native speakers)
SPR_data_z_native_verb <- filter(SPR_data_z_main, speaker_group == "Native speakers" & region == "verb")
View(SPR_data_z_native_verb)

# selected model:
model_native_verb <- lmer(log(RT) ~ modal * flavor + 
                           (1 | ID) +
                           (1 + flavor | item_name),
                         data = SPR_data_z_native_verb)

summary(model_native_verb)
confint(model_native_verb, method = "Wald") 

# run log-models of L2 data ------------------------
# filter and inspect data (region modal, L2 speakers)
SPR_data_z_L2_modal <- filter(SPR_data_z_main, speaker_group == "L2 speakers" & region == "modal")
View(SPR_data_z_L2_modal)

# selected model:
model_L2_modal <- lmer(log(RT) ~ modal * flavor + 
                             (1 | ID) +
                             (1 | item_name),
                           data = SPR_data_z_L2_modal)

summary(model_L2_modal)
confint(model_L2_modal, method = "Wald") 

# filter and inspect data (region MF1, L2 speakers)
SPR_data_z_L2_MF1 <- filter(SPR_data_z_main, speaker_group == "L2 speakers" & region == "MF1")
View(SPR_data_z_L2_MF1)

# selected model:
model_L2_MF1 <- lmer(log(RT) ~ modal * flavor + 
                         (1 | ID) +
                         (1 | item_name),
                       data = SPR_data_z_L2_MF1)

summary(model_L2_MF1)
confint(model_L2_MF1, method = "Wald") 

# filter and inspect data (region MF2, L2 speakers)
SPR_data_z_L2_MF2 <- filter(SPR_data_z_main, speaker_group == "L2 speakers" & region == "MF2")
View(SPR_data_z_L2_MF2)

# selected model:
model_L2_MF2 <- lmer(log(RT) ~ modal * flavor + 
                       (1 | ID) +
                       (1 | item_name),
                     data = SPR_data_z_L2_MF2)

summary(model_L2_MF2)
confint(model_L2_MF2, method = "Wald") 

# filter and inspect data (region MF2, L2 speakers)
SPR_data_z_L2_verb <- filter(SPR_data_z_main, speaker_group == "L2 speakers" & region == "verb")
View(SPR_data_z_L2_verb)

# selected model:
model_L2_verb <- lmer(log(RT) ~ modal * flavor + 
                       (1 + modal | ID) +
                       (1 + flavor | item_name),
                     data = SPR_data_z_L2_verb)

summary(model_L2_verb)
confint(model_L2_verb, method = "Wald") 


# model comparison targeting interaction:
model_L2_verb_noint <- lmer(log(RT) ~ modal + flavor + 
                        (1 + modal | ID) +
                        (1 + flavor | item_name),
                      data = SPR_data_z_L2_verb)

anova(model_L2_verb, model_L2_verb_noint) 

levels(SPR_data_z_L2_verb$modal) 
levels(SPR_data_z_L2_verb$flavor) 

# simple effects analysis
pairs(
  emmeans(model_L2_verb,
          ~ modal | flavor),
  adjust = "holm"
)

# explore participant variation: (interaction was not a random effect in the model)
participant_means <- SPR_data_z_L2_verb %>%
  group_by(ID, modal, flavor) %>%
  summarise(meanRT = mean(log(RT)), .groups = "drop")

# reshape
library(tidyr)

participant_wide <- participant_means %>%
  unite(condition, modal, flavor) %>%
  pivot_wider(names_from = condition,
              values_from = meanRT)

# compute interaction effects by participant
participant_wide_int <- participant_wide %>%
  mutate(interaction_effect =
           (muss_bouletic - `muss_non-bouletic`) -
           (soll_bouletic - `soll_non-bouletic`))

participant_wide_int

# histogram
ggplot(participant_wide_int,
       aes(x = interaction_effect)) +
  geom_histogram(bins = 20) +
  geom_vline(xintercept = 0,
             linetype = "dashed") +
  labs(x = "Participant interaction effect (region verb, L2 speakers)",
       y = "Count")

# inspect participants with largest effect
participant_wide_int %>%
  arrange(desc(abs(interaction_effect))) %>%
  head(10)

# density plot
ggplot(participant_wide_int,
       aes(x = interaction_effect)) +
  geom_density(fill = "grey70") +
  geom_vline(xintercept = 0,
             linetype = "dashed") +
  labs(x = "Participant interaction effect (region verb, L2 speakers)",
       y = "Density")


# do the same for the main effect of context: -------------
participant_wide_flavor <- participant_wide %>%
  mutate(context_effect =
           ((muss_bouletic + soll_bouletic) / 2) -
           ((`muss_non-bouletic` + `soll_non-bouletic`) / 2))


# histogram
ggplot(participant_wide_flavor,
       aes(x = context_effect)) +
  geom_histogram(bins = 20) +
  geom_vline(xintercept = 0,
             linetype = "dashed") +
  labs(x = "Participant main effect of context (region verb, L2 speakers)",
       y = "Count")

# inspect participants with largest effect
participant_wide_flavor %>%
  arrange(desc(abs(context_effect))) %>%
  head(10)

# density plot
ggplot(participant_wide_flavor,
       aes(x = context_effect)) +
  geom_density(fill = "grey70") +
  geom_vline(xintercept = 0,
             linetype = "dashed") +
  labs(x = "Participant main effect of context (region verb, L2 speakers)",
       y = "Density")


# same for the interaction effect in region MF2 with native speakers, for comparison ------------------
participant_means_native <- SPR_data_z_native_MF2 %>%
  group_by(ID, modal, flavor) %>%
  summarise(meanRT = mean(log(RT)), .groups = "drop")

# reshape
library(tidyr)

participant_wide_native <- participant_means_native %>%
  unite(condition, modal, flavor) %>%
  pivot_wider(names_from = condition,
              values_from = meanRT)

# compute interaction effects by participant
participant_wide_native <- participant_wide_native %>%
  mutate(interaction_effect =
           (muss_bouletic - `muss_non-bouletic`) -
           (soll_bouletic - `soll_non-bouletic`))

participant_wide_native

# histogram
ggplot(participant_wide_native,
       aes(x = interaction_effect)) +
  geom_histogram(bins = 20) +
  geom_vline(xintercept = 0,
             linetype = "dashed") +
  labs(x = "Participant interaction effect (region MF2, native speakers)",
       y = "Count")

# inspect participants with largest effect
participant_wide_native %>%
  arrange(desc(abs(interaction_effect))) %>%
  head(1)

print(participant_wide_native)

# density plot
ggplot(participant_wide_native,
       aes(x = interaction_effect)) +
  geom_density(fill = "grey70") +
  geom_vline(xintercept = 0,
             linetype = "dashed") +
  labs(x = "Participant interaction effect (region MF2, native speakers)",
       y = "Density")

# explore item effect in the L2 speaker data -------------
View(SPR_data_z_L2_verb)

item_means <- SPR_data_z_L2_verb %>%
  group_by(item_name, modal, flavor) %>%
  summarise(meanRT = mean(log(RT)), .groups = "drop")

# reshape
library(tidyr)

item_wide <- item_means %>%
  unite(condition, modal, flavor) %>%
  pivot_wider(names_from = condition,
              values_from = meanRT)

# compute interaction effects by participant
item_wide_int <- item_wide %>%
  mutate(interaction_effect =
           (muss_bouletic - `muss_non-bouletic`) -
           (soll_bouletic - `soll_non-bouletic`))

item_wide_int

# histogram
ggplot(item_wide_int,
       aes(x = interaction_effect)) +
  geom_histogram(bins = 20) +
  geom_vline(xintercept = 0,
             linetype = "dashed") +
  labs(x = "Item interaction effect (region verb, L2 speakers)",
       y = "Count")

# inspect items with largest effect
item_wide_int %>%
  arrange(desc(abs(interaction_effect))) %>%
  head(10)

# density plot
ggplot(item_wide_int,
       aes(x = interaction_effect)) +
  geom_density(fill = "grey70") +
  geom_vline(xintercept = 0,
             linetype = "dashed") +
  labs(x = "Item interaction effect (region verb, L2 speakers)",
       y = "Density")

# same for main effect of flavor ------------

item_wide_flavor <- item_wide %>%
  mutate(context_effect =
           ((muss_bouletic + soll_bouletic) / 2) -
           ((`muss_non-bouletic` + `soll_non-bouletic`) / 2))


# histogram
ggplot(item_wide_flavor,
       aes(x = context_effect)) +
  geom_histogram(bins = 20) +
  geom_vline(xintercept = 0,
             linetype = "dashed") +
  labs(x = "Item main effect of context (region verb, L2 speakers)",
       y = "Count")

# inspect items with largest effect
item_wide_flavor %>%
  arrange(desc(abs(context_effect))) %>%
  head(10)

# density plot
ggplot(item_wide_flavor,
       aes(x = context_effect)) +
  geom_density(fill = "grey70") +
  geom_vline(xintercept = 0,
             linetype = "dashed") +
  labs(x = "Item main effect of context (region verb, L2 speakers)",
       y = "Density")

# try a model with only interaction as predictor (and include as random effect w/o correlations)
model_L2_verb_intonly <- lmer(log(RT) ~ modal:flavor + (1 + modal:flavor || ID) +
                                                       (1 + modal:flavor || item_name), data = SPR_data_z_L2_verb)

summary(model_L2_verb_intonly) 

############################
# Analysis of the L2 SPR data with additional factor "experience" can be found in the file "follow_up_analyses_experience"
############################

q()

