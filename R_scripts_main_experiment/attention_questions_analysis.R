library(plyr)
library(purrr)
library(readr)
library(dplyr)

# read data

native_data <- read.csv("results_native_complete.csv")
L2_data <- read.csv("L2_data_with_demographics.csv")

# sanity checks
View(native_data)
View(L2_data)

n_distinct(native_data$PROLIFIC_ID)
n_distinct(L2_data$ID)

# filter and combine data
# unify column names
data_native <- plyr::rename(native_data, c("Order.number.of.item" = "order_number","PennElementName" = "controller_type", "Label"="trial_type", "Value" = "word", "PROLIFIC_ID" = "ID",
                                                   "experiment"="item_type", "item"="item_name", "parameter"="flavor", "Sentence..or.sentence.MD5."="sentence","Reading.time"="RT"))

data_native_attention_main <- filter(data_native, trial_type == "SPR-trial-attention" & item_type == "main")

data_L2 <- plyr::rename(L2_data, c("word_or_rating"= "word"))

data_L2_attention_main <- filter(data_L2, trial_type == "SPR-trial-attention" & item_type == "main")

View(data_native_attention_main)
View(data_L2_attention_main)

# add column for speaker group
data_native_attention_main$speaker_group <- "Native speakers"
data_L2_attention_main$speaker_group <- "L2 speakers"

# select columns
data_native_attention_reduced <- select(data_native_attention_main, c("order_number","trial_type","ID","controller_type", "item_type", "item_name", "condition", "modal", "flavor", "RT", 
                                                "sentence", "speaker_group"))

data_L2_attention_reduced <- select(data_L2_attention_main, c("order_number","trial_type","ID","controller_type", "item_type", "item_name", "condition", "modal", "flavor", "RT", 
                                        "sentence", "speaker_group"))

# combine and inspect data from speaker groups
data_attention_combined <- rbind(data_native_attention_reduced, data_L2_attention_reduced)

View(data_attention_combined)

#  filter for only the attention questions
data_attention_questions <- filter(data_attention_combined, controller_type == "Question")

View(data_attention_questions)

# visualise data
library(ggplot2)

cbp1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73", 
          "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


plot_attention <- data_attention_questions %>%
  arrange(RT) %>%
  ggplot(aes(x=speaker_group, fill=RT)) +
  geom_bar(position=position_fill(), colour="black", alpha = 0.8) +
  xlab("tbd") +
  ylab("tbd") +
  theme_bw() +
  scale_fill_manual(values = cbp1) +
  scale_alpha_manual(values = c(1, .5), breaks = NULL) +
  facet_wrap(~condition)

print(plot_attention)

# summary
attention_SPR_points <- data_attention_questions %>% group_by(speaker_group, flavor) %>%
  dplyr::summarise(count_ones = sum(RT == 1, na.rm = TRUE), 
            count_zeros = sum(RT == 0, na.rm = TRUE),
            prop_false = round(count_zeros / (count_zeros + count_ones),3))


View(attention_SPR_points)

# error rate by speaker group
error_rate_groups <- data_attention_questions %>% group_by(speaker_group) %>%
  dplyr::summarise(count_ones = sum(RT == 1, na.rm = TRUE), 
            count_zeros = sum(RT == 0, na.rm = TRUE),
            prop_false = round(count_zeros / (count_zeros + count_ones),3))

View(error_rate_groups) 


# run a model
str(attention_SPR_points)

data_attention_questions$speaker_group <- factor(data_attention_questions$speaker_group)
data_attention_questions$modal <- factor(data_attention_questions$modal)
data_attention_questions$flavor <- factor(data_attention_questions$flavor)
data_attention_questions$RT <- factor(data_attention_questions$RT)

library(lme4)

model_attention <- glmer(RT ~ modal * flavor * speaker_group + 
                           (1 + flavor | ID) + (1 | item_name),
                         data = data_attention_questions, family = "binomial", control=glmerControl(optimizer="bobyqa"))

summary(model_attention)

model_attention_simpl <- glmer(RT ~ modal + flavor + speaker_group + 
                           (1 + flavor | ID) + (1 | item_name),
                         data = data_attention_questions, family = "binomial", control=glmerControl(optimizer="bobyqa"))

summary(model_attention_simpl)

# check if the difference between context types is significant for L2 speakers
library(emmeans)

pairs(
  emmeans(model_attention,
          ~ flavor | speaker_group,
          mode = "latent"),
  adjust = "holm"
) 

# check wrong answers by speaker group, context, item
attention_points_item <- data_attention_questions %>% group_by(speaker_group, flavor, item_name) %>%
  summarise(count_ones = sum(RT == 1, na.rm = TRUE), count_zeros = sum(RT == 0, na.rm = TRUE))

View(attention_points_item)

# some items elicited more wrong answers (items 19, 11, 21 -- these seem to be items where the answer options are very similar), 
# but that is independent of context/flavor condition or speaker group

q()

