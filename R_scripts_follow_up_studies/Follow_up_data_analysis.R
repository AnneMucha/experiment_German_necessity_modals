library(lme4)
library(ordinal)
library(MASS)
library(ggplot2)
library(plyr)
library(purrr)
library(readr)

# Data analysis of the follow up rating studies (by example of the "bouletic" inference study)

# read and inspect file with complete data after participant exclusion
results_norming <- read.csv("results_norming_complete.csv", encoding = "UTF-8") 

View(results_norming)

# ---------------------------- analyze rating data --------------------------------------------------------------------------------------------------------------

# filter for rating data
library(dplyr)
ratings_norming <- filter(results_norming, PennElementName == "AcceptabilityJudgment" & Value != "NULL" & item != "NULL")
View(ratings_norming)

n_distinct(ratings_norming$PROLIFIC_ID) 

# turn ratings into numeric values
ratings_norming$Value <- as.numeric(as.character(ratings_norming$Value))

# calculate means and SDs
mean_ratings <- ratings_norming %>% group_by(condition) %>% summarize(mean = mean(Value))
mean_ratings 

sd_ratings <- ratings_norming %>% group_by(condition) %>% summarize(sd = sd(Value))
sd_ratings

# plot acceptability ratings ("parameter" is where the context variable is stored -- bouletic vs. non-bouletic)

# aggregate data
data_norming_plot <- ratings_norming %>%                     
  group_by(condition, experiment) %>%                           
  summarise(                                             
    M  = mean(Value, na.rm = TRUE),             
    N  = n(),                                   
    SE = sd(Value, na.rm = TRUE) / sqrt(N),     
    .groups = "drop"
  )

View(data_norming_plot)

# make plot
p <- ggplot(data_norming_plot, aes(x = condition, y = M)) +
  #geom_line(aes(linetype = parameter)) +
  geom_point(aes(shape = experiment), size = 3) +
  geom_errorbar(aes(ymin = M - SE, ymax = M + SE), width = 0.2) +
  labs(
    x = "Condition",
    y = "Mean Acceptability Rating",
    shape = "experiment",
    linetype = NULL
  ) +
  facet_wrap(~ experiment, scales = "free_x") +
  coord_cartesian(ylim = c(1, 7)) +
  theme_bw(base_size = 16)

p


# checking for difference between contexts ------------------------------
ratings_norming_targets <- filter(ratings_norming, experiment == "main")
View(ratings_norming_targets)

# turn ratings into a factor 
ratings_norming_targets$Value <- factor(ratings_norming_targets$Value, level=c("1","2","3","4","5","6","7"), label=c("1","2","3","4","5","6","7"))
ratings_norming_targets$parameter <- factor(ratings_norming_targets$parameter)

# sum-code contrasts
contrasts(ratings_norming_targets$parameter) <- contr.sum

# fit an ordinal mixed model with maximal effect structure
model_norming <- clmm(Value ~ parameter + (1 + parameter | PROLIFIC_ID) + (1 + parameter | item), data = ratings_norming_targets)

summary(model_norming) 
confint(model_norming)


# plot by item --------------
# turn ratings back into numeric values
ratings_norming_targets$Value <- as.numeric(as.character(ratings_norming_targets$Value))

item_summary <- ratings_norming_targets %>%
  group_by(experiment, item, condition) %>%
  summarise(
    M = mean(Value, na.rm = TRUE),
    SE = sd(Value, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

View(item_summary)

# plotting by item
ggplot(item_summary,
       aes(x = item, y = M, color = condition, group = condition)) +
  geom_point(size = 2) +
  geom_line() +
  geom_errorbar(aes(ymin = M - SE, ymax = M + SE),
                width = 0.2) +
  facet_wrap(~experiment, scales = "free_x") +
  coord_cartesian(ylim = c(1, 7)) +
  labs(
    x = "Item",
    y = "Mean Bouletic Inference Rating",
    color = "Condition"
  ) +
  theme_bw(base_size = 16) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# correlate ratings and bouletic inference
# read native speaker data
results_native <- read.csv("results_native_complete.csv", encoding = "UTF-8") 

View(results_native)
View(ratings_norming_targets)

# filter rating data:
results_native_ratings_non_boul <- filter(results_native, PennElementName == "AcceptabilityJudgment" &
                                                               Value != "NULL" & 
                                                               item != "NULL" &
                                                              experiment == "main" & 
                                                               condition == "non-bouletic-soll") 

# select rating and item
results_native_ratings_non_boul <- dplyr::select(results_native_ratings_non_boul, c("Value","item"))

results_native_ratings_non_boul <- plyr::rename(results_native_ratings_non_boul, c("Value"= "acceptability"))

View(results_native_ratings_non_boul)

results_native_ratings_non_boul$acceptability <- as.numeric(as.character(results_native_ratings_non_boul$acceptability))

item_summary_ratings <- results_native_ratings_non_boul %>%
  group_by(item) %>%
  summarise(
    Mean_accept = mean(acceptability, na.rm = TRUE),
    SE_accept = sd(acceptability, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

View(item_summary_ratings)

# filter norming data

results_norming_non_boul <- filter(ratings_norming_targets, parameter == "non-bouletic")

View(results_norming_non_boul)

# select rating and item
results_norming_non_boul <- dplyr::select(results_norming_non_boul, c("Value","item"))

results_norming_non_boul <- plyr::rename(results_norming_non_boul, c("Value"= "bouletic.inference"))

results_norming_non_boul$bouletic.inference <- as.numeric(as.character(results_norming_non_boul$bouletic.inference))

item_summary_inference <- results_norming_non_boul %>%
  group_by(item) %>%
  summarise(
    Mean_inference = mean(bouletic.inference, na.rm = TRUE),
    SE_inference = sd(bouletic.inference, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

View(item_summary_inference)

# check for mismatches
setdiff(item_summary_ratings$item,
        item_summary_inference$item)

# merge data frames
item_summary_nb <- merge(
  item_summary_ratings,
  item_summary_inference,
  by = "item"
)

View(item_summary_nb)

# write csv file for combined analysis of inferences
write.csv(item_summary_nb,".../item_summary_nb_bouletic.csv", row.names = FALSE)

# reshape data for plotting
library(tidyr)
library(dplyr)

item_summary_long <- item_summary_nb %>%
  pivot_longer(
    cols = c(Mean_accept, Mean_inference),
    names_to = "measure",
    values_to = "mean"
  ) %>%
  mutate(
    se = ifelse(measure == "Mean_accept",
                SE_accept,
                SE_inference),
    measure = dplyr::recode(
      measure,
      Mean_accept = "Acceptability",
      Mean_inference = "Inference"
    )
  )

View(item_summary_long)

ggplot(item_summary_long,
       aes(x = item,
           y = mean,
           color = measure,
           group = measure)) +
  geom_point(size = 2) +
  geom_line() +
  geom_errorbar(aes(ymin = mean - se,
                    ymax = mean + se),
                width = 0.2) +
  coord_cartesian(ylim = c(1, 7)) +
  labs(
    x = "Item",
    y = "Mean Rating non-bouletic + sollen",
    color = NULL
  ) +
  theme_bw(base_size = 16) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# correlation test
cor.test(
  item_summary_nb$Mean_accept,
  item_summary_nb$Mean_inference,
  method = "pearson"
)

# double-check with linear model
m <- lm(Mean_inference ~ Mean_accept,
        data = item_summary_nb)

summary(m)

ggplot(item_summary_nb,
       aes(x = Mean_accept,
           y = Mean_inference)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = TRUE) +
  theme_bw(base_size = 16) +
  labs(
    x = "Mean Acceptability Rating",
    y = "Mean Inference Rating"
  )

summary(item_summary_nb$Mean_accept)
sd(item_summary_nb$Mean_accept)

summary(item_summary_nb$Mean_inference)
sd(item_summary_nb$Mean_inference)


q()

