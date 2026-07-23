library(lme4)
library(ordinal)
library(MASS)
library(ggplot2)
library(plyr)
library(purrr)
library(readr)

# Combining the data to correlate the higher-rated inference (bouletic or reportative) with acceptability judgements

# data files with acceptability and inference data in the non-bouletic/sollen condition
summary_bouletic_inf <- read.csv("item_summary_nb_bouletic.csv", encoding = "UTF-8") 
summary_reportative_inf <- read.csv("item_summary_nb_report.csv", encoding = "UTF-8") 

View(summary_bouletic_inf)
View(summary_reportative_inf)

str(summary_bouletic_inf)
str(summary_reportative_inf)

# double-check correlation with each inference
cor.test(
  summary_bouletic_inf$Mean_accept,
  summary_bouletic_inf$Mean_inference,
  method = "pearson"
) 

cor.test(
  summary_reportative_inf$Mean_accept,
  summary_reportative_inf$Mean_report_inference,
  method = "pearson"
) 


# as a sanity check, look at mean ratings
library(dplyr)
summary_bouletic_inf %>% summarize(overall_accept = mean(Mean_accept), overall_boul = mean(Mean_inference))
summary_reportative_inf %>% summarize(overall_accept = mean(Mean_accept), overall_rep = mean(Mean_report_inference))

# check for mismatches
setdiff(summary_bouletic_inf$item,
        summary_reportative_inf$item)

# merge data frames
item_summary_inf_combined <- merge(
  summary_bouletic_inf,
  summary_reportative_inf,
  by = c("item","Mean_accept", "SE_accept")
)

View(item_summary_inf_combined)

# fit different item level regressions to see if the combination explains more than individual inferences (Mean_inference = bouletic inference)
m1 <- lm(
  Mean_accept ~ Mean_inference,
  data = item_summary_inf_combined
)

m2 <- lm(
  Mean_accept ~ Mean_report_inference,
  data = item_summary_inf_combined
)

m3 <- lm(
  Mean_accept ~ Mean_inference +
    Mean_report_inference,
  data = item_summary_inf_combined
)

summary(m1)
summary(m2)
summary(m3)

# normalize inference ratings
item_summary_inf_combined$z_bouletic <-
  as.numeric(scale(item_summary_inf_combined$Mean_inference))

item_summary_inf_combined$z_reportative <-
  as.numeric(scale(item_summary_inf_combined$Mean_report_inference))

View(item_summary_inf_combined)

# select highest inference rating
item_summary_inf_combined$max_z_inference <-
  pmax(item_summary_inf_combined$z_bouletic,
       item_summary_inf_combined$z_reportative)

# correlation test
cor.test(item_summary_inf_combined$Mean_accept,
         item_summary_inf_combined$max_z_inference)



# rename item values for the plot:
library(dplyr)

item_summary_inf_combined <- 
  item_summary_inf_combined %>%
  mutate(item = gsub("^main-", "M", item))


library(ggplot2)

ggplot(item_summary_inf_combined,
       aes(x = Mean_inference,
           y = Mean_report_inference,
           color = Mean_accept)) +
  geom_point(size = 4) +
  scale_color_viridis_c() +
  theme_bw(base_size = 16) +
  labs(
    x = "Mean Bouletic Inference Rating",
    y = "Mean Reportative Inference Rating",
    color = "Mean\nAcceptability"
  ) 

# plot with labels
library(ggrepel)

ggplot(item_summary_inf_combined,
       aes(x = z_bouletic,
           y = z_reportative,
           color = Mean_accept)) +
  geom_hline(yintercept = 0, color = "black", alpha = 0.4, linewidth = 0.8) +
  geom_vline(xintercept = 0, color = "black", alpha = 0.4, linewidth = 0.8) +
  geom_point(size = 3) +
  geom_text_repel(aes(label = item)) +
  scale_color_viridis_c() +
  theme_bw(base_size = 16) +
  labs(
    x = "Mean Bouletic Inference Rating (z-transformed)",
    y = "Mean Reportative Inference Rating (z-transformed)",
    color = "Mean\nAcceptability"
  ) +
  xlim(-2.5, 2)


# remove outlier items with high acceptability but low inference ratings
item_summary_inf_combined_red2 <- filter(item_summary_inf_combined, !(item %in% c("M10","M18")))
View(item_summary_inf_combined_red2)

# corr with max inference
cor.test(item_summary_inf_combined_red2$Mean_accept,
         item_summary_inf_combined_red2$max_z_inference) 

# corr with bouletic inference only
cor.test(item_summary_inf_combined_red2$Mean_accept,
         item_summary_inf_combined_red2$Mean_inference) 

# corr with reportative inference only
cor.test(item_summary_inf_combined_red2$Mean_accept,
         item_summary_inf_combined_red2$Mean_report_inference) 


# model acceptability ratings based on max inference (unfiltered)
results_native <- read.csv("results_native_complete.csv", encoding = "UTF-8") 
View(results_native)

library(dplyr)

native_accept_non_boul_sollen <- filter(results_native,PennElementName == "AcceptabilityJudgment" &
                                             Value != "NULL" &
                                             condition == "non-bouletic-soll")

View(native_accept_non_boul_sollen)

# rename item values for the merge:
native_accept_non_boul_sollen <- 
  native_accept_non_boul_sollen %>%
  mutate(item = gsub("^main-", "M", item))

# merge dfs 
inference_data_acc <- left_join(
  native_accept_non_boul_sollen,
  item_summary_inf_combined,
  by = "item"
)

View(inference_data_acc)
str(inference_data_acc)

inference_data_acc$Value <- as.numeric(as.character(inference_data_acc$Value))

# fit a model to predict acceptability from max z inference
model_acc_max <- lmer(Value ~ max_z_inference + (1 | PROLIFIC_ID) + (1 | item), data = inference_data_acc)
library(lmerTest)
summary(model_acc_max) 
q()

