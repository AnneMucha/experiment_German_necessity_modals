library(readr)
library(dplyr)
library(ggplot2)

# load rating data for L1 and L2 speakers
rating_data_combined <- read.csv("rating_data_combined.csv")

# filter for main experiment
rating_combined_main <- filter(rating_data_combined, item_type == "main")

View(rating_combined_main)

# create a new column for post-hoc annotation of non-bouletic flavours
rating_combined_main <- rating_combined_main %>%
  mutate(
    non_bouletic_flavor = case_when(
      item_name %in% c("main-1","main-2","main-4","main-5","main-8","main-15","main-18","main-19","main-19","main-21","main-29") ~ "teleological",
      item_name %in% c("main-6","main-14","main-16","main-17","main-28") ~ "deontic",
      item_name %in% c("main-11","main-12","main-13") ~ "circumstantial",
      TRUE                  ~ "ambiguous"
    )
  )

# define speake group as factor
rating_combined_main$speaker_group <- factor(rating_combined_main$speaker_group, levels = c("Native speakers", "L2 speakers"))

# plot ratings by non-bouletic context types (first only for native speakers)
data_rating_native <- filter(rating_combined_main, speaker_group =="Native speakers")

# aggregating data and plotting acceptability ratings
data_rating_flavors_plot <- data_rating_native %>%                     
  group_by(flavor, non_bouletic_flavor, modal) %>%                           
  summarise(                                             
    M  = mean(rating, na.rm = TRUE),             
    N  = n(),                                            
    SE = sd(rating, na.rm = TRUE) / sqrt(N),     
    .groups = "drop"
  )

View(data_rating_flavors_plot)


p1 <- ggplot(data_rating_flavors_plot, aes(x = flavor, y = M, group = modal)) +
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
  facet_wrap(~ non_bouletic_flavor) +
  theme_bw(base_size = 16)

p1

data_rating_L2 <- filter(rating_combined_main, speaker_group =="L2 speakers")

# aggregating data and plotting acceptability ratings
data_rating_L2_flavors_plot <- data_rating_L2 %>%                     
  group_by(flavor, non_bouletic_flavor, modal) %>%                           
  summarise(                                             
    M  = mean(rating, na.rm = TRUE),             
    N  = n(),                                            
    SE = sd(rating, na.rm = TRUE) / sqrt(N),     
    .groups = "drop"
  )

View(data_rating_L2_flavors_plot)


p2 <- ggplot(data_rating_L2_flavors_plot, aes(x = flavor, y = M, group = modal)) +
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
  facet_wrap(~ non_bouletic_flavor) +
  theme_bw(base_size = 16)

p2

# plot by-item ratings
# native speakers

# aggregating data and plotting acceptability ratings
data_rating_items_plot <- data_rating_native %>%                     
  group_by(flavor, item_name, modal) %>%                           
  summarise(                                             
    M  = mean(rating, na.rm = TRUE),             
    N  = n(),                                            
    SE = sd(rating, na.rm = TRUE) / sqrt(N),     
    .groups = "drop"
  )

View(data_rating_items_plot)

p_item <- ggplot(data_rating_items_plot, aes(x = flavor, y = M, group = modal)) +
  geom_line(aes(linetype = modal)) +
  geom_point(aes(shape = modal), size = 3) +
  geom_errorbar(aes(ymin = M - SE, ymax = M + SE), width = 0.2) +
  labs(
    x = "Flavours (context)",
    y = "Mean Acceptability Rating L1 speakers",
    shape = "Modal",
    linetype = "Modal"
  ) +
  coord_cartesian(ylim = c(1, 7)) +
  facet_wrap(~ item_name) +
  theme_bw(base_size = 16)

p_item

# L2 speakers
# aggregating data and plotting acceptability ratings
data_rating_items_plot_L2 <- data_rating_L2 %>%                     
  group_by(flavor, item_name, modal) %>%                           
  summarise(                                             
    M  = mean(rating, na.rm = TRUE),             
    N  = n(),                                            
    SE = sd(rating, na.rm = TRUE) / sqrt(N),     
    .groups = "drop"
  )

View(data_rating_items_plot_L2)

p_item_L2 <- ggplot(data_rating_items_plot_L2, aes(x = flavor, y = M, group = modal)) +
  geom_line(aes(linetype = modal)) +
  geom_point(aes(shape = modal), size = 3) +
  geom_errorbar(aes(ymin = M - SE, ymax = M + SE), width = 0.2) +
  labs(
    x = "Flavours (context)",
    y = "Mean Acceptability Rating L2 speakers",
    shape = "Modal",
    linetype = "Modal"
  ) +
  coord_cartesian(ylim = c(1, 7)) +
  facet_wrap(~ item_name) +
  theme_bw(base_size = 16)

p_item_L2

# -----------------------------------------
# read SPR data for L1 and L2 speakers (already cleaned)
SPR_data_native <- read.csv("SPR_cleaned_native.csv", encoding = "UTF-8")
View(SPR_data_native)

SPR_data_L2 <- read.csv("SPR_L2_no_threshold.csv", encoding = "UTF-8") 
View(SPR_data_L2)

# reformat SPR data, include column for 'experience'
SPR_data_native <- plyr::rename(SPR_data_native, c("Order.number.of.item" = "order_number", "Label"="trial_type", "Value" = "word", "PROLIFIC_ID" = "ID",
                                                   "experiment"="item_type", "item"="item_name", "parameter"="flavor", "Sentence..or.sentence.MD5."="sentence","Reading.time"="RT"))

SPR_data_L2 <- plyr::rename(SPR_data_L2, c("word_or_rating"= "word"))

SPR_data_native$speaker_group <- "Native speakers"
SPR_data_native$experience2 <- "native"

SPR_data_L2$speaker_group <- "L2 speakers"


SPR_native_reduced <- select(SPR_data_native, c("order_number","trial_type","ID", "group","item_type", "item_name", "condition", "modal", "flavor","region","word", "RT", 
                                                "sentence","mean_rt", "sd_rt_3","word_length", "predicted_RT","residuals", "speaker_group", "experience2"))

SPR_L2_reduced <- select(SPR_data_L2, c("order_number","trial_type","ID", "group","item_type", "item_name", "condition", "modal", "flavor","region","word", "RT", 
                                        "sentence","mean_rt", "sd_rt_3","word_length", "predicted_RT","residuals", "speaker_group", "experience2"))

SPR_data_combined <- rbind(SPR_native_reduced, SPR_L2_reduced)

View(SPR_data_combined)

# add z-scored RTs
SPR_data_z <- SPR_data_combined %>%
  group_by(ID) %>%
  mutate(RT_z = as.numeric(scale(RT)))

# select data from main experiment
SPR_data_z_main <- SPR_data_z %>% filter(item_type == "main")
View(SPR_data_z_main)

# define factors and levels
SPR_data_z_main$speaker_group <- factor(SPR_data_z_main$speaker_group, levels=c("Native speakers", "L2 speakers"))
SPR_data_z_main$condition <- factor(SPR_data_z_main$condition)
SPR_data_z_main$region <- factor(SPR_data_z_main$region, levels = c("intro", "subject", "modal", "MF1", "MF2", "verb"))

# plot by item z-scored RTs for both groups
# native speakers
SPR_data_z_main_native <- filter(SPR_data_z_main, speaker_group == "Native speakers")
View(SPR_data_z_main_native)

# aggregate data 
agg_data_z_main_native <- SPR_data_z_main_native %>%
  group_by(region, condition, item_name) %>%
  summarise(mean_RT_z = mean(RT_z),
            se = sd(RT_z, na.rm = TRUE) / sqrt(n()),  
            .groups = "drop")

plot_RT_z_items_native <- ggplot(agg_data_z_main_native, aes(x = region, y = mean_RT_z, group = condition, color = condition)) +
  geom_line(linewidth = 0.5) +
  geom_point(size = 1) +
  facet_wrap(~ item_name) +
  labs(
    title = "Mean z-scored Reading Times Across Regions, Conditions and Items",
    x = "Region",
    y = "Mean z-scored Reading Time (native speakers)",
    color = "Condition"
  ) 

plot_RT_z_items_native

# L2 speakers
SPR_data_z_main_L2 <- filter(SPR_data_z_main, speaker_group == "L2 speakers")
View(SPR_data_z_main_L2)

# aggregate data 
agg_data_z_main_L2 <- SPR_data_z_main_L2 %>%
  group_by(region, condition, item_name) %>%
  summarise(mean_RT_z = mean(RT_z),
            se = sd(RT_z, na.rm = TRUE) / sqrt(n()),  
            .groups = "drop")

plot_RT_z_items_L2 <- ggplot(agg_data_z_main_L2, aes(x = region, y = mean_RT_z, group = condition, color = condition)) +
  geom_line(linewidth = 0.5) +
  geom_point(size = 1) +
  facet_wrap(~ item_name) +
  labs(
    title = "Mean z-scored Reading Times Across Regions, Conditions and Items",
    x = "Region",
    y = "Mean z-scored Reading Time (L2 speakers)",
    color = "Condition"
  ) 

plot_RT_z_items_L2

# plot RTs for only experienced L2 speakers (to see if they behave more native-like)
L2_experienced_SPR <- filter(SPR_data_z_main_L2, experience2 == ">10 years")
View(L2_experienced_SPR)

# aggregate data 
agg_data_z_main_L2_exp <- L2_experienced_SPR %>%
  group_by(region, condition, item_name) %>%
  summarise(mean_RT_z = mean(RT_z),
            se = sd(RT_z, na.rm = TRUE) / sqrt(n()),  
            .groups = "drop")

plot_RT_z_items_L2_exp <- ggplot(agg_data_z_main_L2_exp, aes(x = region, y = mean_RT_z, group = condition, color = condition)) +
  geom_line(linewidth = 0.5) +
  geom_point(size = 1) +
  facet_wrap(~ item_name) +
  labs(
    title = "Mean z-scored Reading Times Across Regions, Conditions and Items",
    x = "Region",
    y = "Mean z-scored Reading Time (L2 speakers)",
    color = "Condition"
  ) 

plot_RT_z_items_L2_exp


# inspect the acceptability ratings for clearly deontic items 
# native speakers
accept_deontic <- filter(data_rating_native, item_name %in% c("main-28","main-27", "main-16", "main-14"))
View(accept_deontic)

data_rating_deontic_plot <- accept_deontic %>%                     
  group_by(flavor, item_name, modal) %>%                           
  summarise(                                             
    M  = mean(rating, na.rm = TRUE),             
    N  = n(),                                            
    SE = sd(rating, na.rm = TRUE) / sqrt(N),     
    .groups = "drop"
  )

p5 <- ggplot(data_rating_deontic_plot, aes(x = flavor, y = M, group = modal)) +
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
  facet_wrap(~ item_name) +
  theme_bw(base_size = 16)

p5

# inspect acceptability ratings roundabout 6 for non-bouletic "sollen" 
# teleological items (native speakers)

data_rating_teleological_native_sollen_non_boul <- filter(data_rating_teleological_native, modal == "soll" & flavor == "non-bouletic")
View(data_rating_teleological_native_sollen_non_boul)

mean(data_rating_teleological_native_sollen_non_boul$rating) # 5.01

# cf. L2 speakers teleological: 5.91 

# inspect average for all items classified as deontic (native speakers)
data_rating_deontic_native_sollen_non_boul <- filter(data_rating_native, non_bouletic_flavor == 'deontic' 
                                                     & modal == "soll" & flavor == "non-bouletic")

View(data_rating_deontic_native_sollen_non_boul)
mean(data_rating_deontic_native_sollen_non_boul$rating) # 5.95

# compare L2 speakers
data_rating_deontic_L2_sollen_non_boul <- filter(data_rating_L2, non_bouletic_flavor == 'deontic' 
                                                     & modal == "soll" & flavor == "non-bouletic")

View(data_rating_deontic_L2_sollen_non_boul)
mean(data_rating_deontic_L2_sollen_non_boul$rating) # 5.88

# compare for only the selected deontic item M16 (one of the items driving the RT effect)
accept_deontic_sollen_non_boul16 <- filter(accept_deontic, modal == "soll" & flavor == "non-bouletic" & item_name == "main-16")
View(accept_deontic_sollen_non_boul16)

mean(accept_deontic_sollen_non_boul16$rating) # 6.17

# item M14 (deontic w/o inference from bouletic)
accept_deontic_sollen_non_boul14 <- filter(accept_deontic, modal == "soll" & flavor == "non-bouletic" & item_name == "main-14")
View(accept_deontic_sollen_non_boul14)

mean(accept_deontic_sollen_non_boul14$rating)
# 5.48, 6.25 for "muss" [to inspect just change the value of "modal" in the filter]

# compare with L2 ratings
accept_deontic_L2 <- filter(data_rating_L2, item_name %in% c("main-28","main-27", "main-16", "main-14"))
View(accept_deontic_L2)

accept_L2_deontic_sollen_non_boul14 <- filter(accept_deontic_L2, modal == "soll" & flavor == "non-bouletic" & item_name == "main-14")
View(accept_L2_deontic_sollen_non_boul14)

mean(accept_L2_deontic_sollen_non_boul14$rating)
# 5.53, 5 for "muss"

# more ratings for individual items in the non-bouletic/sollen condition
item_21_ratings <- filter(rating_combined_main, item_name== "main-21")
View(item_21_ratings)

# aggregate data for all conditions
item_21_ratings_summary <- item_21_ratings %>%                     
  group_by(flavor, modal, speaker_group) %>%                           
  summarise(                                             
    M  = mean(rating, na.rm = TRUE),             
    N  = n(),                                            
    SE = sd(rating, na.rm = TRUE) / sqrt(N),     
    .groups = "drop"
  )

View(item_21_ratings_summary)

p21 <- ggplot(item_21_ratings_summary, aes(x = flavor, y = M, group = modal)) +
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

p21

# ----------------

# reported in the manuscript
item_15_ratings <- filter(rating_combined_main, item_name== "main-15")

# aggregate data
item_15_ratings_summary <- item_15_ratings %>%                     
  group_by(flavor, modal, speaker_group) %>%                           
  summarise(                                             
    M  = mean(rating, na.rm = TRUE),             
    N  = n(),                                            
    SE = sd(rating, na.rm = TRUE) / sqrt(N),     
    .groups = "drop"
  )

View(item_15_ratings_summary)

p15 <- ggplot(item_15_ratings_summary, aes(x = flavor, y = M, group = modal)) +
  geom_line(aes(linetype = modal)) +
  geom_point(aes(shape = modal), size = 3) +
  geom_errorbar(aes(ymin = M - SE, ymax = M + SE), width = 0.2) +
  labs(
    x = "Flavours (context)",
    y = "Mean Acceptability Rating for Item 15 (teleological)",
    shape = "Modal",
    linetype = "Modal"
  ) +
  coord_cartesian(ylim = c(1, 7)) +
  facet_wrap(~ speaker_group) +
  theme_bw(base_size = 16)

p15

# -----------------

item_1_ratings <- filter(rating_combined_main, item_name== "main-1")

# aggregate data
item_1_ratings_summary <- item_1_ratings %>%                     
  group_by(flavor, modal, speaker_group) %>%                           
  summarise(                                             
    M  = mean(rating, na.rm = TRUE),             
    N  = n(),                                            
    SE = sd(rating, na.rm = TRUE) / sqrt(N),     
    .groups = "drop"
  )

View(item_1_ratings_summary)

p1 <- ggplot(item_1_ratings_summary, aes(x = flavor, y = M, group = modal)) +
  geom_line(aes(linetype = modal)) +
  geom_point(aes(shape = modal), size = 3) +
  geom_errorbar(aes(ymin = M - SE, ymax = M + SE), width = 0.2) +
  labs(
    x = "Flavours (context)",
    y = "Mean Acceptability Rating for Item 1 (teleological)",
    shape = "Modal",
    linetype = "Modal"
  ) +
  coord_cartesian(ylim = c(1, 7)) +
  facet_wrap(~ speaker_group) +
  theme_bw(base_size = 16)

p1

q()
