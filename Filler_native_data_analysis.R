library(lme4)
library(ordinal)
library(MASS)
library(ggplot2)
library(plyr)
library(car)
library(data.table)
library(scales)
library(purrr)
library(readr)

# read and inspect file with complete data after participant exclusion
results_native <- read.csv("results_native_complete.csv", encoding = "UTF-8") 
View(results_native)

# filter for rating data
library(dplyr)
results_native_accept <- filter(results_native, PennElementName == "AcceptabilityJudgment" & Value != "NULL")
View(results_native_accept)

# filter for the filler experiment
results_filler_accept <- filter(results_native_accept, experiment == "filler")
View(results_filler_accept)

# turn ratings into numeric values
results_filler_accept$Value <- as.numeric(as.character(results_filler_accept$Value))

# calculate means and SDs
mean_ratings <- results_filler_accept %>% group_by(condition) %>% summarize(mean = mean(Value))
mean_ratings 

sd_ratings <- results_filler_accept %>% group_by(condition) %>% summarize(sd = sd(Value))
sd_ratings

# plot acceptability ratings -----------------

# aggregate data
data_filler_plot <- results_filler_accept %>%                     
  group_by(condition) %>%                           
  summarise(                                             
    M  = mean(Value, na.rm = TRUE),             
    N  = n(),                                   
    SE = sd(Value, na.rm = TRUE) / sqrt(N),     
    .groups = "drop"
  )

View(data_filler_plot)

# make plot
p <- ggplot(data_filler_plot, aes(x = condition, y = M, group = condition)) +
  geom_point(aes(shape = condition), size = 3) +
  geom_errorbar(aes(ymin = M - SE, ymax = M + SE), width = 0.2) +
  labs(
    x = "Modal",
    y = "Mean Acceptability Rating",
    shape = "Modal",
    linetype = NULL
  ) +
  coord_cartesian(ylim = c(1, 7)) +
  theme_bw(base_size = 16)

p

# modeling the rating data ------------------------------

# turn ratings into a factor 
results_filler_accept$Value <- factor(results_filler_accept$Value, level=c("1","2","3","4","5","6","7"), label=c("1","2","3","4","5","6","7"))

# fit an ordinal mixed model with maximal effect structure
model_accept_filler <- clmm(Value ~ condition + (1 + condition | PROLIFIC_ID) + (1 + condition | item), data = results_filler_accept)
summary(model_accept_filler) # significant difference: p = 0.0244
confint(model_accept_filler) # [-0.9513899, -0.06557206]

# analysing SPR data ----------------------------------------

# filter for SPR data
results_native_SPR <- filter(results_native, PennElementName == "DashedSentence" & Label != "practice-SPR")

# change confusing column name
results_native_SPR <- plyr::rename(results_native_SPR,c('Parameter'='region'))

# define levels for regions
results_native_SPR$region <- factor(results_native_SPR$region, level = c("1","2", "3", "4", "5", "6"), 
                                    label = c("intro","subject", "modal", "MF1", "MF2", "verb"))


# turn RTs into numeric values
results_native_SPR$Reading.time <- as.numeric(as.character(results_native_SPR$Reading.time))

# scatterplot for raw reading times 
ggplot(results_native_SPR, aes(x=region, y=Reading.time, color=condition)) + geom_point() 


# exclude outliers more than 3 SDs away from the mean
results_native_SPR_cleaned <- results_native_SPR %>%
  group_by(region, condition) %>% 
  mutate(
    mean_rt = mean(Reading.time, na.rm = TRUE),   
    sd_rt = sd(Reading.time, na.rm = TRUE),      
    diff = abs(Reading.time - mean_rt),
    sd_rt_3 = 3 * sd_rt
  ) %>%
  filter(
    abs(Reading.time - mean_rt) <= sd_rt_3      
  )

View(results_native_SPR_cleaned)

# exclude RTs below 100 ms (if any)
results_native_SPR_cleaned <- filter(results_native_SPR_cleaned, Reading.time >= 100) 

# updated scatter plot
ggplot(results_native_SPR_cleaned, aes(x=region, y=Reading.time, color=condition)) + geom_point()

# Calculate residual reading times -----------------
# Value column contains the sentences, turn to character values 
results_native_SPR_cleaned$Value <- as.character(results_native_SPR_cleaned$Value)

#create a new column for word length
results_native_SPR_cleaned$word_length <- nchar(results_native_SPR_cleaned$Value)

# new df with column for residual RTs
residual_data_all <- results_native_SPR_cleaned %>%
  group_by(PROLIFIC_ID) %>%
  group_modify(~ {
    model <- lm(Reading.time ~ word_length + region, data = .x)  
    .x$predicted_RT <- predict(model) 
    .x$residuals <- residuals(model)  
    return(.x)  
  }) %>%
  ungroup()

# plot residual RTs for filler conditions ------------------------

residual_data_filler <- filter(residual_data_all, experiment == "filler")

# aggregate data for regions and conditions
agg_residual_data_filler <- residual_data_filler %>%
  group_by(region, condition) %>%
  summarise(mean_ResRT = mean(residuals),
            se = sd(residuals, na.rm = TRUE) / sqrt(n()),  
            ci = 1.96 * se,  
            .groups = "drop") 

View(agg_residual_data_filler)

# plot aggregated residual RTs
ggplot(agg_residual_data_filler, aes(x = region, y = mean_ResRT, group = condition, color = condition)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  geom_ribbon(aes(ymin = mean_ResRT - se, ymax = mean_ResRT + se,fill = condition), alpha = 0.1, color = NA) +
  labs(
    title = " ",
    x = "Region",
    y = "Mean Residual Reading Time",
    color = "Condition"
  ) + 
  theme(legend.title = element_text(size=14), 
        legend.text = element_text(size=13),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=11),
        axis.title.x = element_text(size=13),
        axis.title.y = element_text(size=13))

# plot the raw reading times ---------------------------------------------------

results_filler_SPR_cleaned <- filter(results_native_SPR_cleaned, experiment == "filler")
View(results_native_SPR_cleaned)

agg_data <- results_filler_SPR_cleaned %>%
  group_by(region, condition) %>%
  summarise(mean_RT = mean(Reading.time),
            se = sd(Reading.time, na.rm = TRUE) / sqrt(n()),  
            ci = 1.96 * se, 
            .groups = "drop")

View(agg_data)

ggplot(agg_data, aes(x = region, y = mean_RT, group = condition, color = condition)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = mean_RT - se, ymax = mean_RT + se), width = 0.2) +
  labs(
    title = "Native speakers",
    x = "Region",
    y = "Mean Reading Time",
    color = "Condition"
  ) 

# --------------- modelling the results with raw RTs -------------------------------------------------------------------------------
# ----- model for each region where the plot suggests differences ---------------

# region "intro" (no difference expected here, not a critical region)
data_intro <- filter(results_filler_SPR_cleaned, region == "intro") 
View(data_intro)

model_intro_filler <- lmer(Reading.time ~ condition + (1 + condition | PROLIFIC_ID) + (1 + condition | item), data = data_intro)
summary(model_intro_filler) # t = -1.721
confint(model_accept_filler, method = "Wald") # [-0.9513899, -0.06557206]

# model comparison:
model_intro_filler_reduced <- lmer(Reading.time ~ (1 + condition | PROLIFIC_ID) + (1 + condition | item), data = data_intro)
anova(model_intro_filler, model_intro_filler_reduced) # p = 0.0899

# region MF1
data_MF1 <- filter(results_filler_SPR_cleaned, region == "MF1")
View(data_MF1)

model_MF1_filler <- lmer(Reading.time ~ condition + (1 + condition | PROLIFIC_ID) + (1 + condition | item), data = data_MF1)
summary(model_MF1_filler) # t = -2.462
confint(model_MF1_filler, method = "Wald") # [-28.61345,-3.247643]

# model comparison:
model_MF1_filler_reduced <- lmer(Reading.time ~ (1 + condition | PROLIFIC_ID) + (1 + condition | item), data = data_MF1)
anova(model_MF1_filler, model_MF1_filler_reduced) # p = 0.01844 *

# region MF2 (sanity check and because it's a critical region)
data_MF2 <- filter(results_filler_SPR_cleaned, region == "MF2")
View(data_MF2)

# model w/o by-item random slopes (otherwise s.f.)
model_MF2_filler <- lmer(Reading.time ~ condition + (1 + condition| PROLIFIC_ID) + (1 | item), data = data_MF2)
summary(model_MF2_filler) # t= -1.041
confint(model_MF2_filler, method = "Wald") # [-18.27703, 5.5954]

# confirmed: no significant difference




