library(lme4)
library(ordinal)
library(MASS)
library(ggplot2)
library(plyr)
library(scales)
library(purrr)
library(readr)

# Exploratory analyses and models for the AJT, native speaker data

# read and inspect file with complete data after participant exclusion
results_native <- read.csv("results_native_complete.csv", encoding = "UTF-8") 

View(results_native)

# ---------------------------- analyze rating data --------------------------------------------------------------------------------------------------------------

# filter for rating data
library(dplyr)
results_native_accept <- filter(results_native, PennElementName == "AcceptabilityJudgment" & Value != "NULL")
View(results_native_accept)

# filter for the main experiment
results_main_accept <- filter(results_native_accept, experiment == "main")
View(results_main_accept)
str(results_main_accept)

# turn ratings into numeric values
results_main_accept$Value <- as.numeric(as.character(results_main_accept$Value))

# calculate means and SDs
mean_ratings <- results_main_accept %>% group_by(condition) %>% summarize(mean = mean(Value))
mean_ratings 

sd_ratings <- results_main_accept %>% group_by(condition) %>% summarize(sd = sd(Value))
sd_ratings

# plot acceptability ratings ("parameter" is where the context variable is stored -- bouletic vs. non-bouletic)

# aggregate data
data_main_plot <- results_main_accept %>%                     
  group_by(parameter, modal) %>%                           
  summarise(                                             
    M  = mean(Value, na.rm = TRUE),             
    N  = n(),                                   
    SE = sd(Value, na.rm = TRUE) / sqrt(N),     
    .groups = "drop"
  )

View(data_main_plot)

# make plot
p <- ggplot(data_main_plot, aes(x = parameter, y = M, group = modal)) +
  geom_line(aes(linetype = modal)) +
  geom_point(aes(shape = modal), size = 3) +
  geom_errorbar(aes(ymin = M - SE, ymax = M + SE), width = 0.2) +
  labs(
    x = "Flavours (context)",
    y = "Mean Acceptability Rating",
    shape = "Modal",
    linetype = NULL
  ) +
  coord_cartesian(ylim = c(1, 7)) +
  theme_bw(base_size = 16)

p


# modeling the rating data ------------------------------

# turn ratings into a factor 
results_main_accept$Value <- factor(results_main_accept$Value, level=c("1","2","3","4","5","6","7"), label=c("1","2","3","4","5","6","7"))

results_main_accept$modal <- factor(results_main_accept$modal)
results_main_accept$parameter <- factor(results_main_accept$parameter)

View(results_main_accept)

# change to sum-coding:
contrasts(results_main_accept$parameter) <- contr.sum
contrasts(results_main_accept$modal) <- contr.sum


# fit an ordinal mixed model with maximal effect structure
model_accept <- clmm(Value ~ parameter * modal + (1 + parameter * modal | PROLIFIC_ID) + (1 + parameter * modal | item), data = results_main_accept)
summary(model_accept) 
confint(model_accept)

# compare the model with a simpler model without varying slopes for interaction
model_accept2 <- clmm(Value ~ parameter * modal + (1 + parameter + modal | PROLIFIC_ID) + (1 + parameter + modal | item), data = results_main_accept)
summary(model_accept2)

anova(model_accept, model_accept2) 
# significantly better model fit with interaction as random effect (p < 0.001)

# fit a model without interaction as predictor, for LRT
model_accept_no_int <- clmm(Value ~ parameter + modal + (1 + parameter * modal | PROLIFIC_ID) + (1 + parameter * modal | item), data = results_main_accept)
summary(model_accept_no_int) 
confint(model_accept_no_int)

# LRT
anova(model_accept, model_accept_no_int) 

# added: investigate variation by cell:
VarCorr(model_accept)

G <- VarCorr(model_accept)$PROLIFIC_ID

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


# -----------
View(results_main_accept)
results_main_accept$Value <- as.numeric(as.character(results_main_accept$Value))

re <- ranef(model_accept)$PROLIFIC_ID
head(re)

pred <- transform(
  re,
  bouletic_muss      = `(Intercept)` + `parameternon-bouletic` + modalsoll + `parameternon-bouletic:modalsoll`,
  bouletic_soll      = `(Intercept)` + `parameternon-bouletic` - modalsoll - `parameternon-bouletic:modalsoll`,
  nonbouletic_muss   = `(Intercept)` - `parameternon-bouletic` + modalsoll - `parameternon-bouletic:modalsoll`,
  nonbouletic_soll   = `(Intercept)` - `parameternon-bouletic` - modalsoll + `parameternon-bouletic:modalsoll`
)

sapply(pred[, 5:8], sd)

# ---------------------------------------- Self-paced reading data from main experiment --------------------------------------------------------

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


# exlude RTs below 100 and over 3000 ms (if any)
results_native_SPR_cleaned <- filter(results_native_SPR_cleaned, Reading.time >= 100 & Reading.time <= 3000) 

# updated scatterplot
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
    model <- lm(Reading.time ~ word_length + region, data = .x)  # Fit the model
    .x$predicted_RT <- predict(model) # add predicted RTs
    .x$residuals <- residuals(model)  # add residuals to the group's data
    return(.x)  # Return the updated data for the group
  }) %>%
  ungroup()

# plot residual RTs for main experiment conditions ------------------------

residual_data2 <- filter(residual_data_all, experiment == "main")

# aggregate data for regions and conditions
agg_residual_data2 <- residual_data2 %>%
  group_by(region, condition) %>%
  summarise(mean_ResRT = mean(residuals),
            se = sd(residuals, na.rm = TRUE) / sqrt(n()),  
            ci = 1.96 * se,  
            .groups = "drop") 

View(agg_residual_data2)

# plot aggregated residual RTs
ggplot(agg_residual_data2, aes(x = region, y = mean_ResRT, group = condition, color = condition)) +
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

results_main_SPR_cleaned <- filter(results_native_SPR_cleaned, experiment == "main")
str(results_main_SPR_cleaned)
View(results_main_SPR_cleaned)

agg_data <- results_main_SPR_cleaned %>%
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
    title = "Mean Reading Times Across Regions",
    x = "Region",
    y = "Mean Reading Time",
    color = "Condition"
  ) +
  ylim(300,430) 

#################################################################################################################
# linear mixed models for the self-paced reading data can be found in the file "L1_L2_combined_analysis_updated"
#################################################################################################################

q()



