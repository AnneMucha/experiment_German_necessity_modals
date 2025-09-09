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

# fit an ordinal mixed model with maximal effect structure
model_accept <- clmm(Value ~ parameter * modal + (1 + parameter * modal | PROLIFIC_ID) + (1 + parameter * modal | item), data = results_main_accept)
summary(model_accept) 
confint(model_accept)


# compare the model with a simpler model without varying slopes for interaction
model_accept2 <- clmm(Value ~ parameter * modal + (1 + parameter + modal | PROLIFIC_ID) + (1 + parameter + modal | item), data = results_main_accept)
summary(model_accept2)

anova(model_accept, model_accept2) 
# significantly better model fit with interaction as random effect (p < 0.001)

# fit a model without interaction as predictor
model_accept_no_int <- clmm(Value ~ parameter + modal + (1 + parameter + modal | PROLIFIC_ID) + (1 + parameter + modal | item), data = results_main_accept)
summary(model_accept_no_int) 
confint(model_accept_no_int)

# compare models with and without interaction
anova(model_accept, model_accept_no_int) 
# p < 0.001 


# ----------------------------------------  analyze Self-paced reading data for main experiment --------------------------------------------------------

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


# --------------- modeling the results with raw RTs -------------------------------------------------------------------------------

# review data set
View(results_main_SPR_cleaned)


# ----- model for each critical region ---------------


data_modal <- filter(results_main_SPR_cleaned, region == "modal")
View(data_modal)

# fitting intercepts-only model to see if there are potential effects
model_modal_raw <- lmer(Reading.time ~ parameter * modal + (1| PROLIFIC_ID) + (1 + parameter | item), data = data_modal)
summary(model_modal_raw)
confint(model_modal_raw, method = "Wald")
# no significant effects in region 3

data_MF1 <- filter(results_main_SPR_cleaned, region == "MF1")
View(data_MF1)

# fitting linear model with maximal random effects structure that fits the data
model_MF1_raw <- lmer(Reading.time ~ parameter * modal + (1 + parameter + modal| PROLIFIC_ID) + (1 + parameter | item), data = data_MF1)
summary(model_MF1_raw)
confint(model_MF1_raw, method = "Wald")

library(sjPlot)
plot_model(model_MF1_raw, type = "re", sort.est = TRUE) 

# model indicates a significant effect of modal 
# interaction close to significant

# fit a model without interaction:

model_MF1_raw_no_int <- lmer(Reading.time ~ parameter + modal + (1 + parameter + modal | PROLIFIC_ID) + (1 + parameter | item) , data= data_MF1)
summary(model_MF1_raw_no_int) 
confint(model_MF1_raw_no_int, method = "Wald")

anova(model_MF1_raw_no_int, model_MF1_raw) # model with interaction does not fit significantly better 


# compare models with and without modal as predictor
model_MF1_raw_no_mod <- lmer(Reading.time ~ parameter + (1  + parameter + modal | PROLIFIC_ID) + (1 + parameter| item) , data= data_MF1)

anova(model_MF1_raw_no_mod, model_MF1_raw_no_int) # no significant difference in model fit

# model with log-transformed RTs
model_MF1_log2 <- lmer(log(Reading.time) ~ parameter * modal + (1  + modal| PROLIFIC_ID) + (1 + parameter | item), data = data_MF1) # best model

# difference not significant
summary(model_MF1_log2)
confint(model_MF1_log2, method = "Wald")

model_MF1_log_no_int <- lmer(log(Reading.time) ~ parameter + modal + (1  + modal| PROLIFIC_ID) + (1 + parameter | item), data = data_MF1)
summary(model_MF1_log_no_int)

anova(model_MF1_log_no_int, model_MF1_log2) 

# conclusion: no reliable effects in region "MF1"

# region 5 -------------------------
data_MF2 <- filter(results_main_SPR_cleaned, region == "MF2")
View(data_MF2)

# fit a model with maximal random effects structure
model_MF2_max <- lmer(Reading.time ~ parameter * modal  + (1 + parameter * modal | PROLIFIC_ID) + (1 + parameter * modal | item), data = data_MF2)  
summary(model_MF2_max)

# this model overfits

# plot random effects:

library(sjPlot)
plot_model(model_MF2_max, type = "re") 

# investigate variation in random effects
summary(model_MF2_max)$varcor

# try a model with only interaction as random slopes

# best-fitting model:

model_MF2_raw <- lmer(Reading.time ~ parameter * modal + (1 | PROLIFIC_ID) + (1 + parameter | item), data = data_MF2)
summary(model_MF2_raw) 
confint(model_MF2_raw, method = "Wald") 
# significant interaction between the factors

# check if the model is better than one without varying slopes:
model_MF2_raw_noslope <- lmer(Reading.time ~ parameter * modal + (1 | PROLIFIC_ID) + (1 | item), data = data_MF2)

anova(model_MF2_raw_noslope, model_MF2_raw) 
# varying slopes significantly improve the model

# investigate effect size
library(MuMIn)
r.squaredGLMM(model_MF2_raw)  

# fit a model without interaction for comparison
model_MF2_raw_no_int <- lmer(Reading.time ~ parameter + modal + (1 | PROLIFIC_ID) + (1 + parameter| item) , data= data_MF2)

# comparing models with and without interaction
anova(model_MF2_raw_no_int, model_MF2_raw) 
# significantly better model fit with interaction as predictor

# ------- model data for final region --------------

data_verb <- filter(results_main_SPR_cleaned, region == "verb")
View(data_verb)

# best-fitting model:
model_verb_raw <- lmer(Reading.time ~ parameter * modal + (1 + parameter| PROLIFIC_ID) + (1 + parameter + modal| item), data = data_verb)
summary(model_verb_raw)
confint(model_verb_raw, method = "Wald")

# no significant effects in this region 



# ---------------------- modeling with residual RTs as a sanity check ------------------------------------------------------------

# fitting models for all critical regions ---------------------------------

# inspect data set
View(residual_data2)

# regions of interest: modal, MF1, MF2, verb
# all models identified as best fits by backwards reduction from maximal model

data_modal <- filter(residual_data2, region == "modal")
View(data_modal)

# best-fitting model:
model_SPR_modal <- lmer(residuals ~ parameter * modal +  (1 + parameter | item), data = data_modal)
summary(model_SPR_modal)
confint(model_SPR_modal, method = "Wald")

# Region 3 (modal): no significant effects

data_MF1 <- filter(residual_data2, region == "MF1")
View(data_MF1)

model_SPR_MF1 <- lmer(residuals ~ parameter * modal +  (1 | item), data = data_MF1)
summary(model_SPR_MF1)
confint(model_SPR_MF1, method = "Wald")

# Region 4: no significant effects

data_MF2 <- filter(residual_data2, region == "MF2")
View(data_MF2)

model_SPR_MF2 <- lmer(residuals ~ parameter * modal  + (1 + parameter | item), data = data_MF2)
summary(model_SPR_MF2)
confint(model_SPR_MF2, method = "Wald")

# Region 5: model confirms significant interaction between context and modal 

# fit model without interaction as predictor
model_SPR_MF2_no_int <- lmer(residuals ~ parameter + modal + (1 + parameter| item), data= data_MF2)

# model comparison
anova(model_SPR_MF2, model_SPR_MF2_no_int)
# significantly better model fit


data_verb <- filter(residual_data2, region == "verb")
View(data_verb)

model_SPR_verb <- lmer(residuals ~ parameter * modal + (1 + parameter| item), data = data_verb)
summary(model_SPR_verb)
confint(model_SPR_verb, method = "Wald")

# approaching significance, double-check with model comparison

# fit a model without interaction
model_SPR_verb_no_int <- lmer(residuals ~ parameter + modal + (1 + parameter | item), data= data_verb)


anova(model_SPR_verb , model_SPR_verb_no_int)
# interaction not quite significant at the 0.05 level (p = 0.06285)

q()



