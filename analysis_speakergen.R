# load libraries
library(tidyverse) 
library(Rmisc) 
library(lme4)
library(car)
library(emmeans)
library(interactions)

# load data
data <- read_csv("speakergen_combined_clean.csv", 
                 col_types = cols(know = col_factor(),
                                  lang = col_factor()))

# processing data
## median split for homogeneity measure
model_data <- model_data %>%
  group_by(item) %>%
  mutate(response = if_else(item %in% c("q_homo1", "q_homo2"),
                            as.integer(response >= median(response, na.rm = TRUE)), # median split
                            response)) %>%
  ungroup()

## recode items 
model_data <- model_data %>%
  dplyr::group_by(ID) %>%
  dplyr::mutate(item = factor(seq.int(1:8))) %>% 
  ungroup()

### MODELING ###

# adults
m_adult <- glmer(response ~ know * lang + (1|item) + (1|ID), 
                 data = subset(model_data, age_group == "adult"), 
                 family = binomial, glmerControl(optimizer = "bobyqa", 
                                                 optCtrl=list(maxfun=2e5)))
# adult summary
Anova(m_adult)

# children
m_child <- glmer(response ~ know * lang * center_age + (1|item) + (1|ID), 
                 data = subset(model_data, age_group == "child"), 
                 family = binomial, glmerControl(optimizer = "bobyqa", 
                                                 optCtrl=list(maxfun=2e5)))
# child summary
Anova(m_child)

# combined
m_both <- glmer(response ~ know * lang * age_group + (1|item) + (1|ID), 
                 data = model_data, 
                 family = binomial, glmerControl(optimizer = "bobyqa", 
                                                 optCtrl=list(maxfun=2e5)))
# child summary
Anova(m_both)

# summary table of mean response
means <- summarySE(model_data, measurevar = "response", 
               groupvars = c("lang", "know", "age_group"), na.rm = TRUE) %>%
  mutate(response = round(response, 2)) %>% 
  mutate('95% CI' = paste("[", round(response - ci, 2), ", ", 
                          round(response + ci, 2), "]", sep = "")) %>% 
  select(-c(sd, se, ci)) ## can rename variables and make table using kable

# contrast testing
## adults
emm_adult <- emmeans(m_adult, ~ know * lang)
d <- pairs(emm_adult, by = "know")

## children
emm_child <- emmeans(m_child, ~ know * lang)
pairs(emm_child, by = "know")

# after looking at figures, I decided to do additional post hoc testing to 
# explore developmental trajectory using Johnson-Neyman

## filter to just generic condition children's data
data_gen_child <- model_data %>%
  filter(lang == "Generic") %>% 
  filter(age_group == "child")

# jn wants numeric predictor
data_gen_child$know <- as.numeric(data_gen_child$know)

# run model
test_generic <- glmer(response ~ know*age_exact + (1|ID), 
                      data = data_gen_child, 
                      family = binomial, 
                      glmerControl(optimizer = "bobyqa", 
                                   optCtrl = list(maxfun = 2e5)))

#run jn analysis
jn_gen <- johnson_neyman(test_generic,
                         pred = know, 
                         modx = age_exact,
                         mod.range = range(data_gen_child$age_exact),
                                       title = "Generic Condition")
# bounds of significance
jn_gen$bounds # 7.123496+


## checking for specific condition
data_spec_child <- model_data %>%
  filter(lang == "Specific") %>% 
  filter(age_group == "child")

# jn wants numeric predictor
data_spec_child$know <- as.numeric(data_spec_child$know)

# run model
test_specific <- glmer(response ~ know*age_exact + (1|ID), 
                      data = data_spec_child, 
                      family = binomial, 
                      glmerControl(optimizer = "bobyqa", 
                                   optCtrl = list(maxfun = 2e5)))

#run jn analysis
jn_spec <- johnson_neyman(test_specific,
                         pred = know, 
                         modx = age_exact,
                         mod.range = range(data_spec_child$age_exact),
                         title = "Generic Condition")
# bounds of significance
jn_spec$bounds #NS
