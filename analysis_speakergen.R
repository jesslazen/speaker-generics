# load libraries
library(tidyverse) 
library(Rmisc) 
library(lme4)
library(car)
library(emmeans)
library(interactions)
library(showtext)
library(ggpubr)
library(cowplot)

#website font
font_add_google("Atkinson Hyperlegible", "atk hyper")
showtext_auto()

# load data
data <- read_csv("data/speakergen_combined_clean.csv", 
                 col_types = cols(know = col_factor(),
                                  lang = col_factor()))

# processing data
## median split for homogeneity measure
### question 1
median_q_homo1 <- median(data$response[data$item == "q_homo1"], na.rm = TRUE)
model_data <- data %>%
  mutate(response = if_else(
    item == "q_homo1",
    as.integer(response >= median_q_homo1),
    response))

### question 2
median_q_homo2 <- median(data$response[data$item == "q_homo2"], na.rm = TRUE)
model_data <- model_data %>%
  mutate(response = if_else(
    item == "q_homo2",
    as.integer(response >= median_q_homo2),
    response))

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
                         title = "Specific Condition")
# bounds of significance
jn_spec$bounds #NS

#jn figure
jn_plot_gen <- jn_gen$plot + 
  theme(legend.position = "none", 
        text = element_text(family ="atk hyper"),
        axis.title.x = element_text(size = 45),
        axis.title.y = element_text(size = 45),
        axis.text = element_text(size = 45),
        legend.text = element_text(size = 35), 
        plot.title = element_text(size = 45),
        legend.background = element_blank()) +
  guides(linetype = FALSE) + 
  labs(x = "Age",
       y = "") + 
  geom_segment(aes(x = (as.numeric(jn_gen$bounds) + .6),
                   y = 0.5,
                   xend = (as.numeric(jn_gen$bounds) + .1),
                   yend = 0.5),
               colour = 'black',
               size = .5,
               arrow = arrow(length = unit(0.3, "cm"))) +
  annotate("label", 
           x = (as.numeric(jn_gen$bounds) + 1), 
           y = 0.5, 
           label = paste(round(as.numeric(jn_gen$bounds), 1), "years"),
           label.padding = unit(0.3, "lines"),
           fontface = "bold",
           family = "atk hyper",
           size = 10) 
  
ggsave("assets/jn_plot_gen.png", jn_plot_gen)

#identical y axes
y_limits <- ggplot_build(jn_gen$plot)$layout$panel_params[[1]]$y.range

jn_plot_spec <- jn_spec$plot + 
  labs(x = "Age",
       y = "Simple Slope of Knowledge Condition\n on Essentialist Response") +
  coord_cartesian(ylim = y_limits) +
  theme(legend.position = "none", 
        text = element_text(family ="atk hyper"),
        axis.title.x = element_text(size = 45),
        axis.title.y = element_text(size = 45,  lineheight = 0.3),
        axis.text = element_text(size = 45),
        plot.title = element_text(size = 45)) +
  guides(linetype = FALSE)

ggsave("assets/jn_plot_spec.png", jn_plot_spec)

#arrange
jn_plot <- ggarrange(jn_plot_spec, jn_plot_gen, nrow= 1,ncol = 2)
ggsave("assets/jn_plot.png", jn_plot, width = 10, height = 6)

#figures
# Calculate averages for each child participant
comp_kid_point <- subset(model_data, age_group == "child") %>%
  dplyr::group_by(ID, lang, know, age_exact, .drop = 
                    TRUE) %>%
  dplyr::summarise(response_mean = mean(response, na.rm = TRUE))

# Calculate averages for each adult participant
comp_adult_point <- subset(model_data, age_group == "adult") %>%
  dplyr::group_by(ID, lang, know, age_exact, .drop = 
                    TRUE) %>%
  dplyr::summarise(response_mean = mean(response, na.rm = TRUE))
comp_adult_point$age_exact <- 11 #for figure

# Calculate group means for adults
comp_means_adult <- summarySE(subset(model_data, age_group == "adult"), 
                              measurevar = "response", 
                              groupvars = c("lang", "know", "age_exact"), na.rm = TRUE) %>%
  mutate(ci_lower = response - ci,
         ci_upper = response + ci)
comp_means_adult$age_exact <- 11 #for figure

s2_comp_plot <- ggplot() + 
  geom_point(
    data = comp_kid_point,
    aes(x = age_exact,
        y = response_mean,
        color = lang,
        shape = lang),
    size = 1,
    alpha = .5,
    show.legend = FALSE) +
  
  geom_smooth(
    data = comp_kid_point,
    aes(x = age_exact,
        y = response_mean,
        color = lang,
        fill = lang),
    method = "glm") + 
  
  geom_point(
    data = comp_adult_point,
    aes(x = age_exact,
        y = response_mean,
        color = lang,
        shape = lang),
    position = position_jitter(w = 0.3, h = 0.02),
    size = 1,
    alpha = .5,
    show.legend = FALSE) + 
  
  geom_errorbar(
    data = comp_means_adult,
    aes(ymin = ci_lower,
        ymax = ci_upper,
        x = age_exact),
    width = .3) +
  
  geom_point(
    data = comp_means_adult,
    aes(y = response,
        x = age_exact,
        fill = lang,
        shape = lang),
    color = "black",
    size = 7) +
  
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(
    breaks = c(4, 5, 6, 7, 8, 9, 10, 11),
    labels = c("4", "5", "6", "7", "8", "9", "10", "Adults")) +
  coord_cartesian(ylim=c(-.05, 1.05)) + 
  theme(text = element_text(size = 45, family ="atk hyper"),
        plot.title = element_text(face = "bold"),
        axis.title.x = element_text(size = 45,
                                    margin = margin(t = 20, r = 0, b = 0, l = 0),
                                    face = "bold"),
        axis.title.y = element_text(size = 45,
                                    margin = margin(t = 0, r = 20, b = 0, l = 0),
                                    face = "bold"),
        axis.text = element_text(size = 45), 
        strip.text = element_text(size = 45, face = "bold"),
        strip.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        panel.background = element_rect(fill = NA),
        legend.position = "right", 
        legend.title = element_text(hjust = 0.5, face = "bold")) +
  labs(x = "Age",
       y = "P(Providing Essentialist Response)") +
  scale_shape_manual(name = "Language Condition",
                     labels = c("Generic", "Specific"), 
                     values = c(22,21)) + 
  scale_color_manual(name = "Language Condition",
                     labels = c("Generic", "Specific"),
                     values = c("#1E88E5", "#FFC107")) +
  scale_fill_manual(name = "Language Condition",
                    labels = c("Generic", "Specific"),
                    values = c("#1E88E5", "#FFC107")) +
  guides(fill = guide_legend(reverse = FALSE,
                             title.position = "top",
                             title.hjust = 0.5),
         shape = guide_legend(reverse = FALSE,
                              title.position = "top",
                              title.hjust = 0.5),
         color = guide_legend(reverse = FALSE,
                              title.position = "top",
                              title.hjust = 0.5)) +
  facet_grid(~know) 

ggsave("assets/fig2.png", s2_comp_plot, width = 10, height = 6)



