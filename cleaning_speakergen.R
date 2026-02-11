# load libraries
library(tidyverse) 

# load data 
## children
child_raw <- read_csv("data/speakergen_child_di.csv",
                      col_types = cols(
                        ID = col_character(),         
                        gender = col_character(), 
                        age_floor = col_integer(),
                        center_age = col_double(),
                        age_exact = col_double(),
                        race = col_character(),     
                        know = col_factor(),  
                        lang = col_factor(),  
                        response = col_integer(),     
                        item = col_character(),
                        task = col_character()))
n_distinct(child_raw$ID) # n = 233

## adults
adult_raw <- read_csv("data/speakergen_adult_di.csv",
                      col_types = cols(
                        ID = col_character(),         
                        gender = col_character(), 
                        age = col_integer(),
                        race = col_character(),     
                        know = col_factor(),  
                        lang = col_factor(),  
                        response = col_integer(),     
                        item = col_character(),
                        task = col_character()))
n_distinct(adult_raw$ID) # n = 220

# label and merge (dropping demographic variables)
adult_raw$age_group <- "adult"
child_raw$age_group <- "child"
full_data <- bind_rows(child_raw, adult_raw)

# check manipulation check pass rate 
## knowledgeable
full_data %>%
  filter(know == "Knowledgeable",
         item %in% c("K_Check1", "K_Check2")) %>%
  summarise(pass_rate = sum(response == 1, na.rm = TRUE) / nrow(.)) # 96%

## unknowledgeable
full_data %>%
  filter(know == "Unknowledgeable",
         item %in% c("U_Check1", "U_Check2")) %>%
  summarise(pass_rate = sum(response == 1, na.rm = TRUE) / nrow(.)) # 95%

# drop attention checks
full_data <- full_data %>%
  filter(!grepl("check", item, ignore.case = TRUE))

# drop participants with < 50% completion (specified in preregistration)
full_data_clean <- full_data %>%
  group_by(ID) %>%
  filter(sum(!is.na(response)) >= 4) #there are 8 items

n_distinct(subset(full_data_clean, age_group == "adult")$ID) 
# 3 adults dropped, n = 217

n_distinct(subset(full_data_clean, age_group == "child")$ID) 
# 0 children dropped, , n = 233

write.csv(full_data_clean, "data/speakergen_combined_clean.csv", row.names = FALSE)

#### done with cleaning! ####

