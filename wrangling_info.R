################################################################################

# Data preparation - Information disclosure

################################################################################
# Basic setup ------------------------------------------------------------------

packages <- c("gtools", "readr", "tibble", "dplyr", "data.table", "tidyr",
              "readxl", "ggplot2", "lme4")

lapply(packages, library, character.only = TRUE)

## Load disclosure data

info_disc <- read_xlsx(#input data here
  
)

# Prepare data for analysis ----------------------------------------------------

info_disc <- info_disc %>% 
  pivot_longer(
    cols = c("activity_1",
             "activity_2",
             "activity_3",
             "activity_4",
             "activity_5",
             "activity_6"),
    names_to = "activity",
    values_to = "detail")

### Code time variables for interrupted time series regression

sos_long <-sos_long %>% 
  mutate(
    acitivity = case_when(
      acitivity == "activity_1" ~ 0,
      acitivity == "activity_2" ~ 1,
      acitivity == "activity_3" ~ 2,
      acitivity == "activity_4" ~ 3,
      acitivity == "activity_5" ~ 4,
      acitivity == "activity_6" ~ 5
    ),
    
    critical = case_when(
      acitivity == "stage_1" ~ 0,
      acitivity == "stage_2" ~ 0,
      acitivity == "stage_3" ~ 0,
      acitivity == "stage_4" ~ 0,
      acitivity == "stage_5" ~ 1,
      acitivity == "stage_6" ~ 2
    ))

write.csv(
  sos_long,
  "data/sos_long.csv",
  row.names = FALSE
)
