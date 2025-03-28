################################################################################

# Data preparation - Qualtrics

################################################################################

# Basic setup ------------------------------------------------------------------

packages <- c("readr", "dplyr", "readxl")

lapply(packages, library, character.only = TRUE)

# Load and clean data ----------------------------------------------------------

## Qualtrics data

MC3_raw <- read_csv(#input data here
  ) %>% slice(-1, -2)
MC3_raw <- MC3_raw  %>%
  select(
    -ends_with(c(
      'Click', 'Submit', 'Count')))

MC4_raw <- read_csv(#input data here
  ) %>% slice(-1, -2)
MC4_raw <- MC4_raw  %>%
  select(
    -ends_with(c(
      'Click', 'Submit', 'Count')))


MC5_raw <- read_csv(#input data here
) %>% slice(-1, -2)
MC5_raw <- MC5_raw  %>%
  select(
    -ends_with(c(
      'Click', 'Submit', 'Count')))


qualtrics_raw <- do.call("rbind", list(MC3_raw,
                                       MC4_raw,
                                       MC5_raw))

#qualtrics_raw <- qualtrics_raw %>% 
  #rename(response_id = ResponseId)

qualtrics_raw <- (type_convert(qualtrics_raw))


# create composite measures-----------------------------------------------------

qualtrics_clean <- qualtrics_raw %>% 
  mutate(
    interview_adj_2_R = case_when(
      interview_adj_2 == 5 ~ 1,
      interview_adj_2 == 4 ~ 2,
      interview_adj_2 == 3 ~ 3,
      interview_adj_2 == 2 ~ 4,
      interview_adj_2 == 1 ~ 5
    ),
    interview_adj_4_R = case_when(
      interview_adj_4 == 5 ~ 1,
      interview_adj_4 == 4 ~ 2,
      interview_adj_4 == 3 ~ 3,
      interview_adj_4 == 2 ~ 4,
      interview_adj_4 == 1 ~ 5
    ),
    interview_perc = (
      interview_adj_1 + 
        interview_adj_2_R + 
        interview_adj_3 + 
        interview_adj_4_R + 
        interview_adj_5 + 
        interview_adj_6)/6,
    
    ###Interviewer quality
    
    interviewer_adj_2_R = case_when(
      interviewer_adj_2 == 5 ~ 1,
      interviewer_adj_2 == 4 ~ 2,
      interviewer_adj_2 == 3 ~ 3,
      interviewer_adj_2 == 2 ~ 4,
      interviewer_adj_2 == 1 ~ 5
    ),
    interviewer_adj_3_R = case_when(
      interviewer_adj_3 == 5 ~ 1,
      interviewer_adj_3 == 4 ~ 2,
      interviewer_adj_3 == 3 ~ 3,
      interviewer_adj_3 == 2 ~ 4,
      interviewer_adj_3 == 1 ~ 5
    ),
    interviewer_adj_6_R = case_when(
      interviewer_adj_6 == 5 ~ 1,
      interviewer_adj_6 == 4 ~ 2,
      interviewer_adj_6 == 3 ~ 3,
      interviewer_adj_6 == 2 ~ 4,
      interviewer_adj_6 == 1 ~ 5
    ),
    interviewer_perc = (
      interviewer_adj_1 + 
        interviewer_adj_2_R + 
        interviewer_adj_3_R + 
        interviewer_adj_4 + 
        interviewer_adj_5 + 
        interviewer_adj_6_R)/6,
    
    ### Self-assessment of performance
    
    interview_statements_2 = case_when(
      interview_statements_2 == 5 ~ 1,
      interview_statements_2 == 4 ~ 2,
      interview_statements_2 == 3 ~ 3,
      interview_statements_2 == 2 ~ 4,
      interview_statements_2 == 1 ~ 5
    ),
    interview_statements_3 = case_when(
      interview_statements_3 == 5 ~ 1,
      interview_statements_3 == 4 ~ 2,
      interview_statements_3 == 3 ~ 3,
      interview_statements_3 == 2 ~ 4,
      interview_statements_3 == 1 ~ 5
    ),
    self_assessment = (
      interview_statements_1 + 
        interview_statements_2 + 
        interview_statements_3 + 
        interview_statements_4)/4,
  )

write.csv(
  qualtrics_clean,
  "data/qualtrics_clean.csv",
  row.names = FALSE
)


