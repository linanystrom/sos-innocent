################################################################################

# Randomization of Mock crime procedure

################################################################################
## Set up ----------------------------------------------------------------------

## Packages

packages <- c("gtools", "readr", "tibble", "dplyr", "data.table", "tidyr",
              "randomizr")

lapply(packages, library, character.only = TRUE)

# ------------------------------------------------------------------------------

sample_cell   <- 30         #N in each cell
nr_cell       <- 4          #N of cells
total_sample  <-sample_cell*nr_cell 
id            <- 1:total_sample

df<-data.frame(id)

df$MC <- complete_ra(N = nrow(df),
                          m_each = c(40,40,40),
                          conditions = c("MC3",
                                         "MC4",
                                         "MC5"))

overall_count <- count(df, MC)

write.csv(df,
          "./random_assignment.csv",
          row.names = FALSE)

ra <- read.csv("random_assignment.csv")

ra <- ra %>% 
  mutate(
    link = case_when(
      MC == "MC3" ~ "https://samgu.eu.qualtrics.com/jfe/form/SV_byMwvFy8eNHzsJE",
      MC == "MC4" ~ "https://samgu.eu.qualtrics.com/jfe/form/SV_eVPxLZ4liQjgIU6",
      MC == "MC5" ~ "https://samgu.eu.qualtrics.com/jfe/form/SV_6PWdoXFv4Sr7Vyu"
    )
  )
ra[,"code"] <- NA

write.csv(ra,
          "./random_assignment_ext.csv",
          row.names = FALSE)
