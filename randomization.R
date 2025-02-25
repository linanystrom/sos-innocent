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
