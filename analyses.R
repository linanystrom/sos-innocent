################################################################################

# SoS Innocent - Main analyses code

################################################################################

# Basic setup ------------------------------------------------------------------

packages <- c("gtools", "readr", "tibble", "dplyr", "data.table", "tidyr",
              "readxl", "ggplot2", "lme4", "TOSTER", "lmerTest", "compute.es")

lapply(packages, library, character.only = TRUE)

# Import data ------------------------------------------------------------------

sos_full      <- read.csv(#input data here
  )
sos_long_full <- read.csv(##input data here
  )

## Filter exclusions

sos           <- sos_full %>% filter(!exclusion %in% c('1'))      
sos_long      <- sos_long_full %>% filter(!exclusion %in% c('1')) 


# Hypothesis testing - Information disclosure ----------------------------------


## Plot Preparation

sos_jitter <- sos %>% 
  mutate(
    jitter_y = runif(nrow(sos), min = -.20, max = .20),
    jitter_x = runif(nrow(sos), min = -.20, max = .20)
  ) %>% 
  select(ID, jitter_y, jitter_x)

sos_plot <- sos_long %>% 
  left_join(sos_jitter, by = "ID") %>% 
  mutate(
    detail_jitter = detail + jitter_y,
    time_jitter   = time   + jitter_x,
    style         = case_when(
      cond == "D" ~ "Direct",
      cond == "S" ~ "SoS",
      culp == 0 ~ "Innocent",
      culp == 1 ~ "Guilty"
    )
  )


sos_plot$cond <- ordered(
  sos_plot$cond, levels = c("Direct","SoS")
)

sos_plot$culp <- ordered(
  sos_plot$culp, levels = c("Innocent","Guilty")
)

## Descriptives

info_desc <- sos_plot %>% 
  group_by(cond, culp, activity) %>% 
  summarise(
    Mean = mean(detail, na.rm = TRUE),
    SD = sd(detail, na.rm = TRUE),
    Median = median(detail, na.rm = TRUE),
    SE = SD/sqrt(n()),
    Upper = Mean + (1.96*SE),
    Lower = Mean - (1.96*SE)
  )


## Plot information disclosure 

info_plot <- ggplot(sos_plot,
                    aes(
                      x = time_jitter,
                      y = detail_jitter,
                      colour = factor(cond)
                    )) +
  facet_wrap(. ~ cond) +
  geom_line(
    aes(
      group = ID
    ),
    size = .05,
    color = "grey",
    position = position_jitter(width = .15),
    alpha = .20
  ) +
  geom_line(
    data = info_desc,
    aes(
      x = time,
      y = Mean,
      group = style
    ),
    size = 1.5
  ) +
  geom_errorbar(
    data = info_desc,
    aes(
      x = activity,
      ymax = Upper,
      ymin = Lower,
      group = style,
      color = factor(cond)
    ),
    inherit.aes = FALSE,
    width = .25
  ) +
  labs(
    y = "Information disclosure",
    x = "Activity",
    color = "Condition"
  ) +
  scale_x_continuous(
    labels = c("1", "2", "3","4","5","6"),
    breaks = 0:5
  ) +
  coord_cartesian(
    ylim = c(0, 5)
  ) +
  theme_classic()

info_plot <- info_plot + theme(legend.position = "none")


## Interrupted time series linear mixed effects model

### Main effect model

info_model_1 <- lmer(detail
                     ~ activity  
                     + critical 
                     + cond
                     + culp
                     + (1|mc/ID) 
                     + (1|interviewer),
                     data = sos_long,
                     REML = FALSE
)

summary(info_model_1)

### 2-way interaction effect model

info_model_int <- lmer(detail
                       ~ activity   
                       + critical 
                       + cond
                       + culp
                       + activity*cond 
                       + critical*cond 
                       + activity*culp
                       + critical*culp
                       + (1|mc/ID) 
                       + (1|interviewer), 
                       data = sos_long,
                       REML = FALSE
)

summary(info_model_int)


### Comparing regression models ANOVA

comp_model_anova <- anova(info_model_1, info_model_int)


### 3-way interaction effect model

info_model_3int <- lmer(detail
                       ~ activity   
                       + critical 
                       + cond
                       + culp
                       + activity*cond 
                       + critical*cond 
                       + activity*culp
                       + critical*culp
                       + activity*cond*culp
                       + critical*cond*culp
                       + (1|mc/ID) 
                       + (1|interviewer), 
                       data = sos_long,
                       REML = FALSE
)

summary(info_model_3int)

### Comparing regression models ANOVA

comp_model_anova <- anova(info_model_int, info_model_3int)

## Guilty suspects

guilty_data <- sos_long %>% filter(culp == 1, activity > 4)

# Sum crit info

guilty_model_1 <- lmer(detail
                     + cond
                     + (1|mc/ID) 
                     + (1|interviewer)
                     + (1|activity),
                     data = guilty_data,
                     REML = FALSE)

summary(guilty_model_1)

guilty_model_2 <- lmer(detail
                       + cond
                       + critical*cond
                       + (1|mc/ID) 
                       + (1|interviewer)
                       + (1|activity),
                       data = guilty_data,
                       REML = FALSE)

summary(guilty_model_2)



