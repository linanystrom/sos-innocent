################################################################################

# Sequential analysis

################################################################################

packages <- c("gsDesign")

lapply(packages, library, character.only = TRUE)

## Determining nominal p values for sequential analysis.
## Interim analysis performed when 75 interviews are complete.
## Nominal p at 75 = .024

gsDesign(k = 2,            # stopping points
         n.fix = 120,      # total sample
         n.I = c(75, 120), # N at stopping points (75 and total)
         test.type = 2,
         sfu = "WT",
         sfupar = .25,
         alpha = .05,
         beta = .80)
