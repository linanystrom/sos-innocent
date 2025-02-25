################################################################################

# Power analysis

################################################################################

# Set up environment -----------------------------------------------------------

## Packages

packages <- c("pwr")

lapply(packages, library, character.only = TRUE)

# ------------------------------------------------------------------------------

pwr.t.test(n = 30, d = .86, sig.level = 0.05) # Effect size obtained from Oleszkiewicz & Watson (2020). 
                                              # Difference between innocent and guilty suspects in early disclosure. 

