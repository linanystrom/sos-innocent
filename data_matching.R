################################################################################

# Data preparation - Data matching

################################################################################

## Load Qualtrics data


qual <- read_csv(
  #input data here
)

## Load disclosure data

info <- read_csv(
  #input data here
)

## Merge data - complete

df <- merge(qual, info, by = "ResponseId")

## Merge data - simple

simple <- qual %>% select(
  ResponseId, cond, culp
)

info_merge <- merge(simple, info, by = "ResponseId") 
