library(statsecol)
library(tidyverse)
data(bowhead_LT)

# if the AIC is lower for just the key function, then it just doesn't even compute the AIC for the adjustment terms?
# order = 2; that forces the AIC to work?

View(bowhead_LT)

# size = group size; averaged when observers disagree
# bf = beafort sea state
# object = unique observation ID
# Region.Label = stratum ID
# Area = stratum area
# Effort = total transect length (in km); this is a proxy for effort


# no truncation necessary if we use this binwidth; truncation more suitable if using binwidth of 0.2? 
ggplot(data = bowhead_LT) + 
  geom_histogram(aes(x = distance, fill = as.factor(size)), binwidth = 0.1)


# questions
# documentation says that "group ID, observations of >1 individuals will have a ow per animal, but a shared Sample.Label, a numeric vector; however object #47 w/ size = 2 only has one row (row 43 in the initial order). Is this accurate?