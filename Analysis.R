library(statsecol)
library(tidyverse)
library(viridisLite)
library(ggridges)
data(bowhead_LT)

# Panic in the Disko Bay: Distance Sampling for Identifying Bowhead Whales

# if the AIC is lower for just the key function, then it just doesn't even compute the AIC for the adjustment terms?
# order = 2; that forces the AIC to work?

View(bowhead_LT)

# size = group size; averaged when observers disagree
# bf = beafort sea state
# object = unique observation ID
# Region.Label = stratum ID
# Area = stratum area
# Effort = total transect length (in km); this is a proxy for effort
# distance = perpendicular distance to observation; assumed to be nautical miles

# no truncation necessary if we use this binwidth; truncation more suitable if using binwidth of 0.2? 
# however, there appears to be some variation 
ggplot(data = bowhead_LT) + 
  geom_histogram(aes(x = distance, fill = as.factor(size)), binwidth = 0.1)

# in contrast, we need a more aggressive binning to uphold monotonic decreases
ggplot(data = bowhead_LT) + 
  geom_histogram(aes(x = distance, fill = as.factor(size)), binwidth = 0.25)

# manually set cutpoints somewhere in the middle
cutpoints = seq(min(bowhead_LT$distance, na.rm = TRUE),
                max(bowhead_LT$distance, na.rm = TRUE),
                by=0.25)

ggplot(data = bowhead_LT) + 
  geom_histogram(aes(x = distance, fill = as.factor(size)), breaks = cutpoints) +
  labs(x = "Distance (nm)",
       y = "Observerd # Individuals",
       title = "title",
       subtitle = "subtitle",
       fill = "Group size") + 
  theme_bw()

ggplot(data = bowhead_LT, aes(x = distance, y = factor(Region.Label))) +
  geom_density_ridges(stat = "binline",
                      bins = 10, draw_baseline = FALSE, na.rm = TRUE)



# questions
# documentation says that "group ID, observations of >1 individuals will have a ow per animal, but a shared Sample.Label, a numeric vector; however object #47 w/ size = 2 only has one row (row 43 in the initial order). Is this accurate?

# how to deal with issues where animals run or hide?
# when they run, we can use aggressive binning to remove non-decreasing portions of g(x)
# when they hide, we can use a cutoff until we're confident that avoidance behaviour have ceased (at a certain distance)
# this leads to no bias but obviously a poor fit; otherwise we have a breakdown in some assumptions and resulting bias
# THEN, we assume perfect detecibility at the truncation point (instead of g(0) = 1)