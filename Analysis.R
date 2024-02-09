library(statsecol)
library(tidyverse)
library(viridisLite)
library(ggridges)
library(Distance)
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
histBreaks = seq(min(bowhead_LT$distance, na.rm = TRUE),
                max(bowhead_LT$distance, na.rm = TRUE),
                by=0.25)

ggplot(data = bowhead_LT) + 
  geom_histogram(aes(x = distance, fill = as.factor(size)), breaks = histBreaks) +
  labs(x = "Distance (nm)",
       y = "Observerd # Individuals",
       title = "title",
       subtitle = "subtitle",
       fill = "Group size") + 
  theme_bw()

ggplot(data = bowhead_LT, aes(x = distance, y = factor(Region.Label))) +
  geom_density_ridges(stat = "binline",
                      bins = 10, draw_baseline = FALSE, na.rm = TRUE)

# fit the NA row
ggplot(data = bowhead_LT, aes(x = distance, y = factor(bf,
                                                       levels = 0:3))) +
  geom_density_ridges(stat = "binline", draw_baseline = FALSE, na.rm = TRUE)

# more visualisation if desired
bowheadGroup <- bowhead_LT %>% group_by(Region.Label) %>% 
  summarise(n = sum(size, na.rm = TRUE),
            Area = median(Area),
            Effort = median(Effort))

conversion.factor <- convert_units(distance_units = "kilometre", 
                                   effort_units = "kilometre",
                                   area_units = "square kilometre")

# first look at no-covariate models
# uniform doesn't match the data, so half-normal and hazard rate fit
# adjustment terms do not improve over base key function
bowhead.hn.null <- ds(data = bowhead_LT, key = "hn", adjustment = NULL,
                      convert_units = conversion.factor)
bowhead.hn.cos  <- ds(data = bowhead_LT, key = "hn", adjustment = "cos",
                      convert_units = conversion.factor)
bowhead.hn.herm <- ds(data = bowhead_LT, key = "hn", adjustment = "herm",
                      convert_units = conversion.factor)
bowhead.hn.poly <- ds(data = bowhead_LT, key = "hn", adjustment = "poly",
                      convert_units = conversion.factor)
bowhead.hr.null <- ds(data = bowhead_LT, key = "hr", adjustment = NULL,
                      convert_units = conversion.factor)
bowhead.hr.cos  <- ds(data = bowhead_LT, key = "hr", adjustment = "cos",
                      convert_units = conversion.factor)
bowhead.hr.herm <- ds(data = bowhead_LT, key = "hr", adjustment = "herm",
                      convert_units = conversion.factor)
bowhead.hr.poly <- ds(data = bowhead_LT, key = "hr", adjustment = "poly",
                      convert_units = conversion.factor)

# half-normal model fails to converge with size alone
bowhead.hn.null.size <- ds(data = bowhead_LT, key = "hn", adjustment = NULL,
                      convert_units = conversion.factor,
                      formula = ~size)

bowhead.hn.null.size <- ds(data = bowhead_LT, key = "hr", adjustment = NULL,
                           convert_units = conversion.factor,
                           formula = ~size + as.factor(Region.Label) + as.factor(bf))


bowhead.hr.null.size <- ds(data = bowhead_LT, key = "hr", adjustment = NULL,
                      convert_units = conversion.factor,
                      formula = ~size)
bowhead.hr.cos.size  <- ds(data = bowhead_LT, key = "hr", adjustment = "cos",
                      convert_units = conversion.factor,
                      formula = ~size,
                      max_adjustments = 5)
bowhead.hr.herm.size <- ds(data = bowhead_LT, key = "hr", adjustment = "herm",
                      convert_units = conversion.factor,
                      formula = ~size)
bowhead.hr.poly.size <- ds(data = bowhead_LT, key = "hr", adjustment = "poly",
                      convert_units = conversion.factor,
                      formula = ~size)