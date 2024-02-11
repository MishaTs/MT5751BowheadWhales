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

# we may also want a plot of group size across region (hence why bowheadGroup was created)

# play around with some auto-set cutpoints
histBreaks <- seq(min(bowhead_LT$distance, na.rm = TRUE),
                max(bowhead_LT$distance, na.rm = TRUE),
                by=0.25)
#settle on some manual boundaries
histBreaks2 <- c(0, 0.6, 0.9, 1.2, 1.5, 1.8, 2.1, max(bowhead_LT$distance, na.rm = TRUE))

#plot the data
ggplot(data = bowhead_LT) + 
  geom_histogram(aes(x = distance, fill = as.factor(size)), breaks = histBreaks2) +
  labs(x = "Distance (km)",
       y = "Observerd # Individuals",
       title = "title",
       subtitle = "subtitle",
       fill = "Group size") + 
  theme_bw()

bowhead_LT_Trunc = bowhead_LT %>% filter(distance >= 0.64) %>% mutate(distance = distance - 0.64)

#plot the data
ggplot(data = bowhead_LT_Trunc) + 
  geom_histogram(aes(x = distance, fill = as.factor(size))) +
  labs(x = "Distance (km)",
       y = "Observerd # Individuals",
       title = "title",
       subtitle = "subtitle",
       fill = "Group size") + 
  theme_bw()

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
bowhead.hn.null <- ds(data = bowhead_LT_Trunc, key = "hn", adjustment = NULL,
                      convert_units = conversion.factor)
bowhead.hn.cos  <- ds(data = bowhead_LT_Trunc, key = "hn", adjustment = "cos",
                      convert_units = conversion.factor)
bowhead.hn.herm <- ds(data = bowhead_LT_Trunc, key = "hn", adjustment = "herm",
                      convert_units = conversion.factor)
bowhead.hn.poly <- ds(data = bowhead_LT_Trunc, key = "hn", adjustment = "poly",
                      convert_units = conversion.factor)
bowhead.hr.null <- ds(data = bowhead_LT_Trunc, key = "hr", adjustment = NULL,
                      convert_units = conversion.factor)
bowhead.hr.cos  <- ds(data = bowhead_LT_Trunc, key = "hr", adjustment = "cos",
                      convert_units = conversion.factor)
bowhead.hr.herm <- ds(data = bowhead_LT_Trunc, key = "hr", adjustment = "herm",
                      convert_units = conversion.factor)
bowhead.hr.poly <- ds(data = bowhead_LT_Trunc, key = "hr", adjustment = "poly",
                      convert_units = conversion.factor)

# we're interested in if size bias exists here
# http://examples.distancesampling.org/Distance-groupsize/Remedy-size-bias-for-dolphin-surveys.html

# dredge some no-adjustment half-normal fits w/ covariates
# sometimes does not converge and AIC varies between 86 and 102
# however, convergence is 86 when successful
bowhead.hn.null.size <- ds(data = bowhead_LT_Trunc, key = "hn", adjustment = NULL,
                      convert_units = conversion.factor,
                      formula = ~size)
# fitting just beaufort sea state leads to a higher AIC and insignificant coefficients
#bowhead.hn.null.bf <- ds(data = bowhead_LT_Trunc, key = "hn", adjustment = NULL,
#                         convert_units = conversion.factor,
#                         formula = ~as.factor(bf))
bowhead.hn.null.size.bf1 <- ds(data = bowhead_LT_Trunc, key = "hn", adjustment = NULL,
                               convert_units = conversion.factor,
                               formula = ~size + as.factor(bf))
# fitting beafort sea state as a factor performs better, so drop this
#bowhead.hn.null.size.bf2 <- ds(data = bowhead_LT_Trunc, key = "hn", adjustment = NULL,
#                               convert_units = conversion.factor,
#                               formula = ~size + bf)
bowhead.hn.null.size.reg <- ds(data = bowhead_LT_Trunc, key = "hn", adjustment = NULL,
                               convert_units = conversion.factor,
                               formula = ~size + as.factor(Region.Label))
bowhead.hn.null.full <- ds(data = bowhead_LT_Trunc, key = "hn", adjustment = NULL,
                           convert_units = conversion.factor,
                           formula = ~size + as.factor(Region.Label) + as.factor(bf))
#tried some interactions and it didn't work
#bowhead.hn.null.size.bf.int <- ds(data = bowhead_LT_Trunc, key = "hn", adjustment = NULL,
#                                  convert_units = conversion.factor,
#                                  formula = ~size * as.factor(bf))
# add some adjustments
bowhead.hn.herm.size <- ds(data = bowhead_LT_Trunc, key = "hn", adjustment = "herm",
                           convert_units = conversion.factor,
                           formula = ~size,
                           nadj = c(2,3,4))
# Poly(4,6) ends up being non-monotonic, so leaving it at Polynomial(4)
bowhead.hn.poly.size <- ds(data = bowhead_LT_Trunc, key = "hn", adjustment = "poly",
                           convert_units = conversion.factor,
                           formula = ~size,
                           nadj = c(1,2,3,4))


# repeat dredge for hazard rate w/ no adjustments
# give up on BF-only and interaction models
bowhead.hr.null.size <- ds(data = bowhead_LT_Trunc, key = "hr", adjustment = NULL,
                           convert_units = conversion.factor,
                           formula = ~size)
bowhead.hr.null.size.bf1 <- ds(data = bowhead_LT_Trunc, key = "hr", adjustment = NULL,
                           convert_units = conversion.factor,
                           formula = ~size + as.factor(bf))
# fitting beafort sea state as a factor performs better, so drop this
#bowhead.hr.null.size.bf2 <- ds(data = bowhead_LT_Trunc, key = "hr", adjustment = NULL,
#                               convert_units = conversion.factor,
#                               formula = ~size + bf)
bowhead.hr.null.size.reg <- ds(data = bowhead_LT_Trunc, key = "hr", adjustment = NULL,
                               convert_units = conversion.factor,
                               formula = ~size + as.factor(Region.Label))
# sometimes does not converge, but (sometimes) performs well when it does: AIC varies between 80 and 120
# overall AIC ends up at 120 more often so probably not worth
bowhead.hr.null.full <- ds(data = bowhead_LT_Trunc, key = "hr", adjustment = NULL,
                           convert_units = conversion.factor,
                           formula = ~size + as.factor(Region.Label) + as.factor(bf))
# add a hermite adjustment to the best-fit model
bowhead.hr.herm.size <- ds(data = bowhead_LT_Trunc, key = "hr", adjustment = "herm",
                           convert_units = conversion.factor,
                           formula = ~size,
                           nadj = c(2,3,4))

# code for markdown can adapt from
# knitr::kable(summarize_ds_models(wren.hn, wren.hr.poly, wren.unif.cos),digits=3,
#             caption="Model comparison table for wren line transect data, Montrave.")
summarize_ds_models(bowhead.hn.null, 
                    bowhead.hr.null, 
                    bowhead.hn.null.size, 
                    bowhead.hn.null.size.bf1,
                    bowhead.hn.null.size.reg, 
                    bowhead.hn.null.full, 
                    bowhead.hr.null.size, 
                    bowhead.hr.null.size.bf1, 
                    bowhead.hr.null.size.reg, 
                    bowhead.hr.null.full,
                    bowhead.hn.herm.size,
                    bowhead.hn.poly.size, output = "plain")

# the base half-normal and size-only are the preferred models using AIC
par(mfrow = c(2,2))
plot(bowhead.hr.null.size , which=2, pl.col = adjustcolor("steelblue",0.5),border=NULL, lwd = 2,
     ylab = "Detection probability (g(x))", xlab = "Distance", las=1,
     main = "Base half-normal model")
# interesting that the covariate model assumes perfect detection of large-sized groups
# this is a quirk of the limited data --> only Region 2 has any within-group variation in size
plot(bowhead.hn.null.size, which=2, pl.col = adjustcolor("steelblue",0.5),border=NULL, lwd = 2, 
     breaks = histBreaks2,
     ylab = "Detection probability (g(x))", xlab = "Distance", las=1,
     main = "Half-normal model with covariates")
# both models have solid GOF but the covariate model is slightly better from visual QQ inspection
gof_ds(bowhead.hn.null, main="Base Observed vs. Expected CDF", ks = TRUE)
gof_ds(bowhead.hn.null.size, main="Covariate Observed vs. Expected CDF", ks = TRUE)

# break out abundance under both models by region
# maybe yolo try it with dplyr instead for the memes
region_table <- unique(bowhead_LT_Trunc[,c("Region.Label", "Area")])
sample_table <- unique(bowhead_LT_Trunc[,c("Region.Label", "Sample.Label", "Effort")])
observation_table <- unique(bowhead_LT_Trunc[,c("object", "Region.Label", "Sample.Label")])
# raw model
bowhead_abd1 <- dht(model = bowhead.hn.null$ddf, 
                    region.table = region_table,
                    sample.table = sample_table,
                    obs.table = observation_table)
# covariate model
# lower overall SE driven by Regions 2 and 9 while 3, 11, 12, and 15 have increased SE
# abundance decreases in regions 2 and 9 while increasing elsewhere
# this is because Regions 2 and 9 are the only sub-regions with size > 1 observations
bowhead_abd2 <- dht(model = bowhead.hn.null.size$ddf, 
                    region.table = region_table,
                    sample.table = sample_table,
                    obs.table = observation_table)

# check variance using bootstraps rather than delta method
# delta-method approximation that assumes independence between uncertainty in the detection function and variability in encounter rate
# takes ~3 minutes on my computer, likely longer for you
# check or delete the "cores = " if you don't have 10 cores on your computer
# I'm running on a 16-core
# for the full process see: http://examples.distancesampling.org/Distance-variance/variance-distill.html
est.boot <- bootdht(model=bowhead.hn.null.size, flatfile=bowhead_LT_Trunc,
                    summary_fun=bootdht_Nhat_summarize,
                    convert_units=conversion.factor, nboot=999, cores=10)
alpha <- 0.50
bootci <- quantile(est.boot$Nhat, probs = c(alpha/2, 1-alpha/2),
                   na.rm=TRUE)

par(mfrow = c(1,1))
hist(est.boot$Nhat, nc=30,
     main="Distribution of bootstrap estimates\nwithout model uncertainty",
     xlab="Estimated abundance")
abline(v=bootci, lwd=2, lty=2)





# repeat boostraps w/ resample by region rather than by transect
# our upper CI increases slightly but the lower CI stays at 0
#est.bootStrata <- bootdht(model=bowhead.hn.null.size, flatfile=bowhead_LT_Trunc,
#                    summary_fun=bootdht_Nhat_summarize,
#                    resample_strata = TRUE,
#                    convert_units=conversion.factor, nboot=999, cores=10)
#bootciStrata <- quantile(est.bootStrata$Nhat, probs = c(alpha/2, 1-alpha/2),
#                    na.rm=TRUE)






#abundance looks weird, so maybe let's try again with density?
#define function to get density
#bootdht_Dhat_summarize <- function(ests, fit) {
#  return(data.frame(D=ests$individuals$D$Estimate))
#}
#
#run the bootstrap
#est.bootDensitySize <- bootdht(model=bowhead.hn.null.size, flatfile=bowhead_LT_Trunc,
#                               summary_fun=bootdht_Dhat_summarize,
#                               convert_units=conversion.factor, nboot=999, cores=10)
# get the 95% CI
#bootciDensitySize <- quantile(est.bootDensitySize$D, probs = c(alpha/2, 1-alpha/2),
#                              na.rm=TRUE)
#par(mfrow = c(1,2))
#hist(est.boot$Nhat, nc=30,
#     main="Distribution of bootstrap estimates\nwithout model uncertainty",
#     xlab="Estimated abundance")
#abline(v=bootci, lwd=2, lty=2)
#hist(est.bootDensitySize$D, nc=30,
#     main="Distribution of bootstrap estimates\nwithout model uncertainty",
#     xlab="Estimated density")
#abline(v=bootciDensitySize, lwd=2, lty=2)
# density still keeps the lower CI at 0





# now irrelevant: play around with some adjustment terms with the size model
# adjustment terms generally aren't included in covariates for whatever reason
# Cosine(2) is preferred but never monotonic so that's not it
#bowhead.hn.cos.size <- ds(data = bowhead_LT_Trunc, key = "hn", adjustment = "cos",
#                          convert_units = conversion.factor,
#                          formula = ~size,
#                          nadj = c(1,2,3,4))
# Hermite(4) is preferred but is non-monotonic; Hermite(4,6) is not as bad
#bowhead.hn.herm.size <- ds(data = bowhead_LT_Trunc, key = "hn", adjustment = "herm",
#                          convert_units = conversion.factor,
#                          formula = ~size,
#                          nadj = c(2,3,4))
# Poly(4,6) ends up being non-monotonic, so leaving it at Polynomial(4)
#bowhead.hn.poly.size <- ds(data = bowhead_LT_Trunc, key = "hn", adjustment = "poly",
#                           convert_units = conversion.factor,
#                           formula = ~size,
#                           nadj = c(1,2,3,4))
# now repeat for half-normal
# non-monotonic and g(x) > 1 with a cosine adjustment
#bowhead.hr.cos.size <- ds(data = bowhead_LT_Trunc, key = "hr", adjustment = "cos",
#                          convert_units = conversion.factor,
#                          formula = ~size,
#                          nadj = c(1,2,3,4))
#Hermite(4,6) surprisingly matches everything
#bowhead.hr.herm.size <- ds(data = bowhead_LT_Trunc, key = "hr", adjustment = "herm",
#                          convert_units = conversion.factor,
#                          formula = ~size,
#                          nadj = c(2,3,4))
#Polynomial adjustments don't match monotonicity
#bowhead.hr.poly.size <- ds(data = bowhead_LT_Trunc, key = "hr", adjustment = "poly",
#                          convert_units = conversion.factor,
#                          formula = ~size,
#                          nadj = c(1,2,3,4))