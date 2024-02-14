# cumulative analysis of the data
library(statsecol)
library(tidyverse)
library(viridisLite)
library(ggridges)
library(Distance)
data(bowhead_LT)

# raw data plot
# looks like hazard rate would be the best fit here
ggplot(data = bowhead_LT) + 
  geom_histogram(aes(x = distance, fill = as.factor(size)), bins= 21) +
  labs(x = "Distance truncated by 0.1 km",
       y = "Observerd # individuals",
       title = "Raw bowhead whale observations",
       subtitle = "Using 21 bins and minor left-truncated distances",
       fill = "Group size") + 
  theme_bw()

# aggressive binning
# definitely half-normal is the better fit here
histBreaks2 <- c(0, 0.6, 0.9, 1.2, 1.5, 1.8, 2.1, max(bowhead_LT$distance, na.rm = TRUE))
ggplot(data = bowhead_LT) + 
  geom_histogram(aes(x = distance, fill = as.factor(size)), breaks = histBreaks2) +
  labs(x = "Distance truncated by 0.1 km",
       y = "Observerd # Individuals",
       title = "Binned bowhead whale observations",
       subtitle = "Using 7 custom bins with minor left-truncated distances",
       fill = "Group size") + 
  theme_bw()

#truncate to get decreasing observations with distance
bowhead_LT_Trunc = bowhead_LT %>% filter(distance >= 0.64) %>% mutate(distance = distance - 0.64)
#plot the data
# both half-normal and hazard rate appear feasible here
ggplot(data = bowhead_LT_Trunc) + 
  geom_histogram(aes(x = distance, fill = as.factor(size)), bins = 14) +
  labs(x = "Distance left-truncated by 0.74 km",
       y = "Observerd # Individuals",
       title = "Filtered bowhead whale observations",
       subtitle = "Using 14 bins with substantially left-truncated distances",
       fill = "Group size") + 
  theme_bw()

conversion.factor <- convert_units(distance_units = "kilometre", 
                                   effort_units = "kilometre",
                                   area_units = "square kilometre")

# using model fits from separate alternatives, we'll only fit no-adjustment HN, HR, and size covariates
# not specifying adjustment = NULL because ds() will automatically pick the low AIC version
baseRawHN <- ds(data = bowhead_LT, key = "hn",
                convert_units = conversion.factor)
baseRawHR <- ds(data = bowhead_LT, key = "hr",
                convert_units = conversion.factor)
# sometimes does not converge
sizeRawHN <- ds(data = bowhead_LT, key = "hn", adjustment = NULL,
                convert_units = conversion.factor,
                formula = ~size)
sizeRawHR <- ds(data = bowhead_LT, key = "hr", adjustment = NULL,
                convert_units = conversion.factor,
                formula = ~size)

# size + half-normal is the best fit on the raw data
summarize_ds_models(baseRawHN, 
                    baseRawHR, 
                    sizeRawHN, 
                    sizeRawHR, output = "plain")

par(mfrow = c(2,2))
plot(baseRawHN, which=2, pl.col = adjustcolor("steelblue",0.5),border=NULL, lwd = 2, 
     breaks = histBreaks2,
     ylab = "Detection probability (g(x))", xlab = "Distance", las=1,
     main = "Base half-normal model")
# interesting that the covariate model assumes perfect detection of large-sized groups
# this is a quirk of the limited data --> only Region 2 has any within-group variation in size
plot(sizeRawHN, which=2, pl.col = adjustcolor("steelblue",0.5),border=NULL, lwd = 2, 
     breaks = histBreaks2,
     ylab = "Detection probability (g(x))", xlab = "Distance", las=1,
     main = "Half-normal model with covariates")
# both models have solid GOF but the covariate model is slightly better from visual QQ inspection
gof_ds(baseRawHN, main="Base Observed vs. Expected CDF", ks = TRUE)
gof_ds(sizeRawHN, main="Covariate Observed vs. Expected CDF", ks = TRUE)

# repeat for binned data
baseBinHN <- ds(data = bowhead_LT, key = "hn",
                cutpoints = histBreaks2,
                convert_units = conversion.factor)
baseBinHR <- ds(data = bowhead_LT, key = "hr",
                cutpoints = histBreaks2,
                convert_units = conversion.factor)
# often does not converge
sizeBinHN <- ds(data = bowhead_LT, key = "hn", adjustment = NULL,
                cutpoints = histBreaks2,
                convert_units = conversion.factor,
                formula = ~size)
sizeBinHR <- ds(data = bowhead_LT, key = "hr", adjustment = NULL,
                cutpoints = histBreaks2,
                convert_units = conversion.factor,
                formula = ~size)

#binned data also prefers models in the same order as unbinned
summarize_ds_models(baseBinHN, 
                    baseBinHR, 
                    sizeBinHN, 
                    sizeBinHR, output = "plain")

# looks like the binned models have a very similar fit (if not identical) to the raw data
par(mfrow = c(1,2))
plot(baseBinHN, which=2, pl.col = adjustcolor("steelblue",0.5),border=NULL, lwd = 2, 
     #breaks = histBreaks2,
     ylab = "Detection probability (g(x))", xlab = "Distance", las=1,
     main = "Base half-normal model")
# interesting that the covariate model assumes perfect detection of large-sized groups
# this is a quirk of the limited data --> only Region 2 has any within-group variation in size
plot(sizeBinHN, which=2, pl.col = adjustcolor("steelblue",0.5),border=NULL, lwd = 2, 
     #breaks = histBreaks2,
     ylab = "Detection probability (g(x))", xlab = "Distance", las=1,
     main = "Half-normal model with covariates")
# GOF produce output but cannot generate plots
# bootstrapping impossible so KS test doesn't happen
gof_ds(baseBinHN, main="Base Observed vs. Expected CDF", ks = TRUE)
gof_ds(sizeBinHN, main="Covariate Observed vs. Expected CDF", ks = TRUE)

# finally repeat for truncated data
# truncated data actually prefers more complex fits with more covariates and adjustment terms
baseTruncHN <- ds(data = bowhead_LT_Trunc, key = "hn",
                convert_units = conversion.factor)
baseTruncHR <- ds(data = bowhead_LT_Trunc, key = "hr",
                convert_units = conversion.factor)
sizeTruncHN <- ds(data = bowhead_LT_Trunc, key = "hn", adjustment = NULL,
                convert_units = conversion.factor,
                formula = ~size)
sizeTruncHR <- ds(data = bowhead_LT_Trunc, key = "hr", adjustment = NULL,
                convert_units = conversion.factor,
                formula = ~size)

# truncated data shows very different rankings
# hazard rate is preferred w/ size covariate, then basic hanf-normal, then size + HN, finally base HR
summarize_ds_models(baseTruncHN, 
                    baseTruncHR, 
                    sizeTruncHN, 
                    sizeTruncHR, output = "plain")


# looks like the binned models have exactly the same fit as the raw data
par(mfrow = c(2,3))
plot(baseTruncHN, which=2, pl.col = adjustcolor("steelblue",0.5),border=NULL, lwd = 2, 
     #breaks = histBreaks2,
     ylab = "Detection probability (g(x))", xlab = "Distance", las=1,
     main = "Base half-normal model")
# interesting that the covariate model assumes perfect detection of large-sized groups
# this is a quirk of the limited data --> only Region 2 has any within-group variation in size
plot(sizeTruncHN, which=2, pl.col = adjustcolor("steelblue",0.5),border=NULL, lwd = 2, 
     #breaks = histBreaks2,
     ylab = "Detection probability (g(x))", xlab = "Distance", las=1,
     main = "Half-normal model with covariates")
plot(sizeTruncHR, which=2, pl.col = adjustcolor("steelblue",0.5),border=NULL, lwd = 2, 
     #breaks = histBreaks2,
     ylab = "Detection probability (g(x))", xlab = "Distance", las=1,
     main = "Hazard rate model with covariates")
# both models have solid GOF but the covariate model is slightly better from visual QQ inspection
gof_ds(baseTruncHN, main="Base Half-Normal Observed vs. Expected CDF", ks = TRUE)
gof_ds(sizeTruncHN, main="Covariate Half-Normal Observed vs. Expected CDF", ks = TRUE)
gof_ds(sizeTruncHR, main="Covariate Hazard Rate Observed vs. Expected CDF", ks = TRUE)


# break out abundance under both models by region
# generate abundance estimates for the best raw data models
region_table <- unique(bowhead_LT[,c("Region.Label", "Area")])
sample_table <- unique(bowhead_LT[,c("Region.Label", "Sample.Label", "Effort")])
observation_table <- unique(bowhead_LT[,c("object", "Region.Label", "Sample.Label")])
# raw model
baseRawHN_N <- dht(model = baseRawHN$ddf, 
                    region.table = region_table,
                    sample.table = sample_table,
                    obs.table = observation_table)
# covariate model
# lower overall SE driven by Regions 2 and 9 while 3, 11, 12, and 15 have increased SE
# abundance decreases in regions 2 and 9 while increasing elsewhere
# this is because Regions 2 and 9 are the only sub-regions with size > 1 observations
sizeRawHN_N <- dht(model = sizeRawHN$ddf, 
                    region.table = region_table,
                    sample.table = sample_table,
                    obs.table = observation_table)

# output the models
save(baseRawHN, baseRawHR, sizeRawHN, sizeRawHR,
     baseBinHN, baseBinHR, sizeBinHN, sizeBinHR,
     baseTruncHN, baseTruncHR, sizeTruncHN, sizeTruncHR,
     file="df-models.RData")

# check variance using bootstraps rather than delta method
# delta-method approximation that assumes independence between uncertainty in the detection function and variability in encounter rate
# takes ~3 minutes on my computer, likely longer for you
# check or delete the "cores = " if you don't have 10 cores on your computer
# I'm running on a 16-core
# for the full process see: http://examples.distancesampling.org/Distance-variance/variance-distill.html
est.boot <- bootdht(model=sizeRawHN, flatfile=bowhead_LT,
                    summary_fun=bootdht_Nhat_summarize,
                    convert_units=conversion.factor, nboot=999, cores=10)
alpha <- 0.50
bootci <- quantile(est.boot$Nhat, probs = c(alpha/2, 1-alpha/2),
                   na.rm=TRUE)
# plot boostraps from the 
par(mfrow = c(1,1))
hist(est.boot$Nhat, nc=30,
     main="Distribution of bootstrap estimates\nwithout model uncertainty",
     xlab="Estimated abundance")
abline(v=bootci, lwd=2, lty=2)