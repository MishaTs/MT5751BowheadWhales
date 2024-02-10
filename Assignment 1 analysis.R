library(statsecol)
library(Distance)
data("bowhead_LT")
hist(bowhead_LT$distance, breaks = 10) #looks like whales avoid transect
summary(bowhead_LT$distance)
#remove NAs
bowhead_LT <- bowhead_LT[!is.na(bowhead_LT$distance),]
#group all observations <0.75 together
hist(bowhead_LT$distance,
     breaks = c(0,0.75,1,1.25,1.5,1.75,2, 2.5),
     main = "Histogram of Distances", xlab = "Distance")
hn <- ds(data = bowhead_LT,
                key = "hn",cutpoints = c(0,0.75,1,1.25,1.5,1.75,2,2.5))
#basic tries fitting cos but is a worse fit
hn.herm <- ds(data = bowhead_LT,
                key = "hn",cutpoints = c(0,0.75,1,1.25,1.5,1.75,2,2.5), 
                adjustment = "herm") 
hn.poly <- ds(data = bowhead_LT,
                key = "hn",cutpoints = c(0,0.75,1,1.25,1.5,1.75,2,2.5), 
                adjustment = "poly")
#both adjustment are the same as base
hn.size <- ds(data = bowhead_LT,
                     key = "hn",cutpoints = c(0,0.75,1,1.25,1.5,1.75,2,2.5),
                     formula = ~size)
hn.bf <- ds(data = bowhead_LT,
                     key = "hn",cutpoints = c(0,0.75,1,1.25,1.5,1.75,2,2.5),
                     formula = ~bf)
hn.sizbf <- ds(data = bowhead_LT,
                   key = "hn",cutpoints = c(0,0.75,1,1.25,1.5,1.75,2,2.5),
                   formula = ~size + bf)

hr <- ds(data = bowhead_LT,
                key = "hr",cutpoints = c(0,0.75,1,1.25,1.5,1.75,2,2.5))
hr.herm <- ds(data = bowhead_LT,
                     key = "hr",cutpoints = c(0,0.75,1,1.25,1.5,1.75,2,2.5), 
                     adjustment = "herm") 
hr.poly <- ds(data = bowhead_LT,
                     key = "hr",cutpoints = c(0,0.75,1,1.25,1.5,1.75,2,2.5), 
                     adjustment = "poly")
#both adjustment are the same as base
hr.size <- ds(data = bowhead_LT,
                     key = "hr",cutpoints = c(0,0.75,1,1.25,1.5,1.75,2,2.5),
                     formula = ~size)
hr.bf <- ds(data = bowhead_LT,
                   key = "hr",cutpoints = c(0,0.75,1,1.25,1.5,1.75,2,2.5),
                   formula = ~bf)
hr.sizbf <- ds(data = bowhead_LT,
                      key = "hr",cutpoints = c(0,0.75,1,1.25,1.5,1.75,2,2.5),
                      formula = ~size + bf)
summarize_ds_models(hn, hr, hn.bf,
                    hn.size, hn.sizbf, hr.bf,
                    hr.size, hr.sizbf)
#basic half norm is best
plot(hn)
gof_ds(hn) #pretty good fit

summary(hn)
#likely an underestimate as whales will be underwater for some of the time
#Hence g(0) < 1, overestimate detection -> underestimate abundance 
N.ests <- hn$dht$individuals$N[1:6,]
N.ests$Label <- as.factor(as.integer(N.ests$Label))
ggplot(N.ests)+
  geom_point(aes(x = Label, y = Estimate))+
  geom_errorbar(aes(x = Label, ymin = lcl, ymax = ucl))+xlab("Region")+
  title(main = "Estimates of Abundance")
#only one observation in regions 3-15 compared to 10 in region 2

