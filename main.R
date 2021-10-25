library(fpp2)
library(Mcomp)
library(cowplot)
library(moments)

### --- DATA ---
all_ts <- subset(M3,"monthly", "industry")
cat(length(all_ts))

show_ts <- function(ts) {
  cat('Series name: ', ts$sn, '\n')
  cat('Series number and period: ', ts$st, '\n')
  cat('Number of observations: ', ts$n, '\n')
  cat('h: ', ts$h, '\n')
  cat('period: ', ts$period, '\n')
  cat('type: ', ts$type, '\n')
  cat('description: ', ts$description, '\n')
  par(mfrow=c(4,1))
  ap <- autoplot(ts) + ggtitle(label=ts$sn, subtitle=ts$description) +
    scale_x_continuous(breaks = scales::extended_breaks(10)) + theme(legend.position="bottom")
  sp <- ggseasonplot(ts$x) + ggtitle('Seasonal Plot (Training)')
  ss <- ggsubseriesplot(ts$x) + ggtitle('Subseries Plot (Training)') + ylab('')
  acf <- ggAcf(ts$x, lag.max = 100) + ggtitle('Autocorrelogram Plot (Training)')
  decomp <- autoplot(mstl(winestocks$x)[,2:4], facets=TRUE) + ggtitle('MSTL (Training)') + ylab('')
  g1 <- plot_grid(ap, sp, ss, acf)
  plot_grid(g1, decomp, rel_widths = c(2, 1), rel_heights = c(3,2))
}

winestocks <- all_ts$N2101

### --- EXPLORATIVE DATA ANALYSIS ---
show_ts(winestocks)
# monthly (f=12) time series with 123 observations of training set
# from the time serie plot we can observe a strong seasonal pattern. Also a moderate
# negative trend is observable from 1983 to 1992 (from 1990 the fluctuation stabilizes). 
# 1982 year was part of an uptrend which breaks in 1983. With the seasonal plot we can
# observe peaks corresponding to winter season and lows to summer season. We have
# an outlier at July 1986 and at May 1990. From the subseries plot we can observe that August is
# the global low (averaged over the years) and November the global maximum (averaged
# over the years too). Subseries plot shows also negative trends for almost all
# the subperiods. The autocorrelogram confirms previous statements: strong seasonal
# feature is observable with positive spikes at lags 12±2 (multiples of seasonal frequency),
# and with significant negatives spikes equally alternated. Spikes amplitude decrease over time
# due to the trend. Seasonal and trend features were also observed using automatic
# time series decomposition.

### --- INDICATORS ---
# As we are working with a single time series the scale dependent property of
# metrics assumes less importance. As two suitable indicators of forecasting
# performance we opt for MAE (which respect to squared metrics account also for under 
# and over estimating) and MAPE. 
# Our goals for the two indicators are .... #TODO

### --- SIMPLE METHODS ---
# As simple methods we consider:
# - average method:         meanf(y, h)
# - naïve / rw method:      naive(y, h) or rwf(y, h)
# - seasonal naïve method:  snaive(y, h)
# - (rw with) drift method: rwf(y, h, drift=TRUE) 
# The seasonal naive should be the most appropriate simple forecating method for 
# our time series due to the strong seasonal component. Let's verify...

train <- window(winestocks$x, start=c(1982,3))
test <- winestocks$xx
H <- winestocks$h
average_method <- meanf(train, h=H)
naive_method <- naive(train, h=H)
seasonal_naive_method <- snaive(train, h=H)
drift_method <- rwf(train, h=H, drift=TRUE) 

autoplot(train) +
  autolayer(average_method, series="Mean", PI=FALSE) +
  autolayer(naive_method, series="Naïve", PI=FALSE) +
  autolayer(seasonal_naive_method, series="Seas Naïve", PI=FALSE) +
  autolayer(drift_method, series="Drift", PI=FALSE) +
  xlab("Year") + ylab('') + 
  guides(colour=guide_legend(title="Forecast"))
  
indicators <- c('MAE', 'MAPE')
test_metrics <- rbind(accuracy(average_method, test)[2,indicators], 
                      accuracy(naive_method, test)[2,indicators],
                      accuracy(seasonal_naive_method, test)[2,indicators],
                      accuracy(drift_method, test)[2,indicators])
rownames(test_metrics) <- c('Mean','Naïve','Seas Naïve', 'Drift')
test_metrics

autoplot(train) +
  autolayer(seasonal_naive_method, series="Seas Naïve", PI=TRUE) +
  autolayer(test, series="Actual") +
  xlab("Year") + ylab('') + 
  guides(colour=guide_legend(title=""))

# seasonal naive turns out to be the best choise. 
accuracy(average_method, test)[,indicators]
# Does it meet  your forecasting goal? TODO
# Residuals...
res <- residuals(seasonal_naive_method)
mean(res, na.rm=TRUE)
skewness(res, na.rm=TRUE)
checkresiduals(seasonal_naive_method)
# Uncorrelation: from the ACF we can see that most of the peaks lie between 
#    the upper and lower significant limits, however at lag 1,2,11, 12 correlation
#    (not so much but) is relevant. Formally, with the Ljung-Box test we do not reject
#    uncorrelation because p-value is < 0.05
# Normally distributed with mean zero: mean is -54, slightly overestimating 
#    (anyway is close to 0...). Distribution is positive skewness (with a small skewness coeff...) 





