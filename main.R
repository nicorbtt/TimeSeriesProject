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

# handle outliers
#winestocks$x[55] = (winestocks$x[56] + winestocks$x[54])/2
train <- winestocks$x
test <- winestocks$xx
H <- winestocks$h
average_method <- meanf(train, h=H)
naive_method <- naive(train, h=H)
seasonal_naive_method <- snaive(train, h=H)
drift_method <- rwf(train, h=H, drift=TRUE) 

autoplot(train) +
  ggtitle('Simple forecast methods') +
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
  ggtitle('Forecasts from Seasonal naive method') + 
  autolayer(seasonal_naive_method, series="S. Naïve", PI=TRUE) +
  autolayer(test, series="Actual") +
  xlab("Year") + ylab('')

# seasonal naive turns out to be the best choise. 
accuracy(seasonal_naive_method, test)[,indicators]
# Does it meet your forecasting goal? TODO
# Residuals...
res <- residuals(seasonal_naive_method)
mean(res, na.rm=TRUE)
skewness(res, na.rm=TRUE)
checkresiduals(seasonal_naive_method)
autoplot(train, series='Train data', color='black') +
  autolayer(fitted(seasonal_naive_method), series='Fitted values') +
  ggtitle('Fitted values from Seasonal naive method') + ylab('') + theme(legend.position="bottom") +
  guides(colour=guide_legend(title=""))

# Uncorrelation: from the ACF we can see that most of the peaks lie between 
#    the upper and lower significant limits. Exceptions: are lag 1-4, 11-12 and 36 
#    where correlation is relevant. Formally, with the Ljung-Box 
#    test we reject the white noise hypothesis (there is correlation! p-value=8.529e-08 is < 0.05)
# Normally distributed with mean zero: mean is -35 (bias!), overestimating 
#    Distribution is positive skewness (with a small skewness coeff...) 
# In deep: The two large peaks in the residual plot correspond to the two outliers previously identified.
#    The seasonal naive simply replicate the last observed season to predict 
#    the following. In the first year in the training data the trend is positive, then it becomes negative till 1990, then stabilize. 
#    Seasonal naive doesn't model trends.  This explain the first 9 points positive 
#    in a row in the residual plots (first positive trend), and also the series of mainly negative values in 
#    the residual plots from the 10th point ahead (second negative trend, overestimation), and so explain the correlation.
#    It's also clear visible in the training data + fitted values plot.


### --- EXPONENTIAL SMOOTHING ---
# In the family of exponential smoothing the we do not opt for SES because our time series
# has a strong seasonal component and not even for trend methods. We opt for Holt-Winters’ seasonal method
# to capture seasonality. 
# TODO discuss and choose method additive or multiplicative, damped or not damped... ?
# maybe multiplicative is slighlty better...

fit1 <- hw(train,seasonal="additive", h=H)
fit2 <- hw(train,seasonal="multiplicative", h=H)
fit3 <- hw(train, damped=TRUE, seasonal="additive", h=H)
fit4 <- hw(train, damped=TRUE, seasonal="multiplicative", h=H)
test_metrics <- rbind(accuracy(fit1, test)[2,indicators], 
                      accuracy(fit2, test)[2,indicators],
                      accuracy(fit3, test)[2,indicators],
                      accuracy(fit4, test)[2,indicators])
rownames(test_metrics) <- c('H-W additive','H-W multiplicative','H-W add. damped', 'H-W mul. damped')
test_metrics

autoplot(train) +
  ggtitle('Forecasts from Holt-Winters multiplicative method') + 
  autolayer(fit2, series="Holt-Winters mult.", PI=TRUE) +
  autolayer(test, series="Actual") +
  xlab("Year") + ylab('')

# TODO check also training set accuracy of all 4 methods
train_metrics <- rbind(accuracy(fit1, test)[1,indicators], 
                      accuracy(fit2, test)[1,indicators],
                      accuracy(fit3, test)[1,indicators],
                      accuracy(fit4, test)[1,indicators])
rownames(train_metrics) <- c('H-W additive','H-W multiplicative','H-W add. damped', 'H-W mul. damped')

# TODO check residual
res <- residuals(fit2)
mean(res, na.rm=TRUE)
skewness(res, na.rm=TRUE)
checkresiduals(fit2)

# TODO compare with simple method...

### --- ETS AND AUTO-ARIMA ---
## TODO
fit_ets <- ets(train)
summary(fit_ets)
autoplot(fit_ets)
accuracy(forecast(fit_ets,H), test)[,indicators]
res <- residuals(fit_ets)
mean(res, na.rm=TRUE)
skewness(res, na.rm=TRUE)
checkresiduals(fit_ets)

# ARIMA
fit_arima <- auto.arima(train)
summary(fit_arima)
autoplot(fit_arima)
accuracy(forecast(fit_arima,H), test)[,indicators]
res <- residuals(fit_arima)
mean(res, na.rm=TRUE)
skewness(res, na.rm=TRUE)
checkresiduals(fit_arima)