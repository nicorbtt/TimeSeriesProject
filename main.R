library(fpp2)
library(Mcomp)
library(cowplot)

# DATA
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
    scale_x_continuous(breaks = scales::extended_breaks(10))
  sp <- ggseasonplot(ts$x) + ggtitle('Seasonal Plot (Training)')
  ss <- ggsubseriesplot(ts$x) + ggtitle('Subseries Plot (Training)')
  acf <- ggAcf(ts$x, lag.max = 100) + ggtitle('Autocorrelogram Plot (Training)')
  plot_grid(ap, sp, ss, acf)
}

winestocks <- all_ts$N2101

# EXPLORATIVE DATA ANALYSIS

show_ts(winestocks)


