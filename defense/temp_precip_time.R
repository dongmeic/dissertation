
path <- "/Users/dongmeichen/Documents/beetle/data/"
data <- read.csv(paste0(path, "longterm_bioclim_mean.csv"))
tmean <- subset(data,var=="Tmean")
summerP2 <- subset(data,var=="summerP2")
btl.grids <- read.csv(paste0(path, "beetle_grids_time.csv"))
na.df <- data.frame(time=1902:1996, btl.t=rep(NA, 95))
btl.grids <- rbind(na.df, btl.grids[,c("time", "btl.t")])[-115,]
out <- "/Users/dongmeichen/Documents/defense/figures/"
png(paste0(out, "beetle_grids.png"), width=12, height=5, units="in", res=300)
par(mar = c(4,4.5,1,4.5))
with(tmean, plot(yrs, x, type="l", col="red3", lwd=2, cex.lab=1.5,
             xlab="Year",
             ylab="Mean temperature"))

# plot(tmean$yrs, tmean$x, type='l')
# lines(summerP2$yrs, median(tmean$x) * ppt$x / max(ppt$x), type='l', col='red')
# points(btl.grids$time, max(tmean$x) * btl.grids$btl.t / max(btl.grids$btl.t))
# axis(side=4, 1:5, at=1:5)

par(new = T)
with(summerP2, plot(yrs, x, type="l", lty=2, col="blue3", lwd=2, axes=F, xlab=NA, ylab=NA, cex.lab=1.5))
axis(side = 4)
mtext(side = 4, line = 3, "Cumulative summer precipitation", cex=1.5)
par(new = T)
with(btl.grids, plot(time, btl.t, col="lightblue", pch=19, cex=1.2, axes=F, xlab=NA, ylab=NA))
# axis(side = 4)
# mtext(side = 4, line = 3, substitute(paste("Number of grid cells (",10^3,")")))
text(btl.t ~time, labels=btl.t, data=btl.grids, cex=0.8, font=2)
legend("topleft",
       legend=c("MPB life cycle mean temperature (°C)", "Two-year cumulative summer precipitation (mm)", "Number of MPB-affected grid cells"),
       lty=c(1,2,0), pch=c(NA, NA, 19), lwd=2, col=c("red3", "blue3", "lightblue"), bty = "n")
dev.off()

shade.where.greater <- function(x, y1, y2) {
  # y1 should be the vector where shading occurs if greater
  n <- length(x)
  min.y <- min(y1, y2)
  max.y <- max(y1, y2)
  plot(x, y1, type='l', ylim=c(range(min.y, max.y)))
  polygon(c(x, rev(x)), 
          c(y1, rep(min.y, n)), 
          col=rgb(0, 0, 0, 0.2), 
          border=rgb(0, 0, 0, 0))
  polygon(c(x, rev(x)), 
          c(y2, rep(min.y, n)), 
          col='white', 
          border=rgb(0, 0, 0, 0))
  lines(x, y2, col=2)
  lines(x, y1)
}


n <- 15
x <- 1:n
y1 <- runif(n)
y2 <- runif(n)

shade.where.greater(x, y1, y2)