library(raster)
library(sf)
library(rgdal)
library(ggplot2)

fun.mode<-function(x){as.numeric(names(sort(-table(x)))[1])}

lst <- list.files('Documents/permafrost-lst-statanalysis/res/', full.names = TRUE, pattern = '*.tif')

lst_stack<-stack(lst)

crs(lst_stack)<-CRS("+init=epsg:3573")

### define palette

pal <- colorRampPalette(c('blue', 'green', 'yellow', 'orange', 'red'))

years <- c('1987-1990', '1993-1996', '2007-2010', '2019-2022')

outlist <- list()

# plot any single raster from stack and export it

# counter <- 1L


for (i in 1:4) {
  
  svg(filename=sprintf("Documents/permafrost-lst-statanalysis/out/%s.svg", years[i]))
  
  plot(lst_stack[[i]], col = pal(20), main=sprintf('Mean LST for %s (August-September)', years[i]), xlab="Easting (m)", ylab="Northing (m)", sub="EPSG:3573 (North Pole LAEA Canada)",
       legend.args=list(text='temp (°C)', side = 1, font = 4, line=1, cex=0.8), zlim=c(-5,35))
  
  dev.off()
  
  val <- getValues(lst_stack[[i]])
  m <- mean(val, na.rm=T)
  stad <- sd(val, na.rm=T)
  med <- median(val, na.rm=T) 
  mode <- fun.mode(val)
  
  outlist[[i]] <- c(years[i], m, mode, med, stad)
  
  his <- hist(lst_stack[[1]], breaks = 40, maxpixels = 1000000)
  dat <- data.frame(freq = his$counts,temp = his$mids)
  
#  png(filename=sprintf("Documents/permafrost-lst-statanalysis/out/%s_hist.png", years[i]))
  
 # hist(lst_stack[[i]], maxpixels=1000000)
  
  plot_curr <- ggplot(dat, aes(x=temp, y=freq)) + 
    geom_bar(stat="identity", fill="#0f2080", alpha=0.8)+
    xlab("Temperature")+ylab("Pixel Frequency") + 
    scale_x_continuous(breaks = seq(-4,40,5))+
    geom_vline(aes(xintercept=m, color="mean"), linetype = "dashed", linewidth = 1) +
    geom_vline(aes(xintercept=mode, color="mode"), linetype = "dashed", linewidth = 1) +
    geom_vline(aes(xintercept=med, color="median"), linetype = "dashed", linewidth = 1)
    scale_color_manual(name = "statistics", values = c(median = "#85c0f9", mean = "#f5793a", mode = "#a95aa1"))
#  dev.off()
  
  ggsave(plot_curr, file=sprintf("Documents/permafrost-lst-statanalysis/out/%s_hist.svg", years[i]), width=10, height=6)
  
#  lines(d$y, lwd=4, col="red")
  
}

df <- data.frame(do.call(rbind,outlist))

colnames(df) <- c("Raster Path", "Mean")
