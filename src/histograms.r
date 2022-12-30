library(raster)
library (sf)
library(rgdal)
library(ggplot2)

fun.mode<-function(x){as.numeric(names(sort(-table(x)))[1])}

lst <- list.files('./statanalys_pf/res', full.names = TRUE, pattern = '*.tif')

lst_stack<-stack(lst)

crs(lst_stack)<-CRS("+init=epsg:3573")

### define palette

pal <- colorRampPalette(c('blue', 'green', 'yellow', 'orange', 'red'))

years <- c('1987-1990', '1993-1996', '2007-2010', '2019-2022')

outlist <- list()

# plot any single raster from stack and export it

# counter <- 1L

for (i in 1:4) {
  
  png(filename=sprintf("./statanalys_pf/out/%s.png", years[i]))
  
  plot(lst_stack[[i]], col = pal(20), main=sprintf('Mean LST for %s (August-September)', years[i]), xlab="Easting (m)", ylab="Northing (m)", sub="EPSG:3573 (North Pole LAEA Canada)",
       legend.args=list(text='temp (°C)', side = 1, font = 4, line=1, cex=0.8), zlim=c(-5,35))
  
  dev.off()
  
  val <- getValues(lst_stack[[i]])
  m <- mean(val, na.rm=T)
  outlist[[i]] <- c(lst_stack[[i]], m)
  
  png(filename=sprintf("./statanalys_pf/out/%s_hist.png", years[i]))
  
  hist(lst_stack[[i]], maxpixels=1000000)
  
  dev.off()
  
  stad <- sd(val, na.rm=T)
  
  d <- density(val, na.rm=T)
  
  lines(d$y, lwd=4, col="red")
  
}

df <- data.frame(do.call(rbind,outlist))

colnames(df) <- c("Raster Path", "Mean")
