



r <- lrn[[3]]
# length(values(lrexB))
# range(values(lrexB), na.rm = T)
# rango<- c(range(values(lrexB), na.rm = T))
# rango[2]-rango[1]
# getValues(r)

raster.range <- function(x){ 
  min.max.raster.values <- range(values(x), na.rm = T)
  
  
  out <- min.max.raster.values[2]- min.max.raster.values[1]
  return(out)
  
   }  


raster.range(r)


# r <- raster(ncols=36, nrows=18)
# r[] <- runif(ncell(r)) 
# # reclassify the values into three groups 
# # all values >= 0 and <= 0.25 become 1, etc.
# m <- c(0, 0.25, 1,  0.25, 0.5, 2,  0.5, 1, 3)
# rclmat <- matrix(m, ncol=3, byrow=TRUE)
# rc <- reclassify(r, rclmat)
# plot(rc)
# 
# a <- seq(min(values(r), na.rm =T), max(values(r), na.rm =T), raster.range(r)/256)[1:256]
# b <- seq(min(values(r), na.rm =T), max(values(r), na.rm =T), raster.range(r)/256)[2:257]
# c <- c(0:255)
# 
# m <- c(rbind(a,b,c))
m <- c(rbind(seq(min(values(r), na.rm =T), max(values(r), na.rm =T), raster.range(r)/256)[1:256],
           seq(min(values(r), na.rm =T), max(values(r), na.rm =T), raster.range(r)/256)[2:257], 
           c(0:255)))


rclmat <- matrix(m, ncol=3, byrow=TRUE)



rc <- reclassify(r, rclmat, right=NA)


par(mfrow=c(1,1))
plot(r)
install.packages("igraph")
library(igraph)


# freq(rc)
# plot(rc)


library(autothresholdr)
library(ijtiff)
library(magrittr)
 auto_thresh_apply_mask(as.integer(values(rc)), method = "Otsu", ignore_white = T, ignore_black = F, ignore_na = TRUE)
#  mk <- rc
# m2 <- mk< 119
  mr2 <- mask(as.integer(values(rc)), 136, ignore_na = TRUE)
musgo <-  rc
values(musgo) <- as.numeric(mr2)
freq(musgo)
plot(musgo, main="norm_blue")
plotRGB(lr, scale=1,asp = nrow(vis.red)/ncol(vis.red)  )
