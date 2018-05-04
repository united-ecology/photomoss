setwd("/home/manu/Desktop/Ndvi_prueba/R_ndvi/crustcover.example") 
library(tiff)
vis.tif <- readTIFF("vis/IMG_100.tif")
vis.red <- raster(vis.tif[,,1])
vis.green <- raster(vis.tif[,,2])
vis.blue <- raster(vis.tif[,,3])

vis <- stack(vis.red, vis.green, vis.blue)
r <- vis

# RGB_stack_DEM <-stack("vis/IMG_100.tif")
bandred <- raster("vis/IMG_100.tif", band=1)
# install.packages("RImageJROI") 
library(RImageJROI)
library(spatstat)

# Importamos el área de interes que creamos con Image J
# usamos como EJEMPLO la número 5 (la numeración de las celdillas las sabemos gracias  a la referencia 
# y orden conocido de los alveolos en la fotografía, algo que fuimos 
# apuntando según creabamos las celdillas en el Image J)
x_roi5<- read.ijroi("/home/manu/Desktop/Ndvi_prueba/R_ndvi/obs_area/RoiSet2/para_indra/win8.roi", verbose = FALSE)

# una vez importada el área de interés a nuestro Global Environment vamos a necesitar hacer una serie de cambios de formato
# hasta llegar a tener objetos SpatialPolygons legibles por crusCover 



# primero lo transformamos a formato owin (ijroi ->  owin)
x_owin5<- ij2spatstat(x_roi5)

# Ahora hacemos dos operaciones que son necesarias para que las ventanas coincidan sobre la imagen que proyecta crustCover:
# 1) es necesario invertir las coordenadas del eje Y de la ventana, pues imageJ proyecta el eje Y
# invertido (y=0 -> borde superior de la imagen) con respecto a cómo lo proyecta crustCover (y=0 -> borde inferior de la imagen) 
# 2) es necesario reescalar las coordenadas de la ventana tal manera que correspondan al rango de X e Y que oscila entre 0 y 1 en la
# imagen proyectada por crustCover

# En el vector de coordenadas y de la ventana hcemos la operación 1 y 2
w5_y_corr <- (nrow(as.matrix(bandred))-(as.data.frame(x_owin5))$y)/nrow(bandred)

# En el vector de coordenadas x de la ventana hacemos solo la operación 2
w5_x<-(as.data.frame(x_owin5))$x/ncol(bandred)


#Unimos los vectores
xym5<-  cbind(x = w5_x, y = w5_y_corr)
#creamos el polígono
p5 <-  Polygon(xym5)
# Creamos la lista de poígonos con un solo poligono, en este ejemplo el pocillo 5
ps5 <- Polygons(list(p5), "pocillo 5")
# creamos el objeto SpatialPolygons
sps <- SpatialPolygons(list(ps5))

rm(ps5, p5, xym5, w5_x, w5_y_corr, w5_y_corr, x_owin5, x_roi5)

# now use the mask function
# rr <- mask(r, sps)

# layout(matrix(c(1,2,3,4), nrow=2))

# layout(matrix(c(1,1,1,2,2,2,1,1,1,2,2,2,1,1,1,2,2,2,3,3,4,4,5,5,3,3,4,4,5,5), nrow=6))

# layout.show(5)

# par(mfrow=c(3,3), mar=c(2,2,2,5))
# plot, and overlay:
# plotRGB(rr);plot(sps,add=TRUE)


# plotRGB(rr, scale = 1, asp = nrow(vis.red)/ncol(vis.red)) 

par(mfrow=c(1,1))
# Plot full raster and polygon                       
plotRGB(r, scale=1,asp = nrow(vis.red)/ncol(vis.red)  )
plot(sps,add=T, fill=NULL, border="red", lwd=2)

# Crop using extent, rasterize polygon and finally, create poly-raster
#          **** This is the code that you are after ****  
cr <- crop(r, extent(sps), snap="out")                    
fr <- rasterize(sps, cr)   
lr <- raster::mask(x=cr, mask=fr)
plot(lr[[1]], scale=1,asp = nrow(vis.red)/ncol(vis.red) )
# Plot results


par(mfrow=c(3,3), mar=c(2,2,2,5))
# par(mfrow=c(1,1))

grayscale_colors <- gray.colors(256,            # number of different color levels 
                                start = 0.0,    # how black (0) to go
                                end = 1.0,      # how white (1) to go
                                gamma = 2.2,    # correction between how a digital 
                                # camera sees the world and how human eyes see it
                                alpha = NULL) 

plot(lr[[1]], main= "red", asp = nrow(vis.red)/ncol(vis.red), col=grayscale_colors)
plot(lr[[2]], main= "green", asp = nrow(vis.red)/ncol(vis.red), col=grayscale_colors)
plot(lr[[3]], main= "blue", asp = nrow(vis.red)/ncol(vis.red), col=grayscale_colors)


# plotRGB(lr, scale= 1, asp = nrow(vis.red)/ncol(vis.red))
# plot(sps,add=T)


# plotRGB(lr, scale= 1, asp = nrow(vis.red)/ncol(vis.red))


# r <- values(lr[[1]])
# g <- values(lr[[2]])
# b <- values(lr[[3]])

#normalización de colores

# rn <- values(lr[[1]])/maxValue(lr[[1]])
# gn <- values(lr[[2]])/maxValue(lr[[2]])
# bn <- values(lr[[3]])/maxValue(lr[[3]])
# 
rn <- values(lr[[1]])
gn <- values(lr[[2]])
bn <- values(lr[[3]])


# rn <- values(lr[[1]])/1
# gn <- values(lr[[2]])/1
# bn <- values(lr[[3]])/1
# 
# rn <- values(lr[[1]])/255
# gn <- values(lr[[2]])/255
# bn <- values(lr[[3]])/255


lrn <- lr
values(lrn[[1]]) <- rn/(rn+gn+bn)
values(lrn[[2]]) <- gn/(rn+gn+bn)
values(lrn[[3]]) <- bn/(rn+gn+bn)

plot(lrn[[1]], main= "normalize_red"  , asp = nrow(vis.red)/ncol(vis.red), col=grayscale_colors)
plot(lrn[[2]], main= "normalize_green", asp = nrow(vis.red)/ncol(vis.red), col=grayscale_colors)
plot(lrn[[3]], main= "normalize_blue" , asp = nrow(vis.red)/ncol(vis.red), col=grayscale_colors)




# # install.packages("CRImage")
# install.packages("autotreshold")
# installed.packages("ijtiff")
# 
# plotRGB(lrn, scale= 1, asp = nrow(vis.red)/ncol(vis.red), col= grayscale_colors)
# 
# 
# auto_thresh_mask(lrn[[1]], method = Otsu, ignore_black = FALSE,
#                  ignore_white = FALSE, ignore_na = FALSE)


# calculate Ex Red, ex green and ex blue

# Excess Red

ex.red<- function(rn, gn){
  1.4*rn-gn
}

ExR<- ex.red(rn, gn) 
lrexR <- lrn

values(lrexR[[1]]) <- ExR
lrexR <- lrexR[[1]]

# par(mfrow=c(1,1))
plot(lrexR, main="Ex_Red", asp = nrow(vis.red)/ncol(vis.red), col=grayscale_colors)

# Excess Green

ex.green <- function(rn, gn, bn){
  2*gn-rn-bn
}
ExG<- ex.green(rn, gn, bn) 
lrexG <- lrn

values(lrexG[[2]]) <- ExG
lrexG <- lrexG[[2]] 
# par(mfrow=c(1,1))
plot(lrexG, main="Ex_Green",asp = nrow(vis.red)/ncol(vis.red), col=grayscale_colors)

# Excess Blue

ex.blue<- function(bn, gn){
  1.4*bn-gn
}

ExB<- ex.blue(bn, gn) 
lrexB <- lrn

values(lrexB[[3]]) <- ExB
lrexB <- lrexB[[3]]
# par(mfrow=c(1,1))
plot(lrexB, main="Ex_Blue", asp = nrow(vis.red)/ncol(vis.red), col=grayscale_colors)


# Excess green minus excess red: ExGR = ExG-ExR

ExGR <- ExG-ExR
lrexGR <- lrn

values(lrexGR[[1]]) <- ExGR
lrexGR <- lrexGR[[1]]
# par(mfrow=c(1,1))
par(mfrow=c(3,3), mar=c(2,2,2,5))
plot(lrexGR, main="ExGreen minus ExRed", asp = nrow(vis.red)/ncol(vis.red), col=grayscale_colors)
hist(lrexGR, breaks=100, main ="distrib_ExGreen - ExRed", xlab = "", col=grayscale_colors)

# Color index of vegetation extraction

CIVE <-  0.441*rn - 0.811*gn + 0.385*bn + 18.78745
lrCIVE <- lrn
values(lrCIVE[[1]]) <- CIVE
lrCIVE <- lrCIVE[[1]]
# par(mfrow=c(1,1))
plot(lrCIVE, main= "CIVE",asp = nrow(vis.red)/ncol(vis.red), col=grayscale_colors)
hist(lrCIVE, breaks=100, main ="distrib_CIVE", xlab = "", col=grayscale_colors)

#Vegetative

VEG <-  gn/((rn^0.667)-bn^(1-0.667)) 
lrVEG <- lrn
values(lrVEG[[1]]) <- VEG
lrVEG <- lrVEG[[1]]
# par(mfrow=c(1,1))

plot(lrVEG, main="VEG", asp = nrow(vis.red)/ncol(vis.red), col=rev(grayscale_colors))
hist(lrVEG, breaks=100, main ="distrib_VEG", xlab = "", col=grayscale_colors)


# histogramas
# 
# hist(lr[[1]], breaks=100, main ="distrib_red", xlab = "", col=grayscale_colors)
# hist(lr[[2]], breaks=100, main ="distrib_green", xlab = "", col=grayscale_colors)
# hist(lr[[3]], breaks=100, main ="distrib_blue", xlab = "", col=grayscale_colors)
# 
# 
# hist(lrn[[1]], breaks=100, main ="distrib_red_norm", xlab = "", col=grayscale_colors)
# hist(lrn[[2]], breaks=100, main ="distrib_green_norm", xlab = "", col=grayscale_colors)
# hist(lrn[[3]], breaks=100, main ="distrib_blue_norm", xlab = "", col=grayscale_colors)
# 
# hist(lrexR[[1]], breaks=100, main ="distrib_ex_red", xlab = "", col=grayscale_colors)
# hist(lrexG[[2]], breaks=100, main ="distrib_ex_green", xlab = "", col=grayscale_colors)
# hist(lrexB[[3]], breaks=100, main ="distrib_ex_blue", xlab = "", col=grayscale_colors)

par(mfrow=c(3,3), mar=c(2,2,2,5))
hist(lr[[1]], breaks=256, main ="distrib_red", xlab = "", col=grayscale_colors)
hist(lr[[2]], breaks=256, main ="distrib_green", xlab = "", col=grayscale_colors)
hist(lr[[3]], breaks=256, main ="distrib_blue", xlab = "", col=grayscale_colors)


hist(lrn[[1]], breaks=25, main ="distrib_red_norm", xlab = "", col=grayscale_colors)
hist(lrn[[2]], breaks=256, main ="distrib_green_norm", xlab = "", col=grayscale_colors)
hist(lrn[[3]], breaks=256, main ="distrib_blue_norm", xlab = "", col=grayscale_colors)

hist(lrexR, breaks=256, main ="distrib_ex_red", xlab = "", col=grayscale_colors)
hist(lrexG, breaks=256, main ="distrib_ex_green", xlab = "", col=grayscale_colors)
hist(lrexB, breaks=256, main ="distrib_ex_blue", xlab = "", col=grayscale_colors)

hist(lrexGR, breaks=100, main ="distrib_ex_green_red", xlab = "", col=grayscale_colors)



hist(r[[1]], breaks=256, main ="all_red",  xlim=c(0,1), xlab = "", col=grayscale_colors)
hist(r[[2]], breaks=256, main ="all_blue", xlim=c(0,1), xlab = "", col=grayscale_colors)
hist(r[[3]], breaks=256, main ="all_blue", xlim=c(0,1), xlab = "", col=grayscale_colors)



# Histogram Colored (blue and red)
# par(mfrow=c(1,3))
# par(mfrow=c(1,1))

# hist(r[[1]], breaks=100, col=rgb(1,0,0,0.8), xlim=c(0,1), main="Overlapping Histogram", xlab= "Variable")
# hist(r[[2]], breaks=100, col=rgb(0,1,0,0.8), xlim=c(0,1), add=F)
# hist(r[[3]], breaks=100, col=rgb(0,0,1,0.8), xlim=c(0,1), add=F)
# hist(lr[[1]], breaks=100, col=rgb(1,0,0,0.8), xlim=c(0,1), main="Overlapping Histogram", xlab= "Variable")
# hist(lr[[2]], breaks=100, col=rgb(0,1,0,0.8), xlim=c(0,1), add=F)
# hist(lr[[3]], breaks=100, col=rgb(0,0,1,0.8), xlim=c(0,1), add=F)
# 
# 
# box()

###################################################################################
