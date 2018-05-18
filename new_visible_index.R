# setwd("/home/manu/Desktop/Ndvi_prueba/R_ndvi/crustcover.example") 
# library(tiff)
# vis.tif <- readTIFF("vis/IMG_100.tif")
# vis.red <- raster(vis.tif[,,1])
# vis.green <- raster(vis.tif[,,2])
# vis.blue <- raster(vis.tif[,,3])
# 
# r <- stack(vis.red, vis.green, vis.blue)
# 
# 
# # RGB_stack_DEM <-stack("vis/IMG_100.tif")
# bandred <- raster("vis/IMG_100.tif", band=1)
# # install.packages("RImageJROI") 
# library(RImageJROI)
# library(spatstat)
# 
# # Importamos el área de interes que creamos con Image J
# # usamos como EJEMPLO la número 5 (la numeración de las celdillas las sabemos gracias  a la referencia 
# # y orden conocido de los alveolos en la fotografía, algo que fuimos 
# # apuntando según creabamos las celdillas en el Image J)
# x_roi5<- read.ijroi("/home/manu/Desktop/Ndvi_prueba/R_ndvi/obs_area/RoiSet2/para_indra/win8.roi", verbose = FALSE)
# 
# # una vez importada el área de interés a nuestro Global Environment vamos a necesitar hacer una serie de cambios de formato
# # hasta llegar a tener objetos SpatialPolygons legibles por crusCover 
# 
# 
# 
# # primero lo transformamos a formato owin (ijroi ->  owin)
# x_owin5<- ij2spatstat(x_roi5)
# 
# # Ahora hacemos dos operaciones que son necesarias para que las ventanas coincidan sobre la imagen que proyecta crustCover:
# # 1) es necesario invertir las coordenadas del eje Y de la ventana, pues imageJ proyecta el eje Y
# # invertido (y=0 -> borde superior de la imagen) con respecto a cómo lo proyecta crustCover (y=0 -> borde inferior de la imagen) 
# # 2) es necesario reescalar las coordenadas de la ventana tal manera que correspondan al rango de X e Y que oscila entre 0 y 1 en la
# # imagen proyectada por crustCover
# 
# # En el vector de coordenadas y de la ventana hcemos la operación 1 y 2
# w5_y_corr <- (nrow(as.matrix(bandred))-(as.data.frame(x_owin5))$y)/nrow(bandred)
# 
# # En el vector de coordenadas x de la ventana hacemos solo la operación 2
# w5_x<-(as.data.frame(x_owin5))$x/ncol(bandred)
# 
# 
# #Unimos los vectores
# xym5<-  cbind(x = w5_x, y = w5_y_corr)
# #creamos el polígono
# p5 <-  Polygon(xym5)
# # Creamos la lista de poígonos con un solo poligono, en este ejemplo el pocillo 5
# ps5 <- Polygons(list(p5), "pocillo 5")
# # creamos el objeto SpatialPolygons
# sps <- SpatialPolygons(list(ps5))
# 
# rm(ps5, p5, xym5, w5_x, w5_y_corr, w5_y_corr, x_owin5, x_roi5)
# 
# # now use the mask function
# # rr <- mask(r, sps)
# 
# # layout(matrix(c(1,2,3,4), nrow=2))
# 
# # layout(matrix(c(1,1,1,2,2,2,1,1,1,2,2,2,1,1,1,2,2,2,3,3,4,4,5,5,3,3,4,4,5,5), nrow=6))
# 
# # layout.show(5)
# 
# # par(mfrow=c(3,3), mar=c(2,2,2,5))
# # plot, and overlay:
# # plotRGB(rr);plot(sps,add=TRUE)
# 
# 
# # plotRGB(rr, scale = 1, asp = nrow(vis.red)/ncol(vis.red)) 
# 
# par(mfrow=c(1,1))
# # Plot full raster and polygon                       
# plotRGB(r, scale=1,asp = nrow(vis.red)/ncol(vis.red)  )
# plot(sps,add=T, fill=NULL, border="red", lwd=2)

# Crop using extent, rasterize polygon and finally, create poly-raster
# #          **** This is the code that you are after ****  
# crop.raster.polygon <- function(r, sps){
#   cr <- crop(r, extent(sps), snap="out")                    
#   fr <- rasterize(sps, cr)   
#   lr <- raster::mask(x=cr, mask=fr)
#   returrn(lr)
# }

# lr <- crop.raster.polygon(r,sps)

# cr <- crop(r, extent(sps), snap="out")                    
# fr <- rasterize(sps, cr)   
# lr <- raster::mask(x=cr, mask=fr)
# plot(lr[[1]], scale=1,asp = nrow(vis.red)/ncol(vis.red) )
# Plot results


# par(mfrow=c(3,3), mar=c(2,2,2,5))
# par(mfrow=c(1,1))

# grayscale_colors <- gray.colors(256,            # number of different color levels 
                                # start = 0.0,    # how black (0) to go
                                # end = 1.0,      # how white (1) to go
                                # gamma = 2.2,    # correction between how a digital 
                                # # camera sees the world and how human eyes see it
                                # alpha = NULL) 

# plot(lr[[1]], main= "red", asp = nrow(vis.red)/ncol(vis.red), col=grayscale_colors)
# plot(lr[[2]], main= "green", asp = nrow(vis.red)/ncol(vis.red), col=grayscale_colors)
# plot(lr[[3]], main= "blue", asp = nrow(vis.red)/ncol(vis.red), col=grayscale_colors)


# plotRGB(lr, scale= 1, asp = nrow(vis.red)/ncol(vis.red))
# plot(sps,add=T)


# plotRGB(lr, scale= 1, asp = nrow(vis.red)/ncol(vis.red))


# r <- values(lr[[1]])
# g <- values(lr[[2]])
# b <- values(lr[[3]])

#normalización de colores

# rrn <- values(lr[[1]])/maxValue(lr[[1]])
# rgn <- values(lr[[2]])/maxValue(lr[[2]])
# rbn <- values(lr[[3]])/maxValue(lr[[3]])
# 



# rrn <- values(lr[[1]])/1
# rgn <- values(lr[[2]])/1
# rbn <- values(lr[[3]])/1
# 
# rrn <- values(lr[[1]])/255
# rgn <- values(lr[[2]])/255
# rbn <- values(lr[[3]])/255


##################################################################################################
rgb.spectral.normalization <- function(r){
  
  red.normalized.coordinates<- values(r[[1]]) / max(values(r[[1]]))
  green.normalized.coordinates <- values(r[[2]]) / max(values(r[[2]]))
  blue.normalized.coordinates <- values(r[[3]]) / max(values(r[[3]]))  
  
  rgb.norm<- r
  values(rgb.norm[[1]]) <- red.normalized.coordinates / (red.normalized.coordinates + green.normalized.coordinates + blue.normalized.coordinates)
  values(rgb.norm[[2]]) <- green.normalized.coordinates / (red.normalized.coordinates+green.normalized.coordinates+blue.normalized.coordinates)
  values(rgb.norm[[3]]) <- blue.normalized.coordinates / (red.normalized.coordinates+green.normalized.coordinates+blue.normalized.coordinates)
  return(rgb.norm)
}

# rrn <- rgb.color.normalization(r)



#####################################################################################################################################
# Excess Red

excess.red<- function(rgb.norm){
 ex.red<- rgb.norm[[1]]
 values(ex.red) <-  1.4*rgb.norm[[1]] - rgb.norm[[2]]
 return(ex.red)
}

# lrexR <- rrrn[[1]]
# values(lrexR) <- ex.red(rrn, rgn)
# # par(mfrow=c(1,1))
# plot(lrexR, main="Ex_Red", asp = nrow(vis.red)/ncol(vis.red), col=grayscale_colors)

########################################################################################################################################
# Excess Green

excess.green <- function(rgb.norm){
  ex.green<- rgb.norm [[2]]
  values(ex.green) <- 2*rgb.norm[[2]] - rgb.norm[[1]] - rgb.norm[[3]]
  return(ex.green)
}


###########################################################################################################################
# Excess Blue

excess.blue<- function(rgb.norm){
  ex.blue <- rgb.norm[[3]]
  values(ex.blue) <- 1.4*rgb.norm[[3]]-rgb.norm[[2]]
  return(ex.blue)
}

##############################################################################################################################
# lrexB <- lrrn[[3]]
# values(lrexB <- ex.blue(rbn, rgn)
#        # par(mfrow=c(1,1))
#        plot(lrexB, main="Ex_Blue", asp = nrow(vis.red)/ncol(vis.red), col=grayscale_colors)
#        
##########################################################################
 # Excess green minus excess red: ExGR = ExG-ExR

excess.green.sub.excess.red <- function(rgb.norm) {
  ex.green <- rgb.norm [[2]]
  values(ex.green) <-
    2 * rgb.norm[[2]] - rgb.norm[[1]] - rgb.norm[[3]]
  
  ex.red <- rgb.norm[[1]]
  values(ex.red) <-  1.4 * rgb.norm[[1]] - rgb.norm[[2]]
  
  exgr <- ex.green - ex.red
  return(exgr)
}
       
       # lrexGR <- lrrn[[1]]
       # 
       # values(lrexGR) <- exgr(lrexG,lrexR)
       # par(mfrow=c(1,1))
       # par(mfrow=c(3,3), mar=c(2,2,2,5))
       # plot(lrexGR, main="ExGreen minus ExRed", asp = nrow(vis.red)/ncol(vis.red), col=grayscale_colors)
       # hist(lrexGR, breaks=100, main ="distrib_ExGreen - ExRed", xlab = "", col=grayscale_colors)
    
######################################################################################
# Color index of vegetation extraction CIVE
cive <- function(rgb.norm) {
  ciove <- rgm.norm[[1]]
  values(ciove) <-
    0.441 * rgb.norm[[1]] - 0.811 * rgb.norm[[2]] + 0.385 * rgb.norm[[3]] + 18.78745
  return(ciove)
}
       # lrCIVE <- lrrn[[1]]
       # values(lrCIVE) <- cive(rrn, rgn, rbn)
       
       # par(mfrow=c(1,1))
       # plot(lrCIVE, main= "CIVE",asp = nrow(vis.red)/ncol(vis.red), col=grayscale_colors)
       # hist(lrCIVE, breaks=100, main ="distrib_CIVE", xlab = "", col=grayscale_colors)
#################################################################
       #Vegetative
       
veg <- function(rgb.norm) {
  vegetative <- rgb.norm[[1]]
  values(vegetative) <-
    rgb.norm[[2]] / ((rgb.norm[[1]] ^ 0.667) - rgb.norm[[3]] ^ (1 - 0.667))
}
       # 
       # lrVEG <- lrrn[[1]]
       # values(lrVEG) <- veg(rrn,rgn,rbn)
################################################################################