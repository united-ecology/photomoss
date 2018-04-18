setwd("/home/indra/Documents/20170901.Musgos/photomoss.local/")

# Instalamos el paquete "crustCover" dede github utilizando e paquete "githubinstall"
# cuando tengamos el paquete construido
source("crustCover.multiareas.r")

# seleccionamos los colores en la carta de color para hacer la calibración
library(rgeos)
library(tiff)
library(raster)
tif.path <- "tif.mossless/"
# chart <- chart.from.tif(tif.path)

################################################################################

# Importamos el área de interes que creamos con Image J
# y lo convertimos en un objeto del tipo SpatialPolygons
source("roi2polygon.r")
roi.path <- "roi/"
roi.paths <- Sys.glob(paste0(roi.path, "*.roi"))[1:4]
polys <- lapply(roi.paths, roi2polygon, tif.path)
# vis.tif <- readTIFF("tif/vis/2018_0124_112118_003_AL_HM.tif")
# vis.red<-raster(vis.tif[,,1]);vis.green<-raster(vis.tif[,,2]);vis.blue<-raster(vis.tif[,,3])
# vis <- stack(vis.red, vis.green, vis.blue)
# plotRGB(vis, scale = 1, asp = nrow(vis.red)/ncol(vis.red))
# plot(chart, add=T, col="green")
lapply(polys, plot, add=T, col="red")
names(polys) <- substring(gsub(".roi", "", roi.paths), 10)
centroids <- lapply(polys, function(poly){gCentroid(poly)})
mapply(text, centroids, gsub(" ", "\t", names(polys)), cex=0.55)
      
# seleccionamos los pixeles de interés que caen en nuestro polygono de interés
obs.dfs <- lapply(polys, function(x){extractPIX.from.Poly(tif.path, x)})
lapply(obs.dfs, head)

################################################################################

library(randomForestSRC)
library(RColorBrewer)
library(dplyr)
source("crustCover.multiareas.r")
# FOUR obs.areas per picture
ccSpectral.multiareas(tif.path, chart = chart, obs.areas = obs.dfs, ml = F, pdf=T)

# trying uneven obs.areas per picture
# my.photo.order <- list(c(1, 3), c(1:4), c(1:4), c(1:4))
# 
# lapply(my.photo.order, function(pocillos){
#       ccSpectral.multiareas(tif.path, chart = chart, obs.areas = obs.dfs[pocillos], ml = F, pdf=T)
# })

