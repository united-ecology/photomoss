setwd("/home/indra/Documents/20170901.Musgos/photomoss.local/")

# Instalamos el paquete "crustCover" dede github utilizando e paquete "githubinstall"
# cuando tengamos el paquete construido
source("crustCover.from.tif.r")

# seleccionamos los colores en la carta de color para hacer la calibración
library(rgeos)
library(tiff)
library(raster)
tif.path <- "tif/"
chart <- chart.from.tif(tif.path)

################################################################################

# Importamos el área de interes que creamos con Image J
# y lo convertimos en un objeto del tipo SpatialPolygons
source("roi2polygon.r")
roi.path <- "roi/"
roi.paths <- Sys.glob(paste0(roi.path, "*.roi"))
# only one ROI
poly <- roi2polygon(roi.paths[[1]], tif.path)
plot(poly, add=T, col="red")

# seleccionamos los pixeles de interés que caen en nuestro polygono de interés
# ONE polygon
obs.df <- extractPIX.from.Poly(tif.path, poly)
# head(obs.df)

################################################################################

# nosotros aun no sabemos el umbral para la guata (probablemente no sea 5, como sugiere el mosscover), nosotros probamos con 0.3
library(randomForestSRC)
library(RColorBrewer)
# one obs.area per picture
ccSpectral.from.tif(tif.path, chart = chart, obs.area = obs.df, ml = F, pdf=T, rasters=T)
