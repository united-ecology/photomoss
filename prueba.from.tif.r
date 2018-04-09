source("/home/manu/Desktop/Ndvi_prueba/R_ndvi/crustcover.from.tif/20180409.crustCover.from.tif.r")

setwd("/home/manu/Desktop/Ndvi_prueba/R_ndvi/Herramientas_R/tif.cruscover/")
library(tiff)
library(raster)
library(rgeos)

###############################################################################################################
# INPUT
chart <- chart.from.tif()
par(mfrow=c(1,1)); obs.area <- drawObs.from.tif()

poly <- obs.area[[2]]
obs.area <- obs.area[[1]]


###############################################################################################################
# CALCULATE
#Set the BSC Index threshold to 5 to exclude uncrusted background soil 
#ccSpectral(chart = chart, obs.area = obs.area, rasters = T, ml = T, thresholds 
#= c(0.3, 0.3, 0.3, 0.3, 0.3, 5, 0.3))

# nosotros aun no sabemos el umbral para la guata (probablemente no sea 5, como sugiere el mosscover), nosotros probamos con 0.3
library(randomForestSRC)
library(RColorBrewer)
ccSpectral.from.tif(chart = chart, obs.area = obs.area, rasters = T, ml = T, thresholds = c(0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3))

#Load the processed spectral data, divide by 1000 to restore to original scale
latest.directory <- Sys.glob("output*")
latest.directory <- latest.directory[length(latest.directory)]
spec.stack <- stack(paste0(latest.directory, "/obs_1_spectral_stack.tif"))/1000

#Load the binary cutoff data
cover.stack <- stack(paste0(latest.directory, "/obs_1_cover_stack.tif"))

#Select the NDVI layer from the spectral stack
obs1.ndvi <- spec.stack[[1]]

#Select the BSCI binary mask from the cover stack, and set uncrusted values as NA
obs1.bsci.bin <- cover.stack[[6]]
rcl <- cbind(0, NA)
obs1.bsci.mask <- reclassify(obs1.bsci.bin, rcl)

#Mask out NDVI values below the BSCI threshold
ndvi.crust <- mask(obs1.ndvi, obs1.bsci.mask)


###############################################################################################################
# MAS PLOTS
# otra manera más completa de representar
# png("example.musgo.en.L.png", width = 12, height = 8, res = 300, units = "in")

layout(matrix(c(1,2,2,3,3,3,4,5,5), nrow=3)) #, widths=c()
layout.show(5)

par(mar=c(2,2,2,2), oma=c(2,2,2,2))
vis.tif <- readTIFF("vis/2018_0124_112118_003_AL.tif")
vis.red <- raster(vis.tif[,,1])
vis.green <- raster(vis.tif[,,2])
vis.blue <- raster(vis.tif[,,3])

vis <- stack(vis.red, vis.green, vis.blue)

plotRGB(vis, scale = 1, asp = nrow(vis.red)/ncol(vis.red))

plotRGB(vis, ext=extent(poly), scale = 1, asp = nrow(vis.red)/ncol(vis.red))
plot(poly, add=T, fill=NULL, border="red", lwd=2)

umbral.guata <- 0.30
ndvi.musgo <- ndvi.crust
value.below <- -1
ndvi.musgo[ndvi.musgo < umbral.guata] <- value.below

breaks <- 100
pal <- c("black", rev(terrain.colors(breaks)))

par(mar=c(2,2,2,2))
image(ndvi.musgo, col = pal, main ="NDVI Values", axes=T, ann=F, asp=1)
legend.label <- seq(min(ndvi.musgo@data@values, na.rm = T), max(ndvi.musgo@data@values, na.rm = T), length.out = 5)
# legend(extent(ndvi.musgo)[2]-0.01, extent(ndvi.musgo)[4]-0.01, bty=0, legend=legend.label, fill= pal)
distrib <- hist(ndvi.musgo, breaks = breaks, main ="NDVI Distribution", xlab = "", col=(pal))
distrib <- hist(ndvi.musgo[ndvi.musgo>value.below], breaks = breaks, main ="NDVI Distribution", xlab = "", col=pal[-1])

# calcular porcentaje de pìxeles:
# - NA
# - observed
# - debajo del threshold (=guata, sombras...)
# - encima del threshold (= musgo vivo)

tot <- length(ndvi.musgo@data@values)
na.surf.percent <- 100 * (length(!which(is.na(ndvi.musgo@data@values)))) / tot
obs.surf.percent <- 100 * (tot - length(which(is.na(ndvi.musgo@data@values)))) / tot
abs.below <- 100* length(which(ndvi.musgo@data@values==value.below)) / tot
abs.over <- 100* length(which(ndvi.musgo@data@values!=value.below)) / tot
rel.below <- 100* abs.below / obs.surf.percent
rel.over <- 100* abs.over / obs.surf.percent

text(x=median(distrib$breaks), y=max(distrib$counts)*0.8, cex=1,
     labels=paste0(
       # "Percentage of live\n matter: ", round(live.surf.percent, 2), " %"
       "NA: ", round(na.surf.percent, 2), " %\n",
       "Observed: ", round(obs.surf.percent, 2), " %\n",
       "Abs below thresh: ", round(abs.below, 2), " %\n",
       "Abs over thresh: ", round(abs.over, 2), " %\n\n",
       "Rel below thresh: ", round(rel.below, 2), " %\n",
       "Rel over thresh: ", round(rel.over, 2), " %\n"
     ))

# dev.off()

###############################################################################################################
# PLOT
# png("workflow_example.guata.png", width = 12, height = 8, res = 300, units = "in")
# par(mfrow = c(1,3), mar=c(4,4,4,4))
# vis <- readTIFF("vis/2018_0124_112118_003_AL.tif", native=T)
# vis.red <- raster(vis.tif[,,1])
# vis.green <- raster(vis.tif[,,2])
# vis.blue <- raster(vis.tif[,,3])
# 
# vis <- stack(vis.red, vis.green, vis.blue)
# 
# ext.obs.area <- extent(c(range(obs.area$col), rev(extent(vis)[4]-range(obs.area$row))))
# plotRGB(vis, ext=extent(poly), scale = 1, asp = nrow(vis.red)/ncol(vis.red))
# 
# par(mar=c(2,2,2,2))
# umbral.guata <- 0.18
# ndvi.musgo <- ndvi.crust
# value.below <- -1
# ndvi.musgo[ndvi.musgo < umbral.guata] <- value.below
# 
# breaks <- 100
# # pal <- colorRampPalette(colors = rev(brewer.pal(11, "Spectral")))(breaks)
# pal <- c("black", rev(terrain.colors(breaks)))
# plot(ndvi.musgo, col = pal, colNA = "white", main ="NDVI Values", nc=1, nr=1, maxnl=1)
# distrib <- hist(ndvi.musgo[ndvi.musgo>value.below], breaks = breaks, main ="NDVI Distribution", xlab = "", col=pal[-1])
# 
# # calcular porcentaje de pìxeles:
# # - NA
# # - observed
# # - debajo del threshold (=guata, sombras...)
# # - encima del threshold (= musgo vivo)
# 
# tot <- length(ndvi.musgo@data@values)
# na.surf.percent <- 100 * (length(!which(is.na(ndvi.musgo@data@values)))) / tot
# obs.surf.percent <- 100 * (tot - length(which(is.na(ndvi.musgo@data@values)))) / tot
# abs.below <- 100* length(which(ndvi.musgo@data@values==value.below)) / tot
# abs.over <- 100* length(which(ndvi.musgo@data@values!=value.below)) / tot
# rel.below <- 100* abs.below / obs.surf.percent
# rel.over <- 100* abs.over / obs.surf.percent
# 
# text(x=median(distrib$breaks), y=max(distrib$counts)*0.8, cex=2,
#      labels=paste0(
#      # "Percentage of live\n matter: ", round(live.surf.percent, 2), " %"
#        "NA: ", round(na.surf.percent, 2), " %\n",
#        "Observed: ", round(obs.surf.percent, 2), " %\n",
#        "Abs below thresh: ", round(abs.below, 2), " %\n",
#        "Abs over thresh: ", round(abs.over, 2), " %\n\n",
#        "Rel below thresh: ", round(rel.below, 2), " %\n",
#        "Rel over thresh: ", round(rel.over, 2), " %\n"
#        ))

# dev.off()