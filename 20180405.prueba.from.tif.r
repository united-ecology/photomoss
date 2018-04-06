source("/home/indra/Documents/20170901.Musgos/para_indra/R_ndvi/Herramientas_R/crustCover.from.tif.r")

setwd("/home/indra/Documents/20170901.Musgos/para_indra/tif/")
chart <- chart.from.tif()
par(mfrow=c(1,1))
obs.area <- drawObs.from.tif()

#Set the BSC Index threshold to 5 to exclude uncrusted background soil 
#ccSpectral(chart = chart, obs.area = obs.area, rasters = T, ml = T, thresholds 
#= c(0.3, 0.3, 0.3, 0.3, 0.3, 5, 0.3))

# nosotros aun no sabemos el umbral para la guata (probablemente no sea 5, como sugiere el mosscover), nosotros probamos con 0.3
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

#Plot the results
pal <- colorRampPalette(colors = rev(brewer.pal(11, "Spectral")))(100)

png("workflow_example.guata.png", width = 12, height = 8, res = 300, units = "in")
par(mfrow = c(1,3), mar=c(4,4,4,4))
vis <- stack("vis/2018_0124_112118_003_AL_HM.tif", native=T)
ext.obs.area <- extent(c(range(obs.area$col), rev(extent(vis)[4]-range(obs.area$row))))
plotRGB(vis, ext=ext.obs.area)
par(mar=c(2,2,2,2))
umbral.guata <- 0.17
ndvi.musgo <- ndvi.crust
ndvi.musgo[ndvi.musgo < umbral.guata] <- NA
plot(ndvi.musgo, col = pal, colNA = "black", main ="NDVI Values")
distrib <- hist(ndvi.musgo, breaks = 100, main ="NDVI Distribution", xlab = "", col=pal)
abline(v=-0.1)

tot <- length(ndvi.musgo@data@values)
live.surf.percent <- 100* (tot - length(which(is.na(ndvi.musgo@data@values)))) / tot
text(x=median(distrib$breaks), y=max(distrib$counts)*0.8, cex=2,
     labels=paste0("Percentage of live\n matter: ", round(live.surf.percent, 2), " %"))
dev.off()
