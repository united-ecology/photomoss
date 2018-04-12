my.draw.obs.from.tif <- function(sps) {
  
  file <- list.files(path="./vis")[1]
  
  vis.tif <- readTIFF(paste("./vis/",file,sep=""))
  
  vis.red <- raster(vis.tif[,,1])
  vis.green <- raster(vis.tif[,,2])
  vis.blue <- raster(vis.tif[,,3])
  
  rgb <- stack(vis.red, vis.green, vis.blue)
  
  # options(warn = -1)

  
  plotRGB(rgb, scale = 1, asp = nrow(vis.red)/ncol(vis.red))
  
     #asp: 
     # numeric. Aspect (ratio of x and y. If NULL, and appropriate value is computed to match 
     # data for the longitude/latitude coordinate reference system, and 1 for planar coordinate 
     # reference systems
     # scale = 1:
     # scale	
     # integer. Maximum (possible) value in the three channels. Defaults to 255 or to the maximum value of x if that is known and 
     # larger than 255
 
  # message("Click at points along the boundary of the observation area in the plotted image. Press the escape key when finished.")
 
  # un.pocillo <- sps
  # options(warn = 0)
  # message("Extracting cells. Please wait.")
  cells <- data.frame(extract(vis.red, sps , cellnumbers = T))[,1]
  out <- data.frame(cells, rowColFromCell(vis.red, cells), xyFromCell(vis.red,cells))
  return(out)
}

