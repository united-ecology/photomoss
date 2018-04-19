chart.from.tif <- function(tif.path, samp.width = 0.01){
      file <- Sys.glob(path = paste0(tif.path, "vis/*.tif"))[1]
      vis.tiff <- tiff::readTIFF(file)
      vis.red <- raster::raster(vis.tiff[, , 1])
      vis.green <- raster::raster(vis.tiff[, , 2])
      vis.blue <- raster::raster(vis.tiff[, , 3])
      rgb <- raster::stack(vis.red, vis.green, vis.blue)
      options(warn = -1)
      
      op <- par(mfrow=c(1,1), mar=c(0,0,0,0), oma=c(0,0,0,0))
      on.exit(par(op))
      # X11()
      raster::plotRGB(rgb, scale = 1, asp = nrow(vis.red)/ncol(vis.red))
      options(warn = 0)
      
      chart.coords <- data.frame(x = numeric(), y = numeric())
      message("Click on all 24 color chart cells in sequence. The sequence follows left to right and starts at cell 1 (brown, top left) and finishes on cell 24 (black, bottom right).")
      for (i in 1:24) {
            options(warn = -1)
            chart.coords[i, 1:2] <- click(xy = T)[1:2]
            options(warn = 0)
      }
      sp.chart <- sp::SpatialPoints(chart.coords)
      chart.buff <- rgeos::gBuffer(sp.chart, width = samp.width, byid = T)
      plot(chart.buff, add = T, col = "green")
      
      return(chart.buff)
}
