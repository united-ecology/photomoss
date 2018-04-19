extractPIX.from.Poly <-
function(tif.path, poly){
      file <- Sys.glob(path = paste0(tif.path, "vis/*.tif"))[1]
      vis.tiff <- tiff::readTIFF(file)
      vis.red <- raster::raster(vis.tiff[, , 1])

      message("Extracting cells. Please wait.")
      cells <- data.frame(raster::extract(vis.red, poly, cellnumbers = T))[, 
                                                                   1]
      out <- data.frame(cells, raster::rowColFromCell(vis.red, cells), 
                        raster::xyFromCell(vis.red, cells))
      return(out)
}
