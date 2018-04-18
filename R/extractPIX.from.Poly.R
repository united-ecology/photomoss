extractPIX.from.Poly <-
function(tif.path, poly){
      file <- Sys.glob(path = paste0(tif.path, "vis/*.tif"))[1]
      vis.tiff <- tiff::readTIFF(file)
      vis.red <- raster(vis.tiff[, , 1])

      message("Extracting cells. Please wait.")
      cells <- data.frame(extract(vis.red, poly, cellnumbers = T))[, 
                                                                   1]
      out <- data.frame(cells, rowColFromCell(vis.red, cells), 
                        xyFromCell(vis.red, cells))
      return(out)
}
