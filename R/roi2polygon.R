roi2polygon <-
function(roi.path, tif.path){
      # Para importar los archivos punto .roi instalamos el paquete "RImageJROI"
      # library(RImageJROI)
      
      # Importamos el área de interes que creamos con Image J
      # usamos como EJEMPLO la número 5 (la numeración de las celdillas las sabemos gracias  a la referencia
      # y orden conocido de los alveolos en la fotografía, algo que fuimos
      # apuntando según creabamos las celdillas en el Image J)
      # roi.path <- "roi/"
      # roi.file.names <- Sys.glob(paste0(roi.path, "*.roi"))
      roi.file.names <- roi.path
      x_roi5 <- RImageJROI::read.ijroi(roi.file.names, verbose = FALSE)
      
      # una vez importada el área de interés a nuestro Global Environment vamos a necesitar hacer una serie de cambios de formato
      # hasta llegar a tener objetos SpatialPolygons legibles por crusCover
      
      # para ello, instalamos el paquete "spatstat"
      # library(spatstat)
      
      # primero lo transformamos a formato owin (ijroi ->  owin)
      x_owin5 <- RImageJROI::ij2spatstat(x_roi5)
      
      # Ahora hacemos dos operaciones que son necesarias para que las ventanas coincidan sobre la imagen que proyecta crustCover:
      # 1) es necesario invertir las coordenadas del eje Y de la ventana, pues imageJ proyecta el eje Y
      # invertido (y=0 -> borde superior de la imagen) con respecto a cómo lo proyecta crustCover (y=0 -> borde inferior de la imagen)
      # 2) es necesario reescalar las coordenadas de la ventana tal manera que
      # correspondan al rango de X e Y que oscila entre 0 y 1 en la imagen proyectada por crustCover
      
      # Establecemos la imagen de referencia para hacer los siguietes cálculos
      # tif.path <- "tif/"
      first.tif.filename <- Sys.glob(paste0(tif.path, "vis/*.tif"))[[1]]
      # library(raster)
      RGB_stack_DEM <- stack(first.tif.filename)
      bandred <- raster(first.tif.filename, band=1)
      
      # En el vector de coordenadas y de la ventana hcemos la operación 1 y 2
      w5_y_corr <- (nrow(as.matrix(bandred)) - (as.data.frame(x_owin5))$y) / nrow(RGB_stack_DEM)
      
      # En el vector de coordenadas x de la ventana hacemos solo la operación 2
      w5_x <- (as.data.frame(x_owin5))$x / ncol(RGB_stack_DEM)
      
      
      #Unimos los vectores
      xym5 <-  cbind(x = w5_x, y = w5_y_corr)
      #creamos el polígono
      p5 <-  sp::Polygon(xym5)
      # Creamos la lista de polígonos con un solo polígono, en este ejemplo el pocillo 5
      ps5 <- sp::Polygons(list(p5), "pocillo 5")
      # creamos el objeto SpatialPolygons
      sps <- sp::SpatialPolygons(list(ps5))
      # plot(sps, add=T, col="red")
      return(sps)
}
