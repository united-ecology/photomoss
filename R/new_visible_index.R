#spectral normalization

rgb.spectral.normalization <- function(r) {
  red.normalized.coordinates <-
    raster::getValues(r[[1]]) / max(raster::getValues(r[[1]]))
  green.normalized.coordinates <-
    raster::getValues(r[[2]]) / max(raster::getValues(r[[2]]))
  blue.normalized.coordinates <-
    raster::getValues(r[[3]]) / max(raster::getValues(r[[3]]))
  
  rgb.norm <- raster::raster(r)
  

  red.norm <-
    raster::setValues (rgb.norm, (
      red.normalized.coordinates / (
        red.normalized.coordinates + green.normalized.coordinates + blue.normalized.coordinates
      )
    ))
  
  
  green.norm <-
    raster::setValues(rgb.norm, (
      green.normalized.coordinates / (
        red.normalized.coordinates + green.normalized.coordinates + blue.normalized.coordinates
      )
    ))
  
  blue.norm <-
    raster::setValues(rgb.norm, (
      blue.normalized.coordinates / (
        red.normalized.coordinates + green.normalized.coordinates + blue.normalized.coordinates
      )
    ))
  
  rgb.norm <- raster::brick(red.norm, green.norm, blue.norm)
  # 
  return(rgb.norm)
}

#####################################################################################################################################


excess.red <- function(r) {
  red.normalized.coordinates <-
    raster::getValues(r[[1]]) / max(raster::getValues(r[[1]]))
  green.normalized.coordinates <-
    raster::getValues(r[[2]]) / max(raster::getValues(r[[2]]))
  blue.normalized.coordinates <-
    raster::getValues(r[[3]]) / max(raster::getValues(r[[3]]))
  
  rgb.norm <- raster::raster(r)
  
  
  red.norm <-
    raster::setValues (rgb.norm, (
      red.normalized.coordinates / (
        red.normalized.coordinates + green.normalized.coordinates + blue.normalized.coordinates
      )
    ))
  
  
  green.norm <-
    raster::setValues(rgb.norm, (
      green.normalized.coordinates / (
        red.normalized.coordinates + green.normalized.coordinates + blue.normalized.coordinates
      )
    ))
  
  blue.norm <-
    raster::setValues(rgb.norm, (
      blue.normalized.coordinates / (
        red.normalized.coordinates + green.normalized.coordinates + blue.normalized.coordinates
      )
    ))
  
  rgb.norm <- raster::brick(red.norm, green.norm, blue.norm)
  
  ex.red <- raster::raster(r[[1]])
  v <-
    1.4 *  raster::getValues(rgb.norm[[1]]) -  raster::getValues(rgb.norm[[2]])
  ex.red <- raster::setValues(ex.red, v)
  
  return(ex.red)
  
}

########################################################################################################################################
# Excess Green

excess.green <- function(r) {
  red.normalized.coordinates <-
    raster::getValues(r[[1]]) / max(raster::getValues(r[[1]]))
  green.normalized.coordinates <-
    raster::getValues(r[[2]]) / max(raster::getValues(r[[2]]))
  blue.normalized.coordinates <-
    raster::getValues(r[[3]]) / max(raster::getValues(r[[3]]))
  
  rgb.norm <- raster::raster(r)
  
  
  red.norm <-
    raster::setValues (rgb.norm, (
      red.normalized.coordinates / (
        red.normalized.coordinates + green.normalized.coordinates + blue.normalized.coordinates
      )
    ))
  
  green.norm <-
    raster::setValues(rgb.norm, (
      green.normalized.coordinates / (
        red.normalized.coordinates + green.normalized.coordinates + blue.normalized.coordinates
      )
    ))
  
  blue.norm <-
    raster::setValues(rgb.norm, (
      blue.normalized.coordinates / (
        red.normalized.coordinates + green.normalized.coordinates + blue.normalized.coordinates
      )
    ))
  
  rgb.norm <- raster::brick(red.norm, green.norm, blue.norm)
  
  
  ex.green <- raster::raster(r[[2]])
  v <-
    2 * raster::getValues(rgb.norm[[2]]) - raster::getValues(rgb.norm[[1]]) - raster::getValues(rgb.norm[[3]])
  ex.green <- raster::setValues(ex.green, v)
  
  return(ex.green)
}


###########################################################################################################################
# Excess Blue
excess.blue <- function(r) {
  red.normalized.coordinates <-
    raster::getValues(r[[1]]) / max(raster::getValues(r[[1]]))
  green.normalized.coordinates <-
    raster::getValues(r[[2]]) / max(raster::getValues(r[[2]]))
  blue.normalized.coordinates <-
    raster::getValues(r[[3]]) / max(raster::getValues(r[[3]]))
  
  rgb.norm <- raster::raster(r)
  
  
  red.norm <-
    raster::setValues (rgb.norm, (
      red.normalized.coordinates / (
        red.normalized.coordinates + green.normalized.coordinates + blue.normalized.coordinates
      )
    ))
  
  green.norm <-
    raster::setValues(rgb.norm, (
      green.normalized.coordinates / (
        red.normalized.coordinates + green.normalized.coordinates + blue.normalized.coordinates
      )
    ))
  
  blue.norm <-
    raster::setValues(rgb.norm, (
      blue.normalized.coordinates / (
        red.normalized.coordinates + green.normalized.coordinates + blue.normalized.coordinates
      )
    ))
  
  rgb.norm <- raster::brick(red.norm, green.norm, blue.norm)

  ex.blue <- raster::raster(r[[3]])
  v <- 1.4 * raster::getValues(rgb.norm[[3]]) - raster::getValues(rgb.norm[[2]])
  ex.blue <- raster::setValues(ex.blue, v)
  return(ex.blue)
}



##########################################################################
# Excess green minus excess red: ExGR = ExG-ExR

excess.green.sub.excess.red <- function(r) {
  red.normalized.coordinates <-
    raster::getValues(r[[1]]) / max(raster::getValues(r[[1]]))
  green.normalized.coordinates <-
    raster::getValues(r[[2]]) / max(raster::getValues(r[[2]]))
  blue.normalized.coordinates <-
    raster::getValues(r[[3]]) / max(raster::getValues(r[[3]]))
  
  rgb.norm <- raster::raster(r)
  
  
  red.norm <-
    raster::setValues (rgb.norm, (
      red.normalized.coordinates / (
        red.normalized.coordinates + green.normalized.coordinates + blue.normalized.coordinates
      )
    ))
  
  green.norm <-
    raster::setValues(rgb.norm, (
      green.normalized.coordinates / (
        red.normalized.coordinates + green.normalized.coordinates + blue.normalized.coordinates
      )
    ))
  
  blue.norm <-
    raster::setValues(rgb.norm, (
      blue.normalized.coordinates / (
        red.normalized.coordinates + green.normalized.coordinates + blue.normalized.coordinates
      )
    ))
  
  rgb.norm <- raster::brick(red.norm, green.norm, blue.norm)
  
  ex.green <- raster::raster(r[[2]])
  v <-
    2 * raster::getValues(rgb.norm[[2]]) - raster::getValues(rgb.norm[[1]]) - raster::getValues(rgb.norm[[3]])
  ex.green <- raster::setValues(ex.green, v)
  
  # ex.green <- raster(rrgb.norm [[2]]
  # values(ex.green) <-
  #   2  * values(rgb.norm[[2]]) - values(rgb.norm[[1]]) - values(rgb.norm[[3]])
  #
  ex.red <- raster::raster(r[[1]])
  v <-
    1.4 *  raster::getValues(rgb.norm[[1]]) -  raster::getValues(rgb.norm[[2]])
  ex.red <- raster::setValues(ex.red, v)
  
  # ex.red <- rgb.norm[[1]]
  # values(ex.red) <-
  #   1.4 * values(rgb.norm[[1]]) - values(rgb.norm[[2]])
  #
  exgr <- raster::raster(r[[1]])
  v <- raster::getValues(ex.green) - raster::getValues(ex.red)
  exgr <- raster::setValues(exgr, v)
  return(exgr)
}



######################################################################################
# Color index of vegetation extraction CIVE
cive <- function(r) {
  red.normalized.coordinates <-
    raster::getValues(r[[1]]) / max(raster::getValues(r[[1]]))
  green.normalized.coordinates <-
    raster::getValues(r[[2]]) / max(raster::getValues(r[[2]]))
  blue.normalized.coordinates <-
    raster::getValues(r[[3]]) / max(raster::getValues(r[[3]]))
  
  rgb.norm <- raster::raster(r)
  
  red.norm <-
    raster::setValues (rgb.norm, (
      red.normalized.coordinates / (
        red.normalized.coordinates + green.normalized.coordinates + blue.normalized.coordinates
      )
    ))
  
  green.norm <-
    raster::setValues(rgb.norm, (
      green.normalized.coordinates / (
        red.normalized.coordinates + green.normalized.coordinates + blue.normalized.coordinates
      )
    ))
  
  blue.norm <-
    raster::setValues(rgb.norm, (
      blue.normalized.coordinates / (
        red.normalized.coordinates + green.normalized.coordinates + blue.normalized.coordinates
      )
    ))
  
  rgb.norm <- raster::brick(red.norm, green.norm, blue.norm)
  
  cive <- raster::raster(r[[1]])
  v <-
    0.441 * raster::getValues(rgb.norm[[1]]) - 0.811 * raster::getValues(rgb.norm[[2]]) + 0.385 * raster::getValues(rgb.norm[[3]]) + 18.78745
  cive <- raster::setValues(cive, v)
  return(cive)
}

#################################################################
#Vegetative

veg <- function(r) {
  red.normalized.coordinates <-
    raster::getValues(r[[1]]) / max(raster::getValues(r[[1]]))
  green.normalized.coordinates <-
    raster::getValues(r[[2]]) / max(raster::getValues(r[[2]]))
  blue.normalized.coordinates <-
    raster::getValues(r[[3]]) / max(raster::getValues(r[[3]]))
  
  rgb.norm <- raster::raster(r)
  
  red.norm <-
    raster::setValues (rgb.norm, (
      red.normalized.coordinates / (
        red.normalized.coordinates + green.normalized.coordinates + blue.normalized.coordinates
      )
    ))
  
  green.norm <-
    raster::setValues(rgb.norm, (
      green.normalized.coordinates / (
        red.normalized.coordinates + green.normalized.coordinates + blue.normalized.coordinates
      )
    ))
  
  blue.norm <-
    raster::setValues(rgb.norm, (
      blue.normalized.coordinates / (
        red.normalized.coordinates + green.normalized.coordinates + blue.normalized.coordinates
      )
    ))
  
  rgb.norm <- raster::brick(red.norm, green.norm, blue.norm)
  
  
  veg <- raster::raster(r[[1]])
  
  v <-
    raster::getValues(rgb.norm[[2]]) / ((raster::getValues(rgb.norm[[1]]) ^ 0.667) - raster::getValues(rgb.norm[[3]]) ^ (1 - 0.667))
   
  veg <- raster::setValues(veg, v)
  return(veg)
}



