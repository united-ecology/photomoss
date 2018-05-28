#crop raster polygon

crop.raster.polygon <- function(r, sps) {
  cr <- crop(r, extent(sps), snap = "out")
  fr <- rasterize(sps, cr)
  lr <- raster::mask(x = cr, mask = fr)
  return(lr)
}
#########################################################################################################################################

#spectral normalization

rgb.spectral.normalization <- function(r) {
  red.normalized.coordinates   <-
    values(r[[1]])  / max(values(r[[1]]), na.rm = T)
  green.normalized.coordinates <-
    values(r[[2]]) / max(values(r[[2]]), na.rm = T)
  blue.normalized.coordinates  <-
    values(r[[3]]) / max(values(r[[3]]), na.rm = T)
  
  rgb.norm <- r
  values(rgb.norm[[1]]) <-
    red.normalized.coordinates   / (
      red.normalized.coordinates + green.normalized.coordinates + blue.normalized.coordinates
    )
  values(rgb.norm[[2]]) <-
    green.normalized.coordinates / (
      red.normalized.coordinates + green.normalized.coordinates + blue.normalized.coordinates
    )
  values(rgb.norm[[3]]) <-
    blue.normalized.coordinates  / (
      red.normalized.coordinates + green.normalized.coordinates + blue.normalized.coordinates
    )
  return(rgb.norm)
}

#####################################################################################################################################
# Excess Red

excess.red <- function(r) {
  red.normalized.coordinates <- values(r[[1]]) / max(values(r[[1]]))
  green.normalized.coordinates <-
    values(r[[2]]) / max(values(r[[2]]))
  blue.normalized.coordinates <-
    values(r[[3]]) / max(values(r[[3]]))
  
  rgb.norm <- r
  values(rgb.norm[[1]]) <-
    red.normalized.coordinates / (
      red.normalized.coordinates + green.normalized.coordinates + blue.normalized.coordinates
    )
  values(rgb.norm[[2]]) <-
    green.normalized.coordinates / (
      red.normalized.coordinates + green.normalized.coordinates + blue.normalized.coordinates
    )
  values(rgb.norm[[3]]) <-
    blue.normalized.coordinates / (
      red.normalized.coordinates + green.normalized.coordinates + blue.normalized.coordinates
    )
  
  ex.red <- rgb.norm[[1]]
  values(ex.red) <-
    1.4 * values(rgb.norm[[1]]) - values(rgb.norm[[2]])
  return(ex.red)
}



########################################################################################################################################
# Excess Green

excess.green <- function(r) {
  red.normalized.coordinates <- values(r[[1]]) / max(values(r[[1]]))
  green.normalized.coordinates <-
    values(r[[2]]) / max(values(r[[2]]))
  blue.normalized.coordinates <-
    values(r[[3]]) / max(values(r[[3]]))
  
  rgb.norm <- r
  values(rgb.norm[[1]]) <-
    red.normalized.coordinates / (
      red.normalized.coordinates + green.normalized.coordinates + blue.normalized.coordinates
    )
  values(rgb.norm[[2]]) <-
    green.normalized.coordinates / (
      red.normalized.coordinates + green.normalized.coordinates + blue.normalized.coordinates
    )
  values(rgb.norm[[3]]) <-
    blue.normalized.coordinates / (
      red.normalized.coordinates + green.normalized.coordinates + blue.normalized.coordinates
    )
  
  ex.green <- rgb.norm [[2]]
  values(ex.green) <-
    2 * values(rgb.norm[[2]]) - values(rgb.norm[[1]]) - values(rgb.norm[[3]])
  
  return(ex.green)
}


###########################################################################################################################
# Excess Blue

excess.blue <- function(r) {
  red.normalized.coordinates   <- values(r[[1]]) / max(values(r[[1]]))
  green.normalized.coordinates <-
    values(r[[2]]) / max(values(r[[2]]))
  blue.normalized.coordinates  <-
    values(r[[3]]) / max(values(r[[3]]))
  
  rgb.norm <- r
  values(rgb.norm[[1]]) <-
    red.normalized.coordinates / (
      red.normalized.coordinates + green.normalized.coordinates + blue.normalized.coordinates
    )
  values(rgb.norm[[2]]) <-
    green.normalized.coordinates / (
      red.normalized.coordinates + green.normalized.coordinates + blue.normalized.coordinates
    )
  values(rgb.norm[[3]]) <-
    blue.normalized.coordinates / (
      red.normalized.coordinates + green.normalized.coordinates + blue.normalized.coordinates
    )
  ex.blue <- rgb.norm[[3]]
  values(ex.blue) <-
    1.4 * values(rgb.norm[[3]]) - values(rgb.norm[[2]])
  return(ex.blue)
}


##########################################################################
# Excess green minus excess red: ExGR = ExG-ExR

excess.green.sub.excess.red <- function(r) {
  red.normalized.coordinates <- values(r[[1]]) / max(values(r[[1]]))
  green.normalized.coordinates <-
    values(r[[2]]) / max(values(r[[2]]))
  blue.normalized.coordinates <-
    values(r[[3]]) / max(values(r[[3]]))
  
  rgb.norm <- r
  values(rgb.norm[[1]]) <-
    red.normalized.coordinates / (
      red.normalized.coordinates + green.normalized.coordinates + blue.normalized.coordinates
    )
  values(rgb.norm[[2]]) <-
    green.normalized.coordinates / (
      red.normalized.coordinates + green.normalized.coordinates + blue.normalized.coordinates
    )
  values(rgb.norm[[3]]) <-
    blue.normalized.coordinates / (
      red.normalized.coordinates + green.normalized.coordinates + blue.normalized.coordinates
    )
  
  ex.green <- rgb.norm [[2]]
  values(ex.green) <-
    2  * values(rgb.norm[[2]]) - values(rgb.norm[[1]]) - values(rgb.norm[[3]])
  
  ex.red <- rgb.norm[[1]]
  values(ex.red) <-
    1.4 * values(rgb.norm[[1]]) - values(rgb.norm[[2]])
  
  exgr <- ex.green - ex.red
  return(exgr)
}



######################################################################################
# Color index of vegetation extraction CIVE
cive <- function(r) {
  red.normalized.coordinates <- values(r[[1]]) / max(values(r[[1]]))
  green.normalized.coordinates <-
    values(r[[2]]) / max(values(r[[2]]))
  blue.normalized.coordinates <-
    values(r[[3]]) / max(values(r[[3]]))
  
  rgb.norm <- r
  values(rgb.norm[[1]]) <-
    red.normalized.coordinates / (
      red.normalized.coordinates + green.normalized.coordinates + blue.normalized.coordinates
    )
  values(rgb.norm[[2]]) <-
    green.normalized.coordinates / (
      red.normalized.coordinates + green.normalized.coordinates + blue.normalized.coordinates
    )
  values(rgb.norm[[3]]) <-
    blue.normalized.coordinates / (
      red.normalized.coordinates + green.normalized.coordinates + blue.normalized.coordinates
    )
  
  ciove <- rgm.norm[[1]]
  values(ciove) <-
    0.441 * values(rgb.norm[[1]]) - 0.811 * values(rgb.norm[[2]]) + 0.385 * values(rgb.norm[[3]]) + 18.78745
  return(ciove)
}

#################################################################
#Vegetative

veg <- function(r) {
  red.normalized.coordinates <- values(r[[1]]) / max(values(r[[1]]))
  green.normalized.coordinates <-
    values(r[[2]]) / max(values(r[[2]]))
  blue.normalized.coordinates <-
    values(r[[3]]) / max(values(r[[3]]))
  
  rgb.norm <- r
  values(rgb.norm[[1]]) <-
    red.normalized.coordinates / (
      red.normalized.coordinates + green.normalized.coordinates + blue.normalized.coordinates
    )
  values(rgb.norm[[2]]) <-
    green.normalized.coordinates / (
      red.normalized.coordinates + green.normalized.coordinates + blue.normalized.coordinates
    )
  values(rgb.norm[[3]]) <-
    blue.normalized.coordinates / (
      red.normalized.coordinates + green.normalized.coordinates + blue.normalized.coordinates
    )
  
  vegetative <- rgb.norm[[1]]
  
  values(vegetative) <-
    values(rgb.norm[[2]]) / ((values(rgb.norm[[1]]) ^ 0.667) - values(rgb.norm[[3]]) ^ (1 - 0.667))
  return(vegetative)
}

################################################################################
