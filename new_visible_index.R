
#########################################################################################################################################

#spectral normalization

rgb.spectral.normalization <- function(r) {
  red.normalized.coordinates <-
    getValues(r[[1]]) / max(getValues(r[[1]]) )
  green.normalized.coordinates <-
    getValues(r[[2]]) / max(getValues(r[[2]]))
  blue.normalized.coordinates <-
    getValues(r[[3]]) / max(getValues(r[[3]]))
  
  rgb.norm <- r
  
  rgb.norm <- setValues(rgb.norm, (red.normalized.coordinates / (
    red.normalized.coordinates + green.normalized.coordinates + blue.normalized.coordinates
  )), layer= 1 )
    rgb.norm <- setValues(rgb.norm, (green.normalized.coordinates / (
    red.normalized.coordinates + green.normalized.coordinates + blue.normalized.coordinates
  )), layer= 2 )
  rgb.norm <- setValues(rgb.norm, (blue.normalized.coordinates / (
    red.normalized.coordinates + green.normalized.coordinates + blue.normalized.coordinates
  )), layer= 3 )
  
  return(rgb.norm)
}


r.rgb.norm <- rgb.spectral.normalization(r)
r.rgb.norm
lr.rgb.norm <- rgb.spectral.normalization(lr)
lr.rgb.norm
#####################################################################################################################################


excess.red <- function(r) {
  
  red.normalized.coordinates <-
   getValues(r[[1]]) / max(getValues(r[[1]]))
  green.normalized.coordinates <-
    getValues(r[[2]]) / max(getValues(r[[2]]))
  blue.normalized.coordinates <-
    getValues(r[[3]]) / max(getValues(r[[3]]))
  
  rgb.norm <- raster(r)
  
  
  red.norm <- setValues (rgb.norm, (red.normalized.coordinates / (
    red.normalized.coordinates + green.normalized.coordinates + blue.normalized.coordinates)))
  
  ??setValues
  green.norm <- setValues(rgb.norm, (green.normalized.coordinates / (
    red.normalized.coordinates + green.normalized.coordinates + blue.normalized.coordinates)))

  blue.norm <- setValues(rgb.norm, (blue.normalized.coordinates / (
    red.normalized.coordinates + green.normalized.coordinates + blue.normalized.coordinates)))

  rgb.norm <- brick(red.norm, green.norm, blue.norm)
  
  ex.red <- raster(r[[1]])
  v <- 1.4 *  getValues(rgb.norm[[1]]) -  getValues(rgb.norm[[2]])
  ex.red <- setValues(ex.red, v)
  
  return(ex.red)
  
  }

# ex.red <- excess.red(lr)
# ex.red
########################################################################################################################################
# Excess Green

excess.green <- function(r) {
  
  red.normalized.coordinates <-
    getValues(r[[1]]) / max(getValues(r[[1]]))
  green.normalized.coordinates <-
    getValues(r[[2]]) / max(getValues(r[[2]]))
  blue.normalized.coordinates <-
    getValues(r[[3]]) / max(getValues(r[[3]]))
  
  rgb.norm <- raster(r)
  
  
  red.norm <- setValues (rgb.norm, (red.normalized.coordinates / (
    red.normalized.coordinates + green.normalized.coordinates + blue.normalized.coordinates)))
  
  green.norm <- setValues(rgb.norm, (green.normalized.coordinates / (
    red.normalized.coordinates + green.normalized.coordinates + blue.normalized.coordinates)))
  
  blue.norm <- setValues(rgb.norm, (blue.normalized.coordinates / (
    red.normalized.coordinates + green.normalized.coordinates + blue.normalized.coordinates)))
  
  rgb.norm <- brick(red.norm, green.norm, blue.norm)
  
  # red.normalized.coordinates <- values(r[[1]]) / max(values(r[[1]]))
  # green.normalized.coordinates <-
  #   values(r[[2]]) / max(values(r[[2]]))
  # blue.normalized.coordinates <-
  #   values(r[[3]]) / max(values(r[[3]]))
  # 
  # rgb.norm <- r
  # values(rgb.norm[[1]]) <-
  #   red.normalized.coordinates / (
  #     red.normalized.coordinates + green.normalized.coordinates + blue.normalized.coordinates
  #   )
  # values(rgb.norm[[2]]) <-
  #   green.normalized.coordinates / (
  #     red.normalized.coordinates + green.normalized.coordinates + blue.normalized.coordinates
  #   )
  # values(rgb.norm[[3]]) <-
  #   blue.normalized.coordinates / (
  #     red.normalized.coordinates + green.normalized.coordinates + blue.normalized.coordinates
  #   )
  # 
  # ex.red <- raster(r[[1]])
  # v <- 1.4 *  getValues(rgb.norm[[1]]) -  getValues(rgb.norm[[2]])
  # ex.red <- setValues(ex.red, v)
  # 
  # return(ex.red)
  
    ex.green <- raster(r[[2]])
    v <- 2 * getValues(rgb.norm[[2]]) - getValues(rgb.norm[[1]]) - getValues(rgb.norm[[3]])
    ex.green <- setValues(ex.green, v)

  return(ex.green)
}

excess.green(lr)
###########################################################################################################################
# Excess Blue
excess.blue <- function(r) {

red.normalized.coordinates <-
  getValues(r[[1]]) / max(getValues(r[[1]]))
green.normalized.coordinates <-
  getValues(r[[2]]) / max(getValues(r[[2]]))
blue.normalized.coordinates <-
  getValues(r[[3]]) / max(getValues(r[[3]]))

rgb.norm <- raster(r)


red.norm <- setValues (rgb.norm, (red.normalized.coordinates / (
  red.normalized.coordinates + green.normalized.coordinates + blue.normalized.coordinates)))

green.norm <- setValues(rgb.norm, (green.normalized.coordinates / (
  red.normalized.coordinates + green.normalized.coordinates + blue.normalized.coordinates)))

blue.norm <- setValues(rgb.norm, (blue.normalized.coordinates / (
  red.normalized.coordinates + green.normalized.coordinates + blue.normalized.coordinates)))

rgb.norm <- brick(red.norm, green.norm, blue.norm)
# excess.blue <- function(r) {
#   red.normalized.coordinates   <- values(r[[1]]) / max(values(r[[1]]))
#   green.normalized.coordinates <-
#     values(r[[2]]) / max(values(r[[2]]))
#   blue.normalized.coordinates  <-
#     values(r[[3]]) / max(values(r[[3]]))
#   
#   rgb.norm <- r
#   values(rgb.norm[[1]]) <-
#     red.normalized.coordinates / (
#       red.normalized.coordinates + green.normalized.coordinates + blue.normalized.coordinates
#     )
#   values(rgb.norm[[2]]) <-
#     green.normalized.coordinates / (
#       red.normalized.coordinates + green.normalized.coordinates + blue.normalized.coordinates
#     )
#   values(rgb.norm[[3]]) <-
#     blue.normalized.coordinates / (
#       red.normalized.coordinates + green.normalized.coordinates + blue.normalized.coordinates
#     )
  ex.blue <- raster(r[[3]])
  v<- 1.4 * values(rgb.norm[[3]]) - values(rgb.norm[[2]])
  ex.blue <- setValues(ex.blue, v)
  return(ex.blue)
}

excess.blue(lr)

##########################################################################
# Excess green minus excess red: ExGR = ExG-ExR

excess.green.sub.excess.red <- function(r) {

  red.normalized.coordinates <-
    getValues(r[[1]]) / max(getValues(r[[1]]))
  green.normalized.coordinates <-
    getValues(r[[2]]) / max(getValues(r[[2]]))
  blue.normalized.coordinates <-
    getValues(r[[3]]) / max(getValues(r[[3]]))
  
  rgb.norm <- raster(r)
  
  
  red.norm <- setValues (rgb.norm, (red.normalized.coordinates / (
    red.normalized.coordinates + green.normalized.coordinates + blue.normalized.coordinates)))
  
  green.norm <- setValues(rgb.norm, (green.normalized.coordinates / (
    red.normalized.coordinates + green.normalized.coordinates + blue.normalized.coordinates)))
  
  blue.norm <- setValues(rgb.norm, (blue.normalized.coordinates / (
    red.normalized.coordinates + green.normalized.coordinates + blue.normalized.coordinates)))
  
  rgb.norm <- brick(red.norm, green.norm, blue.norm)
    
  # red.normalized.coordinates <- 
  #   values(r[[1]]) / max(values(r[[1]]))
  # green.normalized.coordinates <-
  #   values(r[[2]]) / max(values(r[[2]]))
  # blue.normalized.coordinates <-
  #   values(r[[3]]) / max(values(r[[3]]))
  # 
  # rgb.norm <- r
  # values(rgb.norm[[1]]) <-
  #   red.normalized.coordinates / (
  #     red.normalized.coordinates + green.normalized.coordinates + blue.normalized.coordinates
  #   )
  # values(rgb.norm[[2]]) <-
  #   green.normalized.coordinates / (
  #     red.normalized.coordinates + green.normalized.coordinates + blue.normalized.coordinates
  #   )
  # values(rgb.norm[[3]]) <-
  #   blue.normalized.coordinates / (
  #     red.normalized.coordinates + green.normalized.coordinates + blue.normalized.coordinates
  #   )

  ex.green <- raster(r[[2]])
  v <- 2 * getValues(rgb.norm[[2]]) - getValues(rgb.norm[[1]]) - getValues(rgb.norm[[3]])
  ex.green <- setValues(ex.green, v)
  
  # ex.green <- raster(rrgb.norm [[2]]
  # values(ex.green) <-
  #   2  * values(rgb.norm[[2]]) - values(rgb.norm[[1]]) - values(rgb.norm[[3]])
  #
  ex.red <- raster(r[[1]])
  v <- 1.4 *  getValues(rgb.norm[[1]]) -  getValues(rgb.norm[[2]])
  ex.red <- setValues(ex.red, v)  
   
  # ex.red <- rgb.norm[[1]]
  # values(ex.red) <-
  #   1.4 * values(rgb.norm[[1]]) - values(rgb.norm[[2]])
  # 
  exgr <- raster(r[[1]])
  v <- getValues(ex.green) - getValues(ex.red)
  exgr <- setValues(exgr,v)
  return(exgr)
}



######################################################################################
# Color index of vegetation extraction CIVE
cive <- function(r) {
  red.normalized.coordinates <-
    getValues(r[[1]]) / max(getValues(r[[1]]))
  green.normalized.coordinates <-
    getValues(r[[2]]) / max(getValues(r[[2]]))
  blue.normalized.coordinates <-
    getValues(r[[3]]) / max(getValues(r[[3]]))
  
  rgb.norm <- raster(r)
  
  red.norm <- setValues (rgb.norm, (red.normalized.coordinates / (
    red.normalized.coordinates + green.normalized.coordinates + blue.normalized.coordinates)))
  
  green.norm <- setValues(rgb.norm, (green.normalized.coordinates / (
    red.normalized.coordinates + green.normalized.coordinates + blue.normalized.coordinates)))
  
  blue.norm <- setValues(rgb.norm, (blue.normalized.coordinates / (
    red.normalized.coordinates + green.normalized.coordinates + blue.normalized.coordinates)))
  
  rgb.norm <- brick(red.norm, green.norm, blue.norm)
  
  cive <- raster(r[[1]])
  v <- 0.441 * getValues(rgb.norm[[1]]) - 0.811 * getValues(rgb.norm[[2]]) + 0.385 * getValues(rgb.norm[[3]]) + 18.78745
  cive <- setValues(cive, v)
  return(cive)
}

#################################################################
#Vegetative

veg <- function(r) {
  
  red.normalized.coordinates <-
    getValues(r[[1]]) / max(getValues(r[[1]]))
  green.normalized.coordinates <-
    getValues(r[[2]]) / max(getValues(r[[2]]))
  blue.normalized.coordinates <-
    getValues(r[[3]]) / max(getValues(r[[3]]))
  
  rgb.norm <- raster(r)
  
  red.norm <- setValues (rgb.norm, (red.normalized.coordinates / (
    red.normalized.coordinates + green.normalized.coordinates + blue.normalized.coordinates)))
  
  green.norm <- setValues(rgb.norm, (green.normalized.coordinates / (
    red.normalized.coordinates + green.normalized.coordinates + blue.normalized.coordinates)))
  
  blue.norm <- setValues(rgb.norm, (blue.normalized.coordinates / (
    red.normalized.coordinates + green.normalized.coordinates + blue.normalized.coordinates)))
  
  rgb.norm <- brick(red.norm, green.norm, blue.norm)
  
  
  veg <- raster(r[[1]])
  
  v <- c(getValues(rgb.norm[[2]]) / ((getValues(rgb.norm[[1]]) ^ 0.667) - getValues(rgb.norm[[3]]) ^ (1 - 0.667)))  
  veg <- setValues(veg, r) 
  return(veg)
}

################################################################################
