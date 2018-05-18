


##################################################################################################
rgb.spectral.normalization <- function(r){
  
  red.normalized.coordinates<- values(r[[1]]) / max(values(r[[1]]))
  green.normalized.coordinates <- values(r[[2]]) / max(values(r[[2]]))
  blue.normalized.coordinates <- values(r[[3]]) / max(values(r[[3]]))  
  
  rgb.norm<- r
  values(rgb.norm[[1]]) <- red.normalized.coordinates / (red.normalized.coordinates + green.normalized.coordinates + blue.normalized.coordinates)
  values(rgb.norm[[2]]) <- green.normalized.coordinates / (red.normalized.coordinates+green.normalized.coordinates+blue.normalized.coordinates)
  values(rgb.norm[[3]]) <- blue.normalized.coordinates / (red.normalized.coordinates+green.normalized.coordinates+blue.normalized.coordinates)
  return(rgb.norm)
}





#####################################################################################################################################
# Excess Red

excess.red<- function(rgb.norm){
 ex.red<- rgb.norm[[1]]
 values(ex.red) <-  1.4*rgb.norm[[1]] - rgb.norm[[2]]
 return(ex.red)
}



########################################################################################################################################
# Excess Green

excess.green <- function(rgb.norm){
  ex.green<- rgb.norm [[2]]
  values(ex.green) <- 2*rgb.norm[[2]] - rgb.norm[[1]] - rgb.norm[[3]]
  return(ex.green)
}


###########################################################################################################################
# Excess Blue

excess.blue<- function(rgb.norm){
  ex.blue <- rgb.norm[[3]]
  values(ex.blue) <- 1.4*rgb.norm[[3]]-rgb.norm[[2]]
  return(ex.blue)
}


##########################################################################
 # Excess green minus excess red: ExGR = ExG-ExR

excess.green.sub.excess.red <- function(rgb.norm) {
  ex.green <- rgb.norm [[2]]
  values(ex.green) <-
    2 * rgb.norm[[2]] - rgb.norm[[1]] - rgb.norm[[3]]
  
  ex.red <- rgb.norm[[1]]
  values(ex.red) <-  1.4 * rgb.norm[[1]] - rgb.norm[[2]]
  
  exgr <- ex.green - ex.red
  return(exgr)
}
       

    
######################################################################################
# Color index of vegetation extraction CIVE
cive <- function(rgb.norm) {
  ciove <- rgm.norm[[1]]
  values(ciove) <-
    0.441 * rgb.norm[[1]] - 0.811 * rgb.norm[[2]] + 0.385 * rgb.norm[[3]] + 18.78745
  return(ciove)
}

#################################################################
       #Vegetative
       
veg <- function(rgb.norm) {
  vegetative <- rgb.norm[[1]]
  values(vegetative) <-
    rgb.norm[[2]] / ((rgb.norm[[1]] ^ 0.667) - rgb.norm[[3]] ^ (1 - 0.667))
}
    
################################################################################