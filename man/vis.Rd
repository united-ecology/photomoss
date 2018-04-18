\name{vis}
\alias{vis}
\docType{data}
\title{
%%   ~~ data name/kind ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data("vis")}
\format{
  The format is:
Formal class 'RasterStack' [package "raster"] with 11 slots
  ..@ filename: chr ""
  ..@ layers  :List of 3
  .. ..$ :Formal class 'RasterLayer' [package "raster"] with 12 slots
  .. .. .. ..@ file    :Formal class '.RasterFile' [package "raster"] with 13 slots
  .. .. .. .. .. ..@ name        : chr ""
  .. .. .. .. .. ..@ datanotation: chr "FLT4S"
  .. .. .. .. .. ..@ byteorder   : chr "little"
  .. .. .. .. .. ..@ nodatavalue : num -Inf
  .. .. .. .. .. ..@ NAchanged   : logi FALSE
  .. .. .. .. .. ..@ nbands      : int 1
  .. .. .. .. .. ..@ bandorder   : chr "BIL"
  .. .. .. .. .. ..@ offset      : int 0
  .. .. .. .. .. ..@ toptobottom : logi TRUE
  .. .. .. .. .. ..@ blockrows   : int 0
  .. .. .. .. .. ..@ blockcols   : int 0
  .. .. .. .. .. ..@ driver      : chr ""
  .. .. .. .. .. ..@ open        : logi FALSE
  .. .. .. ..@ data    :Formal class '.SingleLayerData' [package "raster"] with 13 slots
  .. .. .. .. .. ..@ values    : num [1:12642560] 0.631 0.631 0.624 0.62 0.639 ...
  .. .. .. .. .. ..@ offset    : num 0
  .. .. .. .. .. ..@ gain      : num 1
  .. .. .. .. .. ..@ inmemory  : logi TRUE
  .. .. .. .. .. ..@ fromdisk  : logi FALSE
  .. .. .. .. .. ..@ isfactor  : logi FALSE
  .. .. .. .. .. ..@ attributes: list()
  .. .. .. .. .. ..@ haveminmax: logi TRUE
  .. .. .. .. .. ..@ min       : num 0.0118
  .. .. .. .. .. ..@ max       : num 0.992
  .. .. .. .. .. ..@ band      : int 1
  .. .. .. .. .. ..@ unit      : chr ""
  .. .. .. .. .. ..@ names     : chr "layer.1"
  .. .. .. ..@ legend  :Formal class '.RasterLegend' [package "raster"] with 5 slots
  .. .. .. .. .. ..@ type      : chr(0) 
  .. .. .. .. .. ..@ values    : logi(0) 
  .. .. .. .. .. ..@ color     : logi(0) 
  .. .. .. .. .. ..@ names     : logi(0) 
  .. .. .. .. .. ..@ colortable: logi(0) 
  .. .. .. ..@ title   : chr(0) 
  .. .. .. ..@ extent  :Formal class 'Extent' [package "raster"] with 4 slots
  .. .. .. .. .. ..@ xmin: num 0
  .. .. .. .. .. ..@ xmax: num 1
  .. .. .. .. .. ..@ ymin: num 0
  .. .. .. .. .. ..@ ymax: num 1
  .. .. .. ..@ rotated : logi FALSE
  .. .. .. ..@ rotation:Formal class '.Rotation' [package "raster"] with 2 slots
  .. .. .. .. .. ..@ geotrans: num(0) 
  .. .. .. .. .. ..@ transfun:function ()  
  .. .. .. ..@ ncols   : int 3808
  .. .. .. ..@ nrows   : int 3320
  .. .. .. ..@ crs     :Formal class 'CRS' [package "sp"] with 1 slot
  .. .. .. .. .. ..@ projargs: chr NA
  .. .. .. ..@ history : list()
  .. .. .. ..@ z       : list()
  .. ..$ :Formal class 'RasterLayer' [package "raster"] with 12 slots
  .. .. .. ..@ file    :Formal class '.RasterFile' [package "raster"] with 13 slots
  .. .. .. .. .. ..@ name        : chr ""
  .. .. .. .. .. ..@ datanotation: chr "FLT4S"
  .. .. .. .. .. ..@ byteorder   : chr "little"
  .. .. .. .. .. ..@ nodatavalue : num -Inf
  .. .. .. .. .. ..@ NAchanged   : logi FALSE
  .. .. .. .. .. ..@ nbands      : int 1
  .. .. .. .. .. ..@ bandorder   : chr "BIL"
  .. .. .. .. .. ..@ offset      : int 0
  .. .. .. .. .. ..@ toptobottom : logi TRUE
  .. .. .. .. .. ..@ blockrows   : int 0
  .. .. .. .. .. ..@ blockcols   : int 0
  .. .. .. .. .. ..@ driver      : chr ""
  .. .. .. .. .. ..@ open        : logi FALSE
  .. .. .. ..@ data    :Formal class '.SingleLayerData' [package "raster"] with 13 slots
  .. .. .. .. .. ..@ values    : num [1:12642560] 0.541 0.557 0.549 0.525 0.533 ...
  .. .. .. .. .. ..@ offset    : num 0
  .. .. .. .. .. ..@ gain      : num 1
  .. .. .. .. .. ..@ inmemory  : logi TRUE
  .. .. .. .. .. ..@ fromdisk  : logi FALSE
  .. .. .. .. .. ..@ isfactor  : logi FALSE
  .. .. .. .. .. ..@ attributes: list()
  .. .. .. .. .. ..@ haveminmax: logi TRUE
  .. .. .. .. .. ..@ min       : num 0.00392
  .. .. .. .. .. ..@ max       : num 0.831
  .. .. .. .. .. ..@ band      : int 1
  .. .. .. .. .. ..@ unit      : chr ""
  .. .. .. .. .. ..@ names     : chr "layer.2"
  .. .. .. ..@ legend  :Formal class '.RasterLegend' [package "raster"] with 5 slots
  .. .. .. .. .. ..@ type      : chr(0) 
  .. .. .. .. .. ..@ values    : logi(0) 
  .. .. .. .. .. ..@ color     : logi(0) 
  .. .. .. .. .. ..@ names     : logi(0) 
  .. .. .. .. .. ..@ colortable: logi(0) 
  .. .. .. ..@ title   : chr(0) 
  .. .. .. ..@ extent  :Formal class 'Extent' [package "raster"] with 4 slots
  .. .. .. .. .. ..@ xmin: num 0
  .. .. .. .. .. ..@ xmax: num 1
  .. .. .. .. .. ..@ ymin: num 0
  .. .. .. .. .. ..@ ymax: num 1
  .. .. .. ..@ rotated : logi FALSE
  .. .. .. ..@ rotation:Formal class '.Rotation' [package "raster"] with 2 slots
  .. .. .. .. .. ..@ geotrans: num(0) 
  .. .. .. .. .. ..@ transfun:function ()  
  .. .. .. ..@ ncols   : int 3808
  .. .. .. ..@ nrows   : int 3320
  .. .. .. ..@ crs     :Formal class 'CRS' [package "sp"] with 1 slot
  .. .. .. .. .. ..@ projargs: chr NA
  .. .. .. ..@ history : list()
  .. .. .. ..@ z       : list()
  .. ..$ :Formal class 'RasterLayer' [package "raster"] with 12 slots
  .. .. .. ..@ file    :Formal class '.RasterFile' [package "raster"] with 13 slots
  .. .. .. .. .. ..@ name        : chr ""
  .. .. .. .. .. ..@ datanotation: chr "FLT4S"
  .. .. .. .. .. ..@ byteorder   : chr "little"
  .. .. .. .. .. ..@ nodatavalue : num -Inf
  .. .. .. .. .. ..@ NAchanged   : logi FALSE
  .. .. .. .. .. ..@ nbands      : int 1
  .. .. .. .. .. ..@ bandorder   : chr "BIL"
  .. .. .. .. .. ..@ offset      : int 0
  .. .. .. .. .. ..@ toptobottom : logi TRUE
  .. .. .. .. .. ..@ blockrows   : int 0
  .. .. .. .. .. ..@ blockcols   : int 0
  .. .. .. .. .. ..@ driver      : chr ""
  .. .. .. .. .. ..@ open        : logi FALSE
  .. .. .. ..@ data    :Formal class '.SingleLayerData' [package "raster"] with 13 slots
  .. .. .. .. .. ..@ values    : num [1:12642560] 0.522 0.537 0.537 0.522 0.533 ...
  .. .. .. .. .. ..@ offset    : num 0
  .. .. .. .. .. ..@ gain      : num 1
  .. .. .. .. .. ..@ inmemory  : logi TRUE
  .. .. .. .. .. ..@ fromdisk  : logi FALSE
  .. .. .. .. .. ..@ isfactor  : logi FALSE
  .. .. .. .. .. ..@ attributes: list()
  .. .. .. .. .. ..@ haveminmax: logi TRUE
  .. .. .. .. .. ..@ min       : num 0.00784
  .. .. .. .. .. ..@ max       : num 0.839
  .. .. .. .. .. ..@ band      : int 1
  .. .. .. .. .. ..@ unit      : chr ""
  .. .. .. .. .. ..@ names     : chr "layer.3"
  .. .. .. ..@ legend  :Formal class '.RasterLegend' [package "raster"] with 5 slots
  .. .. .. .. .. ..@ type      : chr(0) 
  .. .. .. .. .. ..@ values    : logi(0) 
  .. .. .. .. .. ..@ color     : logi(0) 
  .. .. .. .. .. ..@ names     : logi(0) 
  .. .. .. .. .. ..@ colortable: logi(0) 
  .. .. .. ..@ title   : chr(0) 
  .. .. .. ..@ extent  :Formal class 'Extent' [package "raster"] with 4 slots
  .. .. .. .. .. ..@ xmin: num 0
  .. .. .. .. .. ..@ xmax: num 1
  .. .. .. .. .. ..@ ymin: num 0
  .. .. .. .. .. ..@ ymax: num 1
  .. .. .. ..@ rotated : logi FALSE
  .. .. .. ..@ rotation:Formal class '.Rotation' [package "raster"] with 2 slots
  .. .. .. .. .. ..@ geotrans: num(0) 
  .. .. .. .. .. ..@ transfun:function ()  
  .. .. .. ..@ ncols   : int 3808
  .. .. .. ..@ nrows   : int 3320
  .. .. .. ..@ crs     :Formal class 'CRS' [package "sp"] with 1 slot
  .. .. .. .. .. ..@ projargs: chr NA
  .. .. .. ..@ history : list()
  .. .. .. ..@ z       : list()
  ..@ title   : chr(0) 
  ..@ extent  :Formal class 'Extent' [package "raster"] with 4 slots
  .. .. ..@ xmin: num 0
  .. .. ..@ xmax: num 1
  .. .. ..@ ymin: num 0
  .. .. ..@ ymax: num 1
  ..@ rotated : logi FALSE
  ..@ rotation:Formal class '.Rotation' [package "raster"] with 2 slots
  .. .. ..@ geotrans: num(0) 
  .. .. ..@ transfun:function ()  
  ..@ ncols   : int 3808
  ..@ nrows   : int 3320
  ..@ crs     :Formal class 'CRS' [package "sp"] with 1 slot
  .. .. ..@ projargs: chr NA
  ..@ history : list()
  ..@ z       : list()
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(vis)
## maybe str(vis) ; plot(vis) ...
}
\keyword{datasets}
