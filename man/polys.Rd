\name{polys}
\alias{polys}
\docType{data}
\title{
%%   ~~ data name/kind ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data("polys")}
\format{
  The format is:
List of 4
 $ arriba-izq :Formal class 'SpatialPolygons' [package "sp"] with 4 slots
  .. ..@ polygons   :List of 1
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] 0.39 0.607
  .. .. .. .. .. .. .. ..@ area   : num 0.0188
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 0.326 0.454 0.454 0.326 0.326 ...
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] 0.39 0.607
  .. .. .. .. ..@ ID       : chr "pocillo 5"
  .. .. .. .. ..@ area     : num 0.0188
  .. ..@ plotOrder  : int 1
  .. ..@ bbox       : num [1:2, 1:2] 0.326 0.533 0.454 0.68
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. ..$ : chr [1:2] "min" "max"
  .. ..@ proj4string:Formal class 'CRS' [package "sp"] with 1 slot
  .. .. .. ..@ projargs: chr NA
 $ arriba-dcha:Formal class 'SpatialPolygons' [package "sp"] with 4 slots
  .. ..@ polygons   :List of 1
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] 0.65 0.61
  .. .. .. .. .. .. .. ..@ area   : num 0.0188
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 0.586 0.714 0.714 0.586 0.586 ...
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] 0.65 0.61
  .. .. .. .. ..@ ID       : chr "pocillo 5"
  .. .. .. .. ..@ area     : num 0.0188
  .. ..@ plotOrder  : int 1
  .. ..@ bbox       : num [1:2, 1:2] 0.586 0.537 0.714 0.684
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. ..$ : chr [1:2] "min" "max"
  .. ..@ proj4string:Formal class 'CRS' [package "sp"] with 1 slot
  .. .. .. ..@ projargs: chr NA
 $ abajo-izq  :Formal class 'SpatialPolygons' [package "sp"] with 4 slots
  .. ..@ polygons   :List of 1
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] 0.392 0.354
  .. .. .. .. .. .. .. ..@ area   : num 0.0188
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 0.328 0.456 0.456 0.328 0.328 ...
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] 0.392 0.354
  .. .. .. .. ..@ ID       : chr "pocillo 5"
  .. .. .. .. ..@ area     : num 0.0188
  .. ..@ plotOrder  : int 1
  .. ..@ bbox       : num [1:2, 1:2] 0.328 0.28 0.456 0.427
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. ..$ : chr [1:2] "min" "max"
  .. ..@ proj4string:Formal class 'CRS' [package "sp"] with 1 slot
  .. .. .. ..@ projargs: chr NA
 $ abajo-dcha :Formal class 'SpatialPolygons' [package "sp"] with 4 slots
  .. ..@ polygons   :List of 1
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] 0.647 0.35
  .. .. .. .. .. .. .. ..@ area   : num 0.0188
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 0.583 0.711 0.711 0.583 0.583 ...
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] 0.647 0.35
  .. .. .. .. ..@ ID       : chr "pocillo 5"
  .. .. .. .. ..@ area     : num 0.0188
  .. ..@ plotOrder  : int 1
  .. ..@ bbox       : num [1:2, 1:2] 0.583 0.277 0.711 0.423
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. ..$ : chr [1:2] "min" "max"
  .. ..@ proj4string:Formal class 'CRS' [package "sp"] with 1 slot
  .. .. .. ..@ projargs: chr NA
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
data(polys)
## maybe str(polys) ; plot(polys) ...
}
\keyword{datasets}
