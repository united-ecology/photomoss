\name{centroids}
\alias{centroids}
\docType{data}
\title{
%%   ~~ data name/kind ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data("centroids")}
\format{
  The format is:
List of 4
 $ arriba-izq :Formal class 'SpatialPoints' [package "sp"] with 3 slots
  .. ..@ coords     : num [1, 1:2] 0.39 0.607
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : chr "1"
  .. .. .. ..$ : chr [1:2] "x" "y"
  .. ..@ bbox       : num [1:2, 1:2] 0.39 0.607 0.39 0.607
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. ..$ : chr [1:2] "min" "max"
  .. ..@ proj4string:Formal class 'CRS' [package "sp"] with 1 slot
  .. .. .. ..@ projargs: chr NA
 $ arriba-dcha:Formal class 'SpatialPoints' [package "sp"] with 3 slots
  .. ..@ coords     : num [1, 1:2] 0.65 0.61
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : chr "1"
  .. .. .. ..$ : chr [1:2] "x" "y"
  .. ..@ bbox       : num [1:2, 1:2] 0.65 0.61 0.65 0.61
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. ..$ : chr [1:2] "min" "max"
  .. ..@ proj4string:Formal class 'CRS' [package "sp"] with 1 slot
  .. .. .. ..@ projargs: chr NA
 $ abajo-izq  :Formal class 'SpatialPoints' [package "sp"] with 3 slots
  .. ..@ coords     : num [1, 1:2] 0.392 0.354
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : chr "1"
  .. .. .. ..$ : chr [1:2] "x" "y"
  .. ..@ bbox       : num [1:2, 1:2] 0.392 0.354 0.392 0.354
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. ..$ : chr [1:2] "min" "max"
  .. ..@ proj4string:Formal class 'CRS' [package "sp"] with 1 slot
  .. .. .. ..@ projargs: chr NA
 $ abajo-dcha :Formal class 'SpatialPoints' [package "sp"] with 3 slots
  .. ..@ coords     : num [1, 1:2] 0.647 0.35
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : chr "1"
  .. .. .. ..$ : chr [1:2] "x" "y"
  .. ..@ bbox       : num [1:2, 1:2] 0.647 0.35 0.647 0.35
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
data(centroids)
## maybe str(centroids) ; plot(centroids) ...
}
\keyword{datasets}
