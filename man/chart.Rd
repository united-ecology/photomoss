\name{chart}
\alias{chart}
\docType{data}
\title{
%%   ~~ data name/kind ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data("chart")}
\format{
  The format is:
Formal class 'SpatialPolygons' [package "sp"] with 4 slots
  ..@ polygons   :List of 24
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. ..@ Polygons :List of 1
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 0.231 0.873
  .. .. .. .. .. .. ..@ area   : num 0.000309
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:21, 1:2] 0.241 0.241 0.239 0.237 0.234 ...
  .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. ..@ plotOrder: int 1
  .. .. .. ..@ labpt    : num [1:2] 0.231 0.873
  .. .. .. ..@ ID       : chr "1"
  .. .. .. ..@ area     : num 0.000309
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. ..@ Polygons :List of 1
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 0.226 0.817
  .. .. .. .. .. .. ..@ area   : num 0.000309
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:21, 1:2] 0.236 0.236 0.235 0.232 0.23 ...
  .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. ..@ plotOrder: int 1
  .. .. .. ..@ labpt    : num [1:2] 0.226 0.817
  .. .. .. ..@ ID       : chr "2"
  .. .. .. ..@ area     : num 0.000309
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. ..@ Polygons :List of 1
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 0.226 0.756
  .. .. .. .. .. .. ..@ area   : num 0.000309
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:21, 1:2] 0.236 0.236 0.235 0.232 0.23 ...
  .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. ..@ plotOrder: int 1
  .. .. .. ..@ labpt    : num [1:2] 0.226 0.756
  .. .. .. ..@ ID       : chr "3"
  .. .. .. ..@ area     : num 0.000309
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. ..@ Polygons :List of 1
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 0.229 0.701
  .. .. .. .. .. .. ..@ area   : num 0.000309
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:21, 1:2] 0.239 0.238 0.237 0.235 0.232 ...
  .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. ..@ plotOrder: int 1
  .. .. .. ..@ labpt    : num [1:2] 0.229 0.701
  .. .. .. ..@ ID       : chr "4"
  .. .. .. ..@ area     : num 0.000309
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. ..@ Polygons :List of 1
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 0.226 0.639
  .. .. .. .. .. .. ..@ area   : num 0.000309
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:21, 1:2] 0.236 0.236 0.235 0.232 0.23 ...
  .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. ..@ plotOrder: int 1
  .. .. .. ..@ labpt    : num [1:2] 0.226 0.639
  .. .. .. ..@ ID       : chr "5"
  .. .. .. ..@ area     : num 0.000309
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. ..@ Polygons :List of 1
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 0.226 0.575
  .. .. .. .. .. .. ..@ area   : num 0.000309
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:21, 1:2] 0.236 0.236 0.235 0.232 0.23 ...
  .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. ..@ plotOrder: int 1
  .. .. .. ..@ labpt    : num [1:2] 0.226 0.575
  .. .. .. ..@ ID       : chr "6"
  .. .. .. ..@ area     : num 0.000309
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. ..@ Polygons :List of 1
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 0.176 0.87
  .. .. .. .. .. .. ..@ area   : num 0.000309
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:21, 1:2] 0.186 0.185 0.184 0.182 0.179 ...
  .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. ..@ plotOrder: int 1
  .. .. .. ..@ labpt    : num [1:2] 0.176 0.87
  .. .. .. ..@ ID       : chr "7"
  .. .. .. ..@ area     : num 0.000309
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. ..@ Polygons :List of 1
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 0.176 0.813
  .. .. .. .. .. .. ..@ area   : num 0.000309
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:21, 1:2] 0.186 0.185 0.184 0.182 0.179 ...
  .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. ..@ plotOrder: int 1
  .. .. .. ..@ labpt    : num [1:2] 0.176 0.813
  .. .. .. ..@ ID       : chr "8"
  .. .. .. ..@ area     : num 0.000309
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. ..@ Polygons :List of 1
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 0.177 0.752
  .. .. .. .. .. .. ..@ area   : num 0.000309
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:21, 1:2] 0.187 0.186 0.185 0.183 0.18 ...
  .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. ..@ plotOrder: int 1
  .. .. .. ..@ labpt    : num [1:2] 0.177 0.752
  .. .. .. ..@ ID       : chr "9"
  .. .. .. ..@ area     : num 0.000309
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. ..@ Polygons :List of 1
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 0.177 0.694
  .. .. .. .. .. .. ..@ area   : num 0.000309
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:21, 1:2] 0.187 0.186 0.185 0.183 0.18 ...
  .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. ..@ plotOrder: int 1
  .. .. .. ..@ labpt    : num [1:2] 0.177 0.694
  .. .. .. ..@ ID       : chr "10"
  .. .. .. ..@ area     : num 0.000309
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. ..@ Polygons :List of 1
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 0.176 0.639
  .. .. .. .. .. .. ..@ area   : num 0.000309
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:21, 1:2] 0.186 0.185 0.184 0.182 0.179 ...
  .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. ..@ plotOrder: int 1
  .. .. .. ..@ labpt    : num [1:2] 0.176 0.639
  .. .. .. ..@ ID       : chr "11"
  .. .. .. ..@ area     : num 0.000309
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. ..@ Polygons :List of 1
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 0.173 0.576
  .. .. .. .. .. .. ..@ area   : num 0.000309
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:21, 1:2] 0.183 0.183 0.181 0.179 0.176 ...
  .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. ..@ plotOrder: int 1
  .. .. .. ..@ labpt    : num [1:2] 0.173 0.576
  .. .. .. ..@ ID       : chr "12"
  .. .. .. ..@ area     : num 0.000309
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. ..@ Polygons :List of 1
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 0.124 0.867
  .. .. .. .. .. .. ..@ area   : num 0.000309
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:21, 1:2] 0.134 0.133 0.132 0.13 0.127 ...
  .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. ..@ plotOrder: int 1
  .. .. .. ..@ labpt    : num [1:2] 0.124 0.867
  .. .. .. ..@ ID       : chr "13"
  .. .. .. ..@ area     : num 0.000309
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. ..@ Polygons :List of 1
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 0.124 0.812
  .. .. .. .. .. .. ..@ area   : num 0.000309
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:21, 1:2] 0.134 0.133 0.132 0.13 0.127 ...
  .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. ..@ plotOrder: int 1
  .. .. .. ..@ labpt    : num [1:2] 0.124 0.812
  .. .. .. ..@ ID       : chr "14"
  .. .. .. ..@ area     : num 0.000309
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. ..@ Polygons :List of 1
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 0.123 0.749
  .. .. .. .. .. .. ..@ area   : num 0.000309
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:21, 1:2] 0.133 0.132 0.131 0.129 0.126 ...
  .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. ..@ plotOrder: int 1
  .. .. .. ..@ labpt    : num [1:2] 0.123 0.749
  .. .. .. ..@ ID       : chr "15"
  .. .. .. ..@ area     : num 0.000309
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. ..@ Polygons :List of 1
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 0.121 0.694
  .. .. .. .. .. .. ..@ area   : num 0.000309
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:21, 1:2] 0.131 0.131 0.13 0.127 0.125 ...
  .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. ..@ plotOrder: int 1
  .. .. .. ..@ labpt    : num [1:2] 0.121 0.694
  .. .. .. ..@ ID       : chr "16"
  .. .. .. ..@ area     : num 0.000309
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. ..@ Polygons :List of 1
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 0.124 0.63
  .. .. .. .. .. .. ..@ area   : num 0.000309
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:21, 1:2] 0.134 0.133 0.132 0.13 0.127 ...
  .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. ..@ plotOrder: int 1
  .. .. .. ..@ labpt    : num [1:2] 0.124 0.63
  .. .. .. ..@ ID       : chr "17"
  .. .. .. ..@ area     : num 0.000309
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. ..@ Polygons :List of 1
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 0.123 0.572
  .. .. .. .. .. .. ..@ area   : num 0.000309
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:21, 1:2] 0.133 0.132 0.131 0.129 0.126 ...
  .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. ..@ plotOrder: int 1
  .. .. .. ..@ labpt    : num [1:2] 0.123 0.572
  .. .. .. ..@ ID       : chr "18"
  .. .. .. ..@ area     : num 0.000309
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. ..@ Polygons :List of 1
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 0.0731 0.8684
  .. .. .. .. .. .. ..@ area   : num 0.000309
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:21, 1:2] 0.0831 0.0826 0.0812 0.079 0.0762 ...
  .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. ..@ plotOrder: int 1
  .. .. .. ..@ labpt    : num [1:2] 0.0731 0.8684
  .. .. .. ..@ ID       : chr "19"
  .. .. .. ..@ area     : num 0.000309
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. ..@ Polygons :List of 1
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 0.0707 0.8116
  .. .. .. .. .. .. ..@ area   : num 0.000309
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:21, 1:2] 0.0807 0.0802 0.0788 0.0766 0.0738 ...
  .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. ..@ plotOrder: int 1
  .. .. .. ..@ labpt    : num [1:2] 0.0707 0.8116
  .. .. .. ..@ ID       : chr "20"
  .. .. .. ..@ area     : num 0.000309
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. ..@ Polygons :List of 1
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 0.0719 0.7535
  .. .. .. .. .. .. ..@ area   : num 0.000309
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:21, 1:2] 0.0819 0.0814 0.08 0.0778 0.075 ...
  .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. ..@ plotOrder: int 1
  .. .. .. ..@ labpt    : num [1:2] 0.0719 0.7535
  .. .. .. ..@ ID       : chr "21"
  .. .. .. ..@ area     : num 0.000309
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. ..@ Polygons :List of 1
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 0.0743 0.6953
  .. .. .. .. .. .. ..@ area   : num 0.000309
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:21, 1:2] 0.0843 0.0839 0.0824 0.0802 0.0774 ...
  .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. ..@ plotOrder: int 1
  .. .. .. ..@ labpt    : num [1:2] 0.0743 0.6953
  .. .. .. ..@ ID       : chr "22"
  .. .. .. ..@ area     : num 0.000309
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. ..@ Polygons :List of 1
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 0.0719 0.6343
  .. .. .. .. .. .. ..@ area   : num 0.000309
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:21, 1:2] 0.0819 0.0814 0.08 0.0778 0.075 ...
  .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. ..@ plotOrder: int 1
  .. .. .. ..@ labpt    : num [1:2] 0.0719 0.6343
  .. .. .. ..@ ID       : chr "23"
  .. .. .. ..@ area     : num 0.000309
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. ..@ Polygons :List of 1
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 0.0683 0.5665
  .. .. .. .. .. .. ..@ area   : num 0.000309
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:21, 1:2] 0.0783 0.0778 0.0764 0.0742 0.0714 ...
  .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. ..@ plotOrder: int 1
  .. .. .. ..@ labpt    : num [1:2] 0.0683 0.5665
  .. .. .. ..@ ID       : chr "24"
  .. .. .. ..@ area     : num 0.000309
  ..@ plotOrder  : int [1:24] 14 5 24 2 19 20 4 22 11 9 ...
  ..@ bbox       : num [1:2, 1:2] 0.0583 0.5565 0.2413 0.8826
  .. ..- attr(*, "dimnames")=List of 2
  .. .. ..$ : chr [1:2] "x" "y"
  .. .. ..$ : chr [1:2] "min" "max"
  ..@ proj4string:Formal class 'CRS' [package "sp"] with 1 slot
  .. .. ..@ projargs: chr NA
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
data(chart)
## maybe str(chart) ; plot(chart) ...
}
\keyword{datasets}
