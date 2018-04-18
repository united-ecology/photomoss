\name{roi2polygon}
\alias{roi2polygon}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
roi2polygon(roi.path, tif.path)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{roi.path}{
%%     ~~Describe \code{roi.path} here~~
}
  \item{tif.path}{
%%     ~~Describe \code{tif.path} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (roi.path, tif.path) 
{
    library(RImageJROI)
    roi.file.names <- roi.path
    x_roi5 <- read.ijroi(roi.file.names, verbose = FALSE)
    library(spatstat)
    x_owin5 <- ij2spatstat(x_roi5)
    first.tif.filename <- Sys.glob(paste0(tif.path, "vis/*.tif"))[[1]]
    library(raster)
    RGB_stack_DEM <- stack(first.tif.filename)
    bandred <- raster(first.tif.filename, band = 1)
    w5_y_corr <- (nrow(as.matrix(bandred)) - (as.data.frame(x_owin5))$y)/nrow(RGB_stack_DEM)
    w5_x <- (as.data.frame(x_owin5))$x/ncol(RGB_stack_DEM)
    xym5 <- cbind(x = w5_x, y = w5_y_corr)
    p5 <- Polygon(xym5)
    ps5 <- Polygons(list(p5), "pocillo 5")
    sps <- SpatialPolygons(list(ps5))
    return(sps)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
