\name{chart.from.tif}
\alias{chart.from.tif}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Selects each color of the calibration card
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
chart.from.tif(tif.path, samp.width = 0.01)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tif.path}{
%%     ~~Describe \code{tif.path} here~~
}
  \item{samp.width}{
%%     ~~Describe \code{samp.width} here~~
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
function (tif.path, samp.width = 0.01) 
{
    file <- Sys.glob(path = paste0(tif.path, "vis/*.tif"))[1]
    vis.tiff <- readTIFF(file)
    vis.red <- raster(vis.tiff[, , 1])
    vis.green <- raster(vis.tiff[, , 2])
    vis.blue <- raster(vis.tiff[, , 3])
    rgb <- stack(vis.red, vis.green, vis.blue)
    options(warn = -1)
    op <- par(mfrow = c(1, 1), mar = c(0, 0, 0, 0), oma = c(0, 
        0, 0, 0))
    on.exit(par(op))
    plotRGB(rgb, scale = 1, asp = nrow(vis.red)/ncol(vis.red))
    options(warn = 0)
    chart.coords <- data.frame(x = numeric(), y = numeric())
    message("Click on all 24 color chart cells in sequence. The sequence follows left to right and starts at cell 1 (brown, top left) and finishes on cell 24 (black, bottom right).")
    for (i in 1:24) {
        options(warn = -1)
        chart.coords[i, 1:2] <- click(xy = T)[1:2]
        options(warn = 0)
    }
    sp.chart <- SpatialPoints(chart.coords)
    chart.buff <- gBuffer(sp.chart, width = samp.width, byid = T)
    plot(chart.buff, add = T, col = "green")
    return(chart.buff)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
