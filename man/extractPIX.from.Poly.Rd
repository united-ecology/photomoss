\name{extractPIX.from.Poly}
\alias{extractPIX.from.Poly}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Given a polygon, selects the pixels contained in it
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
extractPIX.from.Poly(tif.path, poly)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tif.path}{
Path (can be relative to the working directory \code{getwd()}) to a directory with 2 subdirectories named "nir" and "vis"
}
  \item{poly}{
An object of class SpatialPolygons that selects the area in which ccSpectral.multiarea will calculate indices.
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

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
