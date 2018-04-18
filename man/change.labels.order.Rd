\name{change.labels.order}
\alias{change.labels.order}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
change.labels.order(path)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{path}{
%%     ~~Describe \code{path} here~~
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
function (path) 
{
    all.names <- read.csv(path)
    pots <- nrow(all.names)
    if ((pots/14)\%\%1 == 0) {
        pots.per.block <- 14
    }
    else {
        if ((pots/10)\%\%1 == 0) {
            pots.per.block <- 10
        }
        else {
            message("Names do not correspond either to blocks of 14 (monosp communities), neither blocks of 10 (mixed communities)")
        }
    }
    blocks <- pots/pots.per.block
    if (pots.per.block == 14) {
        block.order <- c(1, NA, 8, NA, 2, 3, 9, 10, 4, 5, 11, 
            12, 6, 7, 13, 14)
    }
    else {
        block.order <- c(1, NA, 6, NA, 2, 3, 7, 8, 4, 5, 9, 10)
    }
    ordered.pots <- unlist(lapply(seq(0, pots.per.block * (blocks - 
        1), by = pots.per.block), "+", block.order))
    names <- all.names[ordered.pots, , drop = F]
    names$names <- as.character(names$names)
    names[is.na(names$names), "names"] <- "mossless"
    print(names)
    write.table(names, "tif.mossless/names.csv", row.names = F, 
        quote = F)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
