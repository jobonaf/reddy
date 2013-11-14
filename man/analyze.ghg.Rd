\name{analyze.ghg}
\alias{analyze.ghg}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
analyze.ghg(dat)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{
%%     ~~Describe \code{dat} here~~
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
function (dat) 
{
    attach(dat)
    mm <- format(as.POSIXct(as.character(dat$date)), format = "\%m")
    Fetch <- Fetch70_1_1_1
    Time <- as.POSIXct(paste(date, time))
    Dat <- data.frame(date = Time, H = H_1_1_1, LE = LE_1_1_1, 
        Fc = Fc_1_1_1, WS = WS_1_1_1, WD = WD_1_1_1, wdir = cut(WD_1_1_1, 
            breaks = seq(0, 360, by = 45)), ustar = ustar_1_1_1, 
        Fetch, month = mm)
    detach(dat)
    library(openair)
    windRose(mydata = Dat, ws = "WS", wd = "WD")
    readline()
    library(rpart)
    tree <- rpart(ustar ~ Fetch + wdir + month, data = Dat)
    plot(tree, compress = T, margin = 0.1)
    text(tree, cex = 0.7)
    readline()
    timeVariation(mydata = Dat, pollutant = c("H", "LE"))
    readline()
    out <- timeVariation(mydata = Dat, pollutant = c("ustar"), 
        plot = F)
    readline()
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
