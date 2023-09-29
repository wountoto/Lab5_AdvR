\name{muniCorr}
\alias{muniCorr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Correlation of the municipalities
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
muniCorr(df)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
%%     ~~Describe \code{df} here~~
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
function (df) 
{
    df1 <- tidyr::pivot_wider(df, id_cols = c(municipality, period), 
        names_from = kpi, values_from = values)
    graphics::pairs(df1[, -c(1:4)], gap = 0.2, col = "darkgreen", 
        pch = 19, lower.panel = NULL)
    base::print(stats::cor(df1[, -c(1:4)]))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
