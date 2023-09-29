\name{muniLines}
\alias{muniLines}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plot time series
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
muniLines(muni, df)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{muni}{
%%     ~~Describe \code{muni} here~~
}
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
function (muni, df) 
{
    if ((!tolower(muni) \%in\% df$municipality) == TRUE || !is.data.frame(df)) {
        print("Check the spelling or choose another municipality")
        stop()
    }
    muni <- tolower(muni)
    indice <- df[df$municipality == muni, ]
    indice <- indice[indice$kpi != "residents", ]
    indice <- indice[indice$kpi != "net_migration", ]
    base::print(ggplot2::ggplot(indice, ggplot2::aes(x = period, 
        y = values, color = kpi)) + ggplot2::geom_line(size = 1.5) + 
        ggplot2::scale_x_continuous(breaks = seq(min(indice$period), 
            max(indice$period), by = 2)) + ggplot2::scale_color_discrete("KPI") + 
        ggplot2::theme_bw() + ggplot2::ylab("Spend per resident") + 
        ggplot2::xlab("Year") + ggplot2::ggtitle(paste0("Time series of municipal spend for ", 
        stringr::str_to_title(muni))) + ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)))
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
