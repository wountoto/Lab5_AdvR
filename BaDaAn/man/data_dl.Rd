\name{data_dl}
\alias{data_dl}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{data download
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
data_dl()
}
%- maybe also 'usage' for other objects documented here.
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
function () 
{
    res <- httr::GET("http://api.kolada.se/v2/data/kpi/N40011,N40010,N09018,N30005,N07037,N09022,N10038,N05002,N20011,N01951,N01800/municipality/0180,1280,1480,0380/year/2013,2014,2015,2016,2017,2018,2019,2020,2021,2022")
    data <- jsonlite::fromJSON(rawToChar(res$content))
    df <- data$values
    for (i in 1:nrow(df)) {
        df[i, 4] <- df$values[[i]][4][df$values[[i]][2] == "T"]
    }
    df$values <- as.numeric(df$values)
    df$municipality[df$municipality == "0180"] <- "stockholm"
    df$municipality[df$municipality == "1280"] <- "malmö"
    df$municipality[df$municipality == "1480"] <- "göteborg"
    df$municipality[df$municipality == "0380"] <- "uppsala"
    df$kpi[df$kpi == "N40011"] <- "labor_spend"
    df$kpi[df$kpi == "N40010"] <- "refugee_spend"
    df$kpi[df$kpi == "N09018"] <- "leisure_spend"
    df$kpi[df$kpi == "N30005"] <- "care_spend"
    df$kpi[df$kpi == "N07037"] <- "infrastructure_spend"
    df$kpi[df$kpi == "N09022"] <- "culture_spend"
    df$kpi[df$kpi == "N10038"] <- "education_spend"
    df$kpi[df$kpi == "N05002"] <- "political_spend"
    df$kpi[df$kpi == "N20011"] <- "elder_disabled_spend"
    df$kpi[df$kpi == "N01951"] <- "residents"
    df$kpi[df$kpi == "N01800"] <- "net_migration"
    return(df)
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
