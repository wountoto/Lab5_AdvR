\name{muniCompare}
\alias{muniCompare}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Comparison of municipalities
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
muniCompare(muni_vec, df, var)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{muni_vec}{
%%     ~~Describe \code{muni_vec} here~~
}
  \item{df}{
%%     ~~Describe \code{df} here~~
}
  \item{var}{
%%     ~~Describe \code{var} here~~
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
function (muni_vec, df, var) 
{
    indice <- df[df$municipality \%in\% muni_vec, ]
    indice <- indice[indice$kpi == var, ]
    Cost <- "Spent"
    cost_title <- "municipal spend on "
    if (var == "residents" | var == "net_migration") {
        cost_title <- "number of "
        Cost <- "Count"
    }
    var_title <- strsplit(var, "_")[[1]][1]
    base::print(ggplot2::ggplot(indice, ggplot2::aes(x = period, 
        y = values, color = municipality)) + ggplot2::geom_line(size = 1.5) + 
        ggplot2::scale_x_continuous(breaks = seq(min(indice$period), 
            max(indice$period), by = 2)) + ggplot2::scale_color_discrete("Municipality") + 
        ggplot2::theme_bw() + ggplot2::ylab(paste0(Cost)) + ggplot2::xlab("Year") + 
        ggplot2::ggtitle(paste0("Comparison of ", cost_title, 
            var_title, " in ", paste0(stringr::str_to_title(muni_vec), 
                collapse = " & "))) + ggplot2::theme(axis.title.y = ggplot2::element_text(angle = 0, 
        vjust = 0.5), plot.title = ggplot2::element_text(hjust = 0.5)))
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
