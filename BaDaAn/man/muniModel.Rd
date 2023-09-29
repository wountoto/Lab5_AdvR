\name{muniModel}
\alias{muniModel}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Create a linear regression
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
muniModel(y, form, df)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{
%%     ~~Describe \code{y} here~~
}
  \item{form}{
%%     ~~Describe \code{form} here~~
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
function (y, form, df) 
{
    if (!(y \%in\% df$kpi || form \%in\% df$kpi)) {
        print("Only the defined for the given municipalities. Also check spelling!")
        stop()
    }
    df1 <- tidyr::pivot_wider(df, id_cols = c(municipality, period), 
        names_from = kpi, values_from = values)
    formula <- formula(paste0(y, "~", paste0(form, collapse = " + ")))
    mod <- lm(formula, data = df1)
    base::print(ggplot2::ggplot(data.frame(res = mod$residuals), 
        ggplot2::aes(x = res)) + ggplot2::geom_density(fill = 8, 
        alpha = 0.7) + ggplot2::theme_bw() + ggplot2::ylab("Density") + 
        ggplot2::xlab(paste0("Residuals \n", deparse(formula))) + 
        ggplot2::ggtitle("Density plot for the residuals") + 
        ggplot2::theme(axis.title.y = ggplot2::element_text(angle = 0, 
            vjust = 0.5), plot.title = ggplot2::element_text(hjust = 0.5)))
    s <- summary(mod)
    return(s)
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
