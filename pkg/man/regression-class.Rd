\name{regression-class}
\Rdversion{1.1}
\docType{class}
\alias{regression-class}
\alias{exposure,regression-method}
\alias{plot,regression-method}
\alias{returns,regression-method}
\alias{show,regression-method}
\alias{summary,regression-method}

\title{Class "regression"}
\description{
  Class "regression" holds the results of an original portfolio, its
  benchmark, and the results of regression analysis of a single-period
  portfolio. 
}
\section{Slots}{
  \describe{
    \item{\code{date.var}:}{Object of class \code{"character"} ~~ }
    \item{\code{ret.var}:}{Object of class \code{"character"} ~~ }
    \item{\code{reg.var}:}{Object of class \code{"character"} ~~ }
    \item{\code{benchmark.weight}:}{Object of class \code{"character"} ~~ }
    \item{\code{portfolio.weight}:}{Object of class \code{"character"} ~~ }
    \item{\code{coefficients}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{benchmark.ret}:}{Object of class \code{"matrix"} ~~ }
    \item{\code{portfolio.ret}:}{Object of class \code{"matrix"} ~~ }
    \item{\code{act.ret}:}{Object of class \code{"matrix"} ~~ }
    \item{\code{act.expo}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{contrib}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{universe}:}{Object of class \code{"data.frame"} ~~ }
  }
}
\section{Methods}{
  \describe{
    \item{exposure}{\code{signature(object = "regression")}: ... }
    \item{plot}{\code{signature(object = "regression")}: ... }
    \item{returns}{\code{signature(object = "regression")}: ... }
    \item{show}{\code{signature(object = "regression")}: ... }
    \item{summary}{\code{signature(object = "regression")}: ... }
	 }
}
\author{  Yang Lu  \email{yang.lu@williams.edu} }
\examples{

## Single-period regression analysis
data(jan)
r1 <-regress(x = jan, date.var = "date", ret.var = "return", reg.var = c("sector",
"value", "growth"), benchmark.weight = "benchmark", portfolio.weight =
"portfolio")
summary(r1)


}
\keyword{classes}
