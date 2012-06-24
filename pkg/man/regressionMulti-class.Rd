\name{regressionMulti-class}
\Rdversion{1.1}
\docType{class}
\alias{regressionMulti-class}
\alias{exposure,regressionMulti-method}
\alias{plot,regressionMulti-method}
\alias{returns,regressionMulti-method}
\alias{show,regressionMulti-method}
\alias{summary,regressionMulti-method}

\title{Class "regressionMulti"}
\description{
  Class "regressionMulti" holds the results of an original portfolio,
  its benchmark, and the results of regression analysis of a
  multi-period portfolio.  
}
\section{Slots}{
  \describe{
    \item{\code{date.var}:}{Object of class \code{"character"} ~~ }
    \item{\code{ret.var}:}{Object of class \code{"character"} ~~ }
    \item{\code{reg.var}:}{Object of class \code{"character"} ~~ }
    \item{\code{benchmark.weight}:}{Object of class \code{"character"} ~~ }
    \item{\code{portfolio.weight}:}{Object of class \code{"character"} ~~ }
    \item{\code{coefficients}:}{Object of class \code{"matrix"} ~~ }
    \item{\code{benchmark.ret}:}{Object of class \code{"matrix"} ~~ }
    \item{\code{portfolio.ret}:}{Object of class \code{"matrix"} ~~ }
    \item{\code{act.ret}:}{Object of class \code{"matrix"} ~~ }
    \item{\code{act.expo}:}{Object of class \code{"matrix"} ~~ }
    \item{\code{contrib}:}{Object of class \code{"matrix"} ~~ }
    \item{\code{universe}:}{Object of class \code{"list"} ~~ }
  }
}
\section{Methods}{
  \describe{
    \item{exposure}{\code{signature(object = "regressionMulti")}: ... }
    \item{plot}{\code{signature(x = "regressionMulti")}: ... }
    \item{returns}{\code{signature(object = "regressionMulti")}: ... }
    \item{show}{\code{signature(object = "regressionMulti")}: ... }
    \item{summary}{\code{signature(object = "regressionMulti")}: ... }
	 }
}

\author{  Yang Lu  \email{yang.lu@williams.edu}}

\examples{
## Multi-period regression analysis
data(quarter)
r2 <-regress(x = quarter, date.var = "date", ret.var = "return", reg.var = c("sector",
"value", "growth"), benchmark.weight = "benchmark", portfolio.weight =
"portfolio")
summary(r2)
}
\keyword{classes}
