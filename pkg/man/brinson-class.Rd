\name{brinson-class}
\docType{class}
\alias{brinson-class}

\alias{show,brinson-method}
\alias{summary,brinson-method}
\alias{exposure,brinson-method}
\alias{returns,brinson-method}
\alias{plot,brinson-method}

\title{Class "brinson"}
\description{ Class "brinson" holds the results of an original
  portfolio, its benchmark, and the results of brinson analysis of a
  single-period portfolio. }

\section{Slots}{
  \describe{
    \item{\code{date.var}:}{ Object of class character storing the
      date variable name in the universe data frame.}
    \item{\code{sector.var}:}{ Object of class character storing the
      sector variable name in the universe data frame.}
    \item{\code{bench.weight}:}{ Object of class character storing the
      benchmark weight variable name in the universe data frame.}
    \item{\code{portfolio.weight}:}{ Object of class character storing the
      portfolio weight variable name in the universe data frame.}
    \item{\code{ret.var}:}{ Object of class character storing the return
      variable name in the universe data frame.}
    \item{\code{weight.port}:}{ Object of class array storing the
      sector weights of the original portfolio.}
    \item{\code{weight.bench}:}{ Object of class array storing the
      sector weights of the benchmark portfolio.}
    \item{\code{ret.port}:}{ Object of class array storing the
      sector returns of the original portfolio.}
    \item{\code{ret.bench}:}{ Object of class array storing the
      sector returns of the benchmark portfolio.}
    \item{\code{q4}:}{ Object of class numeric storing the information
      of the 4th quadrant in the brinson matrix. It refers to return of
      the benchmark portfolio.}
    \item{\code{q3}:}{ Object of class numeric storing the information
      of the 3rd quadrant in the brinson matrix. It refers to return of
      the portfolio with benchmark sector weights and portfolio sector
      returns.}
    \item{\code{q2}:}{ Object of class numeric storing the information
      of the 2nd quadrant in the brinson matrix. It refers to return of
      the portfolio with portfolio sector weights and benchmark sector
      returns.}
    \item{\code{q1}:}{ Object of class numeric storing the information
      of the 1st quadrant in the brinson matrix. It refers to return of
      the original portfolio.}
    \item{\code{universe}:}{ Data frame storing the universe
      environment. }
    
  }
}

\section{Methods}{
\describe{
  \item{show}{\code{signature(object = "brinson")}. Summarize
    the essential information about the portfolio.}
  \item{summary}{\code{signature(object = "brinson")}. Summarize
    the portfolio and the brinson analysis.}
  \item{exposure}{\code{signature(object = "brinson")}. Calculate and
    display the sector exposure of a portfolio. }
  \item{returns}{\code{signature(object = "brinson")}. Calculate the
    contribution of various effects based on the brinson model. }
  \item{plot}{\code{signature(object = "brinson")}. Plot the exposure or
    the return of a portfolio. }
}
}

\author{  Yang Lu }

\examples{

data(s1)

## Single-period brinson analysis

p1 <- brinson(x = s1, date.var = "date", sector.var = "sector",
                    bench.weight = "benchmark", portfolio.weight =
                    "portfolio", ret.var = "fwd.ret.1m")

summary(p1)

exposure(p1)

returns(p1)

plot(p1, type = "exposure")

plot(p1, type = "returns")

}


\keyword{classes}