\name{returns}
\alias{returns}
\title{Calculate the contribution of various effects based on the brinson model}

\description{

  Calculate and display the results of brinson analysis. Allocation
  effect, selection effect, interaction, and active return are
  displayed.
  
}

\usage{ returns(object) }

\arguments{

  \item{object}{ An object of either class \code{brinson} or class
    \code{brinsonMulti}.}
  
}

\value{

  Return a matrix of 4 rows including \code{Allocation Effect},
  \code{Selection Effect}, \code{Interaction Effect}, and \code{Active
  Return}.

}

\author{Yang Lu \email{Yang.Lu@williams.edu}}

\examples{

data(s1)

## Single-period brinson analysis results

p1 <- brinson(x = s1, date.var = "date", sector.var = "sector",
                    bench.weight = "benchmark", portfolio.weight =
                    "portfolio", ret.var = "fwd.ret.1m")

returns(p1)

## Multi-period brinson analysis results

data(s2)

p2 <- brinson(x = s2, date.var = "date", sector.var = "sector",
                    bench.weight = "benchmark", portfolio.weight =
                    "portfolio", ret.var = "fwd.ret.1m")

returns(p2)

}
\keyword{file}