\name{packageRegistry}
\alias{packageRegistry}
\title{Package Registry}
\usage{
  packageRegistry(name, error = TRUE)
}
\arguments{
  \item{name}{Name of a sub-registry}

  \item{error}{a logical that indicate whether an error is
  thrown if the sub-registry is not found (default) or just
  \code{NULL}.}
}
\value{
  a \code{\link[=regobj]{registry}} object or \code{NULL}
  (see argument \code{error}).
}
\description{
  Package Registry
}

