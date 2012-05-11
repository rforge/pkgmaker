\name{is_something}
\alias{is.dir}
\alias{isFALSE}
\alias{isInteger}
\alias{isNA}
\alias{isNumber}
\alias{is_something}
\alias{isString}
\title{Testing Object Type}
\usage{
  isNA(x)

  isFALSE(x)

  isNumber(x, int.ok = TRUE)

  isInteger(x)

  isString(x)

  is.dir(x)
}
\arguments{
  \item{x}{an R object}
}
\value{
  \code{TRUE} or \code{FALSE}
}
\description{
  Testing Object Type

  Tests if a variable is exactly NA (logical, character,
  numeric or integer)

  Tests if a variable is exactly FALSE

  Tests if a variable is a single number

  Tests if an object is a single integer

  Test if an object is a single character string

  Tests if a filename is a directory
}
\seealso{
  \code{\link{isTRUE}}
}

