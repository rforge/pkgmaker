\name{makeUnitVignette}
\alias{makeUnitVignette}
\title{Make Vignette for Unit Tests}
\usage{
  makeUnitVignette(pkg,
    file = paste(pkg, "-unitTests.pdf", sep = ""), ...)
}
\arguments{
  \item{pkg}{Package name}

  \item{file}{Output file (.Rnw, .tex, or .pdf)}

  \item{...}{extra arguments passed to
  \code{\link{utest}}.}
}
\value{
  Result of running unit test suite
}
\description{
  Builds a vignette for unit tests in a package using the
  \code{\link{utest}} and a template vignette file.
}

