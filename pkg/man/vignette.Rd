\name{rnw}
\alias{as.rnw}
\alias{rnw}
\alias{rnwChildren}
\alias{rnwCompiler}
\alias{rnwDriver}
\alias{rnwIncludes}
\title{Utilities for Vignettes}
\usage{
  rnw(x, file = NULL, ...)

  as.rnw(x, ...)

  rnwCompiler(x)

  rnwDriver(x)

  rnwIncludes(x)

  rnwChildren(x)
}
\arguments{
  \item{x}{vignette source file specification as a path or
  a \code{rnw} object.}

  \item{file}{output file}

  \item{...}{extra arguments passed to \code{as.rnw} that
  can be used to force certain building parameters.}
}
\description{
  \code{rnw} provides a unified interface to run vignettes
  that detects the type of vignette (Sweave or
  \code{\link[knitr]{knitr}}), and which Sweave driver to
  use (either automatically or from an embedded command
  \code{\\VignetteDriver} command).

  \code{as.rnw} creates a S3 \code{rnw} object that
  contains information about a vignette, e.g., source
  filename, driver, fixed included files, etc..

  \code{rnwDriver} tries to detect the vignette compiler to
  use on a vignette source file, e.g., \code{\link{Sweave}}
  or \code{\link[knitr]{knitr}}.

  \code{rnwDriver} tries to detect Sweave driver to use on
  a vignette source file, e.g., \code{SweaveCache},
  \code{highlight}, etc..

  \code{rnwIncludes} detects fixed includes, e.g., image or
  pdf files, that are required to build the final document.

  \code{rnwChildren} detects included vignette documents
  and return them as a list of vignette objects.
}

