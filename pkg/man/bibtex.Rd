\name{packageReferenceFile}
\alias{latex_bibliography}
\alias{packageReferenceFile}
\title{Bibtex Utilities}
\usage{
  packageReferenceFile(PACKAGE = NULL)

  latex_bibliography(PACKAGE, file = "")
}
\arguments{
  \item{PACKAGE}{package name}

  \item{file}{connection where to print}
}
\description{
  \code{packageReferenceFile} returns the path to a package
  REFERENCES.bib file.

  \code{latex_bibliography} prints or return a LaTeX
  command that includes a package bibliography file if it
  exists.
}

