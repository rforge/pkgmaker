\name{list.libs}
\alias{list.libs}
\title{Library Files Utilities}
\usage{
  list.libs(dir, ..., all.platforms = FALSE)
}
\arguments{
  \item{dir}{directory}

  \item{all.platform}{a logical that indicates whether to
  list library files for the current platform only
  (default) or all platforms (Unix, Windows, Mac).}

  \item{...}{extra arguments passed to
  \code{\link{list.files}}.}
}
\value{
  a character vector
}
\description{
  Lists binary library files in a directory
}

