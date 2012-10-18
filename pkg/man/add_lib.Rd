\name{add_lib}
\alias{add_lib}
\title{Adding Package Libraries}
\usage{
  add_lib(..., append = FALSE)
}
\arguments{
  \item{...}{paths to add to .libPath}

  \item{append}{logical that indicates that the paths
  should be appended rather than prepended.}
}
\description{
  Prepend/append paths to the library path list, using
  \code{\link{.libPaths}}.
}

