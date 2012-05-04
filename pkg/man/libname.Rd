\name{libname}
\alias{libname}
\title{Extract libname from a path, removing the directory part of the path, as
well as the platform specific library extension.}
\usage{
  libname(x)
}
\arguments{
  \item{x}{a filename}
}
\description{
  Extract libname from a path, removing the directory part
  of the path, as well as the platform specific library
  extension.
}
\examples{
libname('mylib.so')
libname('/some/path/somewhere/mylib.dll')
}

