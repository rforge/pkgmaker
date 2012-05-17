\name{as.package_options}
\alias{as.package_options}
\alias{listPackageOptions}
\alias{packageOptions}
\title{\code{as.package_options} creates an object such as the
ones used to stores package specific options.}
\usage{
  as.package_options(x, defaults = NULL)

  packageOptions(..., .DATA = packageName())

  listPackageOptions()
}
\arguments{
  \item{x}{a character string, a list or an object of class
  \code{package_options}.}

  \item{defaults}{\code{NULL} or a list of default options
  with their values.}
}
\value{
  a character vector (possibly empty).
}
\description{
  \code{as.package_options} creates an object such as the
  ones used to stores package specific options.

  \code{packageOptions} returns a list of package specific
  options, and behaves as the base function
  \code{\link[base]{options}}.

  \code{listPackageOptions} returns the names of all option
  sets currently defined.
}
\examples{
listPackageOptions()
}

