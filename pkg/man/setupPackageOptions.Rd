\name{setupPackageOptions}
\alias{setupPackageOptions}
\title{Package Specific Options}
\usage{
  setupPackageOptions(name = NULL, parent = NULL,
    envir = parent.frame(), defaults = NULL, reset = FALSE,
    clear = FALSE)
}
\arguments{
  \item{name}{Set name}

  \item{parent}{Parent name, typically the name of the
  package one wants to define the options for. This is only
  used as a prefix for the name of the associated global
  option: \code{package:<parent>#<name>}.}

  \item{envir}{Environment where to define the option
  access functions.}

  \item{reset}{a logical indicating whether the option set
  should be reset if it already exists. The default is
  \code{FALSE} (i.e. no reset), because one generally wants
  to keep options possibly saved in a reloaded workspace.}
}
\description{
  The following functions to access/set the options from
  the set are assigned in \code{envir}: \describe{
  \item{<subset>Options}{} \item{<subset>GetOption}{} }
}

