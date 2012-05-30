\name{addToLogger}
\alias{addToLogger}
\title{Enhancing RUnit Logger}
\usage{
  addToLogger(name, value, logger = NULL)
}
\arguments{
  \item{logger}{global logger object (i.e. object
  \code{.testLogger} in the global environment
  \code{.GlobalEnv})}

  \item{value}{object to append to the logger. If
  \code{value} is a function it is added to the list and is
  accessible via \code{.testLogger$name}. If \code{value}
  is a variable it is added to the local environment and is
  therefore accessible in all logging functions.}

  \item{logger}{an optional logger object}
}
\value{
  the modified logger object. Note that the global object
  is also modified if \code{logger} is \code{NULL}.
}
\description{
  Adds a function or a local variable to RUnit global
  logger.
}

