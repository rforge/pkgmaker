\docType{methods}
\name{utest}
\alias{utest}
\alias{utest,character-method}
\alias{utest,function-method}
\alias{utest-methods}
\alias{utest,RUnitTestSuite-method}
\title{Running Unit Tests}
\usage{
  utest(x, ...)

  \S4method{utest}{function}(x, run = TRUE)

  \S4method{utest}{character}(x,
    filter = "^runit.+\\.[rR]$", fun = "^test\\.", ...,
    testdir = "tests", framework = c("RUnit", "testthat"),
    quiet = Sys.getenv("RCMDCHECK") != "FALSE")

  \S4method{utest}{RUnitTestSuite}(x, ..., quiet = FALSE)
}
\description{
  Run unit tests in a variety of settings.
}
\section{Methods}{
  \describe{

  \item{utest}{\code{signature(x = "function")}: Run the
  unit test assoicated to a function. }

  \item{utest}{\code{signature(x = "character")}: Run a
  package test suite }

  \item{utest}{\code{signature(x = "RUnitTestSuite")}: Runs
  a RUnit test suite }

  }
}
\keyword{methods}

