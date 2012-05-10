# General test utility functions to check the type of objects
# 
# Author: Renaud Gaujoux
# Creation: 30 Apr 2012
###############################################################################

#' Testing Object Type 
#' 
#' @name is_something
#' @rdname is_something
NULL

#' Tests if a variable is exactly NA (logical, character, numeric or integer)
#' 
#' @param an R object
#' @rdname is_something
#' @export
isNA <- function(x) 
	identical(x, NA) || identical(x, as.character(NA)) || identical(x, as.numeric(NA)) || identical(x, as.integer(NA))  

#' Tests if a variable is exactly FALSE
#' 
#' @rdname is_something
#' @seealso \code{\link{isTRUE}}
#' @export
isFALSE <- function(x) identical(x, FALSE)

#' Tests if a variable is a single number
#' @rdname is_something
#' @export
isNumber <- function(x, int.ok=TRUE){ 
	is.numeric(x) && length(x) == 1 && (int.ok || !is.integer(x))
}

#' Tests if an object is a single integer
#' @rdname is_something
#' @export
isInteger <- function(x){ 
	is.integer(x) && length(x) == 1
}


#' Test if an object is a single character string
#' @rdname is_something
#' @export
isString <- function(x) is.character(x) && length(x) == 1L

#' Tests if a filename is a directory
is.dir <- function(x) file_test('-d', x)
