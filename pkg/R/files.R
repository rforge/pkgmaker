# Filesystem related functions
# 
# Author: Renaud Gaujoux
###############################################################################

#' Library Files Utilities
#' 
#' Lists binary library files in a directory
#' 
#' @param dir directory
#' @param all.platform a logical that indicates whether to list library files for 
#' the current platform only (default) or all platforms (Unix, Windows, Mac).
#' @param ... extra arguments passed to \code{\link{list.files}}.
#' 
#' @return a character vector
#' @export
#' @rdname libutils
list.libs <- function(dir, ..., all.platforms=FALSE){
	
	p <- if( !all.platforms ){
		str_c("\\", .Platform$dynlib.ext, "$")
	}else{
		p <- str_c("(\\.", c('so', 'dll'), , ')', collapse='|')
		str_c(p, '$')
	}
	list.files(dir, pattern=p, ...)
}

#' Extract libname from a path, removing the directory part of the path, as 
#' well as the platform specific library extension.
#' 
#' @param x a filename
#' 
#' @examples
#' 
#' libname('mylib.so')
#' libname('/some/path/somewhere/mylib.dll') 
#' 
libname <- function(x){
	sub(str_c("\\", .Platform$dynlib.ext, "$"), "", basename(libs))
}


#' Source Multiple Files
#' 
#' Vectorised version of \code{source}.
#' 
#' @param x character vector containing filenames
#' @param ... extra arguments passed to \code{\link{source}}.
#' 
#' @export
source_files <- function(x, pattern=NULL, ...){
	if( length(x) == 1L && is.dir(x) )
		x <- list.files(x, pattern=pattern, full.names=TRUE)
	invisible(sapply(x, source, ...))
}
