# Package specific option system
# 
# Author: Renaud Gaujoux
# Creation: 25 Apr 2012
###############################################################################


#' Managing Package Specific Option
#' 
#' 
#' the following functions to access/set the options from the set are assigned 
#' in \code{envir}:
#' \describe{
#' \item{<subset>Options}{}
#' \item{<subset>GetOption}{}
#' }
#' 
#' @param name Set name
#' @param parent Parent name, typically the name of the package one wants to 
#' define the options for. This is only used as a prefix for the name of the 
#' associated global option: \code{package:<parent>#<name>}.
#' @param envir Environment where to define the option access functions. 
#' @param reset a logical indicating whether the option set should be reset if 
#' it already exists. The default is \code{FALSE} (i.e. no reset), because one 
#' generally wants to keep options possibly saved in a reloaded workspace.
#'
#' @export
setupPackageOptions <- function(name, parent=NULL, default=list(NULL)
								, envir=parent.frame(), reset=FALSE){
	
	if( is.null(parent) ) parent <- packageName()
	pkg <- parent
	subset <- name
	
	# define option set
	.optset <- .pkg <- pkg
	if( subset != .pkg )
		.optset <- paste(.pkg, '#', subset, sep='')
	
	.options <- function(...) packageOptions(.optset, ...)
	
	fun <- 
			list(
					options = .options
					, getOption=function (x, default = NULL) 
					{
						if (missing(default)) 
							return(.options(x)[[1L]])
						if (x %in% names(.options())) 
							.options(x)[[1L]]
						else default
					}
					, resetOptions = function(name){
						opts <- .options()
						options(setNames(default, attr(opts, 'optionset')))
					}
			)
	
	opts <- .options()
	if( is.null(opts) )
		fun$resetOptions()
	else if( reset ){ # reset the option set if it already exists
		options(setNames(list(NULL), attr(opts, 'optionset')))
		fun$resetOptions()
	}
	print(.options())
	mapply(function(nam, f){assign(nam, f, envir=envir)}
			, paste(subset, names(fun), sep=''), fun)
	
	invisible(TRUE)
}


packageOptions <- function(optset=packageName(), ...){
	
	# get the option set from the standard global options
	goptname <- paste('package:', optset, sep='')
	opts <- getOption(goptname)
	if( nargs() == 1L ){
		# add attribute to track the option set
		if( !is.null(opts) ) attr(opts, 'optionset') <- goptname
		return(opts)
	}
	if( is.null(opts) ) opts <- list()
	
	dots <- list(...)
	params <- dots
	if( is.null(names(dots)) ){
		if( length(dots) > 1L )
			stop("Invalid multiple arguments: all argument must be named")
		if ( is.list(dots[[1L]]) ) 
			params <- dots[[1L]]
	}
	
	# READ ACCESS
	if ( is.null(names(params)) ){
		if( !is.character(c(...)) )
			stop('character strings expected for option names')
		
		cparams <- c(...)
		# retrieve options as a list (use sapply to get non set options also named)
		res <- sapply(cparams, function(n) opts[[n]], simplify=FALSE)		
		return(res)
	}
	
	# WRITE ACCESS
	old <- sapply(names(params), 
			function(name){
				# assign the new value into the options environment
				val <- params[[name]]
				old <- opts[[name]]
				opts[[name]] <<- val
				# return the option's old value
				old
			}
			, simplify = FALSE
	)	
	old <- old[!sapply(old, is.null)]
	# update the list in the global options
	options(setNames(list(opts), goptname))
	
	# return old values of the modified options
	return(invisible(old))	
}

#' Returns the names of all option sets currently defined.
#' @return a character vector (possibly empty).
#'
#' @export 
#' @examples
#' packageOptionSets()
#' 
packageOptionSets <- function(){
	grep('^package:', names(options()), value=TRUE)
} 
