# Package specific option system
# 
# Author: Renaud Gaujoux
# Creation: 25 Apr 2012
###############################################################################


#' Package Specific Options
#' 
#' The following functions to access/set the options from the set are assigned 
#' in \code{envir}:
#' \describe{
#' \item{<subset>Options}{}
#' \item{<subset>GetOption}{}
#' }
#' 
#' @param ... a single named list or named arguments that provide the default 
#' options and their values.
#' @param NAME name of the set of options.
#' This is used as a prefix for the name of the associated global 
#' option: \code{package:<name>}.
#' @param ENVIR environment where the option wrapper functions will be defined.
#' No function is defined if \code{ENVIR=NULL} 
#' @param RESET a logical that indicates whether the option set should overwrite
#' one that already exists if necessary. 
#' The default is \code{FALSE} (i.e. no reset), because one generally wants to 
#' keep options possibly saved in a reloaded workspace.
#'
#' @export
setupPackageOptions <- function(..., NAME=NULL, ENVIR=parent.frame(), RESET=FALSE){
	
	defaults <- .list_or_named_dots(...)
	
	# get calling package
	pkg <- packageName(.Global=TRUE)
	
	# prefix for the wrapper functions
	fprefix <- if( is.null(NAME) ) tolower(pkg) else NAME
	
	# define name for the option set
	optname <- pkg
	if( !is.null(NAME) )
		optname <- paste(optname, NAME, sep=':')
	
	# create package_options object
	optobj <- as.package_options(optname, defaults=defaults)
	
	# check if options with the same key are not already registered
	oldobj <- getOption(optobj$name)
	if( is.null(oldobj) || RESET ){
		# register the package_options object in global options
		message(if( is.null(oldobj) ) "Setting" else "Resetting"
				, " package specific options: ", optobj$name
				, " (", length(optobj$options())," default option(s))")
		options(setNames(list(optobj), optobj$name))
		
	}	
	# load registered package_options object from global options
	optobj <- getOption(optobj$name)
	stopifnot( !is.null(optobj) )
	
	# define wrapper functions in the supplied environment
	if( !is.null(ENVIR) ){
		isfun <- unlist(eapply(optobj, is.function))
		isfun <- isfun[names(isfun) != 'newOption']
		ifun <- which(isfun)
		lapply(names(isfun)[ifun], function(x){ 
				assign(paste(fprefix, x, sep='.'), optobj[[x]], envir=ENVIR)
		})
	}
	
	# return package_options object
	optobj
}

is.package_options <- function(x){
	is(x, 'package_options')
}

#' @S3method print package_options
print.package_options <- function(x, ...){
	cat("<Package specific options: ", x$name, ">\n", sep='')
	cat("Registered: ", !is.null(getOption(x$name)), "\n", sep='')
	def <- if( identical(x$.options, x$.defaults) ) " <as default>"
	# show options
	if( length(x$.options) ){
		cat("Options",def,":\n", sep='');
		str(x$.options) 
	}else 
		cat("Options: none\n")
	# show defaults
	if( is.null(def) ){
		cat("Defaults:\n"); str(x$.defaults)
	}
}

#' @export
as.package_options <- function(x, defaults=NULL){
	
	# early exit if already a package_options object
	if( is.package_options(x) ){
		
		# new defaults?: clone into a new package_options object
		if( !missing(defaults) && is.list(defaults) ){
			optname <- tempfile(str_c(x$name, '_'))
			x <- as.package_options(x$.options, defaults)
			x$name <- optname
		}
	
		return(x)
	}
	
	# create a package_options object
	.OPTOBJ <- structure(list2env(list(name=NULL, .options=NULL, .defaults=defaults))
						, class='package_options')
	
	if( is.character(x) ){
		
		# get the option data from global options
		x <- sub("^package:", '', x)
		goptname <- paste('package:', x[1L], sep='')
		
		opts <- getOption(goptname)
		# directly return the registered object if it exists
		if( !is.null(opts) ) return( opts )
		
		.OPTOBJ$name <- goptname
		
	}else if( is.list(x) ){
		.OPTOBJ$name <- tempfile('package:')
		.OPTOBJ$.options <- x
	}else
		stop("Invalid argument `x`: must be a character string or a list.")
	
	# define options() 
	.OPTOBJ$options <- function(...) packageOptions(..., .DATA=.OPTOBJ)
	# define getOption
	.OPTOBJ$getOption <- function (x, default = NULL) 
	{
		# use local specific function options()
		options <- .OPTOBJ$options
		
		if (missing(default)) 
			return(options(x)[[1L]])
		if (x %in% names(options())) 
			options(x)[[1L]]
		else default
	}
	# define newOption
	.OPTOBJ$newOption <- function(name, value){
		if( name %in% names(.OPTOBJ$.defaults) )
			stop("Option '", name, "' is already defined for '", .OPTOBJ$name, "'")
		.OPTOBJ$.defaults[[name]] <- value
		.OPTOBJ$.options[[name]] <- value
	}
	# define resetOptions
	.OPTOBJ$resetOptions <- function(..., ALL=FALSE){
		
		defaults <- .OPTOBJ$.defaults
		if( ALL ){
			.OPTOBJ$.options <- NULL
		}else if( length(list(...)) > 0L ){
			onames <- c(...)
			if( !is.character(onames) )
				stop('character strings expected for resetting option names')
			o <- .OPTOBJ$options()
			defaults <- defaults[names(o) %in% onames]
		}
		if( !is.null(defaults) ){
			.OPTOBJ$options(defaults)
		}
	}
	# define showOptions
	.OPTOBJ$printOptions <- function() print(.OPTOBJ)
	
	# initialise with default options 
	.OPTOBJ$resetOptions()
	
	# return pacakge_options object
	.OPTOBJ
}

.list_or_named_dots <- function(...){
	
	dots <- list(...)
	if( length(dots) == 0L ) return()
	
	params <- dots
	if( is.null(names(dots)) && length(dots)==1L ){
		if ( is.list(dots[[1L]]) ){ 
			params <- dots[[1L]]
			if( is.null(names(params)) || any(names(params)=='') )
				stop("single list argument must only have named elements")
		}
	}
	params
}

# internal function that mimic the behaviour of the base function 
# \code{\link[base]{options}}.
.options <- function(..., .DATA){
	
	opts <- if( is.package_options(.DATA) ) .DATA$.options else .DATA
	
	params <- .list_or_named_dots(...)
	# return complete option list if no other argument was passed
	if( is.null(params) ) return(opts)
	
	# initialise opts to an empty list if necessary 
	if( is.null(opts) ) opts <- list()
	stopifnot( is.list(opts) )
	
	# READ ACCESS
	if ( is.null(names(params)) ){
		if( !is.character(c(...)) )
			stop('character strings expected for option names')
		
		cparams <- c(...)
		# retrieve options as a list (use sapply to also get non-existing options)
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
	#old <- old[!sapply(old, is.null)]

	# update package_options object if necessary
	if( is.package_options(.DATA) ) .DATA$.options <- opts
	
	# return old values of the modified options
	return( invisible(old) )
}

#' \code{packageOptions} returns a list of package specific options, and behaves
#' as the base function \code{\link[base]{options}}.
#'  
#' @export
#' @rdname options
packageOptions <- function(..., .DATA = packageName()){
		
	narg <- nargs() - !missing(.DATA)
	
	# create/retrieve a package_options object from .DATA
	optobj <- as.package_options(.DATA)
	
	# call .options on package_options object
	.options(..., .DATA = optobj)
}

#' Returns the names of all option sets currently defined.
#' @return a character vector (possibly empty).
#'
#' @export 
#' @examples
#' packageOptionSets()
#' 
listPackageOptions <- function(){
	grep('^package:', names(options()), value=TRUE)
} 
