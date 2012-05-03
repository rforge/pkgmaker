# Development utility functions
# 
# Author: Renaud Gaujoux
# Creation: 25 Apr 2012
###############################################################################

#' @include namespace.R
NULL

#' Executing R Commands
#' @export
R.exec <- function(...){	
	system(paste(file.path(R.home(), 'bin', 'R'),' ', ..., sep=''))
}

#' Executes R CMD commands
#' @export
R.CMD <- function(cmd, ...){
	R.exec('CMD ', cmd, ' ', ...)
}

#' Executes R CMD SHLIB commands
#' @export
R.SHLIB <- function(libname, ...){
	R.CMD('SHLIB', '-o ', libname, .Platform$dynlib.ext, ...)
}

#' Compile Source Files from a Development Package
#' 
#' @param pkg the name of the package to compile
#' @param load a logical indicating whether the compiled library should be loaded
#' after the compilation (default) or not.
#' 
#' @return None
#' @export
compile_src <- function(pkg, load=TRUE){
	
	if( !missing(pkg) ){
		library(devtools)
		p <- as.package(pkg)
		path <- p$path
	}else{
		pkg <- packageName()
		path <- packagePath()
	}
	
	owd <- getwd()
	on.exit(setwd(owd))
	
	# Compile code in /src
	srcdir <- file.path(path, 'src')
	if( file.exists(srcdir) ){
		cat("# DEVMODE: Compiling src/ ... ")		
		setwd(srcdir)
		Sys.setenv(R_PACKAGE_DIR=path)
		R.SHLIB(pkg, " *.cpp ")
		cat("OK\n")
		if( load )
			load_c(pkg)
	}
}

#' Package Development Utilities
#' 
#' \code{packageEnv} is a slight modification from \code{\link{topenv}}, which 
#' returns the caller's top environment, also in the case of development packages
#' which are wrapped in an environment by \code{\link[devtools]{load_all}}.
#' 
#' @inheritParams base::topenv
#' 
#' @rdname devutils
#' @return an environment
#' @export
packageEnv <- function (envir = parent.frame(), matchThisEnv = getOption("topLevelEnvironment")) 
{
	pkgmakerEnv <- topenv()
	n <- 1
	while (!identical(envir, emptyenv())) {
		nm <- attributes(envir)[["names", exact = TRUE]]
		nm2 <- environmentName(envir)
		if ((is.character(nm) && length(grep("^package:", nm))) ||
				length(grep("^package:", nm2)) ||
				identical(envir, matchThisEnv) || identical(envir, 
						.GlobalEnv) || identical(envir, baseenv()) || .Internal(isNamespaceEnv(envir)) || 
				exists(".packageName", envir = envir, inherits = FALSE)){
		
			# go through pkgmaker frames
			if( identical(envir, pkgmakerEnv) ){
				n <- n + 1
				envir <- parent.frame(n)
			}else return(envir)
		
		}else envir <- parent.env(envir)
	}
	return(.GlobalEnv)
}

#' \code{packageName} returns the current package's name.
#' 
#' @rdname devutils
#' @return a character string
packageName <- function(){
	
	# retrieve package environment
	e <- packageEnv()
	
	# try with name from environment
	nm <- environmentName(e)
	if( isNamespace(e) ) return(nm)
	else if( grepl("^package:", nm) ) # should work for devtools packages
		return(sub("^package:", "", nm))
	
	# try to find the name from the package's environment (namespace) 
	if( exists('.packageName', e) && .packageName != 'datasets' ){
		if( .packageName != '' )
			return(.packageName)
	}
	# get the info from the loadingNamespace
	info <- getLoadingNamespace(info=TRUE)
	if( !is.null(info) ) # check whether we are loading the namespace 
		info$pkgname
	else{# error
		stop("Could not reliably determine package name [", nm, "]")
	}
}

#' \code{packagePath} returns the current package's root directory, which is 
#' its installation/loading directory in the case of an installed package, or
#' its source directory served by devtools. 
#' 
#' @param ... arguments passed to \code{\link{file.path}}.
#' 
#' @rdname devutils
#' @return a character string
packagePath <- function(..., PACKAGE=NULL){
	
	# try to find the path from the package's environment (namespace)
	path <- 
		if( !is.null(PACKAGE) )	system.file(package=PACKAGE)
		else if( exists('.packageName', packageEnv()) && .packageName != 'datasets' ){
			# get the path from installation
			system.file(package=.packageName)		
		}

	# somehow this fails when loading an installed package but is works 
	# when loading a package during the post-install check
	if( is.null(path) || path == '' ){
		# get the info from the loadingNamespace
		info <- getLoadingNamespace(info=TRUE)
		path <- 
			if( !is.null(info) ) # check whether we are loading the namespace 
				file.path(info$libname, info$pkgname)
			else{# we are in dev mode: use devtools
				library(devtools)
				p <- as.package(.LOCAL_PKG_NAME)
				
				# handle special sub-directories of the package's root directory
				if( nargs() == 0 || sub("^/?([^/]+).*", "\\1", list(...)[1]) %in% c('tests', 'data','R','src') )
					p$path
				else file.path(p$path,'inst')
				
			}
	}	
	stopifnot( !is.null(path) && path != '' )
	
	# add other part of the path
	file.path(path, ...)	
}

#' Tests if a package is installed
#' @rdname devutils
#' @export
isPackageInstalled <- function(..., lib.loc=NULL){
	
	inst <- utils::installed.packages(lib.loc=lib.loc)
	pattern <- '^([a-zA-Z.]+)(_([0-9.]+)?)?$';
	res <- sapply(list(...), function(p){
				vers <- gsub(pattern, '\\3', p)
				print(vers)
				pkg <- gsub(pattern, '\\1', p)
				print(pkg)
				if( !(pkg %in% rownames(inst)) ) return(FALSE);
				p.desc <- inst[pkg,]
				if( (vers != '') && compareVersion(vers, p.desc['Version']) > 0 ) return(FALSE);
				TRUE
			})
	all(res)
}

#stripLatex <- function(x){
#	gsub("\\\\.\\{(.)\\}", "\\1", x)
#}



NotImplemented <- function(msg){
	stop("Not implemented - ", msg)
}

