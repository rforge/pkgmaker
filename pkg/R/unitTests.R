# Unit tests utilities
# 
# Author: Renaud Gaujoux
# Creation: 25 Apr 2012
###############################################################################

#' @include utils.R
#' @include logging.R 
NULL

#' Load RUnit Compatible Package
#' 
#' Loads the package responsible for the implementation of the RUnit framework,
#' choosing amongst \sQuote{RUnitX}, \sQuote{svUnit} and \sQuote{RUnit}.
#' 
#' @param ... arguments passed to \code{\link{requirePackage}}.
#' 
#' @return nothing
#' @keywords internal
#' 
requireRUnit <- function(...){
	runit <- 'RUnit'
	if( length(find.package('RUnitX')) ) runit <- 'RUnitX'
	else if( length(find.package('svUnit')) ) runit <- 'svUnit'
	if( !is.null(path.package(runit, quiet=TRUE)) )
		message("Using RUnit framework provider: ", runit)
	requirePackage(runit, ...)
}

#' Make Vignette for Unit Tests
#' 
#' @param pkg Package name
#' @param file Output Sweave (.Rnw) document
#' @return Result of running unit test suite
#'  
#' @export
#' 
makeUnitVignette <- function(pkg, file=paste(pkg, '-unitTests.Rnw', sep='')){
	
	# generate the vignette for unit test on exit
	if( !is.null(file) )
		on.exit( writeUnitVignette(pkg, file) )
	# load this package
	require( pkg, character.only = TRUE )
	
	## load RUnit
	requireRUnit("Make unit test vignette")
	if( file.exists( "unitTests-results" ) ){
		file.remove("unitTests-results") 
	}
	dir.create( "unitTests-results" )
	
	path <- system.file("unitTests", package = pkg)
	testSuite <- defineTestSuite(name=paste(pkg, "unit testing")
			, dirs = path
			, rngKind = "default",
			, rngNormalKind = "default")
	tests <- runTestSuite(testSuite)
	printHTMLProtocol(tests, fileName= sprintf( "unitTests-results/%s-unitTests.html" , pkg ) )
	printTextProtocol(tests, fileName= sprintf( "unitTests-results/%s-unitTests.txt"  , pkg ) )
	
	# check for errors
	err <- getErrors(tests)
	errMsg <- NULL
	if( err$nFail > 0) {
		errMsg <- c(errMsg, sprintf( "unit test problems: %d failures\n", err$nFail))
	}
	if( err$nErr > 0) {
		errMsg <- c(errMsg, sprintf( "unit test problems: %d errors\n", err$nErr))
	}
	# stop if any failure or error occured
	if( length(errMsg) > 0L )
		stop(errMsg)
	
	# copy result in tmp directory if possible
	if( file.exists( "/tmp" ) ){
		file.copy( sprintf( "unitTests-results/%s-unitTests.txt" , pkg ) , "/tmp", overwrite = TRUE )
		file.copy( sprintf( "unitTests-results/%s-unitTests.html", pkg ) , "/tmp", overwrite = TRUE )
	}
	
	# return result of unit test suite
	err
}

#' Writes Unit Tests Vignette 
#' 
#' Writes a vignette that contains the results from running unit test suites.
#' 
#' @param pkg Package name
#' @param file Output Sweave (.Rnw) file
#' 
#' @export
#' 
writeUnitVignette <- function(pkg, file){
	
	Rnw.template <- 
			"
			\\documentclass[10pt]{article}
			%\\VignetteIndexEntry{@pkg@-unitTests}
			\\usepackage{vmargin}
			\\setmargrb{0.75in}{0.75in}{0.75in}{0.75in}
			
			\\RequirePackage{ae,mathpple}    % ae as a default font pkg works with Sweave
			\\RequirePackage[T1]{fontenc}
			
			<<echo=FALSE,print=FALSE>>=
			pkg <- '@pkg@'
			require( pkg, character.only=TRUE )
			prettyVersion <- packageDescription(pkg)$Version
			prettyDate <- format(Sys.Date(), '%B %e, %Y')
			authors <- packageDescription(pkg)$Author
			@
			
			\\usepackage[colorlinks]{hyperref}
			\\author{\\Sexpr{authors}}
			\\title{\\texttt{\\Sexpr{pkg}}: Unit testing results}
			\\date{\\texttt{\\Sexpr{pkg}} version \\Sexpr{prettyVersion} as of \\Sexpr{prettyDate}}
			\\begin{document}
			\\maketitle
			
			\\begin{verbatim}
			@results@
			\\end{verbatim}
			
			\\section*{Session Information}
			@sessionInfo@
			
			\\end{document}
			"
	# load the results of the unit tests
	results <- file.path('unitTests-results', paste(pkg, '-unitTests.txt', sep=''))
	results <- 
	if( file.exists( results ) ){
		paste(readLines(results), collapse="\n")
	} else{
		'unit test results not available'
	}
	
	# substitute template variables
	contents <- Rnw.template
	# package name
	contents <-	gsub("@pkg@", pkg, contents)
	# unit test results
	contents <-	gsub("@results@", results, contents)
	# session info (as when calling this function)
	contents <-	gsub("@sessionInfo@", gsub("\\", "\\\\", paste(toLatex(sessionInfo()), collapse="\n"), fixed=TRUE), contents)
	
	# write into the file
	writeLines(contents, file)
}

# Unit test frameworks data
.UFdata <- list(
	RUnit = list(
		file_pattern="^runit.*\\.[rR]$"
		, fun_pattern="^test\\."
		, check_pattern = "^check.+"
		, check_functions = c(
				'checkTrue'
				, 'checkIdentical'
				, 'checkEquals'
				, 'checkEqualsNumeric'
				, 'checkException'
		)
	)
	, testthat = list(
		file_pattern="^test.*\\.[rR]$"
		, check_pattern = "^(expect_.+)|(test_that$)" 
		, check_functions = c(
				"test_that"
				, "expect_equal"
				, "expect_equivalent"
				, "expect_error"
				, "expect_false"
				, "expect_identical"
				, "expect_is"
				, "expect_match"
				, "expect_message"
				, "expect_output"
				, "expect_that"
				, "expect_true"
				, "expect_warning"   
		)
	)
)

#' Inferring Unit Test Framework
#' 
#' @param x an filename, a function or the body of a function
#' @param eval a logical that indicates if the value of \code{x} should be used.
#' 
#' @return the name of the framework as a character string or NULL if
#' it could not be detected.
#' 
#' @importFrom codetools makeCodeWalker
#' @export
utestFramework <- function(x, eval=FALSE){
	
	# check if one should detect within an expression
	expr <- if( missing(eval) || !eval ) substitute(x) 
			else if( is.function(x) ) body(x)
	
	# walk code using codetools looking up for known test functions
	if( !is.null(expr) ){
		cw <- codetools::makeCodeWalker(leaf= function(e, w) if( is.symbol(e) ) cat(e, "\n"))
		s <- str_trim(capture.output(walkCode(expr, cw)))
		if( length(s) > 1L ){
			for( f in names(.UFdata) ){
				if( any(s %in% .UFdata[[f]]$check_functions) ){
					return(f)
				}
			}
		}
		# not found without evaluating
		if( !missing(eval) && !eval ) return()
		if( missing(eval) ){ # try evaluating
			return(utestFramework(x, eval=TRUE))
		}
	}
	
	if( !is.character(x) )
		stop("Invalid argument `x`: expecting a character string")
	path <- x
	framework <- NULL
	tf <- if( is.dir(path) ) list.files(path, "\\.[rR]$") else path
	for( f in names(.UFdata) ){
		if( any(grepl(.UFdata[[f]]$file_pattern, tf)) ){
			return(f)
		}
	}
	
	if( is.null(framework) )
		stop("Could not determine unit test framework used in directory: '", path, "'")
	framework
}

#' Embedded Unit Tests
#' 
#' The function \code{unit.test} provides a way to write unit tests embedded within
#' package source files.
#' These tests are stored and organised in the package namespace, and can be run using 
#' the unified interface provided by the function \code{link{utest}}.
#' Both Runit and testthat tests are supported -- and automatically detected.
#' 
#' 
#' @param x single character string used as test identifier/label
#' @param expr expression containing the actual test commands.
#' It is not evaluated, but only stored in the package namespace.
#' @param framework Unit test framework
#' @param envir the definition environment of object \code{x}.
#' 
#' @return a test function with no arguments that wrapping around \code{expr} 
#' 
#' @export
#' 
unit.test <- function(x, expr, framework=NULL, envir=parent.frame()){
	
	sid <- as.character(deparse(substitute(x)))	
	hash <- suppressWarnings(digest(x))
	# get test environment
	eTest <- packageTestEnv()
	# wrap test into a function
	f <- function(){}
	environment(f) <- eTest
	body(f) <- substitute({expr})
	
	if( !grepl('"', sid) )
	{
		lmessage('Creating unit test for object: `', sid, '`')
		eval(substitute(attr(x, 'test') <- f, list(x=substitute(x), f=f)), envir)
	}else
		lmessage('Creating unit test: ', sid)
	
	# add the test to the package test environment
	eTest[[str_c(sid, ':', hash)]] <- list(test=f, name=sid, object=is.name(x))
	# return the test function
	f
}

#' Returns the package internal environment where unit tests are stored.
#' 
#' @param pkg package name.
#' If missing the caller's package is assumed. 
#' 
#' @export
packageTestEnv <- function(pkg){
	
	if( !missing(pkg) && !is.null(pkg) ){
		e <- packageEnv(pkg)
		return( e$.packageTest )
	}
	
	e <- packageEnv()
	# create test environment if necessary
	if( is.null(e$.packageTest) )
		e$.packageTest <- new.env(e)
	e$.packageTest
}


list.tests <- function(x, pattern=NULL){
	
}

#unit.test(packageEnv, {print('test for packageEnv')})
#unit.test('lmlm', {print('test for something else')})

#utest <- function(x, ..., framework="RUnit", PACKAGE=NULL){
#		
#	if( missing(x) )
#		x <- packagePath('unitTests', PACKAGE=PACKAGE)
#	else if( class(x)[1] != 'character')
#		return( UseMethod('utest', x) )
#	
#	if( is.null(framework) ){
#		stop("Not implemented")
#	}else{
#		# change directory to run tests
#		owd <- setwd(x)
#		on.exit(setwd(owd))
#		# run tests under selected framework
#		class(x) <- framework
#		utest(x, ..., PACKAGE=PACKAGE)
#		# output test result
#	}
#}

#' Running Unit Tests
#' 
#' Run unit tests in a variety of settings.
#' This is still \strong{very} experimental.
#' 
#' @param x object to which a unit test is attached
#' @param ... extra arguments to allow extensions and are passed to 
#' the unit framework running funcitons. 
#'
#' @inline
#' @export
setGeneric('utest', function(x, ...) standardGeneric('utest'))
#' Run the unit test assoicated to a function. 
#' 
#' @param run a lgoical that indicates if the unit test should be run
setMethod('utest', 'function',
	function(x, run = TRUE){
		# get actual name of the function
		sid <- as.character(deparse(substitute(x, parent.frame())))
		# remove leading namespace specifications
		sid <- sub("^[^:]+:::?", "", sid)
		# get the package's  
		pkg <- attr(x, 'package')
		eTest <- packageTestEnv(pkg)
		if( is.null(eTest) ) return()
		tfun <- ls(eTest, pattern=str_c("^", sid, ":"))		
	}
)
#' Run a package test suite
#' 
#' @param filter pattern to match files that contain the definition of 
#' the unit tests functions to run.
#' @param fun patter to match the test functions to run.
#' @param testdir directory where to look for the test files
#' @param framework unit test framework
#' @param quiet a logical that indicates if the tests should be run silently
#'  
setMethod('utest', 'character', 
		function(x, filter="^runit.+\\.[rR]$", fun="^test\\.", ...
				, testdir='tests', framework=c('RUnit', 'testthat')
				, quiet = Sys.getenv("RCMDCHECK") != "FALSE"){
			
			cat("#########################\n")
			#print(system('env'))
			# detect type of input string
			path <- 
					if( grepl("^package:", x) ){# installed package
						pkg <- sub("^package:", "", x)
						if( is.null(path <- path.package(pkg, quiet=TRUE)) ){
							library(pkg, character.only=TRUE)
							path <- path.package(pkg)
						}
						file.path(path, testdir)
					}else{
						# try to find a corresponding development package
						if( require.quiet(devtools) 
								&& is.package(pkg <- as.package2(x, error=FALSE)) ){
							load_all(pkg, TRUE)
							file.path(pkg$path, 'inst', testdir)
						}else{ # assume x is a path  
							x
						}
					}
			
			# check that the path exists
			if( !file.exists(path) )
				stop("Unit test directory '", path, "' does not exist")
			
			# detect unit test framework: RUnit or testthat?
			framework <- 
					if( missing(framework) ) utestFramework(path)
					else match.arg(framework)
			message("Using unit test framework: ", framework)
			
			# load default patterns
			up <- .UFdata[[framework]]
			if( missing(filter) ) filter <- up$file_pattern
			if( missing(fun) ) fun <- up$fun_pattern
			
			# run tests
			if( is.dir(path) ){ # all tests in a directory
				if( framework == 'RUnit' ){ # RUnit
					
					requireRUnit("Running RUnit test suites")
					s <- defineTestSuite(x, path
							, testFileRegexp=filter
							, testFuncRegexp=fun, ...)
					utest(s, quiet=quiet)
					
				}else if( framework == 'testthat' ){ # testthat
					
					requirePackage('testthat', "Running testthat unit test suites")
					test_dir(path, filter=filter, ...)
					
				}
			}else{ # single test file
				if( framework == 'RUnit' ){ # RUnit
					
					requireRUnit("Running RUnit unit test file")
					runTestFile(path, testFuncRegexp=fun, ...)
					
				}else if( framework == 'testthat' ){ # testthat
					
					requirePackage('testthat', "Running testthat unit test file")
					test_file(path, ...)
					
				}
			}
			
		}
)

setOldClass('RUnitTestSuite')
#' Runs a RUnit test suite
setMethod('utest', 'RUnitTestSuite',
	function(x, ..., quiet=FALSE){
		requireRUnit("Running RUnit test suites")
		if( quiet ){
			suppressWarnings(suppressMessages(out <- capture.output(
				tests <- runTestSuite(x, ...)
			)))
		}else 
			tests <- runTestSuite(x, ...)
		
		pathReport <- str_c("utest.", sub("[:]", "_", x$name))
		## Report to stdout and text files
		cat("------------------- UNIT TEST SUMMARY ---------------------\n\n")
		printTextProtocol(tests, showDetails=FALSE)
		printTextProtocol(tests, showDetails=FALSE,
				fileName=paste(pathReport, "Summary.txt", sep=""))
		printTextProtocol(tests, showDetails=TRUE,
				fileName=paste(pathReport, ".txt", sep=""))
		
		## Report to HTML file
		printHTMLProtocol(tests, fileName=paste(pathReport, ".html", sep=""))
		
		## Return stop() to cause R CMD check stop in case of
		##  - failures i.e. FALSE to unit tests or
		##  - errors i.e. R errors
		tmp <- getErrors(tests)
		if(tmp$nFail > 0 | tmp$nErr > 0) {
			stop(paste("\n\nunit testing failed (#test failures: ", tmp$nFail,
							", #R errors: ",  tmp$nErr, ")\n\n", sep=""))
		}
	}
)