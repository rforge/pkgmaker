# Unit tests utilities
# 
# Author: Renaud Gaujoux
# Creation: 25 Apr 2012
###############################################################################


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
	runit <- "RUnit" ; require( runit, character.only = TRUE )
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

#' Running Unit Tests
#' 
#' Run unit tests in a variety of settings.
#'
#' @export
setGeneric('utest', function(x, ...) standardGeneric('utest'))

utestPattern <- function(framework){
	list(RUnit = list(filter="^runit.+\\.[rR]$", fun="^test\\.")
		, testthat = list(filter="^test.*\\.[rR]$"))[[framework]]
}

utestFramework <- function(path){
	
	framework <- NULL
	tf <- if( is.dir(path) ) list.files(path, "\\.[rR]$") else path
	for( f in c('RUnit', 'testthat') ){
		if( any(grepl(utestPattern(f)$filter, tf)) ){
			framework <- f
			break
		}
	}
	
	if( is.null(framework) )
		stop("Could not determine unit test framework used in directory: '", path, "'")
	framework
}

setMethod('utest', 'character', 
	function(x, filter="^runit.+\\.[rR]$", fun="^test\\.", ...
			, testdir='tests', framework=c('RUnit', 'testthat')){
		
		# detect type of input string
		path <- 
		if( grepl("^package:", x) ){# installed package
			pkg <- sub("^package:", "", x)
			library(pkg, character.only=TRUE)
			system.file(testdir, PACKAGE=pkg)
		}else{
			# try to find a corersponding development package
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
		up <- utestPattern(framework)
		if( missing(filter) ) filter <- up$filter
		if( missing(fun) ) fun <- up$fun
		
		# run tests
		if( is.dir(path) ){ # all tests in a directory
			if( framework == 'RUnit' ){ # RUnit
				
				requirePackage('RUnit', "Running RUnit unit test suites")
				s <- defineTestSuite(x, path
							, testFileRegexp=filter
							, testFuncRegexp=fun, ...)
				utest(s)
				
			}else if( framework == 'testthat' ){ # testthat
				
				requirePackage('testthat', "Running testthat unit test suites")
				test_dir(path, filter=filter, ...)
				
			}
		}else{ # single test file
			if( framework == 'RUnit' ){ # RUnit
			
				requirePackage('RUnit', "Running RUnit unit test file")
				runTestFile(path, testFuncRegexp=fun, ...)
			
			}else if( framework == 'testthat' ){ # testthat
			
				requirePackage('testthat', "Running testthat unit test file")
				test_file(path, ...)
				
			}
		}
		
	}
)

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

setOldClass('RUnitTestSuite')
setMethod('utest', 'RUnitTestSuite',
	function(x, ...){
		requirePackage('RUnit', "Running RUnit test suites")
		runTestSuite(x, ...)
	}
)