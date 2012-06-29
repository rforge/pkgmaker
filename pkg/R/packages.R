# Package related functions
# 
# Author: Renaud Gaujoux
# Creation: 29 Jun 2012
###############################################################################


#' Setting Mirrors and Repositories
#' 
#' \code{setBiocMirror} sets all Bioconductor repositories (software, data, 
#' annotation, etc.).
#' so that they are directly available to \code{\link{install.packages}}.
#' It differs from \code{\link{chooseBioCmirror}} in that it effectively enables 
#' the repositories.
#' 
#' @param url or Bioconductor mirror url
#' @param version version number
#' @param unique logical that indicate if duplicated urls or names should be 
#' removed.
#'
#' @rdname mirrors
#' @export 
setBiocMirror <- function(url='http://www.bioconductor.org', version=NULL, unique=TRUE){
	
    #get all bioconductor repos      
    biocRepos <- getBiocRepos(url, version)
	
	repos <- c(biocRepos, getOption('repos'))
	if( unique ){
		nam <- names(repos)
		repos <- repos[!duplicated(repos) & (!duplicated(nam) | nam=='')]
	}
    options(repos=repos)
}

#' \code{getBiocMirror} is a shortcut for \code{getOption('BioC_mirror')}, which 
#' returns the current Bioconductor mirror as used by \code{biocLite}.
#'  
#' @export
#' @rdname mirrors
getBiocMirror <- function(){
	getOption('BioC_mirror')
}
#' \code{getBiocRepos} returns urls to all Bioconductor repositories on a 
#' given mirror.
#' 
#' @export
#' @rdname mirrors
getBiocRepos <- function(url='http://www.bioconductor.org', version=NULL){
	
	if( is.null(url) ){
		url <- getBiocMirror()
		if( is.null(url) )
			stop("No Bioconductor mirror was setup. Use `setBiocMirror`.")
	}
	
	## BioConductor CRAN-style repositories.
	## The software repo (bioc) _must_ be the first element.
	biocParts <- c(
			bioc='bioc'
			, biocData='data/annotation'
			, biocExp='data/experiment'
			, biocExtra='extra'
    )
	
	# define version suffix for bioconductor repo
	if( is.null(version) ){
		assoc <- list(`2`=c(7L, 2L))
		Rv <- as.integer(sub("([0-9]+).*", "\\1", R.version$minor))
		offset <- assoc[[R.version$major]]
	    version <- paste(R.version$major, offset[2L] + Rv - offset[1L], sep='.')
	}
	
	#add version suffix for bioconductor repo
    setNames(paste(url, 'packages', version, biocParts, sep='/'), names(biocParts))
}

#' \code{setCRANMirror} sets the preferred CRAN mirror.
#' 
#' @rdname mirrors
#' @export
setCRANMirror <- function(url='http://cran.r-project.org', unique=TRUE){
	
	repos <- c(CRAN=url, getOption('repos'))
	if( unique ){
		nam <- names(repos)
		repos <- repos[!duplicated(repos) & (!duplicated(nam) | nam=='')]
	}
    options(repos=repos)
}
