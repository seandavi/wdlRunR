#' Utility to fetch the wdltool JAR file 
#'
#' This function simply downloads the wdltool JAR file and puts it in
#' the destfile location. The JAR file is picked up from
#' \url{https://github.com/broadinstitute/wdltool/releases}.
#'
#' A side-effect is that the environment variable, \code{WDLTOOL_JAR}
#' will be set to the path to the downloaded jarfile.
#'
#' @param wdltool_version string representing the version number
#' @param destfile string The full path to the wdltool jar file
#'     location on the local system
#'
#'
#' 
#' @return destfile location [invisibly]
#'
#' @importFrom httr GET
#'
#' @seealso See lots of details at \url{https://github.com/broadinstitute/wdltool}.
#' 
#' @examples
#' version = '0.8'
#' tmpfile = file.path(tempdir(),'wdltool.jar')
#' fp = getWdltoolJar(wdltool_version = version,
#'                    destfile = tmpfile)
#' fp
#' unlink(fp)
#'
#' @export
getWdltoolJar = function(wdltool_version,
                         destfile = 'wdltool.jar') {
    fname = destfile
    httr::GET(sprintf('https://github.com/broadinstitute/wdltool/releases/download/%s/wdltool-%s.jar',
                      wdltool_version,
                      wdltool_version),
              write_disk(fname,overwrite = TRUE))
    Sys.setenv('WDLTOOL_JAR'=fname)
    message('WDLTOOL_JAR environment variable set to\n',
            fname)
    invisible(fname)
}

#' return inputs from wdltool
#'
#' This little function just runs the command-line wdltool inputs on a
#' wdl file and returns a data.frame of input names and input types.
#'
#' @param wdltoolJar The filename of the wdltool jar file; if not
#'     specified, the environment variable \code{WDLTOOL_JAR} is
#'     consulted.
#' @param wdlfile The filename of a WDL file from which to get inputs.
#' @return a 2-column data.frame with the input \code{name} and the
#'     input \code{type} as the two columns.
#'
#' @importFrom jsonlite fromJSON
#'
#' @examples
#' version = '0.8'
#' tmpfile = file.path(tempdir(),'wdltool.jar')
#' fp = getWdltoolJar(wdltool_version = version,
#'                    destfile = tmpfile)
#' wdltoolInputs(system.file(package='wdlRunR','wdl/fastqc_task.wdl'))
#' 
#' @export
wdltoolInputs <- function(wdlfile,
                          wdltoolJar = Sys.getenv('WDLTOOL_JAR',"wdltool.jar")) {
    stopifnot(file.exists(wdltoolJar))
    # capture output 
    res = fromJSON(system2('java',
                           sprintf('-jar %s inputs %s',wdltoolJar,wdlfile),
                           stdout=TRUE))
    # convert to 2-column data frame
    res = data.frame(name=names(res),inputType=unname(res,force=TRUE),
                     stringsAsFactors = FALSE)
    colnames(res) = c('name','type')
    return(res)
}
    
