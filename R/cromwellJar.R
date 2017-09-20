#' Utility to fetch the cromwell JAR file
#'
#' The purpose of this R package is to interact with the
#' Broad Cromwell execution engine.
#' The Cromwell server is contained in a JAVA JAR file. This
#' function simply downloads the cromwell JAR file
#' and puts it in the destfile location. The JAR file is picked up from
#' \url{https://github.com/broadinstitute/cromwell/releases}.
#'
#'
#'
#' @param cromwell_version string representing the version number
#' @param destfile string The full path to the cromwell jar file location on the local system
#'
#' @return destfile location [invisibly]
#'
#' @importFrom httr GET
#'
#' @seealso See lots of details at \url{https://github.com/broadinstitute/cromwell}.
#'
#' @examples
#' version = '29'
#' tmpfile = file.path(tempdir(),'cromwell.jar')
#' fp = getCromwellJar(cromwell_version = version)
#' fp
#' unlink(fp)
#'
#' @export
getCromwellJar = function(release='latest',destfile = file.path(tempdir(),'cromwell.jar')) {
    fname = destfile
    httr::GET(.releaseJarURL(release), write_disk(fname, overwrite = TRUE), progress())
    message(sprintf('Cromwell version %s downloaded to:\n  %s',release,destfile))
    invisible(fname)
}

.cromwellReleaseInfo = function() {
    releaseURL = httr::modify_url(url = 'https://api.github.com',path='/repos/broadinstitute/cromwell/releases')
    jsonlite::fromJSON(httr::content(httr::GET(releaseURL),'text'),simplifyDataFrame=TRUE)
}

cromwellReleases = function() {
    .cromwellReleaseInfo()$tag_name
}

.releaseJarURL = function(release='latest') {
    rel_info = .cromwellReleaseInfo()
    id = rel_info[1,'id']
    if(release!='latest') {
        relrow = which(release == rel_info$tag)
        if(relrow==integer(0)) stop('release not found. Available releases include:',paste(rel_info$tag_name))
        id = rel_info[relrow,'id']
    }
    assetURL = httr::modify_url(url = 'https://api.github.com',
                                path=sprintf('/repos/broadinstitute/cromwell/releases/%d/assets',id))
    tmp = jsonlite::fromJSON(httr::content(httr::GET(assetURL),'text'),simplifyDataFrame = TRUE)
    row = grep('jar$',tmp$name)
    return(tmp[row,'browser_download_url'])
}
