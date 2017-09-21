#' @include class-server.R
NULL

.cromwellVersion = function(baseUrl, ...) {
    path = 'engine/v1/version'
    resp = .cromwell_GET(baseUrl, path = path, ...)
    ret = resp$content$cromwell
    attr(ret,'path') = path
    attr(ret,'when') = Sys.time()
    class(ret) = c('cromwell_version','cromwell_api',class(ret))
    return(ret)
}


#' Get version of the cromwell server
#'
#' @param ... passed directly to httr `GET` (for including `timeouts`, `handles`, etc.)
#'
#' @return character(1) with the cromwell server version
#'
#' @importFrom httr GET
#'
#'
#' @references \url{https://github.com/broadinstitute/cromwell#get-version}
#'
#' @examples
#' \dontrun{
#' cw = new('Cromwell')
#' version(cw)
#' }
setGeneric('version', function(object, ...) {
    standardGeneric('version')
})

#' @rdname version
setMethod('version', signature('Cromwell'),
          function(object, ...) {
              .cromwellVersion(baseURL(object), ...)
          })

