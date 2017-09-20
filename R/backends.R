#' @include class-server.R
NULL

# @param ... passed directly to httr `GET` (for including `timeouts`, `handles`, etc.)
#
# @return a list that includes backend details
#
# 
#' @importFrom httr GET
.cromwellBackends = function(baseUrl, ...) {
    path = 'api/workflows/v1/backends'
    resp = .cromwell_GET(baseUrl, path = path, ...)
    ret = resp$content
    attr(ret,'path') = path
    attr(ret,'when') = Sys.time()
    class(ret) = c('cromwell_backends','cromwell_api',class(ret))
    return(ret)
}


#' List available backends for a cromwell endpoint
#'
#' This endpoint returns a list of the backends supported by the
#' server as well as the default backend.
#'
#' @param ... passed directly to httr `GET` (for including `timeouts`, `handles`, etc.)
#'
#' @return a list that includes backend details
#'
#' @references \url{https://github.com/broadinstitute/cromwell#get-apiworkflowsversionbackends}
#'
#' @seealso Other server info endpoints including \code{\link{stats}} and \code{\link{version}}
#' 
#' @examples
#' \dontrun{
#' jw = new('Cromwell')
#' backends(jw)
#' }
setGeneric('backends', function(object, ...) {
    standardGeneric('backends')
})

#' @describeIn backends Describe available backends on a Cromwell server
setMethod('backends', signature('Cromwell'),
          function(object, ...) {
              .cromwellBackends(baseURL(object), ...)
          })
