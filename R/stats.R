#' @include class-server.R

# @param ... passed directly to httr `GET` (for including `timeouts`, `handles`, etc.)
#' @importFrom httr GET
.cromwellStats = function(baseUrl,...) {
    path = 'api/engine/v1/stats'
    resp = .cromwell_GET(baseUrl, path = path, ...)
    ret = resp$content
    attr(ret,'path') = path
    attr(ret,'when') = Sys.time()
    class(ret) = c('cromwell_stats','cromwell_api',class(ret))
    return(ret)
}

#' Get current statistics for cromwell endpoint
#'
#' This endpoint returns some basic statistics on the current state of
#' the engine. At the moment that includes the number of running
#' workflows and the number of active jobs.
#'
#' @param ... passed directly to httr `GET` (for including `timeouts`, `handles`, etc.)
#'
#' @return a list containing engine stats
#'
#' @importFrom httr GET
#'
#' @seealso Other server info endpoints including \code{\link{backends}} and \code{\link{version}}
#' 
#' @references \url{https://github.com/broadinstitute/cromwell#get-apiengineversionstats}
#'
#' @examples
#' \dontrun{
#' stats()
#' }
setGeneric('stats', function(object, ...) {
    standardGeneric('stats')
})


#' @rdname stats
setMethod('stats', signature('Cromwell'),
          function(object, ...) {
              .cromwellStats(baseURL(object), ...)
          })

