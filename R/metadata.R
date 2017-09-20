#' Get metadata associated with one or more workflow ids
#'
#' This endpoint returns a superset of the data from
#' #get-workflowsversionidlogs in essentially the same format
#' (i.e. shards are accounted for by an array of maps, in the same
#' order as the shards). In addition to shards, every attempt that was
#' made for this call will have its own object as well, in the same
#' order as the attempts. Workflow metadata includes submission,
#' start, and end datetimes, as well as status, inputs and
#' outputs. Call-level metadata includes inputs, outputs, start and
#' end datetime, backend-specific job id, return code, stdout and
#' stderr. Date formats are ISO with milliseconds.
#' 
#' @param ids A character() vector of cromwell IDs, typically returned
#'     from a batch submission or from a call to \code{\link{cromwellQuery}}.
#' @param ... passed directly to httr `GET` (for including `timeouts`,
#'     `handles`, etc.)
#'
#' @references \url{https://github.com/broadinstitute/cromwell#get-apiworkflowsversionidmetadata}
#'
#' @return a list of metadata lists
#'
#' @importFrom httr GET
#' @importFrom stats setNames
#'
#' @examples
#' \dontrun{
#' res = cromwellQuery(terms=c(status='Succeeded',name='taskName'))
#' head(res)
#' metalist = cromwellMetadata(res$id)
#' str(metalist,list.len=5)
#' }
#'
.cromwellMetadata = function(ids,query=NULL,...) {
    retlist = lapply(ids,function(id) {
        path=sprintf('api/workflows/v1/%s/metadata',id)
        resp = cromwell_GET(path = path, query=query, ...)
        ret = resp$content
        attr(ret,'path') = path
        attr(ret,'when') = Sys.time()
        class(ret) = c('cromwell_output','cromwell_api',class(ret))
        ret
    })
    retlist = setNames(retlist,ids)
    attr(retlist,'when') = Sys.time()
    class(retlist) = c('cromwell_output_list','cromwell_api',class(retlist))
    return(retlist)
}

