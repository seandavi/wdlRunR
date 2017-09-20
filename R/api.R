#' Set and get base URL and port for cromwell server
#'
#' The Cromwell server presents a RESTFul API. The base URL is of the form:
#' `http://EXAMPLE.COM:PORT`. The current approach to changing that
#' url is to set an option, `cromwellBase` to a valid URL (without trailing slash).
#' This URL will then be used throughout the `cRomwell` package.  If no option is set,
#' the server is assumed to be running at `http://localhost:8000`.
#'
#' @param base_url character(1) specifying the url and port of the
#'     cromwell server
#' @return A character(1) base url (default \code{http://localhost:8000})
#' 
#' @examples
#' cromwellBase()
#'
#' # set a bogus host
#' setCromwellBase('http://example.com:8111')
#' cromwellBase()
#'
#' # and set back to NULL to get the default behavior
#' setCromwellBase()
#' cromwellBase()
#'
#' @export
cromwellBase <- function() {
    base_url = getOption('cromwellBase', default="http://localhost:8000")
    return(base_url)
}

#' @rdname cromwellBase
#' @export
setCromwellBase <- function(base_url=NULL) {
    if(!is.null(base_url)) {
        stopifnot(is.character(base_url),length(base_url)==1)
        options('cromwellBase' = base_url)
        invisible(base_url)
    }
    base_url = "http://localhost:8000"
    options('cromwellBase' = base_url)
    invisible(base_url)
}

#' Perform a GET request to cromwell server
#'
#' See the docmentation at
#' \href{https://github.com/broadinstitute/cromwell#rest-api}{the
#' cromwell github site} for details. Generally, this is not meant to
#' be called by the end user. Rather, use the endpoint-specific
#' functions. See \code{\link{cromwellBase}} for details of setting
#' the base URL and port.
#'
#' @param path The path part of the URL
#' @param query Any query terms as a named character vector
#' @param ... passed directly to httr `GET` (for including `timeouts`,
#'     `handles`, etc.)
#'
#' @keywords internal
#' 
#' @importFrom httr modify_url
#' @importFrom httr GET
#'
.cromwell_GET <- function(baseUrl,path,query=NULL,...) {
    url <- modify_url(baseUrl, path = path, query = query)
    resp <- GET(url,...)
    return(.cromwell_process_response(resp))
}

#' Perform a POST request to cromwell server
#'
#' See the docmentation at
#' \href{https://github.com/broadinstitute/cromwell#rest-api}{the
#' cromwell github site} for details. Generally, this is not meant to
#' be called by the end user. Rather, use the endpoint-specific
#' functions.
#'
#' @param path The path part of the URL
#' @param body A list that will become the multipart form that is passed as the request body
#' @param ... passed directly to httr `POST` (for including `timeouts`, `handles`, etc.)
#'
#' @keywords internal
#' 
#' @importFrom httr modify_url
#' @importFrom httr POST
#'
#' @seealso \code{\link{cromwellBatch}}
#'
.cromwell_POST = function(baseUrl, path,body,...) {
    url = modify_url(baseUrl, path = path)
    resp = POST(url, body = body, ...)
    return(.cromwell_process_response(resp))
}

#' Check cromwell response
#'
#' @param resp a \code{\link{response}} object
#'
#' @return a simple list that includes the actual `content` and the complete `response` object.
#'
#' @import httr
#'
.cromwell_process_response = function(resp) {
    if (http_type(resp) != "application/json") {
        stop("API did not return json", call. = FALSE)
    }

    parsed <- httr::content(resp,'parsed')

    if (!(status_code(resp) %in% c(200,201))) {
        stop(
            sprintf(
                "Cromwell API request failed [%s]\n%s\n<%s>",
                status_code(resp),
                parsed$message,
                parsed$documentation_url
            ),
            call. = FALSE
        )
    }

    structure(
        list(
            content = parsed,
            response = resp
        ),
        class = c("cromwell_api")
    )
}

#' Get the info about cromwell workflows
#'
#' Each of the following terms can be specified one or more
#' times. Simply create a named list or named character vector.
#' 
#' \describe{
#'   \item{name}{The name of a job; may be specified more than once}
#'   \item{status}{one of Succeeded, Failed, Running}
#'   \item{id}{an id of a cromwell job}
#'   \item{start}{a timestamp of the form "2015-11-01T07:45:52.000-05:00", including mandatory offset}
#'   \item{end}{a timestamp of the form "2015-11-01T07:45:52.000-05:00", including mandatory offset}
#'   \item{page}{if paging is used, what page to select}
#'   \item{pagesize}{if paging is used, how many records per page}
#' }
#'
#' @param name character vector of workflow names.
#' @param id character vector of workflow IDs.
#' @param label character vector of workflow labels.
#' @param status character vector of workflow status values. The available
#'     statuses are: Submitted, Running, Aborting, Aborted, Failed, and Succeeded.
#' @param start a \code{Date} object specifying the workflow start time. Only workflows
#'     started after this are returned.
#' @param end a \code{Date} object specifying the workflow end time. Only workflows
#'     that completed after this are returned.
#' 
#' @param ... passed directly to httr `GET` (for including `timeouts`,
#'     `handles`, etc.)
#'
#' @return a data.frame of query results
#'
#' @importFrom jsonlite fromJSON
#'
#' @examples
#' \dontrun{
#' res = cromwellQuery(status='Succeeded')
#' head(res)
#' }
#'
#' @export
.cromwellQuery = function(query=NULL, ...) {
    cnames = c('name', 'id', 'label', 'status') # character vectors
    dnames = c('start', 'end') # date objects required    
    .statuses = c('Submitted','Running','Aborting','Aborted','Failed','Succeeded')
    
    path = 'api/workflows/v1/query'
    resp = cromwell_GET(path=path,query=query,...)
    x = lapply(cnames,function(cname) {
      as.character(sapply(resp$content$results, '[[', cname))
    })
    x = setNames(x,cnames)
    x = data.frame(x, stringsAsFactors = FALSE)
    #x = do.call(rbind.fill,lapply(resp$content$results,as.data.frame))
    if('start' %in% colnames(x))
        x$start = strptime(substr(as.character(x$start),1,19),format="%Y-%m-%dT%H:%M:%S",tz="UTC")
    else
        x$start = NA
    if('end' %in% colnames(x))
        x$end = strptime(substr(as.character(x$end),1,19),format="%Y-%m-%dT%H:%M:%S",tz="UTC")
    else
        x$end = NA
    x$end = as.POSIXct(x$end,tz=Sys.timezone())
    x$start = as.POSIXct(x$start,tz=Sys.timezone())
    # deal with situation when no ends exist
    # subtraction ends up "failing", so need
    # to catch error
    x$duration = tryCatch({x$end-x$start},
                          error=function(e) "")
    # Coerce to difftime so that column is always
    # difftime, even when only NA
    x$duration = as.difftime(x$duration)
    attr(x,'when') = Sys.time()
    attr(x,'path') = path
    class(x) = c('cromwell_query','cromwell_api','data.frame')
    return(x)
}


#' Abort a cromwell job
#'
#' @param id A cromwell id as a string
#' @param ... passed directly to httr `POST` (for including
#'     `timeouts`, `handles`, etc.)
#'
#' @importFrom httr POST
#'
#' @examples
#' #cromwellAbort('ID')
.cromwellAbort = function(id, ...) {
    return(cromwell_POST(path=sprintf('api/workflows/v1/%s/abort',id),body=NULL,...))
}

#' Get output paths associated with one or more workflow ids
#'
#'
#' @param ids a character vector of Cromwell ids. See
#'     \code{\link{cromwellQuery}} for details of how to query
#'     Cromwell for available ids.
#' @param ... passed directly to httr `POST` (for including
#'     `timeouts`, `handles`, etc.)
#' @return a list of output lists.
#'
#' @references \url{https://github.com/broadinstitute/cromwell#get-apiworkflowsversionidoutputs}
#'
#' @importFrom httr GET
#' @importFrom stats setNames
#' @importFrom dplyr bind_rows
#' @import magrittr
#'
#' @examples
#' \dontrun{
#' res = cromwellQuery(terms=c(status='Succeeded',name='taskName'))
#' head(res)
#' outfilelist = cromwellOutputs(res$id)
#' str(outfilelist,list.len=5)
#' }
#'
.cromwellOutputs = function(ids, ...) {
    retlist = lapply(ids,function(id) {
        path = sprintf('api/workflows/v1/%s/outputs', id)
        resp = cromwell_GET(path = path, ...)
        ret = resp$content$outputs
        attr(ret,'path') = path
        attr(ret,'when') = Sys.time()
        class(ret) = c('cromwell_output','cromwell_api',class(ret))
        ret
    })
    retlist = setNames(retlist,ids)
    attr(retlist,'when') = Sys.time()
    class(retlist) = c('cromwell_output_list','cromwell_api',class(retlist))
    retval = sapply(names(x), function(y) {if(length(x[[y]])==0) return(NULL) else return(data_frame(id=y,output=names(x[[y]]),uri=unlist(x[[y]])))}) %>%
        Filter(function(tmp) !is.null(tmp),.) %>%
        bind_rows()
    return(retval)
}



#' Get log paths associated with one or more workflow ids
#'
#' This will return paths to the standard out and standard error files
#' that were generated during the execution of all calls in a
#' workflow.  A call has one or more standard out and standard error
#' logs, depending on if the call was scattered or not. In the latter
#' case, one log is provided for each instance of the call that has
#' been run.
#'
#'
#' @param ids a character vector of Cromwell ids. See
#'     \code{\link{cromwellQuery}} for details of how to query
#'     Cromwell for available ids.
#' @param ... passed directly to httr `POST` (for including
#'     `timeouts`, `handles`, etc.)
#' @return a list of logfile lists. There will be one list item for
#'     each id. Each of these list items will contain another list
#'     with log outputs from each workflow step in the submitted
#'     workflow.
#'
#' @importFrom stats setNames
#' @importFrom httr GET
#'
#' @references \url{https://github.com/broadinstitute/cromwell#get-apiworkflowsversionidlogs}
#'
#' @examples
#' \dontrun{
#' res = cromwellQuery(terms=c(status='Succeeded',name='taskName'))
#' head(res)
#' loglist = cromwellLogs(res$id)
#' str(loglist,list.len=5)
#' }
#'
.cromwellLogs = function(ids, ...) {
    "Get log paths associated with one or more workflow ids

parameters:

ids: a character vector of ids for which to fetch the logs

This will return paths to the standard out and standard error files
 that were generated during the execution of all calls in a
 workflow.  A call has one or more standard out and standard error
 logs, depending on if the call was scattered or not. In the latter
 case, one log is provided for each instance of the call that has
 been run.
"

    retlist = lapply(ids,function(id) {
        path = sprintf('api/workflows/v1/%s/logs', id)
        resp = cromwell_GET(path = path, ...)
        ret = resp$content$calls
        if(is.null(ret)) return(NULL)
        attr(ret,'path') = path
        attr(ret,'when') = Sys.time()
        class(ret) = c('cromwell_log','cromwell_api',class(ret))
        ret
    })
    retlist = setNames(retlist,ids)
    retlist = Filter(function(tmp) !is.null(tmp),retlist)
    retval = bind_rows(
        sapply(names(retlist), function(id) {
            logval = retlist[[id]]
            datrows = do.call(rbind,lapply(logval,function(y) y[[1]]))
            return(data.frame(id = id,datrows))}))
    return(retval)
    attr(retlist,'when') = Sys.time()
    class(retlist) = c('cromwell_log_list','cromwell_api',class(retlist))
    return(retlist)
}


#' Submit a cromwell batch job
#'
#' This function submits a set of one or more inputs to cromwell. It
#' is much more efficient than submitting a single job at a time.  See
#' \href{https://github.com/broadinstitute/cromwell#post-apiworkflowsversionbatch}{the
#' cromwell \code{batch} API documentation} for details.
#'
#' @param wdlSource Represents the
#'     \href{https://software.broadinstitute.org/wdl/}{WDL} A string
#'     (character vector of length 1) or an
#'     \code{\link[httr]{upload_file}} object. See details below.
#' @param workflowInputs A \code{data.frame} that will be coerced to a
#'     json array or a JSON string (as a \code{character} vector of
#'     length 1), or an \code{\link[httr]{upload_file}} object. See
#'     details below.
#' @param workflowOptions A \code{list}, a JSON string (as a
#'     \code{character} vector of length 1, or an
#'     \code{\link[httr]{upload_file}} object. See details below.
#' @param customLabels A named \code{character} vector with key-value
#'     pairs to assign to all batch tasks.
#' @param timeout The number of seconds to wait for a response. Batch
#'     jobs can take quite some time for cromwell to process, so this
#'     will typically need to be set to a large value to allow for a
#'     completed response.
#' @param ... passed directly to httr `POST` (for including
#'     `timeouts`, `handles`, etc.)
#'
#' @return If a timeout does not occur (this is pretty common....),
#'     then a list that contains the submission status.
#'
#' @details TODO details
#'
#' @importFrom jsonlite toJSON
#'
.cromwellBatch = function(baseUrl,
                          wdlSource,
                         workflowInputs,
                         customLabels = NULL,
                         workflowOptions=NULL,
                         timeout = 120,
                         ...) {
    if(!(is.data.frame(workflowInputs) | (is.character(workflowInputs) & length(workflowInputs)==1)))
        stop('workflowInputs should be a data.frame or a character vector of length 1')
    if(is.data.frame(workflowInputs))
        inputs = toJSON(workflowInputs)
    else
        inputs = workflowInputs
    opts = workflowOptions
    if(!is.null(workflowOptions)) {
    if(!(is.list(workflowOptions) | (is.character(workflowOptions) & length(workflowOptions)==1)))
        stop('workflowOptions should be a data.frame or a character vector of length 1')
    if(is.list(workflowOptions))
        opts = toJSON(workflowOptions)
    }
    body = list(wdlSource       = wdlSource,
                workflowInputs  = inputs,
                customLabels = toJSON(customLabels,auto_unbox=TRUE),
                workflowOptions = opts)

    return(cromwell_POST(baseUrl,'/api/workflows/v1/batch',body = body, encode = 'multipart',
                timeout(timeout), ...))
}

#' Submit a single cromwell job
#'
#' This function submits a set of one or more inputs to cromwell. It
#' is much more efficient than submitting a single job at a time.  See
#' \href{https://github.com/broadinstitute/cromwell#post-apiworkflowsversionbatch}{the
#' cromwell \code{batch} API documentation} for details.
#'
#' @param wdlSource A \code{list}, a JSON string (as a \code{character} vector of length 1,
#'   or an \code{\link[httr]{upload_file}} object. See details below.
#' @param workflowInputs A \code{list}, a JSON string (as a \code{character} vector of length 1,
#'   or an \code{\link[httr]{upload_file}} object. See details below.
#' @param workflowOptions A \code{list}, a JSON string (as a \code{character} vector of length 1,
#'   or an \code{\link[httr]{upload_file}} object. See details below.
#' @param timeout The number of seconds to wait for a response. Batch jobs can take
#'   quite some time for cromwell to process, so this will typically need to be set
#'   to a large value to allow for a completed response.
#' @param ... passed directly to httr `POST` (for including `timeouts`, `handles`, etc.)
#'
#' @details abc details
#'
#' @importFrom httr POST
#'
.cromwellSingle = function(wdlSource,
                          workflowInputs,
                          customLabels = NULL,
                          workflowOptions=NULL,
                          timeout = 120,
                          ...) {
    body = list(wdlSource       = wdlSource,
                workflowInputs  = workflowInputs,
                workflowOptions = workflowOptions)

    return(cromwell_POST('/api/workflows/v1',body = body,
                timeout(timeout), ...))
}





#
# Classes
#
###################################

###################################
#
# Outputs
#
###################################


#' Get output paths associated with one or more workflow ids
#'
#'
#' @param ids a character vector of Cromwell ids. See
#'     \code{\link{cromwellQuery}} for details of how to query
#'     Cromwell for available ids.
#' @param ... passed directly to httr `POST` (for including
#'     `timeouts`, `handles`, etc.)
#' @return a list of output lists.
#'
#' @references \url{https://github.com/broadinstitute/cromwell#get-apiworkflowsversionidoutputs}
#'
#' @importFrom httr GET
#' @importFrom stats setNames
#' @importFrom dplyr bind_rows
#' @import magrittr
#'
#' @examples
#' \dontrun{
#' res = cromwellQuery(terms=c(status='Succeeded',name='taskName'))
#' head(res)
#' outfilelist = cromwellOutputs(res$id)
#' str(outfilelist,list.len=5)
#' }
#'
setGeneric('outputs', function(object, ids, ...) {
    standardGeneric('outputs')
})

#' @describeIn Cromwell get outputs for one or more workflow ids
setMethod('outputs', signature('Cromwell', 'missing'),
          function(object, ids, ...) {
              stop("NOT IMPLEMENTED")
          })

#' @describeIn Cromwell get outputs for one or more workflow ids
setMethod('outputs', signature('Cromwell', 'character'),
          function(object, ids, ...) {
              .cromwellOutputs(ids, ...)
          })


###################################
#
# Jobs 
#
###################################

setOldClass('form_file')

setClass('KeyValuePairs',
         contains = "list")

setValidity('KeyValuePairs',
            function(object) {
                if(!inherits(object,"list")) {
                    stop("KeyValuePairs needs to be a list")
                }
                if(!all(sapply(object,length)==1)) {
                    stop("KeyValuePairs list values must all be of length 1")
                }
                if(is.null(names(object)) ||
                   !all(nchar(names(object))>0)) {
                    stop("KeyValuePairs elements must all have names")
                }
            }
            )

#' importFrom httr form_file
setClassUnion('characterOrFormFile', c('character', 'form_file'))
setClassUnion('characterOrNULL', c('character', 'form_file'))
setClassUnion('characterOrFormFileOrNULL', c('characterOrFormFile','NULL'))

setClass('BatchTemplate',
         representation(workflowSource = "characterOrFormFile",
                        customLabels   = "characterOrNULL",
                        workflowOptions = "list"))

###################################
#
# Logs 
#
###################################

