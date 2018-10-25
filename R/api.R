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
cromwell_GET <- function(path,query=NULL,...) {
    url <- modify_url(cromwellBase(), path = path, query = query)
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
cromwell_POST = function(path,body,...) {
    url = modify_url(cromwellBase(), path = path)
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
#' @importFrom lubridate fast_strptime
#' @importFrom tibble as_tibble
#'
#' @examples
#' \dontrun{
#' res = cromwellQuery(status='Succeeded')
#' head(res)
#' }
#'
#' @export
cromwellQuery = function(name = NULL, id = NULL, status = NULL, start = NULL, end = NULL, label = NULL, ...) {
    cnames = c('name', 'id', 'status') # character vectors
    dnames = c('submission', 'start', 'end') # date objects required

    .statuses = c('Submitted','Running','Aborting','Aborted','Failed','Succeeded')

    query = list(name=name, id=id, status=status, start=start, end=end, label=label)
    path = 'api/workflows/v1/query'
    resp = cromwell_GET(path=path,query=query,...)
    x = lapply(c(cnames,dnames),function(cname) {
      as.character(sapply(resp$content$results, '[[', cname))
    })
    x = setNames(x,c(cnames,dnames))
    x = data.frame(x, stringsAsFactors = FALSE)
    #x = do.call(rbind.fill,lapply(resp$content$results,as.data.frame))
    if('start' %in% colnames(x))
        x$start = fast_strptime(as.character(x$start),format="%Y-%m-%dT%H:%M:%OS%z")
    else
        x$start = NA
    if('end' %in% colnames(x))
        x$end = fast_strptime(as.character(x$end),format="%Y-%m-%dT%H:%M:%OS%z")
    else
        x$end = NA
    if('submission' %in% colnames(x))
      x$submission = fast_strptime(as.character(x$submission),format="%Y-%m-%dT%H:%M:%OS%z")
    else
      x$submission = NA
    x$end = as.POSIXct(x$end,tz=Sys.timezone())
    x$start = as.POSIXct(x$start,tz=Sys.timezone())
    x$submission = as.POSIXct(x$submission,tz=Sys.timezone())
    # deal with situation when no ends exist
    # subtraction ends up "failing", so need
    # to catch error
    x$duration = tryCatch({x$end-x$start},
                          error=function(e) "")
    # Coerce to difftime so that column is always
    # difftime, even when only NA
    x$duration = as.difftime(x$duration)
    x = as_tibble(x)
    attr(x,'when') = Sys.time()
    attr(x,'path') = path
    class(x) = c('cromwell_query','cromwell_api',class(x))
    return(x)
}


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
#' @export
cromwellMetadata = function(ids,query=NULL,...) {
    retlist = lapply(ids,function(id) {
        path=sprintf('api/workflows/v1/%s/metadata',id)
        resp = cromwell_GET(path = path, query=query, ...)
        ret = resp$content
        attr(ret,'path') = path
        attr(ret,'when') = Sys.time()
        class(ret) = c('cromwell_metadata','cromwell_api',class(ret))
        ret
    })
    retlist = setNames(retlist,ids)
    attr(retlist,'when') = Sys.time()
    class(retlist) = c('cromwell_metadata_list','cromwell_api',class(retlist))
    return(retlist)
}

print.cromwell_metadata_list = function(x, ...) {
  cat(sprintf("<Cromwell Metadata List>\nlength: %d", length(x)))
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
#' @export
cromwellAbort = function(id, ...) {
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

#' @examples
#' \dontrun{
#' res = cromwellQuery(terms=c(status='Succeeded',name='taskName'))
#' head(res)
#' outfilelist = cromwellOutputs(res$id)
#' str(outfilelist,list.len=5)
#' }
#'
#' @export
cromwellOutputs = function(ids, ...) {
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
    return(retlist)
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
#' @export
cromwellLogs = function(ids, ...) {
    retlist = lapply(ids,function(id) {
        path = sprintf('api/workflows/v1/%s/logs', id)
        resp = cromwell_GET(path = path, ...)
        ret = resp$content$calls
        attr(ret,'path') = path
        attr(ret,'when') = Sys.time()
        class(ret) = c('cromwell_log','cromwell_api',class(ret))
        ret
    })
    retlist = setNames(retlist,ids)
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
#' @export
cromwellBatch = function(wdlSource,
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
    else
        opts = workflowOptions
    }
    body = list(wdlSource       = wdlSource,
                workflowInputs  = inputs,
                customLabels = toJSON(customLabels,auto_unbox=TRUE),
                workflowOptions = opts)

    return(cromwell_POST('/api/workflows/v1/batch',body = body, encode = 'multipart',
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
#' @export
cromwellSingle = function(wdlSource,
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


#' List available backends for a cromwell endpoint
#'
#' This endpoint returns a list of the backends supported by the
#' server as well as the default backend.
#'
#' @param ... passed directly to httr `GET` (for including `timeouts`, `handles`, etc.)
#'
#' @return a list that includes backend details
#'
#' @importFrom httr GET
#'
#' @references \url{https://github.com/broadinstitute/cromwell#get-apiworkflowsversionbackends}
#'
#' @examples
#' #cromwellBackends()
#' @export
cromwellBackends = function(...) {
    path = 'api/workflows/v1/backends'
    resp = cromwell_GET(path = path, ...)
    ret = resp$content
    attr(ret,'path') = path
    attr(ret,'when') = Sys.time()
    class(ret) = c('cromwell_backends','cromwell_api',class(ret))
    return(ret)
}

print.cromwell_backends = function(x, ...) {
  cat(sprintf("Supported backends: \n    %s\nDefault backend: \n    %s", 
                paste(x$supportedBackends,collapse = '\n    '), x$defaultBackend))
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
#' @references \url{https://github.com/broadinstitute/cromwell#get-apiengineversionstats}
#'
#' @examples
#' #cromwellStats()
#' @export
cromwellStats = function(...) {
    path = '/engine/v1/stats'
    resp = cromwell_GET(path = path, ...)
    ret = resp$content
    attr(ret,'path') = path
    attr(ret,'when') = Sys.time()
    class(ret) = c('cromwell_stats','cromwell_api',class(ret))
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
#' @references \url{https://github.com/broadinstitute/cromwell#get-version}
#'
#' @examples
#' #cromwellVersion()
#' @export
cromwellVersion = function(...) {
    path = '/engine/v1/version'
    resp = cromwell_GET(path = path, ...)
    ret = resp$content$cromwell
    attr(ret,'path') = path
    attr(ret,'when') = Sys.time()
    class(ret) = c('cromwell_version','cromwell_api',class(ret))
    return(ret)
}

print.cromwell_version = function(x, ...) {
  cat(sprintf("Cromwell server version: %s", x))
}


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
#' version = '24'
#' tmpfile = file.path(tempdir(),'cromwell.jar')
#' fp = getCromwellJar(cromwell_version = version)
#' fp
#' unlink(fp)
#'
#' @export
getCromwellJar = function(cromwell_version,destfile = file.path(tempdir(),'cromwell.jar')) {
    fname = destfile
    httr::GET(sprintf('https://github.com/broadinstitute/cromwell/releases/download/%s/cromwell-%s.jar',
                      cromwell_version,cromwell_version),write_disk(fname,overwrite = TRUE))
    invisible(fname)
}

#' Cromwell reference class
#'
setRefClass('wdlrunr',
            fields = list(
                host = 'character',
                port = 'integer'
            ),
            methods = list(
                initialize         = function(host ='localhost',
                                              port = 8000) {
                    "This is test documentation"
                    initFields(host = host,port = as.integer(port))
                },
                getCromwellJar     = getCromwellJar,
                version            = cromwellVersion,
                stats              = cromwellStats,
                backends           = cromwellBackends,
                submit             = cromwellSingle,
                batch              = cromwellBatch,
                logs               = cromwellLogs,
                outputs            = cromwellOutputs,
                abort              = cromwellAbort,
                metadata           = cromwellMetadata,
                query              = cromwellQuery
            ))
