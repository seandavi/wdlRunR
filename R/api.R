.cromwell_base <- function() {
    base_url = getOption('cromwell_base', default="http://localhost:8000")
    return(base_url)
}

#' Perform a GET request to cromwell server
#'
#' See the docmentation at \href{https://github.com/broadinstitute/cromwell#rest-api}{the cromwell github site} for details. Generally, this is not meant to be called by the end user. Rather, use the endpoint-specific functions.
#'
#' @param path The path part of the URL
#' @param query Any query terms as a named character vector
#' @param ... passed directly to httr `POST` (for including `timeouts`, `handles`, etc.)
#'
cromwell_GET <- function(path,query=NULL,...) {
    url <- modify_url(.cromwell_base(), path = path, query = query)
    print(url)
    resp <- GET(url,...)
    return(.cromwell_process_response(resp))
}

#' Perform a GET request to cromwell server
#'
#' See the docmentation at \href{https://github.com/broadinstitute/cromwell#rest-api}{the cromwell github site} for details. Generally, this is not meant to be called by the end user. Rather, use the endpoint-specific functions.
#'
#' @param path The path part of the URL
#' @param body A list that will become the multipart form that is passed as the request body
#' @param ... passed directly to httr `POST` (for including `timeouts`, `handles`, etc.)
#'
cromwell_POST = function(path,body,...) {
    url = modify_url(.cromwell_base(), path = path)
    resp = POST(url, body = body, ...)
    return(resp)
    return(.cromwell_process_response(resp))
}


.cromwell_process_response = function(resp) {                    
    if (http_type(resp) != "application/json") {
        stop("API did not return json", call. = FALSE)
    }
    
    parsed <- jsonlite::fromJSON(content(resp, "text"), simplifyVector = FALSE)
    
    if (status_code(resp) != 200) {
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
#' @param terms terms about the workflow
#' @param ... passed directly to httr `GET` (for including `timeouts`, `handles`, etc.)
#' 
#' @return a data.frame of query results
#'
#' @importFrom httr GET
#' @importFrom plyr rbind.fill
#' 
#' @examples
#' #cromwellQuery(terms=c(status='Succeeded',name='taskName'))
#' @export
cromwellQuery = function(terms=NULL, ...) {
    path = 'api/workflows/v1/query'
    resp = cromwell_GET(path=path,query=terms)

    x = do.call(rbind.fill,lapply(resp$content$results,as.data.frame))
    if('start' %in% colnames(x)) 
        x$start = strptime(substr(as.character(x$start),1,19),format="%Y-%m-%dT%H:%M:%S",tz="UTC")
    else
        x$start = NA
    if('end' %in% colnames(x)) 
        x$end = strptime(substr(as.character(x$end),1,19),format="%Y-%m-%dT%H:%M:%S",tz="UTC")
    else
        x$end = NA
    x$duration = x$end-x$start
    attr(x,'when') = Sys.time()
    attr(x,'path') = path
    class(x) = c('cromwell_query','cromwell_api','data.frame')
    return(x)
}


#' Get metadata associated with one or more workflow ids
#'
#' @param id A cromwell id as a string
#' @param ... passed directly to httr `GET` (for including `timeouts`, `handles`, etc.)
#' 
#' @return a list of metadata lists
#'
#' @importFrom httr GET
#'
#' @examples
#' #cromwellQuery(ids=c('1','2','abc'))
#' @export
cromwellMetadata = function(id, ...) {
    path=sprintf('api/workflows/v1/%s/metadata',id)
    resp = cromwell_GET(path = path, ...)

    x = resp$content
    attr(x,'when') = Sys.time()
    attr(x,'path') = path
    class(x) = c('cromwell_metadata','cromwell_api',class(x))
    x
}

#' Abort a cromwell job
#'
#' @param id A cromwell id as a string
#' @param ... passed directly to httr `GET` (for including `timeouts`, `handles`, etc.)
#' 
#' @importFrom httr GET
#' 
#' @examples
#' #cromwellQuery(ids=c('1','2','abc'))
#' @export
cromwellAbort = function(id, ...) {
    return(cromwell_GET(path=sprintf('api/workflows/v1/%s/abort')))
}

#' Get output paths associated with one or more workflow ids
#'
#' @param id a cromwell job id
#' @param ... passed directly to httr `POST` (for including `timeouts`, `handles`, etc.)
#'
#' @return a list of output lists
#'
#' @importFrom httr GET
#'
#' @examples
#' #cromwellOutputs(ids)
#' @export
cromwellOutputs = function(id, ...) {
    path = sprintf('api/workflows/v1/%s/outputs', id)
    resp = cromwell_GET(path = path)
    ret = resp$content$calls
    attr(ret,'path') = path
    attr(ret,'when') = Sys.time()
    class(ret) = c('cromwell_logs','cromwell_api',class(ret))
    return(ret)
}



#' Get output paths associated with one or more workflow ids
#'
#' @param id a cromwell job id
#' @param ... passed directly to httr `POST` (for including `timeouts`, `handles`, etc.)
#'
#' @return a list of logfile lists
#'
#' @importFrom httr GET
#'
#' @examples
#' #cromwellLogs(id)
#' @export
cromwellLogs = function(id, ...) {
    path = sprintf('api/workflows/v1/%s/logs', id)
    resp = cromwell_GET(path = path)
    ret = resp$content$calls
    attr(ret,'path') = path
    attr(ret,'when') = Sys.time()
    class(ret) = c('cromwell_logs','cromwell_api',class(ret))
    return(ret)
}


#' Submit a cromwell batch job
#'
#' This function submits a set of one or more inputs to cromwell. It is much more efficient
#' than submitting a single job at a time.  See
#' \href{https://github.com/broadinstitute/cromwell#post-apiworkflowsversionbatch}{the cromwell \code{batch} API documentation} for details.
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
cromwellBatch = function(wdlSource,
                         workflowInputs,
                         workflowOptions=NULL,
                         timeout = 120,
                         ...) {
    body = list(wdlSource       = wdlSource,
                workflowInputs  = workflowInputs,
                workflowOptions = workflowOptions)

    return(cromwell_POST('/api/workflows/v1/batch',body = body, encode = 'multipart',
                timeout(timeout), ...))
}

