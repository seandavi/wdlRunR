library(httr)
cromwell_path <- function(path) {
  base_url = getOption('cromwell_base',default="http://localhost:8000")
  base_url <- modify_url(base_url, path = path)
  return(base_url)
}

#' Get the info about cromwell workflows
#'
#' @param terms
#'
#' @return a data.frame of query results
#'
#' @importFrom httr GET
#' @importFrom plyr rbind.fill
#' @examples
#' #cromwellQuery(terms='status=Succeeded')
#' @export
cromwellQuery = function(terms=NULL) {
    req_url = cromwell_path('api/workflows/v1/query')

    if(!is.null(terms)) {
        req_url = modify_url(req_url,query=terms)
        return(content(GET(req_url)))
    }
    df = do.call(rbind.fill,lapply(content(GET(cromwell_path('api/workflows/v1/query')),flatten=TRUE)$results,as.data.frame))                
    return(df)
}


#' Get metadata associated with one or more workflow ids
#'
#' @param id
#'
#' @return a list of metadata lists
#'
#' @importFrom httr GET
#' @examples
#' #cromwellQuery(ids=c('1','2','abc'))
#' @export
cromwellMetadata = function(id) {
    req_url = cromwell_path(sprintf('api/workflows/v1/%s/metadata',
                                     id))
    return(content(GET(req_url)))
}

#' Abort a cromwell job
#'
#' @param id
#'
#' 
#' @importFrom httr GET
#' @examples
#' #cromwellQuery(ids=c('1','2','abc'))
#' @export
cromwellAbort = function(id) {
    req_url = cromwell_path(sprintf('api/workflows/v1/%s/abort',
                                     id))
    return(content(GET(req_url)))
}

#' Get output paths associated with one or more workflow ids
#'
#' @param id
#'
#' @return a list of output lists
#'
#' @importFrom httr GET
#' @examples
#' #cromwellOutputs(ids)
#' @export
cromwellOutputs = function(id) {
    return(content(cromwell_api(sprintf('/api/workflows/v1/%s/outputs',
                               id))))
}



#' Get output paths associated with one or more workflow ids
#'
#' @param id
#'
#' @return a list of logfile lists
#'
#' @importFrom httr GET
#'
#' #' @examples
#' #cromwellLogs(ids)
#' @export
cromwellLogs = function(id) {
    return(content(GET(sprintf('http://146.148.109.181:8000/api/workflows/v1/%s/logs',
                               id))))
}

