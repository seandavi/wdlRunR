#' @include class-server.R
NULL

#' Set and get base URL and port for cromwell server
#'
#' The Cromwell server presents a RESTFul API. The base URL is of the form:
#' `http://EXAMPLE.COM:PORT`.
#' 
#' @return A character(1) base url (default \code{http://localhost:8000})
#' 
#' @examples
#' jb = new('Cromwell')
#' baseURL(jw)
#'
#' # set a bogus host
#' baseURL(jw) = 'http://example.com:8111'
#' baseURL()
#'
#' # Does error checking
#' # baseURL(x) = 'abc'
#' @export
setGeneric('baseURL', function(object) {
    standardGeneric('baseURL')
})



#' Get the baseURL from a \code{\link{Cromwell-class}} object
#'
#' @rdname baseURL
#' 
#' @export
setMethod('baseURL', signature=c('Cromwell'), function(object) {
    return(object@baseURL)
})

#' @rdname baseURL
#'
#' @param value a character(1) representing a complete URL, such as \code{http://localhost:8000}
#' 
#' @export
setGeneric('baseURL<-', function(object, value) {
    standardGeneric('baseURL<-')
})

#' @rdname baseURL
#' 
#' @export
setReplaceMethod('baseURL', c('Cromwell', 'character'), function(object, value) {
    object@baseURL = value
    validObject(object)
    return(object)
})
