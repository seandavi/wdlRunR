#' An S4 class to represent a Cromwell server
#'
#' @slot baseUrl A character(1) string representing the connection
#'     method (http), the server name, and the port. For example, 
#'     \code{"http://localhost:8000"}
#'
setClass('Cromwell',
         representation(baseURL = "character"),
         prototype(baseURL = "http://localhost:8000")
         )

# Cromwell validity
#
# Checks:
#   - http or https
#   - servername not NULL
#   - port, if present, integer between 1 and 65535
#' @importFrom httr parse_url
check_cromwell = function(object) {
    parsed = parse_url(object@baseURL)
    .msg = NULL
    if(is.null(parsed$scheme) | is.null(parsed$hostname))
        .msg = c(.msg,'Specified URL is apparently malformed and must include scheme and hostname.')
    if(!(parsed$scheme %in% c('https','http')))
        .msg = c(.msg, 'URLs should be either http or https')
    if(!is.null(parsed$port))
        if(!(as.integer(parsed$port) %in% 1:65535))
            .msg = c(.msg,'If specified, port should be an integer between 1 and 65535')
    if(!is.null(.msg))
        stop(.msg)
    TRUE
}

setValidity("Cromwell", check_cromwell)
