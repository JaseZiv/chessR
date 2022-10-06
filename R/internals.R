#' Check Status function
#'
#' @param res Response from API
#'
check_status <- function(res) {

  x = httr::status_code(res)

  if(x != 200) stop("The API returned an error", call. = FALSE)

}
