#' Deal Class Object
#'
#' The Deal Class creates an object that is able to handle long/short position
#' with stop-loss, take-profit and trailing stop features.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
Deal <- R6Class(
    classname = "Deal",

    #### Public Attributes & Methods ####
    public = list(

        initialize = function(price, side) {

            private$price <- price
            private$side <- side
            private$timestamp <- Sys.time()

        }

    ),

    #### Public Attributes & Methods ####
    private = list(

        price = NULL,
        side = NULL,
        timestamp = NULL

    ),

    lock_class = TRUE

)
