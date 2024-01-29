

#' launch_RegionSizeR
#' @description Starts the RegionSizer application in the client’s browser.

#' @param host host link (defaults to the local machine "127.0.0.1")
#' @param port port number (randomly chosen unless specified as a certain number)
#' @returns No return value.
#' @export
#' @examples
#'
#'
#' if(interactive()) {
#'     launch_RegionSizeR()
#' }
#'

launch_RegionSizeR<- function(host = "127.0.0.1", port = NULL) {


    # Run the application
    shiny::runApp(appDir = system.file("demoapp", package = "RegionSizeR"), host = host, port = port)

}
