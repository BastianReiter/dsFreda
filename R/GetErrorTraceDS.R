
#' GetErrorTraceDS
#'
#' Run a server-side function while tracing calls. In an error occurs, the trace is returned to the client.
#'
#' Server-side AGGREGATE method
#'
#' @param FunctionCall.S \code{call}
#'
#' @return A
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
GetErrorTraceDS <- function(FunctionCall.S)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  # --- For Testing Purposes ---
  # FunctionCall.S <-

  # --- Argument Validation ---
  assert_that(is.call(FunctionCall.S))

#-------------------------------------------------------------------------------

  .Logged <- FALSE

  withCallingHandlers(expr = eval(FunctionCall.S, envir = .GlobalEnv),
                      error = function(e)
                              {
                                  if (.Logged == TRUE) return()      # only log the first (innermost) error
                                  .Logged <<- TRUE

                                  Trace <- rlang::trace_back()
                                  ErrorMessage <- c(paste("Error:", conditionMessage(e)),
                                                     "",
                                                     format(Trace, simplify = "none"))

                                  return(ErrorMessage)
                              })
}
