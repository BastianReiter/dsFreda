
#' GetSystemSpecsDS
#'
#' Get CPU and RAM specifications of local system using \code{ps} package functions
#'
#' Server-side AGGREGATE method
#'
#' @return A \code{list} containing
#'            \itemize{ \item CPU.Count - Number of physical or logical CPU cores
#'                      \item RAM.Total - Total installed RAM
#'                      \item RAM.Available - RAM that is available instantly }
#'
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
GetSystemSpecsDS <- function()
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  RAMSpecs <- ps_system_memory()

  return(list(CPU.Count = ps_cpu_count(),
              RAM.Total = RAMSpecs$total,
              RAM.Available = RAMSpecs$avail))
}
