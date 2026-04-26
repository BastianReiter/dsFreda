
#' CheckMetaData
#'
#' Check for correctness of meta data describing a hierarchical relational data set.
#'
#' @param MetaData \code{list} - A \code{list} of \code{data.frames} describing the structural and semantic characteristics of a hierarchical data model.
#'            \itemize{ \item \code{Meta.Tables}
#'                      \item \code{Meta.Features}
#'                      \item \code{Meta.Relationships}
#'                      \item \code{Meta.Links}
#'                      \item \code{Meta.Values}}
#'
#' @return \code{list} of messages
#'
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
CheckMetaData <- function(MetaData)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  # --- For Testing Purposes ---
  # MetaData <- list(Tables = dsCCPhos::Meta.Tables,
  #                  Relationships = dsCCPhos::Meta.Relationships,
  #                  Links = dsCCPhos::Meta.Links,
  #                  Features = dsCCPhos::Meta.Features,
  #                  Values = dsCCPhos::Meta.Values)

  # --- Argument Validation ---
  assert_that(is.list(MetaData),
              is.data.frame(MetaData$Tables),
              is.data.frame(MetaData$Relationships),
              is.data.frame(MetaData$Links),
              is.data.frame(MetaData$Features),
              is.data.frame(MetaData$Values))

#-------------------------------------------------------------------------------

  CheckSuccessful <- FALSE


  # Assumptions:
  #   - Names of Primary and Foreign Key features should be equal


  SeedTableName <- MetaData$Tables %>%
                        filter(Role == "Seed") %>%
                        pull(TableName.Curated)

  if (length(SeedTableName) != 1) { CheckSuccessful <- FALSE}


#-------------------------------------------------------------------------------
  return(list(Table = Table,
              Report = Report,
              AffectedRootObjects = AffectedRootObjects))
}
