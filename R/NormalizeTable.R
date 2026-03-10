
#' NormalizeTable
#'
#' `r lifecycle::badge("experimental")` \cr\cr
#' Carries out table harmonization procedures (like 'splitting/expanding') based on a given rule set.
#'
#' Uses \code{tidyr} functionality to transform table based on normalization rules defined in a given rule set (Default: Proc.TableNormalization)
#'
#' @param Table \code{data.frame} containing data to be transformed
#' @param PrimaryKey \code{character vector} - Name of features that serve as table's primary key
#' @param ForeignKey \code{character vector} - Names of features that serve as table's foreign key (usually primary key of data set root subjects)
#' @param RuleSet \code{data.frame} - Contains predefined set of normalization rules
#'
#' @return The transformed input \code{data.frame}
#'
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
NormalizeTable <- function(Table,
                           PrimaryKey,
                           ForeignKey = NULL,
                           RuleSet)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  # --- For Testing Purposes ---
  # Table <- DataSet$SystemicTherapy
  # PrimaryKey <- "SystemicTherapyID"
  # ForeignKey <- c("DiagnosisID", "PatientID")
  # RuleSet <- dsCCPhos::Proc.TableNormalization %>% filter(Table == "SystemicTherapy")

  # --- Argument Validation ---
  assert_that(is.data.frame(Table),
              is.character(PrimaryKey),
              is.data.frame(RuleSet))
  stopifnot("ERROR: 'PrimaryKey' must contain column names of 'Table'." = (PrimaryKey %in% names(Table)))
  if (length(ForeignKey) > 0) { assert_that(is.character(ForeignKey))
                                stopifnot("ERROR: 'ForeignKey' must contain column names of 'Table'." = (all(ForeignKey %in% names(Table)))) }

#-------------------------------------------------------------------------------

  # If no 'ForeignKey' is passed, set the variable equal to 'PrimaryKey' (this ensures syntactic validity throughout function proceedings)
  if (length(ForeignKey) == 0) { ForeignKey <- PrimaryKey }

  # Temporary security measure: Define permitted functions
  PermittedFunctions <- c("separate_longer",
                          "separate_wider")

#-------------------------------------------------------------------------------

  # Initiate report object
  Report <- tibble(ProcessExecution = "Initiated",
                   ReportType = "Message",
                   Message = paste0("Table normalization initiated."),
                   MessageClass = "Info",
                   Timestamp = Sys.time())

  # Sort normalization rules by evaluation order
  RuleSet <- RuleSet %>%
                  arrange(EvaluationOrder)

  # Loop through all normalization rules
  for (i in 1:nrow(RuleSet))
  {
      # Initiate report for current normalization rule
      Report.CurrentRule <- NULL
      # Get current rule expression as string
      Expression <- RuleSet$Expression[i]
      # Get target feature name (if there is one)
      TargetFeatureName <- RuleSet$Feature[i]

      # Check if 'Expression' string is available and if it belongs to the set of permitted functions
      if (!is.na(Expression))
      {
          if (any(str_starts(Expression, PermittedFunctions)))
          {
              Expression <- str_replace(Expression, ".Table", ".")
              if (!is.na(TargetFeatureName)) { Expression <- str_replace(Expression, ".Feature", TargetFeatureName) }

              # Prodecure for tidyr::separate_longer_delim()
              if (str_starts(Expression, "separate_longer_delim"))
              {
                  # Get string / pattern used in 'delim' argument of function 'separate_longer_delim()'
                  # This pattern will be used further down to obtain number of value splits and thus number of added rows
                  # Note: '<#delim: ... #>' is pseudo-code marking the content of delim argument in complete expression
                  DelimPattern <- str_match(Expression, "<#delim:(.*?)#>")[,2]
                  # If function 'regex()' is used in delim argument, get its 'inside' pattern string
                  if (str_detect(Expression, "<#delim:regex\\(")) { DelimPattern <- str_match(Expression, "<#delim:regex\\('(.*?)'\\)#>")[,2] }
                  # Fix over-escaped string
                  DelimPattern <- str_replace_all(DelimPattern, "\\\\\\\\", "\\\\")

                  # Get rid of pseudo-code '<#delim: ... #>' in 'Expression'
                  Expression <- str_replace_all(string = Expression,
                                                pattern = "<#delim:(.*?)#>",
                                                replacement = "\\1")

                  # For printing purposes in Report, de-escape string
                  Expression.Print <- str_replace_all(Expression, "\\\\\\\\", "\\\\")

                  # Create REPORT DETAILS for current normalization rule
                  Report.CurrentRule <- Table %>%
                                            mutate(.CountValueSeparations = if_else(is.na(.data[[TargetFeatureName]]), 0,
                                                                                    str_count(.data[[TargetFeatureName]], DelimPattern))) %>%      # How many separations are occurring in the values of the target feature based on the given pattern in 'DelimPattern'?
                                            filter(.CountValueSeparations > 0) %>%
                                            summarize(ProcessExecution = "Executed",
                                                      ReportType = "Details",
                                                      DetailsGroup = paste0("Normalization Rule: ", Expression),
                                                      CountRootSubjects.Affected = n_distinct(across(all_of(ForeignKey))),
                                                      CountRecords.Affected = n(),
                                                      CountRecords.Added = sum(.CountValueSeparations),
                                                      Message = paste0("Table normalization rule '", Expression.Print, "' affected ", CountRecords.Affected, " table records of ", CountRootSubjects.Affected, " root subjects and led to the addition of ", CountRecords.Added, " records."),
                                                      MessageClass = "Success",
                                                      Timestamp = Sys.time())

                  # EXECUTE table normalization procedure by evaluating expression
                  Table <- Table %>%
                                mutate(.OriginalID = .data[[PrimaryKey]]) %>%      # Preserve original primary key
                                eval(expr = parse(text = Expression)) %>%      # Evaluate expression (separate_longer_delim(...)). Note: If new records are added, the value in .OriginalID gets copied unchanged.
                                mutate(.SubID = sequence(rle(.OriginalID)$lengths),      # This numbers the records that originally came from one 'parent record'
                                       !!PrimaryKey := if_else(.SubID == 1,      # The feature holding the primary key gets re-assigned: Original records keep the original primary key values, new ones get a new primary key value
                                                               .OriginalID,
                                                               paste0(.OriginalID, "-", .SubID)),
                                       .IsOriginal := if_else(.SubID == 1,      # For tracking purposes: Mark all records that were 'artificially added' when executing separate_longer_delim()
                                                              TRUE,
                                                              FALSE)) %>%
                                select(-.OriginalID,
                                       -.SubID)
              }

              #### TO DO ####
              # Add prodecure for tidyr::separate_wider()

          } else {

          Report.CurrentRule <- tibble(ProcessExecution = "Failed",
                                       ReportType = "Message",
                                       Message = paste0("Table normalization: The following rule in 'RuleSet' is not valid and can not be processed: '", Expression , "'!"),
                                       MessageClass = "Warning",
                                       Timestamp = Sys.time())
          }
      }

      # Add report of current normalization rule to full report
      Report <- Report %>%
                    bind_rows(Report.CurrentRule)
  }

  # Add feature to all report records
  Report <- Report %>%
                mutate(ProcessTopic = "Table normalization")

#-------------------------------------------------------------------------------
  return(list(Table = Table,
              Report = Report))
}
