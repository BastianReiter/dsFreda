
#' NormalizeTable
#'
#' `r lifecycle::badge("experimental")` \cr\cr
#' Carries out table harmonization procedures (like 'splitting/expanding') based on a given rule set.
#'
#' Uses \code{tidyr} functionality to transform table based on normalization rules defined in a given rule set (Default: Proc.TableNormalization)
#'
#' @param Table \code{data.frame} containing data to be transformed
#' @param PrimaryKey \code{character vector} - Name of features that serve as table's primary key
#' @param RootSubjectKey \code{character vector} - Names of features that identify root subjects in current table, functioning as a foreign key (usually primary key of data set root subjects)
#' @param RuleSet \code{data.frame} - Contains predefined set of normalization rules
#' @param PrintMessages \code{logical} - Whether to print report messages during function proceedings
#'
#' @return The transformed input \code{data.frame}
#'
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
NormalizeTable <- function(Table,
                           PrimaryKey,
                           RootSubjectKey = NULL,
                           RuleSet,
                           PrintMessages = TRUE)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  # --- For Testing Purposes ---
  # Table <- DataSet$SystemicTherapy
  # PrimaryKey <- "SystemicTherapyID"
  # RootSubjectKey <- c("DiagnosisID", "PatientID")
  # RuleSet <- dsCCPhos::Proc.TableNormalization %>% filter(Table == "SystemicTherapy")
  # PrintMessages <- TRUE

  # --- Argument Validation ---
  assert_that(is.data.frame(Table),
              is.character(PrimaryKey),
              is.data.frame(RuleSet),
              is.flag(PrintMessages))
  stopifnot("ERROR: 'PrimaryKey' must contain column names of 'Table'." = (PrimaryKey %in% names(Table)))
  if (length(RootSubjectKey) > 0) { assert_that(is.character(RootSubjectKey))
                                stopifnot("ERROR: 'RootSubjectKey' must contain column names of 'Table'." = (all(RootSubjectKey %in% names(Table)))) }

#-------------------------------------------------------------------------------

  # Temporary security measure: Define permitted functions
  PermittedFunctions <- c("separate_longer",
                          "separate_wider")

#-------------------------------------------------------------------------------


  # Create auxiliary ID feature to get guaranteed unique values ('PrimaryKey' feature might not always be unique)
  if (!(".AuxID" %in% names(Table)))
  {
      Table <- Table %>% ungroup() %>% mutate(.AuxID = row_number())

  } else {    # In the (unlikely) case of preexistence of a feature named '.AuxID' stop and print error message

      stop("ERROR: The passed table contains a feature called '.AuxID' which interferes with function protocol. Please rename this feature and try again.", call. = FALSE)
  }

  # Initiate report object
  Report <- Log.New(ProcessExecution = "Initiated",
                    ReportType = "Message",
                    Message = paste0("Table normalization initiated."),
                    MessageClass = "Info",
                    PrintMessage = PrintMessages)

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
                                                      CountRootSubjects.Affected = n_distinct(pick(RootSubjectKey)),
                                                      CountRecords.Affected = n(),
                                                      CountRecords.Added = sum(.CountValueSeparations),
                                                      Message = paste0("Table normalization rule '", Expression.Print, "' affected ", CountRecords.Affected, " table records of ", CountRootSubjects.Affected, " root subjects and led to the addition of ", CountRecords.Added, " records."),
                                                      MessageClass = "Success") %>%
                                            Log.Make(PrintMessage = PrintMessages)

                  # EXECUTE table normalization procedure by evaluating expression
                  Table <- Table %>%
                                eval(expr = parse(text = Expression)) %>%      # Evaluate expression (separate_longer_delim(...)). Note: If new records are added, the value in .AuxID gets copied unchanged.
                                mutate(.CountSpawns = rep(rle(.AuxID)$lengths, times = rle(.AuxID)$lengths),      # Count the number of same .AuxID values
                                       .HasBeenSplit = if_else(.CountSpawns > 1, TRUE, FALSE),      # For tracking purposes
                                       .SubID = sequence(rle(.AuxID)$lengths),      # This numbers the records that originally came from one 'parent record'
                                       .IsArtificial = if_else(.SubID > 1, TRUE, FALSE),      # For tracking purposes: Mark all records that were 'artificially added' when executing separate_longer_delim()
                                       !!PrimaryKey := if_else(.SubID == 1,      # The feature holding the primary key gets re-assigned: Original records keep the original primary key values, new ones get a new primary key value
                                                               .data[[PrimaryKey]],
                                                               paste0(.data[[PrimaryKey]], "-", .SubID))) %>%
                                select(-.SubID)
              }

              #### TO DO ####
              # Add prodecure for tidyr::separate_wider()

          } else {

            Report.CurrentRule <- Log.New(ProcessExecution = "Failed",
                                          ReportType = "Message",
                                          Message = paste0("Table normalization: The following rule in 'RuleSet' is not valid and can not be processed: '", Expression , "'!"),
                                          MessageClass = "Warning",
                                          PrintMessage = PrintMessages)
          }
      }

      # Add report of current normalization rule to full report
      Report <- Report %>%
                    Log.Add(Report.CurrentRule)
  }

  # Get rid of .AuxID (not needed anymore)
  Table <- Table %>% select(-.AuxID)

  # Add feature to all report records
  Report <- Report %>%
                mutate(ProcessTopic = "Table normalization")

#-------------------------------------------------------------------------------
  return(list(Table = Table,
              Report = Report))
}
