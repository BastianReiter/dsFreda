
#' NormalizeTable
#'
#' Carries out table harmonization procedures (like 'splitting/expanding') based on a given rule set.
#'
#' Uses \code{tidyr} functionality to transform table based on normalization rules defined in a given rule set (Default: Proc.TableNormalization)
#'
#' @param Table \code{data.frame} containing data to be transformed
#' @param TableName \code{string} - Name of the table (to enable mapping to normalization rules)
#' @param PrimaryKey \code{character vector} - Name of features that serve as table's primary key
#' @param ForeignKey \code{character vector} - Names of features that serve as table's foreign key (usually primary key of data set 'root subjects')
#' @param RuleSet \code{data.frame} - Contains predefined set of normalization rules
#' @param RuleSet.Profile \code{string} - Profile name stated in 'RuleSet'
#'
#' @return The transformed input \code{data.frame}
#'
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
NormalizeTable <- function(Table,
                           TableName,
                           PrimaryKey,
                           ForeignKey = NULL,
                           RuleSet,
                           RuleSet.Profile)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  # --- For Testing Purposes ---
  # Table <- DataSet$SystemicTherapy
  # TableName <- "SystemicTherapy"
  # PrimaryKey <- "SystemicTherapyID"
  # ForeignKey <- c("DiagnosisID", "PatientID")
  # RuleSet <- dsCCPhos::Proc.TableNormalization
  # RuleSet.Profile <- "Default"

  # --- Argument Validation ---
  assert_that(is.data.frame(Table),
              is.string(TableName),
              is.character(PrimaryKey),
              is.data.frame(RuleSet),
              is.string(RuleSet.Profile))
  stopifnot("ERROR: 'PrimaryKey' must contain column names of 'Table'." = (PrimaryKey %in% names(Table)))
  if (length(ForeignKey) > 0) { assert_that(is.character(ForeignKey))
                                stopifnot("ERROR: 'ForeignKey' must contain column names of 'Table'." = (all(ForeignKey %in% names(Table)))) }
  stopifnot("ERROR: Value of argument 'RuleSet.Profile' must be a column name of 'RuleSet'." = (RuleSet.Profile %in% names(RuleSet)))

#-------------------------------------------------------------------------------

  # If no 'ForeignKey' is passed, set the variable equal to 'PrimaryKey' (this ensures syntactic validity throughout function proceedings)
  if (length(ForeignKey) == 0) { ForeignKey <- PrimaryKey }

  # Temporary security measure: Define permitted functions
  PermittedFunctions <- c("separate_longer",
                          "separate_wider")

#-------------------------------------------------------------------------------

  # Create default report object
  Report <- tibble(ProcessTopic = "Table normalization",
                   ProcessExecuted = FALSE,
                   ReportType = "Message",
                   DetailsGroup = NA_character_,
                   CountRootSubjects = NA_integer_,
                   CountEntries = NA_integer_,
                   Message = paste0("Table normalization: Table '", tablename, "' is missing or empty."),
                   MessageClass = "Info",
                   Timestamp = Sys.time())

  # Filter relevant rules from given rule set
  RelevantRules <- RuleSet %>%
                        filter(Profile == RuleSet.Profile
                                & Table == TableName) %>%
                        arrange(EvaluationOrder)

  if (length(RelevantRules ) > 0 && nrow(RelevantRules) > 0)
  {
      Report <- Report %>%
                    mutate(Message = paste0("Table normalization for '", tablename, "': Process initiated."))


      # Save all valid (permitted) expressions in 'ValidExpressions' and report a warning for all others
      for (i in 1:nrow(RelevantRules))
      {
          Expression <- RelevantRules$Expression[i]
          # Get target feature name (if there is one)
          TargetFeatureName <- RelevantRules$Feature[i]

          # Check if 'Expression' string is available and if it belongs to the set of permitted functions
          if (!is.na(Expression))
          {
              if (any(str_starts(Expression, PermittedFunctions)))
              {
                  Expression <- str_replace(Expression, ".Table", ".")
                  if (!is.na(TargetFeatureName)) { Expression <- str_replace(Expression, ".Feature", TargetFeatureName) }


                  if (str_starts(Expression, "seaparate_longer_delim"))
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

                      Tracker <- Table %>%
                                    mutate(.EntryID = row_number(),
                                           .CountValueSeparations = if_else(is.na(.data[[TargetFeatureName]]), 0,
                                                                            str_count(.data[[TargetFeatureName]], DelimPattern))) %>%      # How many separations are occurring in the values of the target feature based on the given pattern in 'DelimPattern'?
                                    filter(.CountValueSeparations > 0) %>%
                                    summarize(ReportType = "Details",
                                              DetailsGroup = paste0("Rule: ", Expression),
                                              CountRootSubjects.Affected = n_distinct(across(all_of(ForeignKey))),
                                              CountEntries.Affected = n(),
                                              CountEntries.Added = sum(.CountValueSeparations),
                                              Message = paste0("Normalization of table '", TableName, "': Rule '", Expression, "' affected ", CountEntries.Affected, " table entries of ", CountRootSubjects.Affected, " root subjects and led to addition of ", CountEntries.Added, " entries."),
                                              MessageClass = "Success",
                                              Timestamp = Sys.time())
                  }




                  # PERFORM table normalization procedures by evaluating expressions
                  TableW <- Table %>%
                                eval(expr = parse(text = Expression))


              } else {

                  Report <- Report %>%
                                add_row(ReportType = "Message",
                                        Message = paste0("Table normalization for table '", tablename, "': The following entry in 'RuleSet' is not valid and can not be processed: '", Expression , "'!"),
                                        MessageClass = "Warning",
                                        Timestamp = Sys.time())
              }
          }


      }

      # Process all valid expressions
      if (length(ValidExpressions) > 0)
      {

          for (i in 1:length(ValidExpressions))
          {
              #Expression <- ValidExpressions[i]






          }

          Report <- Report %>%
                        mutate(ProcessExecuted = TRUE)

      }

  } else {

      Report <- Report %>%
                    mutate(Message = paste0("Table normalization: Found no rules for table '", tablename, "'."),
                           MessageClass = "Info",
                           Timestamp = Sys.time())
  }

  return(list(Table = Table,
              Report = Report,
              AffectedRootSubjects = AffectedRootSubjects))
}
