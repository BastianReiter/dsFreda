
#' NormalizeTable
#'
#' `r lifecycle::badge("experimental")` \cr\cr
#' Carries out table normalization procedures (like 'splitting/expanding') based on a given rule set.
#'
#' Uses \code{tidyr} functionality to transform table based on normalization rules defined in a given rule set (Default: Proc.TableNormalization)
#'
#' @param Table \code{data.frame} containing data to be transformed
#' @param TableName \code{string} - The table's name, used for command line messaging
#' @param PrimaryKey \code{character vector} - Name of features that serve as table's primary key
#' @param RootSubjectKey \code{character vector} - Names of features that identify Root subjects in current table, functioning as a foreign key (usually primary key of data set root subjects)
#' @param SeedSubjectKey \code{string} - The name of the feature identifying Seed subjects in table
#' @param RuleSet \code{data.frame} - Contains predefined set of normalization rules
#' @param PrintMessages \code{logical} - Whether to print report messages during function proceedings
#'
#' @return A \code{list} containing:
#'              \itemize{ \item Table (\code{data.frame})
#'                        \item Counter (\code{data.frame})
#'                        \item Log (\code{data.frame}) }
#'
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
NormalizeTable <- function(Table,
                           TableName = NA_character_,
                           PrimaryKey,
                           RootSubjectKey = NULL,
                           SeedSubjectKey,
                           RuleSet,
                           PrintMessages = TRUE)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  # --- For Testing Purposes ---
  # Table <- DataSet$SystemicTherapy
  # TableName <- "SystemicTherapy"
  # PrimaryKey <- PrimaryKeys[[TableName]]
  # RootSubjectKey <- RootSubjectKeys[[TableName]]
  # SeedSubjectKey <- SeedPrimaryKey
  # RuleSet <- dsCCPhos::Proc.TableNormalization %>% filter(Table == TableName)
  # PrintMessages <- TRUE

  # --- Argument Validation ---
  assert_that(is.data.frame(Table),
              is.string(TableName),
              is.character(PrimaryKey),
              is.character(RootSubjectKey),
              is.string(SeedSubjectKey),
              is.data.frame(RuleSet),
              is.flag(PrintMessages))
  stopifnot("ERROR: 'PrimaryKey' must contain column names of 'Table'." = (PrimaryKey %in% names(Table)))
  stopifnot("ERROR: 'RootSubjectKey' must contain column names of 'Table'." = (all(RootSubjectKey %in% names(Table))))
  stopifnot("ERROR: 'SeedSubjectKey' must be a column name of 'Table'!" = (SeedSubjectKey %in% names(Table)))

#-------------------------------------------------------------------------------

  # Temporary security measure: Define permitted functions
  PermittedFunctions <- c("mutate",
                          "separate_longer",
                          "separate_wider",
                          "summarize",
                          "summarise")

#-------------------------------------------------------------------------------

  # Initiate log
  Log <- Log.New(Table = TableName,
                 ProcessExecution = "Initiated",
                 Message = "Table normalization initiated.",
                 MessageClass = "Info",
                 PrintMessage = PrintMessages)

  # Initiate Counter
  Counter <- NULL

  # Sort normalization rules by evaluation order
  RuleSet <- RuleSet %>%
                  arrange(EvaluationOrder)

  # Loop through all normalization rules
  for (i in 1:nrow(RuleSet))
  {
      # Initiate log entry for current normalization rule
      Log.CurrentRule <- NULL
      # Get current rule expression as string
      Expression <- RuleSet$Expression[i]
      # Get target feature name (if there is one)
      TargetFeatureName <- RuleSet$Feature[i]

      # Check if 'Expression' string is available and if it belongs to the set of permitted functions
      if (!is.na(Expression))
      {
          if (!any(str_starts(Expression, PermittedFunctions)))
          {
              Log.CurrentRule <- Log.New(Table = TableName,
                                         ProcessExecution = "Failed",
                                         Message = paste0("The following rule in 'RuleSet' is not valid and can not be processed: '", Expression , "'!"),
                                         MessageClass = "Warning",
                                         PrintMessage = PrintMessages)

          } else {

              #-----------------------------------------------------------------
              # Prodecure for dplyr::mutate()
              #-----------------------------------------------------------------
              if (str_starts(Expression, "mutate"))
              {

                  # Substitute '.Table' and '.Feature' in Expression according to needs of further proceedings
                  Expression <- str_replace_all(Expression, ".Table", "Table")
                  if (!is.na(TargetFeatureName)) { Expression <- str_replace_all(Expression, ".Feature", TargetFeatureName) }

                  # Save original (unmutated) target feature in a 'Detector' table
                  Detector <- Table %>%
                                  select(all_of(c(PrimaryKey, RootSubjectKey, TargetFeatureName))) %>%
                                  rename(".TargetFeature.Original" = !!sym(TargetFeatureName))

                  # EXECUTE table normalization procedure by evaluating expression
                  Table <- eval(expr = parse(text = Expression)) %>% ungroup()

                  # Column-bind original and mutated target feature to detect where changes occurred
                  Detector <- Detector %>%
                                  bind_cols(Table[TargetFeatureName]) %>%
                                  rename(".TargetFeature.Mutated" = !!sym(TargetFeatureName)) %>%
                                  mutate(.HasChanged = case_when(is.na(.TargetFeature.Original) & is.na(.TargetFeature.Mutated) ~ FALSE,
                                                                 is.na(.TargetFeature.Original) | is.na(.TargetFeature.Mutated) ~ TRUE,      # This is effectively a logical XOR (only ONE of both is NA)
                                                                 .TargetFeature.Original != .TargetFeature.Mutated ~ TRUE,
                                                                 .default = FALSE)) %>%
                                  filter(.HasChanged == TRUE)

                  # Create COUNTER entry for current normalization rule
                  Counter.CurrentRule <- Detector %>%
                                              summarize(Table = TableName,
                                                        ProcessTopic = "Table normalization",
                                                        ProcessTopic.Subgroup = paste0("Normalization Rule: ", Expression),
                                                        ProcessExecution = "Executed",
                                                        CountLevel = "Subgroup",
                                                        CountRecords.Detected = n(),
                                                        CountRecords.Change = 0,      # mutate() never leads to changes in records count
                                                        CountRootSubjects.Affected = n_distinct(pick(all_of(RootSubjectKey))),
                                                        CountSeedSubjects.Affected = n_distinct(pick(all_of(SeedSubjectKey))),
                                                        Message = paste0("Table normalization rule '", Expression, "' affected ", CountRootSubjects.Affected, " <Root subjects> / ", CountSeedSubjects.Affected, " <Seed subjects>. No records were removed or added."),
                                                        MessageClass = "Success") %>%
                                              Counter.Make()

                  # Add current Counter entry to full Counter
                  Counter <- Counter %>%
                                  bind_rows(Counter.CurrentRule)

                  # Create log entry
                  Log.CurrentRule <- Counter.CurrentRule %>%
                                          Log.Make() %>%
                                          mutate(ProcessExecution = "Executed")


              #-----------------------------------------------------------------
              # Prodecure for tidyr::separate_longer_delim()
              #-----------------------------------------------------------------
              } else if (str_starts(Expression, "separate_longer_delim")) {

                  # Create auxiliary ID feature to get guaranteed unique values ('PrimaryKey' feature might not always be unique)
                  if (!(".AuxID" %in% names(Table)))
                  {
                      Table <- Table %>% ungroup() %>% mutate(.AuxID = row_number())

                  } else {    # In the (unlikely) case of preexistence of a feature named '.AuxID' stop and print error message

                      stop("ERROR: The passed table contains a feature called '.AuxID' which interferes with function protocol. Please rename this feature and try again.", call. = FALSE)
                  }

                  # Substitute '.Table' and '.Feature' in Expression according to needs of further proceedings
                  Expression <- str_replace_all(Expression, ".Table", ".")
                  if (!is.na(TargetFeatureName)) { Expression <- str_replace_all(Expression, ".Feature", TargetFeatureName) }

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

                  # For printing purposes in Log, de-escape string
                  Expression.Print <- str_replace_all(Expression, "\\\\\\\\", "\\\\")

                  # Create COUNTER entry for current normalization rule
                  Counter.CurrentRule <- Table %>%
                                            mutate(.CountValueSeparations = if_else(is.na(.data[[TargetFeatureName]]), 0,
                                                                                    str_count(.data[[TargetFeatureName]], DelimPattern))) %>%      # How many separations are occurring in the values of the target feature based on the given pattern in 'DelimPattern'?
                                            filter(.CountValueSeparations > 0) %>%
                                            summarize(Table = TableName,
                                                      ProcessTopic = "Table normalization",
                                                      ProcessTopic.Subgroup = paste0("Normalization Rule: ", Expression),
                                                      ProcessExecution = "Executed",
                                                      CountLevel = "Subgroup",
                                                      CountRecords.Detected = n(),
                                                      CountRecords.Added = sum(.CountValueSeparations),
                                                      CountRootSubjects.Affected = n_distinct(pick(all_of(RootSubjectKey))),
                                                      CountSeedSubjects.Affected = n_distinct(pick(all_of(SeedSubjectKey))),
                                                      Message = paste0("Table normalization rule '", Expression.Print, "' affected ", CountRecords.Detected, " table records of ", CountRootSubjects.Affected, " <Root subjects> / ", CountSeedSubjects.Affected, " <Seed subjects> and led to the addition of ", CountRecords.Added, " records."),
                                                      MessageClass = "Success") %>%
                                            Counter.Make()

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
                                select(-.AuxID,
                                       -.SubID)

                  # Count Root / Seed subjects and records in current table version
                  CurrentCount <- Table %>%
                                      summarize(Records = n(),
                                                RootSubjects = n_distinct(pick(all_of(RootSubjectKey))),
                                                SeedSubjects = n_distinct(pick(all_of(SeedSubjectKey))))

                  # Add current root subject and record counts to Counter entry
                  Counter.CurrentRule <- Counter.CurrentRule %>%
                                              mutate(CountRecords.Post = CurrentCount$Records,
                                                     CountRootSubjects.Post = CurrentCount$RootSubjects,
                                                     CountSeedSubjects.Post = CurrentCount$SeedSubjects)

                  # Add current Counter entry to full Counter
                  Counter <- Counter %>%
                                  bind_rows(Counter.CurrentRule)

                  # Create log entry
                  Log.CurrentRule <- Counter.CurrentRule %>%
                                          Log.Make() %>%
                                          mutate(ProcessExecution = "Executed")


              #-----------------------------------------------------------------
              # Procedure for dplyr::summarize()
              #-----------------------------------------------------------------
              } else if (any(str_starts(Expression, c("summarize", "summarise")))) {

                  # Substitute '.Table' and '.Feature' in Expression according to needs of further proceedings
                  Expression <- str_replace_all(Expression, ".Table", "Table")
                  if (!is.na(TargetFeatureName)) { Expression <- str_replace_all(Expression, ".Feature", TargetFeatureName) }

                  # Save the record counts attributed to each root subject ahead of rule execution
                  Aux.CountSubjectRecords <- Table %>%
                                                  group_by(pick(all_of(RootSubjectKey))) %>%
                                                  summarize(CountSubjectRecords.Pre = n())

                  # EXECUTE table normalization procedure by evaluating expression
                  Table <- eval(expr = parse(text = Expression)) %>% ungroup()

                  # Which root subjects were affected by the procedure?
                  Detector.RootSubjectAffection <- Table %>%
                                                      group_by(pick(all_of(RootSubjectKey))) %>%
                                                          summarize(CountSubjectRecords.Post = n()) %>%
                                                      left_join(Aux.CountSubjectRecords, by = join_by(!!!syms(RootSubjectKey))) %>%
                                                      mutate(.RootSubjectIsAffected = CountSubjectRecords.Pre != CountSubjectRecords.Post) %>%
                                                      filter(.RootSubjectIsAffected == TRUE)

                  # Create COUNTER entry for current normalization rule
                  Counter.CurrentRule <- Detector.RootSubjectAffection %>%
                                              summarize(Table = TableName,
                                                        ProcessTopic = "Table normalization",
                                                        ProcessTopic.Subgroup = paste0("Normalization Rule: ", Expression),
                                                        ProcessExecution = "Executed",
                                                        CountLevel = "Subgroup",
                                                        CountRecords.Change = sum(CountSubjectRecords.Post) - sum(CountSubjectRecords.Pre),
                                                        CountRootSubjects.Affected = n_distinct(pick(all_of(RootSubjectKey))),
                                                        CountSeedSubjects.Affected = n_distinct(pick(all_of(SeedSubjectKey))),
                                                        Message = paste0("Table normalization rule '", Expression, "' affected ", CountRootSubjects.Affected, " <Root subjects> / ", CountSeedSubjects.Affected, " <Seed subjects> and reduced the table by ", abs(CountRecords.Change), " records."),
                                                        MessageClass = "Success") %>%
                                              Counter.Make()

                  # Add current Counter entry to full Counter
                  Counter <- Counter %>%
                                  bind_rows(Counter.CurrentRule)

                  # Create log entry
                  Log.CurrentRule <- Counter.CurrentRule %>%
                                          Log.Make() %>%
                                          mutate(ProcessExecution = "Executed")
              }

              #### TO DO ####
              # Add prodecure for tidyr::separate_wider()

          }
      }

      # Add log entry of current normalization rule to full log
      Log <- Log %>%
                Log.Add(Log.CurrentRule,
                        PrintMessage = PrintMessages)
  }

  # Complement log
  Log <- Log %>%
            mutate(ProcessTopic = "Table normalization")

#-------------------------------------------------------------------------------
  return(list(Table = Table,
              Counter = Counter,
              Log = Log))
}
