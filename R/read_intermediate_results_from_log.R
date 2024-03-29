# read_intermediate_results_from_log -------------------------------------------

#' Read Intermediate Results from Log File
#'
#' @param file path to log file
#' @param pattern_remove regular expression matching lines to remove from the
#'   log file before looking for "variable=expression" assignments
read_intermediate_results_from_log <- function(
    file = file.path(tempdir(), "abimo_input_result.log"),
    pattern_remove = "Start|unknown|angenommen|nicht definiert|std::"
)
{
  #file <- file.path(tempdir(), "abimo_input_result.log")
  raw_text <- readLines(file)

  # Remove irrelevant lines
  text <- grep(pattern_remove, raw_text, value = TRUE, invert = TRUE)

  # Pattern matching lines with code
  pattern_code <- "^\\*\\*\\* Code: "

  # Keep only lines with code or with equal sign (variable=value)
  text <- grep(paste0(pattern_code, "|="), text, value = TRUE)

  if (length(text) == 0L) {
    message("No intermediates found in log file. Returning NULL.")
    return(NULL)
  }

  textblocks <- kwb.utils::extractRowRanges(
    text,
    pattern = pattern_code,
    startOffset = 0L
  )

  # Function to get the number of elements matching a pattern
  n_matching <- function(x, pattern) length(grep(pattern, x))

  # In each block all but the first line must contain the equal sign
  stopifnot(all(
    lengths(textblocks) - 1L == sapply(textblocks, n_matching, "=")
  ))

  # Read the codes from the first lines of the text blocks
  code_lines <- sapply(textblocks, "[", 1L)

  # Name the list elements according to the codes
  names(textblocks) <- gsub(pattern_code, "", code_lines)

  # Convert the text blocks with first (= code) line excluded to matrices
  matrices <- lapply(textblocks, function(x) {
    matrix(
      data = unlist(strsplit(x[-1L], "=")),
      ncol = 2,
      byrow = TRUE,
      dimnames = list(NULL, c("variable", "value"))
    )
  })

  result <- kwb.utils::rbindAll(
    x = matrices,
    nameColumn = "code",
    namesAsFactor = FALSE
  )

  # Convert usage string to numeric (number of letter in alphabet)
  is_usage <- result$variable == "ut.usage"
  result$value[is_usage] <- match(result$value[is_usage], LETTERS)

  # Let "code" be the first column
  kwb.utils::moveColumnsToFront(result, "code")
}
