# read_intermediate_results_from_log -------------------------------------------

#' Read Intermediate Results from Log File
#'
#' @param file path to log file
read_intermediate_results_from_log <- function(
    file = file.path(tempdir(), "abimo_input_result.log")
)
{
  raw_text <- readLines(file)

  # Remove informative lines or empty lines
  # e.g. "Nutzungstyp nicht definiert fuer Element"
  pattern_remove <- "Start|unknown|unbekannt|Nutzungstyp|angenommen|^$"
  pattern_code <- "^\\*\\*\\* Code: "

  text <- grep(pattern_remove, raw_text, value = TRUE, invert = TRUE)

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
  stopifnot(all(lengths(textblocks) == sapply(textblocks, n_matching, "=")))

  # Read the codes from the first lines of the text blocks
  code_lines <- sapply(textblocks, "[", 1L)

  # Name the list elements according to the codes
  names(textblocks) <- gsub(pattern, "", code_lines)

  # Convert the text blocks with first (= code) line excluded to matrices
  matrices <- lapply(textblocks, function(x) {
    matrix(
      data = unlist(strsplit(x[-1L], "=")),
      ncol = 2,
      byrow = TRUE,
      dimnames = list(NULL, c("variable", "value"))
    )
  })

  result <- kwb.utils::rbindAll(matrices, nameColumn = "code")

  kwb.utils::moveColumnsToFront(result, "code")
}
