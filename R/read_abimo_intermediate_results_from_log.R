# read_abimo_intermediate_results_from_log -------------------------------------

#' Read Intermediate Results from Log File
#'
#' @param file path to log file
read_abimo_intermediate_results_from_log <- function(
    file = file.path(tempdir(), "abimo_input_result.log")
)
{
  text <- readLines(file)

  # Remove informative lines
  text <- grep("unknown", text, value = TRUE, invert = TRUE)

  pattern <- "^\\*\\*\\* Code: "

  starts <- grep(pattern, text)
  ends <- kwb.utils::startsToEnds(starts, lastStop = length(text))

  textblocks <- lapply(
    X = mapply(seq.int, starts, ends),
    FUN = function(indices) text[indices]
  )

  result <- lapply(textblocks, function(x) {
    as.data.frame(matrix(
      ncol = 2,
      byrow = TRUE,
      unlist(strsplit(grep("=", x, value = TRUE), "="))
    ))
  })

  names(result) <- gsub(pattern, "code_", text[starts])

  result
}
