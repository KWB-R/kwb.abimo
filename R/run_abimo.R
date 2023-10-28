# run_abimo --------------------------------------------------------------------

#' Run Abimo with Input Data or Input File
#'
#' @param input_file path to input dbf file
#' @param input_data data frame from which a temporary input file is to be
#'   generated
#' @param output_file path to output file. By default the output file has the
#'   same name as the input file with "_result" appended
#' @param config_file optional. Path to config.xml file
#' @param config optional. Configuration object of class "abimoConfig", as
#'   returned by \code{create_configurator}. If given, \code{config_file} is
#'   ignored.
#' @param tag version tag of Abimo release to be used, see
#'   \url{https://github.com/KWB-R/abimo/releases}.
#' @param read_intermediates if \code{TRUE} the values of intermediate variables
#'   are read from the log file (if applicable). The default is \code{FALSE}.
#' @return data frame, read from dbf file that was created by Abimo.exe.
#'   If \code{read_intermediates} is \code{TRUE}, intermediate results are
#'   returned in the attribute "intermediates"
#' @export
run_abimo <- function(
  input_file = NULL,
  input_data = NULL,
  output_file = NULL,
  config_file = NULL,
  config = NULL,
  tag = latest_abimo_version(),
  read_intermediates = FALSE
)
{
  if (is.null(input_file) && is.null(input_data)) {
    stop("Either input_file or input_data must be given")
  }

  if (is.null(input_file)) {

    check_types(input_data)

    input_file <- file.path(tempdir(), "abimo_input.dbf")

    write.dbf.abimo(input_data, input_file)
  }

  if (!is.null(config)) {

    stopifnot(inherits(config, "abimo_config"))

    if (!is.null(config_file)) {
      warning(
        "run_abimo(): 'config_file' is ignored as 'config' object is given!"
      )
    }

    file_name <- format(Sys.time(), "config_%Y%m%d-%H%M%S.xml")

    config_file <- config$save(file = file.path(tempdir(), file_name))
  }

  output_file <- kwb.utils::defaultIfNULL(
    output_file, default_output_file(input_file)
  )

  args <- full_quoted_path(c(input_file, output_file))

  if (!is.null(config_file)) {
    args <- c(args, paste("--config", full_quoted_path(config_file)))
  }

  # TODO: Let Abimo.exe return non-failure exit codes!
  suppressWarnings(run_abimo_command_line(args, tag = tag))

  result <- foreign::read.dbf(output_file)

  # Read intermediate results from the log file
  intermediates <- if (read_intermediates) {
    catAndRun(
      "Reading intermediate results from ABIMO log file",
      read_abimo_intermediate_results_from_log(
        file = kwb.utils::replaceFileExtension(output_file, ".log")
      )
    )
  } # else NULL

  # Return the intermediate results as an attribute
  structure(result, intermediates = intermediates)
}

# default_output_file ----------------------------------------------------------

#' @importFrom kwb.utils replaceFileExtension
default_output_file <- function(input_file)
{
  kwb.utils::replaceFileExtension(input_file, "_result.dbf")
}

# full_quoted_path -------------------------------------------------------------
full_quoted_path <- function(x)
{
  # E.g. replace leading tilde by home directory
  x <- path.expand(x)

  # Convert slashes to backslashes on windows
  if (on_windows()) {
    x <- kwb.utils::windowsPath(x)
  }

  # Surround paths in double quotes just in case they contain spaces
  paste0('"', x, '"')
}
