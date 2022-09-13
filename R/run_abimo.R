# run_abimo --------------------------------------------------------------------

#' Run Abimo with Input Data or Input File
#'
#' @param input_file path to input dbf file
#' @param input_data data frame from which a temporaryinput file is to be
#'   generated
#' @param output_file path to output file. By default the output file has the
#'   same name as the input file with "_result" appended
#' @param config_file optional. Path to config.xml file
#' @param config optional. Configuration object of class "abimoConfig", as
#'   returned by \code{create_configurator}. If given, \code{config_file} is
#'   ignored.
#' @param tag version tag of Abimo release to be used, see
#'   \url{https://github.com/KWB-R/abimo/releases}.
#' @return data frame, read from dbf file that was created by Abimo.exe
#' @export
run_abimo <- function(
  input_file = NULL, input_data = NULL, output_file = NULL, config_file = NULL,
  config = NULL, tag = latest_abimo_version()
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

  if (! is.null(config)) {
    stopifnot(inherits(config, "abimo_config"))
    if (! is.null(config_file)) {
      warning(
        "run_abimo(): 'config_file' is ignored as 'config' object is given!"
      )
    }
    timestring <- format(Sys.time(), "%Y%m%d-%H%M%S")
    file_name <- paste0("config_", timestring, ".xml")
    config_file <- config$save(file = file.path(tempdir(), file_name))
  }

  if (! check_abimo_binary(tag)) {
    stop("Could not install Abimo!")
  }

  output_file <- kwb.utils::defaultIfNULL(
    output_file, default_output_file(input_file)
  )

  args <- kwb.utils::fullWinPath(c(input_file, output_file))

  if (! is.null(config_file)) {
    args <- c(args, paste("--config", kwb.utils::fullWinPath(config_file)))
  }

  # TODO: Let Abimo.exe return non-failure exit codes!
  suppressWarnings(run_abimo_command_line(args, tag = tag))

  foreign::read.dbf(output_file)
}

# latest_abimo_version ---------------------------------------------------------
latest_abimo_version <- function()
{
  "v3.3.0"
}

# default_output_file ----------------------------------------------------------
default_output_file <- function(input_file)
{
  paste0(kwb.utils::removeExtension(input_file), "_result.dbf")
}

# run_abimo_command_line -------------------------------------------------------

#' Run Abimo on the Command Line
#'
#' @param args vector of arguments to be passed to Abimo
#' @param tag version tag of Abimo release to be used, see
#'   \url{https://github.com/KWB-R/abimo/releases}
#' @return The function returns what Abimo.exe sent to the standard output (as a
#'   vector of character).
#' @export
run_abimo_command_line <- function(args, tag = latest_abimo_version())
{
  output <- system2(abimo_binary(tag), args = args, stdout = TRUE)

  output
}

# abimo_help -------------------------------------------------------------------
abimo_help <- function()
{
  run_abimo_command_line("--help")
}

# abimo_version ----------------------------------------------------------------
abimo_version <- function()
{
  run_abimo_command_line("--version")
}
