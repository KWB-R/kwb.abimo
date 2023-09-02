# import any function of remotes, just to let R CMD Check not complain...
#' @importFrom remotes available_packages
NULL

# abimo_binary -----------------------------------------------------------------
abimo_binary <- function(tag = latest_abimo_version())
{
  formats <- list(
    Windows = "%s.exe",
    Linux = "%s",
    Darwin = "%s.app/Contents/MacOS/GNUSparseFile.0/%s"
  )

  file.path(
    extdata_file(),
    paste0("abimo_", tag, "_", get_architecture_suffix()),
    sprintf(kwb.utils::selectElements(formats, get_os_type()), "Abimo")
  )
}

# abimo_help -------------------------------------------------------------------
abimo_help <- function(...)
{
  run_abimo_command_line("--help", ...)
}

# abimo_version ----------------------------------------------------------------
abimo_version <- function(...)
{
  run_abimo_command_line("--version", ...)
}

# appendSubToFile --------------------------------------------------------------

#' Add "SUB" field to dbf-File
#'
#' Adds "SUB" field to the end of an existing  file, as expected by some
#' older applications (such as input-dbf-file for ABIMO)
#' function by grandmaster HAUKESON
#'
#' @param filename Path of file name of data.frame
#'
#' @return dbf file with sub field
#' @export
appendSubToFile <- function (filename)
{
  con <- file(filename, "ab")

  on.exit(close(con))

  writeBin(as.raw(0x1A), con)
}

# check_abimo_binary -----------------------------------------------------------
check_abimo_binary <- function(tag = latest_abimo_version())
{
  file <- abimo_binary(tag)

  if (!file.exists(file)) {
    install_abimo(tag)
  }

  if (!file.exists(file)) {
    kwb.utils::stopFormatted(
      "Could not install or find Abimo (no such file: %s)", file
    )
  }

  file
}

# default_config -----------------------------------------------------------------

#' Default ABIMO config.xml path
#'
#' @export
#' @examples
#' kwb.abimo::default_config()
default_config <- function()
{
  extdata_file("config.xml")
}

# get_bagrov_curves_from_abimo -------------------------------------------------

#' Get Bagrov curves from Abimo software
#'
#' Call Abimo with the --write-bagrov-table argument being set and convert the
#' console output to a data frame
#'
#' @return data frame with columns \code{P_over_Ep} (P/Ep), \code{Ea_over_Ep}
#'   (Ea/Ep), \code{effectivity} (n-value)
#' @export
#' @importFrom utils read.table
get_bagrov_curves_from_abimo <- function()
{
  # Let Abimo.exe create a table with data for the Bagrov curves
  abimo_output <- run_abimo_command_line("--write-bagrov-table")

  # Read the console output as if it was the content of a csv file
  data <- read.table(text = abimo_output, header = TRUE, sep = ",")

  kwb.utils::renameColumns(data, list(
    bag = "effectivity",
    x = "P_over_Ep",
    y = "Ea_over_Ep"
  ))
}

# latest_abimo_version ---------------------------------------------------------
latest_abimo_version <- function()
{
  "v3.3.0"
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
  command <- check_abimo_binary(tag)

  output <- try(system2(command, args = args, stdout = TRUE))

  if (kwb.utils::isTryError(output)) {
    stop(
      "system2() failed. Files below ", path, ":\n",
      paste(dir(path, recursive = TRUE), collapse = "\n")
    )
  }

  output
}

#' writes data.frame into ABIMO-dbf
#'
#' Saves an existing data.frame into dBase-format
#' and adds "SUB" field to the end of the file
#' as required by ABIMO
#'
#' @param df_name name of data.frame
#' @param new_dbf path of new ABIMO-input file to be written (.dbf)
#'
#' @return dbf file that can be processed by ABIMO
#' @importFrom foreign write.dbf
#' @export
write.dbf.abimo <- function (df_name, new_dbf)
{
  foreign::write.dbf(df_name, new_dbf)
  appendSubToFile(new_dbf)
}
