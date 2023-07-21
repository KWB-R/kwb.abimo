# import any function of remotes, just to let R CMD Check not complain...
#' @importFrom remotes available_packages
NULL

# abimo_binary -----------------------------------------------------------------
abimo_binary <- function(tag = latest_abimo_version())
{
  file.path(extdata_file(), paste0("abimo_", tag, "_win64"), "Abimo.exe")
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
  output <- system2(abimo_binary(tag), args = args, stdout = TRUE)

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
