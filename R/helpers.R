# import any function of remotes, just to let R CMD Check not complain...
#' @importFrom remotes available_packages
NULL

# abimo_binary -----------------------------------------------------------------
abimo_binary <- function(tag = latest_abimo_version())
{
  name <- "Abimo"

  executables <- list(
    Windows = paste0(name, ".exe"),
    Linux = name,
    Darwin = paste0(name, ".app/Contents/MacOS/GNUSparseFile.0/", name)
  )

  file.path(
    extdata_file(),
    paste0("abimo_", tag, "_", get_architecture_suffix()),
    kwb.utils::selectElements(executables, get_os_type())
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

# github_pat -------------------------------------------------------------------
# Provide non-exported function github_pat() from package remotes
#' @importFrom utils getFromNamespace
github_pat <- utils::getFromNamespace("github_pat", "remotes")

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
#' @importFrom kwb.utils isTryError printIf
#' @export
run_abimo_command_line <- function(args, tag = latest_abimo_version())
{
  command <- check_abimo_binary(tag)

  output <- try(system2(command, args = args, stdout = TRUE))

  if (kwb.utils::isTryError(output)) {

    kwb.utils::printIf(TRUE, command)
    cat("\n")
    kwb.utils::printIf(TRUE, args)

    path <- dirname(command)

    stop(
      "system2() failed (see above for arguments). Files below ", path, ":\n",
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
  na_counts <- sapply(df_name, kwb.utils::nNA)
  is_double <- sapply(df_name, is.double)

  may_cause_warnings <- is_double & (na_counts > 0L)

  if (any(may_cause_warnings)) {
    message(
      "foreign::write.dbf() may cause warnings due to NA values in the ",
      "following numeric columns: ",
      paste(collapse = ", ", sprintf(
        "\"%s\" (%d-times)",
        names(which(may_cause_warnings)),
        na_counts[may_cause_warnings]
      ))
    )
  }

  foreign::write.dbf(df_name, new_dbf)

  appendSubToFile(new_dbf)
}
