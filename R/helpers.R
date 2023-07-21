# abimo_binary -----------------------------------------------------------------
abimo_binary <- function(tag = latest_abimo_version())
{
  file.path(extdata_file(), paste0("abimo_", tag, "_win64"), "Abimo.exe")
}

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

# latest_abimo_version ---------------------------------------------------------
latest_abimo_version <- function()
{
  "v3.3.0"
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
