# extdata_file -----------------------------------------------------------------

#' Get Path to File in This Package
#'
#' @inheritParams kwb.utils::extdataFile
#' @export
extdata_file <- kwb.utils::createFunctionExtdataFile("kwb.abimo")

# get_architecture_suffix ------------------------------------------------------
get_architecture_suffix <- function()
{
  suffixes <- list(
    Linux = "linux",
    Darwin = "macos",
    Windows = "windows"
  )

  kwb.utils::selectElements(suffixes, get_os_type())
}

# get_os_type ------------------------------------------------------------------
get_os_type <- function()
{
  os_type <- Sys.info()[["sysname"]]

  stopifnot(os_type %in% c("Windows", "Linux", "Darwin"))

  os_type
}

# on_linux ---------------------------------------------------------------------
on_linux <- function()
{
  get_os_type() == "Linux"
}

# on_macos ---------------------------------------------------------------------
on_macos <- function()
{
  get_os_type() == "Darwin"
}

# on_windows -------------------------------------------------------------------
on_windows <- function()
{
  get_os_type() == "Windows"
}
