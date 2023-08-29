# extdata_file -----------------------------------------------------------------

#' Get Path to File in This Package
#'
#' @inheritParams kwb.utils::extdataFile
#' @export
extdata_file <- kwb.utils::createFunctionExtdataFile("kwb.abimo")

# get_architecture_suffix ------------------------------------------------------
get_architecture_suffix <- function()
{
  if (on_windows() && on_64bit()) {
      return("win64")
  }

  if (!on_windows() && on_64bit()) {
    return("unix64")
  }

  kwb.utils::printIf(TRUE, .Platform)
  kwb.utils::printIf(TRUE, R.version)

  stop(
    "Unexpected platform type and architecture. ",
    "Expected: one of 'win64', 'unix64'",
    call. = FALSE
  )
}

# get_os_architecture ----------------------------------------------------------
get_os_architecture <- function()
{
  kwb.utils::selectElements(R.version, "arch")
}

# get_os_type ------------------------------------------------------------------
get_os_type <- function()
{
  kwb.utils::selectElements(.Platform, "OS.type")
}

# on_64bit ---------------------------------------------------------------------
on_64bit <- function()
{
  get_os_architecture() == "x86_64"
}

# on_windows -------------------------------------------------------------------
on_windows <- function()
{
  get_os_type() == "windows"
}
