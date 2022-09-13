# check_abimo_binary -----------------------------------------------------------
check_abimo_binary <- function(tag = latest_abimo_version())
{
  file <- abimo_binary(tag)

  if (file.exists(file)) {
    return(TRUE)
  }

  file.exists(install_abimo(tag))
}

# abimo_binary -----------------------------------------------------------------
abimo_binary <- function(tag = latest_abimo_version())
{
  file.path(extdata_file(), paste0("abimo_", tag, "_win64"), "Abimo.exe")
}

# install_abimo ----------------------------------------------------------------

#' @importFrom archive archive_extract
install_abimo <- function(tag = latest_abimo_version())
{
  exdir <- dirname(abimo_binary(tag))

  kwb.utils::catAndRun(paste("Installing Abimo to", exdir), {

    # Download abimo executable and dependencies in zip file
    #repo = "KWB-R/abimo"; tag = "v3.3.0"
    zip_file <- download_asset(repo = "KWB-R/abimo", tag = tag)

    kwb.utils::createDirectory(exdir)

    archive::archive_extract(zip_file, dir = exdir, strip_components = 1L)
  })

  invisible(exdir)
}

# import any function of remotes, just to let R CMD Check not complain...
#' @importFrom remotes available_packages
NULL

# download_asset ---------------------------------------------------------------

#' @importFrom utils download.file
download_asset <- function(repo, tag, destfile = NULL)
{
  asset_info <- get_asset_info(repo, tag)

  if (is.null(destfile)) {
    destfile <- file.path(
      tempdir(),
      kwb.utils::selectElements(asset_info, "name")
    )
  }

  github_pat <- utils::getFromNamespace("github_pat", "remotes")

  utils::download.file(
    kwb.utils::selectElements(asset_info, "url"),
    destfile,
    headers = c(
      Authorization = paste("token", github_pat()),
      Accept = "application/octet-stream"
    ),
    mode = "wb"
  )

  destfile
}

# get_asset_info ---------------------------------------------------------------

#' @importFrom gh gh
get_asset_info <- function(repo, tag)
{
  url_releases <- kwb.utils::resolve(
    "https://api.github.com/repos/<repo>/releases",
    repo = repo
  )

  release_info <- gh::gh(url_releases)

  tag_names <- sapply(release_info, "[[", "tag_name")

  match.arg(tag, tag_names)

  assets <- release_info[[which(tag == tag_names)]]$assets

  if (! length(assets)) {
    stop("There are no assets for release ", version)
  }

  asset <- assets[[1L]]

  kwb.utils::selectElements(asset, c("name", "url"))
}
