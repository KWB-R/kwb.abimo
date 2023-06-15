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
install_abimo <- function(tag = latest_abimo_version(), arch = "win64")
{
  if (arch != "win64") {
    stop("Currently, the abimo executable is only available for win64")
  }

  exdir <- dirname(abimo_binary(tag))

  kwb.utils::catAndRun(paste("Installing Abimo to", exdir), {

    # Download abimo executable and dependencies in zip file
    #repo = "KWB-R/abimo"; tag = "v3.2.2"
    zip_files <- download_assets(
      repo = "KWB-R/abimo",
      tag = tag,
      pattern = sprintf("abimo_%s_%s\\.", tag, arch)
    )

    stopifnot(length(zip_files) == 1L)

    kwb.utils::createDirectory(exdir)

    archive::archive_extract(
      zip_files[1L],
      dir = exdir,
      strip_components = 1L
    )
  })

  invisible(exdir)
}

# import any function of remotes, just to let R CMD Check not complain...
#' @importFrom remotes available_packages
NULL

# download_assets -------------------------------------------------------

#' @importFrom utils download.file
download_assets <- function(
    repo,
    tag,
    destdir = tempdir(),
    pattern = NULL,
    accept = "application/octet-stream"
)
{
  asset_info <- get_asset_info(repo, tag)

  if (!is.null(pattern)) {
    asset_info <- asset_info[grepl(pattern, asset_info$name), ]
  }

  github_pat <- utils::getFromNamespace("github_pat", "remotes")

  for (i in seq_len(nrow(asset_info))) {

    utils::download.file(
      url = asset_info$url[i],
      destfile = file.path(destdir, asset_info$name[i]),
      headers = c(
        Authorization = paste("token", github_pat()),
        Accept = accept
      ),
      mode = "wb"
    )
  }

  file.path(destdir, asset_info$name)
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

  do.call(rbind, lapply(assets, function(asset) {
    kwb.utils::asNoFactorDataFrame(asset[c("name", "url")])
  }))
}
