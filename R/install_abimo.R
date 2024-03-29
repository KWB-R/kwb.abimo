# install_abimo ----------------------------------------------------------------

#' Install ABIMO
#'
#' @param tag tag of ABIMO version to be installed, e.g. "v3.2.2"
#' @param arch target system architecture, one of "windows", "linux", "macos"
#' @param \dots further arguments passed to \code{kwb.abimo:::download_assets}
#' @importFrom archive archive_extract
#' @importFrom kwb.utils catAndRun createDirectory stringList
#' @export
install_abimo <- function(
    tag = latest_abimo_version(),
    arch = get_architecture_suffix(),
    ...
)
{
  expected_architectures <- c("windows", "linux", "macos")

  if (!arch %in% expected_architectures) {
    stop(
      "Currently, the abimo executable is only available for one of these ",
      "'architectures': ",
      kwb.utils::stringList(expected_architectures),
      call. = FALSE
    )
  }

  exdir <- dirname(abimo_binary(tag))

  kwb.utils::catAndRun(paste("Installing Abimo to", exdir), {

    # Download abimo executable and dependencies in zip file
    zip_files <- download_assets(
      repo = "KWB-R/abimo",
      tag = tag,
      pattern = sprintf("abimo_%s_%s\\.", tag, arch),
      ...
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

# download_assets --------------------------------------------------------------

#' @importFrom utils download.file
download_assets <- function(
    repo,
    tag,
    destdir = tempdir(),
    pattern = NULL,
    accept = "application/octet-stream",
    timeout = getOption("timeout"),
    token = github_pat()
)
{
  old_options <- options(timeout = timeout)
  on.exit(options(old_options))

  asset_info <- get_asset_info(repo, tag, token = token)

  if (!is.null(pattern)) {
    asset_info <- asset_info[grepl(pattern, asset_info$name), ]
  }

  for (i in seq_len(nrow(asset_info))) {

    utils::download.file(
      url = asset_info$url[i],
      destfile = file.path(destdir, asset_info$name[i]),
      headers = c(
        Authorization = paste("token", token),
        Accept = accept
      ),
      mode = "wb"
    )
  }

  file.path(destdir, asset_info$name)
}

# get_asset_info ---------------------------------------------------------------

#' @importFrom gh gh
#' @importFrom kwb.utils asNoFactorDataFrame selectElements
get_asset_info <- function(repo, tag, token = github_pat())
{
  url_releases <- sprintf("https://api.github.com/repos/%s/releases", repo)

  release_info <- gh::gh(url_releases, .token = token)

  tag_names <- sapply(release_info, kwb.utils::selectElements, "tag_name")

  match.arg(tag, tag_names)

  assets <- kwb.utils::selectElements(
    release_info[[which(tag == tag_names)]],
    "assets"
  )

  if (!length(assets)) {
    stop("There are no assets for release ", tag, call. = FALSE)
  }

  do.call(rbind, lapply(assets, function(asset) {
    kwb.utils::asNoFactorDataFrame(
      kwb.utils::selectElements(asset, c("name", "url"))
    )
  }))
}
