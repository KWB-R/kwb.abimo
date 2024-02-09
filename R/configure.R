# create_configurator ----------------------------------------------------------

#' Create Configuration Object from Abimo Configuration File
#'
#' @param xml_file path to "config.xml". By default a configuration file that is
#' stored in this package (\code{system.file("extdata/config.xml")}) is used.
#' @return object of class "abimo_config"
#' @importFrom xml2 read_xml write_xml xml_attr xml_find_all xml_replace
#' @export
create_configurator <- function(xml_file = NULL)
{
  xml_file <- kwb.utils::defaultIfNULL(xml_file, default_config())

  x <- xml2::read_xml(xml_file)

  safely_get_node <- function(xpath) {
    nodeset <- xml2::xml_find_all(x, xpath)
    stopifnot(length(nodeset) == 1L)
    nodeset[[1L]]
  }

  safely_get_at <- function(nodes, i) {
    stopifnot(i %in% seq_along(nodes))
    nodes[[i]]
  }

  get_nodes <- function(xpath) {
    xml2::xml_find_all(x, xpath)
  }

  get_node <- function(xpath, i) {
    safely_get_at(get_nodes(xpath), i)
  }

  set_node <- function(xpath, i, new_node) {
    nodes <- xml2::xml_find_all(x, xpath)
    safely_get_at(nodes, i)
    xml2::xml_replace(nodes[[i]], new_node)
  }

  # Return a list of functions that allow to inspect/modify the xml tree x
  structure(class = "abimo_config", list(
    get = function(xpath) {
      node <- safely_get_node(xpath)
      xml2::xml_attr(node, "value")
    },

    set = function(xpath, value) {
      node <- safely_get_node(xpath)
      xml2::xml_attr(node, "value") <- value
    },

    get_nodes = get_nodes,

    get_node = get_node,

    set_node = set_node,

    modify_node = function(xpath, i, ...) {
      set_node(xpath, i, set_node_attributes(get_node(xpath, i), ...))
    },

    # Return the full xml tree
    get_config = function() {
      x
    },

    save = function(name, file = NULL) {
      save_config(x, xml_file, name, file)
    }
  ))
}

# default_config_file ----------------------------------------------------------
default_config_file <- function()
{
  kwb.utils::warningDeprecated(
    "default_config_file",
    "default_config"
  )
}

# default_config ---------------------------------------------------------------

#' Default ABIMO config.xml path
#'
#' @export
#' @examples
#' kwb.abimo::default_config()
default_config <- function()
{
  extdata_file("config.xml")
}

# get_xpaths -------------------------------------------------------------------

#' Return List of XPath Expressions to Address Config Elements
#'
#' @return List structure containing XPath expressions (see e.g.
#'   \url{https://www.w3schools.com/xml/xpath_intro.asp})
#' @export
get_xpaths <- function()
{
  area_types <- c(
    "Dachflaechen",
    "Belaglsklasse1",
    "Belaglsklasse2",
    "Belaglsklasse3",
    "Belaglsklasse4"
  )

  output_columns <- c(
    "R",
    "ROW",
    "RI",
    "RVOL",
    "ROWVOL",
    "RIVOL",
    "FLAECHE",
    "VERDUNSTUNG"
  )

  diverse <- c(
    "BERtoZero",
    "NIEDKORRF"
  )

  # Helper functions
  xpath_section_item <- function(section) {
    sprintf("//section[@name='%s']/item", section)
  }

  xpath_key_in_section_item <- function(section, key) {
    paste0(xpath_section_item(section), sprintf("[@key='%s']", key))
  }

  apply_named <- function(x, fun, ...) {
    lapply(stats::setNames(nm = x), fun, ...)
  }

  list(
    Infiltrationsfaktoren = apply_named(
      area_types,
      xpath_key_in_section_item,
      section = "Infiltrationsfaktoren"
    ),
    Bagrovwerte = apply_named(
      area_types,
      xpath_key_in_section_item,
      section = "Bagrovwerte"
    ),
    ErgebnisNachkommaStellen = apply_named(
      output_columns,
      xpath_key_in_section_item,
      section = "ErgebnisNachkommaStellen"
    ),
    Gewaesserverdunstung = xpath_section_item("Gewaesserverdunstung"),
    PotentielleVerdunstung = xpath_section_item("PotentielleVerdunstung"),
    Diverse = apply_named(
      diverse,
      xpath_key_in_section_item,
      section = "Diverse"
    )
  )
}

# set_node_attributes ----------------------------------------------------------
set_node_attributes <- function(node, ...)
{
  args <- list(...)

  for (name in names(args)) {
    xml2::xml_attr(node, name) <- args[[name]]
  }

  node
}

# save_config ------------------------------------------------------------------
save_config <- function(x, xml_file, name, file)
{
  file <- kwb.utils::defaultIfNULL(file, file.path(dirname(xml_file), name))
  kwb.utils::catAndRun(paste("Writing", file), xml2::write_xml(x, file))
  file
}
