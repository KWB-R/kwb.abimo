# read_config ------------------------------------------------------------------
read_config <- function(file = default_config_file())
{
  dive_into(x = xml2::read_xml(file), file)
}

# dive_into --------------------------------------------------------------------
dive_into <- function(x, xml_file, depth = 0L)
{
  #children <- xml_children(x)
  children <- get_named_children(x)

  if (length(children) == 0L) {

    x$get <- function(name = NULL) {
      attrs <- xml2::xml_attrs(x)
      if (is.null(name)) {
        return(attrs)
      }
      kwb.utils::selectElements(as.list(attrs), name)
    }

    x$set <- function(...) {
      args <- list(...)
      for (name in names(args)) {
        xml2::xml_attr(x, name) <- args[[name]]
      }
    }

    return(x)
  }

  result <- lapply(children, dive_into, xml_file, depth = depth + 1L)

  result$remove <- function(name) {
    xml2::xml_remove(kwb.utils::selectElements(children, name))
  }

  result$get_xml <- function() {
    x
  }

  if (depth == 0L) {

    result$update = function() {
      dive_into(x, xml_file)
    }

    result$save = function(name, file = NULL) {
      save_config(x, xml_file, name, file)
    }

    class(result) <- "abimo_config"
  }

  result
}

# get_named_children -----------------------------------------------------------
get_named_children <- function(x)
{
  children <- xml2::xml_children(x)

  # Node names
  node_names <- sapply(children, xml2::xml_name)

  node_names <- append_suffix_from_attribute(node_names, children, "name")
  node_names <- append_suffix_from_attribute(node_names, children, "key")

  node_names <- kwb.utils::makeUnique(
    node_names, warn = FALSE, sep = "_", simple = TRUE
  )

  # Return list of children with determined names
  stats::setNames(lapply(children, identity), node_names)
}

# append_suffix_from_attribute -------------------------------------------------
append_suffix_from_attribute <- function(x, children, attr_name)
{
  # attribute values
  attr_values <- sapply(children, xml2::xml_attr, attr_name)

  # Which nodes have the requested attribute?
  has_attr <- ! is.na(attr_values)

  # Add suffix "_<attr_value>" to nodes with requested attribute
  x[has_attr] <- paste0(x[has_attr], "_", attr_values[has_attr])

  x
}
