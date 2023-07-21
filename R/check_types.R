# check_types ------------------------------------------------------------------
check_types <- function(abimo_data)
{
  # The following lines were used to help create the code below
  #types <- sapply(kwb.abimo::abimo_input_2019, typeof)
  #writeLines(sprintf("%s = \"%s\",", names(types), types))

  expected_types <- list(
    CODE = "character",
    BEZIRK = "integer",
    STAGEB = "integer",
    BLOCK = "integer",
    TEILBLOCK = "integer",
    NUTZUNG = "integer",
    TYP = "integer",
    FLGES = "double",
    VG = "double",
    PROBAU = "double",
    PROVGU = "double",
    REGENJA = "integer",
    REGENSO = "integer",
    BELAG1 = "integer",
    BELAG2 = "integer",
    BELAG3 = "integer",
    BELAG4 = "integer",
    VGSTRASSE = "double",
    STR_BELAG1 = "double",
    STR_BELAG2 = "double",
    STR_BELAG3 = "double",
    STR_BELAG4 = "double",
    KANAL = "integer",
    KANART = "integer",
    KAN_BEB = "integer",
    KAN_VGU = "integer",
    KAN_STR = "integer",
    FELD_30 = "integer",
    FELD_150 = "integer",
    FLUR = "double",
    STR_FLGES = "double",
    KOR_FL_N = "double",
    AGEB1_NR = "integer",
    AGEB1 = "character"
  )

  for (column in names(abimo_data)) {

    expected <- kwb.utils::selectElements(expected_types, column)

    if (typeof(abimo_data[[column]]) != expected) {
      stop("Column '", column, "' does not have the expected type (",
           expected, ")!", call. = FALSE)
    }
  }
}
