#' adapt ABIMO output dbf-file to Berlin shape file
#'
#' changes order in dbf-file to match geographical
#' shape file of Berlin "Stadtstruktur"
#'
#' @param ABIMO_out data.frame of ABIMO output file
#' @param file_georef path of dbf file that matches existing shape (incl. path)
#' @param out_file file path and file name for ordered ABIMO output file (to be linked to shape files)
#'
#' @return ordered dbf returned and written to out_file
#' @importFrom foreign write.dbf
#' @export
ABIMO_adapt_map <- function (
  ABIMO_out,
  file_georef,
  out_file
)
{
  #read ABIMO output file
  x <- ABIMO_out

  #read georeferenced dbf file
  y <- foreign::read.dbf(file = file_georef, as.is = TRUE)


  #match polygons
  index <- match(y$SCHLUESSEL, x$CODE)
  x.out <- x[index,]
  if(length(x$CODE) > length(y$SCHLUESSEL)) {
    print('Warning: some polygons without matching geometry')
  }

  #write dbf file
  foreign::write.dbf(dataframe = x.out, file = out_file)
  print(paste('ordered ABIMO file written to', out_file))

  x.out

}


#' join ABIMO in- and output files
#'
#' @param file_ABIMO_out path of ABIMO output file in dbf format (incl. path)
#' @param file_ABIMO_in path of ABIMO input file in dbf format (incl. path)
#'
#' @return data.frame with matched ABIMO in- and output data
#' @export
#'
abimo_comb_in_out <- function (
  file_ABIMO_out,
  file_ABIMO_in
)
{
  #read ABIMO output file
  x <- foreign::read.dbf(file = file_ABIMO_out, as.is = TRUE)

  #read ABIMO input file
  y <- foreign::read.dbf(file = file_ABIMO_in, as.is = TRUE)

  #same length?
  if(length(x$CODE) != length(y$CODE)) {
    stop('In- and output files do not match!')
  }

  #match order
  index <- match(y$CODE, x$CODE)
  x <- x[index,]

  #combine the two files
  x.out <- cbind(y, x[,-1])


  x.out

}

#' add ISU5 ID to dbf file from geoportal
#'
#' the ID is an unambiguous identifier
#' for all blocks of the Berlin Geoportal
#' but is usually hidden
#'
#' @param x_no_ID data.frame created from dbf file, downloaded from Berlin geoportal
#' @param ID_dbf path of dbf file including ids, default is set to local folder with ISU5-Ids
#'
#' @return data.frame of x_no_ID with a new column "ID"
#'
#' @importFrom foreign read.dbf
#' @export
add_ISU5_ID <- function (
  x_no_ID,
  ID_dbf = "C:/Aendu_lokal/ABIMO_Paper/Daten/Karten/Basis_ISU5_Daten_2015/ISU5_ID.dbf"
)
{
  #read ISU5 dbf-file
  x_ID <- foreign::read.dbf(file = ID_dbf, as.is = TRUE)

  #combine files
  y <- cbind(x_ID, x_no_ID)

  y

}

#' Compares ABIMO-output-file to reference (or other ABIMO output file)
#'
#' Compares two ABIMO-output-files by
#' plotting parameters compared to 1:1 line into pdf-File and by
#' doing a simple column statistics.
#'
#' @param x_reference reference data frame with ABIMO output (can be ABIMO output or downloaded from Berlin geoportal)
#' @param x_new new ABIMO output to be compared to reference
#'
#' @return data.frame of column statistics; plots and evaluation open as pdf
#'
#' @importFrom kwb.utils preparePdf hsShowPdf
#' @importFrom gridExtra grid.table
#' @importFrom grDevices dev.off
#' @importFrom graphics lines
#' @export
abimo_compare_output <- function (
  x_reference,
  x_new
)
{
  #Ordnen nach CODE
  index <- match(x_reference$CODE, x_new$CODE)
  x_new <- x_new[index,]

  #Reduktion auf Vergleichs-Spalten
  comp_names <- c("VERDUNSTUN", "ROW", "RI")
  x_reference <- x_reference[,comp_names]
  x_new <- x_new[,comp_names]

  #Berechnen der absoluten und prozentualen Differenz aller numerischen Spalten
  x_diff <- x_reference-x_new
  x_diff_perc <- abs(x_diff)/x_reference*100

  #Einfache Spalten-Statistik durchf?hren
  avg_reference <- colMeans(x_reference)
  avg_new <- colMeans(x_new)
  avg_diff <- colMeans(abs(x_diff))
  avg_perc <- colMeans(x_diff_perc, na.rm = TRUE)
  max_diff_perc <- (1:3)
  for(i in 1:3) {
    max_diff_perc[i] <- max(x_diff_perc[,i], na.rm = TRUE)
  }

  diff_tab <- data.frame(name=names(x_new), avg_Geoportal=avg_reference, avg_KWB=avg_new,
                         avg_diff_betrag=avg_diff, avg_diff_percent=avg_perc, row.names = NULL)

  #results in pdf
  pdfFile <- file.path(tempdir(), "pdf_compare_abimo.pdf")
  kwb.utils::preparePdf(pdfFile)

  #table
  gridExtra::grid.table(diff_tab)

  #old vs new Plot
  for (comp in comp_names) {
    graphics::plot(x = x_reference[[comp]], y = x_new[[comp]], main = comp, xlab = "reference [mm]", ylab = "new run [mm]")
    max_value <- max(c(x_reference[[comp]], x_new[[comp]]))
    graphics::lines(x = (0:max_value), y = (0:max_value), col = "red")
  }

  grDevices::dev.off()

  kwb.utils::hsShowPdf(pdfFile)

  #output table
  diff_tab

}


#' Reads two ABIMO output files
#' @description Reads a new ABIMO output file in dbase format. In addition the
#' original SENSTADTUM output file is read and made comparable. Alternatively
#' two new ABIMO output files can be read. Output are two comparable (same
#' dimensions and column names) data frames.
#' @param SENSTADTUM_dbf path of original SENSTADTUM-database
#' @param new_dbf path of new output-database
#'
#' @return output are two comparable (same dimensions and column names) data frames.
#' @export

ABIMO_read_output <- function (SENSTADTUM_dbf, new_dbf) {
  #dbase ABIMO Output Files Laden
  x_original <- foreign::read.dbf(SENSTADTUM_dbf)
  x_out <- foreign::read.dbf(new_dbf)

  #Vergleich ob CODE uebereinstimmend
  verify_CODE <- all(x_original$CODE == x_out$CODE)
  if(verify_CODE == TRUE) cat("CODE-Felder stimmen ueberein") else cat("CODE-Felder stimmen nicht ueberein! Ergebnisse nicht verwendbar!")

  #Reduktion der Original-Daten auf Output-Spalten
  colnames <- names(x_out)
  x_original_out <- x_original[,colnames]

  #Runden auf eine Dezimalstelle (um vergleichbar mit SenStadtUm-Original zu sein), aufrunden bei *.50
  x_out[,2:9] <- x_out[,2:9] + 0.00000001
  x_out[,2:9] <- round(x_out[,2:9],1)

  ABIMO_out <- list(SENSTADTUM = x_original_out, NEW = x_out)
}


#' calculate groundwater recharge and interflow
#'
#' uses correction factor to calculate groundwater recharge
#' from infiltration RI. Difference is interflow.
#' Requires a combined data.frame of ABIMO output and input,
#' e.g. by using function \code{\link{abimo_comb_in_out}}
#'
#' @param abimo_df data.frame of ABIMO output file, merged with input file
#'
#' @return input data.frame with two new columns "RI_K" and "INTERF"
#' @export
abimo_grwater_interflow <- function (
  abimo_df
)
{

  #calculate groundwater recharge
  abimo_df$RI_K <- abimo_df$RI * abimo_df$KOR_FL_N

  #calculate interflow
  abimo_df$INTERF <- abimo_df$RI - abimo_df$RI_K

  abimo_df

}


#' change "BERtoZero"-settings in Abimo config.xml to true
#'
#' as default irrigation of pervious areas
#' is assumed based on "Nutzung" and "Typ".
#' This function turns of irrigation for all areas
#' (BERtoZero = true)
#'
#' @param file_in path and file name of abimo xml-input file, default is
#' \code{\link{default_config}}
#' @param file_out path and file name to write changed abimo xml-input file
#' @param line_BER line number in xml-file, where BERtoZero is defined, default is 56
#'
#' @return abimo xml-input file with changed BERtoZero-setting
#' @export
abimo_xml_BER <- function (
  file_in = default_config(),
  file_out,
  line_BER = 56
)
{
  #read abimo xml file as lines
  textlines <- readLines(file_in)

  #change BER setting
  textlines[line_BER] <- gsub(pattern = "false", replacement = "true", textlines[line_BER])

  writeLines(textlines, con = file_out)
}

#' Helper function: replace value
#'
#' @description searches string for parameter=<pattern_value> and replaces with
#' parameter=<new_value> for all found entries
#'
#' @param string string with ABIMO config
#' @param new_value new parameter value
#' @param parameter parameter name to search for (default: "etp")
#' @param pattern_value pattern of value field (default: '\"\[0-9\]+?\\.?\[0-9\]+?\"')
#' @return returns string with modified parameter = value
#' @export
#' @examples
#' ### Simple string
#' string <- '<item bezirke="15,16,18,19,20" etp="807" etps="600" />'
#' replace_value(string, new_value = 100, parameter = "etp")
#' replace_value(string, new_value = 100, parameter = "etps")
#' ### Default ABIMO config
#' config <- readLines(kwb.abimo::default_config())
#' replace_value(config, new_value = 100, parameter = "etp")
replace_value <- function(
    string,
    new_value,
    parameter = "etp",
    pattern_value = "\"[0-9]+?\\.?[0-9]+?\""
)
{
  gsub(
    pattern = sprintf("%s=%s", parameter, pattern_value),
    replacement = sprintf("%s=\"%s\"", parameter, new_value),
    x = string
  )
}

#' change potential evaporation in Abimo config.xml
#'
#' potential evaporation (annual and summer)
#' is a boundary condition defined in config.xml.
#' This function sets potential evaporation
#' to a given value for all surfaces (except lakes and rivers)
#'
#' @param file_in path and file name of abimo xml-input file, default is
#' \code{\link{default_config}}
#' @param file_out path and file name to write changed abimo xml-input file
#' @param evap_annual annual potential evaporation
#' @param evap_summer potential evaporation for summer months
#'
#' @return abimo xml-input file with changed potential evaporation
#' @export
abimo_xml_evap <- function (
  file_in = default_config(),
  file_out,
  evap_annual,
  evap_summer
)
{
  #read abimo xml file as lines
  textlines <- readLines(file_in)

  #change evap settings
  textlines <- replace_value(textlines,
                            new_value = evap_annual,
                            parameter = "etp")

  textlines <- replace_value(textlines,
                            new_value = evap_summer,
                            parameter = "etps")

  writeLines(textlines, con = file_out)
}


#' calculate Berlin average of water balance terms
#'
#' multiplies each water balance component
#' by area, sums them up and divides the sum
#' by the total surface
#'
#' @param abimo_df data.frame of ABIMO output file, merged with input file
#'
#' @return table with averages in mm of water balance components
#' @export
abimo_Berlin_average <- function (
  abimo_df
)
{

  #output table
  x_out <- data.frame("evaporation" = NA,
                      "infiltration" = NA,
                      "interflow" = NA,
                      "runoff" = NA)

  #calculate volumes by BTF
  abimo_df$evaporation <- abimo_df$VERDUNSTUN * abimo_df$FLAECHE
  abimo_df$infiltration <- abimo_df$RI_K * abimo_df$FLAECHE
  abimo_df$interflow <- abimo_df$INTERF * abimo_df$FLAECHE
  abimo_df$runoff <- abimo_df$ROW * abimo_df$FLAECHE

  #averages
  for (comp_wb in colnames(x_out)) {

    x_out[[comp_wb]][1] <- sum(abimo_df[[comp_wb]]) / sum(abimo_df$FLAECHE)

  }

  x_out

}


