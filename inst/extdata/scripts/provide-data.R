#
# Provide the .dbf file that we received from Leilah Haag as dataset in the
# kwb.abimo package
#

# Path to .dbf file
file <- "C:/development/qt-projects/abimo/abimo/data/abimo_2019_mitstrassen.dbf"

# Read the .dbf file as a data frame. According to the documentation of the
# read.dbf() function, character columns are converted to factors
data <- foreign::read.dbf(kwb.utils::safePath(file))

# What column types do we have?
str(data)

# Which columns are factors now?
is_factor <- sapply(data, is.factor)

# Convert factor columns back to character and set the Encoding to "latin1"
data[is_factor] <- lapply(data[is_factor], function(x) {
  x <- as.character(x)
  Encoding(x) <- "latin1"
  x
})

# We should have only character, integer or numeric columns now
str(data)

# To avoid character encoding problems, replace special characters in "AGEB1"
ageb1_new <- kwb.utils::substSpecialChars(data$AGEB1, deuOnly = TRUE)

# What has changed?
unique(ageb1_new[ageb1_new != data$AGEB1])

# Update column with new text values
data$AGEB1 <- ageb1_new

# Should "AGEB1_NR" be of type integer?
unique(data$AGEB1_NR)

# I would say yes
data$AGEB1_NR <- as.integer(data$AGEB1_NR)

# Save modified data frame "data" as dataset "abimo_input_2019" in the package
abimo_input_2019 <- data

# Save data frame in package
usethis::use_data(abimo_input_2019, overwrite = TRUE)

# What is the file size of the saved data frame?
file.size("data/abimo_input_2019.rda")
