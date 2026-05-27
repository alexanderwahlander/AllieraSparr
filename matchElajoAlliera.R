# Libraries
library(dplyr)

# Load data
dR <- read.delim("RDBElajo.csv", sep = ",", check.names = FALSE, fileEncoding = "UTF-8", stringsAsFactors = FALSE, na.strings = c("", "NA"))
dA <- read.delim("AllieraReport.csv", sep = ";", check.names = FALSE, fileEncoding = "UTF-8", stringsAsFactors = FALSE, na.strings = c("", "NA"))

colnames(dR)[2] <- "Kortnamn"

# Match
# This filters only rows where Förnamn AND Efternamn AND Kortnamn match exactly
matched_df <- inner_join(dR, dA, by = c("Förnamn", "Efternamn", "Kortnamn"), relationship = "many-to-many")

# Define the list of extra columns we want to include
extra_cols <- c("Placering", "Behörighet", "Behörighet Placering", "Mifare4[DR]", "Mifare4-RCO[D9]", "MifareUID[DR]", 
                "MifareUID[XR]", "RCO[D9]", "Solid prox[D]", "UnknownRCO[D]", "UnknownRCOFixed[D9]")

# Initialize the third dataframe with the matching keys
match_csv <- matched_df %>% select(Förnamn, Efternamn, Kortnamn)

# Safely pull the extra columns into the new dataframe
# If a column exists in both files, it coalesces them (takes whichever is not null)
for (col in extra_cols) {
  col_x <- paste0(col, ".x")
  col_y <- paste0(col, ".y")
  
  if (col_x %in% colnames(matched_df) && col_y %in% colnames(matched_df)) {
    match_csv[[col]] <- coalesce(matched_df[[col_x]], matched_df[[col_y]])
  } else if (col %in% colnames(matched_df)) {
    match_csv[[col]] <- matched_df[[col]]
  } else if (col_x %in% colnames(matched_df)) {
    match_csv[[col]] <- matched_df[[col_x]]
  } else if (col_y %in% colnames(matched_df)) {
    match_csv[[col]] <- matched_df[[col_y]]
  } else {
    match_csv[[col]] <- NA # Filled with NA if missing from both files
  }
}

#match_csv <- match_csv %>% distinct()

#Remove duplicate rows
cardID_cols <- c("Mifare4[DR]", "Mifare4-RCO[D9]", "MifareUID[DR]",
               "MifareUID[XR]", "RCO[D9]", "Solid prox[D]",
               "UnknownRCO[D]", "UnknownRCOFixed[D9]")

for (col in cardID_cols) {
  match_csv <- match_csv %>%
    filter(is.na(.data[[col]]) | !duplicated(.data[[col]]))
}

write.csv(match_csv, "match.csv", row.names = FALSE)