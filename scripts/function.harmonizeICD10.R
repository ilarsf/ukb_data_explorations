library(stringr)

#' Harmonize ICD-10 Code
#'
#' This function standardizes ICD-10 codes by ensuring they follow a consistent format.
#'
#' @param icd A character vector representing the ICD-10 codes to be harmonized.
#' @return A character vector representing the harmonized ICD-10 codes, or NA if the input is invalid.
#' @details The function processes the ICD-10 codes by:
#' - Removing any leading or trailing whitespace.
#' - Extracting and preserving any alphabetical prefix.
#' - Inserting a period after the first two digits if not already present.
#' - Handling any suffixes appropriately.
#' @examples
#' harmonize_icd10(c("A123", "B45.6", "C78D"))
harmonize_icd10 <- function(icd) {
  valid <- !is.na(icd) & icd != "" & icd != "NA"
  icd[!valid] <- NA
  
  icd <- str_trim(icd)
  prefix <- str_extract(icd, "^[A-Z]+")
  icd <- str_remove(icd, "^[A-Z]+")
  
  suffix <- str_extract(icd, "[A-Za-z]+[0-9]?$")
  icd <- str_remove(icd, "[A-Za-z]+[0-9]?$")

  # Insert period after the first two digits if not already present and more than two digits
  icd <- ifelse(nchar(icd) > 2 & !str_detect(icd, "\\."), 
                str_replace(icd, "^(\\d{2})(\\d+)", "\\1.\\2"), icd)

  # Reassemble the ICD code carefully checking for NA components
  result_icd <- ifelse(is.na(prefix), "", prefix) %>% 
                str_c(ifelse(is.na(icd), "", icd), sep = "") %>%
                str_c(ifelse(is.na(suffix), "", suffix), sep = "")

  result_icd[!valid] <- NA  # Reset NA where original was invalid
  return(result_icd)
}

# Example usage of the harmonize_icd10 function
# harmonize_icd10(c("A123", "B45.6", "C78D", "", NA, "NA"))
