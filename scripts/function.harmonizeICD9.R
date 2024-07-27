library(stringr)

#' Harmonize ICD-9 Codes
#'
#' This function standardizes ICD-9 codes by ensuring they follow a consistent format.
#' @param icd A character vector representing the ICD-9 codes to be harmonized.
#' @return A character vector representing the harmonized ICD-9 codes, or NA if the input is invalid.
#' @details The function handles various cases such as codes with prefixes, suffixes, and missing values.
harmonize_icd9 <- function(icd) {
  # Replace invalid entries with NA
  valid <- !is.na(icd) & icd != "" & icd != "NA"
  icd[!valid] <- NA
  
  # Handle prefixes and determine the number of digits before the decimal
  prefix <- str_extract(icd, "^[A-Z]+")
  # Ensure nums is always valid; default to 3 unless prefix is specifically 'V', then 2
  nums <- ifelse(!is.na(prefix) & prefix == "V", 2, 3)
  
  # Remove the prefix for further processing
  icd <- str_remove(icd, "^[A-Z]+")

  # Detect and handle suffixes
  suffix <- str_extract(icd, "[A-Za-z]+$")
  icd <- str_remove(icd, "[A-Za-z]+$")

  # Check if a decimal should be added and add if necessary
  icd <- ifelse(nchar(icd) > nums & !str_detect(icd, "\\."), 
                str_replace(icd, paste0("^(\\d{", nums, "})", "(\\d+)"), "\\1.\\2"), icd)

  # Extract and format the numeric part of the code
  icd1 <- str_pad(str_extract(icd, "^\\d+"), nums, "left", pad = "0")
  icd2 <- str_extract(icd, "(?<=\\.)\\d+")

  # Recombine all parts to form the harmonized ICD code, ensuring no NA parts contribute
  result_icd <- ifelse(is.na(prefix), "", prefix) %>% 
                str_c(ifelse(is.na(icd1), "", icd1), sep = "") %>%
                str_c(ifelse(is.na(icd2), "", paste0(".", icd2)), sep = "") %>% 
                str_c(ifelse(is.na(suffix), "", suffix), sep = "")

  # Ensure result is NA where original was invalid
  result_icd[!valid] <- NA
  return(result_icd)
}

# Example usage of the harmonize_icd9 function
harmonize_icd9(c("V123", "1234A", "V123A", "456.78", "", NA, "NA"))
