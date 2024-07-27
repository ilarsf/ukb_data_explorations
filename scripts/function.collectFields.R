#' Collect and save field listings from a specified URL
#'
#' This function collects field listings from a specified URL, processes the data, and saves it to a file.
#'
#' @param base_url The base URL for the data source
#' @param vt_codes A character vector of codes to append to the base URL for different data tables
#' @param datatypes A character vector of datatypes corresponding to each vt_code
#' @param output_file The file path where the collected data will be saved
#' @return None. The function writes the collected data to the specified output file.
#' @import data.table
#' @import htmltab
#' @export
collect_and_save_field_listings <- function(base_url, vt_codes, datatypes, output_file) {
    # Ensure required libraries are loaded
    library(data.table)
    library(htmltab)

    # Initialize an empty list to store all tables
    all_tables <- list()

    # Loop through each vt_code to fetch and process the corresponding table
    for (i in seq_along(vt_codes)) {
        # Construct the full URL for the current vt_code
        table_url <- paste0(base_url, vt_codes[i])

        # Fetch the table from the URL and convert it to a data.table
        tableX <- data.table(htmltab(doc = table_url, which = 1))

        # Add the corresponding datatype to the table
        tableX$datatype <- datatypes[i]

        # Store the processed table in the list
        all_tables[[i]] <- tableX
    }

    # Combine all tables into a single data.table
    all_tables <- rbindlist(all_tables)

    # Write the combined table to the specified output file
    fwrite(all_tables, output_file, sep = "\t")
}

# Example usage of the collect_and_save_field_listings function
# example_base_url <- "https://biobank.ndph.ox.ac.uk/showcase/list.cgi?it=0&vt="
# example_vt_codes <- c("11", "21", "22", "31", "41", "51", "61", "101")
# example_datatypes <- c(
#   "Integer",
#   "Categorical (single)",
#   "Categorical (multiple)",
#   "Continuous",
#   "Text",
#   "Date",
#   "Time",
#   "Compound",
#   "unknown"
# )
# example_output_file <- "./data/FieldListing.txt"

# Uncomment the following line to run the example
# collect_and_save_field_listings(example_base_url, example_vt_codes, example_datatypes, example_output_file)
