#' Extract and reformat UKB data
#' 
#' This script contains functions to extract and reformat UKB data.
#' 
#' @import data.table
#' @import optparse
#' @import methods
#' @import intervals
#' @import httr
#' @import readr
#' @import parallel
#' @import RCurl
#' @import htmltidy
#' @import XML

# Load necessary libraries
suppressPackageStartupMessages({
    library(data.table)
    library(optparse)
    library(methods)
    library(intervals)
    library(httr)
    library(readr)
    library(parallel)
    library(RCurl)
    library(htmltidy)
    library(XML)
})

# Set the number of cores for parallel processing
n_cores <- ceiling(detectCores() / 4)
setDTthreads(n_cores)

# Load data tables with field descriptions
all_tables <- fread("./data/Fields_in_Available_Data.txt", header = TRUE)
all_basket_fields <- fread("./data/Fields_in_Available_Baskets.txt", sep = "\t")

# List of withdrawn files
file_withdrawn <- list.files("./data", "w.+.csv", full.name = TRUE)

#' Extract field data from UKB
#'
#' @param field_id The field ID to extract
#' @param basket The basket associated with the field
#' @param file_basket The file containing the basket data
#' @param subfield_ids The subfield IDs to extract
#' @param out_file The output file path
#' @param file_withdrawn The file containing withdrawn IDs
#' @param overwrite Whether to overwrite existing files
#' @return None
extract_field_data <- function(field_id, basket, file_basket, subfield_ids, out_file, file_withdrawn, overwrite = FALSE) {
    if (!file.exists(dirname(out_file))) dir.create(dirname(out_file), recursive = TRUE)
    if (file.exists(out_file) && !overwrite) {
        print("Existing File not overwritten")
        return()
    }
    setDTthreads(12)

    wids <- readLines(file_withdrawn)
    tempdata <- fread(file_basket, select = c("f.eid", strsplit(subfield_ids, ",")[[1]]), na.strings = "")
    tempdata <- tempdata[apply(tempdata[, -1], 1, function(x) any(!is.na(x) & x != "" & x != "NA")), ]
    tempdata_long <- melt(tempdata,
        measure = patterns(paste0("^f.", field_id)),
        variable.name = "instance",
        value.name = paste0("f.", field_id)
    )
    tempdata_long[, `:=`(instance = gsub(paste0("f.", field_id, "\\."), "", instance))]
    if (tempdata_long[, is.character(get(paste0("f.", field_id)))]) {
        tempdata_long <- tempdata_long[get(paste0("f.", field_id)) != "NA", ]
    } else {
        tempdata_long <- tempdata_long[!is.na(get(paste0("f.", field_id))), ]
    }
    tempdata_long[, (paste0("f.", field_id)) := gsub("\"\"", "", get(paste0("f.", field_id)))]
    tempdata_long <- tempdata_long[order(f.eid), ]

    tempdata_long <- tempdata_long[order(f.eid), ][!f.eid %in% wids, ]
    fwrite(tempdata_long, out_file, sep = "\t", quote = FALSE)
    return()
}

#' Print option list for the script
#'
#' @param option_list A list of options
#' @return None
print_option_list <- function(option_list) {
    cat("Option overview:\n\n")
    cat(sprintf("%-20s %-10s %s\n", "Name", "Type", "Default"))
    cat(strrep("-", 43), "\n")

    for (option in option_list) {
        long_opt <- slot(option, "long_flag")
        short_opt <- slot(option, "short_flag")
        option_names <- paste(long_opt, short_opt, sep = ", ")

        option_type <- slot(option, "type")
        option_default <- slot(option, "default")

        if (is.null(option_type)) {
            option_type <- "logical"
        }

        if (is.null(option_default)) {
            option_default <- "NULL"
        } else if (is.logical(option_default)) {
            option_default <- ifelse(option_default, "TRUE", "FALSE")
        }

        cat(sprintf("%-20s %-10s %s\n", option_names, option_type, option_default))
    }
}

#' Reformat UKB data
#'
#' @param fields A vector of field IDs to reformat
#' @param data_coding Whether to enable data coding
#' @param only_info Whether to display only info
#' @param keep_instances Whether to keep instances in the output
#' @param recode Whether to recode variables
#' @return A list containing the reformatted data and field info
reformat_ukb <- function(fields, data_coding = FALSE, only_info = FALSE, keep_instances = FALSE, recode = TRUE) {
    # Print info about fields
    field_baskets <- all_basket_fields[`Field ID` %in% fields & !is.na(basket), ]
    field_info <- unique(field_baskets[, .(`Field ID`, Description, Category, datatype, DataLocation, DataCoding = NA_character_)])
    fields <- field_info[, `Field ID`]

    if (data_coding) {
        field_info$URL <- url <- paste0("https://biobank.ctsu.ox.ac.uk/crystal/field.cgi?id=", fields)
        data_codes <- NULL
        for (i in seq_len(nrow(field_info))) {
            u <- url[i]
            doc_raw <- getURL(u)
            doc <- tidy_html(doc_raw)
            html <- htmlTreeParse(doc, useInternal = TRUE)
            txt <- xpathApply(html, "//body//text()[not(ancestor::script)][not(ancestor::style)][not(ancestor::noscript)]", xmlValue)
            txt <- unlist(unlist(txt))
            coded_data <- which(grepl("Data-Coding", txt))
            if (length(coded_data) == 1) {
                data_codes <- c(data_codes, txt[which(grepl("Data-Coding", txt)) + 1])
            } else {
                data_codes <- c(data_codes, NA)
            }
        }
        field_info$DataCoding <- data_codes
        print(field_info)
        if (only_info) {
            return(field_info[, .(`Field ID`, Description, Category, datatype, DataCoding, DataLocation, URL)])
        }
    } else {
        print(field_info[, 1:4])
        if (only_info) {
            return(field_info[, .(`Field ID`, Description, Category, datatype)])
        }
    }

    field_data <- list()

    for (j in seq_len(nrow(field_info))) {
        field_id <- field_info[j, `Field ID`]
        if (field_info[j, !grepl("merged", DataLocation)]) {
            extract_field_data(
                field_id = field_id,
                basket = field_baskets[`Field ID` == field_id, basket],
                file_basket = field_baskets[`Field ID` == field_id, basketfile],
                subfield_ids = field_baskets[`Field ID` == field_id, subfield_id],
                out_file = field_info[j, DataLocation],
                file_withdrawn = file_withdrawn,
                overwrite = FALSE
            )
        }
        fdata <- fread(field_info[j, DataLocation], colClasses = "character")
        if (keep_instances) {
            setnames(fdata, "instance", paste0("i.", field_id))
            fdata[, c("f.eid", paste0(c("f.", "i."), field_id)), with = FALSE]
        } else {
            fdata[, instance := NULL]
        }

        if ("basket" %in% names(fdata) && keep_instances) {
            setnames(fdata, "basket", paste0("b.", field_id))
            fdata[, c("f.eid", paste0(c("f.", "i.", "b."), field_id)), with = FALSE]
        } else if ("basket" %in% names(fdata) && !keep_instances) {
            fdata[, basket := NULL]
        }

        fdata <- unique(fdata)

        coding_id <- field_info[j, DataCoding]
        if (recode && !is.na(coding_id)) {
            if (!file.exists("./data/coding")) dir.create("./data/coding", recursive = TRUE)
            file_coding <- paste0("./data/coding/coding", coding_id, ".tsv")
            if (!file.exists(file_coding)) {
                url <- "https://biobank.ctsu.ox.ac.uk/crystal/codown.cgi"
                form_data <- list(id = coding_id)
                response <- httr::POST(url, body = form_data, encode = "form")
                if (httr::status_code(response) == 200) {
                    cat("Request was successful!\n")
                    content_text <- rawToChar(response$content)
                    coding_table <- data.table(read_delim(content_text, delim = "\t", col_names = TRUE, show_col_types = FALSE))
                    fwrite(coding_table, file_coding, sep = "\t", quote = FALSE)
                } else {
                    cat("Request failed! Status code:", httr::status_code(response), "\n")
                }
            } else {
                coding_table <- fread(file_coding, sep = "\t")
            }

            if (all(is.numeric(fdata[[paste0("f.", field_id)]]))) {
                fdata[, (paste0("c.", field_id)) := factor(get(paste0("f.", field_id)),
                    levels = coding_table$coding,
                    labels = coding_table$meaning
                )]
            }
        }
        field_data[[paste0("f.", field_id)]] <- fdata
    }
    return(list("data" = field_data, "info" = field_info))
}

# Define the option list for the script
option_list <- list(
    make_option(c("--fields", "-f"),
        type = "character",
        default = "",
        help = "Fields to include, separated by commas [default: %default]"
    ),
    make_option(c("--data_coding", "-d"),
        action = "store_true",
        default = FALSE,
        help = "Enable data coding [default: %default]"
    ),
    make_option(c("--only_info", "-o"),
        action = "store_true",
        default = FALSE,
        help = "Display only info [default: %default]"
    ),
    make_option(c("--keep_instances", "-k"),
        action = "store_true",
        default = FALSE,
        help = "Keep instances in the output [default: %default]"
    ),
    make_option(c("--recode", "-r"),
        action = "store_true",
        default = TRUE,
        help = "Recode variables [default: %default]"
    ),
    make_option(c("--prefix", "-p"),
        type = "character",
        default = "",
        help = "Output file prefix. Leave empty for interactive session [default: %default]"
    )
)

# Parse the command line options
parser <- OptionParser(option_list = option_list)
options <- parse_args(parser)
print_option_list(option_list)

# Main script execution
if (options$fields != "" || options$prefix != "") {
    options$fields <- strsplit(options$fields, ",")[[1]]
    results <- reformat_ukb(
        fields = options$fields,
        data_coding = options$data_coding,
        only_info = options$only_info,
        keep_instances = options$keep_instances,
        recode = options$recode
    )

    if (options$output_prefix != "") {
        output_file <- paste0(options$output_prefix, ".Rsav")
        save(results, file = output_file)
        cat("Results saved to:", output_file, "\n")
    }
} else {
    print("Not all parameters present")
    print("Use function as follows: `reformat_ukb(fields, data_coding = FALSE, only_info = FALSE, keep_instances = FALSE, recode = TRUE)`")
}

# Example usage of the reformat_ukb function
# example_fields <- c("field1", "field2")
# results <- reformat_ukb(fields = example_fields, data_coding = TRUE, only_info = FALSE, keep_instances = TRUE, recode = TRUE)
# print(results)