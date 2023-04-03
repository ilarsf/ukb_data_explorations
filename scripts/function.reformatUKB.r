# Rscript written by Lars Fritsche to extract / reformat UKB data

options(stringsAsFactors = FALSE)
library("data.table")
library("optparse")
library("intervals")

# data.table with the fields and their descriptions
all_tables <- fread("./data/Fields_in_Available_Data.txt", header = TRUE)

# data.table with the merged baskets
data_file <- "./data/merged_baskets.txt"

ukb_data_columns <- scan(data_file, what = character(0), sep = "\t", nlines = 1, quiet = TRUE)

# merge consecutive numbers to intervals and collapse
get_ranges <- function(colnumbers, collapse = TRUE) {
    cx <- clusters(colnumbers, 1)
    ux <- colnumbers[which(!colnumbers %in% unlist(cx))]
    ix <- sapply(cx, function(x) paste(range(x), collapse = "-"))
    out <- c(ux, ix)
    out <- out[order(as.numeric(gsub("\\-.+", "", out)))]
    paste(out, collapse = ",")
}

reformat_ukb <- function(fields, data_coding = FALSE, only_info = FALSE) {
    # print info about fields
    fieldinfo <- all_tables[which(all_tables$"Field ID" %in% fields & !is.na(all_tables$basket)), ]

    if (data_coding) {
        require("RCurl")
        require("htmltidy")
        require("XML")
        fieldinfo$URL <- url <- paste0("https://biobank.ctsu.ox.ac.uk/crystal/field.cgi?id=", fieldinfo$"Field ID")
        data_codes <- NULL
        for (i in seq_along(fieldinfo)) {
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
        fieldinfo$"Data Coding" <- paste0("https://biobank.ctsu.ox.ac.uk/crystal/coding.cgi?id=", data_codes)
        print(data.table(fieldinfo))
        if (only_info) {
            return(return(list("info" = data.table(fieldinfo, data_codes))))
        }
    } else {
        print(fieldinfo[, 1:4])
    }


    # Extract all fields
    allfields <- fieldinfo$subfield_id[which(fieldinfo$subfield_id != "n/a")]

    selected_columns <- c("id", unique(unlist(strsplit(allfields, ","))))
    print(paste("Reading all entries across", length(selected_columns), "columns"))

    colnumbers <- which(ukb_data_columns %in% selected_columns)

    reformatted <- fread(
        cmd = paste("cut -f", get_ranges(colnumbers), data_file), header = TRUE,
        colClasses = "character"
    )

    fieldpattern <- "f\\.(.+)\\..+\\..+"
    entrypattern <- "f\\..+\\.(.+\\..+)"

    keep <- which(rowSums(is.na(reformatted[, -1]) | reformatted[, -1] == "", na.rm = TRUE) != ncol(reformatted[, -1]))

    reformatted <- reformatted[keep, ]

    entries <- names(reformatted)[-1]
    entry_numbers <- gsub(entrypattern, "\\1", entries)
    uentries <- unique(entry_numbers)

    # reformat cancer registry (allow multiple entries by person)
    reformatted2 <- list()
    for (e in uentries) {
        new_entries <- entries[which(entry_numbers == e)]
        new_rows <- reformatted[, c("id", new_entries), with = FALSE]
        setnames(new_rows, new_entries, gsub(fieldpattern, "\\1", new_entries))
        missing_fields <- fields[which(!fields %in% names(new_rows))]
        for (missing_field in missing_fields) {
            new_rows[[as.character(missing_field)]] <- NA
        }
        reformatted2[[e]] <- new_rows[, c("id", fields), with = FALSE]
    }

    reformatted2 <- rbindlist(reformatted2)

    # remove empty entries
    keep <- which(rowSums(is.na(reformatted2[, -1]) | reformatted2[, -1] == "", na.rm = TRUE) != ncol(reformatted2[, -1]))
    reformatted2 <- reformatted2[keep, ]
    print(paste(nrow(reformatted2), "lines after filtering empty lines"))
    if (!data_coding) {
        return(reformatted2)
    } else {
        return(list("data" = reformatted2, "info" = data.table(fieldinfo, data_codes)))
    }
}

option_list <- list(
    make_option("--fields",
        type = "character", default = "",
        help = "comma-separated fields"
    ),
    make_option("--output",
        type = "character", default = "",
        help = "Full path to output file"
    )
)

parser <- OptionParser(usage = "%prog [options]", option_list = option_list)
args <- parse_args(parser, positional_arguments = 0)
opt <- args$options

if (opt$fields != "" || opt$output != "") {
    output <- reformat_ukb(as.integer(strsplit(opt$fields, ",")[[1]]))
    fwrite(output, opt$output, sep = "\t", col.names = TRUE, row.names = FALSE, quote = TRUE)
} else {
    print("Not all parameters present")
}
