library(data.table)

setDTthreads(24)

withdrawn <- list.files("./data", "w.+.csv", full.name = TRUE)

all_tables <- fread("./data/FieldListing.txt", sep = "\t")
all_tables[, `:=`(subfield_id = as.character(NA), basket = as.character(NA), basketfile = as.character(NA))]

baskets <- readLines("./data/baskets.txt")

names(baskets) <- gsub(".tab", "", basename(baskets))

baskets <- baskets[order(names(baskets))]

counts <- NULL

all_basket_fields <- list()

for (b in seq_along(baskets)) {
    btemp <- names(fread(baskets[b], nrow = 1, header = TRUE))
    bcols <- data.table("field" = gsub("[f\\.]{0,2}([0-9]+)[\\.\\-].+", "\\1", btemp), "subfield" = btemp)

    bcols <- split(bcols, bcols$field)
    basket_fields <- list()

    for (bcol in names(bcols)) {
        available <- which(all_tables$"Field ID" == bcol)
        counts <- c(counts, length(available))
        if (length(available) > 0) {
            all_tables$subfield_id[available] <- paste(bcols[[bcol]]$subfield, collapse = ",")
            all_tables$basket[available] <- names(baskets)[b]
            all_tables$basketfile[available] <- baskets[b]

            basket_fields[[bcol]] <- data.table(
                `Field ID` = bcol,
                basket = names(baskets)[b], basketfile = baskets[b], subfield_id = paste(bcols[[bcol]]$subfield, collapse = ",")
            )
        }
    }
    basket_fields <- rbindlist(basket_fields)
    all_basket_fields[[b]] <- basket_fields
}
all_tables <- all_tables[order(as.integer(all_tables$"Field ID")), ]
fwrite(all_tables, "./data/Fields_in_Available_Data.txt",
    sep = "\t", quote = TRUE, row.names = FALSE, col.names = TRUE, na = "n/a"
)

all_basket_fields <- rbindlist(all_basket_fields)
all_basket_fields <- merge(all_tables[, .(`Field ID` = as.character(`Field ID`), Description, Category, datatype), ],
    all_basket_fields,
    by = "Field ID", all.x = T
)
all_basket_fields[, Instances := gsub("f\\.[0-9]+\\.", "", subfield_id)]
fwrite(all_basket_fields, "./data/Fields_in_Available_Baskets.txt",
    sep = "\t", quote = TRUE, row.names = FALSE, col.names = TRUE, na = "n/a"
)

all_basket_fields <- all_basket_fields[order(`Field ID`, -as.numeric(gsub("ukb", "", basket))), ]
all_basket_fields <- all_basket_fields[!duplicated(paste(`Field ID`, subfield_id, Instances, sep = ":")), ]
all_basket_fields[, nInstances := nchar(gsub("[^,]", "", Instances)) + 1]

dup_fields <- all_basket_fields[duplicated(`Field ID`), unique(`Field ID`)]

# Remove individuals who withdrew
wids <- readLines(sort(withdrawn, decreasing = TRUE)[1])

dir.create("./data/merged_fields", recursive = TRUE)

# For duplicated fields with a different number of instances: create files with all baskets
for (i in seq_along(dup_fields)) {
    dup_field <- dup_fields[i]
    binfo <- all_basket_fields[`Field ID` == dup_field, ]
    dup_data <- list()
    for (j in seq_len(nrow(binfo))) {
        tempdata <- fread(binfo[j, basketfile], select = c("f.eid", strsplit(binfo[j, subfield_id], ",")[[1]]), na.strings = "")
        tempdata <- tempdata[apply(tempdata[, -1], 1, function(x) any(!is.na(x) & x != "" & x != "NA")), ]
        tempdata_long <- melt(tempdata, measure = patterns(paste0("^f.", dup_field)), value.name = paste0("f.", dup_field))
        tempdata_long[, `:=`(instance = gsub(paste0("f.", dup_field, "\\."), "", variable), variable = NULL)]
        if (tempdata_long[, is.character(get(paste0("f.", dup_field)))]) {
            tempdata_long <- tempdata_long[get(paste0("f.", dup_field)) != "NA", ]
        } else {
            tempdata_long <- tempdata_long[!is.na(get(paste0("f.", dup_field))), ]
        }
        tempdata_long[, (paste0("f.", dup_field)) := gsub("\"\"", "", get(paste0("f.", dup_field)))]
        tempdata_long <- tempdata_long[order(f.eid), ]
        tempdata_long[, basket := binfo[j, basket]]
        dup_data[[j]] <- tempdata_long
    }
    dup_data <- rbindlist(dup_data)
    # order by ID, remove withdrawn individuals
    dup_data <- dup_data[order(f.eid), ][!f.eid %in% wids, ]
    fwrite(dup_data, paste0("./data/merged_fields/FieldID_", dup_field, ".tsv"), sep = "\t", quote = F)
}

# For fields with the number of instances: create file for the latest basket
unique_fields <- all_basket_fields[!`Field ID` %in% dup_fields & !is.na(basket), .(`Field ID`, subfield_id, basket, basketfile)]
unique_fields <- unique_fields[order(`Field ID`, -as.numeric(gsub("ukb", "", basket))), ]
unique_fields <- unique_fields[!duplicated(`Field ID`), ]


all_basket_fields[, DataLocation := ifelse(`Field ID` %in% dup_fields,
    paste0("./data/merged_fields/FieldID_", `Field ID`, ".tsv"),
    paste0("./data/fields/FieldID_", `Field ID`, "_", basket, ".tsv")
)]

fwrite(all_basket_fields, "./data/Fields_in_Available_Baskets.txt", sep = "\t", quote = T)
