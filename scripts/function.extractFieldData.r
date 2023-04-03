extractFieldData <- function(field_id, basket, file_basket, subfield_ids, out_file, file_withdrawn, overwrite = FALSE) {
    if (!file.exists(dirname(out_file))) dir.create(dirname(out_file), recursive = TRUE)
    if (file.exists(out_file) && overwrite) {
        print("Existing File not overwritten")
        return()
    }
    require(data.table)
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
