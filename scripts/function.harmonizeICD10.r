harmonizeICD10 <- function(icd) {
    if (is.na(icd) || icd == "" || icd == "NA") {
        return(NA)
    }

    if (grepl("^[A-Z]+", icd)) {
        i0 <- gsub("^([A-Z]+).+", "\\1", icd)
    } else {
        i0 <- character(0)
    }
    icd <- gsub("^[A-Z]+", "", icd)

    has_suffix <- grepl("[A-Za-z]+$|[A-Za-z]+[0-9]$", icd)

    if (has_suffix) {
        if (grepl("[A-Za-z]+$", icd)) {
            i3 <- gsub(".+([A-Za-z]+)$", "\\1", icd)
            icd <- gsub("[A-Za-z]+$", "", icd)
        } else {
            i3 <- gsub(".+([A-Za-z]+[0-9])$", "\\1", icd)
            icd <- gsub("[A-Za-z]+[0-9]$", "", icd)
        }
    } else {
        i3 <- character(0)
    }
    if (nchar(icd) > 2 && !grepl("\\.", icd)) {
        icd <- paste0(substr(icd, 1, 2), ".", substr(icd, 3, nchar(icd)))
    }
    itemp <- strsplit(icd, "\\.")[[1]]

    i1 <- formatC(as.integer(gsub("^[A-Z]+", "", itemp[1])), width = 2, flag = 0)

    if (length(itemp) == 2) {
        i2 <- paste0(".", itemp[2])
    } else {
        i2 <- character(0)
    }
    paste0(i0, i1, i2)
}
