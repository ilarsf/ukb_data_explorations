## Tutorial: Extracting and Processing ICD Data from UK Biobank

This tutorial guides you through the process of extracting and processing ICD10 and ICD9 diagnostic codes from the UK Biobank data, including steps for collecting field listings, summarizing available data, and extracting relevant diagnostic information.

### Overview

The scripts are organized into three main steps:
1. **Collect and Save Field Listings**
2. **Load and Inspect the Collected Field Listings**
3. **Find Relevant Data Fields and Extract Data**

### Requirements

Ensure you have installed all necessary R packages as outlined in the README. Also, verify that you have the UK Biobank data files and appropriate permissions to access them.

### Step 1: Collect and Save Field Listings

This initial step involves collecting field listings from the UK Biobank online showcase and saving them to a text file. This process is typically done once for a new data pull or when updates to the data fields are expected. This is done using the `collect_and_save_field_listings` function defined in `function.collectFields.R`.

#### Script Explanation

```R
source("./scripts/function.collectFields.R")

base_url <- "https://biobank.ndph.ox.ac.uk/showcase/list.cgi?it=0&vt="
vt_codes <- c("11", "21", "22", "31", "41", "51", "61", "101")
datatypes <- c(
    "Integer",
    "Categorical (single)",
    "Categorical (multiple)",
    "Continuous",
    "Text",
    "Date",
    "Time",
    "Compound",
    "unknown"
)
output_file <- "./data/FieldListing.txt"

collect_and_save_field_listings(base_url, vt_codes, datatypes, output_file)
```

This script loads the function for collecting field listings, sets up the base URL for the UK Biobank showcase, and specifies the variable types and output file. It then calls the function to perform the data collection.

**Important:** Execute this step once per data pull to update or gather the latest available fields.

### Step 2: Load and Inspect the Collected Field Listings

After collecting the field listings, this step processes the data to prepare it for detailed analysis. Like Step 1, this should be performed once per data pull to ensure data integrity and relevance.

Before you proceed with loading and inspecting the collected field listings, ensure your data files are properly set up:

#### Data Preparation

1. **TAB-Delimited UKB Baskets**:
   - Ensure that the absolute paths of your TAB-delimited UKB basket files are listed in a single text file named `baskets.txt`. Place this file in the `./data/` directory.
   - Example format in `baskets.txt`:
     ```
     /driveA/UKB/ukb12345.tab
     /driveA/UKB/ukb23456.tab
     ```
   - Each path should be on a new line to ensure the scripts can correctly read each file path.

2. **Withdrawn Samples**:
   - Place the latest file containing withdrawn samples, typically named like `w#####_########.csv`, in the `./data/` folder.
   - This file is crucial for filtering out withdrawn participants from the analysis to maintain data accuracy.

After ensuring that your data environment is correctly set up, you can proceed to load and inspect the field listings.

#### Script Explanation

```R
source("./scripts/function.summarizeAvailableData.R")

summarize_available_data(
    data_dir = "./data",
    field_listing_file = "FieldListing.txt",
    baskets_file = "baskets.txt",
    output_dir = "./data",
    withdrawn_pattern = "w.+.csv",
    num_threads = 24
)
```

This script loads the necessary function from the `function.summarizeAvailableData.R` file and summarizes the available data fields based on the inputs from the `FieldListing.txt` and the specified `baskets.txt`. It uses multi-threading to accelerate the processing of potentially large datasets, ensuring efficient data handling.

**Notes**:  
1. There is no need to repeat this step unless there is a new data pull or updates to the dataset.
2. Processing time may increase significantly if the same fields are found across multiple baskets. The script is optimized to handle large datasets, but the presence of duplicate fields can lead to extended processing times. It's important to consider this when running the script, especially with extensive datasets or limited computational resources.

### Step 3: Find Relevant Data Fields and Extract Data

The final step focuses on identifying relevant ICD10 and ICD9 fields and extracting the necessary data for analysis. It involves checking for available data, extracting specific codes, harmonizing them, and preparing the final dataset.

#### Detailed Script Actions

1. **Load Field Data**: Load all fields from available data baskets to identify which contain the required ICD10 and ICD9 codes.
2. **Check and Print Available ICD Data**: Filter out fields that contain ICD10 and ICD9 codes and print them to ensure correctness.
3. **Extract Data**: Specify fields for ICD10 and ICD9, extract them along with the date of the first diagnosis, and ensure data integrity by matching instances and baskets.
4. **Harmonize Codes**: Clean and standardize the ICD codes using predefined harmonization functions.
5. **Merge and Rename Data**: Combine ICD10 and ICD9 data into a single dataset and rename columns for better readability.
6. **Save Final Data**: Save the harmonized and merged data to a file, ensuring it is ready for further analysis or reporting.

#### Script Explanation

```R
source("./scripts/function.extractUKBdata.R")

# Set the number of threads for parallel processing
cpus <- parallel::detectCores()
setDTthreads(cpus)

# Function to process and harmonize ICD data
process_icd_data <- function(fields, harmonize_func, lexicon, basket = NULL) {
    extracted_data <- reformat_ukb(
        fields = fields,
        data_coding = TRUE,
        only_info = FALSE,
        keep_instances = TRUE,
        recode = TRUE
    )
    codes <- extracted_data$data[[paste0("f.", fields[1])]]
    first_date <- extracted_data$data[[paste0("f.", fields[2])]]

    if (!is.null(basket)) {
        if (paste0("b.", fields[1]) %in% names(codes)) {
            codes <- codes[codes[[paste0("b.", fields[1])]] == basket]
        }

        if (paste0("b.", fields[2]) %in% names(first_date)) {
            first_date <- first_date[first_date[[paste0("b.", fields[2])]] == basket]
        }
    }
    data <- merge(
        codes, first_date,
        by.x = c("f.eid", paste0("i.", fields[1])),
        by.y = c("f.eid", paste0("i.", fields[2])),
        all = TRUE
    )
    setnames(
        data, c("f.eid", paste0("i.", fields[1]), paste0("f.", fields[1]), paste0("f.", fields[2])),
        c("id", "instance", "icd_code", "icd_first_date")
    )
    data[, icd_code_clean := harmonize_func(icd_code)]
    data$lexicon <- lexicon
    setnames(
        data, c("icd_code", "icd_first_date", "icd_code_clean"),
        c("icd_code", "icd_first_date", "icd_code_clean")
    )
    return(data)
}

# Process ICD9 data
icd9_fields <- c("41271", "41281")
icd9_data <- process_icd_data(icd9_fields, harmonize_icd9, "ICD9")

# Process ICD10 data
# Here we specify the basket, because only one basket contains both ICD10 fields (code and date) and thus the same instance IDs
icd10_fields <- c("41270", "41280")
icd10_data <- process_icd_data(icd10_fields, harmonize_icd10, "ICD10", basket = "ukb12345")

# Combine the ICD10 and ICD9 data
icd_data <- rbind(
    icd9_data[, .(id, instance, icd_code, icd_code_clean, icd_first_date, lexicon)],
    icd10_data[, .(id, instance, icd_code, icd_code_clean, icd_first_date, lexicon)]
)

# Optionally, save the combined dataset to a file
fwrite(icd_data, "./results/ICD_data.tsv", sep = "\t", quote = TRUE, row.names = FALSE, col.names = TRUE)
```