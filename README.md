## UK Biobank Data Extraction and Processing

This repository contains scripts for collecting, summarizing, and extracting medical data fields from the UK Biobank resource.

### Prerequisites

Before running these scripts, ensure you have the following R packages installed:
- `RCurl`
- `XML`
- `data.table`
- `htmltab`
- `htmltidy`
- `httr`
- `intervals`
- `methods`
- `optparse`
- `parallel`
- `readr`
- `stringr`

You can install these packages using the following command in R:

```R
install.packages(c("RCurl", "XML", "data.table", "htmltab", "htmltidy", "httr", "intervals", "methods", "optparse", "parallel", "readr", "stringr"))
```

You will also need access to UK Biobank data files and appropriate permissions to use them.

### Installation

Clone this repository to your local machine using:

```bash
git clone git@github.com:ilarsf/ukb_data_explorations.git
```

### Data Preparation

Ensure your data environment is set up correctly before running the scripts:

1. **TAB-Delimited UKB Baskets**:
   - Locate the absolute paths of your TAB-delimited UKB basket files.
   - Add these paths to a single text file named `baskets.txt` and save this file in the `./data/` directory.
   - Example format in `baskets.txt`:
     ```
     /driveA/UKB/ukb12345.tab
     /driveA/UKB/ukb23456.tab
     ```
   - Ensure each path is on a new line.

2. **Withdrawn Samples**:
   - Obtain the latest file containing withdrawn samples, typically named like `w#####_########.csv`.
   - Place this CSV file in the `./data/` folder.

### Files Generated

Running the scripts will generate the following files in your project directory:
- `FieldListing.txt`: Contains the list of fields collected from the UK Biobank.
- `Fields_in_Available_Baskets.txt`: Details fields available in different data baskets.

### Running the Scripts

The project consists of multiple R scripts organized into several steps:

1. **Collecting and Saving Field Listings**
2. **Loading and Inspecting the Collected Field Listings**
3. **Finding Relevant Data Fields and Extracting Data**

Run the scripts sequentially as they build on the output of the previous steps.

### Contributions

To contribute to this project, please fork the repository and submit a pull request.

### License

This project is licensed under the MIT License - see the LICENSE.md file for details.

### Acknowledgements

- UK Biobank for providing the data.

## Tutorial

For detailed instructions on how to use these scripts, see the [tutorial on extracting and processing ICD data](TUTORIAL.md).
