# ukb_data_explorations

## Basic concepts
1. Summarization of UKB fields
2. Summarization of available UKB baskets
3. Data extraction from baskets with instances

## Required R libraries
- data.table
- optparse
- parallel
- intervals
- htmltab
- methods
- httr
- readr
- bitops
    

## Step 1: Describe your data
Add the absolute paths (e.g. `/driveA/UKB/ukb####.tab`) of your TAB-delimited UKB baskets to a single text file `./data/baskets.txt`
Add the latest file with withdrawn samples 'w#####_########.csv' to './data/' folder

## Step 2: Collectin information about UKB fields
```bash
Rscript ./scripts/function.collectFields.r
```

## Step 3: Summarize the available data and prepare data across baskets
```bash
Rscript ./scripts/function.summarizeAvailableData.r
```

## Step 4: Extract field information and data and recode with instances:
```bash
Rscript ./scripts/function.extractUKBdata.r --help

Usage: ./scripts/function.extractUKBdata.r [options]


Options:
	-f FIELDS, --fields=FIELDS
		Fields to include, separated by commas [default: ]

	-d, --data_coding
		Enable data coding [default: FALSE]

	-o, --only_info
		Display only info [default: FALSE]

	-k, --keep_instances
		Keep instances in the output [default: FALSE]

	-r, --recode
		Recode variables [default: TRUE]

	-p PREFIX, --prefix=PREFIX
		Output file prefix. Leave empty for interactive session [default: ]

	-h, --help
		Show this help message and exit
```
