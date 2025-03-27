library(haven)       # For reading .sas7bdat files
library(data.table)  # For efficient data handling
library(here)        # For project-relative paths

# Set the directory where your files are stored
data_dir <- here("data")  # Ensures it looks inside "data" folder in your project

# Define the range of years and construct file paths
years <- 2015:2024
file_paths <- file.path(data_dir, paste0("lfs_", years, ".sas7bdat"))

# Specify variables to keep
lfs_var <- c("survyear", "survmnth", "lfsstat", "prov",
             "age_12", "sex", "educ", "ftptlast", "cowmain", "immig",
             "naics_21", "noc_10", "hrlyearn", "estsize", "firmsize", "ftptmain", "utothrs",
             "finalwt", "permtemp", "schooln", "tenure", "cma","marstat", "mjh",
             "noc_43", "union")

# Function to load and filter dataset
load_lfs_data <- function(file) {
  if (!file.exists(file)) {
    message("File not found: ", file)
    return(NULL)
  }
  
  df <- read_sas(file)  # Load dataset
  print(paste("Loaded:", file, "with", nrow(df), "rows"))  # Debugging
  
  # Ensure selected variables exist
  existing_vars <- intersect(lfs_var, names(df))  # Keep only available variables
  missing_vars <- setdiff(lfs_var, names(df))  # Find missing variables
  
  if (length(missing_vars) > 0) {
    message("Missing variables in ", file, ": ", paste(missing_vars, collapse = ", "))
  }
  
  df <- df[, existing_vars, drop = FALSE]  # Select available columns
  return(as.data.table(df))  # Convert to data.table for efficiency
}

# Load all datasets into a list
list_dt <- lapply(file_paths, load_lfs_data)

# # Remove NULL elements (failed file loads)
# list_dt <- Filter(Negate(is.null), list_dt)

# Combine into one data.table
combined_data <- rbindlist(list_dt, use.names = TRUE, fill = TRUE)

# make a date column
# Ensure 'survyear' and 'survmnth' are numeric
combined_data[, survyear := as.integer(survyear)]
combined_data[, survmnth := as.integer(survmnth)]

# Create the new 'date' column in "YYYY-MM" format
combined_data[, date := sprintf("%04d-%02d", survyear, survmnth)]



