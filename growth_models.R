library(r4ss)
library(furrr)
library(future)
library(parallelly)
library(purrr)

mod_paths <- list.dirs(here::here(), full.names = TRUE, recursive = FALSE)
mod_names <- basename(mod_paths)

# Identify growth-0 model
growth0_idx <- grep("growth-0", mod_names)
growth0_path <- mod_paths[growth0_idx]
other_mod_paths <- mod_paths[-growth0_idx]
other_mod_names <- mod_names[-growth0_idx]

## 1. Run growth-0 full estimation with hessian
message("Running full estimation for growth-0...")
for(i in growth0_path){
  r4ss::run(
    dir = i,
    exe = here::here("ss3.exe"),
    skipfinished = FALSE,
    verbose = TRUE
  )
}


## 2. Run all other models WITHOUT estimation and hessian (as validation runs)
run_model_from_par <- function(model_dir) {
  message("Running validation for: ", basename(model_dir))
  tryCatch({
    r4ss::run(
      dir = model_dir,
      exe = here::here("ss3.exe"),
      extras = "-stopph 0 -nohess",
      skipfinished = FALSE,
      verbose = TRUE
    )
    # Confirm output
    file.exists(file.path(model_dir, "control.ss_new"))
  }, error = function(e) {
    message("Error: ", e$message)
    FALSE
  })
}

ncores <- parallelly::availableCores(omit = 1)
future::plan(future::multisession, workers = ncores)

furrr::future_map(other_mod_paths[-1], run_model_from_par, .progress = TRUE)

future::plan(future::sequential)

## 3. Get output

all_models <- r4ss::SSgetoutput(dirvec = other_mod_paths[2:22], modelnames = basename(other_mod_paths[2:22]))

# Create a vector of seasons matching the list order
seas_vec <- ifelse(grepl("twoseas", names(all_models)), 2, 1)

# Map over both the models and the season vector
# .x is the model, .y is the season
plots <- purrr::map2(all_models, seas_vec, ~r4ss::SSplotBiology(
  replist = .x,
  subplots = 1:2,
  seas = .y,
  print = TRUE
))


# growth_df <- all_models |> 
#   purrr::map("growthseries") |>    # Extract $growthseries from every element
#   purrr::list_rbind(names_to = "model_name")

# write_csv(growth_df, "all_models_growthseries.csv")


# Write as text file
output_file <- "all_models_growth_report.txt"

# specific 'clean' start: If file exists, delete it so we don't append to old runs
if (file.exists(output_file)) file.remove(output_file)

# Iterate and append to the file
# purrr::iwalk(all_models, function(model_obj, model_name) {
#   # A. Write the Model Name followed by a new line
#   cat(model_name, "\n", file = output_file, append = TRUE)
#   # B. Write the growthseries dataframe
#   # We use write.table for a clean text look (sep="\t" makes it tab-separated)
#     write.table(model_obj, 
#                 file = output_file, 
#                 append = TRUE, 
#                 row.names = FALSE, 
#                 quote = FALSE, 
#                 sep = "\t") # Change to sep="," if you prefer CSV format
  
#   # C. Write two new lines for spacing before the next entry
#   cat("\n\n", file = output_file, append = TRUE)
# })

# Custom function to get subseas 1 and 2 because r4ss just gives subseas 1
extract_mean_size <- function(mod) {
  # mod is a list as returned by r4ss::SSgetoutput
  # Get Report.sso path
  repfile <- mod$inputs$repfile
  
  # Get values needed
  accuage <- mod$accuage
  startyr <- mod$startyr
  endyr <- mod$endyr
  
  # Read the raw Report.sso file into a table
  # First, get column count
  get_ncol <- function(file) {
    # Get maximum column count from first 1000 lines
    max(sapply(readLines(file, n = 1000), function(x) length(strsplit(x, " +")[[1]])))
  }
  ncols <- get_ncol(repfile)
  rawrep <- read.table(
    file = repfile,
    col.names = 1:ncols,
    fill = TRUE,
    quote = "",
    colClasses = "character",
    nrows = -1,
    comment.char = "",
    blank.lines.skip = FALSE
  )
  
  # Helper: emptytest
  emptytest <- function(x) { sum(!is.na(x) & x == "") / length(x) }
  
  # Helper: match_report_table
  match_report_table <- function(
    string1, adjust1,
    string2 = NULL, adjust2 = -1,
    which_blank = 1,
    cols = "nonblank",
    matchcol1 = 1, matchcol2 = 1,
    obj = rawrep,
    blank_lines = NULL,
    substr1 = TRUE, substr2 = TRUE,
    header = FALSE, type.convert = FALSE
  ) {
    line1 <- match(
      string1,
      if (substr1) substring(obj[, matchcol1], 1, nchar(string1)) else obj[, matchcol1]
    )
    if (is.null(string2)) {
      if (is.null(blank_lines)) blank_lines <- which(apply(obj, 1, emptytest) == 1)
      line2 <- blank_lines[blank_lines > line1][which_blank]
      if (is.na(line2)) line2 <- nrow(obj)
    } else {
      line2 <- match(
        string2,
        if (substr2) substring(obj[, matchcol2], 1, nchar(string2)) else obj[, matchcol2]
      )
    }
    if (is.na(line1) | is.na(line2)) return(NULL)
    if (is.numeric(cols)) {
      out <- obj[(line1 + adjust1):(line2 + adjust2), cols]
    }
    if (cols[1] == "all") {
      out <- obj[(line1 + adjust1):(line2 + adjust2), ]
    }
    if (cols[1] == "nonblank") {
      out <- obj[(line1 + adjust1):(line2 + adjust2), ]
      out <- out[, apply(out, 2, emptytest) < 1]
    }
    if (header && nrow(out) > 0) {
      names(out) <- gsub("^$", "NoName", out[1, ])
      out <- out[-1, ]
    }
    if (type.convert) out <- type.convert(out, as.is = TRUE)
    return(out)
  }
  
  # Extract mean_size table
  mean_size <- match_report_table(
    "MEAN_SIZE_TIMESERIES",
    1,
    "mean_size_Jan_1",
    -2,
    cols = 1:(4 + accuage + 1),
    header = TRUE,
    type.convert = TRUE
  )
  # Filter for Sex==1 and years
  ms1 <- mean_size[
    mean_size[["Morph"]] == 1 &
    mean_size[["Yr"]] >= startyr &
    mean_size[["Yr"]] < endyr,
  ]
  
  return(ms1)
}

mean_size_list <- purrr::map(all_models, extract_mean_size)
names(mean_size_list) <- names(all_models)

output_file <- "all_models_growth_report.txt"

# specific 'clean' start: If file exists, delete it so we don't append to old runs
if (file.exists(output_file)) file.remove(output_file)

# Iterate and append to the file
purrr::iwalk(mean_size_list, function(model_obj, model_name) {
  # A. Write the Model Name followed by a new line
  cat(model_name, "\n", file = output_file, append = TRUE)
  # B. Write the growthseries dataframe
  # We use write.table for a clean text look (sep="\t" makes it tab-separated)
    write.table(model_obj, 
                file = output_file, 
                append = TRUE, 
                row.names = FALSE, 
                quote = FALSE, 
                sep = "\t") # Change to sep="," if you prefer CSV format
  
  # C. Write two new lines for spacing before the next entry
  cat("\n\n", file = output_file, append = TRUE)
})

growth_df <- mean_size_list |> 
  purrr::list_rbind(names_to = "model_name")

write.csv(growth_df, "all_models_growthseries.csv")
