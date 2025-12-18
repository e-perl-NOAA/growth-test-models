library(r4ss)

## ---- PARAMETERS ----
original_file <- "C:\\Users\\elizabeth.gugliotti\\Desktop\\ss3-issues\\growth-model-testing\\growth-1-oneseas"

## ---- 1. READ DATA ----
inputs <- SS_read(original_file)
lencomp_orig <- inputs$dat$lencomp

# For augmented output
rows_to_expand <- !lencomp_orig$year == -9999

# Function to split/duplicate a row into year and year+0.5
expand_comp <- function(row, type) {
  row1 <- row
  row2 <- row
  row1$year <- row1$year
  row2$year <- row2$year + 0.5
  row1$month <- 1
  row2$month <- 1
  row1$Nsamp <- row1$Nsamp / 2
  row2$Nsamp <- row2$Nsamp / 2
  # Halve bin values (length bins start after column : Yr Month Fleet Sex Part) 
  if(type == "length"){
     bin_start <- 7
  } else if(type == "age"){
     bin_start <- 10
  } else {
    stop("Type must be either 'length' or 'age'")
  }
  bin_cols <- bin_start:ncol(row)
  row1[bin_cols] <- row1[bin_cols] / 2
  row2[bin_cols] <- row2[bin_cols] / 2
  rbind(row1, row2)
}

# ---- 2. Expand and TEST ----
test_failures <- list()
lencomp_expanded <- NULL

for(i in which(rows_to_expand)) {
  orig_row <- lencomp_orig[i, ]
  new_rows <- expand_comp(orig_row, type = "length")
  # test: each of the new rows is exactly half the original values
  for(j in 1:2) {
    test_ok <- TRUE
    # Compare Nsamp
    if(abs(new_rows[j, "Nsamp"] * 2 - orig_row[["Nsamp"]]) > 1e-8) {
      test_ok <- FALSE
    }
    # Compare bins (should all be half within floating point tolerance)
    for(col in names(orig_row)[8:length(orig_row)]) {
      if(abs(as.numeric(new_rows[j, col]) * 2 - as.numeric(orig_row[[col]])) > 1e-8) {
        test_ok <- FALSE
      }
    }
    if(!test_ok) {
      test_failures[[length(test_failures) + 1]] <- list(
        orig_row = orig_row,
        new_row  = new_rows[j, ],
        row_index = i,
        split_index = j
      )
    }
  }
  # Stack rows for output
  lencomp_expanded <- rbind(lencomp_expanded, new_rows)
}

# Output test result
if(length(test_failures) == 0) {
  message("All expanded rows are exactly half of the original (for non half-years) -- test passed!")
} else {
  message("Test failed in ", length(test_failures), " cases.")
  # You may wish to print details for inspection:
  print(test_failures)
}

inputs$dat$lencomp <- lencomp_expanded


## ---- 3. Set up new age bins data----
# Original bins (vector)
a_error <- inputs$dat$ageerror

# A function to duplicate each column
ages <- colnames(a_error)
dup_ages <- rep(ages, each = 2)
ageerror_stretched <- map_dfc(ages, ~{
  vals <- a_error[[.x]]
  # Return two columns (with the same name!)
  # Trick: setting colnames via set_names
  set_names(as.data.frame(list(vals, vals)), c(.x, .x))
})
colnames(ageerror_stretched) <- dup_ages

# Duplicate the columns
ageerror_dup <- duplicate_columns(a_error)

## Setup new age comps with half years
agecomp_orig <- inputs$dat$agecomp
rows_to_expand <- !agecomp_orig$year == -9999
test_failures <- list()
agecomp_expanded <- NULL

for(i in which(rows_to_expand)) {
  orig_row <- agecomp_orig[i, ]
  new_rows <- expand_comp(orig_row, type = "age")
  # test: each of the new rows is exactly half the original values
  for(j in 1:2) {
    test_ok <- TRUE
    # Compare Nsamp
    if(abs(new_rows[j, "Nsamp"] * 2 - orig_row[["Nsamp"]]) > 1e-8) {
      test_ok <- FALSE
    }
    # Compare bins (should all be half within floating point tolerance)
    for(col in names(orig_row)[8:length(orig_row)]) {
      if(abs(as.numeric(new_rows[j, col]) * 2 - as.numeric(orig_row[[col]])) > 1e-8) {
        test_ok <- FALSE
      }
    }
    if(!test_ok) {
      test_failures[[length(test_failures) + 1]] <- list(
        orig_row = orig_row,
        new_row  = new_rows[j, ],
        row_index = i,
        split_index = j
      )
    }
  }
  # Stack rows for output
  agecomp_expanded <- rbind(agecomp_expanded, new_rows)
}

## ---- .4 Prepare to expand the agecomp ----
inputs$dat$agecomp <- agecomp_expanded

write.csv(agecomp_expanded, "C:\\Users\\elizabeth.gugliotti\\Desktop\\ss3-issues\\growth-model-testing\\growth-1-seas_as_years\\agecomp_expanded.csv", row.names = FALSE)
write.csv(lencomp_expanded, "C:\\Users\\elizabeth.gugliotti\\Desktop\\ss3-issues\\growth-model-testing\\growth-1-seas_as_years\\lencomp_expanded.csv", row.names = FALSE)

SS_write(inputs, "C:\\Users\\elizabeth.gugliotti\\Desktop\\ss3-issues\\growth-model-testing\\growth-1-seas_as_years\\newdat")
