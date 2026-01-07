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
      extras = "-maxfn 0 -phase 50 -nohess",
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

