library(isotone)
library(ncdf4)
library(radiant.data)


# Function for closing all connection and files 
closeAllConnections <- function() {
  open_files <- showConnections(all = TRUE)
  for (i in seq_len(nrow(open_files))) {
    if (open_files[i, "description"] != "stdin" && open_files[i, "description"] != "stdout" && open_files[i, "description"] != "stderr") {
      close(getConnection(as.integer(open_files[i, "class"])))
    }
  }
  cat("Tutti i file aperti sono stati chiusi.\n")
}


# This script is developed to execute all project scripts in an orderly manner

result <- tryCatch({
  # Generation of a table with the list of level 2 files and their characteristics.
  source("./Lev2database.R")
  source("./Lev2database_layers.R")
  source("./system.R")
  
  # Construction of tables with profile data, divided by wavelength.
  source("./lev3pro_355.R")
  source("./lev3pro_532.R")
  source("./lev3pro_1064.R")
  
  # Generation of netCDF files containing profile data.
  source("./lev3pro_nm_files.R")
  source("./lev3pro_ns_files.R")
  source("./lev3pro_s_files.R")  # update leap year check
  source("./lev3pro_y_files.R")
  
  # Construction of tables with integrated values, divided by type of source Level 2 files (e or b).
  source("./lev3int_b.R")
  source("./lev3int_e.R")
  source("./lev3int_pbl.R")
  
  # Reorganization of the integrated values, dividing them by wavelength.
  source("./lev3int_355.R")
  source("./lev3int_532.R")
  source("./lev3int_1064.R")
  
  # Generation of netCDF files containing profile data.
  source("./lev3int_nm_files.R")
  source("./lev3int_ns_files.R")
  source("./lev3int_s_files.R")   # update leap year check
  source("./lev3int_y_files.R")

  # Generation of the table with the layer values.
  source("./lev3int_layers.R")
  
  # Generation of netCDF files containing the layer values.
  source("./lev3layers_nm_files.R")
  source("./lev3layers_ns_files.R")
  source("./lev3layers_s_files.R")
  source("./lev3layers_y_files.R")
  log(-1)
}, error = function(e) {
  cat("Errore catturato: ", e$message, "\n")
  NA
}, finally = {
  closeAllConnections()
  cat("Esecuzione del blocco finally\n")
})

# , warning = function(w) {
#  cat("Avvertimento: ", w$message, "\n")
#  invokeRestart("muffleWarning")
# }