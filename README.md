# actris-level3-aerosol-profiling-climatology

CODES TO GENERATE LEVEL 3 FILES:

To generate the EARLINET level 3 files run Main.py script which execute, in order manner, the following steps:

  1: Generation of a table with the list of level 2 files and their characteristics, using Lev2database.R, Lev2database_layers.R and system.R scripts.
  
  2.1: Construction of tables with profile data, divided by wavelength, using lev3pro_355.R, lev3pro_532.R and lev3pro_1064.R scripts.
  
  2.2: Generation of netCDF files containing profile data, using lev3pro_nm_files.R, lev3pro_ns_files.R, lev3pro_s_files.R and lev3pro_y_files.R scripts.
  
  3.1: Construction of tables with integrated values, divided by type of source Level 2 files (e or b), using lev3int_b.R, lev3int_e.R and lev3int_pbl.R scripts.
  
  3.2: Reorganization of the integrated values, dividing them by wavelength, using lev3int_355.R, lev3int_532.R and lev3int_1064.R scripts.
  
  3.3: Generation of netCDF files containing profile data, using lev3int_nm_files.R, lev3int_ns_files.R, lev3int_s_files.R and lev3int_y_files.R scripts.
  
  4.1: Generation of the table with the values of the layers, using lev3int_layers.R script.
  
  4.2: Generation of netCDF files containing the layer values, using lev3layers_nm_files.R, lev3layers_ns_files.R, lev3layers_s_files.R and lev3layers_y_files.R scripts.
