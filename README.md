# ACTRIS Level 3 Aerosol Profiling Climatology

## Overview
This guide provides a concise set of instructions to help users set up and run the ACTRIS Level 3 aerosol profiling climatology software. The software processes atmospheric lidar data to generate Level 3 NetCDF files with integrated, profile-based, and layer-resolved statistics.
## Repository and Data
### Clone the Repository
Run the following lines of code from the terminal:
git clone https://github.com/actris-ares/actris-level3-aerosol-profiling-climatology.git
cd actris-level3-aerosol-profiling-climatology
### Download Level 2 Data
Download Level 2 NetCDF files for the desired stations from the EARLINET Data Portal (https://data.earlinet.org/earlinet/).
## Folder Structure
Ensure the following folders exist:  
./New/ &emsp; # Contains Level 2 NetCDF files organized by station  
./Layers/&nbsp;&nbsp;&nbsp;è&nbsp;&nbsp;&nbsp;un&nbsp;&nbsp;&nbsp;testo&nbsp;&nbsp;# Contains layer files (one per station)  
./Level3/Profiles/&nbsp;&nbsp;&nbsp;è&nbsp;&nbsp;&nbsp;un&nbsp;&nbsp;&nbsp;testo&nbsp;&nbsp;# Output: Level 3 profile NetCDFs  
./Level3/Integrated/&nbsp;&nbsp;&nbsp;è&nbsp;&nbsp;&nbsp;un&nbsp;&nbsp;&nbsp;testo&nbsp;&nbsp;# Output: Level 3 integrated NetCDFs  
./Level3/Layers/&nbsp;&nbsp;&nbsp;è&nbsp;&nbsp;&nbsp;un&nbsp;&nbsp;&nbsp;testo&nbsp;&nbsp;# Output: Level 3 layer histogram NetCDFs  
## Required Files
Place the following files in the root directory:
station.csv – Station metadata
Climatol2.log – Log file for climatological filtering
Calipso2.log – Log file for CALIPSO filtering
## Configuration
Before running the main script, open Main.R and configure the following variable:
Release
Set the time range for processing:
release <- c(2000, 2021)  # Example: process data from 2000 to 2021
## Execution
Run the Main Script
In an R session or terminal:
source("Main.R")
This will execute the full processing pipeline and generate NetCDF files in the appropriate Level3/ subdirectories.
## System Requirements
Tested OS: Ubuntu Mate 22.04 (Linux).
Recommended OS: All OS on which the R environment is installed.
R Packages Required:
isotone
ncdf4
radiant.data
dplyr
tidyr
No external dependencies beyond R packages are required.
## Output
The following NetCDF files will be generated:
Level3/Profiles/<station>/ – Profile-based statistics
Level3/Integrated/<station>/ – Integrated statistics
Level3/Layers/<station>/ – Layer-resolved histograms
Each file follows ACTRIS conventions and includes metadata such as time bounds, wavelength, and statistical descriptors.
