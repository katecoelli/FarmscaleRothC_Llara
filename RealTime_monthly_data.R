###################################################################################
#### script to get the monthly data in the appropriate form for running RothC #####
##################################################################################

### Author: Kate Coelli - adapted from Sabastine's "process_soil_and_gridded_data.R"
### Date: 26/07/2021
### Email: kate.coelli@sydney.edu.au

### Required Libraries
library(tidyverse)
library(rgdal)
library(raster)
library(SoilR)

### Required functions
source("../../../../Useful_functions/Sabastines_RothC_functions/data_prep_and_params_optimisatn_functions.R")

### CRS
GDA94_latlong = CRS("+init=epsg:4283")
GDA94_xy_55 = CRS("+init=epsg:28355")
GDA94_xy_56 = CRS("+init=epsg:28356")

# Monthly data required to run the RothC model in real time.
# We will collect monthly data from 2001-2020


#### Required Columns

# site_id
# yr_mon
# NDVI
# cover
# ET
# rain
# evap
# mean_temp
# rootzoneSM
# topSM


### Inputs

#Data requirements

# - The Monthly data are:

# 1. Landsat NDVI: This data was obteained from The Google Earth Engine (GEE)
# The script for this can be found here:https://code.earthengine.google.com/dd86709e54061668f8555d88f1907cba

# 2. Evapotranspiration (ET): This data is derived from the 8-day MODIS ET. In processing we disaggregated the data to
# daily values and then compute the monthly sum from the daily data. The Script can be found here:
# https://code.earthengine.google.com/?scriptPath=users%2FPhD_research%2FRothC%3AET_extraction
# note - in order for this code to work the coordinates must be in long lat, with longitude listed first

# 3. Climate data: The climate data is from the SILO climate files. The variables extracted include rainfall,
# min and max temperatures, and pan evapotranspiration.

# 4. Soil moisture: This soil moisture data was derived from a soil water balance model. The data was generated
# by Niranjan at a daily time step. Again we averaged the values to a monthly

# - The soil data contains measured soil C fractions, total soil C, particle size fractions including clay,
# and field capacity (FC).

# - There are two separate files that contains site information. These site information include
#   soil sampling date and landuse type.



####################################
############# Climate ##############
####################################

### Temperature


### Rainfall


### Evapotranspiration 



