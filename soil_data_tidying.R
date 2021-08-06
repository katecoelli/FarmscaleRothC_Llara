###########################################################################
#### script to get the data in the appropriate form for running RothC #####
###########################################################################

### Author: Kate Coelli
### Date: 21/07/2021
### Email: kate.coelli@sydney.edu.au

### Update: updated to tidy Llara data 


# Soil Data - dataframe which contains the following variables
# SOC
# POC (RPM in RothC)
# HOC (HUM in RothC)
# ROC (IOM in RothC)
# Sand %
# Clay %
# Field Capacity
# Date of sampling - Need to talk to Tom about this for long ambiguous surveys


### Required Libraries
library(tidyverse)
library(rgdal)
library(raster)
library(mapview)

### CRS
GDA94_latlong = CRS("+init=epsg:4283")
GDA94_xy_55 = CRS("+init=epsg:28355")
GDA94_xy_56 = CRS("+init=epsg:28356")

### Required Functions
# These functions can be found in my github repo - https://github.com/katecoelli/Useful_functions
source("../../../../useful_functions/Soil related functions.R")

##############################
######### Site Data ##########
##############################


### Farm boundary
#boundary<- readOGR("../../../../Data/Farms/Llara/boundary/Llara.shp")


### Soil data
#read in original data
#data has been splined to 0-30cm
soil_data<- read.csv("../../../../Data/Farms/Llara/Processed_Data/BGINNS_splined_SOC_PSA.csv")#start with this, but there are other points to merge
soil_data$Depth_Upper<- 0
soil_data$Depth_Lower<- 30


#sample locations
sample_locations<- SpatialPointsDataFrame(soil_data[4:3], soil_data[2], proj4string = GDA94_latlong)
#check long and lat in right order and referencing system correct using mapview - will not plot in right spot otherwise
mapview(sample_locations)
writeOGR(sample_locations, dsn="../Processed_Data/Llara_locations", layer = "Llara_locations_BG", driver =  "ESRI Shapefile")


#select relevant variables
soil_data<- soil_data%>%
  dplyr::select(Long, Lat, Site_ID, Depth_Upper, Depth_Lower, Clay, Sand, SOC)%>%
  rename(Upper = Depth_Upper, Lower = Depth_Lower)


##############################
####### Gridded Data #########
##############################

### Fraction Maps
## These fraction maps are from Jon Gray et al 2019

ROC<-raster("../../../../Data/NSW_extent_data/SOC_fractions_Gray/ROCtph0_30_boot_mean_gda 190305.tif")
HOC<- raster("../../../../Data/NSW_extent_data/SOC_fractions_Gray/HOCtph0_30_boot_mean_gda 190305.tif")
POC<-raster("../../../../Data/NSW_extent_data/SOC_fractions_Gray/POCtph0_30_boot_mean_final_gda_190305.tif")


#extract fractions
soil_data$ROC<- raster::extract(ROC, soil_data[1:2])
soil_data$HOC<- raster::extract(HOC, soil_data[1:2])
soil_data$POC<- raster::extract(POC, soil_data[1:2])

######################
####### tidy #########
######################

#data already in 0-30cm depths

#calculate Field Capacity using function
soil_data_tidy<- soil_data%>%
  mutate(FC=FC(Sand, Clay))%>%
  mutate(bucket_size = FC * soil.thick * 10)

#calculate BD and convert SOC (%) to stocks (t/ha)
soil_data_tidy<- soil_data_tidy%>%
  mutate(BD=bd_glob(SOC, Van_Bemelen_factor, Sand, mid_depth= soil.thick/2))%>%
  mutate(SOC=SOC*soil.thick*BD) #SOC(%)*BD*depth(cm)



####################################
####### Final Soil Data df #########
####################################

#final tidy 
soil_data_final<- soil_data_tidy%>%
  rename(ID=Site_ID)%>%
  mutate(Sample.Date = "15/07/2018")%>% #date was just "July" so I've set all dates as mid point in july
  dplyr::select(Long, Lat, ID, Sample.Date, everything(), -BD)%>%
  mutate(year = substr(Sample.Date, nchar(Sample.Date)-3, nchar(Sample.Date)))



# NB - do not include fractions because they will be determined for each site in the equil. run


#######################################################
####### Final Soil Data for RothC Equilibrium #########
#######################################################

soil_data_equil<- soil_data_final%>%
  select(-HOC, -POC, -Sand, -FC, -bucket_size, -year)

write_csv(soil_data_final, "../Processed_Data/soil_data_equil.csv")




##############################################
####### Final Soil Data for RothC RT #########
##############################################

#NB THIS CAN ONLY OCCUR ONCE EQUIL RUN HAS HAPPENED

#Select required data for RT RothC run - ensure correct column names
  #site_id
  #year
  #TOC
  #RPM
  #HUM
  #IOM
  #depth
  #bucket_size
  #clay

fractions<- read.csv("../Processed_Data/fractions_for_initialisation.csv") #this file created in equilibrium run RMD

soil_data_RT<- soil_data_final%>%
  add_column(depth = 30)%>%
  select(ID, year, SOC, bucket_size, Clay)%>%
  full_join(fractions[,c("ID", "RPM", "HUM", "IOM")])%>%
  rename(site_id=ID, TOC = SOC)
  
soil_data_RT$depth<- 30  

write_csv(soil_data_RT, "../Processed_Data/soil_data_RT.csv")



