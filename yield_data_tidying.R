#####################################################
#### script to tidy all the yield data for llara#####
#####################################################

#### Steps required #####

# 1. add sample year column to txt files
# 2. add crop type column to txt files - names need to be consistent with lookup table
# 3. add a field column to txt files ????
# 4. rasterize yield data
# 5. Stack yield data by year
# 6. Extract data by sample location
# 7. Add extracted data to sample locations df
# 8. Build scanio dataframes based on cycling through yield options and the associated climate data
  # e.g. if a point repeats yield from 2016 and 2017, collect that weather data to match

### Author: Kate Coelli
### Date: 05/08/2021
### Email: kate.coelli@sydney.edu.au

rm(list = ls())


###############################################
############# Required information ############
###############################################

##### Required Libraries #####
library(tidyverse)
library(purrr)
library(mapview)
library(rgdal)
library(gstat)
library(raster)


##### CRS #####
GDA94_latlong = CRS("+init=epsg:4283")
GDA94_xy_55 = CRS("+init=epsg:28355")

WGS84_latlong = CRS("+init=epsg:4326")
WGS84_xy_55 = CRS("+init=epsg:32755")

##### Files ######

# read in data from Brett - in txt krigged format
yield_files_krigged<- list.files("../../../../Data/Farms/Llara/Yield", pattern = "_kr", full.names = TRUE, recursive = TRUE)

yield_files_krigged<- as.data.frame(yield_files_krigged)

yield_files_krigged<- yield_files_krigged%>%
  filter(!str_detect(yield_files_krigged, "elev"))%>%
  filter(!str_detect(yield_files_krigged, "moisture"))%>%
  filter(!str_detect(yield_files_krigged, "protein"))%>%
  filter(!str_detect(yield_files_krigged, "GM"))

yield_files_krigged<- list(yield_files_krigged$yield_files_krigged)

yield_files_krigged<- yield_files_krigged[[1]]


#add extra information for 22 files, including which type of txt separator the columns use
extra_information<- data.frame(file = seq(1:22), sep = NA, croptype = NA, year = NA, field = NA, fieldcode = NA)

extra_information$sep<- c(",",
                                ",",
                                ",",
                                ",",
                                ",",
                                ",",
                                ",",
                                ",",
                                "",
                                ",",
                                ",",
                                ",",
                                ",",
                                "\t",
                                ",",
                                ",",
                                "",
                                "",
                                "",
                                "",
                                "",
                                "")

extra_information$year<- c(2016,2016,2016,2016,2017,2017,2017,2017,2017,2017,2016,2016,2016,2017,2017,2017,2018,2019,2019,2020,2020,2020)
extra_information$croptype<- c("wheat", "fieldpea", "canola", "fababeans", "wheat", "chickpea", "canola", "chickpea", "sorghum", "canola", "wheat",
                               "canola", "chickpea", "wheat", "wheat", "wheat", "cotton", "wheat", "canola", "fababeans", "wheat", "wheat")

extra_information$field<- c("Campey 2", "Campey 3", "Campey 4_5", "Campey 7_8_9", "Campey 1", "Campey 2", "Campey 3", "Campey 4_5", "Campey 6",
                            "Campey 7_8_9", "Llara 2", "Llara 3", "Llara 4", "Llara 1", "Llara 3", "Llara 4", "Llara 2", "Llara 1", "Llara 2",
                            "Llara 1", "Llara 2", "Llara 3") #note one of the 7_8_9 is labelled as 7_8 in the folder but is actually 7_8_9, so I reassigned
                                                              #accordingly here
extra_information$index<- c("2", "3", "4", "6", "1", "2", "3", "4", "5", "6", "8", "9", "10", "7", "9", "10", "8", "7", "8", "7", "8", "9")

# link extra information to dataframes
yield_dfs<- list()
yield_dataframes<- for(i in 1:length(yield_files_krigged)){
  tmp<- read.delim(yield_files_krigged[i], sep = extra_information[i, "sep"])
  yield_dfs[[i]]<- tmp
}


for(i in 1:length(yield_dfs)){
  yield_dfs[[i]]["year"]<- rep(extra_information[i, "year"], nrow(yield_dfs[[i]]))
  yield_dfs[[i]]["croptype"]<- rep(extra_information[i, "croptype"], nrow(yield_dfs[[i]]))
  yield_dfs[[i]]["field"]<- rep(extra_information[i, "field"], nrow(yield_dfs[[i]]))
  yield_dfs[[i]]["index"]<- rep(extra_information[i, "index"], nrow(yield_dfs[[i]]))
}

##Tidy##
#remove additional column in some of the dfs
#now columns should be:
  #X (Easting)
  #Y (Northing)
  #Predicted
  #Prediction Error
  #year
  #croptype

for(i in (length(yield_dfs)-5):length(yield_dfs)){
  yield_dfs[[i]]<- yield_dfs[[i]][-1]
}

yield_dfs[[9]]<- yield_dfs[[9]][-1]

#remove 2017 L1 wheat - some data seems to be missing - ask TOM if he wants to search for it.
yield_dfs[[14]]<- NULL 


# Convert from list of dataframes to a large dataframe

yield_large_df<- do.call(rbind, yield_dfs)


# Load in boundaries for all fields

field_boundary_files<- list.files("../../../../Data/Farms/Llara/boundary", pattern = ".shp", full.names = TRUE, recursive = TRUE)
field_boundaries_individual<- field_boundary_files[-c(7,8,13)]
Campey_1<- readOGR(field_boundaries_individual[1])
Campey_2<- readOGR(field_boundaries_individual[2])
Campey_3<- readOGR(field_boundaries_individual[3])
Campey_4_5<- readOGR(field_boundaries_individual[4])
Campey_6<- readOGR(field_boundaries_individual[5])
#for some reason campey6 doesnt have dataframe information
Campey_6@data<- Campey_4_5@data
Campey_6@data$Field<- "Campey 6"
Campey_7_8_9<-readOGR(field_boundaries_individual[6])
Llara_1<-readOGR(field_boundaries_individual[7])
Llara_2<-readOGR(field_boundaries_individual[8])
Llara_3<-readOGR(field_boundaries_individual[9])
Llara_4<-readOGR(field_boundaries_individual[10])
all_fields<- bind(Campey_1, Campey_2, Campey_3, Campey_4_5, Campey_6, Campey_7_8_9, Llara_1, Llara_2, Llara_3, Llara_4)
plot(all_fields)

all_fields@proj4string

# Transform boundary shapefile
all_fields<- spTransform(all_fields, WGS84_xy_55)

names(all_fields@polygons)<- all_fields@data$Field

#load in boundary for farm
farm<- readOGR("../../../../Data/Farms/Llara/boundary/Farm_boundary_new.shp")
farm@proj4string #in correct CRS already

#################################################
##### create farm-wide raster for each year #####
#################################################

#Note - 2021 raster was already made by mikaela 

yield_Llara_2021_ras<- raster("../../../../Data/Farms/Llara/Yield/Campey/2021/cotton_2021_C6_7_8_9_yield.tif")
plot(yield_Llara_2021_ras, main = "2021 Yield Llara")

# Filter df by year

yield_Llara_2016<- yield_large_df%>%
  filter(year == 2016)
yield_Llara_2016<- SpatialPointsDataFrame(yield_Llara_2016[1:2], yield_Llara_2016, proj4string = WGS84_xy_55)


yield_Llara_2017<- yield_large_df%>%
  filter(year == 2017)
yield_Llara_2017<- SpatialPointsDataFrame(yield_Llara_2017[1:2], yield_Llara_2017, proj4string = WGS84_xy_55)


yield_Llara_2018<- yield_large_df%>%
  filter(year == 2018)
yield_Llara_2018<- SpatialPointsDataFrame(yield_Llara_2018[1:2], yield_Llara_2018, proj4string = WGS84_xy_55)


yield_Llara_2019<- yield_large_df%>%
  filter(year == 2019)
yield_Llara_2019<- SpatialPointsDataFrame(yield_Llara_2019[1:2], yield_Llara_2019, proj4string = WGS84_xy_55)


yield_Llara_2020<- yield_large_df%>%
  filter(year == 2020)
yield_Llara_2020<- SpatialPointsDataFrame(yield_Llara_2020[1:2], yield_Llara_2020, proj4string = WGS84_xy_55)

yield_by_year<- list(yield_Llara_2016, yield_Llara_2017, yield_Llara_2018, yield_Llara_2019, yield_Llara_2020 )

names(yield_by_year)<- c(2016, 2017, 2018, 2019, 2020)


# form annual field boundaries


field_boundary_by_year<- list()

for(i in 1:length(yield_by_year)){
  yield_info<- yield_by_year[[i]]
  field_index<- as.numeric(unique(yield_info$index))
  field_boundaries<- all_fields[field_index,]
  field_boundary_by_year[[i]]<- field_boundaries
}

names(field_boundary_by_year)<-c(2016, 2017, 2018, 2019, 2020)

# Create loop to form grid for all field boundaries per year

grid_empty_by_year<- list()

for(i in 1:length(field_boundary_by_year)){
  ## 2.  Create grid
  # load shapefiles
  # create an empty raster within this polygon
  field<- field_boundary_by_year[[i]]
  bbox(field)
  grid_empty = raster(xmn= bbox(field)[1], ymn= bbox(field)[2], xmx = bbox(field)[3], ymx = bbox(field)[4], 
                      resolution = 10,
                      crs = WGS84_xy_55)
  grid_empty[grid_empty] = 0 # set to zero
  # now crop and mask
  grid_empty = crop(grid_empty, field)
  # Mask the raster to prepare it to delineate the boundaries
  grid_empty = mask(grid_empty, field)
  # And now convert it to SpatialPixel which is used for kriging
  grid_empty_sp = as(grid_empty, "SpatialPixels")
  plot(grid_empty_sp)
  grid_empty_by_year[[i]]<- grid_empty_sp

}

names(grid_empty_by_year)<- c(2016, 2017, 2018, 2019, 2020)





# This adapts Pat's inverse distance weighting code to have constant coordinate intervals

# Create raster for each year in a loop

yield_raster_by_year<- list()

for(i in 1:length(yield_by_year)){
  ### IDW - new local
  t1 = Sys.time()
  yield_grid = idw(formula = Predicted ~ 1,
                        locations = yield_by_year[[i]],
                        newdata = grid_empty_by_year[[i]],
                        idp = 1,
                        nmax = 100,
                        nmin = 10,
                        maxdist = 500)
  Sys.time()-t1
  
  # turn into a raster
  yield_raster = raster(yield_grid)
  yield_raster = crop(yield_raster, field_boundary_by_year[[i]])
  yield_raster = mask(yield_raster, field_boundary_by_year[[i]])
  #combine in list
  yield_raster_by_year[[i]]<- yield_raster
}

names(yield_raster_by_year)<-c(2016, 2017, 2018, 2019, 2020)


#plot rasters by year

#2016
plot(yield_raster_by_year[[1]], main = paste0("2016 yield Llara"))
lines(farm)

#2017
plot(yield_raster_by_year[[2]], main = paste0("2017 yield Llara"))
lines(farm)

#2018
plot(yield_raster_by_year[[3]], main = paste0("2018 yield Llara"))
lines(farm)

#2019
plot(yield_raster_by_year[[4]], main = paste0("2019 yield Llara"))
lines(farm)

#2020
plot(yield_raster_by_year[[5]], main = paste0("2020 yield Llara"))
lines(farm)

# write rasters to file
writeRaster(yield_raster_by_year[[1]],"../Processed_Data/Yield/IDW_yield_2016.tif")
writeRaster(yield_raster_by_year[[2]],"../Processed_Data/Yield/IDW_yield_2017.tif")
writeRaster(yield_raster_by_year[[3]],"../Processed_Data/Yield/IDW_yield_2018.tif")
writeRaster(yield_raster_by_year[[4]],"../Processed_Data/Yield/IDW_yield_2019.tif")
writeRaster(yield_raster_by_year[[5]],"../Processed_Data/Yield/IDW_yield_2020.tif")

