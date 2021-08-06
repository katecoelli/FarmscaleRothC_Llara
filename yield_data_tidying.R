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
##### Required Libraries #####
library(tidyverse)
library(purrr)
library(mapview)
library(rgdal)
library(gstat)

GDA94_latlong = CRS("+init=epsg:4283")
GDA94_xy_55 = CRS("+init=epsg:28355")

WGS84_latlong = CRS("+init=epsg:4326")
WGS84_xy_55 = CRS("+init=epsg:32755")

##### Files ######

# read in data from Brett - in txt krigged format
yield_files_krigged<- list.files("../../../../Data/Farms/Llara/Yield", pattern = "_kr", recursive = TRUE)

yield_files_krigged<- as.data.frame(yield_files_krigged)

yield_files_krigged<- yield_files_krigged%>%
  filter(!str_detect(yield_files_krigged, "elev"))%>%
  filter(!str_detect(yield_files_krigged, "moisture"))%>%
  filter(!str_detect(yield_files_krigged, "protein"))%>%
  filter(!str_detect(yield_files_krigged, "GM"))

yield_files_krigged<- list(yield_files_krigged$yield_files_krigged)

yield_files_krigged<- yield_files_krigged[[1]]

for(i in 1:length(yield_files_krigged)){
  yield_files_krigged[i]<- paste0("../../../../Data/Farms/Llara/Yield/", yield_files_krigged[i] )
}

#add extra information for 22 files, including which type of txt separator the columns use
extra_information<- data.frame(file = seq(1:22), sep = NA, croptype = NA, year = NA)

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


# link extra information to dataframes
yield_dfs<- list()
yield_dataframes<- for(i in 1:length(yield_files_krigged)){
  tmp<- read.delim(yield_files_krigged[i], sep = extra_information[i, "sep"])
  yield_dfs[[i]]<- tmp
}


for(i in 1:length(yield_dfs)){
  yield_dfs[[i]]["year"]<- rep(extra_information[i, "year"], nrow(yield_dfs[[i]]))
  yield_dfs[[i]]["croptype"]<- rep(extra_information[i, "croptype"], nrow(yield_dfs[[i]]))
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


#combine to form large dataframe
yield_large_df<- do.call("rbind", yield_dfs)


###################################################################
######## filter by year to create farm raster for each year #######
###################################################################

#Note - 2021 raster was already made by mikaela 

yield_Llara_2021_ras<- raster("../../../../Data/Farms/Llara/Yield/Campey/2021/cotton_2021_C6_7_8_9_yield.tif")
plot(yield_Llara_2021, main = "2021 Yield Llara")

## Filter by year to derive annual dataframes

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


head(yield_Llara_2016)

# This adapts Pat's inverse distance weighting code to have constant coordinate intervals

###########################
# applicable to all years #
###########################

## 1.  Buffer the field boundaries 
# re-project to planar coordinate system
# Load in boundary for field
farm = readOGR("../../../../Data/Farms/Llara/boundary/Farm_boundary_new.shp")
farm@proj4string

## 2.  Create grid
# load shapefiles
# create an empty raster within this polygon
bbox(farm)
grid_empty = raster(xmn= bbox(farm)[1], ymn= bbox(farm)[2], xmx = bbox(farm)[3], ymx = bbox(farm)[4], 
                    resolution = 10,
                    crs = WGS84_xy_55)
grid_empty[grid_empty] = 0 # set to zero
# now crop and mask
grid_empty = crop(grid_empty, farm)
# Mask the raster to prepare it to delineate the boundaries
grid_empty = mask(grid_empty, farm)
# And now convert it to SpatialPixel which is used for kriging
grid_empty_sp = as(grid_empty, "SpatialPixels")
plot(grid_empty_sp)

#######################
##### 2016 raster #####
#######################

### IDW - new local
t1 = Sys.time()
yield_Llara_2016_grid = idw(formula = Predicted ~ 1,
                       locations = yield_Llara_2016,
                       newdata = grid_empty_sp,
                       idp = 1,
                       nmax = 100,
                       nmin = 10,
                       maxdist = 500)
Sys.time()-t1

# turn into a raster
yield_Llara_2016_ras = raster(yield_Llara_2016_grid)
yield_Llara_2016_ras = crop(yield_Llara_2016_ras, farm)
yield_Llara_2016_ras = mask(yield_Llara_2016_ras, farm)
plot(yield_Llara_2016_ras, main = "2016 yield Llara")



#######################
##### 2017 raster #####
#######################

### IDW - new local
t1 = Sys.time()
yield_Llara_2017_grid = idw(formula = Predicted ~ 1,
                            locations = yield_Llara_2017,
                            newdata = grid_empty_sp,
                            idp = 1,
                            nmax = 100,
                            nmin = 10,
                            maxdist = 500)
Sys.time()-t1

# turn into a raster
yield_Llara_2017_ras = raster(yield_Llara_2017_grid)
yield_Llara_2017_ras = crop(yield_Llara_2017_ras, farm)
yield_Llara_2017_ras = mask(yield_Llara_2017_ras, farm)
plot(yield_Llara_2017_ras, main = "2017 yield Llara")


#######################
##### 2018 raster #####
#######################

### IDW - new local
t1 = Sys.time()
yield_Llara_2018_grid = idw(formula = Predicted ~ 1,
                            locations = yield_Llara_2018,
                            newdata = grid_empty_sp,
                            idp = 1,
                            nmax = 100,
                            nmin = 10,
                            maxdist = 500)
Sys.time()-t1

# turn into a raster
yield_Llara_2018_ras = raster(yield_Llara_2018_grid)
yield_Llara_2018_ras = crop(yield_Llara_2018_ras, farm)
yield_Llara_2018_ras = mask(yield_Llara_2018_ras, farm)
plot(yield_Llara_2018_ras, main = "2018 yield Llara")



#######################
##### 2019 raster #####
#######################

### IDW - new local
t1 = Sys.time()
yield_Llara_2019_grid = idw(formula = Predicted ~ 1,
                            locations = yield_Llara_2019,
                            newdata = grid_empty_sp,
                            idp = 1,
                            nmax = 100,
                            nmin = 10,
                            maxdist = 500)
Sys.time()-t1

# turn into a raster
yield_Llara_2019_ras = raster(yield_Llara_2019_grid)
yield_Llara_2019_ras = crop(yield_Llara_2019_ras, farm)
yield_Llara_2019_ras = mask(yield_Llara_2019_ras, farm)
plot(yield_Llara_2019_ras, main = "2019 yield Llara")



#######################
##### 2020 raster #####
#######################

### IDW - new local
t1 = Sys.time()
yield_Llara_2020_grid = idw(formula = Predicted ~ 1,
                            locations = yield_Llara_2020,
                            newdata = grid_empty_sp,
                            idp = 1,
                            nmax = 100,
                            nmin = 10,
                            maxdist = 500)
Sys.time()-t1

# turn into a raster
yield_Llara_2020_ras = raster(yield_Llara_2020_grid)
yield_Llara_2020_ras = crop(yield_Llara_2020_ras, farm)
yield_Llara_2020_ras = mask(yield_Llara_2020_ras, farm)
plot(yield_Llara_2020_ras, main = "2020 yield Llara")

###################################################################
######## stack all rasters together #######
###################################################################

yield_Llara_stack<- stack(yield_Llara_2016_ras, yield_Llara_2017_ras, yield_Llara_2018_ras, yield_Llara_2019_ras, yield_Llara_2020_ras)

writeRaster(yield_Llara_stack[[1]], "../Processed_Data/Yield/yield_Llara_2016.tif")
writeRaster(yield_Llara_stack[[2]], "../Processed_Data/Yield/yield_Llara_2017.tif")
writeRaster(yield_Llara_stack[[3]], "../Processed_Data/Yield/yield_Llara_2018.tif")
writeRaster(yield_Llara_stack[[4]], "../Processed_Data/Yield/yield_Llara_2019.tif")
writeRaster(yield_Llara_stack[[5]], "../Processed_Data/Yield/yield_Llara_2020.tif")


yield_Llara_stack@crs
yield_Llara_2021_ras@crs

plot(yield_Llara_stack)


######################################################
#### Join all yields and crops together in big df ####
######################################################
rm(GDA94_latlong)
rm(GDA94_xy_55)

environment<- lapply(ls(), get)

#add CRS back in
GDA94_latlong = CRS("+init=epsg:4283")
GDA94_xy_55 = CRS("+init=epsg:28355")


yield_llara<- environment%>%
  reduce(full_join)


yield_llara_2016<- yield_llara%>%
  filter(year ==2016)

yield_llara_2016<- yield_llara%>%
  filter(year ==2016)

yield_llara_2016<- yield_llara%>%
  filter(year ==2016)

yield_llara_2016<- yield_llara%>%
  filter(year ==2016)

yield_llara_2016<- yield_llara%>%
  filter(year ==2016)



yield_llara_sp<- SpatialPointsDataFrame(yield_llara[1:2], yield_llara, proj4string = GDA94_latlong)
mapview(yield_llara_sp)
