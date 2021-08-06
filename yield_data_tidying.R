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

GDA94_latlong = CRS("+init=epsg:4283")
GDA94_xy_55 = CRS("+init=epsg:28355")


##### Files ######



#2016
C2_2016_wheat<- read.csv("../../../../Data/Farms/Llara/Yield/Campey/2016/C2/wheat_lancer16_mga.txt")
head(C2_2016_wheat)
C2_2016_wheat<- C2_2016_wheat%>%
  select(Longitude, Latitude, yield_t_ha)%>%
  rename(long = Longitude, lat = Latitude)%>%
  mutate(year = 2016, crop_type = "wheat")

C3_2016_fieldpea<- read.csv("../../../../Data/Farms/Llara/Yield/Campey/2016/C3/fieldpea_16_mga.txt")
head(C3_2016_fieldpea)
C3_2016_fieldpea<- C3_2016_fieldpea%>%
  select(longitude, latitude, yield_t_ha)%>%
  rename(long = longitude, lat = latitude)%>%
  mutate(year = 2016, crop_type = "fieldpea")

C4_5_2016_canola<- read.csv("../../../../Data/Farms/Llara/Yield/Campey/2016/C4_5/canola16_mga.txt")
head(C4_5_2016_canola)
C4_5_2016_canola<- C4_5_2016_canola%>%
  select(longitude, latitude, yield_t_ha)%>%
  rename(long = longitude, lat = latitude)%>%
  mutate(year = 2016, crop_type = "canola")

C7_8_2016_fababeans<- read.csv("../../../../Data/Farms/Llara/Yield/Campey/2016/C7_8/fababeans_16_mga.txt")
head(C7_8_2016_fababeans)
C7_8_2016_fababeans<- C7_8_2016_fababeans%>%
  select(longitude, latitude, yield_t_ha)%>%
  rename(long = longitude, lat = latitude)%>%
  mutate(year = 2016, crop_type = "fababeans")

L2_2016_wheat<- read.csv("../../../../Data/Farms/Llara/Yield/North Llara/2016/L2/L2_wheat_lancer16_mga.txt")
head(L2_2016_wheat)
L2_2016_wheat<- L2_2016_wheat%>%
  select(longitude, latitude, yield_t_ha)%>%
  rename(long = longitude, lat = latitude)%>%
  mutate(year = 2016, crop_type = "wheat")

L3_2016_canola<- read.csv("../../../../Data/Farms/Llara/Yield/North Llara/2016/L3/L3_canola16_mga.txt")
head(L3_2016_canola)
L3_2016_canola<- L3_2016_canola%>%
  select(longitude, latitude, yield_t_ha)%>%
  rename(long = longitude, lat = latitude)%>%
  mutate(year = 2016, crop_type = "canola")

L4_2016_chickpea<- read.csv("../../../../Data/Farms/Llara/Yield/North Llara/2016/L4/L4_chickpea16_mga.txt")
head(L4_2016_chickpea)
L4_2016_chickpea<- L4_2016_chickpea%>%
  select(longitude, latitude, yield_t_ha)%>%
  rename(long = longitude, lat = latitude)%>%
  mutate(year = 2016, crop_type = "chickpea")


#2017
C1_2017_wheat<- read.csv("../../../../Data/Farms/Llara/Yield/Campey/2017/C1/wheat17_mga.txt")
head(C1_2017_wheat)
C1_2017_wheat<- C1_2017_wheat%>%
  select(longitude, latitude, yield_t_ha)%>%
  rename(long = longitude, lat = latitude)%>%
  mutate(year = 2017, crop_type = "wheat")


C2_2017_chickpea<- read.csv("../../../../Data/Farms/Llara/Yield/Campey/2017/C2/Chickpea17_mga.txt")
head(C2_2017_chickpea)
C2_2017_chickpea<- C2_2017_chickpea%>%
  select(longitude, latitude, yield_t_ha)%>%
  rename(long = longitude, lat = latitude)%>%
  mutate(year = 2017, crop_type = "chickpea")

C3_2017_canola<- read.csv("../../../../Data/Farms/Llara/Yield/Campey/2017/C3/canola17_mga.txt")
head(C3_2017_canola)
C3_2017_canola<- C3_2017_canola%>%
  select(longitude, latitude, yield_t_ha)%>%
  rename(long = longitude, lat = latitude)%>%
  mutate(year = 2017, crop_type = "canola")

C4_5_2017_chickpea<- read.csv("../../../../Data/Farms/Llara/Yield/Campey/2017/C4_5/chickpea17_mga.txt")
head(C4_5_2017_chickpea)
C4_5_2017_chickpea<- C4_5_2017_chickpea%>%
  select(longitude, latitude, yield_t_ha)%>%
  rename(long = longitude, lat = latitude)%>%
  mutate(year = 2017, crop_type = "chickpea")

C6_2017_chickpea<- read.csv("../../../../Data/Farms/Llara/Yield/Campey/2017/C6/sorghum17_mga.txt")
head(C6_2017_chickpea)
C6_2017_chickpea<- C6_2017_chickpea%>%
  select(longitude, latitude, yield_t_ha)%>%
  rename(long = longitude, lat = latitude)%>%
  mutate(year = 2017, crop_type = "chickpea")


C7_8_9_2017_canola<- read.csv("../../../../Data/Farms/Llara/Yield/Campey/2017/C7_8_9/canola17_mga.txt")
head(C7_8_9_2017_canola)
C7_8_9_2017_canola<- C7_8_9_2017_canola%>%
  select(longitude, latitude, yield_t_ha)%>%
  rename(long = longitude, lat = latitude)%>%
  mutate(year = 2017, crop_type = "canola")

L1_2017_wheat<- read.csv("../../../../Data/Farms/Llara/Yield/North Llara/2017/L1/wheat17_mga.txt")
head(L1_2017_wheat)#no long lat needs transformation first
L1_2017_wheat<- SpatialPointsDataFrame(L1_2017_wheat[2:1], L1_2017_wheat, proj4string = GDA94_xy_55)
L1_2017_wheat<- spTransform(L1_2017_wheat, GDA94_latlong)
L1_2017_wheat<- cbind(L1_2017_wheat@data, L1_2017_wheat@coords)
colnames(L1_2017_wheat)[(ncol(L1_2017_wheat)-1):ncol(L1_2017_wheat)]<- c("long", "lat")
head(L1_2017_wheat)
L1_2017_wheat<- L1_2017_wheat%>%
  select(long, lat, yield_t_ha)%>%
  mutate(year = 2017, crop_type = "wheat")


L3_2017_wheat<- read.csv("../../../../Data/Farms/Llara/Yield/North Llara/2017/L3/L3durum_wheat17_mga.txt")
head(L3_2017_wheat)
L3_2017_wheat<- L3_2017_wheat%>%
  select(longitude, latitude, yield_t_ha)%>%
  rename(long = longitude, lat = latitude)%>%
  mutate(year = 2017, crop_type = "wheat")


L4_2017_wheat<- read.csv("../../../../Data/Farms/Llara/Yield/North Llara/2017/L4/wheat17_wgs.txt")
head(L4_2017_wheat)
L4_2017_wheat<- L4_2017_wheat%>%
  select(longitude, latitude, yield_t_ha)%>%
  rename(long = longitude, lat = latitude)%>%
  mutate(year = 2017, crop_type = "wheat")

#2018
L2_2018_cotton<- read.csv("../../../../Data/Farms/Llara/Yield/North Llara/2018/L2/cotton_mga.txt")
head(L2_2018_cotton)#no long lat needs transformation first
L2_2018_cotton<- SpatialPointsDataFrame(L2_2018_cotton[1:2], L2_2018_cotton, proj4string = GDA94_xy_55)
L2_2018_cotton<- spTransform(L2_2018_cotton, GDA94_latlong)
L2_2018_cotton<- cbind(L2_2018_cotton@data, L2_2018_cotton@coords)
head(L2_2018_cotton)
colnames(L2_2018_cotton)[(ncol(L2_2018_cotton)-1):ncol(L2_2018_cotton)]<- c("long", "lat")
L2_2018_cotton<- L2_2018_cotton%>%
  select(long, lat, VRYIELDBAL)%>%
  rename(yield_t_ha=VRYIELDBAL)%>%
  mutate(year = 2018, crop_type = "cotton")

#2019
L1_2019_wheat<- read.csv("../../../../Data/Farms/Llara/Yield/North Llara/2019/L1/wheat_19_mga.txt")
head(L1_2019_wheat)#no long lat needs transformation first
L1_2019_wheat<- SpatialPointsDataFrame(L1_2019_wheat[1:2], L1_2019_wheat, proj4string = GDA94_xy_55)
L1_2019_wheat<- spTransform(L1_2019_wheat, GDA94_latlong)
L1_2019_wheat<- cbind(L1_2019_wheat@data, L1_2019_wheat@coords)
head(L1_2019_wheat)
colnames(L1_2019_wheat)[(ncol(L1_2019_wheat)-1):ncol(L1_2019_wheat)]<- c("long", "lat")
L1_2019_wheat<- L1_2019_wheat%>%
  select(long, lat, yield_t_ha)%>%
  mutate(year = 2019, crop_type = "wheat")


L2_2019_canola<- read.csv("../../../../Data/Farms/Llara/Yield/North Llara/2019/L2/L2_canola_mga.txt")
head(L2_2019_canola)#no long lat needs transformation first
L2_2019_canola<- SpatialPointsDataFrame(L2_2019_canola[1:2], L2_2019_canola, proj4string = GDA94_xy_55)
L2_2019_canola<- spTransform(L2_2019_canola, GDA94_latlong)
L2_2019_canola<- cbind(L2_2019_canola@data, L2_2019_canola@coords)
head(L2_2019_canola)
colnames(L2_2019_canola)[(ncol(L2_2019_canola)-1):ncol(L2_2019_canola)]<- c("long", "lat")
L2_2019_canola<- L2_2019_canola%>%
  select(long, lat, yield_t_ha)%>%
  mutate(year = 2019, crop_type = "canola")


#2020
L1_2020_fababeans<- read.csv("../../../../Data/Farms/Llara/Yield/North Llara/2020/L1/fababeans_20_mga.txt")
head(L1_2020_fababeans)#no long lat needs transformation first
L1_2020_fababeans<- SpatialPointsDataFrame(L1_2020_fababeans[1:2], L1_2020_fababeans, proj4string = GDA94_xy_55)
L1_2020_fababeans<- spTransform(L1_2020_fababeans, GDA94_latlong)
L1_2020_fababeans<- cbind(L1_2020_fababeans@data, L1_2020_fababeans@coords)
head(L1_2020_fababeans)
colnames(L1_2020_fababeans)[(ncol(L1_2020_fababeans)-1):ncol(L1_2020_fababeans)]<- c("long", "lat")
L1_2020_fababeans<- L1_2020_fababeans%>%
  select(long, lat, yield_t_ha)%>%
  mutate(year = 2020, crop_type = "fababeans")

L2_2020_wheat<- read.csv("../../../../Data/Farms/Llara/Yield/North Llara/2020/L2/wheat20_mga.txt")
head(L2_2020_wheat)#no long lat needs transformation first
L2_2020_wheat<- SpatialPointsDataFrame(L2_2020_wheat[1:2], L2_2020_wheat, proj4string = GDA94_xy_55)
L2_2020_wheat<- spTransform(L2_2020_wheat, GDA94_latlong)
L2_2020_wheat<- cbind(L2_2020_wheat@data, L2_2020_wheat@coords)
head(L2_2020_wheat)
colnames(L2_2020_wheat)[(ncol(L2_2020_wheat)-1):ncol(L2_2020_wheat)]<- c("long", "lat")
L2_2020_wheat<- L2_2020_wheat%>%
  select(long, lat, yield_t_ha)%>%
  mutate(year = 2020, crop_type = "wheat")

L3_2020_wheat<- read.csv("../../../../Data/Farms/Llara/Yield/North Llara/2020/L3/L3wheat20_mga.txt")
head(L3_2020_wheat)#no long lat needs transformation first
L3_2020_wheat<- SpatialPointsDataFrame(L3_2020_wheat[1:2], L3_2020_wheat, proj4string = GDA94_xy_55)
L3_2020_wheat<- spTransform(L3_2020_wheat, GDA94_latlong)
L3_2020_wheat<- cbind(L3_2020_wheat@data, L3_2020_wheat@coords)
head(L3_2020_wheat)
colnames(L3_2020_wheat)[(ncol(L3_2020_wheat)-1):ncol(L3_2020_wheat)]<- c("long", "lat")
L3_2020_wheat<- L3_2020_wheat%>%
  select(long, lat, yield_t_ha)%>%
  mutate(year = 2020, crop_type = "wheat")

#2021 (different format)
C6_2021_cotton<- read.csv("../../../../Data/Farms/Llara/Yield/Campey/2021/C6/Narrabri_C6_2021_Cotton.csv")
head(C6_2021_cotton)
C6_2021_cotton<- C6_2021_cotton%>%
  select(Longitude, Latitude, Yield.ba.ha.)%>%
  rename(long = Longitude, lat = Latitude, yield_t_ha = Yield.ba.ha.)%>%
  mutate(year = 2021, crop_type = "cotton")

C6_2021_cotton_sp<- SpatialPointsDataFrame(C6_2021_cotton[1:2], C6_2021_cotton)
gridded(C6_2021_cotton_sp)<-TRUE 

C7_8_9_2021_cotton<- read.csv("../../../../Data/Farms/Llara/Yield/Campey/2021/C7_8_9/Narrabri_C7_8_9_2021_Cotton.csv")
head(C7_8_9_2021_cotton)
C7_8_9_2021_cotton<- C7_8_9_2021_cotton%>%
  select(Longitude, Latitude, Yield.ba.ha.)%>%
  rename(long = Longitude, lat = Latitude, yield_t_ha = Yield.ba.ha.)%>%
  mutate(year = 2021, crop_type = "cotton")


yield_files_krigged<- list.files("../../../../Data/Farms/Llara/Yield", pattern = "_kr", recursive = TRUE)

yield_files_krigged_test<- as.data.frame(yield_files_krigged)

yield_files_krigged_test<- yield_files_krigged_test%>%
  filter(!str_detect(yield_files_krigged, "elev"))%>%
  filter(!str_detect(yield_files_krigged, "moisture"))%>%
  filter(!str_detect(yield_files_krigged, "protein"))

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
