## Description: Script to run pca calculations to create social vulnerability indices and return PCA results, 
## rerun for each year after changing year and file location inputs 
## Authors: Kate Nelson
## Date: Last modified 12/16/2024


# Setup ----

  library(sf)
  
  wd<-getwd()
  
  source(paste0(wd,'/pca_final.R'))
  
  
  ct <- st_read("E:/Intersection of Social Vulnerability and Flood/tlgdb_2018_a_us_substategeo.gdb", layer = "Census_Tract")
  ct$STATEFP <- substr(ct$GEOID,1,2) 
  ct$COUNTYFP <- substr(ct$GEOID,1,5) 
  st_write(ct,"tl_2018_censustract.shp", driver = "ESRI Shapefile", append=FALSE)
  
  bg <- st_read("E:/Intersection of Social Vulnerability and Flood/tlgdb_2018_a_us_substategeo.gdb", layer = "Block_Group")
  bg$STATEFP <- substr(bg$GEOID,1,2) 
  bg$COUNTYFP <- substr(bg$GEOID,1,5) 
  st_write(bg,"tl_2018_blockgroup.shp", driver = "ESRI Shapefile", append=FALSE)
  


# Run county data ----
  
  # build.pca <- function(data, boundaries, extent, scale)
  # data: demographic dataset
  # boundaries: shapefile of geographic area
  # extent: designate data use for nation, state, or county
  # scale: geographic area of input spatial boundary and demographic data resolution
  
  s<-build.pca('/county data by states/all_Final_Table.csv', '/tl_2018_us_county.shp', 'US', 'county') 
  s<-build.pca('/county data by states/all_Final_Table.csv', '/tl_2018_us_county.shp', 'State', 'county') 


# Combine and run pca on census tract data ----
  
  files<-list.files(path= paste0(wd,"/census tract data by states/"), pattern= '_Final_Table') #get list of state files
  f<-paste0(wd,"/census tract data by states/",files, sep="")
  
  states<-lapply(f, read.csv) #read in all datafiles 
  all_states<-do.call(rbind, states) #merge all states
  library(readr)
  write_csv(all_states, paste0(wd,"/census tract data by states/all_states.csv"))
  
  s<-build.pca('/census tract data by states/all_states.csv', '/tl_2018_censustract.shp', 'US', 'tract') 
  s<-build.pca('/census tract data by states/all_states.csv', '/tl_2018_censustract.shp', 'State', 'tract') 
  s<-build.pca(data='/census tract data by states/all_states.csv', boundaries='/tl_2018_censustract.shp', extent='County', scale='tract') 


# Combine and run pca on block group data ----
  
  files<-list.files(path= paste0(wd,"/block group data by states/"), pattern= '_Final_Table') #get list of state files
  f<-paste0(wd,"/block group data by states/",files, sep="")
  
  states<-lapply(f, read.csv) #read in all datafiles 
  all_states<-do.call(rbind, states) #merge all states
  library(readr)
  write_csv(all_states, paste0(wd,"/block group data by states/all_states.csv"))
  
  s<-build.pca(data='/block group data by states/all_states.csv', boundaries='/tl_2018_blockgroup.shp', extent='US', scale='blockgroup') 
  s<-build.pca('/block group data by states/all_states.csv', '/tl_2018_blockgroup.shp', 'State', 'blockgroup') 
  s<-build.pca(data='/block group data by states/all_states.csv', boundaries='/tl_2018_blockgroup.shp', extent='County', scale='blockgroup2') 
  
  
  t<-readRDS(paste0(wd,"/SVI_County_blockgroup2.rds"))
  t<-readRDS(paste0(wd,"/SVI_State_blockgroup.rds"))
  
  
  
