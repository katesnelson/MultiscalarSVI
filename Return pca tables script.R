## Description: Script to run pca calculations to create social vulnerability indices and return PCA results, 
    ## rerun for each year after changing year and file location inputs 
## Authors: Kate Nelson
## Date: Last modified 12/16/2024


# Setup ----

  wd<-getwd()
  
  library(pacman)
  p_load(tidyverse, sf, tigris, foreach)

  source('create_pca_results_output.R', 'pca_final.R')
  

# Get spatial boundaries ----
  
  states <- states(, year = 2018) 
  
  counties <- counties(, year = 2018)
  
  states_list <- states$GEOID
  
  tracts <- foreach(i = 1:length(states_list), .combine = rbind) %do% {
              t <- tracts(state = states_list[i], cb = TRUE, year = 2018)
  }
  
  blockgroups <- foreach(i = 1:length(states_list), .combine = rbind) %do% {
    t <- block_groups(state = states_list[i], cb = TRUE, year = 2018)
  }



source('create_pca_results_output.R')




# Run pca on county data to return loading tables and fit ----




retrieve.pca(data = 'county2018/all_states.csv', boundaries = counties, extent ='US', scale = 'county', year = '2018') 
retrieve.pca(data = 'county2018/all_states.csv', boundaries = counties, extent = 'State', scale = 'county', year = '2018') 



# Combine and run pca on census tract data to return loading tables and fit ----



retrieve.pca(data = 'tract2018/all_states.csv', boundaries = tracts,  extent ='US', scale ='tract', year = '2018') 
retrieve.pca(data = 'tract2018/all_states.csv', boundaries = tracts, extent = 'State', scale = 'tract', year = '2018') 
retrieve.pca(data = 'tract2018/all_states.csv', boundaries = tracts, extent ='County', scale ='tract', year = '2018') 



#Combine and run pca on block group data  to return loading tables and fit ----



retrieve.pca(data = 'blockgroup2018/all_states.csv', boundaries = blockgroups, extent ='US', scale ='blockgroup', year = '2018') 
retrieve.pca(data = 'blockgroup2018/all_states.csv', boundaries = blockgroups, extent = 'State', scale ='blockgroup', year = '2018') 
retrieve.pca(data = 'blockgroup2018/all_states.csv', boundaries = blockgroups, extent ='County', scale ='blockgroup', year = '2018') 



# Build Master Dataset----


## trial for state extents results ----
  files<-list.files(path="PCA_Results/", pattern= '_State_county') #get list of state extent county res PCAs
  f<-paste0("PCA_Results/",files, sep="")
  state_cnty<-lapply(f, readRDS) #read in all datafiles 
  all_states<-do.call(bind_rows, state_cnty) #merge all states
  
      # get some small n examples
      all_states %>%
        filter(STATEFP == '04') %>% #AZ
        .[[1,1]] #92% cum var explained with n=15 for county res and state extent
      
      all_states %>%
        filter(STATEFP == '09') %>% #CT
        .[[1,1]] #95% cum var explained with n=8


## trial for pulling all results ----
  files<-list.files(path="PCA_Results/") #get list of all PCA results
  f<-paste0("PCA_Results/",files, sep="")
  pcas<-lapply(f, readRDS) #read in all datafiles 
  all_pca<-do.call(bind_rows, pcas) #merge all pcas
  
      # get some examples for small n locations above and new
      all_pca %>%
        filter(STATEFP == '04') %>% #glimpse()
        .[[2, 1]] #68% cum var explained with state extent, tract res with n = 1732
      
      all_pca %>%
        filter(STATEFP == "09") %>% #glimpse()
        .[[2, 1]] #69% cum var explained with state extent, tract res with n = 872 
  

## Actual pull of all results ----
  files<-list.files(path="PCA_Results/") #get list of all PCA results
  f<-paste0("PCA_Results/",files, sep="")
  pcas<-lapply(f, readRDS) #read in all datafiles 
  all_pca<-do.call(bind_rows, pcas) #merge all pcas


  saveRDS(all_pca, "all_pca_results_12172024.rds")  
  

# Get some examples for small n locations above and new ----

  ## Arizona ----
    all_pca %>%
      filter(str_detect(COUNTYFP, '^04')) %>% #glimpse()
      .[[19, 1]] #100% cum var explained with 6 factors for county extent, bg res with n = 6 - 04011 (only avail in 2019)
    
    all_pca %>%
      filter(str_detect(COUNTYFP, '^04')) %>% #glimpse()
      .[[18, 1]] #88% cum var explained with 8 factors for county extent, bg res with n = 18 - 040009(in 2019, avail all years)
   
    all_pca %>%
      filter(str_detect(COUNTYFP, '^04009')) %>% #glimpse()
      .[[1, 1]] #82% cum var explained with 8 factors for county extent, bg res with n = 25 - 040009(in 2018, avail all years) - factors substantivaly similar
    
    ## Connecticut ----
    
    all_pca %>%
      filter(STATEFP == "09") %>% #glimpse()
      .[[5, 1]] #96% cum var explained with 5 components state extent, cnty res in 2018 with n = 8
    
    all_pca %>%
      filter(STATEFP == "09") %>% #glimpse()
      .[[6, 1]] #96% cum var explained with 5 components state extent, cnty res in 2019 with n = 8, substantively similar
    
    all_pca %>%
      filter(STATEFP == "09") %>% #glimpse()
      .[[7, 1]] #98% cum var explained with 6 components state extent, cnty res in 2020 with n = 8, still similar factors but added nursing home


    all_pca %>%
      filter(STATEFP == "09") %>% #glimpse()
      .[[8, 1]] #95% cum var explained with 5 components state extent, cnty res in 2020 with n = 8, still similar factors but nursing home removed and wealth moved up in importance
    
    ## New Mexico ----

      all_pca %>%
      filter(str_detect(COUNTYFP, '^35')) %>% glimpse() # Small n counties to check "35037", 35051, 35053
      
      all_pca %>%
        filter(str_detect(COUNTYFP, '^35037')) %>% #glimpse() 
      .[[1, 1]] #100% cum var explained with 7 factors for county extent, bg res with n = 8 - 35037(2018, avail all years)
    
      all_pca %>%
        filter(str_detect(COUNTYFP, '^35037')) %>% #glimpse() 
        .[[2, 1]] #92% cum var explained with 7 factors for county extent, bg res with n = 11 - 35037(2019, avail all years) factors not really consistent with 2018
      
      all_pca %>%
        filter(str_detect(COUNTYFP, '^35037')) %>% #glimpse() 
        .[[3, 1]] #94% cum var explained with 7 factors for county extent, bg res with n = 10 - 35037(2020, avail all years) factors stable between 2019 and 2020
      
      all_pca %>%
        filter(str_detect(COUNTYFP, '^35037')) %>% #glimpse() 
        .[[4, 1]] #95% cum var explained with 7 factors for county extent, bg res with n = 11 - 35037(2021, avail all years) factors stable between 2020 and 2021
      
      
      all_pca %>%
        filter(str_detect(COUNTYFP, '^35051')) %>% #glimpse() 
        .[[1, 1]] #100% cum var explained with 6 factors for county extent, bg res with n = 7 - 35051(2018, avail all years)
      
      all_pca %>%
        filter(str_detect(COUNTYFP, '^35051')) %>% #glimpse() 
        .[[2, 1]] #100% cum var explained with 6 factors for county extent, bg res with n = 7 - 35051(2019, avail all years) factors not really consistent with 2018
      
      all_pca %>%
        filter(str_detect(COUNTYFP, '^35051')) %>% #glimpse() 
        .[[3, 1]] #97% cum var explained with 7 factors for county extent, bg res with n = 9 - 35051(2020, avail all years) factors not really consistent with 2019
      
      all_pca %>%
        filter(str_detect(COUNTYFP, '^35051')) %>% #glimpse() 
        .[[4, 1]] #100% cum var explained with 5 factors for county extent, bg res with n = 6 - 35051(2021, avail all years) factors not really consistent with 2020
      