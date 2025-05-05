# Colin Goodman 2019 / 2020
# Final source.R, CLI prepped
# This script interfaces with the US Census APIs to pull population data.


# https://github.com/walkerke/tidycensus/issues/136

##Packages##

library(tidycensus)
library(tidyverse)
library(sf)
library(dplyr)
library(tryCatchLog)

#setwd("C:\\Users\\Colin\\Desktop\\Research\\nelson_proj")

##Key##

census_api_key("c53a5080701d109ad2d52d51b482a50be585c6b6")

## Input Variables##

population_group <- 'block group' # default
# "tract" "block group" "county"

year <- 2018 #default

args <- commandArgs()

fileConn <- file("output.txt") # for debug
writeLines(args, fileConn)
close(fileConn)

population_group <- args[7]

year <- as.numeric(args[6])

if (args[8] == 'ALL') {
  states <- unique(fips_codes$state)[1:51]
  print("ALL")
} else {
  states = list()
  for (item in 8:length(args)) {
    states[[item-7]] <- trimws(args[item]) # 
  }
}

dimensions <- c('B01001','B01002','B01003','B02001','B03003','B08134','B09002','B09019','B09020','B15003','B17021','B19001','B19055','B19301','B22010','B23025','B25001','B25002','B25007','B25008','B25010','B25033','B25044','B25056','B25064','B25075','B25077','C16002','C24010')
state_total <- length(states)

#setwd("C:/Users/Colin/Desktop/workspace") # for colin's desktop


##Code##

county_or_ct_pull<-function(states){
  
  all<- vector(mode = 'list', length = 0) #placeholder for multiple states data
  
  for(i in 1:length(states)) {
    this_state <- states[i]
    print(this_state)
    #print('State ', i, ' of ', state_total, ' ', this_state, '\n') # for debug
    file_name = paste(this_state,'.csv', sep = '')
    
    complete_state <- vector(mode = 'list', length = 0) # place holder list for 1 county with all desired attributes
    
    # This loop populates 'complete_county'
    for(k in 1:length(dimensions)) { # for every dimension (column/attribute) we want
      this_dimension <- dimensions[k]
      print(paste0(this_dimension, ' on ', this_state, '\n'))
      print(this_dimension)
      #county_instance <- get_acs(geography = population_group, table = this_dimension, state = this_state, county = this_county, cache_table = TRUE) # call API for 1 county on 1 dimension
      #complete_county <- rbind(complete_county, county_instance)
      tryCatch({
        print("TEST")
        state_instance <- get_acs(geography = population_group, table = this_dimension, state = this_state, year = year, cache_table = TRUE) # call API for 1 county on 1 dimension
        complete_state <- rbind(complete_state, state_instance)
      }, error=function(cond) {
        print(paste0('Error encountered',this_state,this_dimension,'\n'))
        state_instance <- c('Null', paste(this_state), this_dimension, '', '') # build state instance with null value, add to list
        complete_state <- rbind(complete_state, state_instance)
      })
      time_res <- strsplit(toString(proc.time() - ptm), " ")
      seconds_elapsed <- time_res[[1]][3]
      print(paste0('time elapsed: ', seconds_elapsed, ' \n'))
    }
    print("attempting to write state to file")
    write.csv(complete_state, file = file_name) # write this state out to a CSV file
    
    all <- rbind(all, complete_state) # add this state to the full list
  }
  if (state_total>1){
    file_name = paste('all.csv', sep = '')
    write.csv(all, file = file_name) # write this state out to a CSV file
  }
}


block_group_pull<-function(states){
  
  all<- vector(mode = 'list', length = 0) #placeholder for multiple states data
  
  for(i in 1:length(states)) {
    this_state <- states[i]
    print(paste0('State', i, 'of', state_total, '(', this_state, ')\n')) # for debug
    file_name = paste(this_state,'.csv', sep = '')
    
    master <- vector(mode = 'list', length = 0) #data in every county of one state, this is where everything ends up
    
    #options(tigris_use_cache=FALSE)
    
    county_state <- tigris::counties( # grabs all counties in the given state
      state = this_state,
      cb = TRUE,
      resolution = "20m",
      year = year,
      class = "sf"
    )
    
    county_total <- nrow(county_state)
    
    
    # This loop iterates for every dimension for every county in a given state. Results get added to list 'master'
    for(j in 1:nrow(county_state)) { 
      #this_county_full <- county_state[j,] #doesn't appear to do anything
      this_county <- county_state[[j,2]] # just grabs county FIPS code
      
      print(paste0(this_county, 'is', j , 'of', county_total , '\n')) # for debug
      
      complete_county <- vector(mode = 'list', length = 0) # place holder list for 1 county with all desired attributes
      
      # This loop populates 'complete_county'
      for(k in 1:length(dimensions)) { # for every dimension (column/attribute) we want
        this_dimension <- dimensions[k]
        print(paste0(this_dimension, ' on ', this_county, '\n'))
        #county_instance <- get_acs(geography = population_group, table = this_dimension, state = this_state, county = this_county,cache_table = TRUE) # call API for 1 county on 1 dimension
        #complete_county <- rbind(complete_county, county_instance)
        tryCatch({
          county_instance <- get_acs(geography = population_group, table = this_dimension, state = this_state, county = this_county, year = year, cache_table = TRUE) # call API for 1 county on 1 dimension
          complete_county <- rbind(complete_county, county_instance)
        }, error=function(cond) {
          print(paste0('Error encountered',this_state,this_county,this_dimension,'\n'))
          county_instance <- c('Null', paste(this_county, ' ', this_state), this_dimension, '', '') # build county instance with null value, add to list
          complete_county <- rbind(complete_county, county_instance)
        })
        time_res <- strsplit(toString(proc.time() - ptm), " ")
        seconds_elapsed <- time_res[[1]][3]
        print(paste0('time elapsed: ', seconds_elapsed, ' \n'))
      }
      
      ##result <- get_acs(geography = "block group", table = "B15003", state = this_state)
      
      master <- rbind(master, complete_county) # add this county to the master list
    }
    
    write.csv(master, file = file_name) # write this state out to a CSV file
    
    #all <- rbind(all, master) # add this state to the full list
    
  }
  
  # if (length(states)>1){
  #   file_name = paste('all.csv', sep = '')
  #   write.csv(all, file = file_name) # write this state out to a CSV file
  # }
} #end of block_group census pull function


ptm <- proc.time() # timing purposes


if (population_group == "block group") {
  
  block_group_pull(states)
  
} else {
  
  county_or_ct_pull(states)
  
}


elapsed <- proc.time() - ptm
print(paste0('Total time: ', elapsed))

##End##
