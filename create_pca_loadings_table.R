## Description: Functions that do pca calculations to create social vulnerability indices but return only loading tables, 
## Authors: Kate Nelson & Colin Goodman (github.com/colingoodman 2020)
## Date: Last modified 12162024

# Setup ----

  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(tidyverse, sf, psych, foreach, doParallel, rlang, factoextra, flextable) #ggbiplot

  scale2 <- function(x, na.rm = FALSE) (x - mean(x, na.rm = na.rm)) / sd(x, na.rm)

# Read in data ----
  
  read_in_data <- function(data, boundaries) {
    
    b<-boundaries 
    b <- b  %>% select(GEOID, STATEFP, COUNTYFP) 
    
    d<-read.csv(data) #read in table of variables
    is.na(d)<-sapply(d, is.infinite)
    d<-d %>% mutate(GEOID = as.character(GEOID)) %>% mutate(GEOID = str_pad(GEOID, width = max(nchar(GEOID)), side="left", pad="0"))
    
    mylist<-list(d,b)
    return(mylist)
  }

# Run PCA ----
  
  # run pca using psych package
  pca_psych <- function(scaled_d, extent, scale, year) {
    
    
    fit <- principal(scaled_d[,c(2:28)], nfactors = 27, rotate = "varimax", impute = "median", missing = TRUE, use="na.or.complete")
    
    # optimize by eigenvalue 
    n <- fit$values > 1
    fit <- principal(scaled_d[,c(2:28)], nfactors=sum(n), rotate="varimax", impute="median", missing=TRUE,  use="na.or.complete")
    
    # summary(fit)
    # fa.diagram(fit)
    cumvar <- fit$Vaccounted[3,sum(n)]
    
    # optimize by number of vars loading on each component
    loadings <- as.data.frame(unclass(fit$loadings))
    trim <- loadings %>% mutate_all(~abs(.)) %>% select_if(~sum(. > 0.4,na.rm=TRUE) < 2) # components to remove based on only one var strongly loading
    n2 <- length(trim)
    fit <- principal(scaled_d[,c(2:28)], nfactors=sum(n)-n2, rotate="varimax", impute="median", missing=TRUE, use="na.or.complete")
    loadings <- as.data.frame(unclass(fit$loadings))
    cumvar1 <- fit$Vaccounted[3,sum(n)-n2]
    vartable <- as.data.frame(fit$Vaccounted)
    
    # Create a report out table for each PCA ----
    
    report_table <- bind_rows(loadings, vartable)
    
    # Create a tibble for holding reporting tables and identifier info ----
    
    STATEFP = scaled_d$STATEFP[1] # get location identifier info applicable to limited extent models
    COUNTYFP = scaled_d$COUNTYFP[1]
    
    
    master_tibble <- tibble(
      report_table = list(report_table),
      extent = list(extent),
      resolution = list(scale),
      year = list(year),
      STATEFP = list(STATEFP),
      COUNTYFP = list(COUNTYFP),
      sample_size = list(nrow(scaled_d))
    )
    
   saveRDS(master_tibble,paste0("PCA_Results/Tibble_", extent, "_",scale, "_", year, "_", STATEFP, "_", COUNTYFP, ".rds"))
    

  rm(fit, n, cumvar, loadings, n2,cumvar1)
}


# Retrieve PCA loading tables
  

  # data: demographic dataset
  # boundaries: shapefile of geographic area
  # extent: designate data use for nation, state, or county
  # scale: geographic area of input spatial boundary and demographic data 
  # returns index value for all locations in the nation
  
  retrieve.pca <- function(data, boundaries, extent, scale, directionality, year) {
  
    mylist<-read_in_data(data, boundaries) 
    
    d<-mylist[[1]]
    b<-mylist[[2]]
    
    if(extent == "US") {
      
      d_sub <- d %>% 
        mutate(missings = rowSums(.[2:28] == 0, na.rm = T) + rowSums(is.na(.))) %>%
        filter(missings <= 13) %>%
        dplyr::select(-missings) #drop records where the official census records have no information for most variables.
      
      if(any(d_sub %>% dplyr::select(QAGEDEP:QUNOCCHU) > 1, na.rm =T)) { #identify if some data might be missing from data pull and drop suspect records
        print(paste0("Potential missing data - percent greater than 100 detected - in ", data, ". Offending records dropped."))
        d_sub <- d_sub %>%
          filter(rowSums(.[7:28] > 1, na.rm = T) < 1)
       
      }
      
      scaled_d <- d_sub %>% mutate_at(vars(2:ncol(d)), scale2, na.rm=T)
     
      SVI <- pca_psych(scaled_d, extent, scale, year)
      
    } else if (extent == "State") {  
           d$STATEFP <- substr(d$GEOID,1,2) 
      
      d_sub <- d %>% 
        mutate(missings = rowSums(.[2:28] == 0, na.rm = T) + rowSums(is.na(.))) %>%
        filter(missings <= 13) %>%
        dplyr::select(-missings) #drop records where the official census records have no information for most variables.
      
      if(any(d_sub %>% dplyr::select(QAGEDEP:QUNOCCHU) > 1, na.rm =T)) { #identify if some data might be missing from data pull and drop suspect records
        print(paste0("Potential missing data - percent greater than 100 detected - in ", data, ". Offending records dropped."))
        d_sub <- d_sub %>%
          filter(rowSums(.[7:28] > 1, na.rm = T) < 1)
        
      }
      
      ds<- d_sub %>% group_by(STATEFP) %>% group_split(., .keep = TRUE)
      
      
      full<-tibble()
      
      foreach (i=1:length(ds)) %do% { # loop for all individual states, then rbind
  
        d_sub<-as.data.frame(ds[[i]])  
        scaled_d<-d_sub %>% mutate_at(vars(2:(ncol(d_sub)-1)), scale2, na.rm=T)
        
        
        if (nrow(scaled_d)>5){
          SVI <- pca_psych(scaled_d, extent, scale, year)
        } 
        
       
      }
      
      
      
    } else if (extent == "County") {
      d$COUNTYFP <- substr(d$GEOID,1,5) 
      
      d_sub <- d %>% 
        mutate(missings = rowSums(.[2:28] == 0, na.rm = T) + rowSums(is.na(.))) %>%
        filter(missings <= 13) %>%
        dplyr::select(-missings) #drop records where the official census records have no information for most variables.
      
      if(any(d_sub %>% dplyr::select(QAGEDEP:QUNOCCHU) > 1, na.rm =T)) { #identify if some data might be missing from data pull and drop suspect records
        print(paste0("Potential missing data - percent greater than 100 detected - in ", data, ". Offending records dropped."))
        d_sub <- d_sub %>%
          filter(rowSums(.[7:28] > 1, na.rm = T) < 1)
       
      }
      
      
      ds<- d_sub %>% group_by(COUNTYFP) %>% group_split(., .keep = TRUE)
        
      full<-tibble()
     
      
        foreach(i=1:length(ds)) %do% { # loop through all the counties and combine
          
          d_sub <- as.data.frame(ds[[i]])
          scaled_d<-d_sub %>% 
            mutate_at(vars(2:(ncol(d_sub)-1)), scale2, na.rm=T) %>%
            drop_na()
          
          if (nrow(scaled_d)>5){
            SVI <- pca_psych(scaled_d, extent, scale, year)
            
        }
        
        
        
    } else {
      stop("Invalid extent")
    }
    

 }} # end of script

