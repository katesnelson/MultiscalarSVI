## Description: Functions that do pca calculations to create social vulnerability indices, 
## Authors: Kate Nelson & Colin Goodman (github.com/colingoodman 2020)
## Date: Last modified 02/22/2023


# Setup ----

  wd<-getwd()

  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(tidyverse, sf, psych, foreach, doParallel, rlang, factoextra, flextable) #ggbiplot
  
  
  scale2 <- function(x, na.rm = FALSE) (x - mean(x, na.rm = na.rm)) / sd(x, na.rm)
  
# Read in Data ----
 
  read_in_data <- function(data, boundaries) {
    
    b<-st_read(paste0(wd,boundaries)) #read in spatial boundaries
    b <- b  %>% select(GEOID, STATEFP, COUNTYFP) #%>% mutate(GEOID=as.numeric(as.character(GEOID)))
    
    d<-read.csv(paste0(wd,data)) #read in table of variables
    is.na(d)<-sapply(d, is.infinite)
    d<-d %>% mutate(GEOID = as.character(GEOID)) %>% mutate(GEOID = str_pad(GEOID, width = max(nchar(GEOID)), side="left", pad="0"))
    
    mylist<-list(d,b)
    return(mylist)
  }

# Build SVI using PCA ----
  
  # create the social vulnerability index using PCA
  # data: demographic dataset
  # boundaries: shapefile of geographic area
  # extent: designate data use for nation, state, or county
  # scale: geographic area of input spatial boundary and demographic data 
  # returns index value for all locations in the nation
  
  build.pca <- function(data, boundaries, extent, scale) {
    
    #prep the data
    
    mylist<-read_in_data(data, boundaries) 
    
    d<-mylist[[1]]
    b<-mylist[[2]]
    
    if(extent == "US") {
      
      scaled_d<-d %>% mutate_at(vars(2:ncol(d)), scale2, na.rm=T)
      SVI <- pca_psych(scaled_d)
      
    } else if (extent == "State") {  
      d$STATEFP <- substr(d$GEOID,1,2) 
      ds<- d %>% group_by(STATEFP) %>% group_split(., keep = TRUE)
      
      
      full<-tibble()
      
      foreach (i=1:length(ds)) %do% { # loop for all individual states, then rbind
        
        d_sub<-as.data.frame(ds[i])  
        scaled_d<-d_sub %>% mutate_at(vars(2:(ncol(d_sub)-1)), scale2, na.rm=T)
        
        if (nrow(scaled_d)>5){
          SVI <- pca_psych(scaled_d)
        } else {
          SVI <- data.frame(matrix(ncol=ncol(full), nrow=0))
          colnames(SVI)<-colnames(full)
        }
        
        if (i == 1){ # combining results for states
          full = SVI
        } else {
          full <- rbind(full[,c("SVI","GEOID")], SVI[,c("SVI","GEOID")]) 
        }
      }
      
      SVI <-full # when done compiling SVI by state save to original SVI object
      
    } else if (extent == "County") {
      d$COUNTYFP <- substr(d$GEOID,1,5) 
      ds<- d %>% group_by(COUNTYFP) %>% group_split(., keep = TRUE)
      
      full<-tibble()
      
      
      foreach(i=1:length(ds)) %do% { # loop through all the counties and combine
        
        d_sub <- as.data.frame(ds[i])
        scaled_d<-d_sub %>% mutate_at(vars(2:(ncol(d_sub)-1)), scale2, na.rm=T)
        
        if (nrow(scaled_d)>1){
          SVI <- possibly_pca(scaled_d)
          if (is.na(SVI)) {
            SVI <- data.frame(matrix(ncol=ncol(full), nrow=0))
            colnames(SVI)<-colnames(full)
          }
        } else {
          SVI <- data.frame(matrix(ncol=ncol(full), nrow=0))
          colnames(SVI)<-colnames(full) 
        }
        
        if (i == 1){ # combining results for counties
          full = SVI
        } else {
          full <- rbind(full[,c("SVI","GEOID")], SVI[,c("SVI","GEOID")])
        }
        print(i)
        
      }
      
      SVI <-full #when done compiling SVI by state save to original SVI object
      
    } else {
      stop("Invalid extent")
    }
    
    SVI_sf <- left_join(b,SVI, by=("GEOID"))
    saveRDS(SVI_sf,paste0(wd,"/SVI_", extent, "_",scale,".rds"))
    
    return(SVI_sf)
  }
  
  # function to run pca and return the SVI to build.pca
  
  
  pca_psych <- function(scaled_d) {
    
    
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
    
    fa.diagram(fit)
    
    # get pca factor score for index
    fa_scores <- as.data.frame(fit$scores)
    loadings$variable <- colnames(scaled_d[,c(2:28)])
    
    # determine directionality
    var_dir<-as.data.frame(t(rbind(
      colnames(scaled_d[2:28]),
      c(-1,1,-1,-1,1,1,1,1,1,1,1,1,-1,1,-1,1,1,1,1,1,1,1,1,-1,1,1,1)
    ))) # for each variable assign a direction to its impact on vulnerability
    
    colnames(var_dir) <- c("variable","direction")
    var_dir$variable <- as.character(var_dir$variable)
    var_dir$direction <- as.numeric(as.character(var_dir$direction))
    
    fac_names<-colnames(fa_scores)
    
    fac_dir <- loadings %>% left_join(., var_dir) %>% mutate_at(.,.vars = vars(fac_names), ~(. * direction)) #multiply the variable loading on a factor by it's direction
    
    fac_dir_summ <- fac_dir %>% summarise_at(., .vars=vars(fac_names),  ~sum(.) ) #sum of loadings times direction --> postive values should indicate a net increase in vulnerability
    
    direction <-fac_dir_summ > 0 # logical vector true if increases vulnerability
    direction <- ifelse(direction =="TRUE", 1,-1) #true is +1, false, -1
    
    # combine the factor scores for each location using the directionality
    # larger positive values indicate higher vulnerability
    
    fa_scores_mod <- as.data.frame(mapply("*",fa_scores,direction)) #multiply factor scores in columns by corresponding directionality value
    SVI <- fa_scores_mod %>% mutate(SVI = rowSums(select(., fac_names))) #sum all factor scores for a single record to get a composite social vulnerability index
    
    # add back GEOID
    SVI <- cbind(SVI, scaled_d$GEOID) 
    names(SVI)[names(SVI)== "scaled_d$GEOID"]<-"GEOID"
    
    
    return(SVI)
    
    rm(fit, n, cumvar, loadings,tri, n2,cumvar1, fa_scores, fac_dir, fac_dir_summ, direction, fa_scores_mod)
  }
  
  
 # end of script



