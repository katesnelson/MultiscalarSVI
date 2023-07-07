
#function should create a social vulnerability index using principal components analysis (pca)
# `data` argument should refer to a demographic dataset as compiled from Colin's script
# `boundaries` argument should refer to a shapefile of block-group, census-tract, or county boundaries
# `extent`` argument indicates if the pca should use all data from the nation, just data from within states, or just data from within counties. 
# These extent choices change the variance within the data used for the statistical pca analysis and ultimately produce different forms of the index across locations.
# `scale` argument indicates if the input spatial boundary and demographic data is at the block-group, census-tract, or county scale
# all forms of the function produce a index value for all locations across the US


#Should probably just create a function for the index creating process to call within the multiscale process so it's cleaner


wd<-"C:/Users/ksnelson/OneDrive - Kansas State University/Intersection of Social Vulnerability and Flood/" #temporary for fcn testing
setwd(wd)

build.pca<-function(data, boundaries, extent, scale){

#modify the library calls 
  
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, sf, psych, foreach, doParallel, rlang, factoextra, flextable, ggbiplot)

#I think these packages, which are also above in the p_load call may not be neccesary in the function form of this script
# library(ggbiplot)
# library(factoextra)
# library(rlang)
# library(flextable)




#####################
### Read in Data ###
####################

#need to add in an `if` statement to read in different spatial boundary files based on `scale` argument then adjust downstream dependencies

cbg<-st_read("cbg_16/US_blck_grp_2016.shp") #read in spatial boundaries, temporary for fcn testing
cbg$GEOID2<-as.numeric(as.character(cbg$GEOID))

#need to update this later to refer to the `data` argument
d<-read.csv(paste0(wd,"attributes_1127.csv")) #read in table of variables
is.na(d)<-sapply(d, is.infinite)

if (extent =="US"){

scale2 <- function(x, na.rm = FALSE) (x - mean(x, na.rm = na.rm)) / sd(x, na.rm)
scaled_d<-d %>% mutate_at(vars(3:29), scale2, na.rm=T)
#d<-d[!is.na(d),]


############################
#Run PCA using psych package
##############################


    fit <- principal(scaled_d[,c(3:29)], nfactors=26, rotate="varimax", impute="median", missing=TRUE)
    
  #optimize pca by eigenvalue  
    n<-fit$values > 1
    fit <- principal(scaled_d[,c(3:29)], nfactors=sum(n), rotate="varimax", impute="median", missing=TRUE)
    summary(fit)
    fa.diagram(fit)
    cumvar<-fit$Vaccounted[3,sum(n)]
 
 #optimize pca by eigenvalue and number of vars loading on each components   
    loadings<-as.data.frame(unclass(fit$loadings))
    trim<-loadings %>% mutate_all(~abs(.)) %>% select_if(~sum(. > 0.4,na.rm=TRUE) < 2) #components to remove based on only one var strongly loading
    n2<-length(trim)
    fit <- principal(scaled_d[,c(3:29)], nfactors=sum(n)-n2, rotate="varimax", impute="median", missing=TRUE)
    loadings<-as.data.frame(unclass(fit$loadings))
    cumvar1<-fit$Vaccounted[3,sum(n)-n2]
    

  #get pca factor score for index
    fa_scores<-as.data.frame(fit$scores)
    loadings$variable<-colnames(scaled_d[,c(3:29)])
    
  #determine directionality
    
    var_dir<-as.data.frame(t(rbind(
              colnames(d[3:length(d)]),
               c(-1,1,-1,-1,1,  #for each variable assign a direction to its impact on vulnerability (e.g. higher percent mobile homes increases vulnerability so --> "1")
               1,1,1,1,1,
               1,1,-1,1,-1,
               1,1,1,1,1,
               1,1,1,-1,1,
               1,1))))
    colnames(var_dir)<-c("variable","direction")
    var_dir$variable<-as.character(var_dir$variable)
    var_dir$direction<-as.numeric(as.character(var_dir$direction))
    
    
    fac_names<-colnames(fa_scores)
    ##Fix the line below. Should multiply by -1 only if direction is not what it should be based on var_dir.
   fac_dir<-loadings %>% left_join(., var_dir) %>% mutate_at(.,.vars = vars(fac_names), ~(. * direction)) #multiply the variable loading on a factor by it's direction
   fac_dir_summ<- fac_dir %>% summarise_at(., .vars=vars(fac_names),  ~sum(.) ) #sum of loadings times direction --> postive values should indicate a net increase in vulnerability
   
   direction<-fac_dir_summ > 0 #logical vector true if increases vulnerability
   direction<- ifelse(direction =="TRUE", 1,-1) #true is +1, false, -1
  
   #Now combine the factor scores for each location using the directionality jsut determined so that larger positive values indicate more vulnerable and visa versa
   
   fa_scores_mod<-as.data.frame(mapply("*",fa_scores,direction)) #multiply factor scores in columns by corresponding directionality value
   SVI <- fa_scores_mod %>% mutate(SVI = rowSums(select(., fac_names))) #sum all factor scores for a single record to get a composite social vulnerability index
 
  #Now add back the geoid, add geometries, and plot
   
   SVI<-cbind(SVI, d$GEOID)
   

   # cbg<-st_read("cbg_16/US_blck_grp_2016.shp")
   
   # cbg$GEOID2<-as.numeric(as.character(cbg$GEOID))
   
   
   SVI_sf<-left_join(cbg[,c("GEOID2","geometry")], SVI, by=c("GEOID2"="d$GEOID"))
    
   
   saveRDS(SVI_sf, paste0("SVI_", extent, "_",scale,".rds"))
   # plot(SVI_sf[, "SVI"])
   
} else {
  
  if (extent = "State") {
    
    d$STATEFP <- substr(d$GEOID,1,2) #for other extents need to update the `substr` call, use https://www.census.gov/programs-surveys/geography/guidance/geo-identifiers.html for reference
    ds<- d %>% group_by(STATEFP) %>% group_split(., keep = TRUE)
    
    full_sf<-tibble()
    
    foreach (i=1:length(ds)) %do% {
    
    d_sub<-as.data.frame(ds[i])  
    scale2 <- function(x, na.rm = FALSE) (x - mean(x, na.rm = na.rm)) / sd(x, na.rm)
    scaled_d<-d_sub %>% mutate_at(vars(3:29), scale2, na.rm=T)
    #d<-d[!is.na(d),]
    
    
    ############################
    #Run PCA using psych package
    ##############################
    
    
    fit <- principal(scaled_d[,c(3:29)], nfactors=26, rotate="varimax", impute="median", missing=TRUE)
    # VSS.scree(scaled_d[,c(3:29)])
    # fit
    # loadings<-as.data.frame(unclass(fit$loadings))
    # regulartable(loadings)
    # abs_loadings<-abs(loadings)
    # n<-colnames(abs_loadings)
    # abs_loadings<-abs_loadings %>% mutate(max_load=do.call(pmax,(.))) #get the strongest loading for each variable
    # summary(fit)
    
    
    #optimize pca by eigenvalue  
    n<-fit$values > 1
    fit <- principal(scaled_d[,c(3:29)], nfactors=sum(n), rotate="varimax", impute="median", missing=TRUE)
    # summary(fit)
    # fa.diagram(fit)
    cumvar<-fit$Vaccounted[3,sum(n)]
    
    #optimize pca by eigenvalue and number of vars loading on each components   
    loadings<-as.data.frame(unclass(fit$loadings))
    trim<-loadings %>% mutate_all(~abs(.)) %>% select_if(~sum(. > 0.4,na.rm=TRUE) < 2) #components to remove based on only one var strongly loading
    n2<-length(trim)
    fit <- principal(scaled_d[,c(3:29)], nfactors=sum(n)-n2, rotate="varimax", impute="median", missing=TRUE)
    loadings<-as.data.frame(unclass(fit$loadings))
    # summary(fit)
    # fa.diagram(fit)
    cumvar1<-fit$Vaccounted[3,sum(n)-n2]
    
    
    #get pca factor score for index
    fa_scores<-as.data.frame(fit$scores)
    loadings$variable<-colnames(scaled_d[,c(3:29)])
    
    #determine directionality
    
      var_dir<-as.data.frame(t(rbind(
        colnames(d[4:length(d)-1]),
        c(-1,1,-1,-1,1,  #for each variable assign a direction to its impact on vulnerability (e.g. higher percent mobile homes increases vulnerability so --> "1")
          1,1,1,1,1,
          1,1,-1,1,-1,
          1,1,1,1,1,
          1,1,1,-1,1,
          1,1))))
      colnames(var_dir)<-c("variable","direction")
      var_dir$variable<-as.character(var_dir$variable)
      var_dir$direction<-as.numeric(as.character(var_dir$direction))
      
      ##Kate - Fix below. Should multiply by -1 only if direction is not what it should be based on var_dir.
      
      fac_names<-colnames(fa_scores)
          fac_dir<-loadings %>% left_join(., var_dir) %>% mutate_at(.,.vars = vars(fac_names), ~(. * direction)) #multiply the variable loading on a factor by it's direction
      fac_dir_summ<- fac_dir %>% summarise_at(., .vars=vars(fac_names),  ~sum(.) ) #sum of loadings times direction --> postive values should indicate a net increase in vulnerability
      
      direction<-fac_dir_summ > 0 #logical vector true if increases vulnerability
      direction<- ifelse(direction =="TRUE", 1,-1) #true is +1, false, -1
    
    #Now combine the factor scores for each location using the directionality just determined so that larger positive values indicate more vulnerable and visa versa
    
      fa_scores_mod<-as.data.frame(mapply("*",fa_scores,direction)) #multiply factor scores in columns by corresponding directionality value
      SVI <- fa_scores_mod %>% mutate(SVI = rowSums(select(., fac_names))) #sum all factor scores for a single record to get a composite social vulnerability index
      
    #Now add back the geoid and add geometries
    
      SVI<-cbind(SVI, d_sub$GEOID)
    
      cbg2 <- cbg %>% filter(GEOID2 %in% d_sub$GEOID)
    
      SVI_sf<-left_join(cbg2[,c("GEOID2","geometry")], SVI[ ,c("d_sub$GEOID","SVI")], by=c("GEOID2"="d_sub$GEOID")) #subsetting to just final SVI column for now (see below)
    
    #Combine results for all states
      
    if(i == 1){
      full_sf = SVI_sf
    
      } else {
      full_sf <- rbind(full_sf, SVI_sf) # with full PCA results is failing because some states have more components then others after optimization, would ideally like to have all information, but for now subset to just final combined score in line above
    }
    }
    
     saveRDS(full_sf, paste0("SVI_", extent, "_",scale,".rds"))
   # plot(full_sf[, "SVI"])
  }

  
  }
     
} #end function
     
     