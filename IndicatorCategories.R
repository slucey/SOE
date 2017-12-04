# assign indicator categories for analysis
# S Gaichas June 2017

rm(list=ls())

library(tidyverse)

setwd("~/Documents/0_Data/ICES_WGNARS/2017 meeting")

ind <- load("~/Documents/0_Data/ICES_WGNARS/2017 meeting/SOE_data.RData")
#ind <- load("~/Data/Projects/ICES_WGNARS/2017 meeting/SOE_data.RData")

# define the categories for each indicator. 

by_var2 <- SOE.data %>%
  group_by(Var) %>%
  summarise(Nyrs=n())

by_var_all_cat <- by_var2 %>%
  mutate(IndCat = case_when(
    grepl("NAO|AMO|Gulf Stream Index|PDO|NEC ATSW|NEC LSSW|AMOC|sea level", .$Var)~"Climate",
    grepl("Zooplankton|Calanus|copepods|copeopods|Centropages|Pseudocalanus|area occupied", .$Var)~"Zoop",
    grepl("Stratification|temp|Temp|alinity|SST|thermal|sst ", .$Var)~"Phys",
    grepl("forecast", .$Var)~"PhysFore",
    grepl("whale|tern", .$Var)~"PET",
    grepl("aquaculture", .$Var)~"Aquacult",
    grepl("RS anomoly", .$Var)~"FishRecr",
    grepl("male|female|combined", .$Var)~"FishCond",
    grepl("ESn", .$Var)~"SurvDiv",
    grepl("Apex Spring|Apex Fall|ivore Spring|ivore Fall|Benthos Spring|Benthos Fall|Other Spring|Other Fall", .$Var)~"SurvBio", #leaves out NA
    grepl("fleet|angler|commercial|Rec ", .$Var)~"Human",
    grepl("Landings|Revenue", .$Var)~"Fishery",
    grepl("PPD", .$Var)~"PrimProd",
    grepl(" DEPTH | LAT | LON |ASDIST|DTEOC", .$Var)~"FishDist"
  ))
  
# join with SOE.data to make new column of categories

SOE.data.cat <- merge(SOE.data, by_var_all_cat)
SOE.data.cat <- subset(SOE.data.cat, select = -Nyrs)

# testing
#dim(SOE.data.cat[is.na(SOE.data.cat$IndCat)])
#SOE.data.cat[is.na(SOE.data.cat$IndCat)]

# make datasets by category to do separately with clustering function
# wide dataset for cluster code
# AMOC times break this so leave it out for now in the filter
# change filter for each category
#Climwide <- SOE.data.cat %>%
#  filter(IndCat == "Climate") %>%
#  select(Time, Var, Value) %>%
#  #filter(Time>1967 & Time<2016) %>%
#  filter(!grepl("AMOC", Var)) %>%
#  spread(Var, Value) 

# get rid of rows with NAs--limits to years common to all indices within a category
#Climwide <- Climwide[ rowSums(is.na(Climwide)) == 0, ]

# need to have relative not absolute scales
#Climwide_s <- Climwide %>% 
#  mutate_each_(funs(scale(.) %>% as.vector), vars(-Time))

# generalize to change filter for each category
cat<-unique(SOE.data.cat$IndCat)
for(cc in cat){
  if(!is.na(cc)){
    ccdat <- SOE.data.cat %>%
      filter(IndCat == cc) %>%
      select(Time, Var, Value) %>%
      filter(!grepl("OTHER|AMOC| NA |GOM  tern|Longterm|GBK temperature|GOM temperature|MAB temperature|NES temperature|SCS temperature", Var)) %>%
      spread(Var, Value) 
    
    # get rid of rows with NAs--limits to years common to all indices within a category
    ccdat <- ccdat[ rowSums(is.na(ccdat)) == 0, ]
    
    # need to have relative not absolute scales
    ccdat_s <- ccdat %>% 
      mutate_each_(funs(scale(.) %>% as.vector), vars(-Time))
    
    assign(paste0(cc,"_wide"),ccdat)
    assign(paste0(cc,"_wide_s"),ccdat_s)
  }
}

# from Catalina's HCA_largescales.R
library(pvclust)

HCA2 <- function(x) {
  #ii = unique(x$ID) #data not set up with scale ID yet
  ii = deparse(substitute(x)) #use data object name in file
  #for(i in 1:length(ii)) {
  #xi = subset(x,ID==ii[i],select=names(x[, !names(x) %in% c("ID")]) )
  xi = subset(x,select=names(x[, !names(x) %in% c("Time")]) ) #no ID column
  #mypath <- file.path("output","analysis","HCA",paste("HCA_", ii[i], ".pdf", sep = ""))
  #mypath2 <- file.path("output","analysis","HCA",paste("HCA_", ii[i], ".txt", sep = ""))
  mypath <- file.path(paste("HCA_", ii, ".pdf", sep = ""))
  mypath2 <- file.path(paste("HCA_", ii, ".txt", sep = ""))
  #pdf(mypath, width=13, height=9)
  pdf(mypath, width=24, height=16)
  
  fit <- pvclust(xi, method.hclust="complete",
                 method.dist="euclidean", nboot = 10000) #nboot = 10000 or 100000
  #plot(fit, print.num=F,col.pv=c('red','white'), main= unique(ii[i]), float=0.01) # dendogram with p values
  plot(fit, print.num=F,col.pv=c('red','white'), float=0.01) # dendogram with p values
  pvrect(fit, alpha=.95, border=3, pv="au", lwd=2)
  #out=pvpick(fit,alpha=.95, pv="au", type="geq", max.only=TRUE)
  op=pvpick(fit,alpha=.95, pv="au", type="geq", max.only=TRUE)$clusters
  #op=pvpick(fit,alpha=.95, pv="au", type="geq", max.only=FALSE)$clusters
  dump("op",file=mypath2)
  dev.off()
  #}
}

# preliminary cladograms for discussion with Gavin, June 14
HCA2(Climate_wide_s)
HCA2(Human_wide_s)
HCA2(PET_wide_s)
HCA2(Fishery_wide_s)
HCA2(FishCond_wide_s)
HCA2(FishDist_wide_s)
HCA2(Phys_wide_s)
HCA2(PrimProd_wide_s)
HCA2(SurvBio_wide_s)
HCA2(SurvDiv_wide_s)
HCA2(Zoop_wide_s)
HCA2(PhysFore_wide_s)

