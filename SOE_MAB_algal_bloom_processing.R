#HAB data processing steps for Mid-Atlantic State of the Ecosystem Report 2017-2018
#Please contact Sean Hardison (sean.hardison@noaa.gov) for underlying data. Processed data 
#can be found at the NEFSC ERDDAP page (http://comet.nefsc.noaa.gov/erddap)

#Many thanks to both VIMS and ODU labs for their sampling efforts, as well as to the 
#Virginia Department of Health for supplying the data for this work. 

rm(list = ls())

## Libraries
library(dplyr);library(readxl);
library(data.table);library(tidyr)

#----------------------------2007-2012 HAB Data----------------------------#
HAB_2007_2012 <- read_excel("HAB_data_2007-2012.xlsx")
HAB_2007_2012$date <- as.character(HAB_2007_2012$date)
HAB_2007_2012 <- HAB_2007_2012 %>% filter(!is.na(cells_per_ml)) %>%
  dplyr::rename(cells_per_L = cells_per_ml) %>%
  filter(!is.na(date)) %>%
  mutate(cells_per_L = cells_per_L * 1000, year = format(as.POSIXct(date), "%Y")) %>% 
  mutate(Longitude = abs(as.numeric(Longitude)))

#Create temporary data.frame to merge at end
HAB1 <- data.frame(Date = as.POSIXct(HAB_2007_2012$date), 
                   Latitude = as.numeric(HAB_2007_2012$Latitude), 
                   Longitude = HAB_2007_2012$Longitude,
                   Species = HAB_2007_2012$species,
                   Conc = HAB_2007_2012$cells_per_L,
                   Location = HAB_2007_2012$Waterbody)

#----------------------------2013 HAB Data----------------------------#

#Possibly incomplete or under-reported?

HAB_2013 <- read_excel("Algal Bloom Results - VIMS 28July2013_KSR.xlsx",skip = 6)

HAB_2013 <- HAB_2013 %>% filter(!is.na(cells_per_ml)) %>%
  dplyr::rename(cells_per_L = cells_per_ml) %>%
  mutate(cells_per_L = cells_per_L*1000, year = "2013") %>%
  mutate(decimalLongitude = abs(as.numeric(decimalLongitude)))


HAB2 <- data.frame(Date = HAB_2013$date,
                   Latitude = HAB_2013$decimalLatitude,
                   Longitude = HAB_2013$decimalLongitude,
                   Species = HAB_2013$species,
                   Conc = HAB_2013$cells_per_L,
                   Location = HAB_2013$`Location (i.e. River Bay name)`)

#----------------------------2014 HAB Data----------------------------#
ODU_pre_filt <- read_excel("2014 ODU data.xlsx")
ODU <- ODU_pre_filt %>% 
  mutate(decimalLongitude = abs(as.numeric(decimalLongitude))) %>%
  filter(!is.na(date)) %>%
  mutate(`Cyanobacteria bloom` = as.character(`Cyanobacteria bloom`))

#Convert wide to long format
long <- gather(ODU, species, cells_per_ml, `Karlodinium veneficum`:`Cyanobacteria bloom`, factor_key = TRUE)

#Filter data where conc == 0
HAB_2014 <- long %>% filter(cells_per_ml != 0)

#Remove strings from numeric variables
HAB_2014$species <- sub("[.]"," ", HAB_2014$species)
HAB_2014$cells_per_ml <- gsub("[A-Za-z+//]",'',HAB_2014$cells_per_ml) 
HAB_2014$cells_per_ml <- as.numeric(HAB_2014$cells_per_ml)

#Temp df
HAB3 <- HAB_2014 %>% 
  mutate(cells_per_L = cells_per_ml*1000, Latitude = decimalLatitude, Longitude = decimalLongitude) %>%
  select(-c(Month, decimalLatitude, decimalLongitude, cells_per_ml)) %>%
  rename(Date = date, Species = species, Conc = cells_per_L)
HAB3$Location <- NA

#No 2015 data

#----------------------------2016 HAB Data----------------------------#
HAB_2016 <- read_excel("HAB_MAP_Data_2016.xlsx")

#Merge some of the species names where ambiguous - moved to the genus level
HAB_2016 <- HAB_2016 %>% mutate(species= 
                                  plyr::mapvalues(species, 
                                                  from = c("Eugelna sanguinea",
                                                           "Microcystin aeruginosa",
                                                           "Microcystis aeruginosa",
                                                           "Alexandrium monilatum-likely",
                                                           "Alexandrium monilatum"),
                                                  to = c("Eugelena spp.", 
                                                         "Microcystis spp.",
                                                         "Microcystis spp.",
                                                         "Alexandrium spp.",
                                                         "Alexandrium spp.")))

#Remove extraneous text
HAB_2016$cells_per_ml <- gsub('[a-zA-Z+<>]','',HAB_2016$cells_per_ml)

HAB_2016 <- HAB_2016 %>%
  filter(!is.na(cells_per_ml)) %>%
  mutate(cells_per_L = as.numeric(cells_per_ml)*1000, year = format(date, "%Y")) %>%
  mutate(decimalLongitude = abs(as.numeric(decimalLongitude)))

HAB4 <- data.frame(Date = HAB_2016$date,
                   Latitude = HAB_2016$decimalLatitude,
                   Longitude = HAB_2016$decimalLongitude,
                   Species = HAB_2016$species,
                   Conc = HAB_2016$cells_per_L,
                   Location = HAB_2016$`Sample Location`)

#Bind all temp data.frames into final
HAB_df <- rbind(HAB1, HAB2, HAB3, HAB4)

#write.csv(HAB_df,"Chesapeake_Bay_algal_blooms.csv")

