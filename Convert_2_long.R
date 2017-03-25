#Create master data set for SOE
#Long format for simplified graphs
if(Sys.info()['sysname'] == "Windows") prefix <- 'Z:'
if(Sys.info()['sysname'] == "Linux")   prefix <- '/home/slucey/EcoAP'

data.dir <- file.path(prefix, 'SOE2017', 'data')

library(data.table)

w2l <- function(x, by, by.name = 'Time', value.name = 'Value'){
  x.new <- copy(x)
  var.names <- names(x)[which(names(x) != by)]
  out <- c()
  setnames(x.new, by, 'by')
  for(i in 1:length(var.names)){
    setnames(x.new, var.names[i], 'V1')
    single.var <- x.new[, list(by, V1)]
    single.var[, Var := var.names[i]]
    out <- rbindlist(list(out, single.var))
    setnames(x.new, 'V1', var.names[i])
  }
  setnames(x.new, 'by', by)
  setnames(out, c('by', 'V1'), c(by.name, value.name))
}

#Shell for all data
SOE.data <- c()

#Read in data from various collaborators
#Climate
climate <- as.data.table(read.csv(file.path(data.dir, '2017_SOE_SABA_CLIMATE_TIME_SERIES_DATA.csv')))
climate.long <- w2l(climate, 'Year')
#fix AMOC variable
AMOC <- climate.long[Var == 'Year.1', ]
AMOC[, c('Time', 'Var') := NULL]
setnames(AMOC, 'Value', 'V1')
AMOC <- cbind(AMOC, climate.long[Var == 'AMOC'])
AMOC[, Time := NULL]
setnames(AMOC, 'V1', 'Time')
climate.fixed <- climate.long[!Var %in% c('Year.1', 'AMOC')]
climate.fixed <- rbindlist(list(climate.fixed, AMOC))
#Remove NA values
climate.fixed <- climate.fixed[!is.na(Value), ]
#Fix Variable names
climate.fixed[Var == 'Gulf.Stream.Index', Var := 'Gulf Stream Index']
climate.fixed[Var == 'NEC.ATSW', Var := 'NEC ATSW']
climate.fixed[Var == 'NEC.LSSW', Var := 'NEC LSSW']
#Add units
climate.fixed[Var %in% c('NAO', 'AMO', 'PDO'), Units := 'unitless']
climate.fixed[Var == 'Gulf Stream Index', Units := 'degrees latitude']
climate.fixed[Var == 'AMOC', Units := 'Sv']
climate.fixed[Var %in% c('NEC ATSW', 'NEC LSSW'), Units := 'percent composition']

SOE.data <- rbindlist(list(SOE.data, climate.fixed))

#Zooplankton
load(file.path(data.dir, 'biovolume_anomaly.rdata'))
biovolume <- as.data.table(biovol)
biovolume[, Year := as.numeric(row.names(biovol))]
biovol.long <- w2l(biovolume, 'Year')
biovol.long[, Var := paste(Var, 'Zooplankton Biovolume')]
biovol.long[, Units := 'anomaly']
biovol.long <- biovol.long[!is.na(Value), ]

SOE.data <- rbindlist(list(SOE.data, biovol.long))

load(file.path(data.dir, 'Calanus_anomaly.rdata'))
calfin <- as.data.table(Calfin)
calfin[, Year := as.numeric(row.names(Calfin))]
calfin.long <- w2l(calfin, 'Year')
calfin.long[, Var := paste(Var, 'Calanus Anomaly')]
calfin.long[, Units := 'anomaly']
calfin.long <- calfin.long[!is.na(Value), ]

SOE.data <- rbindlist(list(SOE.data, calfin.long))

load(file.path(data.dir, 'Spring_Calanus_log_abundance.rdata'))
calfin.2 <- as.data.table(calfin.log.abund)
calfin.2[, Year := as.numeric(row.names(calfin.log.abund))]
calfin.long.2 <- w2l(calfin.2, 'Year')
calfin.long.2[, Var := paste(Var, 'Calanus log abundance')]
calfin.long.2[, Units := 'log abundance per 100m^-3']
calfin.long.2 <- calfin.long.2[!is.na(Value), ]

SOE.data <- rbindlist(list(SOE.data, calfin.long.2))

load(file.path(data.dir, 'FALL_Calanus_log_abundance.rdata'))
calfin.3 <- as.data.table(calfin.fall.log.abund)
calfin.3[, Year := as.numeric(row.names(calfin.fall.log.abund))]
calfin.long.3 <- w2l(calfin.3, 'Year')
calfin.long.3[, Var := paste(Var, 'Calanus log abundance')]
calfin.long.3[, Units := 'log abundance per 100m^-3']
calfin.long.3 <- calfin.long.3[!is.na(Value), ]

SOE.data <- rbindlist(list(SOE.data, calfin.long.3))

load(file.path(data.dir, 'Pse_Cty_Cham_Tlon_SMcopepods_anomaly.rdata'))
small <- as.data.table(small.copepods)
small[, Year := as.numeric(row.names(small.copepods))]
small.long <- w2l(small, 'Year')
small.long[, Var := paste(Var, 'small copeopods anomaly')]
small.long[, Units := 'anomaly']
small.long <- small.long[!is.na(Value), ]

SOE.data <- rbindlist(list(SOE.data, small.long))

load(file.path(data.dir, 'Zooplankton_KDE_EPU_area_coverage.rdata'))
for(i in 1:10){
  area <- as.data.table(EPU.area[[i]][[1]])
  area[, Year := as.numeric(row.names(EPU.area[[i]][[1]]))]
  area.long <- w2l(area, 'Year')
  grep('Fall', names(EPU.area[[i]]))
  var.name <- names(EPU.area)[i]
  var.name <- sub("_", " ", var.name)
  area.long[, Var := paste(var.name, 'area occupied', Var)]
  area.long[, Units := 'km^2']
  area.long <- area.long[!is.na(Value), ]
  
  SOE.data <- rbindlist(list(SOE.data, area.long))
}

#Physical
physical <- as.data.table(read.csv(file.path(data.dir, 'PhysicalUpdated2015_SML_cleaned.csv')))
physical.long <- w2l(physical, 'Year')
physical.long <- physical.long[!is.na(Value), ]

#Fix variable names
physical.long[Var == 'Stratification..0.50.m..MAB_core', Var := 'Stratification (0-50m) MAB core']
physical.long[Var == 'Stratification..0.50.m..GB_core', Var := 'Stratification (0-50m) GB core']
physical.long[Var == 'Stratification..0.50.m..GOM_core', Var := 'Stratification (0-50m) GOM core']
physical.long[Var == 'Stratification..0.50.m..SS_core', Var := 'Stratification (0-50m) SS core']
physical.long[, Var := gsub("\\.", " ", Var)]
#Add units
physical.long[Var %like% 'temp', Units := 'degrees C']

SOE.data <- rbindlist(list(SOE.data, physical.long))

ersst <- as.data.table(read.csv(file.path(data.dir, 'ERSST.csv')))
setnames(ersst, c('Year', 'Unit'), c('Time', 'Units'))
setcolorder(ersst, names(SOE.data))
ersst[, Units := 'degrees C']

SOE.data <- rbindlist(list(SOE.data, ersst))

thermal <- as.data.table(read.csv(file.path(data.dir, 'SOE th_area_nes.csv')))
#rename variables
thermal[Var == 'ColdNES', Var := 'NES Cold thermal habitat']
thermal[Var == 'MidNES',  Var := 'NES Mid thermal habitat']
thermal[Var == 'WarmNES', Var := 'NES Warm thermal habitat']
#Change units
setnames(thermal, c('Year', 'Unit'), c('Time', 'Units'))
thermal[, Units := 'km^2']
setcolorder(thermal, names(SOE.data))

SOE.data <- rbindlist(list(SOE.data, thermal))

ersst.2 <- as.data.table(read.csv(file.path(data.dir, 'SOE_Annual_ERSST.csv')))
#fix columns/units
setnames(ersst.2, c('Year', 'Unit'), c('Time', 'Units'))
ersst.2[, Units := 'degrees C']
setcolorder(ersst.2, names(SOE.data))

SOE.data <- rbindlist(list(SOE.data, ersst.2))

longterm.sd <- as.data.table(read.csv(file.path(data.dir, 'ltsd.csv')))
longterm.sd[, DATE := NULL]
sd.long <- w2l(longterm.sd, 'DOY')
#change variable names
sd.long[, Var := paste(Var, 'Longterm temperature SD')]
sd.long[, Units := 'unitless']

SOE.data <- rbindlist(list(SOE.data, sd.long))

longterm.mean <- as.data.table(read.csv(file.path(data.dir, 'ltmean.csv')))
longterm.mean[, DATE := NULL]
mean.long <- w2l(longterm.mean, 'DOY')
#change variable names
mean.long[, Var := paste(Var, 'Longterm temperature mean')]
mean.long[, Units := 'degrees C']

SOE.data <- rbindlist(list(SOE.data, mean.long))

current <- as.data.table(read.csv(file.path(data.dir, 'currentyr.csv')))
current[, DATE := NULL]
current.long <- w2l(current, 'DOY')
#change variable names
current.long[, Var := paste(Var, 'temperature')]
current.long[, Units := 'degrees C']

SOE.data <- rbindlist(list(SOE.data, current.long))

forecast <- as.data.table(read.csv(file.path(data.dir, 'forecast_data.csv')))
epu <- unique(forecast[, Ecoreg])
for(i in 1:4){
  epu.forecast <- forecast[Ecoreg == epu[i], ]
  epu.forecast[, c('Ecoreg', 'Date') := NULL]
  epu.long <- w2l(epu.forecast, 'Dseq')
  epu.long[, Var := paste(epu[i], Var, 'forecast')]
  epu.long[, Units := 'anomaly']
  
  SOE.data <- rbindlist(list(SOE.data, epu.long))
}

terns <- as.data.table(read.csv(file.path(data.dir, 'terntotalpop.csv')))
setnames(terns, c('Year', 'Unit'), c('Time', 'Units'))
terns[, Source := NULL]
setcolorder(terns, names(SOE.data))

SOE.data <- rbindlist(list(SOE.data, terns))

whales <- as.data.table(read.csv(file.path(data.dir, 'RightWhalePopNARW2016.csv')))
setnames(whales, c('Year', 'Unit'), c('Time', 'Units'))
whales[, Source := NULL]
whales <- whales[!is.na(Value),]
setcolorder(whales, names(SOE.data))

SOE.data <- rbindlist(list(SOE.data, whales))

mariculture <- as.data.table(read.csv(file.path(data.dir, 'Mariculture.csv')))
setnames(mariculture, c('Year', 'Unit'), c('Time', 'Units'))
mariculture <- mariculture[, list(Time, Value, Var, Units)]
mariculture[Units == 'Whole lbs', Var := paste(Var, 'weight')]
mariculture[Units == '$', Var := paste(Var, 'value')]
mariculture <- mariculture[!is.na(Time), ]

SOE.data <- rbindlist(list(SOE.data, mariculture))

#Fish resources
load(file.path(data.dir, 'dat_spec_rec0_epu.Rdata'))
prod <- as.data.table(dat_spec_rec0_epu)
epu <- unique(prod[, EPU])
species <- unique(prod[, SCINAME])
for(i in 1:length(epu)){
  for(j in 1:length(species)){
    species.epu.long <- prod[EPU == epu[i] & SCINAME == species[j], list(YEAR, rs_anom)]
    species.epu.long[, Var := paste(epu[i], species[j], 'RS anomoly')]
    setnames(species.epu.long, c('YEAR', 'rs_anom'), c('Time', 'Value'))
    species.epu.long <- species.epu.long[!is.na(Value), ]
    #Add units
    species.epu.long[, Units := 'anomoly'] 
    SOE.data <- rbindlist(list(SOE.data, species.epu.long))
  }
}

load(file.path(data.dir, 'dat_spec_rec0.Rdata'))
prod.2 <- as.data.table(dat_spec_rec0)
species <- unique(prod.2[, SCINAME])
for(i in 1:length(species)){
  species.prod.long <- prod.2[SCINAME == species[i], list(YEAR, rs_anom)]
  species.prod.long[, Var := paste('All', species[i], 'RS anomoly')]
  setnames(species.prod.long, c('YEAR', 'rs_anom'), c('Time', 'Value'))
  species.prod.long <- species.prod.long[!is.na(Value), ]
  #Add units
  species.prod.long[, Units := 'anomoly']
  SOE.data <- rbindlist(list(SOE.data, species.prod.long))
}

cond <- as.data.table(read.csv(file.path(data.dir, 'RelativeWeight_Geo_2016.csv')))
cond.data <- cond[, 4:ncol(cond), with = F]
cond.data <- as.data.table(t(cond.data))
cond.data[, Year := 1992:2016]
names <- paste(cond[, stock], cond[, species], cond[, sex])
setnames(cond.data, paste0('V', 1:46), names)
cond.long <- w2l(cond.data, 'Year')
cond.long[, Units := 'unitless']

SOE.data <- rbindlist(list(SOE.data, cond.long))

load(file.path(data.dir, 'Fall_aggregate_survey_biomass.RData'))
SOE.data <- rbindlist(list(SOE.data, fall.agg))

load(file.path(data.dir, 'Spring_aggregate_survey_biomass.RData'))
SOE.data <- rbindlist(list(SOE.data, spring.agg))

load(file.path(data.dir, 'Expected_number_of_species.RData'))
SOE.data <- rbindlist(list(SOE.data, ESn.epu))

#Human dimensions
load(file.path(data.dir, 'Rec_participants.Rdata'))
rec <- as.data.table(REC_PARTICIPANTS)
region <- c('NORTH ATLANTIC', 'MID-ATLANTIC')
region.name <- c('North Atlantic', 'Mid-Atlantic')
for(i in 1:2){
  region.rec <- rec[Region == region[i], list(Year, Total)]
  region.long <- w2l(region.rec, 'Year')
  region.long[, Var := paste(region.name[i], 'Rec participation')]
  region.long[, Units := 'n anglers']
  SOE.data <- rbindlist(list(SOE.data, region.long))
}

load(file.path(data.dir, 'Rec_angler_trips.Rdata'))
trips <- as.data.table(ANGLER_TRIPS)
region <- c('NORTH ATLANTIC', 'MID-ATLANTIC')
region.name <- c('North Atlantic', 'Mid-Atlantic')
for(i in 1:2){
  region.trips <- trips[Region == region[i], list(Year, Angler.Trips)]
  region.long <- w2l(region.trips, 'Year')
  region.long[, Var := paste(region.name[i], 'angler trips')]
  region.long[, Units := 'n trips']
  SOE.data <- rbindlist(list(SOE.data, region.long))
}

fleetdiv <- as.data.table(read.csv(file.path(data.dir, 'MAFMCavgfleetdiversity.csv')))
setnames(fleetdiv, c('year', 'eShannonrev'), c('Time', 'Value'))
fleetdiv[, Var := 'Mid-Atlantic average fleet diversity']
fleetdiv[, Units := 'effective Shannon index']
SOE.data <- rbindlist(list(SOE.data, fleetdiv))

fleetcount <- as.data.table(read.csv(file.path(data.dir, 'MAFMCfleetcount.csv')))
setnames(fleetcount, c('year', 'fleet_count'), c('Time', 'Value'))
fleetcount[, Var := 'Mid-Atlantic fleet count']
fleetcount[, Units := 'n fleets']
SOE.data <- rbindlist(list(SOE.data, fleetcount))

sppcount <- as.data.table(read.csv(file.path(data.dir, 'MAFMCspeciesdiversity_average.csv')))
setnames(sppcount, c('year', 'eShannonrev'), c('Time', 'Value'))
sppcount[, Var := 'Mid-Atlantic commercial species diversity']
sppcount[, Units := 'effective Shannon index']
SOE.data <- rbindlist(list(SOE.data, sppcount))

fleetdiv.NE <- as.data.table(read.csv(file.path(data.dir, 'NEFMCavgfleetdiversity.csv')))
setnames(fleetdiv.NE, c('year', 'eShannonrev'), c('Time', 'Value'))
fleetdiv.NE[, Var := 'New England average fleet diversity']
fleetdiv.NE[, Units := 'effective Shannon index']
SOE.data <- rbindlist(list(SOE.data, fleetdiv.NE))

fleetcount.NE <- as.data.table(read.csv(file.path(data.dir, 'NEFMCfleetcount.csv')))
setnames(fleetcount.NE, c('year', 'fleet_count'), c('Time', 'Value'))
fleetcount.NE[, Var := 'New England fleet count']
fleetcount.NE[, Units := 'n fleets']
SOE.data <- rbindlist(list(SOE.data, fleetcount.NE))

sppcount.NE <- as.data.table(read.csv(file.path(data.dir, 'NEFMCspeciesdiversity_average.csv')))
setnames(sppcount.NE, c('year', 'eShannonrev'), c('Time', 'Value'))
sppcount.NE[, Var := 'New England commercial species diversity']
sppcount.NE[, Units := 'effective Shannon index']
SOE.data <- rbindlist(list(SOE.data, sppcount.NE))

#Commercial landings and revenue
load(file.path(data.dir, 'comland_meatwt_deflated.RData'))
load(file.path(data.dir, 'Species_codes.RData'))

comland.spp <- merge(comland, spp[, list(NESPP3, EBFM.PDT)], by = 'NESPP3')

setkey(comland.spp, YEAR, EPU, EBFM.PDT)
landings <- comland.spp[, sum(SPPLIVMT), by = key(comland.spp)]
setnames(landings, 'V1', 'SPPLIVMT')

landings[, Var := paste(EPU, EBFM.PDT, 'Landings')]
landings[, c('EPU', 'EBFM.PDT') := NULL]
landings[, Units := 'metric tons']
setnames(landings, c('YEAR', 'SPPLIVMT'), c('Time', 'Value'))

revenue <- comland.spp[, sum(SPPVALUE), by = key(comland.spp)]
setnames(revenue, 'V1', 'SPPVALUE')

revenue[, Var := paste(EPU, EBFM.PDT, 'Revenue')]
revenue[, c('EPU', 'EBFM.PDT') := NULL]
revenue[, Units := 'US Dollars']
setnames(revenue, c('YEAR', 'SPPVALUE'), c('Time', 'Value'))

soe.com <- rbindlist(list(landings, revenue))
SOE.data <- rbindlist(list(SOE.data, soe.com))


primary <- as.data.table(read.csv(file.path(data.dir, '1998_2016-SEAWIFS_MODIS_PP-NES_ECOREGIONS.csv')))
primary[, Time := as.numeric(substr(PERIOD, 3, 6))]
#remove 2002 MODIS
primary <- primary[!(Time == 2002 & SENSOR == 'MODISA'), ]
primary[, Var := paste(SUBAREA, PROD)]
setnames(primary, 'AMEAN', 'Value')
primary.long <- primary[, list(Time, Value, Var)]
primary.long[Var %like% 'PPD', Units := 'gC m^-2 d^-1']
primary.long[Var %like% 'RATIO', Units := 'unitless']

SOE.data <- rbindlist(list(SOE.data, primary.long))


#New Kevin data
sstSD <- as.data.table(read.csv(file.path(data.dir, 'stt_sd_ts.csv')))
sstSD.long <- w2l(sstSD, by = 'Year')
sstSD.long[, Var := paste(Var, 'sst standard deviation')]
sstSD.long[, Units := 'degrees C']

SOE.data <- rbindlist(list(SOE.data, sstSD.long))

dist.fall <- as.data.table(read.csv(file.path(data.dir, 'dhdc_1_2017_fallASS_sprDATA.csv')))
sp <- unique(dist.fall[, SP])
load(file.path(data.dir, 'Species_codes.RData'))
species <- spp[SVSPP %in% sp, list(SVSPP, COMNAME)]

for(i in 1:length(sp)){
  sp.dist.fall <- dist.fall[SP == sp[i], ]
  sp.long <- w2l(sp.dist.fall, by = 'YR')
  sp.long <- sp.long[!Var %in% c('X', 'SP'), ]
  sp.long[, Var := paste(species[SVSPP == sp[i], COMNAME], Var)]
  sp.long[Var %like% 'LAT',    Units := 'degrees N']
  sp.long[Var %like% 'LON',    Units := 'degrees W']
  sp.long[Var %like% 'DEPTH',  Units := 'meters']
  sp.long[Var %like% 'ASDIST', Units := 'km']
  sp.long[Var %like% 'DTEOC',  Units := 'm']
  
  SOE.data <- rbindlist(list(SOE.data, sp.long))
}

sea.level <- as.data.table(read.csv(file.path(data.dir, 'slh_2017.csv')))
sea.level.long <- w2l(sea.level, by = 'Year')
sea.level.long <- sea.level.long[!is.na(Value), ]
sea.level.long[Var == 'North', States := 'ME MA']
sea.level.long[Var == 'Middle', States := 'RI CT NY NJ PA']
sea.level.long[Var == 'South', States := 'DE MD DC VA']
sea.level.long[, Var := paste('Relative sea level change', States)]
sea.level.long[, States := NULL]
sea.level.long[, Units := 'm']

SOE.data <- rbindlist(list(SOE.data, sea.level.long))

sea.level <- as.data.table(read.csv(file.path(data.dir, 'slh_2017_all states.csv')))
sea.level.long <- w2l(sea.level, by = 'Year')
sea.level.long[, Var := 'Relative sea level change all states']
sea.level.long[, Units := 'm']

SOE.data <- rbindlist(list(SOE.data, sea.level.long))

fall.surveyBT <- as.data.table(read.csv(file.path(data.dir, 'surveyBT oct.csv')))
fall.temp.long <- w2l(fall.surveyBT, by = 'Year')
fall.temp.long[, Var := paste(Var, 'fall bottom temperature')]
fall.temp.long[, Units := 'degrees C']

SOE.data <- rbindlist(list(SOE.data, fall.temp.long))

spring.surveyBT <- as.data.table(read.csv(file.path(data.dir, 'surveyBT apr.csv')))
spring.temp.long <- w2l(spring.surveyBT, by = 'Year')
spring.temp.long[, Var := paste(Var, 'spring bottom temperature')]
spring.temp.long[, Units := 'degrees C']

SOE.data <- rbindlist(list(SOE.data, spring.temp.long))

# Sarah added more Kevin data from word files

zoopCtyp <- as.data.table(read.csv(file.path(data.dir, 'MABzoopCtypicus.csv')))
SOE.data <- rbindlist(list(SOE.data, zoopCtyp))

save(SOE.data, file = file.path(data.dir, 'SOE_data.RData'))
#Run Assign_EPU.R to add EPU designations
