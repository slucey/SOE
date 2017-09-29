# script to manage Kevin's habitat indices

library(tidyverse)

data.dir  <- '/users/sgaichas/Documents/0_Data/ESR/SOE2017/data'

load(file.path(data.dir, 'Species_codes.RData'))

fall_habdat <- read.csv(file.path(data.dir,"hab_areas_fall.csv"))
                                   
spring_habdat <- read.csv(file.path(data.dir,"hab_areas_spring.csv"))
                          
#decision, use habitat interval bound closest to 0.4, about the mean for all species in both datasets
#dont know what this means but should compare apples to apples across spp?
#nope, that drops black sea bass
#how about use inteval 6 for all of them

fall_habdat <- fall_habdat[,c("svspp", "year", "hab_int_num", "hab_inter_high", "hab_area_cum_high_km2")]
spring_habdat <- spring_habdat[,c("svspp", "year", "hab_int_num", "hab_inter_high", "hab_area_cum_high_km2")]

#fall_hab4 <- fall_habdat %>%
#  group_by(svspp, year) %>%
#  filter(hab_inter_high >= 0.4) %>% #keep intervals at or above 0.4
#  slice(1L) #takes the first row of each which is closest to 0.4
#
#spring_hab4 <- spring_habdat %>%
#  group_by(svspp, year) %>%
#  filter(hab_inter_high >= 0.4) %>% #keep intervals at or above 0.4
#  slice(1L) #takes the first row of each which is closest to 0.4

fall_hab4 <- fall_habdat %>%
  group_by(svspp, year) %>%
  filter(hab_int_num == 6) %>% #keep intervals at or above 0.4
  slice(1L) #takes the first row of each which is closest to 0.4

spring_hab4 <- spring_habdat %>%
  group_by(svspp, year) %>%
  filter(hab_int_num == 6) %>% #keep intervals at or above 0.4
  slice(1L) #takes the first row of each which is closest to 0.4


# MAFMC species svspp codes
# surfclam, ocean quahog, summerfl, scup, bsb, atlmack, butter, longsq, shrtsq, gtile, bltile, bluef, spdog, monk
MAFMC<-data.frame(svspp = c( 403, 409, 103, 143, 141, 121, 131, 503, 502, 151, 621, 135, 15, 197))
MAFMCspp <- merge(spp, MAFMC, by.x="SVSPP", by.y="svspp")

fall_hab4_mafmc <- merge(fall_hab4, MAFMCspp, by.x="svspp", by.y="SVSPP")
fall_hab4_mafmc <- fall_hab4_mafmc %>%
  select(svspp, year, hab_int_num, hab_inter_high, hab_area_cum_high_km2, COMNAME)

fall_sp <- ggplot(fall_hab4_mafmc, aes(year, hab_area_cum_high_km2)) + geom_point()
fall_sp + facet_wrap(~COMNAME)

spring_hab4_mafmc <- merge(spring_hab4, MAFMCspp, by.x="svspp", by.y="SVSPP")
spring_hab4_mafmc <- spring_hab4_mafmc %>%
  select(svspp, year, hab_int_num, hab_inter_high, hab_area_cum_high_km2, COMNAME)

spring_sp <- ggplot(spring_hab4_mafmc, aes(year, hab_area_cum_high_km2)) + geom_point()
spring_sp + facet_wrap(~COMNAME)

fall_hab4_mafmc_soe <- fall_hab4_mafmc %>%
  select(year, hab_area_cum_high_km2, COMNAME) %>%
  rename(Time = year, Value = hab_area_cum_high_km2, Var = COMNAME)

spring_hab4_mafmc_soe <- spring_hab4_mafmc %>%
  select(year, hab_area_cum_high_km2, COMNAME) %>%
  rename(Time = year, Value = hab_area_cum_high_km2, Var = COMNAME)

save(fall_hab4_mafmc_soe, file=file.path(data.dir,"fall_hab4_mafmc_soe.RData"))
save(spring_hab4_mafmc_soe, file=file.path(data.dir, "spring_hab4_mafmc_soe.Rdata"))