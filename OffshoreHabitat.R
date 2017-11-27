# script to manage Kevin's habitat indices

library(tidyverse)

data.dir  <- '/users/sgaichas/Documents/0_Data/ESR/SOE2017/data'

load(file.path(data.dir, 'Species_codes.RData'))

fall_habdat <- read.csv(file.path(data.dir,"hab_areas_fall.csv"))
                                   
spring_habdat <- read.csv(file.path(data.dir,"hab_areas_spring.csv"))

# November 21, 2017
# Kevin's method is to use the habitat interval that is largest for the species
# logic is that this is the best habitat, seems reasonable
# from Kevin's plot hab and cpue.R

#bar_dat = aggregate(tdat$hab_area_int_km2_s, list(tdat$hab_int_num), mean, na.rm = T)

#barplot(bar_dat[,2],names.arg = bar_dat[,1])

#bar_dat <- bar_dat[order(bar_dat$x, decreasing = T),] 

#print(c(sps$SVSPP[i],bar_dat$Group.1[1]))

#level_1 = bar_dat[1,1]
#level_2 = bar_dat[2,1]
#level_3 = bar_dat[3,1]

#hx1 = year[hab_int_num==level_1]

#hy1 = hab_area_cum_low_km2_s[hab_int_num==level_1]/1000
#hy1mean = mean(hy1,na.rm=T)
#hy1sd = sd(hy1,na.rm=T)

# similarly, using tidyverse code find the interval with highest mean habitat
fall_habmax <- fall_habdat %>%
  group_by(svspp, hab_int_num) %>%
  summarise(meanhab = mean(hab_area_int_km2_s, na.rm = T)) %>%
  arrange(desc(meanhab)) %>%
  slice(1L) #takes the first row which is highest mean

spring_habmax <- spring_habdat %>%
  group_by(svspp, hab_int_num) %>%
  summarise(meanhab = mean(hab_area_int_km2_s, na.rm = T)) %>%
  arrange(desc(meanhab)) %>%
  slice(1L) #takes the first row which is highest mean

#now keep only the habmax interval number for plotting
fall_habmaxall <- left_join(fall_habmax, fall_habdat)

spring_habmaxall <- left_join(spring_habmax, spring_habdat)

# Kevin used hab_area_cum_low_km2_s/1000 as habitat

# MAFMC species svspp codes
# surfclam, ocean quahog, summerfl, scup, bsb, atlmack, butter, longsq, shrtsq, gtile, bltile, bluef, spdog, monk
MAFMC<-data.frame(svspp = c( 403, 409, 103, 143, 141, 121, 131, 503, 502, 151, 621, 135, 15, 197))
MAFMCspp <- merge(spp, MAFMC, by.x="SVSPP", by.y="svspp")

fall_habmax_mafmc <- merge(fall_habmaxall, MAFMCspp, by.x="svspp", by.y="SVSPP")
fall_habmax_mafmc <- fall_habmax_mafmc %>%
  select(svspp, year, hab_int_num, hab_inter_low, hab_area_cum_low_km2_s, COMNAME)

spring_habmax_mafmc <- merge(spring_habmaxall, MAFMCspp, by.x="svspp", by.y="SVSPP")
spring_habmax_mafmc <- spring_habmax_mafmc %>%
  select(svspp, year, hab_int_num, hab_inter_low, hab_area_cum_low_km2_s, COMNAME)

fall_habmax_mafmc_soe <- fall_habmax_mafmc %>%
  select(year, hab_area_cum_low_km2_s, COMNAME) %>%
  rename(Time = year, Value = hab_area_cum_low_km2_s, Var = COMNAME)

spring_habmax_mafmc_soe <- spring_habmax_mafmc %>%
  select(year, hab_area_cum_low_km2_s, COMNAME) %>%
  rename(Time = year, Value = hab_area_cum_low_km2_s, Var = COMNAME)

save(fall_habmax_mafmc_soe, file=file.path(data.dir,"fall_habmax_mafmc_soe.RData"))
save(spring_habmax_mafmc_soe, file=file.path(data.dir, "spring_habmax_mafmc_soe.Rdata"))

# the below was done in Sept 2017 prior to discussion with Kevin and was not used
                          
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