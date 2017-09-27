#Add EPUs to data set
if(Sys.info()['sysname'] == "Windows") prefix <- 'Z:'
if(Sys.info()['sysname'] == "Linux")   prefix <- '/home/slucey/EcoAP'
if(Sys.info()['sysname'] == "Darwin")  prefix <- '/Users/sgaichas/Documents/0_Data/ESR'

data.dir <- file.path(prefix, 'SOE2017', 'data')

library(data.table)

load(file.path(data.dir, 'SOE_data.RData'))

SOE.data[, EPU := factor(NA, levels = c('SS', 'GOM', 'GB', 'MAB', 'ALL'))]

SOE.data[Var %like% 'MAB' | Var %like% 'Mid-Atl', EPU := 'MAB']
SOE.data[Var %like% 'GB'  | Var %like% 'George',  EPU := 'GB']
SOE.data[Var %like% 'RI ' & !Var %like% 'CHESTERI', EPU := 'GB']
SOE.data[Var %like% 'GOM', EPU := 'GOM']
SOE.data[Var %like% 'ME ' & !Var %like% 'BROSME', EPU := 'GOM']
SOE.data[Var %like% 'SS ' & !Var %like% 'CHUSS', EPU := 'SS']
SOE.data[Var %like% 'SCS', EPU := 'SS']
SOE.data[Var %in% c('Surface temp SS', 'Bottom temp SS', 'Surface salinity SS', 
                    'Bottom salinity SS'), EPU := 'SS']
SOE.data[Var %like% 'VA ', EPU := 'MAB']

SOE.data[is.na(EPU), EPU := 'ALL']
#Remove "Other EPU" category from all
SOE.data[Var %like% 'OTHER' & Var %like% 'Landings', EPU := 'OTHER']
SOE.data[Var %like% 'OTHER' & Var %like% 'Revenue', EPU := 'OTHER']

save(SOE.data, file = file.path(data.dir, 'SOE_data.RData'))
