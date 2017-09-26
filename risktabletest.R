# testing conditional formatting for risk assessment table
# Sept 2017
# from https://www.r-bloggers.com/conditional-formatting-of-a-table-in-r/

library(condformat)

#my.data <- matrix(rexp(200, rate=.1), ncol=20)
#summary(my.data)

#my.data <- as.data.frame(my.data)

#color.picker <- function(z){
#  if(is.na(z)){return(0)}
#  else if(z <= 20){return(1)}
#  else if( z > 20 & z <= 80){return(2)}
#  else {return(3)}
#}

#condformat(my.data) +
#  rule_fill_gradient(CPOE, low = rgb(1,1,1), high = rgb(0,1,0)) +
#  rule_fill_gradient(Verbal, low = rgb(1,1,1), high = rgb(1,0,0)) +
#  rule_fill_gradient(Written, low = rgb(1,1,1), high = rgb(1,0,0)) +
#  rule_fill_discrete(Cosigned, expression = sapply(Cosigned,
#                    color.picker),colours=c("0" = "white", "1" = "red", 
#                    "2" =  "yellow", "3" = "lightgreen"))

color.picker <- function(z){
  if(is.na(z)){return(0)}
  else if( z == "l"){return(1)}
  else if( z == "lm"){return(2)}
  else if( z == "mh"){return(3)}
  else {return(4)}
}

color.picker2 <- function(z){
  if( z == "na"){return("white")}
  else if( z == "l"){return("green")}
  else if( z == "lm"){return("yellow")}
  else if( z == "mh"){return("orange")}
  else {return("red")}
}

# spplist     oc,  sc,  flk, scp, bsb, mack, but, lsq, ssq, gtile,  btile,  blu, dog, monk

risk.species<-data.frame(
  Species = c("Ocean Quahog", "Surfclam", "Summer flounder", "Scup", "Black sea bass", "Atl. mackerel", "Butterfish", "Longfin squid", "Shortfin squid", "Golden tilefish", "Blueline tilefish", "Bluefish", "Spiny dogfish", "Monkfish"),
  Fstatus = c("l", "l", "h", "l", "l", "mh", "l", "lm", "lm", "l", "h", "l", "l", "lm"),
  Bstatus = c("l", "l", "lm", "l", "l", "mh", "l", "lm", "lm", "lm", "mh", "lm", "lm", "lm"),
  Assess  = c("l", "l", "l",  "l", "l", "h",  "l", "lm", "lm", "l", "h", "l", "lm", "h"),
  FoodWeb1 = c("l", "l", "lm", "l", "l", "mh", "lm", "mh", "mh", "l", "l", "l", "l", "l"),
  FoodWeb2 = c("l", "l", "na", "na", "na", "na", "na", "na", "na", "na", "na", "na", "na", "na"),
  EcoProd = c("lm","lm","lm","lm","lm","lm","lm","lm","lm","lm","lm","lm","lm","lm"),
  PopDiv = c("na", "na", "na", "na", "na", "na", "na", "na", "na", "na", "na", "na", "na", "na"),
  EcoDiv = c("lm","lm","lm","lm","lm","lm","lm","lm","lm","lm","lm","lm","lm","lm"),
  Climate = c("h", "mh", "lm", "lm", "mh", "lm", "l", "l", "l", "mh", "mh","l", "l", "l"),
  DistShift = c("mh", "mh", "mh", "mh", "mh", "mh", "h", "mh", "h", "l", "l", "mh", "h", "mh"),
  EstHabitat = c(),
  OffHabitat = c(),
  CommProf = c(),
  RecVal = c(),
  FishRes1 = c(),
  FishRes2 = c(),
  FishRes3 = c(),
  FishRes4 = c(),
  FishRes5 = c(),
  CommJobs = c(),
  RecJobs = c(),
  Social = c(),
  ComFood = c(),
  RecFood = c(),
  FoodSafe = c(),
  MgtControl = c(),
  TecInteract = c(),
  OceanUse = c(),
  RegComplex = c(),
  Discards = c(),
  Allocation = c() 
)

condformat(risk.species) +
  rule_fill_discrete(Fstatus, expression = sapply(Fstatus,color.picker2),colours=identity) +
  rule_fill_discrete(Bstatus, expression = sapply(Bstatus,color.picker2),colours=identity) +
  rule_fill_discrete(Assess, expression = sapply(Assess,color.picker2),colours=identity) +
  rule_fill_discrete(FoodWeb1, expression = sapply(FoodWeb1,color.picker2),colours=identity) +
  rule_fill_discrete(FoodWeb2, expression = sapply(FoodWeb2,color.picker2),colours=identity) +
  rule_fill_discrete(EcoProd, expression = sapply(EcoProd,color.picker2),colours=identity) +
  rule_fill_discrete(PopDiv, expression = sapply(PopDiv,color.picker2),colours=identity) +
  rule_fill_discrete(EcoDiv, expression = sapply(EcoDiv,color.picker2),colours=identity) +
  rule_fill_discrete(Climate, expression = sapply(Climate,color.picker2),colours=identity) +
  rule_fill_discrete(DistShift, expression = sapply(DistShift,color.picker2),colours=identity) +
  rule_fill_discrete(EstHabitat, expression = sapply(EstHabitat,color.picker2),colours=identity) +
  rule_fill_discrete(OffHabitat, expression = sapply(OffHabitat,color.picker2),colours=identity) +
  rule_fill_discrete(FoodSafe, expression = sapply(FoodSafe,color.picker2),colours=identity) +
  rule_fill_discrete(MgtControl, expression = sapply(MgtControl,color.picker2),colours=identity) +
  rule_fill_discrete(TecInteract, expression = sapply(TecInteract,color.picker2),colours=identity) +
  rule_fill_discrete(OceanUse, expression = sapply(OceanUse,color.picker2),colours=identity) +
  rule_fill_discrete(RegComplex, expression = sapply(RegComplex,color.picker2),colours=identity) +
  rule_fill_discrete(Discards, expression = sapply(Discards,color.picker2),colours=identity) +
  rule_fill_discrete(Allocation, expression = sapply(Allocation,color.picker2),colours=identity) 
  


condformat(risk.species) +
  rule_fill_discrete(Fstatus, expression = sapply(Fstatus,
                                                  color.picker),colours=c("0" = "white", "1" = "green", 
                                                                          "2" =  "yellow", "3" = "orange", "4" = "red")) +
  rule_fill_discrete(Bstatus, expression = sapply(Bstatus,
                                                  color.picker),colours=c("0" = "white", "1" = "green", 
                                                                          "2" =  "yellow", "3" = "orange", "4" = "red"))


#color_pick <- function(column) {
#  sapply(column,
#         FUN = function(z) {
#           if(is.na(z)){return("white")}
#           else if( z == "l"){return("green")}
#           else if( z == "lm"){return("yellow")}
#           else if( z == "mh"){return("orange")}
#           else {return("red")}
#           if (value < 4.7) {
#             return("red")
#           } else if (value < 5.0) {
#             return("yellow")
#           } else {
#             return("green")
#           }
#         })
#}

#condformat(my.data) +  
#  rule_fill_discrete_(my.data[,2], ~ color_pick(my.data[,2]), colours = identity)+
#  rule_fill_discrete_(my.data[,3], ~ color_pick(my.data[,3]), colours = identity)
  