library(tidyverse)

library(dplyr)

MechaCar <- read.csv("MechaCar_mpg.csv",stringsAsFactors = F,check.names = F)
MechaCarlm <- lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD,data=MechaCar)

summary(MechaCarlm)

SuspensionCoil <- read.csv("Suspension_Coil.csv",stringsAsFactors = F, check.names = F)

total_summary <- SuspensionCoil %>%
  summarize(Mean=mean(PSI),Median=median(PSI),Variance=var(PSI),SD=sd(PSI))

lot_summary <- SuspensionCoil %>% group_by(Manufacturing_Lot) %>%
  summarize(Mean=mean(PSI),Median=median(PSI),Variance=var(PSI),SD=sd(PSI), .groups='keep')

t.test(SuspensionCoil$PSI,mu = 1500)

t.test(subset(SuspensionCoil,Manufacturing_Lot=="Lot1")$PSI,mu = 1500)

t.test(subset(SuspensionCoil,Manufacturing_Lot=="Lot2")$PSI,mu = 1500)

t.test(subset(SuspensionCoil,Manufacturing_Lot=="Lot3")$PSI,mu = 1500)






