Proficiency tests are performed according to the ISO 13528:
R codes have been written for:

- Z’_score
- Z _score
- En_number

##########  Z'_score   #############################################################################################

setwd("D:/Input_data/Rel_Categories/Input_data/Rel_Categories")

SCEs <- list.files(path ="D:/Input_data/Rel_Categories/Input_data/Rel_Categories",
                   pattern = "SCEs") #this includes only files with "rel"


for (i in c(1:length(SCEs))) {
  assign(gsub("[.]csv$","",SCEs[i]), read.csv(SCEs[i], header=TRUE, 
                                              as.is = 1))
}

##########################################################################
############################# BioB Z'_Score ##############################

########### read the SCE values calculated from the dispersion model #####

SCEs_SYNT <- read.csv("SCEs_SYNT.csv", header=TRUE ,",", as.is = 1)
BioB_SYNT <- SCEs_SYNT[1,3]
U_BioB_SYNT <- SCEs_SYNT[1,4]

########## z'_score calculation for Source Contribution Estimation

BioB_SCEs$Z_prime <- BioB_SCEs[,2]
BioB_SCEs[,3] <- (BioB_SCEs[,2] - BioB_SYNT)/sqrt((BioB_SYNT*0.5)^2 + (U_BioB_SYNT)^2)
str(BioB_SCEs)

######## SAVE FILES BEFORE RUNNING SCRIPT WITH OTHER FACTOR CLASS #####

names <- BioB_SCEs[1]
Z_prime <- BioB_SCEs[3]
BioB_Z_prime <- cbind(names,Z_prime)
write.csv(BioB_Z_prime, file = "BioB_Z_prime.csv", na = "") 

########## Box-Plot for Z'-Score ####################################

jpeg('D:/Input_data/Rel_Categories/Input_data/Rel_Categories/Z_prime_graphs/BioB_Z_prime.jpg',
     quality = 100, bg = "white", res = 200, width = 7, height = 7, units = "in")

names <- (BioB_SCEs[,1])
names <-as.vector(names)

par(mar=c(12, 4, 2, 2) + 0.3)
oldpar <- par(las=2)
barplot(BioB_SCEs$Z_prime, width = 0.9, space = 0.35, 
        names.arg = names, border = 1,
        xlab= "", ylab = "Z'_score",
        xlim = c(1, (nrow(BioB_Z_prime)+5)), ylim = c(-4, 4),
        cex.lab=1, cex.axis=0.8, cex.names = 0.8, col.axis="black")
title(main ="Biomass Burning Z'_score", font.main= 1.5)
abline(h=0,col=1, lty=1, lwd=1)
abline(h=1,col=4, lty=1, lwd=2)
abline(h=2,col=1, lty=1, lwd=2)
abline(h=3,col=3, lty=1, lwd=2)
abline(h=-1,col=4, lty=1, lwd=2)
abline(h=-2,col=1, lty=1, lwd=2)
abline(h=-3,col=3, lty=1, lwd=2)
par(oldpar)

dev.off()

##########################################################################
############################# SO4 Z'_Score ##############################

########### read the SCE values calculated from the dispersion model #####

SCEs_SYNT <- read.csv("SCEs_SYNT.csv", header=TRUE ,",", as.is = 1)
SO4_SYNT <- SCEs_SYNT[2,3]
U_SO4_SYNT <- SCEs_SYNT[2,4]

########## z'_score calculation for Source Contribution Estimation

SO4_SCEs$Z_prime <- SO4_SCEs[,2]
SO4_SCEs[,3] <- (SO4_SCEs[,2] - SO4_SYNT)/sqrt((SO4_SYNT*0.5)^2 + (U_SO4_SYNT)^2)
str(SO4_SCEs)

######## SAVE FILES BEFORE RUNNING SCRIPT WITH OTHER FACTOR CLASS #####

names <- SO4_SCEs[1]
Z_prime <- SO4_SCEs[3]
SO4_Z_prime <- cbind(names,Z_prime)
write.csv(SO4_Z_prime, file = "SO4_Z_prime.csv", na = "") 

########## Box-Plot for Z'-Score ####################################

jpeg('D:/Input_data/Rel_Categories/Input_data/Rel_Categories/Z_prime_graphs/SO4_Z_prime.jpg',
     quality = 100, bg = "white", res = 200, width = 7, height = 7, units = "in")

names <- (SO4_SCEs[,1])
names <-as.vector(names)

par(mar=c(12, 4, 2, 2) + 0.3)
oldpar <- par(las=2)
barplot(SO4_SCEs$Z_prime, width = 0.9, space = 0.35, 
        names.arg = names, border = 1,
        xlab= "", ylab = "Z'_score",
        xlim = c(1, (nrow(SO4_Z_prime)+4)), ylim = c(-4, 4),
        cex.lab=1, cex.axis=0.8, cex.names = 0.8, col.axis="black")
title(main ="SO4 Z'_score", font.main= 1.5)
abline(h=0,col=1, lty=1, lwd=1)
abline(h=1,col=4, lty=1, lwd=2)
abline(h=2,col=1, lty=1, lwd=2)
abline(h=3,col=3, lty=1, lwd=2)
abline(h=-1,col=4, lty=1, lwd=2)
abline(h=-2,col=1, lty=1, lwd=2)
abline(h=-3,col=3, lty=1, lwd=2)
par(oldpar)

dev.off()

##########################################################################
############################# NO3 Z'_Score ##############################

########### read the SCE values calculated from the dispersion model #####

SCEs_SYNT <- read.csv("SCEs_SYNT.csv", header=TRUE ,",", as.is = 1)
NO3_SYNT <- SCEs_SYNT[3,3]
U_NO3_SYNT <- SCEs_SYNT[3,4]

########## z'_score calculation for Source Contribution Estimation

NO3_SCEs$Z_prime <- NO3_SCEs[,2]
NO3_SCEs[,3] <- (NO3_SCEs[,2] - NO3_SYNT)/sqrt((NO3_SYNT*0.5)^2 + (U_NO3_SYNT)^2)
str(NO3_SCEs)

######## SAVE FILES BEFORE RUNNING SCRIPT WITH OTHER FACTOR CLASS #####

names <- NO3_SCEs[1]
Z_prime <- NO3_SCEs[3]
NO3_Z_prime <- cbind(names,Z_prime)
write.csv(NO3_Z_prime, file = "NO3_Z_prime.csv", na = "") 

########## Box-Plot for Z'-Score ####################################

jpeg('D:/Input_data/Rel_Categories/Input_data/Rel_Categories/Z_prime_graphs/NO3_Z_prime.jpg',
     quality = 100, bg = "white", res = 200, width = 7, height = 7, units = "in")

names <- (NO3_SCEs[,1])
names <-as.vector(names)

par(mar=c(12, 4, 2, 2) + 0.3)
oldpar <- par(las=2)
barplot(NO3_SCEs$Z_prime, width = 0.9, space = 0.35, 
        names.arg = names, border = 1,
        xlab= "", ylab = "Z'_score",
        xlim = c(0.5, (nrow(NO3_Z_prime)+4)), ylim = c(-4, 4),
        cex.lab=1, cex.axis=0.8, cex.names = 0.8, col.axis="black")
title(main ="NO3 Z'_score", font.main= 1.5)
abline(h=0,col=1, lty=1, lwd=1)
abline(h=1,col=4, lty=1, lwd=2)
abline(h=2,col=1, lty=1, lwd=2)
abline(h=3,col=3, lty=1, lwd=2)
abline(h=-1,col=4, lty=1, lwd=2)
abline(h=-2,col=1, lty=1, lwd=2)
abline(h=-3,col=3, lty=1, lwd=2)
par(oldpar)

dev.off()

##########################################################################
############################# DUST Z'_Score ##############################

########### read the SCE values calculated from the dispersion model #####

SCEs_SYNT <- read.csv("SCEs_SYNT.csv", header=TRUE ,",", as.is = 1)
DUST_SYNT <- SCEs_SYNT[4,3]
U_DUST_SYNT <- SCEs_SYNT[4,4]

########## z'_score calculation for Source Contribution Estimation

DUST_SCEs$Z_prime <- DUST_SCEs[,2]
DUST_SCEs[,3] <- (DUST_SCEs[,2] - DUST_SYNT)/sqrt((DUST_SYNT*0.5)^2 + (U_DUST_SYNT)^2)
str(DUST_SCEs)

######## SAVE FILES BEFORE RUNNING SCRIPT WITH OTHER FACTOR CLASS #####

names <- DUST_SCEs[1]
Z_prime <- DUST_SCEs[3]
DUST_Z_prime <- cbind(names,Z_prime)
write.csv(DUST_Z_prime, file = "DUST_Z_prime.csv", na = "") 

########## Box-Plot for Z'-Score ####################################

jpeg('D:/Input_data/Rel_Categories/Input_data/Rel_Categories/Z_prime_graphs/DUST_Z_prime.jpg',
     quality = 100, bg = "white", res = 200, width = 7, height = 7, units = "in")

names <- (DUST_SCEs[,1])
names <-as.vector(names)

par(mar=c(12, 4, 2, 2) + 0.3)
oldpar <- par(las=2)
barplot(DUST_SCEs$Z_prime, width = 0.9, space = 0.35, 
        names.arg = names, border = 1,
        xlab= "", ylab = "Z'_score",
        xlim = c(0.5, (nrow(DUST_Z_prime)+4)), ylim = c(-4, 4),
        cex.lab=1, cex.axis=0.8, cex.names = 0.8, col.axis="black")
title(main ="Dust Z'_score", font.main= 1.5)
abline(h=0,col=1, lty=1, lwd=1)
abline(h=1,col=4, lty=1, lwd=2)
abline(h=2,col=1, lty=1, lwd=2)
abline(h=3,col=3, lty=1, lwd=2)
abline(h=-1,col=4, lty=1, lwd=2)
abline(h=-2,col=1, lty=1, lwd=2)
abline(h=-3,col=3, lty=1, lwd=2)
par(oldpar)

dev.off()


##########################################################################
############################# ROAD Z'_Score ##############################

########### read the SCE values calculated from the dispersion model #####

SCEs_SYNT <- read.csv("SCEs_SYNT.csv", header=TRUE ,",", as.is = 1)
ROAD_SYNT <- SCEs_SYNT[5,3]
U_ROAD_SYNT <- SCEs_SYNT[5,4]

########## z'_score calculation for Source Contribution Estimation

ROAD_SCEs$Z_prime <- ROAD_SCEs[,2]
ROAD_SCEs[,3] <- (ROAD_SCEs[,2] - ROAD_SYNT)/sqrt((ROAD_SYNT*0.5)^2 + (U_ROAD_SYNT)^2)
str(ROAD_SCEs)

######## SAVE FILES BEFORE RUNNING SCRIPT WITH OTHER FACTOR CLASS #####

names <- ROAD_SCEs[1]
Z_prime <- ROAD_SCEs[3]
ROAD_Z_prime <- cbind(names,Z_prime)
write.csv(ROAD_Z_prime, file = "ROAD_Z_prime.csv", na = "") 

########## Box-Plot for Z'-Score ####################################

jpeg('D:/Input_data/Rel_Categories/Input_data/Rel_Categories/Z_prime_graphs/ROAD_Z_prime.jpg',
     quality = 100, bg = "white", res = 200, width = 7, height = 7, units = "in")

names <- (ROAD_SCEs[,1])
names <-as.vector(names)

par(mar=c(12, 4, 2, 2) + 0.3)
oldpar <- par(las=2)
barplot(ROAD_SCEs$Z_prime, width = 0.9, space = 0.35, 
        names.arg = names, border = 1,
        xlab= "", ylab = "Z'_score",
        xlim = c(0.5, (nrow(ROAD_Z_prime)+3)), ylim = c(-4, 4),
        cex.lab=1, cex.axis=0.8, cex.names = 0.8, col.axis="black")
title(main ="Road Dust Z'_score", font.main= 1.5)
abline(h=0,col=1, lty=1, lwd=1)
abline(h=1,col=4, lty=1, lwd=2)
abline(h=2,col=1, lty=1, lwd=2)
abline(h=3,col=3, lty=1, lwd=2)
abline(h=-1,col=4, lty=1, lwd=2)
abline(h=-2,col=1, lty=1, lwd=2)
abline(h=-3,col=3, lty=1, lwd=2)
par(oldpar)

dev.off()

##########################################################################
############################# SALT Z'_Score ##############################

########### read the SCE values calculated from the dispersion model #####

SCEs_SYNT <- read.csv("SCEs_SYNT.csv", header=TRUE ,",", as.is = 1)
SALT_SYNT <- SCEs_SYNT[6,3]
U_SALT_SYNT <- SCEs_SYNT[6,4]

########## z'_score calculation for Source Contribution Estimation

SALT_SCEs$Z_prime <- SALT_SCEs[,2]
SALT_SCEs[,3] <- (SALT_SCEs[,2] - SALT_SYNT)/sqrt((SALT_SYNT*0.5)^2 + (U_SALT_SYNT)^2)
str(SALT_SCEs)

######## SAVE FILES BEFORE RUNNING SCRIPT WITH OTHER FACTOR CLASS #####

names <- SALT_SCEs[1]
Z_prime <- SALT_SCEs[3]
SALT_Z_prime <- cbind(names,Z_prime)
write.csv(SALT_Z_prime, file = "SALT_Z_prime.csv", na = "") 

########## Box-Plot for Z'-Score ####################################

jpeg('D:/Input_data/Rel_Categories/Input_data/Rel_Categories/Z_prime_graphs/SALT_Z_prime.jpg',
     quality = 100, bg = "white", res = 200, width = 7, height = 7, units = "in")

names <- (SALT_SCEs[,1])
names <-as.vector(names)

par(mar=c(12, 4, 2, 2) + 0.3)
oldpar <- par(las=2)
barplot(SALT_SCEs$Z_prime, width = 0.9, space = 0.35, 
        names.arg = names, border = 1,
        xlab= "", ylab = "Z'_score",
        xlim = c(1, (nrow(SALT_Z_prime)+4)), ylim = c(-4, 6),
        cex.lab=1, cex.axis=0.8, cex.names = 0.8, col.axis="black")
title(main ="Salt Z'_score", font.main= 1.5)
abline(h=0,col=1, lty=1, lwd=1)
abline(h=1,col=4, lty=1, lwd=2)
abline(h=2,col=1, lty=1, lwd=2)
abline(h=3,col=3, lty=1, lwd=2)
abline(h=-1,col=4, lty=1, lwd=2)
abline(h=-2,col=1, lty=1, lwd=2)
abline(h=-3,col=3, lty=1, lwd=2)
par(oldpar)

dev.off()

##########################################################################
############################# TRA Z'_Score ##############################

########### read the SCE values calculated from the dispersion model #####

SCEs_SYNT <- read.csv("SCEs_SYNT.csv", header=TRUE ,",", as.is = 1)
TRA_SYNT <- SCEs_SYNT[7,3]
U_TRA_SYNT <- SCEs_SYNT[7,4]

########## z'_score calculation for Source Contribution Estimation

TRA_SCEs$Z_prime <- TRA_SCEs[,2]
TRA_SCEs[,3] <- (TRA_SCEs[,2] - TRA_SYNT)/sqrt((TRA_SYNT*0.5)^2 + (U_TRA_SYNT)^2)
str(TRA_SCEs)

######## SAVE FILES BEFORE RUNNING SCRIPT WITH OTHER FACTOR CLASS #####

names <- TRA_SCEs[1]
Z_prime <- TRA_SCEs[3]
TRA_Z_prime <- cbind(names,Z_prime)
write.csv(TRA_Z_prime, file = "TRA_Z_prime.csv", na = "") 

########## Box-Plot for Z'-Score ####################################

jpeg('D:/Input_data/Rel_Categories/Input_data/Rel_Categories/Z_prime_graphs/TRA_Z_prime.jpg',
     quality = 100, bg = "white", res = 200, width = 7, height = 7, units = "in")

names <- (TRA_SCEs[,1])
names <-as.vector(names)

par(mar=c(12, 4, 2, 2) + 0.3)
oldpar <- par(las=2)
barplot(TRA_SCEs$Z_prime, width = 0.9, space = 0.35, 
        names.arg = names, border = 1,
        xlab= "", ylab = "Z'_score",
        xlim = c(1, (nrow(TRA_Z_prime)+5)), ylim = c(-4, 4),
        cex.lab=1, cex.axis=0.8, cex.names = 0.8, col.axis="black")
title(main ="Trraffic Z'_score", font.main= 1.5)
abline(h=0,col=1, lty=1, lwd=1)
abline(h=1,col=4, lty=1, lwd=2)
abline(h=2,col=1, lty=1, lwd=2)
abline(h=3,col=3, lty=1, lwd=2)
abline(h=-1,col=4, lty=1, lwd=2)
abline(h=-2,col=1, lty=1, lwd=2)
abline(h=-3,col=3, lty=1, lwd=2)
par(oldpar)

dev.off()

##########################################################################
############################# INDU Z'_Score ##############################

########### read the SCE values calculated from the dispersion model #####

SCEs_SYNT <- read.csv("SCEs_SYNT.csv", header=TRUE ,",", as.is = 1)
INDU_SYNT <- SCEs_SYNT[8,3]
U_INDU_SYNT <- SCEs_SYNT[8,4]

########## z'_score calculation for Source Contribution Estimation

INDU_SCEs$Z_prime <- INDU_SCEs[,2]
INDU_SCEs[,3] <- (INDU_SCEs[,2] - INDU_SYNT)/sqrt((INDU_SYNT*0.5)^2 + (U_INDU_SYNT)^2)
str(INDU_SCEs)

######## SAVE FILES BEFORE RUNNING SCRIPT WITH OTHER FACTOR CLASS #####

names <- INDU_SCEs[1]
Z_prime <- INDU_SCEs[3]
INDU_Z_prime <- cbind(names,Z_prime)
write.csv(INDU_Z_prime, file = "INDU_Z_prime.csv", na = "") 

########## Box-Plot for Z'-Score ####################################

jpeg('D:/Input_data/Rel_Categories/Input_data/Rel_Categories/Z_prime_graphs/INDU_Z_prime.jpg',
     quality = 100, bg = "white", res = 200, width = 7, height = 7, units = "in")

names <- (INDU_SCEs[,1])
names <-as.vector(names)

par(mar=c(13, 4, 2, 2) + 0.3)
oldpar <- par(las=2)
barplot(INDU_SCEs$Z_prime, width = 0.9, space = 0.35, 
        names.arg = names, border = 1,
        xlab= "", ylab = "Z'_score",
        xlim = c(1, (nrow(INDU_Z_prime)+5.5)), ylim = c(-4, 4),
        cex.lab=1, cex.axis=0.8, cex.names = 0.8, col.axis="black")
title(main ="Industry-Cement Z'_score", font.main= 1.5)
abline(h=0,col=1, lty=1, lwd=1)
abline(h=1,col=4, lty=1, lwd=2)
abline(h=2,col=1, lty=1, lwd=2)
abline(h=3,col=3, lty=1, lwd=2)
abline(h=-1,col=4, lty=1, lwd=2)
abline(h=-2,col=1, lty=1, lwd=2)
abline(h=-3,col=3, lty=1, lwd=2)
par(oldpar)

dev.off()
