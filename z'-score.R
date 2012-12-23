
Proficiency tests are performed according to the ISO 13528:
R codes have been written for:

Z’- score
Z - score
En- number

#############################################################################################################

setwd(D:/Input_data/Rel_Categories/Input_data/Rel_Categories")

SCEs <- list.files(path =D:/Input_data/Rel_Categories/Input_data/Rel_Categories",
                   pattern = "SCEs") #this includes only files with "rel"


for (i in c(1:length(SCEs))) {
  assign(gsub("[.]csv$","",SCEs[i]), read.csv(SCEs[i], header=TRUE, 
                                              as.is = 1))
}


################## Calculation of Source Contribution Estimation (SCE) ###
###################  for each CLASS #####################################

##########################################################################
############################# BioB_SCEs ##################################

median <- median(BioB_SCEs[,2])

BioB_SCEs$ABS <- abs(BioB_SCEs[,2]-median) ####column for absolute difference values
BioB_SCEs$SCE_Iter1 <- BioB_SCEs[,2]  ####column for the first iteration

s <- 1.483*median(BioB_SCEs[,3])
d <- 1.5*s
diff <- median - d
sum  <- median + d

###############FIRST ITERATION
for (i in seq_len(nrow(BioB_SCEs))) {
  if (BioB_SCEs[i,4]< diff) {
    BioB_SCEs[i,4]<- diff 
  } else if (BioB_SCEs[i,4] > sum) {
    BioB_SCEs[i,4] <- sum
  } else 
    BioB_SCEs[i,4]
}

BioB_SCEs$SCE_Iter2 <- BioB_SCEs[,4] ####column for the second iteration
s <- 1.134*sd(BioB_SCEs[,4])
d <- 1.5*s
diff <- mean(BioB_SCEs[,4]) - d
sum  <- mean(BioB_SCEs[,4]) + d

##############SECOND ITERATION
for (i in seq_len(nrow(BioB_SCEs))) {
  if (BioB_SCEs[i,5]< diff) {
    BioB_SCEs[i,5]<- diff 
  } else if (BioB_SCEs[i,5] > sum) {
    BioB_SCEs[i,5] <- sum
  } else 
    BioB_SCEs[i,5]
}

X <- mean(BioB_SCEs[,5])  ###assigned value ALWAYS CALCULATED 
###on the LAST ITERATION

##########Z score calculation for Source Contribution Estimation
########################## Z_SCE ###############################

BioB_SCEs$Z_score <- BioB_SCEs[,5]
BioB_SCEs[,6] <- (BioB_SCEs[,2] - X)/(X*0.5)  ####proficiency at 50%
str(BioB_SCEs)

######## SAVE FILES BEFORE RUNNING SCRIPT WITH OTHER FACTOR CLASS #####
names <- BioB_SCEs[1]
Zscores <- BioB_SCEs[6]
BioB_Zscore <- cbind(names,Zscores)
write.csv(BioB_Zscore, file = "BioB_Zscore.csv", na = "") 

########## Box-Plot for Z-Score ####################################

jpeg('D:/Input_data/Rel_Categories/Input_data/Rel_Categories/Z_graphs/BioB_Z.jpg',
     quality = 100, bg = "white", res = 200, width = 7, height = 7, units = "in")

names <- (BioB_SCEs[,1])
names <-as.vector(names)

par(mar=c(12, 4, 2, 2) + 0.3)
oldpar <- par(las=2)
barplot(BioB_SCEs$Z_score, width = 0.9, space = 0.35, 
        names.arg = names, border = 1,
        xlab= "", ylab = "Z_score",
        xlim = c(1, (nrow(BioB_Zscore)+5)), ylim = c(-4, 6.5),
        cex.lab=1, cex.axis=0.8, cex.names = 0.8, col.axis="black")
title(main ="Z-score (Biomass Burning)", font.main= 1.5)
abline(h=0,col=1, lty=1, lwd=1)
abline(h=1,col=4, lty=1, lwd=2)
abline(h=2,col=1, lty=1, lwd=2)
abline(h=3,col=3, lty=1, lwd=2)
abline(h=-1,col=4, lty=1, lwd=2)
abline(h=-2,col=1, lty=1, lwd=2)
abline(h=-3,col=3, lty=1, lwd=2)
par(oldpar)

dev.off()

##################################################################

rm(list=ls(all=TRUE)) 

#################################################################

setwd(D:/Input_data/Rel_Categories/Input_data/Rel_Categories")

SCEs <- list.files(path =D:/Input_data/Rel_Categories/Input_data/Rel_Categories",
                   pattern = "SCEs") #this includes only files with "rel"


for (i in c(1:length(SCEs))) {
  assign(gsub("[.]csv$","",SCEs[i]), read.csv(SCEs[i], header=TRUE, 
                                              as.is = 1))
}

#########################################################################
############################# SO4_SCEs ##################################

median <- median(SO4_SCEs[,2])

SO4_SCEs$ABS <- abs(SO4_SCEs[,2]-median) ####column for absolute difference values
SO4_SCEs$SCE_Iter1 <- SO4_SCEs[,2]  ####column for the first iteration

s <- 1.483*median(SO4_SCEs[,3])
d <- 1.5*s
diff <- median - d
sum  <- median + d

###############FIRST ITERATION
for (i in seq_len(nrow(SO4_SCEs))) {
  if (SO4_SCEs[i,4]< diff) {
    SO4_SCEs[i,4]<- diff 
  } else if (SO4_SCEs[i,4] > sum) {
    SO4_SCEs[i,4] <- sum
  } else 
    SO4_SCEs[i,4]
}

SO4_SCEs$SCE_Iter2 <- SO4_SCEs[,4] ####column for the second iteration
s <- 1.134*sd(SO4_SCEs[,4])
d <- 1.5*s
diff <- mean(SO4_SCEs[,4]) - d
sum  <- mean(SO4_SCEs[,4]) + d

##############SECOND ITERATION
for (i in seq_len(nrow(SO4_SCEs))) {
  if (SO4_SCEs[i,5]< diff) {
    SO4_SCEs[i,5]<- diff 
  } else if (SO4_SCEs[i,5] > sum) {
    SO4_SCEs[i,5] <- sum
  } else 
    SO4_SCEs[i,5]
}

X <- mean(SO4_SCEs[,5])  ###assigned value ALWAYS CALCULATED 
###on the LAST ITERATION

##########Z score calculation for Source Contribution Estimation
#######Z_SCE##########

SO4_SCEs$Z_score <- SO4_SCEs[,5]
SO4_SCEs[,6] <- (SO4_SCEs[,2] - X)/(X*0.5)  ####proficiency at 50%

######## SAVE FILES BEFORE RUNNING SCRIPT WITH OTHER FACTOR CLASS #####
names <- SO4_SCEs[1]
Zscores <- SO4_SCEs[6]
SO4_Zscore <- cbind(names,Zscores)
write.csv(SO4_Zscore, file = "SO4_Zscore.csv", na = "") 

########## Box-Plot for Z-Score ####################################

jpeg('D:/Input_data/Rel_Categories/Input_data/Rel_Categories/Z_graphs/SO4_Z.jpg',
     quality = 100, bg = "white", res = 200, width = 7, height = 7, units = "in")

names <- (SO4_SCEs[,1])
names <-as.vector(names)

par(mar=c(12, 5, 3, 2) + 0.2)
oldpar <- par(las=2)
barplot(SO4_SCEs$Z_score, width = 0.9, space = 0.35, 
        names.arg = names, border = 1,
        xlab= "", ylab = "Z_score",
        xlim = c(1, (nrow(SO4_Zscore)+4)), ylim = c(-4, 4),
        cex.lab=1, cex.axis=0.8, cex.names = 0.8, col.axis="black")
title(main ="Z-score (Sulfate,SO4)", font.main= 1.5)
abline(h=0,col=1, lty=1, lwd=1)
abline(h=1,col=4, lty=1, lwd=2)
abline(h=2,col=1, lty=1, lwd=2)
abline(h=3,col=3, lty=1, lwd=2)
abline(h=-1,col=4, lty=1, lwd=2)
abline(h=-2,col=1, lty=1, lwd=2)
abline(h=-3,col=3, lty=1, lwd=2)
par(oldpar)

dev.off()

##################################################################

rm(list=ls(all=TRUE))

################################################################

setwd(D:/Input_data/Rel_Categories/Input_data/Rel_Categories")

SCEs <- list.files(path =D:/Input_data/Rel_Categories/Input_data/Rel_Categories",
                   pattern = "SCEs") #this includes only files with "rel"


for (i in c(1:length(SCEs))) {
  assign(gsub("[.]csv$","",SCEs[i]), read.csv(SCEs[i], header=TRUE, 
                                              as.is = 1))
}


#########################################################################
############################# NO3_SCEs ##################################

median <- median(NO3_SCEs[,2])

NO3_SCEs$ABS <- abs(NO3_SCEs[,2]-median) ####column for absolute difference values
NO3_SCEs$SCE_Iter1 <- NO3_SCEs[,2]  ####column for the first iteration

s <- 1.483*median(NO3_SCEs[,3])
d <- 1.5*s
diff <- median - d
sum  <- median + d

###############FIRST ITERATION
for (i in seq_len(nrow(NO3_SCEs))) {
  if (NO3_SCEs[i,4]< diff) {
    NO3_SCEs[i,4]<- diff 
  } else if (NO3_SCEs[i,4] > sum) {
    NO3_SCEs[i,4] <- sum
  } else 
    NO3_SCEs[i,4]
}

NO3_SCEs$SCE_Iter2 <- NO3_SCEs[,4] ####column for the second iteration
s <- 1.134*sd(NO3_SCEs[,4])
d <- 1.5*s
diff <- mean(NO3_SCEs[,4]) - d
sum  <- mean(NO3_SCEs[,4]) + d

##############SECOND ITERATION
for (i in seq_len(nrow(NO3_SCEs))) {
  if (NO3_SCEs[i,5]< diff) {
    NO3_SCEs[i,5]<- diff 
  } else if (NO3_SCEs[i,5] > sum) {
    NO3_SCEs[i,5] <- sum
  } else 
    NO3_SCEs[i,5]
}

NO3_SCEs$SCE_Iter3 <- NO3_SCEs[,5] ####column for the second iteration
s <- 1.134*sd(NO3_SCEs[,5])
d <- 1.5*s
diff <- mean(NO3_SCEs[,5]) - d
sum  <- mean(NO3_SCEs[,5]) + d

##############THIRD ITERATION
for (i in seq_len(nrow(NO3_SCEs))) {
  if (NO3_SCEs[i,6]< diff) {
    NO3_SCEs[i,6]<- diff 
  } else if (NO3_SCEs[i,6] > sum) {
    NO3_SCEs[i,6] <- sum
  } else 
    NO3_SCEs[i,6]
}

NO3_SCEs$SCE_Iter4 <- NO3_SCEs[,6] ####column for the second iteration
s <- 1.134*sd(NO3_SCEs[,6])
d <- 1.5*s
diff <- mean(NO3_SCEs[,6]) - d
sum  <- mean(NO3_SCEs[,6]) + d

##############FOURTH ITERATION
for (i in seq_len(nrow(NO3_SCEs))) {
  if (NO3_SCEs[i,7]< diff) {
    NO3_SCEs[i,7]<- diff 
  } else if (NO3_SCEs[i,7] > sum) {
    NO3_SCEs[i,7] <- sum
  } else 
    NO3_SCEs[i,7]
}

NO3_SCEs$SCE_Iter5 <- NO3_SCEs[,7] ####column for the second iteration
s <- 1.134*sd(NO3_SCEs[,7])
d <- 1.5*s
diff <- mean(NO3_SCEs[,7]) - d
sum  <- mean(NO3_SCEs[,7]) + d

##############FIFTH ITERATION
for (i in seq_len(nrow(NO3_SCEs))) {
  if (NO3_SCEs[i,8]< diff) {
    NO3_SCEs[i,8]<- diff 
  } else if (NO3_SCEs[i,8] > sum) {
    NO3_SCEs[i,8] <- sum
  } else 
    NO3_SCEs[i,8]
}

NO3_SCEs$SCE_Iter6 <- NO3_SCEs[,8] ####column for the second iteration
s <- 1.134*sd(NO3_SCEs[,8])
d <- 1.5*s
diff <- mean(NO3_SCEs[,8]) - d
sum  <- mean(NO3_SCEs[,8]) + d

##############SIXTH ITERATION
for (i in seq_len(nrow(NO3_SCEs))) {
  if (NO3_SCEs[i,9]< diff) {
    NO3_SCEs[i,9]<- diff 
  } else if (NO3_SCEs[i,9] > sum) {
    NO3_SCEs[i,9] <- sum
  } else 
    NO3_SCEs[i,9]
}

NO3_SCEs$SCE_Iter7 <- NO3_SCEs[,9] ####column for the second iteration
s <- 1.134*sd(NO3_SCEs[,9])
d <- 1.5*s
diff <- mean(NO3_SCEs[,9]) - d
sum  <- mean(NO3_SCEs[,9]) + d

##############SEVENTH ITERATION
for (i in seq_len(nrow(NO3_SCEs))) {
  if (NO3_SCEs[i,10]< diff) {
    NO3_SCEs[i,10]<- diff 
  } else if (NO3_SCEs[i,10] > sum) {
    NO3_SCEs[i,10] <- sum
  } else 
    NO3_SCEs[i,10]
}

NO3_SCEs$SCE_Iter8 <- NO3_SCEs[,10] ####column for the second iteration
s <- 1.134*sd(NO3_SCEs[,10])
d <- 1.5*s
diff <- mean(NO3_SCEs[,10]) - d
sum  <- mean(NO3_SCEs[,10]) + d

##############EIGHT ITERATION
for (i in seq_len(nrow(NO3_SCEs))) {
  if (NO3_SCEs[i,11]< diff) {
    NO3_SCEs[i,11]<- diff 
  } else if (NO3_SCEs[i,11] > sum) {
    NO3_SCEs[i,11] <- sum
  } else 
    NO3_SCEs[i,11]
}

NO3_SCEs$SCE_Iter9 <- NO3_SCEs[,11] ####column for the second iteration
s <- 1.134*sd(NO3_SCEs[,11])
d <- 1.5*s
diff <- mean(NO3_SCEs[,11]) - d
sum  <- mean(NO3_SCEs[,11]) + d

##############NINETH ITERATION
for (i in seq_len(nrow(NO3_SCEs))) {
  if (NO3_SCEs[i,12]< diff) {
    NO3_SCEs[i,12]<- diff 
  } else if (NO3_SCEs[i,12] > sum) {
    NO3_SCEs[i,12] <- sum
  } else 
    NO3_SCEs[i,12]
}

NO3_SCEs$SCE_Iter10 <- NO3_SCEs[,12] ####column for the second iteration
s <- 1.134*sd(NO3_SCEs[,12])
d <- 1.5*s
diff <- mean(NO3_SCEs[,12]) - d
sum  <- mean(NO3_SCEs[,12]) + d

##############TENTH ITERATION
for (i in seq_len(nrow(NO3_SCEs))) {
  if (NO3_SCEs[i,13]< diff) {
    NO3_SCEs[i,13]<- diff 
  } else if (NO3_SCEs[i,13] > sum) {
    NO3_SCEs[i,13] <- sum
  } else 
    NO3_SCEs[i,13]
}

NO3_SCEs$SCE_Iter11 <- NO3_SCEs[,13] ####column for the second iteration
s <- 1.134*sd(NO3_SCEs[,13])
d <- 1.5*s
diff <- mean(NO3_SCEs[,13]) - d
sum  <- mean(NO3_SCEs[,13]) + d

##############ELEVENTH ITERATION
for (i in seq_len(nrow(NO3_SCEs))) {
  if (NO3_SCEs[i,14]< diff) {
    NO3_SCEs[i,14]<- diff 
  } else if (NO3_SCEs[i,14] > sum) {
    NO3_SCEs[i,14] <- sum
  } else 
    NO3_SCEs[i,14]
}

NO3_SCEs$SCE_Iter12 <- NO3_SCEs[,14] ####column for the second iteration
s <- 1.134*sd(NO3_SCEs[,14])
d <- 1.5*s
diff <- mean(NO3_SCEs[,14]) - d
sum  <- mean(NO3_SCEs[,14]) + d

##############ELEVENTH ITERATION
for (i in seq_len(nrow(NO3_SCEs))) {
  if (NO3_SCEs[i,15]< diff) {
    NO3_SCEs[i,15]<- diff 
  } else if (NO3_SCEs[i,15] > sum) {
    NO3_SCEs[i,15] <- sum
  } else 
    NO3_SCEs[i,15]
}


X <- mean(NO3_SCEs[,15])  ###assigned value ALWAYS CALCULATED 
###on the LAST ITERATION

##########Z score calculation for Source Contribution Estimation
#######Z_SCE##########

NO3_SCEs$Z_score <- NO3_SCEs[,15]
NO3_SCEs[,16] <- (NO3_SCEs[,2] - X)/(X*0.5)  ####proficiency at 50%

######## SAVE FILES BEFORE RUNNING SCRIPT WITH OTHER FACTOR CLASS #####
names <- NO3_SCEs[1]
Zscores <- NO3_SCEs[16]
NO3_Zscore <- cbind(names,Zscores)
write.csv(NO3_Zscore, file = "NO3_Zscore.csv", na = "") 

########## Box-Plot for Z-Score ####################################

jpeg('D:/Input_data/Rel_Categories/Input_data/Rel_Categories/Z_graphs/NO3_Z.jpg',
     quality = 100, bg = "white", res = 200, width = 7, height = 7, units = "in")

names <- (NO3_SCEs[,1])
names <-as.vector(names)

par(mar=c(12, 5, 3, 1) + 0.3)
oldpar <- par(las=2)
barplot(NO3_SCEs$Z_score, width = 0.9, space = 0.35, 
        names.arg = names, border = 1,
        xlab= "", ylab = "Z_score",
        xlim = c(1, (nrow(NO3_Zscore)+4)), ylim = c(-4, 4),
        cex.lab=1, cex.axis=0.8, cex.names = 0.8, col.axis="black")
title(main ="Z-score (Nitrate, NO3)", font.main= 1.5)
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
##################################################################

rm(list=ls(all=TRUE))

################################################################

setwd(D:/Input_data/Rel_Categories/Input_data/Rel_Categories")

SCEs <- list.files(path =D:/Input_data/Rel_Categories/Input_data/Rel_Categories",
                   pattern = "SCEs") #this includes only files with "rel"


for (i in c(1:length(SCEs))) {
  assign(gsub("[.]csv$","",SCEs[i]), read.csv(SCEs[i], header=TRUE, 
                                              as.is = 1))
}


##########################################################################
############################# DUST_SCEs ##################################

median <- median(DUST_SCEs[,2])

DUST_SCEs$ABS <- abs(DUST_SCEs[,2]-median) ####column for absolute difference values
DUST_SCEs$SCE_Iter1 <- DUST_SCEs[,2]  ####column for the first iteration

s <- 1.483*median(DUST_SCEs[,3])
d <- 1.5*s
diff <- median - d
sum  <- median + d

###############FIRST ITERATION
for (i in seq_len(nrow(DUST_SCEs))) {
  if (DUST_SCEs[i,4]< diff) {
    DUST_SCEs[i,4]<- diff 
  } else if (DUST_SCEs[i,4] > sum) {
    DUST_SCEs[i,4] <- sum
  } else 
    DUST_SCEs[i,4]
}

DUST_SCEs$SCE_Iter2 <- DUST_SCEs[,4] ####column for the second iteration
s <- 1.134*sd(DUST_SCEs[,4])
d <- 1.5*s
diff <- mean(DUST_SCEs[,4]) - d
sum  <- mean(DUST_SCEs[,4]) + d

##############SECOND ITERATION
for (i in seq_len(nrow(DUST_SCEs))) {
  if (DUST_SCEs[i,5]< diff) {
    DUST_SCEs[i,5]<- diff 
  } else if (DUST_SCEs[i,5] > sum) {
    DUST_SCEs[i,5] <- sum
  } else 
    DUST_SCEs[i,5]
}

DUST_SCEs$SCE_Iter3 <- DUST_SCEs[,5] ####column for the second iteration
s <- 1.134*sd(DUST_SCEs[,5])
d <- 1.5*s
diff <- mean(DUST_SCEs[,5]) - d
sum  <- mean(DUST_SCEs[,5]) + d

##############THIRD ITERATION
for (i in seq_len(nrow(DUST_SCEs))) {
  if (DUST_SCEs[i,6]< diff) {
    DUST_SCEs[i,6]<- diff 
  } else if (DUST_SCEs[i,6] > sum) {
    DUST_SCEs[i,6] <- sum
  } else 
    DUST_SCEs[i,6]
}

DUST_SCEs$SCE_Iter4 <- DUST_SCEs[,6] ####column for the second iteration
s <- 1.134*sd(DUST_SCEs[,6])
d <- 1.5*s
diff <- mean(DUST_SCEs[,6]) - d
sum  <- mean(DUST_SCEs[,6]) + d

##############FOURTH ITERATION
for (i in seq_len(nrow(DUST_SCEs))) {
  if (DUST_SCEs[i,7]< diff) {
    DUST_SCEs[i,7]<- diff 
  } else if (DUST_SCEs[i,7] > sum) {
    DUST_SCEs[i,7] <- sum
  } else 
    DUST_SCEs[i,7]
}

DUST_SCEs$SCE_Iter5 <- DUST_SCEs[,7] ####column for the second iteration
s <- 1.134*sd(DUST_SCEs[,7])
d <- 1.5*s
diff <- mean(DUST_SCEs[,7]) - d
sum  <- mean(DUST_SCEs[,7]) + d

##############FIFTH ITERATION
for (i in seq_len(nrow(DUST_SCEs))) {
  if (DUST_SCEs[i,8]< diff) {
    DUST_SCEs[i,8]<- diff 
  } else if (DUST_SCEs[i,8] > sum) {
    DUST_SCEs[i,8] <- sum
  } else 
    DUST_SCEs[i,8]
}

DUST_SCEs$SCE_Iter6 <- DUST_SCEs[,8] ####column for the second iteration
s <- 1.134*sd(DUST_SCEs[,8])
d <- 1.5*s
diff <- mean(DUST_SCEs[,8]) - d
sum  <- mean(DUST_SCEs[,8]) + d

##############SIXTH ITERATION
for (i in seq_len(nrow(DUST_SCEs))) {
  if (DUST_SCEs[i,9]< diff) {
    DUST_SCEs[i,9]<- diff 
  } else if (DUST_SCEs[i,9] > sum) {
    DUST_SCEs[i,9] <- sum
  } else 
    DUST_SCEs[i,9]
}

DUST_SCEs$SCE_Iter7 <- DUST_SCEs[,9] ####column for the second iteration
s <- 1.134*sd(DUST_SCEs[,9])
d <- 1.5*s
diff <- mean(DUST_SCEs[,9]) - d
sum  <- mean(DUST_SCEs[,9]) + d

##############SEVENTH ITERATION
for (i in seq_len(nrow(DUST_SCEs))) {
  if (DUST_SCEs[i,10]< diff) {
    DUST_SCEs[i,10]<- diff 
  } else if (DUST_SCEs[i,10] > sum) {
    DUST_SCEs[i,10] <- sum
  } else 
    DUST_SCEs[i,10]
}

DUST_SCEs$SCE_Iter8 <- DUST_SCEs[,10] ####column for the second iteration
s <- 1.134*sd(DUST_SCEs[,10])
d <- 1.5*s
diff <- mean(DUST_SCEs[,10]) - d
sum  <- mean(DUST_SCEs[,10]) + d

##############EIGHT ITERATION
for (i in seq_len(nrow(DUST_SCEs))) {
  if (DUST_SCEs[i,11]< diff) {
    DUST_SCEs[i,11]<- diff 
  } else if (DUST_SCEs[i,11] > sum) {
    DUST_SCEs[i,11] <- sum
  } else 
    DUST_SCEs[i,11]
}

DUST_SCEs$SCE_Iter9 <- DUST_SCEs[,11] ####column for the second iteration
s <- 1.134*sd(DUST_SCEs[,11])
d <- 1.5*s
diff <- mean(DUST_SCEs[,11]) - d
sum  <- mean(DUST_SCEs[,11]) + d

##############NINETH ITERATION
for (i in seq_len(nrow(DUST_SCEs))) {
  if (DUST_SCEs[i,12]< diff) {
    DUST_SCEs[i,12]<- diff 
  } else if (DUST_SCEs[i,12] > sum) {
    DUST_SCEs[i,12] <- sum
  } else 
    DUST_SCEs[i,12]
}

DUST_SCEs$SCE_Iter10 <- DUST_SCEs[,12] ####column for the second iteration
s <- 1.134*sd(DUST_SCEs[,12])
d <- 1.5*s
diff <- mean(DUST_SCEs[,12]) - d
sum  <- mean(DUST_SCEs[,12]) + d

##############TENTH ITERATION
for (i in seq_len(nrow(DUST_SCEs))) {
  if (DUST_SCEs[i,13]< diff) {
    DUST_SCEs[i,13]<- diff 
  } else if (DUST_SCEs[i,13] > sum) {
    DUST_SCEs[i,13] <- sum
  } else 
    DUST_SCEs[i,13]
}

DUST_SCEs$SCE_Iter11 <- DUST_SCEs[,13] ####column for the second iteration
s <- 1.134*sd(DUST_SCEs[,13])
d <- 1.5*s
diff <- mean(DUST_SCEs[,13]) - d
sum  <- mean(DUST_SCEs[,13]) + d

##############ELEVENTH ITERATION
for (i in seq_len(nrow(DUST_SCEs))) {
  if (DUST_SCEs[i,14]< diff) {
    DUST_SCEs[i,14]<- diff 
  } else if (DUST_SCEs[i,14] > sum) {
    DUST_SCEs[i,14] <- sum
  } else 
    DUST_SCEs[i,14]
}


X <- mean(DUST_SCEs[,14])  ###assigned value ALWAYS CALCULATED 
###on the LAST ITERATION

##########Z score calculation for Source Contribution Estimation
#######Z_SCE##########

DUST_SCEs$Z_score <- DUST_SCEs[,14]
DUST_SCEs[,15] <- (DUST_SCEs[,2] - X)/(X*0.5)  ####proficiency at 50%

######## SAVE FILES BEFORE RUNNING SCRIPT WITH OTHER FACTOR CLASS #####
names <- DUST_SCEs[1]
Zscores <- DUST_SCEs[15]
DUST_Zscore <- cbind(names,Zscores)
write.csv(DUST_Zscore, file = "DUST_Zscore.csv", na = "") 

########## Box-Plot for Z-Score ####################################

jpeg('D:/Input_data/Rel_Categories/Input_data/Rel_Categories/Z_graphs/DUST_Z.jpg',
     quality = 100, bg = "white", res = 200, width = 7, height = 7, units = "in")

names <- (DUST_SCEs[,1])
names <-as.vector(names)

par(mar=c(12, 4, 3, 1) + 0.2)
oldpar <- par(las=2)
barplot(DUST_SCEs$Z_score, width = 0.9, space = 0.35, 
        names.arg = names, border = 1,
        xlab= "", ylab = "Z_score",
        xlim = c(1, (nrow(DUST_Zscore)+4)), ylim = c(-4, 4),
        cex.lab=1, cex.axis=0.8, cex.names = 0.8, col.axis="black")
title(main ="Z-score (Dust Re-suspension)", font.main= 1.5)
abline(h=0,col=1, lty=1, lwd=1)
abline(h=1,col=4, lty=1, lwd=2)
abline(h=2,col=1, lty=1, lwd=2)
abline(h=3,col=3, lty=1, lwd=2)
abline(h=-1,col=4, lty=1, lwd=2)
abline(h=-2,col=1, lty=1, lwd=2)
abline(h=-3,col=3, lty=1, lwd=2)
par(oldpar)

dev.off()

##################################################################

rm(list=ls(all=TRUE))

################################################################

setwd(D:/Input_data/Rel_Categories/Input_data/Rel_Categories")

SCEs <- list.files(path =D:/Input_data/Rel_Categories/Input_data/Rel_Categories",
                   pattern = "SCEs") #this includes only files with "rel"


for (i in c(1:length(SCEs))) {
  assign(gsub("[.]csv$","",SCEs[i]), read.csv(SCEs[i], header=TRUE, 
                                              as.is = 1))
}


##########################################################################
############################# ROAD_SCEs ##################################

median <- median(ROAD_SCEs[,2])

ROAD_SCEs$ABS <- abs(ROAD_SCEs[,2]-median) ####column for absolute difference values
ROAD_SCEs$SCE_Iter1 <- ROAD_SCEs[,2]  ####column for the first iteration

s <- 1.483*median(ROAD_SCEs[,3])
d <- 1.5*s
diff <- median - d
sum  <- median + d

###############FIRST ITERATION
for (i in seq_len(nrow(ROAD_SCEs))) {
  if (ROAD_SCEs[i,4]< diff) {
    ROAD_SCEs[i,4]<- diff 
  } else if (ROAD_SCEs[i,4] > sum) {
    ROAD_SCEs[i,4] <- sum
  } else 
    ROAD_SCEs[i,4]
}

ROAD_SCEs$SCE_Iter2 <- ROAD_SCEs[,4] ####column for the second iteration
s <- 1.134*sd(ROAD_SCEs[,4])
d <- 1.5*s
diff <- mean(ROAD_SCEs[,4]) - d
sum  <- mean(ROAD_SCEs[,4]) + d

##############SECOND ITERATION
for (i in seq_len(nrow(ROAD_SCEs))) {
  if (ROAD_SCEs[i,5]< diff) {
    ROAD_SCEs[i,5]<- diff 
  } else if (ROAD_SCEs[i,5] > sum) {
    ROAD_SCEs[i,5] <- sum
  } else 
    ROAD_SCEs[i,5]
}

X <- mean(ROAD_SCEs[,5])  ###assigned value ALWAYS CALCULATED 
###on the LAST ITERATION

##########Z score calculation for Source Contribution Estimation
#######Z_SCE##########

ROAD_SCEs$Z_score <- ROAD_SCEs[,5]
ROAD_SCEs[,6] <- (ROAD_SCEs[,2] - X)/(X*0.5)  ####proficiency at 50%

######## SAVE FILES BEFORE RUNNING SCRIPT WITH OTHER FACTOR CLASS #####
names <- ROAD_SCEs[1]
Zscores <- ROAD_SCEs[6]
ROAD_Zscore <- cbind(names,Zscores)
write.csv(ROAD_Zscore, file = "ROAD_Zscore.csv", na = "") 

########## Box-Plot for Z-Score ####################################

jpeg('D:/Input_data/Rel_Categories/Input_data/Rel_Categories/Z_graphs/ROAD_Z.jpg',
     quality = 100, bg = "white", res = 200, width = 7, height = 7, units = "in")

names <- (ROAD_SCEs[,1])
names <-as.vector(names)

par(mar=c(11, 5, 3, 1) + 0.3)
oldpar <- par(las=2)
barplot(ROAD_SCEs$Z_score, width = 0.9, space = 0.35, 
        names.arg = names, border = 1,
        xlab= "", ylab = "Z_score",
        xlim = c(0.5, (nrow(ROAD_Zscore)+3)), ylim = c(-4, 4),
        cex.lab=1, cex.axis=0.8, cex.names = 0.8, col.axis="black")
title(main ="Z-score (Road Dust)", font.main= 1.5)
abline(h=0,col=1, lty=1, lwd=1)
abline(h=1,col=4, lty=1, lwd=2)
abline(h=2,col=1, lty=1, lwd=2)
abline(h=3,col=3, lty=1, lwd=2)
abline(h=-1,col=4, lty=1, lwd=2)
abline(h=-2,col=1, lty=1, lwd=2)
abline(h=-3,col=3, lty=1, lwd=2)
par(oldpar)

dev.off()

##################################################################

rm(list=ls(all=TRUE))

################################################################

setwd(D:/Input_data/Rel_Categories/Input_data/Rel_Categories")

SCEs <- list.files(path =D:/Input_data/Rel_Categories/Input_data/Rel_Categories",
                   pattern = "SCEs") #this includes only files with "rel"


for (i in c(1:length(SCEs))) {
  assign(gsub("[.]csv$","",SCEs[i]), read.csv(SCEs[i], header=TRUE, 
                                              as.is = 1))
}

#########################################################################
############################# SALT_SCEs ##################################

median <- median(SALT_SCEs[,2])

SALT_SCEs$ABS <- abs(SALT_SCEs[,2]-median) ####column for absolute difference values
SALT_SCEs$SCE_Iter1 <- SALT_SCEs[,2]  ####column for the first iteration

s <- 1.483*median(SALT_SCEs[,3])
d <- 1.5*s
diff <- median - d
sum  <- median + d

###############FIRST ITERATION
for (i in seq_len(nrow(SALT_SCEs))) {
  if (SALT_SCEs[i,4]< diff) {
    SALT_SCEs[i,4]<- diff 
  } else if (SALT_SCEs[i,4] > sum) {
    SALT_SCEs[i,4] <- sum
  } else 
    SALT_SCEs[i,4]
}

SALT_SCEs$SCE_Iter2 <- SALT_SCEs[,4] ####column for the second iteration
s <- 1.134*sd(SALT_SCEs[,4])
d <- 1.5*s
diff <- mean(SALT_SCEs[,4]) - d
sum  <- mean(SALT_SCEs[,4]) + d

##############SECOND ITERATION
for (i in seq_len(nrow(SALT_SCEs))) {
  if (SALT_SCEs[i,5]< diff) {
    SALT_SCEs[i,5]<- diff 
  } else if (SALT_SCEs[i,5] > sum) {
    SALT_SCEs[i,5] <- sum
  } else 
    SALT_SCEs[i,5]
}

X <- mean(SALT_SCEs[,5])  ###assigned value ALWAYS CALCULATED 
###on the LAST ITERATION

##########Z score calculation for Source Contribution Estimation
#######Z_SCE##########

SALT_SCEs$Z_score <- SALT_SCEs[,5]
SALT_SCEs[,6] <- (SALT_SCEs[,2] - X)/(X*0.5)  ####proficiency at 50%

######## SAVE FILES BEFORE RUNNING SCRIPT WITH OTHER FACTOR CLASS #####
names <- SALT_SCEs[1]
Zscores <- SALT_SCEs[6]
SALT_Zscore <- cbind(names,Zscores)
write.csv(SALT_Zscore, file = "SALT_Zscore.csv", na = "") 

########## Box-Plot for Z-Score ####################################

jpeg('D:/Input_data/Rel_Categories/Input_data/Rel_Categories/Z_graphs/SALT_Z.jpg',
     quality = 100, bg = "white", res = 200, width = 7, height = 7, units = "in")

names <- (SALT_SCEs[,1])
names <-as.vector(names)

par(mar=c(12, 5, 3, 1) + 0.3)
oldpar <- par(las=2)
barplot(SALT_SCEs$Z_score, width = 0.9, space = 0.35, 
        names.arg = names, border = 1,
        xlab= "", ylab = "Z_score",
        xlim = c(0.5, (nrow(SALT_Zscore)+4)), ylim = c(-4, 4),
        cex.lab=1, cex.axis=0.8, cex.names = 0.8, col.axis="black")
title(main ="Z-score (Salt)", font.main= 1.5)
abline(h=0,col=1, lty=1, lwd=1)
abline(h=1,col=4, lty=1, lwd=2)
abline(h=2,col=1, lty=1, lwd=2)
abline(h=3,col=3, lty=1, lwd=2)
abline(h=-1,col=4, lty=1, lwd=2)
abline(h=-2,col=1, lty=1, lwd=2)
abline(h=-3,col=3, lty=1, lwd=2)
par(oldpar)

dev.off()

##################################################################

##################################################################

rm(list=ls(all=TRUE))

################################################################

setwd(D:/Input_data/Rel_Categories/Input_data/Rel_Categories")

SCEs <- list.files(path =D:/Input_data/Rel_Categories/Input_data/Rel_Categories",
                   pattern = "SCEs") #this includes only files with "rel"


for (i in c(1:length(SCEs))) {
  assign(gsub("[.]csv$","",SCEs[i]), read.csv(SCEs[i], header=TRUE, 
                                              as.is = 1))
}


########################################################################
############################# TRA_SCEs #################################

median <- median(TRA_SCEs[,2])

TRA_SCEs$ABS <- abs(TRA_SCEs[,2]-median) ####column for absolute difference values
TRA_SCEs$SCE_Iter1 <- TRA_SCEs[,2]  ####column for the first iteration

s <- 1.483*median(TRA_SCEs[,3])
d <- 1.5*s
diff <- median - d
sum  <- median + d

###############FIRST ITERATION
for (i in seq_len(nrow(TRA_SCEs))) {
  if (TRA_SCEs[i,4]< diff) {
    TRA_SCEs[i,4]<- diff 
  } else if (TRA_SCEs[i,4] > sum) {
    TRA_SCEs[i,4] <- sum
  } else 
    TRA_SCEs[i,4]
}

TRA_SCEs$SCE_Iter2 <- TRA_SCEs[,4] ####column for the second iteration
s <- 1.134*sd(TRA_SCEs[,4])
d <- 1.5*s
diff <- mean(TRA_SCEs[,4]) - d
sum  <- mean(TRA_SCEs[,4]) + d

##############SECOND ITERATION
for (i in seq_len(nrow(TRA_SCEs))) {
  if (TRA_SCEs[i,5]< diff) {
    TRA_SCEs[i,5]<- diff 
  } else if (TRA_SCEs[i,5] > sum) {
    TRA_SCEs[i,5] <- sum
  } else 
    TRA_SCEs[i,5]
}

X <- mean(TRA_SCEs[,5])  ###assigned value ALWAYS CALCULATED 
###on the LAST ITERATION

##########Z score calculation for Source Contribution Estimation
#######Z_SCE##########

TRA_SCEs$Z_score <- TRA_SCEs[,5]
TRA_SCEs[,6] <- (TRA_SCEs[,2] - X)/(X*0.5)  ####proficiency at 50%

######## SAVE FILES BEFORE RUNNING SCRIPT WITH OTHER FACTOR CLASS #####
names <- TRA_SCEs[1]
Zscores <- TRA_SCEs[6]
TRA_Zscore <- cbind(names,Zscores)
write.csv(TRA_Zscore, file = "TRA_Zscore.csv", na = "") 

########## Box-Plot for Z-Score ####################################

jpeg('D:/Input_data/Rel_Categories/Input_data/Rel_Categories/Z_graphs/TRA_Z.jpg',
     quality = 100, bg = "white", res = 200, width = 7, height = 7, units = "in")

names <- (TRA_SCEs[,1])
names <-as.vector(names)

par(mar=c(12, 3, 2, 1) + 0.1)
oldpar <- par(las=2)
barplot(TRA_SCEs$Z_score, width = 0.9, space = 0.35, 
        names.arg = names, border = 1,
        xlab= "", ylab = "Z_score",
        xlim = c(1, (nrow(TRA_Zscore)+5)), ylim = c(-4, 5),
        cex.lab=1, cex.axis=0.8, cex.names = 0.8, col.axis="black")
title(main ="Z-score (Traffic)", font.main= 1.5)
abline(h=0,col=1, lty=1, lwd=1)
abline(h=1,col=4, lty=1, lwd=2)
abline(h=2,col=1, lty=1, lwd=2)
abline(h=3,col=3, lty=1, lwd=2)
abline(h=-1,col=4, lty=1, lwd=2)
abline(h=-2,col=1, lty=1, lwd=2)
abline(h=-3,col=3, lty=1, lwd=2)
par(oldpar)

dev.off()

##################################################################

rm(list=ls(all=TRUE))

################################################################

setwd(D:/Input_data/Rel_Categories/Input_data/Rel_Categories")

SCEs <- list.files(path =D:/Input_data/Rel_Categories/Input_data/Rel_Categories",
                   pattern = "SCEs") #this includes only files with "rel"


for (i in c(1:length(SCEs))) {
  assign(gsub("[.]csv$","",SCEs[i]), read.csv(SCEs[i], header=TRUE, 
                                              as.is = 1))
}

#########################################################################
############################# INDU_SCEs ##################################

median <- median(INDU_SCEs[,2])

INDU_SCEs$ABS <- abs(INDU_SCEs[,2]-median) ####column for absolute difference values
INDU_SCEs$SCE_Iter1 <- INDU_SCEs[,2]  ####column for the first iteration

s <- 1.483*median(INDU_SCEs[,3])
d <- 1.5*s
diff <- median - d
sum  <- median + d

###############FIRST ITERATION
for (i in seq_len(nrow(INDU_SCEs))) {
  if (INDU_SCEs[i,4]< diff) {
    INDU_SCEs[i,4]<- diff 
  } else if (INDU_SCEs[i,4] > sum) {
    INDU_SCEs[i,4] <- sum
  } else 
    INDU_SCEs[i,4]
}

INDU_SCEs$SCE_Iter2 <- INDU_SCEs[,4] ####column for the second iteration
s <- 1.134*sd(INDU_SCEs[,4])
d <- 1.5*s
diff <- mean(INDU_SCEs[,4]) - d
sum  <- mean(INDU_SCEs[,4]) + d

##############SECOND ITERATION
for (i in seq_len(nrow(INDU_SCEs))) {
  if (INDU_SCEs[i,5]< diff) {
    INDU_SCEs[i,5]<- diff 
  } else if (INDU_SCEs[i,5] > sum) {
    INDU_SCEs[i,5] <- sum
  } else 
    INDU_SCEs[i,5]
}

X <- mean(INDU_SCEs[,5])  ###assigned value ALWAYS CALCULATED 
###on the LAST ITERATION

##########Z score calculation for Source Contribution Estimation
#######Z_SCE##########

INDU_SCEs$Z_score <- INDU_SCEs[,5]
INDU_SCEs[,6] <- (INDU_SCEs[,2] - X)/(X*0.5)  ####proficiency at 50%

######## SAVE FILES BEFORE RUNNING SCRIPT WITH OTHER FACTOR CLASS #####
names <- INDU_SCEs[1]
Zscores <- INDU_SCEs[6]
INDU_Zscore <- cbind(names,Zscores)
write.csv(INDU_Zscore, file = "INDU_Zscore.csv", na = "") 

########## Box-Plot for Z-Score ####################################

jpeg('D:/Input_data/Rel_Categories/Input_data/Rel_Categories/Z_graphs/INDU_Z.jpg',
     quality = 100, bg = "white", res = 200, width = 7, height = 7, units = "in")

names <- (INDU_SCEs[,1])
names <-as.vector(names)

par(mar=c(11, 3, 2, 1) + 1.5)
oldpar <- par(las=2)
barplot(INDU_SCEs$Z_score, width = 0.9, space = 0.35, 
        names.arg = names, border = 1,
        xlab= "", ylab = "Z_score",
        xlim = c(1, (nrow(INDU_Zscore)+5.5)), ylim = c(-4, 4),
        cex.lab=1, cex.axis=0.8, cex.names = 0.7, col.axis="black")
title(main ="Z-score (Industry-Cement)", font.main= 1.5)
abline(h=0,col=1, lty=1, lwd=1)
abline(h=1,col=4, lty=1, lwd=2)
abline(h=2,col=1, lty=1, lwd=2)
abline(h=3,col=3, lty=1, lwd=2)
abline(h=-1,col=4, lty=1, lwd=2)
abline(h=-2,col=1, lty=1, lwd=2)
abline(h=-3,col=3, lty=1, lwd=2)
par(oldpar)

dev.off()

##################################################################

rm(list=ls(all=TRUE))

################################################################

setwd(D:/Input_data/Rel_Categories/Input_data/Rel_Categories")

SCEs <- list.files(path =D:/Input_data/Rel_Categories/Input_data/Rel_Categories",
                   pattern = "SCEs") #this includes only files with "rel"


for (i in c(1:length(SCEs))) {
  assign(gsub("[.]csv$","",SCEs[i]), read.csv(SCEs[i], header=TRUE, 
                                              as.is = 1))
}


#########################################################################
############################# SEC_SCEs ##################################

median <- median(SEC_SCEs[,2])

SEC_SCEs$ABS <- abs(SEC_SCEs[,2]-median) ####column for absolute difference values
SEC_SCEs$SCE_Iter1 <- SEC_SCEs[,2]  ####column for the first iteration

s <- 1.483*median(SEC_SCEs[,3])
d <- 1.5*s
diff <- median - d
sum  <- median + d

###############FIRST ITERATION
for (i in seq_len(nrow(SEC_SCEs))) {
  if (SEC_SCEs[i,4]< diff) {
    SEC_SCEs[i,4]<- diff 
  } else if (SEC_SCEs[i,4] > sum) {
    SEC_SCEs[i,4] <- sum
  } else 
    SEC_SCEs[i,4]
}

SEC_SCEs$SCE_Iter2 <- SEC_SCEs[,4] ####column for the second iteration
s <- 1.134*sd(SEC_SCEs[,4])
d <- 1.5*s
diff <- mean(SEC_SCEs[,4]) - d
sum  <- mean(SEC_SCEs[,4]) + d

##############SECOND ITERATION
for (i in seq_len(nrow(SEC_SCEs))) {
  if (SEC_SCEs[i,5]< diff) {
    SEC_SCEs[i,5]<- diff 
  } else if (SEC_SCEs[i,5] > sum) {
    SEC_SCEs[i,5] <- sum
  } else 
    SEC_SCEs[i,5]
}

X <- mean(SEC_SCEs[,5])  ###assigned value ALWAYS CALCULATED 
###on the LAST ITERATION

##########Z score calculation for Source Contribution Estimation
#######Z_SCE##########

SEC_SCEs$Z_score <- SEC_SCEs[,5]
SEC_SCEs[,6] <- (SEC_SCEs[,2] - X)/(X*0.5)  ####proficiency at 50%

######## SAVE FILES BEFORE RUNNING SCRIPT WITH OTHER FACTOR CLASS #####
names <- SEC_SCEs[1]
Zscores <- SEC_SCEs[6]
SEC_Zscore <- cbind(names,Zscores)
write.csv(SEC_Zscore, file = "SEC_Zscore.csv", na = "") 

########## Box-Plot for Z-Score ####################################

jpeg('D:/Input_data/Rel_Categories/Input_data/Rel_Categories/Z_graphs/SEC_Z.jpg',
     quality = 100, bg = "white", res = 200, width = 7, height = 7, units = "in")

names <- (SEC_SCEs[,1])
names <-as.vector(names)

par(mar=c(13, 5, 2, 1) + 0.9)
oldpar <- par(las=2)
barplot(SEC_SCEs$Z_score, width = 0.9, space = 0.35, 
        names.arg = names, border = 1,
        xlab= "", ylab = "Z_score",
        xlim = c(0.5, (nrow(SEC_Zscore)+2)), ylim = c(-4, 4),
        cex.lab=1, cex.axis=0.8, cex.names = 0.8, col.axis="black")
title(main ="Z-score (Secondary Emissions)", font.main= 1.5)
abline(h=0,col=1, lty=1, lwd=1)
abline(h=1,col=4, lty=1, lwd=2)
abline(h=2,col=1, lty=1, lwd=2)
abline(h=3,col=3, lty=1, lwd=2)
abline(h=-1,col=4, lty=1, lwd=2)
abline(h=-2,col=1, lty=1, lwd=2)
abline(h=-3,col=3, lty=1, lwd=2)
par(oldpar)

dev.off()

