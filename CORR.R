Comparative analysis begins with performing Pearson correlation classified data.
Correlation is then carried on all possible pairs of factor profiles found by each laboratory.
Also correlation with a reference source profile is performed.
The R code: CORR.R acts as follow:
- loads data
- makes all possible correlations between factor profiles
- makes correlations between factor and reference source profiles.
- makes boxplots for correlation between factor profiles
- makes matplots for correlation between factor and reference source profiles.

Threshold limit of acceptability for the Pearson correlation are set at 0.6.

#############################################################################################################

setwd(D:/Input_data/Rel_Categories")

REL <- list.files(path =D:/Input_data/Rel_Categories",
                  pattern = "rel") #this includes only files with "rel"

UNC <- list.files(path =D:/Input_data/Rel_Categories",
                  pattern = "_U") #this includes only files with "UNC"

SCEs <- list.files(path =D:/Input_data/Rel_Categories",
                   pattern = "SCEs") #this includes only files with "SCEs"


nrows <- 39   ##### number of lines in the input data (chemical species)
for (i in c(1:length(REL))) {
  assign(gsub("[.]csv$","",REL[i]), read.csv(REL[i], header=TRUE, 
                                             as.is = 1)[-nrows,-1])
}


for (i in c(1:length(UNC))) {
  assign(gsub("[.]csv$","",UNC[i]), read.csv(UNC[i], header=TRUE, 
                                             as.is = 1)[-nrows,-1])
}


for (i in c(1:length(SCEs))) {
  assign(gsub("[.]csv$","",SCEs[i]), read.csv(SCEs[i], header=TRUE, 
                                              as.is = 1))
}

library("matrixStats")

BioB_Pear=cor(BioB_rel[,2:length(BioB_rel)], use="pairwise.complete.obs")
BioB_Pear=as.data.frame(BioB_Pear)
BioB_Pear_LOG=cor(BioB_rel_LOG[,2:length(BioB_rel)], use="pairwise.complete.obs")
BioB_Pear_LOG=as.data.frame(BioB_Pear_LOG)
write.csv(BioB_Pear, file = "BioB_Pear.csv", na = "") 
write.csv(BioB_Pear_LOG, file = "BioB_Pear_LOG.csv", na = "")

SO4_Pear=cor(SO4_rel[,2:length(SO4_rel)], use="pairwise.complete.obs")
SO4_Pear=cor(SO4_rel[,2:24], use="pairwise.complete.obs")
SO4_Pear=as.data.frame(SO4_Pear)
SO4_Pear_LOG=cor(SO4_rel_LOG[,2:length(SO4_rel)], use="pairwise.complete.obs")
SO4_Pear_LOG=as.data.frame(SO4_Pear_LOG)
write.csv(SO4_Pear, file = "SO4_Pear.csv", na = "") 
write.csv(SO4_Pear_LOG, file = "SO4_Pear_LOG.csv", na = "")

NO3_Pear=cor(NO3_rel[,2:length(NO3_rel)], use="pairwise.complete.obs")
NO3_Pear=as.data.frame(NO3_Pear)
NO3_Pear_LOG=cor(NO3_rel_LOG[,2:length(NO3_rel)], use="pairwise.complete.obs")
NO3_Pear_LOG=as.data.frame(NO3_Pear_LOG)
write.csv(NO3_Pear, file = "NO3_Pear.csv", na = "") 
write.csv(NO3_Pear_LOG, file = "NO3_Pear_LOG.csv", na = "")

DUST_Pear=cor(DUST_rel[,2:length(DUST_rel)], use="pairwise.complete.obs")
DUST_Pear=as.data.frame(DUST_Pear)
DUST_Pear_LOG=cor(DUST_rel_LOG[,2:length(DUST_rel)], use="pairwise.complete.obs")
DUST_Pear_LOG=as.data.frame(DUST_Pear_LOG)
write.csv(DUST_Pear, file = "DUST_Pear.csv", na = "") 
write.csv(DUST_Pear_LOG, file = "DUST_Pear_LOG.csv", na = "")

ROAD_Pear=cor(ROAD_rel[,2:length(ROAD_rel)], use="pairwise.complete.obs")
ROAD_Pear=as.data.frame(ROAD_Pear)
ROAD_Pear_LOG=cor(ROAD_rel_LOG[,2:length(ROAD_rel)], use="pairwise.complete.obs")
ROAD_Pear_LOG=as.data.frame(ROAD_Pear_LOG)
write.csv(ROAD_Pear, file = "ROAD_Pear.csv", na = "") 
write.csv(ROAD_Pear_LOG, file = "ROAD_Pear_LOG.csv", na = "")

SALT_Pear=cor(SALT_rel[,2:length(SALT_rel)], use="pairwise.complete.obs")
SALT_Pear=as.data.frame(SALT_Pear)
SALT_Pear_LOG=cor(SALT_rel_LOG[,2:length(SALT_rel)], use="pairwise.complete.obs")
SALT_Pear_LOG=as.data.frame(SALT_Pear_LOG)
write.csv(SALT_Pear, file = "SALT_Pear.csv", na = "") 
write.csv(SALT_Pear_LOG, file = "SALT_Pear_LOG.csv", na = "")

TRA_Pear=cor(TRA_rel[,2:length(TRA_rel)], use="pairwise.complete.obs")
TRA_Pear=as.data.frame(TRA_Pear)
TRA_Pear_LOG=cor(TRA_rel_LOG[,2:length(TRA_rel)], use="pairwise.complete.obs")
TRA_Pear_LOG=as.data.frame(TRA_Pear_LOG)
write.csv(TRA_Pear, file = "TRA_Pear.csv", na = "") 
write.csv(TRA_Pear_LOG, file = "TRA_Pear_LOG.csv", na = "")

INDU_Pear=cor(INDU_rel[,2:length(INDU_rel)], use="pairwise.complete.obs")
INDU_Pear=as.data.frame(INDU_Pear)
INDU_Pear_LOG=cor(INDU_rel_LOG[,2:length(INDU_rel)], use="pairwise.complete.obs")
INDU_Pear_LOG=as.data.frame(INDU_Pear_LOG)
write.csv(INDU_Pear, file = "INDU_Pear.csv", na = "") 
write.csv(INDU_Pear_LOG, file = "INDU_Pear_LOG.csv", na = "")

SEC_Pear=cor(SEC_rel[,2:length(SEC_rel)], use="pairwise.complete.obs")
SEC_Pear=as.data.frame(SEC_Pear)
SEC_Pear_LOG=cor(SEC_rel_LOG[,2:length(SEC_rel)], use="pairwise.complete.obs")
SEC_Pear_LOG=as.data.frame(SEC_Pear_LOG)
write.csv(SEC_Pear, file = "SEC_Pear.csv", na = "") 
write.csv(SEC_Pear_LOG, file = "SEC_Pear_LOG.csv", na = "")


########################################################################

########### The following code can be written for each CLASS ###########

############################ BioB ######################################

BioB_ff <- subset(BioB_Pear[1:(length(BioB_rel)-2),])   #########factors
BioB_fs <- subset(BioB_Pear[(length(BioB_rel)-1),1:(length(BioB_rel)-2)])  #########sources
BioB_ff_LOG <- subset(BioB_Pear_LOG[1:(length(BioB_rel)-2),]) #########factors
BioB_fs_LOG <- subset(BioB_Pear_LOG[(length(BioB_rel)-1),1:(length(BioB_rel)-2)]) #########sources

##percentile and median (by columns)
STAT_BioB <- apply(BioB_ff, 2, quantile, probs = c(.25,.5,.75)) #2 is for the columns
#1 is for the lines
STAT_BioB_fs <- apply(BioB_fs, 1, quantile, probs = c(.25,.5,.75))

STAT_BioB_LOG <- apply(BioB_ff_LOG, 2, quantile, probs = c(.25,.5,.75))
STAT_BioB_LOG_fs <- apply(BioB_fs_LOG, 1, quantile, probs = c(.25,.5,.75))

BioB_Median <- as.matrix(STAT_BioB[2,1:(length(BioB_rel)-2)])
BioB_Median_LOG <- as.matrix(STAT_BioB_LOG[2,1:(length(BioB_rel)-2)])
BioB_Median_fs <- as.matrix(STAT_BioB_fs[2,])
BioB_Median_LOG_fs <- as.matrix(STAT_BioB_LOG_fs[2,])


BioB_fs <- t(BioB_fs)
BioB_fs_LOG <- t(BioB_fs_LOG)

write.csv(BioB_Median, file = "BioB_Median.csv", na = "")
write.csv(BioB_Median_LOG, file = "BioB_Median_LOG.csv", na = "")
write.csv(BioB_Median_fs, file = "BioB_Median_fs.csv", na = "")
write.csv(BioB_Median_LOG_fs, file = "BioB_Median_LOG_fs.csv", na = "")


########################## Test 1 & Test 2 (BioB) #######################

test_1_BioB <- BioB_Median[,1] > 0.6  ####Threshold limit of acceptability for the Pearson correlation are set at 0.6.
test_2_BioB <- BioB_fs[,1] > 0.6
write.csv(test_1_BioB, file = "test_1_BioB.csv", na = "")
write.csv(test_2_BioB, file = "test_2_BioB.csv", na = "")


############# BioB - box-plot factor vs factor, raw data ###############


par(mar=c(11.5, 5, 2, 1) + 0.3)

oldpar <- par(las=2)
boxplot(as.data.frame(BioB_ff),varwidth=TRUE, outline = FALSE,
        xlab= "", ylab = "Pearson_Corr-raw",
        xlim = c(1, (length(BioB_rel)-2.5)), ylim = c(-0.6, 1), 
        boxwex = 0.75,cex.lab=1, cex.axis=0.8, col.axis="black", 
        col="cyan")

abline(h=0.6,col=2, lty=2, lwd=3)
title(main ="Biomass Burning: factor vs factor", font.main= 1.5)
par(oldpar)



############# BioB - box-plot factor vs source profile, raw data #######


Names = rownames(BioB_Pear)
str(Names)
BioB_Pear$fFactor <- factor(rownames(BioB_Pear),levels = unique(rownames(BioB_Pear)))
str(BioB_Pear)


par(mar=c(11, 5, 2, 1) + 0.3)
oldpar <- par(las=2)
matplot(BioB_Pear[,(length(BioB_rel)-1)], xlab= "", ylab = "Pearson_Corr-raw",
        xlim = c(1, (length(BioB_rel)-2.5)), ylim = c(-0.6, 1), cex.lab=1, cex.axis=0.8, 
        col.axis="black", pch=c(19,16,1,2,3,6),
        xaxt="n", yaxt="n")
grid()

axis(side=1, at = 1:(length(BioB_rel)-1), labels = BioB_Pear[,"fFactor"], cex.axis =0.7)
axis(side=2, at = seq(-1,1,0.4))

abline(h=0.6,col=2, lty=2, lwd=3)
title(main ="Biomass Burning: factor vs source profiles", font.main= 1.5)
par(oldpar)

leg.txt <- c("BioB_REF")
legend (22, -0.1, leg.txt,col = c(1,2,3,6),pch =c(19,1,2,3,6), bg="white",
        cex = 0.7, text.font = 1 )



########################################################################

########################## Test 3 & Test 4 (BioB)#######################

test_3_BioB <- BioB_Median_LOG[,1] > 0.6
test_4_BioB <- BioB_fs_LOG[,1] > 0.6
write.csv(test_3_BioB, file = "test_3_BioB.csv", na = "")
write.csv(test_4_BioB, file = "test_4_BioB.csv", na = "")

#############  BioB - box-plot factor vs factor, LOG data ###############


par(mar=c(11.5, 5, 2, 1) + 0.3)

oldpar <- par(las=2)
boxplot(as.data.frame(BioB_ff_LOG),varwidth=TRUE,outline = FALSE,
        xlab= "", ylab = "Pearson_Corr-LOG",
        xlim = c(1, (length(BioB_rel)-2.5)), ylim = c(-0.6, 1), 
        boxwex = 0.75,cex.lab=1, cex.axis=0.8, col.axis="black", 
        col="cyan")

abline(h=0.6,col=2, lty=2, lwd=3)
title(main ="Biomass Burning: factor vs factor", font.main= 1.5)
par(oldpar)



############# BioB - box-plot factor vs source profile, LOG data ################


Names = rownames(BioB_Pear_LOG)
str(Names)
BioB_Pear$fFactor <- factor(rownames(BioB_Pear),levels = unique(rownames(BioB_Pear_LOG)) )
str(BioB_Pear_LOG)


par(mar=c(11, 5, 3, 1) + 0.3)
oldpar <- par(las=2)
matplot(BioB_Pear_LOG[,(length(BioB_rel)-1)], xlab= "", ylab = "Pearson_Corr-LOG",
        xlim = c(1, (length(BioB_rel)-2.5)),ylim = c(-0.6, 1), 
        cex.lab=1, cex.axis=0.8, col.axis="black", pch=c(19,1,2,3,6),
        xaxt="n", yaxt="n")
grid()

axis(side=1, at = 1:(length(BioB_rel)-1), labels = BioB_Pear[,"fFactor"], cex.axis =0.7)
axis(side=2, at = seq(-1,1,0.4))

abline(h=0.6,col=2, lty=2, lwd=3)
title(main ="Biomass Burning: factor vs source profiles", font.main= 1.5)
par(oldpar)

leg.txt <- c("BioB_REF")
legend (22, -0.1, leg.txt,col = c(1,2,3,6),pch =c(19,1,2,3,6), bg="white",
        cex = 0.7, text.font = 1 )

#########################################################################################


repeat the same code for the other categories.....
