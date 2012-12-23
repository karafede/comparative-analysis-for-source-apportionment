Comparative analysis takes also into account uncertainties of data. For this purpose, the
Weighted Difference (WD) is a way to compare data between laboratories considering uncertainties.

The R code: WEIGH_DIFF.R acts as follow:
- loads data
- makes all possible WD between factor profiles
- makes WD between factor and reference source profiles.
- makes boxplots for WD between factor profiles
- makes matplots for WD between factor and reference source profiles.

Threshold limit of acceptability for the Pearson correlation are set at 2.0


setwd("D:/Input_data/Rel_Categories")


REL <- list.files(path ="D:/Input_data/Rel_Categories",
                  pattern = "rel") #this includes only files with "rel"

UNC <- list.files(path ="D:/Input_data/Rel_Categories",
                  pattern = "_U") #this includes only files with "U"

SCEs <- list.files(path ="D:/Input_data/Rel_Categories",
                   pattern = "SCEs") #this includes only files with "SCEs"


for (i in c(1:length(REL))) {
  assign(gsub("[.]csv$","",REL[i]), read.csv(REL[i], header=TRUE, 
                                             as.is = 1)[,-1])
}


for (i in c(1:length(UNC))) {
  assign(gsub("[.]csv$","",UNC[i]), read.csv(UNC[i], header=TRUE, 
                                             as.is = 1)[,-1])
}


for (i in c(1:length(SCEs))) {
  assign(gsub("[.]csv$","",SCEs[i]), read.csv(SCEs[i], header=TRUE, 
                                              as.is = 1))
}


#####LOOP for Weighted Difference (WD) calcualation#####################

############################ BioB ######################################


BioB_rel <- BioB_rel[,-1]   ###raw data
BioB_U <- BioB_U[,-1]   ###raw data uncertainties

BioB_WD <- expand.grid(names(BioB_rel), names(BioB_rel))  ###make all possile permutations
BioB_WD_U <- expand.grid(names(BioB_U), names(BioB_U))  ###make all possile permutations


for (i in seq_len(nrow(BioB_WD))) {
  BioB_WD[i,"EN"] <- mean(abs((BioB_rel[,BioB_WD[i,1]] - BioB_rel[,BioB_WD[i,2]])/sqrt((BioB_U[,BioB_WD_U[i,1]])^2 + (BioB_U[,BioB_WD_U[i,2]])^2))  ,na.rm = TRUE)
}


BioB_WD <- BioB_WD[,3]
BioB_WD <- BioB_WD*is.finite(BioB_WD)
BioB_WD <- matrix(BioB_WD,nrow = ncol(BioB_rel), ncol = ncol(BioB_rel),
                  dimnames = list(names(BioB_rel), names(BioB_rel)))

diag(BioB_WD[,]) <- NaN

BioB_WD=as.data.frame(BioB_WD)
write.csv(BioB_WD, file = "BioB_WD.csv", na = "")

########### The following code can be written for each CLASS ###########
########################################################################

BioB_WD_ff <- subset(BioB_WD[1:(length(BioB_rel)-1),])
BioB_WD_fs <- subset(BioB_WD[(length(BioB_rel)),1:(length(BioB_rel)-1)])

##percentile and median (by columns)
STAT_BioB=apply(BioB_WD_ff, 2, quantile,na.rm = TRUE, probs = c(.25,.5,.75)) #2 is for the columns
#1 is for the lines
STAT_BioB_fs=apply(BioB_WD_fs, 1, quantile,na.rm = TRUE, probs = c(.25,.5,.75))

BioB_Median_WD <- as.matrix(STAT_BioB[2,1:(length(BioB_rel)-1)])
BioB_Median_WD_fs <- as.matrix(STAT_BioB_fs[2,]) ####factro-source

BioB_WD_fs <- t(BioB_WD_fs)
write.csv(BioB_Median_WD, file = "BioB_WD_Med.csv", na = "")
write.csv(BioB_Median_WD_fs, file = "BioB_WD_Med_fs.csv", na = "")

########################## Test 5 & Test 6 (BioB)########################

test_5_BioB <- BioB_Median_WD[,1] < 2
test_6_BioB <- BioB_WD_fs[, 1] < 2
write.csv(test_5_BioB, file = "test_5_BioB.csv", na = "TRUE")
write.csv(test_6_BioB, file = "test_6_BioB.csv", na = "")


################# BioB - box-plot factor vs factor #######################


par(mar=c(12, 5, 3, 1) + 0.3)

oldpar <- par(las=2)
boxplot(as.data.frame(BioB_WD_ff),varwidth=TRUE,outline = FALSE,
        xlab= "", ylab = "Weigthed difference",
        xlim = c(1, (length(BioB_rel)-1.5)), ylim = c(0, 10), 
        boxwex = 0.75,cex.lab=1, cex.axis=0.8, col.axis="black", 
        col="orange")

abline(h=2,col=2, lty=2, lwd=3)
title(main ="Biomass Burning (Weighted Difference by factors)", font.main= 1.5)
par(oldpar)


################## BioB - box-plot factor vs source profile ####################


Names = rownames(BioB_WD)
BioB_WD$fFactor <- factor(rownames(BioB_WD),levels = unique(rownames(BioB_WD)) )

par(mar=c(12, 5, 3, 1) + 0.3)
oldpar <- par(las=2)
matplot(BioB_WD[,(length(BioB_rel))], xlab= "", ylab = "Weigthed difference",
        xlim = c(1, (length(BioB_rel)-1.2)),ylim = c(0, 6),cex.lab=1, cex.axis=1, 
        col.axis="black", pch=c(17,1,2,3,6), xaxt="n", yaxt="n")
grid()

axis(side=1, at = 1:(length(BioB_rel)), labels = BioB_WD[,"fFactor"], cex.axis =0.8)
axis(side=2, at = seq(0,8,2))

abline(h=2,col=2, lty=2, lwd=3)
title(main ="Biomass Burning (WD by factors vs source profiles)", font.main = 1)
par(oldpar)

leg.txt <- c("BioB_REF")
legend (18, 6, leg.txt,col = c(1,2,3,6),pch =c(17,1,2,3,6), bg="white",
        cex = 0.7, text.font = 1 )

###############################################################################################################

repeat the same code for the other categories.....
