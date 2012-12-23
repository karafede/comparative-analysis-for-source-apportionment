Perform overall statistical analysis of Pearson Correlation: 


setwd("D:/Input_data/Rel_Categories/Input_data/Rel_Categories")

MEDIANS <- list.files(path ="D:/Input_data/Rel_Categories/Input_data/Rel_Categories",
                      pattern = "Median") #this includes only files with "Median"

for (i in c(1:length(MEDIANS))) {
  assign(gsub("[.]csv$","",MEDIANS[i]), read.csv(MEDIANS[i], header=TRUE, 
                                                 as.is = 1))
}

############# Statistics by FACTORS (PEARSON) ##################################

TOT_PEARS <- rbind(BioB_Median, SO4_Median, NO3_Median, DUST_Median,
                   ROAD_Median, SALT_Median, TRA_Median, 
                   INDU_Median, SEC_Median)

TOT_PEARS_LOG <- rbind(BioB_Median_LOG, SO4_Median_LOG, NO3_Median_LOG, DUST_Median_LOG,
                       ROAD_Median_LOG, SALT_Median_LOG, TRA_Median_LOG, 
                       INDU_Median_LOG, SEC_Median_LOG)

length(row.names(BioB_Median)) 
length(na.omit(row.names(BioB_Median)))
length(row.names(SO4_Median))
length(row.names(NO3_Median))
length(row.names(DUST_Median))
length(row.names(ROAD_Median))
length(row.names(SALT_Median))
length(row.names(TRA_Median))
length(row.names(INDU_Median))
length(row.names(SEC_Median))

BioB <- as.matrix(array("BioB", c(length(row.names(BioB_Median)),1)))
SO4 <- as.matrix(array("SO4", c(length(row.names(SO4_Median)),1)))
NO3 <- as.matrix(array("NO3", c(length(row.names(NO3_Median)),1)))
DUST <- as.matrix(array("DUST", c(length(row.names(DUST_Median)),1)))
ROAD <- as.matrix(array("ROAD", c(length(row.names(ROAD_Median)),1)))
SALT <- as.matrix(array("SALT", c(length(row.names(SALT_Median)),1)))
TRA <- as.matrix(array("TRA", c(length(row.names(TRA_Median)),1)))
INDU <- as.matrix(array("INDU", c(length(row.names(INDU_Median)),1)))
SEC <- as.matrix(array("SEC", c(length(row.names(SEC_Median)),1)))

FACTORS <- rbind(BioB, SO4, NO3, DUST, ROAD, SALT, TRA, INDU,SEC)


############## Box-plot Pearson by Factors (ROW data) #################

jpeg('D:/Input_data/Rel_Categories/Input_data/Rel_Categories/STAT_graphs/Factors_Pear.jpg',
     quality = 100, bg = "white", res = 200, width = 7, height = 7, units = "in")

par(mar=c(8, 5, 3, 2) + 0.3)
oldpar <- par(las=2)
PEAR <- boxplot(TOT_PEARS[,2] ~ FACTORS , varwidth=TRUE,outline = FALSE,
                data = TOT_PEARS, 
                xlab= "", ylab = "Pearson_Corr-Raw (median)",
                xlim = c(0.5, 9.5), ylim = c(0, 1.1))

text(x = 1:length(PEAR$n), y = PEAR$stats[5, ] + 0.07, PEAR$n, 
     font = 4, cex = 0.8)

abline(h=0.6,col=2, lty=2, lwd=3)
title(main ="Pearson Correlation by factors (Raw data)", font.main= 1.5)
par(oldpar)

dev.off()

############## Box-plot Pearson by Factors (LOG data) #################

jpeg('D:/Input_data/Rel_Categories/Input_data/Rel_Categories/STAT_graphs/Factors_Pear_log.jpg',
     quality = 100, bg = "white", res = 200, width = 7, height = 7, units = "in")


oldpar <- par(las=2)
par(mar=c(8, 5, 3, 2) + 0.3)
PEARLOG <- boxplot(TOT_PEARS_LOG[,2] ~ FACTORS , varwidth=TRUE,outline = FALSE,
                   data = TOT_PEARS_LOG, 
                   xlab= "", ylab = "Pearson_Corr-LOG (median)",
                   xlim = c(0.5, 9.5), ylim = c(0, 1.1))

text(x = 1:length(PEARLOG$n), y = PEARLOG$stats[5, ] + 0.07, PEARLOG$n, 
     font = 4, cex = 0.8)

abline(h=0.6,col=2, lty=2, lwd=3)
title(main ="Pearson Correlation by factors (Log data)", font.main= 1.5)
par(oldpar)

dev.off()

######################################################################
######### Statistics by PARTICIPANTS (CODES) - ROW DATA ##############

TOT_PEARS_transp <- t(TOT_PEARS)
write.csv(TOT_PEARS_transp, file = "TOT_PEARS_transp.csv", na = "")
TOT_PEARS_transp <- read.csv("TOT_PEARS_transp.csv", header = TRUE, skip = 1)


######################################################################
Q1_PEAR <- t(subset(TOT_PEARS_transp,
                    select=grep("Q1", colnames(TOT_PEARS_transp))))
Q1 <- as.matrix(array("Q1", c(length(Q1_PEAR),1)))
######################################################################
M1_PEAR <- t(subset(TOT_PEARS_transp,
                    select=grep("M1", colnames(TOT_PEARS_transp))))
M1 <- as.matrix(array("M1", c(length(M1_PEAR),1)))
######################################################################
F1_PEAR <- t(subset(TOT_PEARS_transp,
                    select=grep("F1", colnames(TOT_PEARS_transp))))
F1 <- as.matrix(array("F1", c(length(F1_PEAR),1)))
######################################################################
C1_PEAR <- t(subset(TOT_PEARS_transp,
                    select=grep("C1", colnames(TOT_PEARS_transp))))
C1 <- as.matrix(array("C1", c(length(C1_PEAR),1)))
######################################################################
J1_PEAR <- t(subset(TOT_PEARS_transp,
                    select=grep("J1", colnames(TOT_PEARS_transp))))
J1 <- as.matrix(array("J1", c(length(J1_PEAR),1)))
######################################################################
L1_PEAR <- t(subset(TOT_PEARS_transp,
                    select=grep("L1", colnames(TOT_PEARS_transp))))
L1 <- as.matrix(array("L1", c(length(L1_PEAR),1)))
######################################################################
S1_PEAR <- t(subset(TOT_PEARS_transp,
                    select=grep("S1", colnames(TOT_PEARS_transp))))
S1 <- as.matrix(array("S1", c(length(S1_PEAR),1)))
######################################################################
S2_PEAR <- t(subset(TOT_PEARS_transp,
                    select=grep("S2", colnames(TOT_PEARS_transp))))
S2 <- as.matrix(array("S2", c(length(S2_PEAR),1)))
######################################################################
A1_PEAR <- t(subset(TOT_PEARS_transp,
                    select=grep("A1", colnames(TOT_PEARS_transp))))
A1 <- as.matrix(array("A1", c(length(A1_PEAR),1)))
######################################################################
B1_PEAR <- t(subset(TOT_PEARS_transp,
                    select=grep("B1", colnames(TOT_PEARS_transp))))
B1 <- as.matrix(array("B1", c(length(B1_PEAR),1)))
######################################################################
R1_PEAR <- t(subset(TOT_PEARS_transp,
                    select=grep("R1", colnames(TOT_PEARS_transp))))
R1 <- as.matrix(array("R1", c(length(R1_PEAR),1)))
######################################################################
T1_PEAR <- t(subset(TOT_PEARS_transp,
                    select=grep("T1", colnames(TOT_PEARS_transp))))
T1 <- as.matrix(array("T1", c(length(T1_PEAR),1)))
######################################################################
P1_PEAR <- t(subset(TOT_PEARS_transp,
                    select=grep("P1", colnames(TOT_PEARS_transp))))
P1 <- as.matrix(array("P1", c(length(P1_PEAR),1)))
######################################################################
O1_PEAR <- t(subset(TOT_PEARS_transp,
                    select=grep("O1", colnames(TOT_PEARS_transp))))
O1 <- as.matrix(array("O1", c(length(O1_PEAR),1)))
######################################################################
N1_PEAR <- t(subset(TOT_PEARS_transp,
                    select=grep("N1", colnames(TOT_PEARS_transp))))
N1 <- as.matrix(array("N1", c(length(N1_PEAR),1)))
######################################################################
X1_PEAR <- t(subset(TOT_PEARS_transp,
                    select=grep("X1", colnames(TOT_PEARS_transp))))
X1 <- as.matrix(array("X1", c(length(X1_PEAR),1)))
######################################################################
Y1_PEAR <- t(subset(TOT_PEARS_transp,
                    select=grep("Y1", colnames(TOT_PEARS_transp))))
Y1 <- as.matrix(array("Y1", c(length(Y1_PEAR),1)))
######################################################################
K1_PEAR <- t(subset(TOT_PEARS_transp,
                    select=grep("K1", colnames(TOT_PEARS_transp))))
K1 <- as.matrix(array("K1", c(length(K1_PEAR),1)))
######################################################################
H1_PEAR <- t(subset(TOT_PEARS_transp,
                    select=grep("H1", colnames(TOT_PEARS_transp))))
H1 <- as.matrix(array("H1", c(length(H1_PEAR),1)))
######################################################################
D1_PEAR <- t(subset(TOT_PEARS_transp,
                    select=grep("D1", colnames(TOT_PEARS_transp))))
D1 <- as.matrix(array("D1", c(length(D1_PEAR),1)))
######################################################################
H3_PEAR <- t(subset(TOT_PEARS_transp,
                    select=grep("H3", colnames(TOT_PEARS_transp))))
H3 <- as.matrix(array("H3", c(length(H3_PEAR),1)))
######################################################################
J2_PEAR <- t(subset(TOT_PEARS_transp,
                    select=grep("J2", colnames(TOT_PEARS_transp))))
J2 <- as.matrix(array("J2", c(length(J2_PEAR),1)))
#####################################################################

CODES <- rbind(Q1, M1, F1, C1, J1, L1, S1, S2, A1, B1, R1,T1, P1, O1, 
               N1, X1, Y1, K1,H1, D1, H3, J2)
PEAR_CODES <- rbind(Q1_PEAR, M1_PEAR, F1_PEAR, C1_PEAR,J1_PEAR,
                    L1_PEAR,S1_PEAR, S2_PEAR,A1_PEAR, B1_PEAR,
                    R1_PEAR, T1_PEAR,P1_PEAR, O1_PEAR, N1_PEAR,
                    X1_PEAR, Y1_PEAR, K1_PEAR,
                    H1_PEAR,D1_PEAR, H3_PEAR,J2_PEAR)


MODELS <- CODES
MODELS <- data.frame(MODELS)


MODELS <- data.frame(sapply(MODELS, gsub, pattern="Q1", replacement="PMF3.0"))
MODELS <- data.frame(sapply(MODELS, gsub, pattern="M1", replacement="PMF2"))
MODELS <- data.frame(sapply(MODELS, gsub, pattern="F1", replacement="PMF3.0"))
MODELS <- data.frame(sapply(MODELS, gsub, pattern="C1", replacement="PMF3.0"))
MODELS <- data.frame(sapply(MODELS, gsub, pattern="J1", replacement="PMF3.0"))
MODELS <- data.frame(sapply(MODELS, gsub, pattern="L1", replacement="PMF2"))
MODELS <- data.frame(sapply(MODELS, gsub, pattern="S1", replacement="CMB8.2"))
MODELS <- data.frame(sapply(MODELS, gsub, pattern="S2", replacement="CMB8.2"))
MODELS <- data.frame(sapply(MODELS, gsub, pattern="A1", replacement="PMF3.0"))
MODELS <- data.frame(sapply(MODELS, gsub, pattern="B1", replacement="ME2"))
MODELS <- data.frame(sapply(MODELS, gsub, pattern="R1", replacement="PMF5.0"))
MODELS <- data.frame(sapply(MODELS, gsub, pattern="T1", replacement="CMB_Robtic"))
MODELS <- data.frame(sapply(MODELS, gsub, pattern="P1", replacement="PMF4.1"))
MODELS <- data.frame(sapply(MODELS, gsub, pattern="O1", replacement="PMF3.0"))
MODELS <- data.frame(sapply(MODELS, gsub, pattern="N1", replacement="CMB8.2"))
MODELS <- data.frame(sapply(MODELS, gsub, pattern="X1", replacement="PMF3.0"))
MODELS <- data.frame(sapply(MODELS, gsub, pattern="Y1", replacement="PMF3.0"))
MODELS <- data.frame(sapply(MODELS, gsub, pattern="K1", replacement="COPREM"))
MODELS <- data.frame(sapply(MODELS, gsub, pattern="H1", replacement="PMF2"))
MODELS <- data.frame(sapply(MODELS, gsub, pattern="D1", replacement="PMF3.0"))
MODELS <- data.frame(sapply(MODELS, gsub, pattern="H3", replacement="PMF3.0"))
MODELS <- data.frame(sapply(MODELS, gsub, pattern="J2", replacement="CMB8.2"))

MODELS <- as.matrix(MODELS)

############## Box-plot Pearson by participants (RAW-data) ##############

jpeg('D:/Input_data/Rel_Categories/Input_data/Rel_Categories/STAT_graphs/Partic_Pear.jpg',
     quality = 100, bg = "white", res = 200, width = 7, height = 7, units = "in")

par(mar=c(8, 5, 3, 2) + 0.3)
oldpar <- par(las=2)
participants <-  boxplot(PEAR_CODES ~ CODES , varwidth=TRUE,outline = FALSE,
                         data = PEAR_CODES, 
                         xlab= "", ylab = "Pearson_Corr-Participants (median-raw)",
                         xlim = c(1, 22), ylim = c(0, 1.1))

text(x = 1:length(participants$n), y = participants$stats[5, ] + 0.07,
     participants$n, font = 4, cex = 0.8)

abline(h=0.6,col=2, lty=2, lwd=3)
title(main ="Pearson Correlation by participants (Raw data)", font.main= 1.5)
par(oldpar)

dev.off()

############## Box-plot Pearson by MODEL TYPE (RAW-data) ##############

jpeg('D:/Input_data/Rel_Categories/Input_data/Rel_Categories/STAT_graphs/Models_Pear.jpg',
     quality = 100, bg = "white", res = 200, width = 7, height = 7, units = "in")


par(mar=c(8, 5, 3, 2) + 0.3)
oldpar <- par(las=2)
models <- boxplot(PEAR_CODES ~ MODELS , varwidth=TRUE,outline = FALSE,
                  data = PEAR_CODES, 
                  xlab= "", ylab = "Pearson_Corr-Models (median-raw)",
                  xlim = c(0.7, 8.2), ylim = c(0, 1.1))

text(x = 1:length(models$n), y = models$stats[5, ] + 0.07,
     models$n, font = 4, cex = 0.8)

abline(h=0.6,col=2, lty=2, lwd=3)
title(main ="Pearson_Corr-Models (median-raw)", font.main= 1.5)
par(oldpar)

dev.off()


######################################################################
######### Statistics by PARTICIPANTS (CODES) - LOG-DATA ##############

TOT_PEARS_LOG_transp <- t(TOT_PEARS_LOG)
write.csv(TOT_PEARS_LOG_transp, file = "TOT_PEARS_LOG_transp.csv", na = "")
TOT_PEARS_LOG_transp <- read.csv("TOT_PEARS_LOG_transp.csv", header = TRUE, skip = 1)


######################################################################
Q1_PEAR <- t(subset(TOT_PEARS_LOG_transp,
                    select=grep("Q1", colnames(TOT_PEARS_LOG_transp))))
Q1 <- as.matrix(array("Q1", c(length(Q1_PEAR),1)))
######################################################################
M1_PEAR <- t(subset(TOT_PEARS_LOG_transp,
                    select=grep("M1", colnames(TOT_PEARS_LOG_transp))))
M1 <- as.matrix(array("M1", c(length(M1_PEAR),1)))
######################################################################
F1_PEAR <- t(subset(TOT_PEARS_LOG_transp,
                    select=grep("F1", colnames(TOT_PEARS_LOG_transp))))
F1 <- as.matrix(array("F1", c(length(F1_PEAR),1)))
######################################################################
C1_PEAR <- t(subset(TOT_PEARS_LOG_transp,
                    select=grep("C1", colnames(TOT_PEARS_LOG_transp))))
C1 <- as.matrix(array("C1", c(length(C1_PEAR),1)))
######################################################################
J1_PEAR <- t(subset(TOT_PEARS_LOG_transp,
                    select=grep("J1", colnames(TOT_PEARS_LOG_transp))))
J1 <- as.matrix(array("J1", c(length(J1_PEAR),1)))
######################################################################
L1_PEAR <- t(subset(TOT_PEARS_LOG_transp,
                    select=grep("L1", colnames(TOT_PEARS_LOG_transp))))
L1 <- as.matrix(array("L1", c(length(L1_PEAR),1)))
######################################################################
S1_PEAR <- t(subset(TOT_PEARS_LOG_transp,
                    select=grep("S1", colnames(TOT_PEARS_LOG_transp))))
S1 <- as.matrix(array("S1", c(length(S1_PEAR),1)))
######################################################################
S2_PEAR <- t(subset(TOT_PEARS_LOG_transp,
                    select=grep("S2", colnames(TOT_PEARS_LOG_transp))))
S2 <- as.matrix(array("S2", c(length(S2_PEAR),1)))
######################################################################
A1_PEAR <- t(subset(TOT_PEARS_LOG_transp,
                    select=grep("A1", colnames(TOT_PEARS_LOG_transp))))
A1 <- as.matrix(array("A1", c(length(A1_PEAR),1)))
######################################################################
B1_PEAR <- t(subset(TOT_PEARS_LOG_transp,
                    select=grep("B1", colnames(TOT_PEARS_LOG_transp))))
B1 <- as.matrix(array("B1", c(length(B1_PEAR),1)))
######################################################################
R1_PEAR <- t(subset(TOT_PEARS_LOG_transp,
                    select=grep("R1", colnames(TOT_PEARS_LOG_transp))))
R1 <- as.matrix(array("R1", c(length(R1_PEAR),1)))
######################################################################
T1_PEAR <- t(subset(TOT_PEARS_LOG_transp,
                    select=grep("T1", colnames(TOT_PEARS_LOG_transp))))
T1 <- as.matrix(array("T1", c(length(T1_PEAR),1)))
######################################################################
P1_PEAR <- t(subset(TOT_PEARS_LOG_transp,
                    select=grep("P1", colnames(TOT_PEARS_LOG_transp))))
P1 <- as.matrix(array("P1", c(length(P1_PEAR),1)))
######################################################################
O1_PEAR <- t(subset(TOT_PEARS_LOG_transp,
                    select=grep("O1", colnames(TOT_PEARS_LOG_transp))))
O1 <- as.matrix(array("O1", c(length(O1_PEAR),1)))
######################################################################
N1_PEAR <- t(subset(TOT_PEARS_LOG_transp,
                    select=grep("N1", colnames(TOT_PEARS_LOG_transp))))
N1 <- as.matrix(array("N1", c(length(N1_PEAR),1)))
######################################################################
X1_PEAR <- t(subset(TOT_PEARS_LOG_transp,
                    select=grep("X1", colnames(TOT_PEARS_LOG_transp))))
X1 <- as.matrix(array("X1", c(length(X1_PEAR),1)))
######################################################################
Y1_PEAR <- t(subset(TOT_PEARS_LOG_transp,
                    select=grep("Y1", colnames(TOT_PEARS_LOG_transp))))
Y1 <- as.matrix(array("Y1", c(length(Y1_PEAR),1)))
######################################################################
K1_PEAR <- t(subset(TOT_PEARS_LOG_transp,
                    select=grep("K1", colnames(TOT_PEARS_LOG_transp))))
K1 <- as.matrix(array("K1", c(length(K1_PEAR),1)))
######################################################################
H1_PEAR <- t(subset(TOT_PEARS_LOG_transp,
                    select=grep("H1", colnames(TOT_PEARS_LOG_transp))))
H1 <- as.matrix(array("H1", c(length(H1_PEAR),1)))
######################################################################
D1_PEAR <- t(subset(TOT_PEARS_LOG_transp,
                    select=grep("D1", colnames(TOT_PEARS_LOG_transp))))
D1 <- as.matrix(array("D1", c(length(D1_PEAR),1)))
######################################################################
H3_PEAR <- t(subset(TOT_PEARS_LOG_transp,
                    select=grep("H3", colnames(TOT_PEARS_LOG_transp))))
H3 <- as.matrix(array("H3", c(length(H3_PEAR),1)))
######################################################################
J2_PEAR <- t(subset(TOT_PEARS_LOG_transp,
                    select=grep("J2", colnames(TOT_PEARS_LOG_transp))))
J2 <- as.matrix(array("J2", c(length(J2_PEAR),1)))
#####################################################################

CODES <- rbind(Q1, M1, F1, C1, J1, L1, S1, S2, A1, B1, R1,T1, P1, O1, 
               N1, X1, Y1, K1,H1, D1, H3, J2)
PEAR_CODES <- rbind(Q1_PEAR, M1_PEAR, F1_PEAR, C1_PEAR,J1_PEAR,
                    L1_PEAR,S1_PEAR, S2_PEAR,A1_PEAR, B1_PEAR,
                    R1_PEAR, T1_PEAR,P1_PEAR, O1_PEAR, N1_PEAR,
                    X1_PEAR, Y1_PEAR, K1_PEAR,
                    H1_PEAR,D1_PEAR, H3_PEAR,J2_PEAR)


MODELS <- CODES
MODELS <- data.frame(MODELS)


MODELS <- data.frame(sapply(MODELS, gsub, pattern="Q1", replacement="PMF3.0"))
MODELS <- data.frame(sapply(MODELS, gsub, pattern="M1", replacement="PMF2"))
MODELS <- data.frame(sapply(MODELS, gsub, pattern="F1", replacement="PMF3.0"))
MODELS <- data.frame(sapply(MODELS, gsub, pattern="C1", replacement="PMF3.0"))
MODELS <- data.frame(sapply(MODELS, gsub, pattern="J1", replacement="PMF3.0"))
MODELS <- data.frame(sapply(MODELS, gsub, pattern="L1", replacement="PMF2"))
MODELS <- data.frame(sapply(MODELS, gsub, pattern="S1", replacement="CMB8.2"))
MODELS <- data.frame(sapply(MODELS, gsub, pattern="S2", replacement="CMB8.2"))
MODELS <- data.frame(sapply(MODELS, gsub, pattern="A1", replacement="PMF3.0"))
MODELS <- data.frame(sapply(MODELS, gsub, pattern="B1", replacement="ME2"))
MODELS <- data.frame(sapply(MODELS, gsub, pattern="R1", replacement="PMF5.0"))
MODELS <- data.frame(sapply(MODELS, gsub, pattern="T1", replacement="CMB_Robtic"))
MODELS <- data.frame(sapply(MODELS, gsub, pattern="P1", replacement="PMF4.1"))
MODELS <- data.frame(sapply(MODELS, gsub, pattern="O1", replacement="PMF3.0"))
MODELS <- data.frame(sapply(MODELS, gsub, pattern="N1", replacement="CMB8.2"))
MODELS <- data.frame(sapply(MODELS, gsub, pattern="X1", replacement="PMF3.0"))
MODELS <- data.frame(sapply(MODELS, gsub, pattern="Y1", replacement="PMF3.0"))
MODELS <- data.frame(sapply(MODELS, gsub, pattern="K1", replacement="COPREM"))
MODELS <- data.frame(sapply(MODELS, gsub, pattern="H1", replacement="PMF2"))
MODELS <- data.frame(sapply(MODELS, gsub, pattern="D1", replacement="PMF3.0"))
MODELS <- data.frame(sapply(MODELS, gsub, pattern="H3", replacement="PMF3.0"))
MODELS <- data.frame(sapply(MODELS, gsub, pattern="J2", replacement="CMB8.2"))

MODELS <- as.matrix(MODELS)

############## Box-plot Pearson by participants (RAW-data) ##############

jpeg('D:/Input_data/Rel_Categories/Input_data/Rel_Categories/STAT_graphs/Partic_Pear_log.jpg',
     quality = 100, bg = "white", res = 200, width = 7, height = 7, units = "in")

par(mar=c(8, 5, 3, 2) + 0.3)
oldpar <- par(las=2)
participants_LOG <-  boxplot(PEAR_CODES ~ CODES , varwidth=TRUE,outline = FALSE,
                         data = PEAR_CODES, 
                         xlab= "", ylab = "Pearson_Corr-Participants (median-log)",
                         xlim = c(1, 22), ylim = c(0, 1.1))

text(x = 1:length(participants_LOG$n), y = participants_LOG$stats[5, ] + 0.07,
     participants_LOG$n, font = 4, cex = 0.8)

abline(h=0.6,col=2, lty=2, lwd=3)
title(main ="Pearson Correlation by participants (Log data)", font.main= 1.5)
par(oldpar)

dev.off()

############## Box-plot Pearson by MODEL TYPE (RAW-data) ##############

jpeg('D:/Input_data/Rel_Categories/Input_data/Rel_Categories/STAT_graphs/Models_Pear_log.jpg',
     quality = 100, bg = "white", res = 200, width = 7, height = 7, units = "in")


par(mar=c(8, 5, 3, 2) + 0.3)
oldpar <- par(las=2)
models_LOG <- boxplot(PEAR_CODES ~ MODELS , varwidth=TRUE,outline = FALSE,
                  data = PEAR_CODES, 
                  xlab= "", ylab = "Pearson_Corr-Models (median-log)",
                  xlim = c(0.7, 8.2), ylim = c(0, 1.1))

text(x = 1:length(models_LOG$n), y = models_LOG$stats[5, ] + 0.07,
     models_LOG$n, font = 4, cex = 0.8)

abline(h=0.6,col=2, lty=2, lwd=3)
title(main ="Pearson_Corr-Models (median-log)", font.main= 1.5)
par(oldpar)

dev.off()
