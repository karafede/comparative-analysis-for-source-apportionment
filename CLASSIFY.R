Data from each laboratory are loaded and classified according to given categories
which are chosen among the main pollutant sources .
The R code: Classify.R acts as follow:
- loads data,
- makes mathematical transformations or comparative analysis purposes, group factor profiles,
- chooses and groups factor profiles into chosen categories
- extract the Source Contribution Estimations (SCE) (and their uncertainties).


In the following the R code written for this purpose:

setwd("D:/Input_data")
#########################################################
labs <- c("Lab1_PMF3.0", "Lab2_PMF2","Lab3_PMF3.0", "Lab4_PMF3.0",
          "Lab5_PMF3.0", "Lab6_PMF2", "Lab7_CMB", "Lab8_CMB", "Lab9_PMF3.0",
          "Lab10_ME2", "Lab11_PMF3.0", "Lab12_CMB", "Lab13_PMF3.0",
          "Lab14_PMF3.0", "Lab15_CMB", "Lab16_PMF3.0", "Lab17_PMF3.0",
          "Lab18_COPREM", "Lab19_PMF2", "Lab20_PMF3.0", 
          "Lab21_APCA","Lab22_PMF3.0", "Lab23_CMB","Lab24_PMF3.0",
          "Lab25_FA-MLRA","Lab26_FA-MLRA","Lab27_PMF3.0", "Reference Profiles")

codes <- c("Lab1","Lab2", "Lab3", "Lab4", "Lab5", "Lab6", "Lab7", "Lab8", "Lab9", "Lab10",
          "Lab11", "Lab12", "Lab13", "Lab14", "Lab15", "Lab16", "Lab17", "Lab18", "Lab19",
          "Lab20", "Lab21", "Lab22", "Lab23","Lab24","Lab25", "Lab26","Lab27", "REF")

Lab_Codes <- data.frame(labs,codes)

#########################################################

Classes <- c("BioB","SO4","NO3","DUST",
              "ROAD", "SALT", "TRA", "INDU", "SEC", "SOA")

codes <- c("Biomass Burning","Nitrates", "Sulphates", "Re-Suspendeded/Soil",
           "Road_Dust","Salt", "Traffic", "Industry and Oil Combustion",
           "Secondary", "Sec.Org.Aerosols")

Categories <- data.frame(Classes,codes)

################### Loading Data #####################################

CONCs <- list.files(path ="D:/Input_data",
                       pattern = "CONC") #this includes only files with "CONC"

MASS <- list.files(path ="D:/Input_data",
                    pattern="CONC")

CONTRs <- list.files(path ="D:/Input_data",
                    pattern = "CONTR") #this includes only files with "CONC"


rel_conc <- gsub(pattern="CONC",x=CONCs,replacement="rel_conc") 
rel_conc <- gsub(pattern=".csv",x=rel_conc,replacement="")

rel_log <- gsub(pattern="CONC",x=CONCs,replacement="rel_log") 
rel_log <- gsub(pattern=".csv",x=rel_log,replacement="")

UNCs <- list.files(path ="D:/Input_data",
                      pattern = "UNC") #this includes only files with "UNC"

U_MASS <- list.files(path ="D:/Input_data",
                   pattern="UNC")

rel_unc <- gsub(pattern="UNC",x=UNCs,replacement="rel_unc") 
rel_unc <- gsub(pattern=".csv",x=rel_unc,replacement="")


TRENDs <- list.files(path ="D:/Input_data",
                  pattern = "TREND") #this includes only files with "TREND"

###### MATRICES CONC, UNC, CONTRIBUTIONS (%) and TRENDS ######

nrows <- 39   ##### number of lines in the input data (chemical species)
for (i in c(1:length(CONCs))) {
  assign(gsub("[.]csv$","",CONCs[i]), as.matrix(read.csv(CONCs[i], header=TRUE, 
  as.is = 1)[-nrows,-1]))
}


for (i in c(1:length(UNCs))) {
  assign(gsub("[.]csv$","",UNCs[i]), as.matrix(read.csv(UNCs[i], header=TRUE, 
                                                        as.is = 1)[-nrows,-1]))
}




##### TOTAL MASS MATRICES #########
for (i in c(1:length(MASS))) {
  assign(gsub("","", MASS[i]), as.matrix(read.csv(MASS[i], header=TRUE,
  as.is = 1)[nrows,-1]))
  NROW = nrow(as.matrix(read.csv(MASS[i], header=TRUE,as.is = 1)[nrows,-1]))
  NCOL = ncol(as.matrix(read.csv(MASS[i], header=TRUE,as.is = 1)[nrows,-1]))
# matrix(assign(gsub("","", MASS[i]), as.matrix(read.csv(MASS[i], header=TRUE,as.is = 1)[48,-1])), nrow = 47, ncol = NCOL, byrow = TRUE)
  assign(gsub("","", MASS[i]), matrix(assign(gsub("","", MASS[i]), 
  as.matrix(read.csv(MASS[i], header=TRUE,as.is = 1)[nrows,-1])), 
  nrow = nrows-1, ncol = NCOL, byrow = TRUE))
  }


############################ LOOP for RELATIVE CONCs #######

for (i in c(1:length(MASS))) {
  
NCOL = ncol(as.matrix(read.csv(MASS[i], header=TRUE,as.is = 1)[nrows,-1]))
    
M <-  assign(gsub("","", MASS[i]), matrix(assign(gsub("","", MASS[i]), 
      as.matrix(read.csv(MASS[i], header=TRUE,as.is = 1)[nrows,-1])), 
      nrow = nrows-1, ncol = NCOL, byrow = TRUE))

ROWMASS <-  assign(gsub("","", MASS[i]), matrix(assign(gsub("","", MASS[i]), 
          as.matrix(read.csv(MASS[i], header=TRUE,as.is = 1)[nrows,-1])), 
        nrow = 1, ncol = NCOL, byrow = TRUE))


ROWnames <-  assign(gsub("","", MASS[i]), matrix(assign(gsub("","", MASS[i]), 
          as.matrix(read.csv(MASS[i], header=TRUE,as.is = 1)[,1])), 
          nrow = nrows, ncol = 1, byrow = TRUE))

COL = ncol(as.matrix(read.csv(CONCs[i], header=TRUE,as.is = 1)[-nrows,-1]))

c <- colnames(as.matrix(read.csv(CONCs[i], header=TRUE, as.is = 1)[-nrows,-1]))
r <-  rownames(as.matrix(read.csv(CONCs[i], header=TRUE, as.is = 1)[-nrows,-1]))

C <-  assign(gsub("[.]csv$","",CONCs[i]), matrix(as.matrix(read.csv(CONCs[i], 
      header=TRUE, as.is = 1)[-nrows,-1]), nrow = nrows-1, ncol = COL,
      byrow=FALSE, dimnames = list(r,c)))

Rel_conc <-C/M
Rel_Log <- -1/log(C/M) #### natural Logarithm
Rel_conc <- rbind(Rel_conc, ROWMASS)   ##### add total mass row

#outputname <- paste("Rel_Data/","CONCs", i,".csv", sep ="")
outputname <- paste("Rel_Data/",gsub("[.]csv$","",rel_conc[i]),".csv", sep ="")
print(outputname)
write.csv(Rel_conc, file = outputname)

outputname_log <- paste("Rel_Data/",gsub("[.]csv$","",rel_log[i]),".csv", sep ="")
print(outputname_log)
write.csv(Rel_Log, file = outputname_log)

}

########################### LOOP for RELATIVE UNCs #######

for (i in c(1:length(MASS))) {
  
u_NCOL = ncol(as.matrix(read.csv(MASS[i], header=TRUE,as.is = 1)[nrows,-1]))
  
u_M <-  assign(gsub("","", MASS[i]), matrix(assign(gsub("","", MASS[i]), 
      as.matrix(read.csv(MASS[i], header=TRUE,as.is = 1)[nrows,-1])), 
     nrow = nrows-1, ncol = u_NCOL, byrow = TRUE))


U_ROWMASS <-  assign(gsub("","", U_MASS[i]), matrix(assign(gsub("","", U_MASS[i]), 
                    as.matrix(read.csv(U_MASS[i], header=TRUE,as.is = 1)[nrows,-1])), 
                    nrow = 1, ncol = u_NCOL, byrow = TRUE))
  
u_COL = ncol(as.matrix(read.csv(UNCs[i], header=TRUE,as.is = 1)[-nrows,-1]))

u_c <- colnames(as.matrix(read.csv(CONCs[i], header=TRUE, as.is = 1)[-nrows,-1]))
u_r <-  rownames(as.matrix(read.csv(CONCs[i], header=TRUE, as.is = 1)[-nrows,-1]))
  
u_C <-  assign(gsub("[.]csv$","",UNCs[i]), matrix(as.matrix(read.csv(UNCs[i], header=TRUE, 
        as.is = 1)[-nrows,-1]), nrow = nrows-1, ncol = u_COL,byrow=FALSE, 
        dimnames = list(u_r,u_c) ))
  
Rel_unc <- u_C/u_M
Rel_unc <- rbind(Rel_unc, U_ROWMASS)   ##### add total mass row
  
outputname <- paste("Rel_Data/",gsub("[.]csv$","",rel_unc[i]),".csv", sep ="")
print(outputname)
write.csv(Rel_unc, file = outputname)
}


########################### LOOP for TRENDS ###############################

for (i in c(1:length(TRENDs))) {
  assign(gsub("[.]csv$","",TRENDs[i]),read.csv(TRENDs[i], header=TRUE, as.is = 1))


ROWnames_TRENDS <-  assign(gsub("","", TRENDs[i]), matrix(assign(gsub("","", TRENDs[i]), 
          as.matrix(read.csv(TRENDs[i], header=TRUE,as.is = 1)[,1])), 
          nrow = 364, ncol = 1, byrow = TRUE))
}
########################################################
###################### read and bind TIME TREND concentration matrices #######

TRENDS <- list.files(path ="D:/Input_data",
            pattern = "TREND") #this includes only files with "TREND"

setwd("D:/Input_data")
TRENDS_Bind = numeric()

for (i in c(1:length(TRENDS))) {
  
  COL_TRENDS = ncol(as.matrix(read.csv(TRENDS[i], header=TRUE,as.is = 1)[,-1]))
  
  COL_NAMES_TRENDS <- colnames(as.matrix(read.csv(TRENDS[i], header=TRUE, as.is = 1)[,-1]))
  ROW_NAMES_TRENDS <-  rownames(as.matrix(read.csv(TRENDS[i], header=TRUE, as.is = 1)[,-1]))
  
  T <-  assign(gsub("[.]csv$","",TRENDS[i]), 
               matrix(as.matrix(read.csv(TRENDS[i], header=TRUE, 
                      as.is = 1)[,-1]), nrow = 364, ncol = COL_TRENDS, 
                      byrow=FALSE, dimnames = list(ROW_NAMES_TRENDS,COL_NAMES_TRENDS)))
  
  TRENDS_Bind <- cbind(TRENDS_Bind,T)
}

SAMPLES <- ROWnames_TRENDS[,]
TRENDS_Bind <- cbind(SAMPLES,TRENDS_Bind) ###add row names


######################## Create CLASS FACTORS for TIME TRENDS #########

BioB_t <- subset(TRENDS_Bind, select=grep("BioB", colnames(TRENDS_Bind)))
BioB_REF_t <- read.csv("BioB_REF_t.csv", header = TRUE) ##reference
BioB_t <- cbind(SAMPLES,BioB_t,BioB_REF_t[2]) ###add row names

SO4_t <- subset(TRENDS_Bind, select=grep("SO4", colnames(TRENDS_Bind)))
SO4_REF_t <- read.csv("SO4_REF_t.csv", header = TRUE) ##reference
SO4_t <- cbind(SAMPLES,SO4_t,SO4_REF_t[2]) ###add row names

NO3_t <- subset(TRENDS_Bind, select=grep("NO3", colnames(TRENDS_Bind)))
NO3_REF_t <- read.csv("NO3_REF_t.csv", header = TRUE) ##reference
NO3_t <- cbind(SAMPLES,NO3_t,NO3_REF_t[2]) ###add row names

DUST_t <- subset(TRENDS_Bind, select=grep("DUST", colnames(TRENDS_Bind)))
DUST_REF_t <- read.csv("DUST_REF_t.csv", header = TRUE) ##reference
DUST_t <- cbind(SAMPLES,DUST_t,DUST_REF_t[2]) ###add row names

ROAD_t <- subset(TRENDS_Bind, select=grep("ROAD", colnames(TRENDS_Bind)))
ROAD_REF_t <- read.csv("ROAD_REF_t.csv", header = TRUE) ##reference
ROAD_t <- cbind(SAMPLES,ROAD_t,ROAD_REF_t[2]) ###add row names

SALT_t <- subset(TRENDS_Bind, select=grep("SALT", colnames(TRENDS_Bind)))
SALT_REF_t <- read.csv("SALT_REF_t.csv", header = TRUE) ##reference
SALT_t <- cbind(SAMPLES,SALT_t,SALT_REF_t[2]) ###add row names

TRA_t <- subset(TRENDS_Bind, select=grep("TRA", colnames(TRENDS_Bind)))
TRA_REF_t <- read.csv("TRA_REF_t.csv", header = TRUE) ##reference
TRA_t <- cbind(SAMPLES,TRA_t,TRA_REF_t[2]) ###add row names

INDU_t <- subset(TRENDS_Bind, select=grep("INDU", colnames(TRENDS_Bind)))
INDU_REF_t <- read.csv("INDU_REF_t.csv", header = TRUE) ##reference
INDU_t <- cbind(SAMPLES,INDU_t,INDU_REF_t[2]) ###add row names

SEC_t <- subset(TRENDS_Bind, select=grep("SEC", colnames(TRENDS_Bind)))
SEC_t <- cbind(SAMPLES,SEC_t) ###add row names


########################### LOOP for CONTR (%) (CONTRIBUTIONS) #############

for (i in c(1:length(CONTRs))) {
  assign(gsub("[.]csv$","",CONTRs[i]),read.csv(CONTRs[i], header=TRUE, as.is = 1))

  ROWnames_CONTRS <-  assign(gsub("","", CONTRs[i]), matrix(assign(gsub("","", CONTRs[i]), 
  as.matrix(read.csv(CONTRs[i], header=TRUE,as.is = 1)[,1])), 
  nrow = nrows, ncol = 1, byrow = TRUE))
  
}

########################################################
###################### read and bind CONTR (%) matrices #######

CONTRS <- list.files(path ="D:/Input_data",
                     pattern = "CONTR") #this includes only files with "TREND"

setwd("D:/Input_data")
CONTRS_Bind = numeric()

for (i in c(1:length(CONTRS))) {
  
  COL_CONTRS = ncol(as.matrix(read.csv(CONTRS[i], header=TRUE,as.is = 1)[,-1]))
  
  COL_NAMES_CONTRS <- colnames(as.matrix(read.csv(CONTRS[i], header=TRUE, as.is = 1)[,-1]))
  ROW_NAMES_CONTRS <-  rownames(as.matrix(read.csv(CONTRS[i], header=TRUE, as.is = 1)[,-1]))
  
  T <-  assign(gsub("[.]csv$","",CONTRS[i]), 
          matrix(as.matrix(read.csv(CONTRS[i], header=TRUE, 
          as.is = 1)[,-1]), nrow = nrows, ncol = COL_CONTRS, 
          byrow=FALSE, dimnames = list(ROW_NAMES_CONTRS,COL_NAMES_CONTRS)))
  
  CONTRS_Bind <- cbind(CONTRS_Bind,T)
}

SAMPLES_CONTRS <- ROWnames_CONTRS[,]
CONTRS_Bind <- cbind(SAMPLES_CONTRS,CONTRS_Bind) ###add row names

#################### Create CLASS FACTORS for CONTRIBUTIONS (%) #########

BioB_CONTR <- subset(CONTRS_Bind, select=grep("BioB", colnames(CONTRS_Bind)))
BioB_CONTR <- cbind(SAMPLES_CONTRS,BioB_CONTR) ###add row names

SO4_CONTR <- subset(CONTRS_Bind, select=grep("SO4", colnames(CONTRS_Bind)))
SO4_CONTR <- cbind(SAMPLES_CONTRS,SO4_CONTR) ###add row names

NO3_CONTR <- subset(CONTRS_Bind, select=grep("NO3", colnames(CONTRS_Bind)))
NO3_CONTR <- cbind(SAMPLES_CONTRS,NO3_CONTR) ###add row names

DUST_CONTR <- subset(CONTRS_Bind, select=grep("DUST", colnames(CONTRS_Bind)))
DUST_CONTR <- cbind(SAMPLES_CONTRS,DUST_CONTR) ###add row names

ROAD_CONTR <- subset(CONTRS_Bind, select=grep("ROAD", colnames(CONTRS_Bind)))
ROAD_CONTR <- cbind(SAMPLES_CONTRS,ROAD_CONTR) ###add row names

SALT_CONTR <- subset(CONTRS_Bind, select=grep("SALT", colnames(CONTRS_Bind)))
SALT_CONTR <- cbind(SAMPLES_CONTRS,SALT_CONTR) ###add row names

TRA_CONTR <- subset(CONTRS_Bind, select=grep("TRA", colnames(CONTRS_Bind)))
TRA_CONTR <- cbind(SAMPLES_CONTRS,TRA_CONTR) ###add row names

INDU_CONTR <- subset(CONTRS_Bind, select=grep("INDU", colnames(CONTRS_Bind)))
INDU_CONTR <- cbind(SAMPLES_CONTRS,INDU_CONTR) ###add row names

SEC_CONTR <- subset(CONTRS_Bind, select=grep("SEC", colnames(CONTRS_Bind)))
SEC_CONTR <- cbind(SAMPLES_CONTRS,SEC_CONTR) ###add row names

###################### read and bind relative concentration matrices #######

REL_CONCENTRATIONS <- list.files(path ="D:/Input_data/Rel_Data",
          pattern = "rel_conc") #this includes only files with "rel_conc"

setwd("D:/Input_data/Rel_Data")
REL_Bind_CONCs = numeric()

for (i in c(1:length(REL_CONCENTRATIONS))) {
 
COL = ncol(as.matrix(read.csv(REL_CONCENTRATIONS[i], header=TRUE,as.is = 1)[,-1]))

COL_NAMES <- colnames(as.matrix(read.csv(REL_CONCENTRATIONS[i], header=TRUE, as.is = 1)[,-1]))
ROW_NAMES <-  rownames(as.matrix(read.csv(REL_CONCENTRATIONS[i], header=TRUE, as.is = 1)[,-1]))
 
 R <-  assign(gsub("[.]csv$","",REL_CONCENTRATIONS[i]), 
              matrix(as.matrix(read.csv(REL_CONCENTRATIONS[i], header=TRUE, 
              as.is = 1)[,-1]), nrow = nrows, ncol = COL, 
              byrow=FALSE, dimnames = list(ROW_NAMES,COL_NAMES)))

REL_Bind_CONCs <- cbind(REL_Bind_CONCs,R)
}

SPECIES <- ROWnames[,]
REL_Bind_CONCs <- cbind(SPECIES,REL_Bind_CONCs) ###add row names

##########################################################################

###################### read and bind rel_log concentration matrices #######

REL_LOG <- list.files(path ="D:/Input_data/Rel_Data",
                                 pattern = "rel_log") #this includes only files with "rel_log"

setwd("D:/Input_data/Rel_Data")
REL_Bind_LOG = numeric()

for (i in c(1:length(REL_LOG))) {
  
  COL_LOG = ncol(as.matrix(read.csv(REL_LOG[i], header=TRUE,as.is = 1)[,-1]))
  
  COL_NAMES_LOG <- colnames(as.matrix(read.csv(REL_LOG[i], header=TRUE, as.is = 1)[,-1]))
  ROW_NAMES_LOG <-  rownames(as.matrix(read.csv(REL_LOG[i], header=TRUE, as.is = 1)[,-1]))
  
  R_LOG <-  assign(gsub("[.]csv$","",REL_LOG[i]), 
              matrix(as.matrix(read.csv(REL_LOG[i], header=TRUE, 
              as.is = 1)[,-1]), nrow = nrows-1, ncol = COL_LOG, 
              byrow=FALSE, dimnames = list(ROW_NAMES_LOG,COL_NAMES_LOG)))
  
  REL_Bind_LOG <- cbind(REL_Bind_LOG,R_LOG)
}
SPECIES_LOG <- ROWnames[-nrows,]
REL_Bind_LOG <- cbind(SPECIES_LOG,REL_Bind_LOG) ###add row names

##########################################################################


######################## Create CLASS FACTORS for CONCENTRATIONS #########

BioB <- subset(REL_Bind_CONCs,
               select=grep("BioB", colnames(REL_Bind_CONCs))) 
BioB_REF_rel <- read.csv("BioB_REF_rel.csv", header = TRUE) ##reference
BioB_rel <- cbind(SPECIES[1:nrows-1],BioB[1:nrows-1,],BioB_REF_rel[2]) ###add row names and references
BioB_SCEs <- as.matrix(BioB[nrows,]) ###SCEs grouped by CLASS FACTORS

BioB_LOG <- subset(REL_Bind_LOG,
               select=grep("BioB", colnames(REL_Bind_LOG)))
BioB_REF_LOG <- -1/log(BioB_REF_rel[2])
BioB_rel_LOG <- cbind(SPECIES_LOG,BioB_LOG,BioB_REF_LOG) ###add row names and references

########### ########## ########## ########### ########### ########### #####

SO4 <- subset(REL_Bind_CONCs, 
              select=grep("SO4", colnames(REL_Bind_CONCs)))
SO4_REF_rel <- read.csv("SO4_REF_rel.csv", header = TRUE) ##reference
SO4_rel <- cbind(SPECIES[1:nrows-1],SO4[1:nrows-1,],SO4_REF_rel[2]) ###add row names and references
SO4_SCEs <- as.matrix(SO4[nrows,]) ###SCEs grouped by CLASS FACTORS

SO4_LOG <- subset(REL_Bind_LOG,
                  select=grep("SO4", colnames(REL_Bind_LOG)))
SO4_REF_LOG <- -1/log(SO4_REF_rel[2])
SO4_rel_LOG <- cbind(SPECIES_LOG,SO4_LOG,SO4_REF_LOG) ###add row names and references

########### ########## ########## ########### ########### ########### #####

NO3 <- subset(REL_Bind_CONCs, 
              select=grep("NO3", colnames(REL_Bind_CONCs)))
NO3_REF_rel <- read.csv("NO3_REF_rel.csv", header = TRUE) ##reference
NO3_rel <- cbind(SPECIES[1:nrows-1],NO3[1:nrows-1,],NO3_REF_rel[2]) ###add row names and references
NO3_SCEs <- as.matrix(NO3[nrows,]) ###SCEs grouped by CLASS FACTORS

NO3_LOG <- subset(REL_Bind_LOG,
                  select=grep("NO3", colnames(REL_Bind_LOG)))
NO3_REF_LOG <- -1/log(NO3_REF_rel[2])
NO3_rel_LOG <- cbind(SPECIES_LOG,NO3_LOG,NO3_REF_LOG) ###add row names and references

########### ########## ########## ########### ########### ########### #####

DUST <- subset(REL_Bind_CONCs, 
               select=grep("DUST", colnames(REL_Bind_CONCs)))
DUST_REF_rel <- read.csv("DUST_REF_rel.csv", header = TRUE) ##reference
DUST_rel <- cbind(SPECIES[1:nrows-1],DUST[1:nrows-1,],DUST_REF_rel[2]) ###add row names and references
DUST_SCEs <- as.matrix(DUST[nrows,]) ###SCEs grouped by CLASS FACTORS

DUST_LOG <- subset(REL_Bind_LOG,
                   select=grep("DUST", colnames(REL_Bind_LOG)))
DUST_REF_LOG <- -1/log(DUST_REF_rel[2])
DUST_rel_LOG <- cbind(SPECIES_LOG,DUST_LOG,DUST_REF_LOG) ###add row names and references

########### ########## ########## ########### ########### ########### #####

ROAD <- subset(REL_Bind_CONCs, 
               select=grep("ROAD", colnames(REL_Bind_CONCs)))
ROAD_REF_rel <- read.csv("ROAD_REF_rel.csv", header = TRUE) ##reference
ROAD_rel <- cbind(SPECIES[1:nrows-1],ROAD[1:nrows-1,],ROAD_REF_rel[2]) ###add row names and references
ROAD_SCEs <- as.matrix(ROAD[nrows,]) ###SCEs grouped by CLASS FACTORS

ROAD_LOG <- subset(REL_Bind_LOG,
                   select=grep("ROAD", colnames(REL_Bind_LOG)))
ROAD_REF_LOG <- -1/log(ROAD_REF_rel[2])
ROAD_rel_LOG <- cbind(SPECIES_LOG,ROAD_LOG,ROAD_REF_LOG) ###add row names and references

########### ########## ########## ########### ########### ########### #####

SALT <- subset(REL_Bind_CONCs, 
               select=grep("SALT", colnames(REL_Bind_CONCs)))
SALT_REF_rel <- read.csv("SALT_REF_rel.csv", header = TRUE) ##reference
SALT_rel <- cbind(SPECIES[1:nrows-1],SALT[1:nrows-1,],SALT_REF_rel[2]) ###add row names and references
SALT_SCEs <- as.matrix(SALT[nrows,]) ###SCEs grouped by CLASS FACTORS

SALT_LOG <- subset(REL_Bind_LOG,
                   select=grep("SALT", colnames(REL_Bind_LOG)))
SALT_REF_LOG <- -1/log(SALT_REF_rel[2])
SALT_rel_LOG <- cbind(SPECIES_LOG,SALT_LOG,SALT_REF_LOG) ###add row names and references

########### ########## ########## ########### ########### ########### #####

TRA <- subset(REL_Bind_CONCs, 
              select=grep("TRA", colnames(REL_Bind_CONCs)))
TRA_REF_rel <- read.csv("TRA_REF_rel.csv", header = TRUE) ##reference
TRA_rel <- cbind(SPECIES[1:nrows-1],TRA[1:nrows-1,],TRA_REF_rel[2]) ###add row names and references
TRA_SCEs <- as.matrix(TRA[nrows,]) ###SCEs grouped by CLASS FACTORS

TRA_LOG <- subset(REL_Bind_LOG,
                   select=grep("TRA", colnames(REL_Bind_LOG)))
TRA_REF_LOG <- -1/log(TRA_REF_rel[2])
TRA_rel_LOG <- cbind(SPECIES_LOG,TRA_LOG,TRA_REF_LOG) ###add row names and references

########### ########## ########## ########### ########### ########### #####

INDU <- subset(REL_Bind_CONCs, 
               select=grep("INDU", colnames(REL_Bind_CONCs)))
INDU_REF_rel <- read.csv("INDU_REF_rel.csv", header = TRUE) ##reference
INDU_rel <- cbind(SPECIES[1:nrows-1],INDU[1:nrows-1,],INDU_REF_rel[2]) ###add row names and references
INDU_SCEs <- as.matrix(INDU[nrows,]) ###SCEs grouped by CLASS FACTORS

INDU_LOG <- subset(REL_Bind_LOG,
                   select=grep("INDU", colnames(REL_Bind_LOG)))
INDU_REF_LOG <- -1/log(INDU_REF_rel[2])
INDU_rel_LOG <- cbind(SPECIES_LOG,INDU_LOG,INDU_REF_LOG) ###add row names and references

########### ########## ########## ########### ########### ########### #####

SEC <- subset(REL_Bind_CONCs, 
              select=grep("SEC", colnames(REL_Bind_CONCs)))
#SEC_REF_rel <- read.csv("SEC_REF_rel.csv", header = TRUE) ##reference
SEC_rel <- cbind(SPECIES[1:nrows-1],SEC[1:nrows-1,]) ###add row names and references
SEC_SCEs <- as.matrix(SEC[nrows,]) ###SCEs grouped by CLASS FACTORS

SEC_LOG <- subset(REL_Bind_LOG,
                   select=grep("SEC", colnames(REL_Bind_LOG)))
#SEC_REF_LOG <- matrix((-1/log(SEC_REF_rel[,-1]))
SEC_rel_LOG <- cbind(SPECIES_LOG,SEC_LOG) ###add row names and references

########### ########## ########## ########### ########### ########### #####


###################### read and bind relative UNCs matrices #######

REL_UNCERTAINTIES <- list.files(path ="D:/Input_data/Rel_Data",
                                 pattern = "rel_unc") #this includes only files with "rel_conc"

setwd("D:/Input_data/Rel_Data")
REL_Bind_UNCs = numeric()

for (i in c(1:length(REL_UNCERTAINTIES))) {
  
COL = ncol(as.matrix(read.csv(REL_UNCERTAINTIES[i], header=TRUE,as.is = 1)[,-1]))
  
u_COL_NAMES <- colnames(as.matrix(read.csv(REL_UNCERTAINTIES[i], header=TRUE, as.is = 1)[,-1]))
u_ROW_NAMES <-  rownames(as.matrix(read.csv(REL_UNCERTAINTIES[i], header=TRUE, as.is = 1)[,-1]))
  
  Z <-  assign(gsub("[.]csv$","",REL_UNCERTAINTIES[i]), 
               matrix(as.matrix(read.csv(REL_UNCERTAINTIES[i], header=TRUE, 
                                         as.is = 1)[,-1]), nrow = nrows, ncol = COL, 
                      byrow=FALSE, dimnames = list(u_ROW_NAMES,u_COL_NAMES)))
  
  REL_Bind_UNCs <- cbind(REL_Bind_UNCs,Z)
}
u_SPECIES <- ROWnames[,]
REL_Bind_UNCs <- cbind(u_SPECIES,REL_Bind_UNCs) ###add row names


######################## Create CLASS FACTORS for UNCERTAINTIES #########

BioB_U <- subset(REL_Bind_UNCs, 
                 select=grep("BioB", colnames(REL_Bind_UNCs)))
BioB_U_SCEs <- as.matrix(BioB_U[nrows,])
BioB_REF_unc <- read.csv("BioB_REF_unc.csv", header = TRUE) ##reference
BioB_U <- cbind(u_SPECIES[1:nrows-1],BioB_U[1:nrows-1,],BioB_REF_unc[2]) ###add row names

########### ########## ########## ########### ########### ########### #####

SO4_U <- subset(REL_Bind_UNCs, 
                select=grep("SO4", colnames(REL_Bind_UNCs)))
SO4_U_SCEs <- as.matrix(SO4_U[nrows,])
SO4_REF_unc <- read.csv("SO4_REF_unc.csv", header = TRUE) ##reference
SO4_U <- cbind(u_SPECIES[1:nrows-1],SO4_U[1:nrows-1,],SO4_REF_unc[2]) ###add row names


########### ########## ########## ########### ########### ########### #####

NO3_U <- subset(REL_Bind_UNCs, select=grep("NO3", colnames(REL_Bind_UNCs)))
NO3_U_SCEs <- as.matrix(NO3_U[nrows,])
NO3_REF_unc <- read.csv("NO3_REF_unc.csv", header = TRUE) ##reference
NO3_U <- cbind(u_SPECIES[1:nrows-1],NO3_U[1:nrows-1,],NO3_REF_unc[2]) ###add row names

########### ########## ########## ########### ########### ########### #####

DUST_U <- subset(REL_Bind_UNCs, 
                 select=grep("DUST", colnames(REL_Bind_UNCs)))
DUST_U_SCEs <- as.matrix(DUST_U[nrows,])
DUST_REF_unc <- read.csv("DUST_REF_unc.csv", header = TRUE) ##reference
DUST_U <- cbind(u_SPECIES[1:nrows-1],DUST_U[1:nrows-1,],DUST_REF_unc[2]) ###add row names

########### ########## ########## ########### ########### ########### #####

ROAD_U <- subset(REL_Bind_UNCs, 
                 select=grep("ROAD", colnames(REL_Bind_UNCs)))
ROAD_U_SCEs <- as.matrix(ROAD_U[nrows,])
ROAD_REF_unc <- read.csv("ROAD_REF_unc.csv", header = TRUE) ##reference
ROAD_U <- cbind(u_SPECIES[1:nrows-1],ROAD_U[1:nrows-1,],ROAD_REF_unc[2]) ###add row names

########### ########## ########## ########### ########### ########### #####

SALT_U <- subset(REL_Bind_UNCs, 
                 select=grep("SALT", colnames(REL_Bind_UNCs)))
SALT_U_SCEs <- as.matrix(SALT_U[nrows,])
SALT_REF_unc <- read.csv("SALT_REF_unc.csv", header = TRUE) ##reference
SALT_U <- cbind(u_SPECIES[1:nrows-1],SALT_U[1:nrows-1,],SALT_REF_unc[2]) ###add row names

########### ########## ########## ########### ########### ########### #####

TRA_U <- subset(REL_Bind_UNCs, 
                select=grep("TRA", colnames(REL_Bind_UNCs)))
TRA_U_SCEs <- as.matrix(TRA_U[nrows,])
TRA_REF_unc <- read.csv("TRA_REF_unc.csv", header = TRUE) ##reference
TRA_U <- cbind(u_SPECIES[1:nrows-1],TRA_U[1:nrows-1,],TRA_REF_unc[2]) ###add row names

########### ########## ########## ########### ########### ########### #####

INDU_U <- subset(REL_Bind_UNCs, 
                 select=grep("INDU", colnames(REL_Bind_UNCs)))
INDU_U_SCEs <- as.matrix(INDU_U[nrows,])
INDU_REF_unc <- read.csv("INDU_REF_unc.csv", header = TRUE) ##reference
INDU_U <- cbind(u_SPECIES[1:nrows-1],INDU_U[1:nrows-1,],INDU_REF_unc[2]) ###add row names

########### ########## ########## ########### ########### ########### #####

SEC_U <- subset(REL_Bind_UNCs, 
                select=grep("SEC", colnames(REL_Bind_UNCs)))
SEC_U_SCEs <- as.matrix(SEC_U[nrows,])
#SEC_REF_unc <- read.csv("SEC_REF_unc.csv", header = TRUE) ##reference
SEC_U <- cbind(u_SPECIES[1:nrows-1],SEC_U[1:nrows-1,]) ###add row names


####### save CATEGORIES (Classes) ###################################

write.csv(BioB_rel, file = "BioB_rel.csv", na = "")  ### CLASS FACTOR concentrations
write.csv(BioB_rel_LOG, file = "BioB_rel_LOG.csv", na = "")  ### CLASS FACTOR concentrations
write.csv(BioB_U, file = "BioB_U.csv", na = "") ### CLASS FACTOR uncertainty
write.csv(BioB_t, file = "BioB_t.csv", na = "") ### CLASS FACTOR TREND
write.csv(BioB_CONTR, file = "BioB_CONTR.csv", na = "") ### CLASS FACTOR CONTRIBUTION
write.csv(BioB_SCEs, file = "BioB_SCEs.csv", na = "") #### SCE (source contribution estimation)
write.csv(BioB_U_SCEs, file = "BioB_U_SCEs.csv", na = "") #### 


write.csv(SO4_rel, file = "SO4_rel.csv", na = "")  ### CLASS FACTOR concentrations
write.csv(SO4_rel_LOG, file = "SO4_rel_LOG.csv", na = "")  ### CLASS FACTOR concentrations
write.csv(SO4_U, file = "SO4_U.csv", na = "") ### CLASS FACTOR uncertainty
write.csv(SO4_t, file = "SO4_t.csv", na = "") ### CLASS FACTOR TREND
write.csv(SO4_CONTR, file = "SO4_CONTR.csv", na = "") ### CLASS FACTOR CONTRIBUTION
write.csv(SO4_SCEs, file = "SO4_SCEs.csv", na = "") #### SCE (source contribution estimation)
write.csv(SO4_U_SCEs, file = "SO4_U_SCEs.csv", na = "") 


write.csv(NO3_rel, file = "NO3_rel.csv", na = "")  ### CLASS FACTOR concentrations
write.csv(NO3_rel_LOG, file = "NO3_rel_LOG.csv", na = "")  ### CLASS FACTOR concentrations
write.csv(NO3_U, file = "NO3_U.csv", na = "") ### CLASS FACTOR uncertainty
write.csv(NO3_t, file = "NO3_t.csv", na = "") ### CLASS FACTOR TREND
write.csv(NO3_CONTR, file = "NO3_CONTR.csv", na = "") ### CLASS FACTOR CONTRIBUTION
write.csv(NO3_SCEs, file = "NO3_SCEs.csv", na = "") #### SCE (source contribution estimation)
write.csv(NO3_U_SCEs, file = "NO3_U_SCEs.csv", na = "") 


write.csv(DUST_rel, file = "DUST_rel.csv", na = "")  ### CLASS FACTOR concentrations
write.csv(DUST_rel_LOG, file = "DUST_rel_LOG.csv", na = "")  ### CLASS FACTOR concentrations
write.csv(DUST_U, file = "DUST_U.csv", na = "") ### CLASS FACTOR uncertainty
write.csv(DUST_t, file = "DUST_t.csv", na = "") ### CLASS FACTOR TREND
write.csv(DUST_CONTR, file = "DUST_CONTR.csv", na = "") ### CLASS FACTOR CONTRIBUTION
write.csv(DUST_SCEs, file = "DUST_SCEs.csv", na = "") #### SCE (source contribution estimation)
write.csv(DUST_U_SCEs, file = "DUST_U_SCEs.csv", na = "") 


write.csv(ROAD_rel, file = "ROAD_rel.csv", na = "")  ### CLASS FACTOR concentrations
write.csv(ROAD_rel_LOG, file = "ROAD_rel_LOG.csv", na = "")  ### CLASS FACTOR concentrations
write.csv(ROAD_U, file = "ROAD_U.csv", na = "") ### CLASS FACTOR uncertainty
write.csv(ROAD_t, file = "ROAD_t.csv", na = "") ### CLASS FACTOR TREND
write.csv(ROAD_CONTR, file = "ROAD_CONTR.csv", na = "") ### CLASS FACTOR CONTRIBUTION
write.csv(ROAD_SCEs, file = "ROAD_SCEs.csv", na = "") #### SCE (source contribution estimation)
write.csv(ROAD_U_SCEs, file = "ROAD_U_SCEs.csv", na = "") 


write.csv(SALT_rel, file = "SALT_rel.csv", na = "")  ### CLASS FACTOR concentrations
write.csv(SALT_rel_LOG, file = "SALT_rel_LOG.csv", na = "")  ### CLASS FACTOR concentrations
write.csv(SALT_U, file = "SALT_U.csv", na = "") ### CLASS FACTOR uncertainty
write.csv(SALT_t, file = "SALT_t.csv", na = "") ### CLASS FACTOR TREND
write.csv(SALT_CONTR, file = "SALT_CONTR.csv", na = "") ### CLASS FACTOR CONTRIBUTION
write.csv(SALT_SCEs, file = "SALT_SCEs.csv", na = "") #### SCE (source contribution estimation)
write.csv(SALT_U_SCEs, file = "SALT_U_SCEs.csv", na = "") 


write.csv(TRA_rel, file = "TRA_rel.csv", na = "")  ### CLASS FACTOR concentrations
write.csv(TRA_rel_LOG, file = "TRA_rel_LOG.csv", na = "")  ### CLASS FACTOR concentrations
write.csv(TRA_U, file = "TRA_U.csv", na = "") ### CLASS FACTOR uncertainty
write.csv(TRA_t, file = "TRA_t.csv", na = "") ### CLASS FACTOR TREND
write.csv(TRA_CONTR, file = "TRA_CONTR.csv", na = "") ### CLASS FACTOR CONTRIBUTION
write.csv(TRA_SCEs, file = "TRA_SCEs.csv", na = "") #### SCE (source contribution estimation)
write.csv(TRA_U_SCEs, file = "TRA_U_SCEs.csv", na = "")


write.csv(INDU_rel, file = "INDU_rel.csv", na = "")  ### CLASS FACTOR concentrations
write.csv(INDU_rel_LOG, file = "INDU_rel_LOG.csv", na = "")  ### CLASS FACTOR concentrations
write.csv(INDU_U, file = "INDU_U.csv", na = "") ### CLASS FACTOR uncertainty
write.csv(INDU_t, file = "INDU_t.csv", na = "") ### CLASS FACTOR TREND
write.csv(INDU_CONTR, file = "INDU_CONTR.csv", na = "") ### CLASS FACTOR CONTRIBUTION
write.csv(INDU_SCEs, file = "INDU_SCEs.csv", na = "") #### SCE (source contribution estimation)
write.csv(INDU_U_SCEs, file = "INDU_U_SCEs.csv", na = "") 


write.csv(SEC_rel, file = "SEC_rel.csv", na = "")  ### CLASS FACTOR concentrations
write.csv(SEC_rel_LOG, file = "SEC_rel_LOG.csv", na = "")  ### CLASS FACTOR concentrations
write.csv(SEC_U, file = "SEC_U.csv", na = "") ### CLASS FACTOR uncertainty
write.csv(SEC_t, file = "SEC_t.csv", na = "") ### CLASS FACTOR TREND
write.csv(SEC_CONTR, file = "SEC_CONTR.csv", na = "") ### CLASS FACTOR CONTRIBUTION
write.csv(SEC_SCEs, file = "SEC_SCEs.csv", na = "") #### SCE (source contribution estimation)
write.csv(SEC_U_SCEs, file = "SEC_U_SCEs.csv", na = "")
