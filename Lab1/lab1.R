library("readxl")

#--------------------EXERCISE 1------------------------------
EPI_data = read_xls(file.choose(), sheet="EPI2010_all countries")
######EDA for WATER_H
tf = is.na(EPI_data$WATER_H)
EPI_WATER_H_na_free = EPI_data[!tf,]
#Grabbing only non NA's and non "NA" rows for WATER_H column. Converting from char to numeric type
EPI_WATER_H_na_free = EPI_WATER_H_na_free[EPI_WATER_H_na_free$WATER_H != "NA",]
EPI_WATER_H_na_free <- transform(EPI_WATER_H_na_free,
                             WATER_H = as.numeric(WATER_H))

#summary(EPI_WATER_H_na_free)
#fivenum(EPI_WATER_H_na_free$WATER_H)
stem(EPI_WATER_H_na_free$WATER_H)
hist(EPI_WATER_H_na_free$WATER_H, main="Histogram of WATER_H", prob=TRUE)
lines(density(EPI_WATER_H_na_free$WATER_H, bw=5, kernel="gaussian"))
boxplot(EPI_WATER_H_na_free$WATER_H, main="boxplot(WATER_H)", names=c("WATER_H"))
#Cumulative Distribution Function: Can be defined for continuous and discrete variables
plot(ecdf(EPI_WATER_H_na_free$WATER_H), do.points=FALSE, verticals=TRUE)
par(pty="s")
#Quantile-Quantile plot for WATER_H column against Normal Distribution
qqnorm(EPI_WATER_H_na_free$WATER_H, main="Normal Q-Q Plot: WATER_H"); qqline(EPI_WATER_H_na_free$WATER_H)


######EDA for AIR_H
EPI_AIR_H_na_free = EPI_data[!is.na(EPI_data$AIR_H),]
#Grabbing only non NA's and non "NA" rows for AIR_H column. Converting from char to numeric type
EPI_AIR_H_na_free = EPI_AIR_H_na_free[EPI_AIR_H_na_free$AIR_H != "NA",]
EPI_AIR_H_na_free <- transform(EPI_AIR_H_na_free,
                                 AIR_H = as.numeric(AIR_H))
#summary(EPI_AIR_H_na_free)
#fivenum(EPI_AIR_H_na_free$AIR_H)
stem(EPI_AIR_H_na_free$AIR_H)
hist(EPI_AIR_H_na_free$AIR_H, main="Histogram of AIR_H", prob=TRUE)
lines(density(EPI_AIR_H_na_free$AIR_H, bw=4, kernel="gaussian"))
boxplot(EPI_AIR_H_na_free$AIR_H, main="boxplot(AIR_H)", names=c("AIR_H"))
#Cumulative Distribution Function: Can be defined for continuous and discrete variables
plot(ecdf(EPI_AIR_H_na_free$AIR_H), do.points=FALSE, verticals=TRUE)
par(pty="s")
#Quantile-Quantile plot for WATER_H column against Normal Distribution
qqnorm(EPI_AIR_H_na_free$AIR_H, main="Normal Q-Q Plot: AIR_H"); qqline(EPI_AIR_H_na_free$AIR_H)

######Comparing WATER_H and AIR_H
boxplot(EPI_WATER_H_na_free$WATER_H, EPI_AIR_H_na_free$AIR_H
        , main="boxplot(WATER_H, AIR_H)", names=c("WATER_H", "AIR_H"))
par(pty="s")
#Quantile-Quantile plot for WATER_H against AIR_H
qqplot(EPI_AIR_H_na_free$AIR_H, EPI_WATER_H_na_free$WATER_H, main="qqplot(WATER_H, AIR_H)", xlab = "AIR_H",
                ylab = "WATER_H")




#Prepping EPI, ENVHEALTH, ECOSYSTEM, DALY, AIR_E, WATER_E, BIODIVERSITY data sets
EPI = EPI_data[!is.na(EPI_data$EPI),]
#Grabbing only non NA's and non "NA" rows . Converting from char to numeric type
EPI = EPI[EPI$EPI != "NA", "EPI"]
EPI <- transform(EPI, EPI = as.numeric(EPI))

ENVHEALTH = EPI_data[!is.na(EPI_data$ENVHEALTH),]
#Grabbing only non NA's and non "NA" rows . Converting from char to numeric type
ENVHEALTH = ENVHEALTH[ENVHEALTH$ENVHEALTH != "NA", "ENVHEALTH"]
ENVHEALTH <- transform(ENVHEALTH, ENVHEALTH = as.numeric(ENVHEALTH))

ECOSYSTEM = EPI_data[!is.na(EPI_data$ECOSYSTEM),]
#Grabbing only non NA's and non "NA" rows . Converting from char to numeric type
ECOSYSTEM = ECOSYSTEM[ECOSYSTEM$ECOSYSTEM != "NA", "ECOSYSTEM"]
ECOSYSTEM <- transform(ECOSYSTEM, ECOSYSTEM = as.numeric(ECOSYSTEM))

DALY = EPI_data[!is.na(EPI_data$DALY),]
#Grabbing only non NA's and non "NA" rows . Converting from char to numeric type
DALY = DALY[DALY$DALY != "NA", "DALY"]
DALY <- transform(DALY, DALY = as.numeric(DALY))

AIR_E = EPI_data[!is.na(EPI_data$AIR_E),]
#Grabbing only non NA's and non "NA" rows . Converting from char to numeric type
AIR_E = AIR_E[AIR_E$AIR_E != "NA", "AIR_E"]
AIR_E <- transform(AIR_E, AIR_E = as.numeric(AIR_E))

WATER_E = EPI_data[!is.na(EPI_data$WATER_E),]
#Grabbing only non NA's and non "NA" rows . Converting from char to numeric type
WATER_E = WATER_E[WATER_E$WATER_E != "NA", "WATER_E"]
WATER_E <- transform(WATER_E, WATER_E = as.numeric(WATER_E))

BIODIVERSITY = EPI_data[!is.na(EPI_data$BIODIVERSITY),]
#Grabbing only non NA's and non "NA" rows . Converting from char to numeric type
BIODIVERSITY = BIODIVERSITY[BIODIVERSITY$BIODIVERSITY != "NA", "BIODIVERSITY"]
BIODIVERSITY <- transform(BIODIVERSITY, BIODIVERSITY = as.numeric(BIODIVERSITY))

par(mar=c(7,1,1,1), oma=c(1,1,1,1))
boxplot(EPI$EPI, ENVHEALTH$ENVHEALTH, ECOSYSTEM$ECOSYSTEM, DALY$DALY, AIR_E$AIR_E, EPI_AIR_H_na_free$AIR_H, WATER_E$WATER_E, EPI_WATER_H_na_free$WATER_H, BIODIVERSITY$BIODIVERSITY
              , names= c("EPI",
                         "ENVHEALTH", "ECOSYSTEM", "DALY", "AIR_E", "AIR_H",
                         "WATER_E", "WATER_H",
                         "BIODIVERSITY"), las=2)

par(pty="s")
#Quantile-Quantile plot for EPI against ENVHEALTH
qqplot(ENVHEALTH$ENVHEALTH, EPI$EPI, main="qqplot(EPI, ENVHEALTH)", xlab = "ENVHEALTH",
       ylab = "EPI")
par(pty="s")
#Quantile-Quantile plot for WATER_H against AIR_H
qqplot(AIR_E$AIR_E, BIODIVERSITY$BIODIVERSITY, main="qqplot(BIODIVERSITY, AIR_E)", xlab = "AIR_E",
       ylab = "BIODIVERSITY")

#--------------------EXERCISE 2------------------------------
#unique(EPI_data["EPI_regions"])
#unique(EPI_data["GEO_subregion"])
hist(EPI_WATER_H_na_free[EPI_WATER_H_na_free$EPI_regions == "Sub-Saharan Africa", "WATER_H"], main="Sub-Saharan Africa: WATER_H", xlab="WATER_H", prob=TRUE)
lines(density(EPI_WATER_H_na_free[EPI_WATER_H_na_free$EPI_regions == "Sub-Saharan Africa", "WATER_H"], bw=5, kernel="gaussian"))
hist(EPI_WATER_H_na_free[EPI_WATER_H_na_free$GEO_subregion == "Southern Africa", "WATER_H"], main="Southern Africa: WATER_H", xlab="WATER_H", prob=TRUE)
lines(density(EPI_WATER_H_na_free[EPI_WATER_H_na_free$GEO_subregion == "Southern Africa", "WATER_H"], bw=5, kernel="gaussian"))


