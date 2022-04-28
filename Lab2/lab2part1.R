library("readxl")
library("ggplot2")

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

EPI_DATA = read_xls(file.choose(), sheet="EPI2010_all countries")

#Preprocess to remove "NA"'s from features of interest
#Central Tendencies values

#Filtering out NA's and "NA"s
EPI = EPI_DATA[!is.na(EPI_DATA$EPI),"EPI"]
EPI = EPI[EPI$EPI != "NA",]
EPI = transform(EPI, EPI = as.numeric(EPI))
mean(EPI$EPI)
median(EPI$EPI)
getmode(EPI$EPI)

ggplot(EPI, aes(x=EPI, y=..density..)) + geom_histogram(binwidth=2) + geom_density(col="red")
ggplot(EPI, aes(x=EPI, y=..density..)) + geom_histogram(binwidth=4) + geom_density(col="red")
ggplot(EPI, aes(x=EPI, y=..density..)) + geom_histogram(binwidth=6) + geom_density(col="red")



#Filtering out NA's and "NA"s
DALY = EPI_DATA[!is.na(EPI_DATA$DALY), "DALY"]
DALY = DALY[DALY$DALY != "NA",]
DALY = transform(DALY, DALY = as.numeric(DALY))
mean(DALY$DALY)
median(DALY$DALY)
getmode(DALY$DALY)

ggplot(DALY, aes(x=DALY, y=..density..)) + geom_histogram(binwidth=2) + geom_density(col="red")
ggplot(DALY, aes(x=DALY, y=..density..)) + geom_histogram(binwidth=4) + geom_density(col="red")
ggplot(DALY, aes(x=DALY, y=..density..)) + geom_histogram(binwidth=6) + geom_density(col="red")


#Filtering out NA's and "NA"s
AIR_E = EPI_DATA[!is.na(EPI_DATA$AIR_E), "AIR_E"]
AIR_E = AIR_E[AIR_E$AIR_E != "NA",]
AIR_E = transform(AIR_E, AIR_E = as.numeric(AIR_E))
mean(AIR_E$AIR_E)
median(AIR_E$AIR_E)
getmode(AIR_E$AIR_E)


ggplot(AIR_E, aes(x=AIR_E, y=..density..)) + geom_histogram(binwidth=2) + geom_density(col="red")
ggplot(AIR_E, aes(x=AIR_E, y=..density..)) + geom_histogram(binwidth=4) + geom_density(col="red")
ggplot(AIR_E, aes(x=AIR_E, y=..density..)) + geom_histogram(binwidth=6) + geom_density(col="red")



CLIMATE = EPI_DATA[!is.na(EPI_DATA$CLIMATE), "CLIMATE"]
CLIMATE = CLIMATE[CLIMATE$CLIMATE != "NA",]
CLIMATE = transform(CLIMATE, CLIMATE = as.numeric(CLIMATE))
mean(CLIMATE$CLIMATE)
median(CLIMATE$CLIMATE)
getmode(CLIMATE$CLIMATE)

ggplot(CLIMATE, aes(x=CLIMATE, y=..density..)) + geom_histogram(binwidth=2) + geom_density(col="red")
ggplot(CLIMATE, aes(x=CLIMATE, y=..density..)) + geom_histogram(binwidth=4) + geom_density(col="red")
ggplot(CLIMATE, aes(x=CLIMATE, y=..density..)) + geom_histogram(binwidth=6) + geom_density(col="red")




boxplot(EPI$EPI, DALY$DALY, AIR_E$AIR_E, CLIMATE$CLIMATE, names=c("EPI", "DALY", "AIR_E", "CLIMATE"))

View(EPI_DATA)

#QQPlots
qqplot(EPI$EPI, DALY$DALY, xlab = "EPI", ylab = "DALY")
qqplot(AIR_E$AIR_E, CLIMATE$CLIMATE, xlab="AIR_E", ylab="CLIMATE")


EPI_DATA_subset = EPI_DATA[,c("EPI_regions", "EPI", "DALY", "AIR_E", "CLIMATE")]
EPI_DATA_subset = na.omit(EPI_DATA_subset[, c("EPI_regions", "EPI", "DALY", "AIR_E", "CLIMATE")])
EPI_DATA_subset = EPI_DATA_subset[EPI_DATA_subset$EPI != "NA" 
                                  & EPI_DATA_subset$DALY != "NA" 
                                  & EPI_DATA_subset$AIR_E != "NA"
                                  & EPI_DATA_subset$CLIMATE != "NA", ]

EPI_DATA_subset[c("EPI", "DALY", "AIR_E", "CLIMATE")] = sapply(EPI_DATA_subset[c("EPI", "DALY", "AIR_E", "CLIMATE")],as.numeric)
#Standard multivalue regression and checking coeffs values isn't the most accurate
#way of determing which predictor variable impacts response most
#Instead standardize the predictor variable data
EPI_subset_standardize = EPI_DATA_subset
EPI_subset_standardize[c("EPI", "DALY", "AIR_E", "CLIMATE")] = scale(EPI_subset_standardize[c("EPI", "DALY", "AIR_E", "CLIMATE")])

#For Sub-Saharan Africa
mm = lm(EPI~DALY+AIR_E+CLIMATE
        , EPI_subset_standardize[EPI_subset_standardize$EPI_regions == "Sub-Saharan Africa",])
coef(mm)
nrow(EPI_subset_standardize[EPI_subset_standardize$EPI_regions == "Sub-Saharan Africa",])
#DALY has highest influence


#For Eastern Europe and Central Asia
mm = lm(EPI~DALY+AIR_E+CLIMATE
        , EPI_subset_standardize[EPI_subset_standardize$EPI_regions == "Eastern Europe and Central Asia",])
coef(mm)
nrow(EPI_subset_standardize[EPI_subset_standardize$EPI_regions == "Eastern Europe and Central Asia",])
#DALY has highest influence


#For Middle East and North Africa
mm = lm(EPI~DALY+AIR_E+CLIMATE
        , EPI_subset_standardize[EPI_subset_standardize$EPI_regions == "Middle East and North Africa",])
coef(mm)
#DALY has highest influence
nrow(EPI_subset_standardize[EPI_subset_standardize$EPI_regions == "Middle East and North Africa",])
#DALY has highest influence

#For North America
mm = lm(EPI~DALY+AIR_E+CLIMATE
        , EPI_subset_standardize[EPI_subset_standardize$EPI_regions == "North America",])
coef(mm)
nrow(EPI_subset_standardize[EPI_subset_standardize$EPI_regions == "North America",])
#Daly has highest influence, but North America isn't very convincing since only
#2 rows correspond to North America