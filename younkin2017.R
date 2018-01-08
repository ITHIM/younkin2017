## ----lib, echo = FALSE, eval = TRUE, results = "hide", warning = FALSE, message = FALSE----
rm(list=ls())
opts_knit$set(self.contained=FALSE)
library("devtools")
library("xtable")
install_github("ITHIM/ITHIM", ref="ithimus")
#install("~/ITHIM/")
library("ITHIM")
NHTS.df <- readRDS(file = "~/GHI/R/data/NHTS.df.rds")
#example("ITHIM")







## ----pATbyMSA, echo = FALSE, eval = TRUE, results = "hide", warning = FALSE, dev=c('pdf', 'png'), message = FALSE, fig.height = 6----
D.pAT <- NHTS.df %>% filter(!(location %in% c("-1", NA))) %>% group_by(id, location) %>% summarise(AT = first(nwalktrp)+first(nbiketrp)) %>% ungroup() %>% filter(!is.na(AT)) %>% mutate( AT.level = ifelse(AT == 0, "0 trips", ifelse(AT < 5, "< 5 trips", "> 5 trips"))) %>% group_by(location, AT.level) %>% summarise( count = n()) %>% ungroup()

D.pAT <- within(D.pAT, AT.level <- factor(AT.level, levels = rev(c("0 trips", "< 5 trips", "> 5 trips"))))

D.total <- D.pAT %>% group_by(location) %>% summarise(total = sum(count))

D.pAT <- full_join(D.pAT,D.total,by="location") %>% mutate(prop = count/total) %>% arrange(desc(AT.level),count)

msaIDFile <- read.csv(system.file("join2.csv", package = "ITHIM"), stringsAsFactors = FALSE)
msaIDFile %>% select(msaName, msaNHTS) %>% filter(!is.na(msaNHTS)) %>% distinct(msaName, msaNHTS)

bar <- msaIDFile %>% select(msaName, msaNHTS) %>% filter(!is.na(msaNHTS)) %>% group_by(msaNHTS) %>% summarise(msaName = first(msaName))
bar <- within(bar, msaNHTS <- factor(msaNHTS))

D.pAT <- full_join(D.pAT, bar, by = c("location" = "msaNHTS"))
D.pAT <- subset(D.pAT, !is.na(AT.level))

msaOrder <- D.pAT %>% filter(AT.level == "0 trips") %>% arrange(desc(prop)) %>% select(location) %>% unlist()
msaNameVec <- bar$msaName
msaIDVec <- bar$msaNHTS
names(msaNameVec) <- msaIDVec

D.pAT <- within(D.pAT, {
    msaName <- factor(msaName, levels = msaNameVec[msaOrder])
    })

ggplot(D.pAT, aes(x = msaName, y = prop, fill = AT.level)) + geom_bar(stat = "identity") + theme_bw() + labs(x = "Metropolitan Statistical Area (MSA)", y = "Proportion", fill = "Weekly Walk/Bike Trips") + coord_flip() + theme(legend.position = "bottom") #+ scale_fill_manual(labels = rev(c("0 trips", "< 5 trips", ">=5 trips")), values = rev(c("red", "yellow","green")))


## ----ohas1, echo = FALSE, eval = TRUE, results = "hide", warning = FALSE, message = FALSE, fig.height = 3----
OHAS.df <- readRDS(file = "~/Bullitt/data/OHAS/OHAS.df.rds")
countyList <- c("Benton","Clackamas","Deschutes","Jackson","Josephine","Lane","Linn","Marion","Multnomah","Polk","Washington")
OHAS.df <- OHAS.df %>% filter(COUNTYNAME %in% countyList)
OHAS.df <- within(OHAS.df, COUNTYNAME <- factor(COUNTYNAME, levels = countyList))





## ----allCounties, eval = TRUE, echo = FALSE, results = "hide", warning = FALSE, error = TRUE, message = TRUE, fig.height = 3----
alpha_w <- 3; alpha_c <- 4.5; nAT <- 3; fAT <- 1/nAT;

OHAS.df <- OHAS.df %>% mutate(ID = paste0(SAMPN, "-", PERNO)) %>% mutate(AGECLASS = ITHIM:::convertToAgeClass(AGE))
trip.df <-  OHAS.df %>% filter( !(TPURP %in% c("20", "96"))) %>% complete(ID,MODE) %>% filter(MODE %in% c("walk","cycle") & !is.na(MODE)) %>% select(ID, MODE, TPURP, TRPDUR)
subject.df <- OHAS.df %>% select(ID, GEND, AGECLASS) %>% distinct()

OHAS.full <- full_join(trip.df, subject.df, by = "ID", suffix = c(".trip",".subject")) %>% filter(MODE %in% c("walk","cycle")) %>% group_by(ID, MODE, GEND, AGECLASS) %>% summarise(TOTDUR = sum(TRPDUR)) %>% ungroup() %>% spread(MODE,TOTDUR) %>% mutate(TA = 7/60*(alpha_w*ifelse(is.na(walk),0,walk) + alpha_c*ifelse(is.na(cycle),0,cycle)))

OHAS.full <- filter(OHAS.full, !(AGECLASS %in% c("00-04","05-14")))
OHAS.full <- within(OHAS.full, AGECLASS <- factor(AGECLASS, levels = c("15-29", "30-44", "45-59", "60-69", "70-79", "80+")))

parameters.TA <- OHAS.full %>% filter(TA > 0 & !is.na(TA)) %>% mutate(logTA = log(TA)) %>% group_by(GEND, AGECLASS) %>% summarise( meanLogTA = mean(logTA), sdLogTA = sd(logTA))

## ----inactivePortland, eval = TRUE, echo = FALSE, results = "hide", warning = FALSE, error = TRUE, message = TRUE, fig.height = 3----
pAT <- OHAS.full %>% group_by(GEND, AGECLASS) %>% summarise( pAT = 1/fAT*sum(TA > 0, na.rm = TRUE)/length(TA))
pAT <- within(pAT, AGECLASS <- factor(AGECLASS))
pAT <- within(pAT, GEND <- factor(GEND))
pAT %>% ggplot(aes(x = GEND, y = pAT, fill = AGECLASS)) + geom_bar(stat = "identity", position = "dodge") + theme_bw() + labs(x = "Sex", y = "Proportion of Active Travelers", fill = "Age")

## ----walkBike, eval = TRUE, echo = FALSE, results = "hide", warning = FALSE, error = TRUE, message = TRUE, fig.height = 3----
OHAS.full %>% filter(TA > 0 & !is.na(TA)) %>% gather("mode", "totDur",4:5) %>% mutate(totDur = ifelse(is.na(totDur),0,totDur)) %>% ggplot(., aes(x=GEND, y = totDur, fill = AGECLASS) ) + geom_boxplot()  + facet_grid(. ~ mode) + theme_bw()  + labs(x = "Sex", y = "Reported One-Day Travel Time", fill = "Age") + scale_y_continuous(trans = "log", breaks = breaks <- exp(seq(0,8, by = 1)), labels = round(breaks,0))

## ----walkBikeAll, eval = TRUE, echo = FALSE, results = "hide", warning = FALSE, error = TRUE, message = TRUE, fig.height = 3----

parameters.TA.All <- OHAS.full %>% group_by(GEND, AGECLASS) %>% mutate(walk = ifelse(is.na(walk),0,walk), cycle = ifelse(is.na(cycle),0,cycle) ) %>% summarise( meanWalk = 7*mean(walk), meanCycle = 7*mean(cycle)) %>% ungroup()

foo <- parameters.TA.All %>% gather("mode","value",3:4)

foo <- within(foo, {
    mode <- ifelse(mode=="meanWalk","walk",ifelse( mode == "meanCycle", "cycle",NA))
    sex <- factor(GEND, levels = c("M","F"))
    ageClass <- factor(AGECLASS, levels = c("00-04", "05-14", "15-29", "30-44", "45-59", "60-69", "70-79", "80+"))
})

foo <- complete(foo, ageClass,sex,mode) %>% select(ageClass, sex, mode, value)

foo <- within(foo, {
    ageClass <- ifelse( ageClass == "00-04", "ageClass1",
                       ifelse( ageClass == "05-14", "ageClass2",
                              ifelse( ageClass == "15-29", "ageClass3",
                                     ifelse( ageClass == "30-44", "ageClass4",
                                            ifelse( ageClass == "45-59", "ageClass5",
                                                   ifelse( ageClass == "60-69", "ageClass6",
                                                          ifelse( ageClass == "70-79", "ageClass7",
                                                                 ifelse( ageClass == "80+", "ageClass8",
                                                                        NA))))))))
})

write.csv(foo, file = "~/ITHIM/inst/activeTravelOHAS.csv", quote = FALSE, row.names = FALSE)

#OHAS.full %>% gather("mode", "totDur",4:5) %>% mutate(totDur = ifelse(is.na(totDur),0,totDur)) %>% ggplot(., aes(x=GEND, y = totDur, fill = AGECLASS) ) + geom_boxplot()  + facet_grid(. ~ mode) + theme_bw()  + labs(x = "Sex", y = "Reported One-Day Travel Time", fill = "Age") #+ scale_y_continuous(trans = "log", breaks = breaks <- exp(seq(0,8, by = 1)), labels = round(breaks,0))

## ----allCountiesPlot, eval = TRUE, echo = FALSE, results = "hide", warning = FALSE, error = TRUE, message = TRUE, fig.height = 3----
ggplot(subset(OHAS.full, TA > 0 & !is.na(TA)), aes(x = GEND, y = TA, fill = AGECLASS)) + geom_boxplot() + theme_bw() + labs(x = "Sex", y = "Travel Activity (natural log scale) (MET-hrs./week)", fill = "Age")  + scale_y_continuous(trans = "log", breaks = breaks <- exp(seq(0,8, by = 1)), labels = round(breaks,0))



## ----atus1, echo = FALSE, eval = TRUE, warning = FALSE, message = FALSE, fig.height = 3, dev=c('pdf', 'png')----
newMETsFile <-'https://raw.githubusercontent.com/vargovargo/ACS_ITHIM/master/ATUS_to_METS_key.csv'

newMETs <- read.csv(file = newMETsFile, stringsAsFactors = FALSE, colClasses = c("character","character","character","character","numeric","character","character","character"))  %>%
  filter(X6.digit.activity.code >= 13000 & X6.digit.activity.code < 14000)
dataColumns <- c(1,5)
D <- newMETs[,dataColumns]
metaData <- newMETs[,-dataColumns]
newMETs <- list(Data = D, metaData = metaData)
activityCode <- ifelse(nchar(newMETs$Data$X6.digit.activity.code)==5,paste0("t0",newMETs$Data$X6.digit.activity.code),paste0("t",newMETs$Data$X6.digit.activity.code))

rownames(newMETs$Data) <- activityCode
rownames(newMETs$metaData) <- activityCode

newMETs$metaData <- within(newMETs$metaData, {
  Travel.related..Y.N. <- as.factor(Travel.related..Y.N.)
})

metKey <- newMETs$Data$AinsworthMETS
names(metKey) <- activityCode

readATUS1yr <- function(year, stateFIPS, metroCode, metKey){

  cpsFile <- paste0("./../data/atuscps_", year, ".dat")

  cps <- read.table(cpsFile, header=T, skip=0, sep="," ) %>%
           within(., {
             TUCASEID <- as.character(TUCASEID)
             TULINENO <- as.character(TULINENO)
             GEREG <- as.factor(GEREG)
             GESTFIPS <- as.numeric(as.character(GESTFIPS))
             GTMETSTA <- as.numeric(as.character(GTMETSTA))
           }) %>%
           filter(TULINENO == "1", GTMETSTA == metroCode, GESTFIPS == stateFIPS)


  summaryFile <- paste0("./../data/atussum_", year, ".dat")
  summary <-
    read.table(summaryFile,
               header = T,
               skip = 0,
               sep = ",") %>% filter(GTMETSTA == 1)
  metaDataColumns <- 1:24
  summaryList <-
    list(Data = summary[, -metaDataColumns], metaData = summary[, metaDataColumns])
  names(summaryList$metaData) <- c("TUCASEID","TUFINLWGT","TRYHHCHILD","TEAGE","TESEX","PEEDUCA","PTDTRACE","PEHSPNON", "GTMETSTA", "TELFS","TEMJOT", "TRDPFTPT","TESCHENR",
                                   "TESCHLVL","TRSPPRES", "TESPEMPNOT","TRERNWA", "TRCHILDNUM","TRSPFTPT","TEHRUSLT", "TUDIARYDAY","TRHOLIDAY", "t010101","t010102")

  summaryList$metaData <- within(summaryList$metaData, {
    TESEX <- as.factor(ifelse(TESEX == 1, "male", ifelse(TESEX == 2, "female", NA)))
    AGECAT <- ifelse(TEAGE < 5, "00-04",
                     ifelse(
                       TEAGE > 4 & TEAGE < 15,
                       "05-14",
                       ifelse(
                         TEAGE > 14 & TEAGE < 30,
                         "15-29",
                         ifelse(
                           TEAGE > 29 & TEAGE < 45,
                           "30-44",
                           ifelse(
                             TEAGE > 44 & TEAGE < 60,
                             "45-59",
                             ifelse(
                               TEAGE > 59 & TEAGE < 70,
                               "60-69",
                               ifelse(TEAGE > 69 &
                                        TEAGE < 80, "70-79", "80+")
                             )
                           )
                         )
                       )
                     ))
  })


  allActivities <- colnames(summaryList$Data)
  nonTravelActivities <-
    rownames(newMETs$metaData)[which(newMETs$metaData$Travel.related..Y.N. == "N")]
  nonTravelActivities <-
    nonTravelActivities[which(nonTravelActivities %in% allActivities)]

  activities <- nonTravelActivities

  metKeyVector <-
    (metKey[activities] - min(metKey, na.rm = TRUE)) / 60 * 7
  names(metKeyVector) <- activities
  metKeyMatrix <-
    matrix(
      metKeyVector,
      nrow = nrow(summaryList$Data),
      ncol = length(metKeyVector),
      byrow = TRUE
    )
  dimnames(metKeyMatrix) <- list(NULL, activities)


  METs <-
    data.frame(
      METs = rowSums(summaryList$Data[, activities] * metKeyMatrix[, activities], na.rm = TRUE),
      AGE = summaryList$metaData$TEAGE,
      SEX = summaryList$metaData$TESEX,
      AGECAT = summaryList$metaData$AGECAT,
      row.names = summaryList$metaData$TUCASEID

    )

  singleYear <- cps %>% merge(METs, by.y = "row.names", by.x = "TUCASEID")


  return(singleYear)

}

foo <- readATUS1yr(year = 2015, stateFIPS = 41,metroCode = 1, metKey = metKey)

# requires downloading additional years of data from ATUS
# we could add a few line to unzip the files at these paths
# paste0("https://www.bls.gov/tus/special.requests/atuscps_",year,".zip")
# paste0("https://www.bls.gov/tus/special.requests/atussum_",year,".zip")
# and opening the file with pattern="\.dat$"


fiveYear <- bind_rows(readATUS1yr(year = 2011, stateFIPS = 41, metroCode = 1, metKey = metKey),
                      readATUS1yr(year = 2012, stateFIPS = 41, metroCode = 1, metKey = metKey),
                      readATUS1yr(year = 2013, stateFIPS = 41, metroCode = 1, metKey = metKey),
                      readATUS1yr(year = 2014, stateFIPS = 41, metroCode = 1, metKey = metKey),
                      readATUS1yr(year = 2015, stateFIPS = 41, metroCode = 1, metKey = metKey)
                      )


# fiveYear  %>% ggplot(aes(x = METs, ..density.., colour = AGECAT)) + geom_freqpoly(binwidth = 25) + facet_grid( SEX ~ .) + labs(x = "Non-Travel Marginal MET-hrs./week", colour = "Age") + coord_cartesian(xlim = c(0, 250))


ggplot(fiveYear, aes(x = SEX, y = METs, fill = AGECAT)) + geom_boxplot() + theme_bw() + labs(x = "Sex", y = "Leisure Activity (natural log scale) (MET-hrs./week", fill = "Age") + scale_y_continuous(trans = "log", breaks = breaks <- exp(seq(0,8, by = 1)), labels = round(breaks,0))

parameters.LA <- fiveYear %>% mutate(logLA = ifelse(METs == 0, log(min(METs[METs!=0])), log(METs))) %>% group_by(SEX, AGECAT) %>% summarise( meanLogLA = mean(logLA,na.rm = TRUE), sdLogLA = sd(logLA, na.rm = TRUE))

parameters.LA <- with(parameters.LA, data.frame(GEND = ifelse(SEX == "female","F", ifelse(SEX == "male", "M", NA)), AGECLASS = AGECAT, meanLogLA, sdLogLA))

parameters.LA.untransformed <- fiveYear %>%  group_by(SEX, AGECAT) %>% summarise( meanMETs = mean(METs,na.rm = TRUE), sdMETs = sd(METs, na.rm = TRUE))

muNonTravelMatrix <- rbind(rep(0.1,2), rep(0.1,2), matrix(parameters.LA.untransformed$meanMETs/parameters.LA.untransformed$meanMETs[1], ncol = 2))
dimnames(muNonTravelMatrix) <- list(paste0("ageClass",1:8),c("F","M"))

muNonTravelMatrix <- muNonTravelMatrix[,2:1]
saveRDS(muNonTravelMatrix, file = "./../data/muNonTravelMatrix.portland.rds")
write.csv(muNonTravelMatrix, file = "./../data/muNonTravelMatrix.csv", quote = FALSE, row.names = FALSE)

parameters.LA.untransformed.ungrouped <- fiveYear %>%  summarise( meanMETs = mean(METs,na.rm = TRUE), sdMETs = sd(METs, na.rm = TRUE))
saveRDS(parameters.LA.untransformed.ungrouped, file = "./../data/parameters.LA.untransformed.ungrouped.rds")
write.csv(parameters.LA.untransformed.ungrouped, file = "./../data/parameters.LA.untransformed.ungrouped.csv", quote = FALSE, row.names = FALSE)
# Note the arbitrary numbers inserted here for bottom two age classes.
#parameters.LA <- rbind(data.frame(GEND = c("F","F","M","M"), AGECLASS = rep(c("00-04", "05-14"),2),meanLogLA = 4, sdLogLA = 0.5), parameters.LA) %>% arrange(GEND, AGECLASS)

parameters.LA <- within(parameters.LA, GEND <- factor(GEND, levels = c("M","F")))
saveRDS(parameters.LA, "./../data/parameters.LA.rds")
write.csv(parameters.LA, file = "./../data/parameters.LA.csv", quote = FALSE, row.names = FALSE)

#ggplot(subset(D, TA > 0), aes(x = GEND, y = TA, fill = AGECLASS)) + geom_boxplot() + ylab("Travel Activity (MET-hrs./week)")


#parameters <- fiveYear %>%  group_by(SEX, AGECAT) %>% summarise(mean = mean(METs, na.rm=T), sd = sd(METs, na.rm=T))

#METmatrix <- rbind(matrix(0,nrow = 2, ncol = 2), matrix(parameters$mean, ncol = 2))

#colnames(METmatrix) <- c(as.character(parameters$SEX[1]),as.character(rev(parameters$SEX)[1]))

#METmatrix <- METmatrix[,c("male","female")]

#METmatrix <- METmatrix/METmatrix[3,2]
#METmatrix[2,] <- METmatrix[3,]
#rownames(METmatrix) <- paste0("ageClass",1:8)
#colnames(METmatrix) <- c("M","F")
#METmatrix
#parameters


## ----burden, eval = TRUE, echo = FALSE, results = "hide", warning = TRUE, error = TRUE, message = TRUE, fig.height = 6----
#burden <- read.csv(file = "./../data/gbd_20170925.csv")
burden <- read.csv(file = "./../data/gbd_Manuscript_2011-2015.csv")
ageBins <- c("00-04", "05-14", "15-29", "30-44", "45-59", "60-69", "70-79", "80+")
names(ageBins) <- paste0("ageClass",1:8)
burden <- burden %>% filter(disease != "RTIs")

burden <- within(burden, {
    AGECLASS <- factor(as.character(ageBins[ageClass]))
    GEND <- factor(sex, levels = c("M","F"))
    disease <- factor(disease, levels = c("BreastCancer","ColonCancer","CVD","Dementia","Depression","Diabetes"))
    burdenType <- factor(burdenType, levels = c("daly","yll","yld","deaths"))
})

burden <- filter(burden, !(AGECLASS %in% c("00-04", "05-14")))
burden <- within(burden, AGECLASS <- factor(AGECLASS, levels = c("15-29", "30-44", "45-59", "60-69", "70-79", "80+")))


## ----parameters, echo = FALSE, eval = TRUE, results = "hide", warning = FALSE, message = FALSE, fig.height = 3----
parameters.LA <- readRDS(file = "./../data/parameters.LA.rds")
doseResponse <- read.csv(file = "./../data/dose-response.csv", header = TRUE)
doseResponse <- filter(doseResponse, !(AGECLASS %in% c("00-04", "05-14")))
doseResponse <- within(doseResponse, {
    GEND <- factor(GEND, levels = c("M","F"))
    AGECLASS <- factor(AGECLASS, levels = c("15-29", "30-44", "45-59", "60-69", "70-79", "80+"))
})

parameters <- inner_join(burden, inner_join(inner_join(pAT, inner_join(parameters.TA, parameters.LA)), doseResponse))

## ----baseline, echo = FALSE, eval = TRUE, results = "hide", warning = FALSE, message = FALSE, fig.height = 3----
pSetList <- list()

for( i in 1:nrow(parameters) ){

  pSetList[[i]] <- with(parameters,{
      new("ParameterSet",
          pAT = pAT[i],
          meanLogTA = meanLogTA[i],
          meanLogLA = meanLogLA[i],
          sdLogTA = sdLogTA[i],
          sdLogLA = sdLogLA[i],
          metWalk = alpha_w,
          metCycle = alpha_c,
          alpha = log(RR1MET)[i],
          k = k[i],
          quantiles = 1:99/100,
          label = as.character(i),
          disease = disease[i],
          sex = GEND[i],
          age = AGECLASS[i],
          burdenType = burdenType[i],
          burdenValue = value)
  })
}

portlandITHIM.baseline <- new("ITHIM", pSetList)

## ----scenario, echo = FALSE, eval = TRUE, results = "hide", warning = FALSE, message = FALSE, fig.height = 3----
pAT.scenario <- 0.75

pSetList <- list()

for( i in 1:nrow(parameters) ){

  pSetList[[i]] <- with(parameters,{
      new("ParameterSet",
          pAT = ifelse(pAT[i] < pAT.scenario, pAT.scenario, pAT[i]),
          meanLogTA = meanLogTA[i],
          meanLogLA = meanLogLA[i],
          sdLogTA = sdLogTA[i],
          sdLogLA = sdLogLA[i],
          metWalk = alpha_w,
          metCycle = alpha_c,
          alpha = log(RR1MET)[i],
          k = k[i],
          quantiles = 1:99/100,
          label = as.character(i),
          disease = disease[i],
          sex = GEND[i],
          age = AGECLASS[i],
          burdenType = burdenType[i],
          burdenValue = value)
  })
}

portlandITHIM.scenario <- new("ITHIM", pSetList)

## ----healthOutcomes, echo = FALSE, eval = TRUE, results = "hide", warning = FALSE, message = FALSE, fig.height = 3----
PAF <- mapply(computeAF, portlandITHIM.baseline, portlandITHIM.scenario)
healthOutcomes <- parameters %>% mutate(deltaBurden = PAF*value) %>% group_by(disease, burdenType) %>% summarise(diseaseDeltaBurden = sum(deltaBurden))
ggplot(healthOutcomes, aes(x = "", y = diseaseDeltaBurden, fill = burdenType)) + geom_bar(stat = "identity", position = "dodge")  + facet_grid(. ~ disease) + theme_bw()  + labs(x = "", y = "Disease Burden Averted", fill = "Burden Type")

## ----healthOutcomes2, echo = FALSE, eval = TRUE, results = "hide", warning = FALSE, message = FALSE, fig.height = 3----
results <- c()
for(pAT.scenario in seq(0.2, 1, length.out = 2)){

    pSetList <- list()

for( i in 1:nrow(parameters) ){

  pSetList[[i]] <- with(parameters,{
      new("ParameterSet",
          pAT = ifelse(pAT[i] < pAT.scenario, pAT.scenario, pAT[i]),
          meanLogTA = meanLogTA[i],
          meanLogLA = meanLogLA[i],
          sdLogTA = sdLogTA[i],
          sdLogLA = sdLogLA[i],
          metWalk = alpha_w,
          metCycle = alpha_c,
          alpha = log(RR1MET)[i],
          k = k[i],
          quantiles = 1:99/100,
          label = as.character(i),
          disease = disease[i],
          sex = GEND[i],
          age = AGECLASS[i],
          burdenType = burdenType[i],
          burdenValue = value)
  })
}

portlandITHIM.scenario <- new("ITHIM", pSetList)

PAF <- mapply(computeAF, portlandITHIM.baseline, portlandITHIM.scenario)
healthOutcomes <- parameters %>% mutate(deltaBurden = PAF*value) %>% group_by(burdenType) %>% summarise(diseaseDeltaBurden = sum(deltaBurden))

results <- rbind(results,cbind(pAT.scenario, healthOutcomes))


}

ggplot(results, aes(x = pAT.scenario, y = diseaseDeltaBurden, color = burdenType)) + geom_line() + theme_bw()  + labs(x = "Minimum Active Travel Participation", y = "Disease Burden Averted", color = "Burden Type")


## ----healthOutcomes3, echo = FALSE, eval = TRUE, results = "hide", warning = FALSE, message = FALSE, fig.height = 3----
results <- c()
for(gamma in seq(1, 10, length.out = 2)){

    pSetList <- list()

for( i in 1:nrow(parameters) ){

  pSetList[[i]] <- with(parameters,{
      new("ParameterSet",
          pAT = pAT[i],
          meanLogTA = log(gamma) + meanLogTA[i],
          meanLogLA = meanLogLA[i],
          sdLogTA = sdLogTA[i],
          sdLogLA = sdLogLA[i],
          metWalk = alpha_w,
          metCycle = alpha_c,
          alpha = log(RR1MET)[i],
          k = k[i],
          quantiles = 1:99/100,
          label = as.character(i),
          disease = disease[i],
          sex = GEND[i],
          age = AGECLASS[i],
          burdenType = burdenType[i],
          burdenValue = value)
  })
}
portlandITHIM.scenario <- new("ITHIM", pSetList)
PAF <- mapply(computeAF, portlandITHIM.baseline, portlandITHIM.scenario)
healthOutcomes <- parameters %>% mutate(deltaBurden = PAF*value) %>% group_by(burdenType) %>% summarise(diseaseDeltaBurden = sum(deltaBurden))
results <- rbind(results,cbind(gamma, healthOutcomes))
}

ggplot(results, aes(x = gamma, y = diseaseDeltaBurden, color = burdenType)) + geom_line() + theme_bw()  + labs(x = "Proportional increase in travel activity", y = "Disease Burden Averted", color = "Burden Type")





## ----appendix, echo = FALSE, eval = TRUE, results = "asis", collapse = TRUE, warning = FALSE, message = FALSE, fig.height = 3----
doseResponseTable <- doseResponse %>% mutate(alpha = round(-log(RR1MET),4)) %>% select(disease,GEND,AGECLASS,alpha) %>% distinct %>% arrange(disease, GEND, AGECLASS)
doseResponseTable <- doseResponseTable %>% spread(AGECLASS, alpha)
xtable(doseResponseTable, label = "alphaTable", caption = "A table of values for the dose-response shape parameter")


