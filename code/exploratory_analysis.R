##===  Data Exploration ====

##=== ! SOS ! ======

# Please open the dataset manually. Go to Rstudio Environment ->  Import Dataset -> From Text..
# That's because the version is controlled

#TOC> =========================================================================================
#TOC>
#TOC>   Section  Title                                            Line
#TOC> -----------------------------------------------------------------------------------------
#TOC>   1        Packages
#TOC>   2        Missing data exploration
#TOC>   3        Manipulate the dataset
#TOC>   4        Growth dataset
#TOC>   5        Clinical dataset
#TOC>   6        Compute descriptive statistics
#TOC> =========================================================================================

# ====    1  Packages  ========================================================================
list.of.packages <-
  c(
    "ggplot2",
    "lubridate",
    "dplyr",
    "plyr",
    "devtools",
    "eeptools",
    "ggpubr",
    'pastecs',
    "factoextra",
    "naniar",
    "FactoMineR",
    "missMDA",
    "VIM",
    "flipTime",
    "zscorer"
  )

# list any missing packages
new.packages <-
  list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
# if packages missing --> install
if (length(new.packages) > 0) {
  install.packages(new.packages, dependencies = TRUE)
}
# load all packages
lapply(list.of.packages, require, character.only = TRUE)


data <- read.csv("F:/Dhaka/Dataset_2.csv")

# ===== 2 Missing data exploration =============================================================

missing_data <-
  data.frame(column = colnames(data),
             prc_value = round(colSums(is.na(data)) / dim(data)[1], digits = 3) * 100, stringsAsFactors = F)

# Plot % missing data per index (column)

plot(round(colSums(is.na(data)) / dim(data)[1], digits = 2) * 100)

# Which columns have more than 50% missing data?

high_rate <- missing_data[which(missing_data$prc_value >= 50), ]
rownames(high_rate)

# Which columns have more than 20% missing data?

high_rate20 <- missing_data[which(missing_data$prc_value >= 20),]
rownames(high_rate20)

# Which columns have less than 50% missing data?

low_rate <- missing_data[which(missing_data$prc_value < 50),]
rownames(low_rate)

# ====== 3 Manipulate the dataset ========================================================================

# Convert date of registration and outcome age

data$Date1<-ifelse(data$Date1=="", NA, data$Date1)
data$Outcome_Date<-ifelse(data$Outcome_Date=="", NA, data$Outcome_Date)

data <-
  data %>% mutate(mut_date1 = AsDate(data$Date1)) %>% mutate(mut_out_date = AsDate(data$Outcome_Date))

## Create a new variable – Duration of hospital stay – from Date1 and Outcome_Date

data <-
  data %>% mutate(hospital_stay = as.numeric(mut_out_date - mut_date1)) # hospital_stay in days

## Calculate weight gain (Discharged_Weight - Weight__kg_1)

data <-
  data %>% mutate(weight_gain = as.numeric(Discharged_Weight - Weight__kg_1))

# Convert character variables to numeric ("Yes" = 1, "No" = 0)

data<-data.frame(lapply(data, function(x) {gsub("\\<Yes\\>", "1", x)}), stringsAsFactors = F)
data<-data.frame(lapply(data, function(x) {gsub("\\<No\\>", "0", x)}), stringsAsFactors = F)

# Convert character variables to numeric ("Male" = 1, "Female" = 2)

data<-data.frame(lapply(data, function(x) {gsub("\\<Male\\>", "1", x)}), stringsAsFactors = F)
data<-data.frame(lapply(data, function(x) {gsub("\\<Female\\>", "2", x)}), stringsAsFactors = F)
#data <- as.data.frame(data)

# ====== 4 Growth dataset ======================================================

# Let's keep only the anthropometric measurements and oedema that might
# have effect on weight for now

clinical <-
  c(
    "Patient_ID",
    "Sex",
    "Date_of_Birth",
    "Oedema",
    "Oedema2",
    "Oedema3",
    "Oedema4",
    "Oedema5",
    "Oedema6",
    "Date1",
    "Date2",
    "Date3",
    "Date4",
    "Date5",
    "Date6",
    "hospital_stay"
  )

anthro <-
  c(
    "Discharged_Weight",
    "Rate_weight_gain",
    "weight_gain",
    "Length__cm_1",
    "Length__cm_2",
    "Length__cm_3",
    "Length__cm_4",
    "Length__cm_5",
    "Length__cm_6",
    "Weight__kg_1",
    "Weight__kg_2",
    "Weight__kg_3",
    "Weight__kg_4",
    "Weight__kg_5",
    "Weight__kg_6",
    "MUAC__in_mm_1",
    "MUAC__in_mm_2",
    "MUAC__in_mm_3",
    "MUAC__in_mm_4",
    "MUAC__in_mm_5",
    "MUAC__in_mm_6"
  )

growth_list <- c(clinical, anthro)
growth_data <- data[, growth_list]

for (i in anthro){
  growth_data[,i] <- as.numeric(growth_data[,i])
}

# Calculate wfl z scores for all time points

growth <-
  addWGSR(
    data = growth_data,
    sex = "Sex",
    firstPart = "Weight__kg_1",
    secondPart = "Length__cm_1",
    index = "wfl",
    output = "wflz_1"
  )

growth <-
  addWGSR(
    data = growth_data,
    sex = "Sex",
    firstPart = "Weight__kg_2",
    secondPart = "Length__cm_2",
    index = "wfl",
    output = "wflz_2"
  )

growth <-
  addWGSR(
    data = growth_data,
    sex = "Sex",
    firstPart = "Weight__kg_3",
    secondPart = "Length__cm_3",
    index = "wfl",
    output = "wflz_3"
  )

growth <-
  addWGSR(
    data = growth_data,
    sex = "Sex",
    firstPart = "Weight__kg_4",
    secondPart = "Length__cm_4",
    index = "wfl",
    output = "wflz_4"
  )

growth <-
  addWGSR(
    data = growth_data,
    sex = "Sex",
    firstPart = "Weight__kg_5",
    secondPart = "Length__cm_5",
    index = "wfl",
    output = "wflz_5"
  )

growth <-
  addWGSR(
    data = growth_data,
    sex = "Sex",
    firstPart = "Weight__kg_6",
    secondPart = "Length__cm_6",
    index = "wfl",
    output = "wflz_6"
  )

# Convert date of birth and calculate age

growth <-
  mutate(growth, mut_date_birth = AsDate(growth$Date_of_Birth))
as.Date(growth$mut_date_birth)
growth <-
  mutate(growth,
         age = age_calc(growth$mut_date_birth, units = 'months'))


# Check again the missing data in the growth dataset

na_freq <-
  data.frame(
    col = character(ncol(growth_data)),
    freq = numeric(ncol(growth_data)),
    stringsAsFactors = F
  )
for (i in 1:ncol(growth_data)) {
  na_freq[i,] <-
    c(col = colnames(growth_data[i]), freq = as.numeric(sum(is.na(growth_data[, i]))))
  #print(paste0(colnames(growth_data[i]), ": ", sum(!is.na(growth_data[,i]))))
}

na_freq$freq <- as.numeric(na_freq$freq)

# Visualizations

ggbarplot(na_freq[c(12:17),], x = "col", y = "freq")

gg_miss_var(growth_data)

res <- summary(aggr(growth_data, sortVar = TRUE))$combinations

# Type casting 


growth_data$Sex<-as.factor(growth_data$Sex)
growth_data$Oedema<-as.factor(growth_data$Oedema)
growth_data$Discharged_Weight<-as.numeric(growth_data$Discharged_Weight)
growth_data$Length__cm_1<-as.numeric(growth_data$Length__cm_1)
growth_data$Weight__kg_1<-as.numeric(growth_data$Weight__kg_1)
growth_data$MUAC__in_mm_1<-as.numeric(growth_data$MUAC__in_mm_1)
growth_data$age<-as.numeric(growth_data$age)
growth<-mutate(growth_data, sex_bin=as.numeric(growth_data$Sex))



growth<-growth[,c(1,9,10,11)]
data<-full_join(growth, clinical_data, by="Patient_ID")
patients<-data$Patient_ID
rownames(data)<-patients
data<-data[,-1]
#data<-full_join(data,clinical_data, by="Patient_ID")
set.seed(100)
train <- sample(nrow(data), 0.75*nrow(data), replace = FALSE)
data_train <- data[train,]
data_test <- data[-train,]


dt<-rpart(wflz~., data = data_train)
rpart.plot(dt)


# ====== 5 Exploring Clinical and Biochemical datasets =========================
clinical <-
  c(
    "Patient_ID",
    "Diarrhoea_Exists",
    "Fever",
    "Night_Sweats",
    "Weight_Loss",
    "Breathlessness",
    "Weakness",
    "Loss_of_Apetite",
    "Cough",
    "Haemoptysis",
    "Vomiting",
    "Haematemesis",
    "Anorexia",
    "Rashes",
    "Odema"
  )

biochemical <-
  c(
    "Patient_ID",
    "Hematocrit_PCV",
    # "Diff_Leucocyte_Count",
    "Total_Leucocyte_Count",
    "Neutrophil",
    "Anion_Gap",
    "ESR",
    "Hemoglobin",
    "Potassium",
    "Sodium",
    "Chloride"
  )

# Diff_Leucocyte_Count has 100% Nas, so it will be excluded
clinical_list <- c(clinical, biochemical)

clinical_data <- data[, clinical_list]

clinical_short <- data[, clinical]
biochemical_short <- data[, biochemical]
patients<-biochemical_short[,1]
rownames(biochemical_short)<-patients
biochemical_short<-biochemical_short[,-1]
biochemical_short <- apply(biochemical_short, 2, as.numeric)

## Check for missing data in the clinical dataset

na_freq <-
  data.frame(
    col = character(ncol(clinical_data)),
    freq = numeric(ncol(clinical_data)),
    stringsAsFactors = F
  )
for (i in 1:ncol(clinical_data)) {
  na_freq[i,] <-
    c(col = colnames(clinical_data[i]), freq = as.numeric(sum(is.na(clinical_data[, i]))))
  #print(paste0(colnames(growth_data[i]), ": ", sum(!is.na(growth_data[,i]))))
}

na_freq$freq <- as.numeric(na_freq$freq)

# Visualizations

# Frequency of Nas per column
gg_miss_var(clinical_data)

# Number of missing entries in each variable and for certain combinations of variables
res <- summary(aggr(clinical_data, sortVar = TRUE))$combinations

# The points for which x (resp. y) is missing are represented in red along the y (resp. x) axis.
# In addition, boxplots of the x and y variables are represented along the axes
# with and without missing values (in red all variables x where y is missing, in blue all variables x where y is observed).

marginplot(clinical_data[, c("Fever", "ESR")])

# Dimensions reduction with PCA for incomplete data biochemical data
# Biochemical data are continuous so a separate analysis will be conducted  for
# the clinical data

nb <-
  estim_ncpPCA(biochemical_short , method.cv = "Kfold", verbose = FALSE)
nb$ncp
plot(0:5, nb$criterion, xlab = "nb dim", ylab = "MSEP")

biochemical_short_df <- as.data.frame(biochemical_short)
res.comp <-
  imputePCA(biochemical_short_df, ncp = 2) # iterativePCA algorithm
res.comp$completeObs[1:3, ] # the imputed data set

imp <- as.data.frame(res.comp$completeObs)

res.pca <- PCA(imp, ncp = 2, graph = FALSE)
plot(res.pca, lab = "quali")

plot(res.pca, choix = "var")

clinical_data2<-clinical_data[,-16]
clinical_data2<-clinical_data2[,-20]

clinical_data2<-mutate(clinical_data2, suspected_infection=character(nrow(clinical_data2)))
clinical_data2<-mutate(clinical_data2, suspected_electrolyte_imbalance=character(nrow(clinical_data2)))

for(i in 1:nrow(clinical_data2)) {
  if(!is.na(clinical_data2[i,]$Hematocrit_PCV) && !is.na(clinical_data2[i,]$Total_Leucocyte_Count) && !is.na(clinical_data2[i,]$Neutrophil)) {
    clinical_data2[i,]$suspected_infection<-"1"
  }
  else {
    clinical_data2[i,]$suspected_infection<-"0"
  }
  
  if(!is.na(clinical_data2[i,]$Potassium) && !is.na(clinical_data2[i,]$Sodium) && !is.na(clinical_data2[i,]$Chloride) && !is.na(clinical_data2[i,]$Anion_Gap)) {
    clinical_data2[i,]$suspected_electrolyte_imbalance<-"1"
  }
  else {
    clinical_data2[i,]$suspected_electrolyte_imbalance<-"0"
  }
}

cols<-c("Patient_ID", "Registration_Date", "Outcome_Date", "Discharged_Weight", "Weight__kg_1")

add_data<-data[,cols]

clinical_data3<-left_join(clinical_data2, add_data, by="Patient_ID")

clinical_data3$Discharged_Weight<-as.numeric(clinical_data3$Discharged_Weight)
clinical_data3$Weight__kg_1<-as.numeric(clinical_data3$Weight__kg_1)

clinical_data3<-mutate(clinical_data3, Duration_hospital_stay=as.numeric(AsDate(clinical_data3$Outcome_Date)-AsDate(clinical_data3$Registration_Date)))
clinical_data3<-mutate(clinical_data3, Weight_Gain=clinical_data3$Discharged_Weight-clinical_data3$Weight__kg_1)
clinical_data3<-mutate(clinical_data3, rate_weight_gain=(clinical_data3$Weight_Gain*1000)/clinical_data3$Duration_hospital_stay)


#====== 6 Compute descriptive statistics =================================================

#res <- stat.desc(growth_data[, -c(1:16, 37)])
#round(res, 2)

# biochemical_short_df<-mutate(biochemical_short_df, Patient_ID=patients)
# 
# growth<-growth_data[,c(1:3,5,17,18,24,30,37)]
# growth$Sex<-as.factor(growth$Sex)
# growth$Oedema<-as.factor(growth$Oedema)
# growth$Discharged_Weight<-as.numeric(growth$Discharged_Weight)
# growth$Length__cm_1<-as.numeric(growth$Length__cm_1)
# growth$Weight__kg_1<-as.numeric(growth$Weight__kg_1)
# growth$MUAC__in_mm_1<-as.numeric(growth$MUAC__in_mm_1)
# growth$age<-as.numeric(growth$age)
# growth<-mutate(growth, sex_bin=as.numeric(growth$Sex))
# 
# #for(i in 1:nrow(growth)) {
# #  print(getWGSR(sex=growth$sex_bin[i], firstPart = growth$Weight__kg_1[i], secondPart = growth$Length__cm_1[i], index="wfl"))
# #}
# growth<-mutate(growth, wflz=getWGSR(sex=growth$sex_bin, firstPart = growth$Weight__kg_1, secondPart = growth$Length__cm_1, index="wfl"))
# #growth<-addWGSR(data=static_growth_data, firstPart = "Weight__kg_1", secondPart = "Length__cm_1", sex = "sex_bin", index = "wfl")
# 
# growth<-growth[,c(1,9,10,11)]
# data<-full_join(growth, clinical_data, by="Patient_ID")
# patients<-data$Patient_ID
# rownames(data)<-patients
# data<-data[,-1]
# #data<-full_join(data,clinical_data, by="Patient_ID")
# set.seed(100)
# train <- sample(nrow(data), 0.75*nrow(data), replace = FALSE)
# data_train <- data[train,]
# data_test <- data[-train,]
# 
# 
# dt<-rpart(wflz~., data = data_train)
# rpart.plot(dt)

set.seed(123)
data_part1<-data[c(1, 232:233)]
data_part2<-clinical_data2[c(1,24,25)]
data2<-full_join(data_part1, data_part2, by="Patient_ID")
data2<-na.omit(data2)
data2$hospital_stay<-as.numeric(data2$hospital_stay)
data2$weight_gain<-as.numeric(data2$weight_gain)
data2$suspected_infection<-as.numeric(data2$suspected_infection)
data2$suspected_electrolyte_imbalance<-as.numeric(data2$suspected_electrolyte_imbalance)
data2<-data2[,-1]
rf<-randomForest(weight_gain ~ .,data=data2, importance=T, proximity=T)
print(rf)
round(importance(rf),2)

set.seed(123)
data_part1<-data[c(1, 232, 234)]
data_part2<-clinical_data2[c(1,24,25)]
data2<-full_join(data_part1, data_part2, by="Patient_ID")
data2<-na.omit(data2)
data2$hospital_stay<-as.numeric(data2$hospital_stay)
data2$rate_weight_gain<-as.numeric(data2$rate_weight_gain)
data2$suspected_infection<-as.numeric(data2$suspected_infection)
data2$suspected_electrolyte_imbalance<-as.numeric(data2$suspected_electrolyte_imbalance)
data2<-data2[,-1]
data2<-data2[-which(is.nan(data2$rate_weight_gain) | is.infinite(data2$rate_weight_gain)),]
rf<-randomForest(rate_weight_gain ~ .,data=data2, importance=T, proximity=T)
print(rf)
round(importance(rf),2)
setdiff(colnames(data), colnames(clinical_data2))
setdiff(colnames(clinical_data2), colnames(data))
