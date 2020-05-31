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
    "zscorer",
    "rpart",
    "rpart.plot",
    "flipTime"
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

install.packages("remotes")
remotes::install_github("Displayr/flipTime")
library(flipTime)

# Loading dataset

# Please load from R global environment. You should be able to see a dataframe with 5043 obs. 22

# Rename dataset

data <- Hospital_dataset

# Check dataset

head(data, 6)

# ===== 2 Missing data exploration =============================================================

missing_data <-
  data.frame(
    column = colnames(data),
    prc_value = round(colSums(is.na(data)) / dim(data)[1], digits = 3) * 100,
    stringsAsFactors = F
  )

# Plot % missing data per index (column)

plot(round(colSums(is.na(data)) / dim(data)[1], digits = 2) * 100)

# Which columns have more than 50% missing data?

high_rate <- missing_data[which(missing_data$prc_value >= 50),]
rownames(high_rate)

# Which columns have more than 20% missing data?

high_rate20 <- missing_data[which(missing_data$prc_value >= 20), ]
rownames(high_rate20)

# Which columns have less than 50% missing data?

low_rate <- missing_data[which(missing_data$prc_value < 50), ]
rownames(low_rate)

# ====== 3 Manipulate the dataset ========================================================================

# Convert date of registration and outcome age

data <-
  data %>% mutate(mut_date1 = AsDate(data$Date1)) %>% mutate(mut_out_date = AsDate(data$Outcome_Date))

## Create a new variable – Duration of hospital stay – from Date1 and Outcome_Date

data <-
  data %>% mutate(hospital_stay = as.numeric(mut_out_date - mut_date1)) # hospital_stay in days

## Calculate weight gain (Discharged_Weight - Weight__kg_1)

data <-
  data %>% mutate(weight_gain = as.numeric(Discharged_Weight - Weight__kg_1))

## Calculate of rate of weight gain
#Rate of weight gain (gm/kg/day)=(( Discharged_Weight - Weight__kg_1) * 1000) / ((Outcome_Date - Date1) * Weight__kg_1)

data <-
  data %>% mutate(rate_weight_gain = (weight_gain * 1000) / hospital_stay * Weight__kg_1) # (gm/kg/day)

## Convert character variables to numeric ("Yes" = 1, "No" = 0)
#
# for (i in 1:ncol(data)) {
#   if (is.character(data[[1, i]])) {
#     data[, i] <-
#       as.numeric(ifelse(data[, i] == "No", 0, 1))
#   }
# }

#data<-apply(data, 2, revalue,c("No"="0", "Yes"="1"))
data <-
  data.frame(lapply(data, function(x) {
    gsub("\\<Yes\\>", "1", x)
  }), stringsAsFactors = F)
data <-
  data.frame(lapply(data, function(x) {
    gsub("\\<No\\>", "0", x)
  }), stringsAsFactors = F)
#data <- as.data.frame(data)

# ====== 4 Growth dataset========================================================================

# Let's keep only the anthropometric measurements and oedema that might have effect on weight for now

clinical <-
  c(
    "Patient_ID",
    "Sex",
    "Date_of_Birth",
    "Odema",
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
    "Date6"
  )

anthro <-
  c(
    "Discharged_Weight",
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

# Convert date of birth and calculate age

growth_data <-
  mutate(growth_data, mut_date_birth = AsDate(growth_data$Date_of_Birth))
as.Date(growth_data$mut_date_birth)
growth_data <-
  mutate(growth_data,
         age = age_calc(growth_data$mut_date_birth, units = 'months'))

# Check again the missing data in the growth dataset

na_freq <-
  data.frame(
    col = character(ncol(growth_data)),
    freq = numeric(ncol(growth_data)),
    stringsAsFactors = F
  )
for (i in 1:ncol(growth_data)) {
  na_freq[i, ] <-
    c(col = colnames(growth_data[i]), freq = as.numeric(sum(is.na(growth_data[, i]))))
  #print(paste0(colnames(growth_data[i]), ": ", sum(!is.na(growth_data[,i]))))
}

na_freq$freq <- as.numeric(na_freq$freq)

# Visualizations

ggbarplot(na_freq[c(12:17), ], x = "col", y = "freq")

gg_miss_var(growth_data)

res <- summary(aggr(growth_data, sortVar = TRUE))$combinations

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

# NOTE: Diff_Leucocyte_Count has 100% Nas, so it will be excluded

#------ Prepare the datasets

clinical_list <- c(clinical, biochemical)
clinical_data <- data[, clinical_list]
clinical_short <- data[, clinical]
biochemical_short <- data[, biochemical]

##--------- Check for missing data in both datasets

na_freq <-
  data.frame(
    col = character(ncol(clinical_data)),
    freq = numeric(ncol(clinical_data)),
    stringsAsFactors = F
  )
for (i in 1:ncol(clinical_data)) {
  na_freq[i, ] <-
    c(col = colnames(clinical_data[i]), freq = as.numeric(sum(is.na(clinical_data[, i]))))
  #print(paste0(colnames(growth_data[i]), ": ", sum(!is.na(growth_data[,i]))))
}

na_freq$freq <- as.numeric(na_freq$freq)

#-------- Visualizations

# Frequency of Nas per column
gg_miss_var(clinical_data)

# Number of missing entries in each variable and for certain combinations of variables
res <- summary(aggr(clinical_data, sortVar = TRUE))$combinations

# The points for which x (resp. y) is missing are represented in red along the y (resp. x) axis.
# In addition, boxplots of the x and y variables are represented along the axes
# with and without missing values (in red all variables x where y is missing, in blue all variables x where y is observed).

marginplot(clinical_data[, c("Fever", "ESR")])

## CLINICAL
patients <- clinical_short[, 1]
rownames(clinical_short) <- patient

##--------- Check for missing data in the clinical dataset
# Frequency of Nas per column
gg_miss_var(clinical_short)
res <- summary(aggr(clinical_short, sortVar = TRUE))$combinations

# Since proportion of missingness is equal among the variables exclude these with Nas
clinical_short <- na.omit(clinical_short)
full_data_id_cl <- clinical_short$Patient_ID

#-------- Detect and represent underlying structures

# Multiple Correspondence Analysis (MCA).

clinical_short <- clinical_short[,-1]
res.mca <- MCA(clinical_short, ncp = 5, graph=TRUE)
eig.val <- get_eigenvalue(res.mca)
fviz_screeplot(res.mca, addlabels = TRUE, ylim = c(0,45))
fviz_mca_var(res.mca, choice = "mca.cor", 
             repel = TRUE, # Avoid text overlapping (slow)
             ggtheme = theme_minimal())

## BIOCHEMICAL
patients <- biochemical_short[, 1]
common_patients <- biochemical_short[which(!biochemical_short$Patient_ID %in% full_data_id_cl),] 
rownames(biochemical_short) <- patients
biochemical_short <- biochemical_short[, -1]
biochemical_short <- apply(biochemical_short, 2, as.numeric)

##--------- Check for missing data in the clinical dataset
# Frequency of Nas per column
gg_miss_var(as.data.frame(biochemical_short))
res <- summary(aggr(biochemical_short, sortVar = TRUE))$combinations

# Since proportion of missingness is equal among the variables exclude these with Nas
#clinical_short <- na.omit(clinical_short)
#full_data_id_cl <- clinical_short$Patient_ID

#-------- PCA
# Dimentions reduction with PCA for incomplete data biochemical data
# Biochemical data are continuous so a separate analysis will be conducted from
# clinical data

nb <-
  estim_ncpPCA(biochemical_short , method.cv = "Kfold", verbose = FALSE)
nb$ncp
plot(0:5, nb$criterion, xlab = "nb dim", ylab = "MSEP")

biochemical_short_df <- as.data.frame(biochemical_short)
res.comp <-
  imputePCA(biochemical_short_df, ncp = 2) # iterativePCA algorithm
res.comp$completeObs[1:3,] # the imputed data set

imp <- as.data.frame(res.comp$completeObs)

res.pca <- PCA(imp, ncp = 2, graph = FALSE)
plot(res.pca, lab = "quali")

plot(res.pca, choix = "var")


eig.val <- get_eigenvalue(res.pca)
fviz_screeplot(res.pca, addlabels = TRUE, ylim = c(0,45))
fviz_pca_var(res.pca, choice = "pca.cor", 
             repel = TRUE, # Avoid text overlapping (slow)
             ggtheme = theme_minimal())

fviz_cos2(res.pca, choice = "var", axes = 1:2)



#====== 6 Compute descriptive statistics =================================================

#res <- stat.desc(growth_data[, -c(1:16, 37)])
#round(res, 2)

clinical_data2<-mutate(clinical_data[,-16],NAs=numeric(nrow(clinical_data)))

for(i in 1:nrow(clinical_data)) {
  row_t<-t(clinical_data[i,])
  clinical_data2[i,]$NAs<-sum(is.na(row_t))
}
common_patients2<-clinical_data2[which(clinical_data2$NAs>10),]$Patient_ID

clinical_data3<-clinical_data2[which(clinical_data2$NAs<11),]

gg_miss_var(clinical_data3)
res <- summary(aggr(clinical_data3[,-c(1,25)], sortVar = F,combined=T))$combinations
