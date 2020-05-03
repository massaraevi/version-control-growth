


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

if (!require(dplyr, quietly = TRUE)) {
  install.packages("dplyr")
  library(dplyr)
}

if (!require(lubridate, quietly = TRUE)) {
  install.packages("lubridate")
  library(lubridate)
}

if (!require(devtools, quietly = TRUE)) {
  install.packages("devtools")
  library(devtools)
}

if (!require(eeptools, quietly = TRUE)) {
  install.packages("eeptools")
  library(eeptools)
}

if (!require(ggpubr, quietly = TRUE)) {
  install.packages("ggpubr")
  library(ggpubr)
}

if (!require(pastecs, quietly = TRUE)) {
  install.packages("pastecs")
  library(pastecs)
}

install_github("Displayr/flipTime", force = TRUE)
library("flipTime")


# Loading dataset

# Please load from R global environment. You should be able to see a dataframe with 5043 obs. 22

# Rename dataset

data <- Hospital_dataset

# Check dataset

head(data, 6)

# ===== 2 Missing data exploration =============================================================

missing_data <-
  data.frame(column = colnames(data),
             prc_value = round(colSums(is.na(data)) / dim(data)[1], digits = 3) * 100)

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

data <-
  
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
  na_freq[i,] <-
    c(col = colnames(growth_data[i]), freq = as.numeric(sum(is.na(growth_data[, i]))))
  #print(paste0(colnames(growth_data[i]), ": ", sum(!is.na(growth_data[,i]))))
}

na_freq$freq <- as.numeric(na_freq$freq)

# Visualizations

ggbarplot(na_freq[c(12:17),], x = "col", y = "freq")

# ====== 5 Clinical dataset========================================================================

clinical <-
  c(
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
    "Hematocrit_PCV",
    "Diff_Leucocyte_Count",
    "Total_Leucocyte_Count",
    "Neutrophil",
    "Anion_Gap",
    "ESR",
    "Hemoglobin",
    "Potassium",
    "Sodium",
    "Chloride"
  )

clinical_list <- c(clinical, biochemical)

clinical_data <- data[, clinical_list]

## Convert character variables to numeric ("Yes" = 1, "No" = 0)

for (i in 1:ncol(clinical_data)) {
  if (is.character(clinical_data[[1, i]])) {
    clinical_data[, i] <-
      as.numeric(ifelse(clinical_data[, i] == "No", 0, 1))
  }
}

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

ggbarplot(na_freq[c(12:17),], x = "col", y = "freq")

#====== 6 Compute descriptive statistics =================================================

res <- stat.desc(growth_data[, -c(1:11, 30)])
round(res, 2)
