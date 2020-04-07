
##===  Data Exploration ====

##=== ! SOS ! ======

# Please open the dataset manually. Go to Rstudio Environment ->  Import Dataset -> From Text..
# This is because the version is controlled 

#TOC> =========================================================================================
#TOC>
#TOC>   Section  Title                                            Line
#TOC> -----------------------------------------------------------------------------------------
#TOC>   1        Packages                                          20
#TOC>   2        Missing data exploration                          65   
#TOC>   3        Growth dataset                                    82
#TOC>   4        Compute descriptive statistics                    118                           
#TOC> =========================================================================================

# ====    1  Packages  ========================================================================

if (! require(dplyr, quietly=TRUE)) {
  install.packages("dplyr")
  library(dplyr)
}

if (! require(lubridate, quietly=TRUE)) {
  install.packages("lubridate")
  library(lubridate)
}

if (! require(devtools, quietly=TRUE)) {
  install.packages("devtools")
  library(devtools)
}

if (! require(eeptools, quietly=TRUE)) {
  install.packages("eeptools")
  library(eeptools)
}

if (! require(ggpubr, quietly=TRUE)) {
  install.packages("ggpubr")
  library(ggpubr)
}

if (! require(pastecs, quietly=TRUE)) {
  install.packages("pastecs")
  library(pastecs)
}

install_github("Displayr/flipTime", force = TRUE)
library("flipTime")

# =============================================================================================

# Loading dataset

Hospital_dataset<-read.csv(file.choose(),header=T)

# Rename dataset 

data<-Hospital_dataset

# Check dataset

head(data, 6)

# ===== 2 Missing data exploration =============================================================

missing_data<-data.frame(column=colnames(data), prc_value=round(colSums(is.na(data))/dim(data)[1], digits=3)*100)

# Plot % missing data per index (column)

plot(round(colSums(is.na(data))/dim(data)[1], digits=2)*100)

# Which columns have more than 50% missing data? 

high_rate<-missing_data[which(missing_data$prc_value>=50),]
rownames(high_rate)

# Which columns have more than 20% missing data? 

high_rate20<-missing_data[which(missing_data$prc_value>=20),]
rownames(high_rate20)

# Which columns have less than 50% missing data? 

low_rate<-missing_data[which(missing_data$prc_value<50),]
rownames(low_rate)

# ====== 3 Growth dataset========================================================================

# Let's keep only the anthropometric measurements for now

clinical <- c("Patient_ID","Sex", "Date_of_Birth", "Odema","Diarrhoea_Exists", "Oedema","Oedema2",
              "Oedema3","Oedema4","Oedema5","Oedema6")

anthro <- c("Length__cm_1", "Length__cm_2", "Length__cm_3", "Length__cm_4", "Length__cm_5", "Length__cm_6",
            "Weight__kg_1", "Weight__kg_2", "Weight__kg_3", "Weight__kg_4", "Weight__kg_5", "Weight__kg_6",   
            "MUAC__in_mm_1", "MUAC__in_mm_2", "MUAC__in_mm_3", "MUAC__in_mm_4", "MUAC__in_mm_5", "MUAC__in_mm_6")

growth_list<-c(clinical, anthro )

growth_data<-data[,growth_list]

# Convert date of birth and calculate age

growth_data<-mutate(growth_data,mut_date_birth = AsDate(growth_data$Date_of_Birth)) 
as.Date(growth_data$mut_date_birth) 
growth_data<- mutate(growth_data,age= age_calc(growth_data$mut_date_birth, units='months'))

# Check again the missing data in the growth dataset

na_freq<-data.frame(col=character(ncol(growth_data)), freq=numeric(ncol(growth_data)), stringsAsFactors = F)
for(i in 1:ncol(growth_data)) {
  na_freq[i,]<-c(col=colnames(growth_data[i]), freq=as.numeric(sum(!is.na(growth_data[,i]))))
  #print(paste0(colnames(growth_data[i]), ": ", sum(!is.na(growth_data[,i]))))
}

na_freq$freq<-as.numeric(na_freq$freq)

# Visualizations 

ggbarplot(na_freq[c(12:17),], x = "col", y = "freq")


#====== 4 Compute descriptive statistics =================================================

res <- stat.desc(growth_data[, -c(1:11,30)])
round(res, 2)


