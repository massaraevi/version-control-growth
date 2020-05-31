# Decision Trees

biochemical_short_df<-mutate(biochemical_short_df, Patient_ID=patients)

growth<-growth_data[,c(1:3,5,17,18,24,30,37)]
growth$Sex<-as.factor(growth$Sex)
growth$Oedema<-as.factor(growth$Oedema)
growth$Discharged_Weight<-as.numeric(growth$Discharged_Weight)
growth$Length__cm_1<-as.numeric(growth$Length__cm_1)
growth$Weight__kg_1<-as.numeric(growth$Weight__kg_1)
growth$MUAC__in_mm_1<-as.numeric(growth$MUAC__in_mm_1)
growth$age<-as.numeric(growth$age)
growth<-mutate(growth, sex_bin=as.numeric(growth$Sex))

#for(i in 1:nrow(growth)) {
#  print(getWGSR(sex=growth$sex_bin[i], firstPart = growth$Weight__kg_1[i], secondPart = growth$Length__cm_1[i], index="wfl"))
#}
growth<-mutate(growth, wflz=getWGSR(sex=growth$sex_bin, firstPart = growth$Weight__kg_1, secondPart = growth$Length__cm_1, index="wfl"))
#growth<-addWGSR(data=static_growth_data, firstPart = "Weight__kg_1", secondPart = "Length__cm_1", sex = "sex_bin", index = "wfl")

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
