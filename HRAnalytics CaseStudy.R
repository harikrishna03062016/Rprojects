rm(list = ls())

library(ggplot2)
library(dplyr)
library(car)
library(caTools)
library(MASS)
library(lubridate)
library(e1071)
library(caret)
library(cowplot)
library(dplyr)
library(tidyr)
library(GGally)
library(ROCR)

#Custom Functions
plotDataNABar<- function(frameData){
  
  colNameVal <- vector(mode="numeric", length=0)
  colNameDataLength <- vector(mode="numeric", length=0)
  
  for(i in 1:length(frameData)){
    #colNameVal <- c(colNameVal,length(frameData[,i])-sum(is.na(frameData[,i])))
    colNameDataLength <- c(colNameDataLength,colnames(frameData)[i])          
    colNameVal <- c(colNameVal,sum(is.na(frameData[,i])))
    
  }
  
  dataValidityMatrix <- data.frame(colNameVal)
  dataValidityMatrix<- cbind(dataValidityMatrix,colNameDataLength)
  
  ggplot(dataValidityMatrix,aes(colNameDataLength,colNameVal))+geom_bar(stat = "identity")+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
 # View(dataValidityMatrix)
}

MyMerge <- function(x, y){
  df <- merge(x, y, by= "EmployeeID", all.x= TRUE, all.y= TRUE)
  return(df)
}


#Data Loading
#----
#Loading Survey Data
empSurveyData <- read.csv("employee_survey_data.csv", stringsAsFactors = FALSE)
str(empSurveyData)

#Loading General Data
empGeneralData <- read.csv("general_data.csv", stringsAsFactors = FALSE)
str(empGeneralData)

#Loading Manager Survey Data
mgrSurveyData <- read.csv("manager_survey_data.csv", stringsAsFactors = FALSE)
str(mgrSurveyData)

#Loading in_time
in_time <- read.csv("in_time.csv", stringsAsFactors = FALSE,header = TRUE)
str(in_time)

#Loading out_time
out_time <- read.csv("out_time.csv", stringsAsFactors = FALSE)
str(out_time)

#Data Cleaning

#Checking for NA values and Duplicate Data

#EmpSurveyData
#--No Duplicate Data present
length(empSurveyData$EmployeeID)
length(unique(empSurveyData$EmployeeID))
plotDataNABar(empSurveyData) #Observed NA Values

#GeneralSurveyData
#--No Duplicate Data present
length(empGeneralData$EmployeeID)
length(unique(empGeneralData$EmployeeID))
plotDataNABar(empGeneralData) #Observed NA values

#Manager SurveyData
#--No NA values present
#--No Duplicate Data present
length(mgrSurveyData$EmployeeID)
length(unique(mgrSurveyData$EmployeeID))
plotDataNABar(mgrSurveyData)# No NA values present

#------------
#Handling Time Data

#Remove columns with all NAs
#--No Duplicate Data present
length(in_time$X)
length(unique(in_time$X))
plotDataNABar(in_time)

#out_time
#--No Duplicate Data present
length(out_time$X)
length(unique(out_time$X))
plotDataNABar(out_time)


#Handling in_time and out_time
x <- colnames(in_time)
y<- colnames(out_time)
length(x)
length(y)

#Checking if all the colnames are matching are not
length(intersect(x,y))

#Checking if NA's are present in corresponding cells in both in_time and out_time
x <- (is.na(in_time))
y<- is.na(out_time)
z <- x-y
sum(z>0)
sum(z<0)

#converting charcters to dates in_time and out_time
length(intersect(in_time$X,empGeneralData$EmployeeID))

#Removing columns which have complete NAs
in_time <- in_time[, colSums(is.na(in_time)) != nrow(in_time)]
out_time <- out_time[, colSums(is.na(out_time))!= nrow(out_time)]

#Replacing the remaian NA values with a random standard date
in_time[is.na(in_time)] <- as.character("2016-01-01 00:00:00")  
out_time[is.na(out_time)] <- as.character("2016-01-01 00:00:00")

#Converting in_time which is wide format to long format
inTimeWide <- gather(in_time,key = keyVal, value = inTime,2:length(in_time))
inTimeWide <- inTimeWide[order(inTimeWide$keyVal,inTimeWide$X),]

#Converting out_time which is in wide format to long format
outTimeWide <- gather(out_time, key = keyVal,value=outTime, 2:length(out_time))
outTimeWide <- outTimeWide[order(outTimeWide$keyVal,outTimeWide$X),]

#Merging in_time and out_time using eployeeID
timeData <- merge(inTimeWide,outTimeWide,by = c("X","keyVal"))

timeData$timeDiff <- as.numeric(as.POSIXct(timeData$outTime)-as.POSIXct(timeData$inTime))/(60*60)

timeData <- group_by(timeData, X)
empTimeAnalysis <- data.frame(summarise(timeData, cov=(100*sd(timeDiff)/mean(timeDiff))))
empTimeAnalysis$offCount <- summarise(timeData,offCount=sum(timeDiff==0))$offCount
empTimeAnalysis$medianTime <-summarise(timeData,medianTime=median(timeDiff))$medianTime

rm(list=c("inTimeWide","outTimeWide","in_time","out_time","timeData"))

#Checking correlation between emptime and cov
cor(empTimeAnalysis$medianTime, empTimeAnalysis$cov)

#Updating the column name to employeeID
colnames(empTimeAnalysis)[1] <- "EmployeeID"

#--------------
#Merging Data
HR_Data <- Reduce(MyMerge, list(empSurveyData,empGeneralData, mgrSurveyData,empTimeAnalysis))
View(HR_Data)

#Data Cleaning in merged data frame

#NAs in merged data frame
plotDataNABar(HR_Data)

#Adjusting NA values
#Steps Available:
#1. If Attrition rate doesnt fall too much then we can ignore the rows with NA values
#2 If Attrition rate drops too much then we might need to make appropriate assumptions 
#and replace the missing values with appropriate mean, median or mode values

#checking Attrition Rate
sum(ifelse(HR_Data$Attrition=='Yes',1,0))/nrow(HR_Data)
nrow(HR_Data)

HR_Data1 <- na.omit(HR_Data)
nrow(HR_Data1)
plotDataNABar(HR_Data1)

#Attrition rate dropped by 0.5% which is assumed to be ok so continuing with removing of data
sum(ifelse(HR_Data1$Attrition=='Yes',1,0))/nrow(HR_Data)

HR_Data <- HR_Data1
rm(list=ls(HR_Data1))

#Creating Performance and Satisfaction factors 
HR_Data$SatisfactionScore <- HR_Data$EnvironmentSatisfaction+HR_Data$JobSatisfaction+HR_Data$WorkLifeBalance
HR_Data$PerformanceScore <- HR_Data$JobInvolvement+HR_Data$PerformanceRating
  
### Data Preparation & Exploratory Data Analysis
str(HR_Data)

# Barcharts for categorical features 
bar_theme1<- theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), 
                   legend.position="none")

colnames(HR_Data)


plot_grid(ggplot(HR_Data, aes(x=factor(EnvironmentSatisfaction),fill=Attrition))+ geom_bar(), 
          ggplot(HR_Data, aes(x=JobSatisfaction,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(HR_Data, aes(x=WorkLifeBalance,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(HR_Data, aes(x=BusinessTravel,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(HR_Data, aes(x=Department,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(HR_Data, aes(x=Education,fill=Attrition))+ geom_bar()+bar_theme1,
          align = "h")   


plot_grid(ggplot(HR_Data, aes(x=EducationField,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(HR_Data, aes(x=Gender,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(HR_Data, aes(x=JobLevel,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(HR_Data, aes(x=JobRole,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(HR_Data, aes(x=MaritalStatus,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(HR_Data, aes(x=Over18,fill=Attrition))+ geom_bar()+bar_theme1,
          align = "h")   


#Removing column over18 as it doesnt have any differentiating Data
unique(HR_Data$Over18)
HR_Data = subset(HR_Data, select = -c(Over18))


plot_grid(ggplot(HR_Data, aes(x=StockOptionLevel,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(HR_Data, aes(x=JobInvolvement,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(HR_Data, aes(x=factor(PerformanceRating),fill=Attrition))+ geom_bar()+bar_theme1,
          align = "h")   


table(HR_Data$StockOptionLevel)

#Quantitative Variables

box_theme<- theme(axis.line=element_blank(),axis.title=element_blank(), 
                  axis.ticks=element_blank(), axis.text=element_blank())

box_theme_y<- theme(axis.line.y=element_blank(),axis.title.y=element_blank(), 
                    axis.ticks.y=element_blank(), axis.text.y=element_blank(),
                    legend.position="none")

plot_grid(ggplot(HR_Data, aes(Age))+ geom_histogram(binwidth = 10),
          ggplot(HR_Data, aes(x="",y=Age))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

plot_grid(ggplot(HR_Data, aes(DistanceFromHome))+ geom_histogram(binwidth = 10),
          ggplot(HR_Data, aes(x="",y=DistanceFromHome))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

plot_grid(ggplot(HR_Data, aes(NumCompaniesWorked))+ geom_histogram(binwidth = 10),
          ggplot(HR_Data, aes(x="",y=NumCompaniesWorked))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)


plot_grid(ggplot(HR_Data, aes(MonthlyIncome))+ geom_histogram(binwidth = 10),
          ggplot(HR_Data, aes(x="",y=MonthlyIncome))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

plot_grid(ggplot(HR_Data, aes(PercentSalaryHike))+ geom_histogram(binwidth = 10),
          ggplot(HR_Data, aes(x="",y=PercentSalaryHike))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

plot_grid(ggplot(HR_Data, aes(TotalWorkingYears))+ geom_histogram(binwidth = 10),
          ggplot(HR_Data, aes(x="",y=TotalWorkingYears))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

quantile(HR_Data$TotalWorkingYears)

plot_grid(ggplot(HR_Data, aes(TrainingTimesLastYear))+ geom_histogram(binwidth = 10),
          ggplot(HR_Data, aes(x="",y=TrainingTimesLastYear))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

plot_grid(ggplot(HR_Data, aes(YearsAtCompany))+ geom_histogram(binwidth = 10),
          ggplot(HR_Data, aes(x="",y=YearsAtCompany))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

plot_grid(ggplot(HR_Data, aes(YearsSinceLastPromotion))+ geom_histogram(binwidth = 10),
          ggplot(HR_Data, aes(x="",y=YearsSinceLastPromotion))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

plot_grid(ggplot(HR_Data, aes(YearsWithCurrManager))+ geom_histogram(binwidth = 10),
          ggplot(HR_Data, aes(x="",y=YearsWithCurrManager))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

#Removing EmployeeCount column as it doesnt have any differentiating information
unique(HR_Data$EmployeeCount)
HR_Data <- subset(HR_Data, select=-c(EmployeeCount))

#Removing StandHours Column as it doesnt hold differentiating information
unique(HR_Data$StandardHours)
HR_Data = subset(HR_Data, select=-c(StandardHours))

#Outlier Treatment
#Monthly Income

boxplot(HR_Data$MonthlyIncome)
HR_Data$MonthlyIncome<-log(HR_Data$MonthlyIncome)
boxplot(HR_Data$MonthlyIncome)

colnames(HR_Data)

# Correlation between numeric variables
#Observed Correlation between 
ggpairs(HR_Data[,c("Age", "DistanceFromHome","Education", "MonthlyIncome","PercentSalaryHike","TotalWorkingYears","TrainingTimesLastYear","YearsAtCompany","YearsSinceLastPromotion","YearsWithCurrManager" )])
ggpairs(HR_Data[,c("TotalWorkingYears","NumCompaniesWorked","YearsAtCompany","YearsSinceLastPromotion","YearsWithCurrManager")])


################################################################
### Data Preparation

HR_Data$Gender<- ifelse(HR_Data$Gender=="Male",1,0)
HR_Data$MaritalStatus<- ifelse(HR_Data$MaritalStatus=="Married",1,0)
HR_Data$Attrition<- ifelse(HR_Data$Attrition=="Yes",1,0)

#Checking Attrition Rate
attritionRate <- sum(HR_Data$Attrition)/nrow(HR_Data)
attritionRate # 16.2% attrition rate

################################################################
# Feature standardisation
HR_Data$Age<-scale(HR_Data$Age)
HR_Data$DistanceFromHome<-scale(HR_Data$DistanceFromHome)
HR_Data$PercentSalaryHike<-scale(HR_Data$PercentSalaryHike)
HR_Data$TotalWorkingYears<-scale(HR_Data$TotalWorkingYears)
HR_Data$TrainingTimesLastYear<-scale(HR_Data$TrainingTimesLastYear)
HR_Data$YearsAtCompany<-scale(HR_Data$YearsAtCompany)
HR_Data$YearsSinceLastPromotion<-scale(HR_Data$YearsSinceLastPromotion)
HR_Data$YearsWithCurrManager<-scale(HR_Data$YearsWithCurrManager)
HR_Data$cov<-scale(HR_Data$cov)
HR_Data$medianTime<-scale(HR_Data$medianTime)
HR_Data$SatisfactionScore<-scale(HR_Data$SatisfactionScore)
HR_Data$PerformanceScore<-scale(HR_Data$PerformanceScore)

#Adjusting the data in numcompanies worked as per the logic
HR_Data$diffYear <- HR_Data$TotalWorkingYears - HR_Data$YearsAtCompany 

length(which(HR_Data$diffYear>0 & HR_Data$NumCompaniesWorked<1))
#People with more than zero experience apart from the current company should have
#NumCompaniesWorked atleast 1 but its noticed that for more than 500 rows it has 0 value which is not valid
#Hence removing the column as a right assumption is not possible

HR_Data <- subset(HR_Data, select=-c(NumCompaniesWorked))

# creating a dataframe of categorical features
HR_Data_Char <- HR_Data[,c("BusinessTravel","Department","JobRole", "EnvironmentSatisfaction","JobSatisfaction","WorkLifeBalance","Education",
                        "EducationField","JobLevel","StockOptionLevel","JobInvolvement","PerformanceRating")]

# converting categorical attributes to factor
HR_Data_Fact<- data.frame(sapply(HR_Data_Char, function(x) factor(x)))
str(HR_Data_Fact)

# creating dummy variables for factor attributes
dummies<- data.frame(sapply(HR_Data_Fact, 
                            function(x) data.frame(model.matrix(~x-1,data =HR_Data_Fact))[,-1]))
							
HR_Data_final<- cbind(HR_Data[,c("Age","Attrition","DistanceFromHome","Gender","MaritalStatus","MonthlyIncome",
                               "PercentSalaryHike","TotalWorkingYears","TrainingTimesLastYear","YearsAtCompany","YearsSinceLastPromotion","YearsWithCurrManager",
							   "cov","offCount","medianTime","SatisfactionScore","PerformanceScore")],dummies)

########################################################################
# splitting the data between train and test
set.seed(100)

indices = sample.split(HR_Data_final$Attrition, SplitRatio = 0.7)

train = HR_Data_final[indices,]

test = HR_Data_final[!(indices),]

########################################################################
# Logistic Regression: 

#Initial model
model_1 = glm(Attrition ~ ., data = train, family = "binomial")
summary(model_1) #AIC 2191.4

model_2<- stepAIC(model_1, direction="both")
summary(model_2)
#Multicollinearity check through vif
sort(vif(model_2))

#Removing EducationField.xLife.Sciences because of low significance and high vif value
model_3<- glm(formula = Attrition ~ Age + MaritalStatus + TotalWorkingYears + TrainingTimesLastYear +    
              YearsAtCompany + YearsSinceLastPromotion + YearsWithCurrManager + medianTime + SatisfactionScore +
			  BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + Department.xResearch...Development + Department.xSales + 
			  JobRole.xLaboratory.Technician + JobRole.xManufacturing.Director +  JobRole.xResearch.Director + JobRole.xResearch.Scientist +
			  JobRole.xSales.Executive + EnvironmentSatisfaction.x2 + JobSatisfaction.x2 +   WorkLifeBalance.x2 + WorkLifeBalance.x3 + Education.x5 +                 
              EducationField.xMarketing + EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + JobLevel.x2 + JobLevel.x5 + 
             StockOptionLevel.x1 + JobInvolvement.x3 +PerformanceRating, family = "binomial", data = train)

summary(model_3)
#Multicollinearity check through vif
sort(vif(model_3))

#Removing YearsAtCompany because of low significance and high vif value

model_4<- glm(formula = Attrition ~ Age + MaritalStatus + TotalWorkingYears + TrainingTimesLastYear     
              + YearsSinceLastPromotion + YearsWithCurrManager + medianTime + SatisfactionScore +
			  BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + Department.xResearch...Development + Department.xSales + 
			  JobRole.xLaboratory.Technician + JobRole.xManufacturing.Director +  JobRole.xResearch.Director + JobRole.xResearch.Scientist +
			  JobRole.xSales.Executive + EnvironmentSatisfaction.x2 + JobSatisfaction.x2 +   WorkLifeBalance.x2 + WorkLifeBalance.x3 + Education.x5 +                 
              EducationField.xMarketing + EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + JobLevel.x2 + JobLevel.x5 + 
              StockOptionLevel.x1 + JobInvolvement.x3 +PerformanceRating, family = "binomial", data = train)
summary(model_4)
sort(vif(model_4))

#Removing BusinessTravel.xTravel_Rarely 		 
model_5<- glm(formula = Attrition ~ Age + MaritalStatus + TotalWorkingYears + TrainingTimesLastYear     
              + YearsSinceLastPromotion + YearsWithCurrManager + medianTime + SatisfactionScore +
			  BusinessTravel.xTravel_Frequently  + Department.xResearch...Development + Department.xSales + 
			  JobRole.xLaboratory.Technician + JobRole.xManufacturing.Director +  JobRole.xResearch.Director + JobRole.xResearch.Scientist +
			  JobRole.xSales.Executive + EnvironmentSatisfaction.x2 + JobSatisfaction.x2 +   WorkLifeBalance.x2 + WorkLifeBalance.x3 + Education.x5 +                 
              EducationField.xMarketing + EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + JobLevel.x2 + JobLevel.x5 + 
             StockOptionLevel.x1 + JobInvolvement.x3 +PerformanceRating, family = "binomial", data = train)
summary(model_5)
sort(vif(model_5))

cor(train$Department.xResearch...Development, train$Department.xSales)

#Removing Department.xSales
model_6<- glm(formula = Attrition ~ Age + MaritalStatus + TotalWorkingYears + TrainingTimesLastYear     
              + YearsSinceLastPromotion + YearsWithCurrManager + medianTime + SatisfactionScore +
			  BusinessTravel.xTravel_Frequently  + Department.xResearch...Development  + 
			  JobRole.xLaboratory.Technician + JobRole.xManufacturing.Director +  JobRole.xResearch.Director + JobRole.xResearch.Scientist +
			  JobRole.xSales.Executive + EnvironmentSatisfaction.x2 + JobSatisfaction.x2 +   WorkLifeBalance.x2 + WorkLifeBalance.x3 + Education.x5 +                 
              EducationField.xMarketing + EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + JobLevel.x2 + JobLevel.x5 + 
             StockOptionLevel.x1 + JobInvolvement.x3 +PerformanceRating, family = "binomial", data = train)
summary(model_6)
sort(vif(model_6))
			 
#Removing JobLevel.x5
model_7<- glm(formula = Attrition ~ Age + MaritalStatus + TotalWorkingYears + TrainingTimesLastYear     
              + YearsSinceLastPromotion + YearsWithCurrManager + medianTime + SatisfactionScore +
			  BusinessTravel.xTravel_Frequently  + Department.xResearch...Development  + 
			  JobRole.xLaboratory.Technician + JobRole.xManufacturing.Director +  JobRole.xResearch.Director + JobRole.xResearch.Scientist +
			  JobRole.xSales.Executive + EnvironmentSatisfaction.x2 + JobSatisfaction.x2 +   WorkLifeBalance.x2 + WorkLifeBalance.x3 + Education.x5 +                 
              EducationField.xMarketing + EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + JobLevel.x2 + 
             StockOptionLevel.x1 + JobInvolvement.x3 +PerformanceRating, family = "binomial", data = train)
summary(model_7)
sort(vif(model_7))

#Removing Education.x5
model_8<- glm(formula = Attrition ~ Age + MaritalStatus + TotalWorkingYears + TrainingTimesLastYear     
              + YearsSinceLastPromotion + YearsWithCurrManager + medianTime + SatisfactionScore +
			  BusinessTravel.xTravel_Frequently  + Department.xResearch...Development  + 
			  JobRole.xLaboratory.Technician + JobRole.xManufacturing.Director +  JobRole.xResearch.Director + JobRole.xResearch.Scientist +
			  JobRole.xSales.Executive + EnvironmentSatisfaction.x2 + JobSatisfaction.x2 +   WorkLifeBalance.x2 + WorkLifeBalance.x3 +                  
              EducationField.xMarketing + EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + JobLevel.x2 + 
             StockOptionLevel.x1 + JobInvolvement.x3 +PerformanceRating, family = "binomial", data = train)			 
summary(model_8)
sort(vif(model_8))

#Removing PerformanceRating
model_9<- glm(formula = Attrition ~ Age + MaritalStatus + TotalWorkingYears + TrainingTimesLastYear     
              + YearsSinceLastPromotion + YearsWithCurrManager + medianTime + SatisfactionScore +
			  BusinessTravel.xTravel_Frequently  + Department.xResearch...Development  + 
			  JobRole.xLaboratory.Technician + JobRole.xManufacturing.Director +  JobRole.xResearch.Director + JobRole.xResearch.Scientist +
			  JobRole.xSales.Executive + EnvironmentSatisfaction.x2 + JobSatisfaction.x2 +   WorkLifeBalance.x2 + WorkLifeBalance.x3 +                  
              EducationField.xMarketing + EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + JobLevel.x2 + 
             StockOptionLevel.x1 + JobInvolvement.x3 , family = "binomial", data = train)	
summary(model_9)
sort(vif(model_9))

#Removing EducationField.xMedical  			 
model_10<- glm(formula = Attrition ~ Age + MaritalStatus + TotalWorkingYears + TrainingTimesLastYear     
              + YearsSinceLastPromotion + YearsWithCurrManager + medianTime + SatisfactionScore +
			  BusinessTravel.xTravel_Frequently  + Department.xResearch...Development  + 
			  JobRole.xLaboratory.Technician + JobRole.xManufacturing.Director +  JobRole.xResearch.Director + JobRole.xResearch.Scientist +
			  JobRole.xSales.Executive + EnvironmentSatisfaction.x2 + JobSatisfaction.x2 +   WorkLifeBalance.x2 + WorkLifeBalance.x3 +                  
              EducationField.xMarketing  + EducationField.xOther + EducationField.xTechnical.Degree + JobLevel.x2 + 
             StockOptionLevel.x1 + JobInvolvement.x3 , family = "binomial", data = train)
summary(model_10)
sort(vif(model_10))
			 
#Removing EducationField.xOther
model_11<- glm(formula = Attrition ~ Age + MaritalStatus + TotalWorkingYears + TrainingTimesLastYear     
              + YearsSinceLastPromotion + YearsWithCurrManager + medianTime + SatisfactionScore +
			  BusinessTravel.xTravel_Frequently  + Department.xResearch...Development  + 
			  JobRole.xLaboratory.Technician + JobRole.xManufacturing.Director +  JobRole.xResearch.Director + JobRole.xResearch.Scientist +
			  JobRole.xSales.Executive + EnvironmentSatisfaction.x2 + JobSatisfaction.x2 +   WorkLifeBalance.x2 + WorkLifeBalance.x3 +                  
              EducationField.xMarketing + EducationField.xTechnical.Degree + JobLevel.x2 + 
             StockOptionLevel.x1 + JobInvolvement.x3 , family = "binomial", data = train)
summary(model_11)
sort(vif(model_11))

#Removing EducationField.xTechnical.Degree 
model_12<- glm(formula = Attrition ~ Age + MaritalStatus + TotalWorkingYears + TrainingTimesLastYear     
              + YearsSinceLastPromotion + YearsWithCurrManager + medianTime + SatisfactionScore +
			  BusinessTravel.xTravel_Frequently  + Department.xResearch...Development  + 
			  JobRole.xLaboratory.Technician + JobRole.xManufacturing.Director +  JobRole.xResearch.Director + JobRole.xResearch.Scientist +
			  JobRole.xSales.Executive + EnvironmentSatisfaction.x2 + JobSatisfaction.x2 +   WorkLifeBalance.x2 + WorkLifeBalance.x3 +                  
              EducationField.xMarketing  + JobLevel.x2 + 
             StockOptionLevel.x1 + JobInvolvement.x3 , family = "binomial", data = train)
summary(model_12)
sort(vif(model_12))
			 
#Removing EnvironmentSatisfaction.x2  			 
model_13<- glm(formula = Attrition ~ Age + MaritalStatus + TotalWorkingYears + TrainingTimesLastYear     
              + YearsSinceLastPromotion + YearsWithCurrManager + medianTime + SatisfactionScore +
			  BusinessTravel.xTravel_Frequently  + Department.xResearch...Development  + 
			  JobRole.xLaboratory.Technician + JobRole.xManufacturing.Director +  JobRole.xResearch.Director + JobRole.xResearch.Scientist +
			  JobRole.xSales.Executive + JobSatisfaction.x2 +   WorkLifeBalance.x2 + WorkLifeBalance.x3 +                  
              EducationField.xMarketing  + JobLevel.x2 + 
             StockOptionLevel.x1 + JobInvolvement.x3 , family = "binomial", data = train)
summary(model_13)
sort(vif(model_13))
			 
#Removing EducationField.xMarketing			 
model_14<- glm(formula = Attrition ~ Age + MaritalStatus + TotalWorkingYears + TrainingTimesLastYear     
              + YearsSinceLastPromotion + YearsWithCurrManager + medianTime + SatisfactionScore +
			  BusinessTravel.xTravel_Frequently  + Department.xResearch...Development  + 
			  JobRole.xLaboratory.Technician + JobRole.xManufacturing.Director +  JobRole.xResearch.Director + JobRole.xResearch.Scientist +
			  JobRole.xSales.Executive + JobSatisfaction.x2 +   WorkLifeBalance.x2 + WorkLifeBalance.x3 + JobLevel.x2 + 
             StockOptionLevel.x1 + JobInvolvement.x3 , family = "binomial", data = train)
summary(model_14)
sort(vif(model_14))

#Removing Department.xResearch...Development			 
model_15<- glm(formula = Attrition ~ Age + MaritalStatus + TotalWorkingYears + TrainingTimesLastYear     
              + YearsSinceLastPromotion + YearsWithCurrManager + medianTime + SatisfactionScore +
			  BusinessTravel.xTravel_Frequently  +JobRole.xLaboratory.Technician + JobRole.xManufacturing.Director +
			  JobRole.xResearch.Director + JobRole.xResearch.Scientist +JobRole.xSales.Executive + JobSatisfaction.x2 +  
			  WorkLifeBalance.x2 + WorkLifeBalance.x3 + JobLevel.x2 +  StockOptionLevel.x1 + JobInvolvement.x3 , family = "binomial", data = train)
summary(model_15)
sort(vif(model_15))

#Removing StockOptionLevel.x1 			  
model_16<- glm(formula = Attrition ~ Age + MaritalStatus + TotalWorkingYears + TrainingTimesLastYear     
              + YearsSinceLastPromotion + YearsWithCurrManager + medianTime + SatisfactionScore +
			  BusinessTravel.xTravel_Frequently  +JobRole.xLaboratory.Technician + JobRole.xManufacturing.Director +
			  JobRole.xResearch.Director + JobRole.xResearch.Scientist +JobRole.xSales.Executive + JobSatisfaction.x2 +  
			  WorkLifeBalance.x2 + WorkLifeBalance.x3 + JobLevel.x2 + JobInvolvement.x3 , family = "binomial", data = train)
summary(model_16)
sort(vif(model_16))
			  
#Removing JobRole.xLaboratory.Technician 			  
model_17<- glm(formula = Attrition ~ Age + MaritalStatus + TotalWorkingYears + TrainingTimesLastYear     
              + YearsSinceLastPromotion + YearsWithCurrManager + medianTime + SatisfactionScore +
			  BusinessTravel.xTravel_Frequently +JobRole.xManufacturing.Director +
			  JobRole.xResearch.Director + JobRole.xResearch.Scientist +JobRole.xSales.Executive + JobSatisfaction.x2 +  
			  WorkLifeBalance.x2 + WorkLifeBalance.x3 + JobLevel.x2 + JobInvolvement.x3 , family = "binomial", data = train)
summary(model_17)
sort(vif(model_17))
			 
#Removing JobRole.xResearch.Scientist 			 
model_18<- glm(formula = Attrition ~ Age + MaritalStatus + TotalWorkingYears + TrainingTimesLastYear     
              + YearsSinceLastPromotion + YearsWithCurrManager + medianTime + SatisfactionScore +
			  BusinessTravel.xTravel_Frequently +JobRole.xManufacturing.Director +
			  JobRole.xResearch.Director  +JobRole.xSales.Executive + JobSatisfaction.x2 +  
			  WorkLifeBalance.x2 + WorkLifeBalance.x3 + JobLevel.x2 + JobInvolvement.x3 , family = "binomial", data = train)
summary(model_18)
sort(vif(model_18))
			  
#Removing JobSatisfaction.x2			  
model_19<- glm(formula = Attrition ~ Age + MaritalStatus + TotalWorkingYears + TrainingTimesLastYear     
              + YearsSinceLastPromotion + YearsWithCurrManager + medianTime + SatisfactionScore +
			  BusinessTravel.xTravel_Frequently +JobRole.xManufacturing.Director +
			  JobRole.xResearch.Director  +JobRole.xSales.Executive + WorkLifeBalance.x2 + WorkLifeBalance.x3 + JobLevel.x2 
			  + JobInvolvement.x3 , family = "binomial", data = train)
summary(model_19)
sort(vif(model_19))
			  
#Removing JobInvolvement.x3
model_20<- glm(formula = Attrition ~ Age + MaritalStatus + TotalWorkingYears + TrainingTimesLastYear     
              + YearsSinceLastPromotion + YearsWithCurrManager + medianTime + SatisfactionScore +
			  BusinessTravel.xTravel_Frequently +JobRole.xManufacturing.Director +
			  JobRole.xResearch.Director  +JobRole.xSales.Executive + WorkLifeBalance.x2 + WorkLifeBalance.x3
			  + JobLevel.x2 , family = "binomial", data = train)
summary(model_20)
sort(vif(model_20))

#Removing JobLevel.x2 			  
model_21<- glm(formula = Attrition ~ Age + MaritalStatus + TotalWorkingYears + TrainingTimesLastYear     
              + YearsSinceLastPromotion + YearsWithCurrManager + medianTime + SatisfactionScore +
			  BusinessTravel.xTravel_Frequently +JobRole.xManufacturing.Director +
			  JobRole.xResearch.Director  +JobRole.xSales.Executive + WorkLifeBalance.x2 
			  + WorkLifeBalance.x3, family = "binomial", data = train)
summary(model_21)
sort(vif(model_21))
			  
#Removing JobRole.xSales.Executive 
model_22<- glm(formula = Attrition ~ Age + MaritalStatus + TotalWorkingYears + TrainingTimesLastYear     
              + YearsSinceLastPromotion + YearsWithCurrManager + medianTime + SatisfactionScore +
			  BusinessTravel.xTravel_Frequently +JobRole.xManufacturing.Director +
			  JobRole.xResearch.Director + WorkLifeBalance.x2 + WorkLifeBalance.x3, family = "binomial", data = train)			  
summary(model_22)
sort(vif(model_22))

#Removing JobRole.xResearch.Director 
model_23<- glm(formula = Attrition ~ Age + MaritalStatus + TotalWorkingYears + TrainingTimesLastYear     
              + YearsSinceLastPromotion + YearsWithCurrManager + medianTime + SatisfactionScore +
			  BusinessTravel.xTravel_Frequently +JobRole.xManufacturing.Director+ WorkLifeBalance.x2 + WorkLifeBalance.x3, family = "binomial", data = train)	
summary(model_23)
sort(vif(model_23))

#Removing TrainingTimesLastYear
model_24<- glm(formula = Attrition ~ Age + MaritalStatus + TotalWorkingYears 
              + YearsSinceLastPromotion + YearsWithCurrManager + medianTime + SatisfactionScore +
			  BusinessTravel.xTravel_Frequently +JobRole.xManufacturing.Director+ WorkLifeBalance.x2 + WorkLifeBalance.x3, family = "binomial", data = train)

summary(model_24)
sort(vif(model_24))

########################################################################
# With 11 significant variables in the model

final_model<- model_24
#######################################################################

### Model Evaluation

### Test Data ####

#predicted probabilities of Attrition 1 for test data


test_pred = predict(final_model, type = "response", 
                    newdata = test[])
					
summary(test_pred)

test$prob <- test_pred
# Let's use the probability cutoff of 50%.

test_pred_attrition<-factor(ifelse(test_pred>=0.50,"Yes","No"))
test_actual_attrition <- factor(ifelse(test$Attrition==1,"Yes","No"))

table(test_actual_attrition,test_pred_attrition)
			  
############################################################

test_pred_attrition <- factor(ifelse(test_pred >= 0.40, "Yes", "No"))
test_conf <- confusionMatrix(test_pred_attrition, test_actual_attrition, positive = "Yes")
test_conf

perform_fn <- function(cutoff) 
{
  predicted_attrition <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_attrition, test_actual_attrition, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

summary(test_pred)

s = seq(.01,.80,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 


plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.01)]


# Let's choose a cutoff value of 0.21 for final model

test_cutoff_attrition <- factor(ifelse(test_pred >=0.21, "Yes", "No"))

conf_final <- confusionMatrix(test_cutoff_attrition, test_actual_attrition, positive = "Yes")

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]

acc

sens

spec

##################################################################################################
### KS -statistic - Test Data ######

test_cutoff_attrition <- ifelse(test_cutoff_attrition=="Yes",1,0)
test_actual_attrition <- ifelse(test_actual_attrition=="Yes",1,0)

library(ROCR)
#on testing  data
pred_object_test<- prediction(test_cutoff_attrition, test_actual_attrition)

performance_measures_test<- performance(pred_object_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test)

####################################################################
# Lift & Gain Chart 

# plotting the lift chart

lift <- function(labels , predicted_prob,groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(Observations = n(),
                                     Attrition=sum(., na.rm = TRUE))) %>%
    
    mutate(Cumulative_Attrition = cumsum(Attrition),
           Gain=Cumulative_Attrition/sum(Attrition)*100,
           Lift=Gain/(bucket*(100/groups))) 
  return(gaintable)
}

Attrition_decile = lift(test_actual_attrition, test_pred, groups = 10)
View(Attrition_decile)