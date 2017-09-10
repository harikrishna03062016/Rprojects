#If one is able to identify these risky loan applicants, 
#then such loans can be reduced thereby cutting down the amount of credit loss.
#Identification of such applicants using EDA is the aim of this case study.
#In other words, the company wants to understand the driving factors
#(or driver variables) behind loan default, i.e. the variables which are 
#strong indicators of default.  The company can utilise this knowledge for 
#its portfolio and risk assessment. 

#Cleaning Environment
rm(list = ls())

#Loading Required Packages
library(dplyr)
library(ggplot2)
library(stringr)
library(lubridate)
library(tidyr)

#----------
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
  
  
  
}

remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}


#End of Custom Functions
#------------


#---------
#Data Sourcing
#Reading Data from a csv file
loanData <- read.delim("loan.csv", sep = ",", stringsAsFactors = FALSE)
dim(loanData)
str(loanData)
View(loanData)  


#Data Cleaning
#Removing columns having only NA Data
#plot displaying columns having NA values
breaks = 40**(1:10)
plotDataNABar(loanData)+scale_y_log10(breaks = breaks)

indexVector <- vector(mode="numeric", length=0)

for(i in 1:length(loanData)){
  if(!all(is.na(loanData[,i]))){
    indexVector <- c(indexVector,i)
  }
  
}

print(paste("Removing ", length(loanData)-length(indexVector)," Columns as they only have NA values"))
loanData <- loanData[indexVector]
plotDataNABar(loanData)+scale_y_log10(breaks = breaks)

#Removing columns having a same values in entire column
indexVector <- vector(mode="numeric", length=0)

for(i in 1:length(loanData)){

    if((!any(is.na(loanData[,i])))&all(loanData[,i]==loanData[1,i])){
    indexVector <- c(indexVector,i)
  }
  
}

print(paste("Removing ", length(indexVector)," Columns as they have same data throughout the column "))

loanData <- loanData[-indexVector]
length(loanData)


plotDataNABar(loanData)+scale_y_log10(breaks = breaks)
length(loanData)

#Removing columns having NA values and one single value
indexVector <- vector(mode="numeric", length=0)

for(i in 1:length(loanData)){
 
  if(all(na.omit(loanData[,i])==na.omit(loanData[,i])[1])){
    indexVector <- c(indexVector,i)
  }
  
}
print(paste("Removing ", length(indexVector)," Columns as they have same data throughout the column "))

loanData <- loanData[-indexVector]

plotDataNABar(loanData)+scale_y_log10(breaks = breaks)
length(loanData)

dim(loanData)
#------
#Removing columns with majority values as NA
loanData <- subset(loanData, select=-c(mths_since_last_delinq,mths_since_last_record))

#Replacing NAs with mean in pub_rec_bankruptcies
round(mean(loanData$pub_rec_bankruptcies, na.rm = TRUE))
loanData$pub_rec_bankruptcies[which(is.na(loanData$pub_rec_bankruptcies))] <- 0

#Removing id and member_id variables as they are categorical and unique to each loan or applicant
#checking for  duplicates
sum(duplicated(loanData$id))

#No duplicates found
loanData <- subset(loanData, select=-c(id))
loanData <- subset(loanData, select=-c(member_id))

#Filtering loan_status field to either default or non default
length(loanData$loan_status=="Current")
loanData <- filter(loanData, loanData$loan_status!="Current")
loanData <- loanData2 
loanData$default <- sapply(loanData$loan_status,function(x){
  if(x=="Fully Paid"){
    0
  }else{
    1
  }
})

#Cleansing int_rate data
loanData$int_rate <- gsub("%","",loanData$int_rate) %>%as.numeric()

#cleansing date fields
loanData$issue_d <- dmy(gsub(" ","",paste("01-",loanData$issue_d)))

#cleansing data total_acc< open_acc
sum(loanData$total_acc <= loanData$open_acc)
loanData <- loanData[-which(loanData$total_acc < loanData$open_acc),]

#cleansing emp_length field

unique(loanData$emp_length)

loanData$emp_length <- gsub("< 1 year","0",loanData$emp_length)

loanData$emp_length <-gsub("n/a","0",loanData$emp_length)
loanData$emp_length <-gsub("\\+","",loanData$emp_length)
loanData$emp_length <-gsub("years","",loanData$emp_length)
loanData$emp_length <-gsub("year","",loanData$emp_length)
loanData$emp_length<-as.numeric(loanData$emp_length)


#Univariate Analysis
#Removing id and member id columns as they are categorical variables
names(loanData)

#1. loan_amnt - Numerical - Has information on Loan Amount
summary(loanData$loan_amnt)
loanAmntPlot <-ggplot(loanData, aes(loan_amnt))
loanAmntPlot+ geom_histogram(binwidth =1000)

#2.funded_amnt - Numerical - Has information on Funding amount
summary(loanData$funded_amnt)
fundedAmnt <- ggplot(loanData,aes(funded_amnt))
fundedAmnt+geom_histogram(binwidth = 1000)

#diff in loan requested and loan funded
loanData$diffInLoans <- loanData$loan_amnt - loanData$funded_amnt
summary(loanData$diffInLoans)
sum(loanData$diffInLoans>0)
mean(loanData$diffInLoans)
sd(loanData$diffInLoans)

#3.funded_amnt_inv - Has information on amount funded by investors - doesnt influence on borrowers default
summary(loanData$funded_amnt_inv)
fundedAmntInv <- ggplot(loanData,aes(funded_amnt_inv))
fundedAmntInv+geom_histogram(binwidth = 1000)

#4.term - is a loan characteristic - shorter the duration for large loans my influence borrowers default
unique(loanData$term)
table(loanData$term)
prop.table(table(loanData$term))*100
ggplot(loanData, aes(term))+geom_bar()

#5. int_rate - helpful in understanding browser assesment
summary(loanData$int_rate)
table(loanData$int_rate)
ggplot(loanData, aes(int_rate))+geom_bar()
boxplot(loanData$int_rate)

#Removing Outliers
loanData$int_rate <- remove_outliers(loanData$int_rate)
loanData <- loanData[-which(is.na(loanData$int_rate)),]
summary(loanData$int_rate)
ggplot(loanData, aes(int_rate))+geom_bar()
boxplot(loanData$int_rate)

#6. installment - Dti is a better measure than installment so removing it
summary(loanData$installment)
boxplot(loanData$installment)
loanData <- subset(loanData, select=-c(installment))

#7. grade - Required for assesement of the borrower
unique(loanData$grade)
table(loanData$grade)
prop.table(table(loanData$grade))*100
ggplot(loanData,aes(grade))+geom_bar()

#8. sub_grade - Required for assesment of the borrower
unique(loanData$sub_grade)
table(loanData$sub_grade)
prop.table(table(loanData$sub_grade))*100
ggplot(loanData,aes(sub_grade))+geom_bar()+theme(axis.text.x = element_text(angle = 90, hjust = 1))

#9. emp_title - information variable, can be removed
unique(loanData$emp_title)
loanData <- subset(loanData, select=-c(emp_title))


#10. emp_length - helpful in borrowers assesment
unique(loanData$emp_length)
summary(loanData$emp_length)
ggplot(loanData,aes(emp_length))+geom_bar()+ theme(axis.text.x = element_text(angle = 90, hjust = 1))

#11. home_ownership - helpful in borrowers assesment
unique(loanData$home_ownership)
table(loanData$home_ownership)
prop.table(table(loanData$home_ownership))*100
ggplot(loanData,aes(home_ownership))+geom_bar()

#Merging Mortgage and Own
#loanData$home_ownership[which(loanData$home_ownership=="MORTGAGE")]<- "OWN"
#Grouping None to Other
loanData$home_ownership <- gsub("NONE","OTHER", loanData$home_ownership)
unique(loanData$home_ownership)
table(loanData$home_ownership)
prop.table(table(loanData$home_ownership))*100
ggplot(loanData,aes(home_ownership))+geom_bar()

#12. annual_inc - helpful in borrowers assesment
head(loanData$annual_inc)
summary(loanData$annual_inc)
ggplot(loanData, aes(annual_inc))+geom_density()
boxplot(loanData$annual_inc)
quantile(loanData$annual_inc, seq(0,1,.1))
#Removing Outliers
loanData$annual_inc <- remove_outliers(loanData$annual_inc)

#Removing NA Data
loanData <- loanData[-which(is.na(loanData$annual_inc)),]
boxplot(loanData$annual_inc)


#13. verification_status - helpful in borrowers assesment
unique(loanData$verification_status)
table(loanData$verification_status)
prop.table(table(loanData$verification_status))*100
ggplot(loanData, aes(verification_status))+geom_bar()

#14. issue_d 
head(loanData$issue_d)
dateData <- group_by(loanData,year(loanData$issue_d))
summarise(dateData, sum(dateData$loan_amnt))
ggplot(loanData, aes(year(loanData$issue_d)))+geom_bar()

#15. loan_status
unique(loanData$loan_status)
table(loanData$loan_status)
prop.table(table(loanData$loan_status))*100
ggplot(loanData,aes(loan_status ))+geom_bar()

#16. url - has no impact on borrowers default
loanData <- subset(loanData, select=-c(url))

#17. desc- has no impact on borrowers default
loanData <- subset(loanData, select=-c(desc))

#18. purpose - helpful in understanding the loan characteristic 
unique(loanData$purpose)
table(loanData$purpose)
prop.table(table(loanData$purpose))*100
ggplot(loanData, aes(purpose))+geom_bar()+ theme(axis.text.x = element_text(angle = 90, hjust = 1))

#19. title - Understanding the behaviour of the title length on default 
class(loanData$title)
loanData$title <- sapply(loanData$title, function(x){
  nchar(x)
})

#20. zip_code - has no influence on borrowers default
summary(loanData$addr_state) 
loanData <- subset(loanData, select=-c(zip_code))

#21. addr_state  
head(loanData$addr_state) 
table(loanData$addr_state)

ggplot(loanData, aes(addr_state))+geom_bar()+theme(axis.text.x = element_text(angle = 90, hjust = 1))

#22. dti - debt to income ratio - helpful in borrowers assesment
summary(loanData$dti)
boxplot(loanData$dti)

str(loanData)
#23. delinq_2yrs - helpful in understanding borrowers credit history
summary(loanData$delinq_2yrs)
table(loanData$delinq_2yrs)
ggplot(loanData, aes(delinq_2yrs))+geom_bar()

#24. earliest_cr_line
summary(loanData$earliest_cr_line)
head(loanData$earliest_cr_line)
loanData$earliest_cr_line <- gsub(" ","",paste("1-",loanData$earliest_cr_line))
loanData$earliest_cr_line <- dmy(loanData$earliest_cr_line)

#25. inq_last_6mths - helpful in understanding borrowers credit history
summary(loanData$inq_last_6mths)
table(loanData$inq_last_6mths)
ggplot(loanData, aes(inq_last_6mths))+geom_bar()

#26. open_acc - helpful in understanding borrowers credit history
summary(loanData$open_acc)
table(loanData$open_acc)
ggplot(loanData, aes(open_acc))+geom_bar()

#27. pub_rec - helpful in understanding borrowers credit history
summary(loanData$pub_rec)
table(loanData$pub_rec_bankruptcies)
ggplot(loanData, aes(pub_rec))+geom_bar()

#28.revol_bal - credit revolving balance - portion of credit card spending that goes unpaid at the end of a billing cycle
loanData <- subset(loanData, select=-c(revol_bal))

#29.revol_util
summary(loanData$revol_util)
loanData$revol_util<-gsub("%","",loanData$revol_util)
loanData$revol_util<-as.numeric(loanData$revol_util)

#30.total_acc
summary(loanData$total_acc)
table(loanData$total_acc)
ggplot(loanData, aes(total_acc))+geom_bar()

#31.out_prncp-Removing it as it is a post loan parameter and may not be helpful in our current analysis 
loanData <- subset(loanData, select=-c(out_prncp))

#32.out_prncp_inv - Removing it as it is a post loan parameter and may not be helpful in our current analysis
loanData <- subset(loanData, select=-c(out_prncp_inv))

#33.total_pymnt - post loan parameter and can be removed
summary(loanData$total_pymnt)
loanData <- subset(loanData, select=-c(total_pymnt))

#34.total_pymnt_inv - Removed as it is a calculation field  
loanData <- subset(loanData, select=-c(total_pymnt_inv))

#35.total_rec_prncp - Its a calculated field of total_pymnt and can be removed
loanData <- subset(loanData, select=-c(total_rec_prncp))

#36.total_rec_int - Its a calculated field of total_pymnt and can be removed
loanData <- subset(loanData, select=-c(total_rec_int))

#37.total_rec_late_fee Removing it as it is a post loan parameter and may not be helpful in our current analysis
loanData <- subset(loanData, select=-c(total_rec_late_fee))

#38.recoveries - Post loan parameter so removing it
summary(loanData$recoveries)
loanData <- subset(loanData, select=-c(recoveries))

#39.collection_recovery_fee - Removing as it talks about post charge off loan 
unique(loanData$collection_recovery_fee)
summary(loanData$collection_recovery_fee)
loanData <- subset(loanData, select=-c(collection_recovery_fee))

#40.last_pymnt_d - Removing it as it is a post loan parameter and may not be helpful in our analysis
head(loanData$last_pymnt_d)
unique(loanData$last_pymnt_d)
loanData <- subset(loanData, select=-c(last_pymnt_d))

#41.last_pymnt_amnt - Removing it as it is a post loan parameter and may not be helpful in our analysis
loanData <- subset(loanData, select=-c(last_pymnt_amnt))

#42.next_pymnt_d - can be removed, as it talks about post loan payment schedules  
loanData <- subset(loanData, select=-c(next_pymnt_d))

#43.last_credit_pull_d - Column is specific to a user credit report pulled date
loanData <- subset(loanData, select=-c(last_credit_pull_d))

#44.pub_rec_bankruptcies
head(loanData$pub_rec_bankruptcies)
summary(loanData$pub_rec_bankruptcies)
table(loanData$pub_rec_bankruptcies)
ggplot(loanData, aes(pub_rec_bankruptcies))+geom_bar()+scale_y_log10(breaks = breaks)

#45.length of borrowers credit history
loanData$credit_hist_length <- as.numeric(loanData$issue_d - loanData$earliest_cr_line)

#46. Current Account Ratio
loanData$curr_acc_ratio <- round(loanData$open_acc/loanData$total_acc,2)


str(loanData)
#----------------------
#Bivariate Analysis


#1. loan_amount vs loan_status
loanAmntStatusData <- subset(loanData,select = c(loan_status,loan_amnt))%>% group_by(loan_status)

summarise(loanAmntStatusData, mean(loan_amnt))
summarise(loanAmntStatusData, sd(loan_amnt))
summarise(loanAmntStatusData, median(loan_amnt))

ggplot(loanData, aes(annual_inc, loan_amnt))+geom_smooth()+facet_grid(loan_status~.)


#2. loan_status vs home_ownership
filData <- subset(loanData, select = c(home_ownership,loan_status))
t <-table(filData)
t
statusHomeMatrix <- prop.table(t,2)*100
statusHomeMatrix

statusHomeMatrix2 <- prop.table(t,1)*100
statusHomeMatrix2

df <- group_by(loanData, home_ownership,loan_status)
summarise(df, countX= length(loan_amnt))

loanStatusStat <- group_by(loanData, loan_status)
summarise(loanStatusStat, countX= (length(loan_amnt)/length(loanStatusStat$loan_amnt))*100)

loanStatusIncomeStat <- group_by(loanData, loan_status,home_ownership ,annual_inc)

ggplot(loanData, aes(home_ownership))+geom_bar(fill = "blue", colour = "red", alpha = 0.1,cex=0.7)+facet_grid(loan_status~.)

chisq.test(filData$loan_status,filData$home_ownership)
#Dependednt as p value is very small

ggplot(loanData,aes(issue_d,loan_amnt))+geom_histogram()+facet_grid(loan_status~.)
ggplot(loanData,aes(open_acc,loan_amnt))+geom_histogram(stat= 'identity')+facet_grid(loan_status~.)

#Loan Status vs Term
ggplot(loanData, aes(term, loan_status))+geom_jitter()

termStatus <- subset(loanData, select = c(loan_status,term))
termStatusTable <-table(termStatus)

prop.table(termStatusTable,1)*100
prop.table(termStatusTable,2)*100
prop.table(termStatusTable)*100

#loanStatus vs int_rate
ggplot(loanData, aes(int_rate))+geom_histogram()+facet_grid(loan_status~.)

#grade vs int_rate
ggplot(loanData,aes(grade,int_rate))+geom_bar(stat = 'identity',fill = "blue", colour = "red", alpha = 0.1,cex=0.7)+facet_grid(loan_status~.)

intGradeStatus <- group_by(loanData,loan_status,grade)%>%summarise(intMean = mean(int_rate))


#delinq_2years vs loan grade
names(loanData)

#loan_status Vs Delinq_2years
ggplot(loanData, aes(delinq_2yrs, fill=loan_status))+geom_bar(position="dodge")+scale_y_continuous(breaks=breaks)

ggplot(loanData, aes(grade,delinq_2yrs, fill=loan_status))+geom_bar(stat = "identity",position="dodge")

ggplot(loanData, aes(sub_grade,delinq_2yrs, fill=loan_status))+geom_bar(stat = "identity",position="dodge")

delingVsStatus <- subset(loanData, select = c(loan_status,delinq_2yrs))
delingVsStatusTable <-table(delingVsStatus)
prop.table(delingVsStatusTable)*100
prop.table(delingVsStatusTable,2)*100

#inquires last 6 months
inq6mthsVsStatus <- subset(loanData,select = c(loan_status,inq_last_6mths))
inq6mthsVsStatusTable <- table(inq6mthsVsStatus)
inq6mthsVsStatusTable

prop.table(inq6mthsVsStatusTable)*100
prop.table(inq6mthsVsStatusTable,2)*100

ggplot(loanData, aes(inq_last_6mths, fill=loan_status))+geom_bar(position="dodge")

#Public Records

t <-table(loanData$loan_status,loanData$curr_acc_ratio)
prop.table(t,2)*100
ggplot(loanData, aes(loan_status,curr_acc_ratio))+geom_boxplot()

ggplot(loanData, aes(purpose)) + geom_bar(fill = "blue", colour = "red", alpha = 0.1,cex=0.7)+facet_grid(loan_status~.)+theme(axis.text.x = element_text(angle = 90, hjust = 1))
chisq.test(loanData$loan_status,loanData$purpose)

ggplot(loanData, aes(curr_acc_ratio))+geom_histogram()+facet_grid(loan_status~.)
ggplot(loanData, aes(dti))+geom_histogram()+facet_grid(loan_status~.)
ggplot(loanData, aes(int_rate))+geom_histogram()+facet_grid(loan_status~.)
ggplot(loanData, aes(emp_length))+geom_histogram()+facet_grid(loan_status~.)
ggplot(loanData, aes(home_ownership))+geom_bar()+facet_grid(loan_status~.)
ggplot(loanData, aes(grade))+geom_bar()+facet_grid(loan_status~.)
ggplot(loanData, aes(sub_grade))+geom_bar()+facet_grid(loan_status~.)
ggplot(loanData, aes(revol_util))+geom_histogram()+facet_grid(loan_status~.)
ggplot(loanData, aes(pub_rec))+geom_bar()+facet_grid(loan_status~.)
ggplot(loanData, aes(pub_rec_bankruptcies))+geom_bar()+facet_grid(loan_status~.)
ggplot(loanData, aes(inq_last_6mths))+geom_bar()+facet_grid(loan_status~.)
ggplot(loanData, aes(delinq_2yrs))+geom_bar()+facet_grid(loan_status~.)
ggplot(loanData, aes(annual_inc))+geom_histogram()+facet_grid(loan_status~.)
ggplot(loanData, aes(addr_state))+geom_bar()+facet_grid(loan_status~.)

stateStatus <- table(loanData$loan_status, loanData$addr_state)
prop.table(stateStatus,2)*100

