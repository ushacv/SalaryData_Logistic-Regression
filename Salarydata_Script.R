1.
# Loading the raw Data
SalaryData=read.csv('C:/Users/harsha/Desktop/Stats+R/CSV Files/SalaryData .csv',
                    na.strings = c(""," ","NA","NULL"), stringsAsFactors = T)
SalaryData
View(SalaryData)

2.
# number of observations and variable
dim(SalaryData)

summary(SalaryData)

3.
# Exploring the data set

str(SalaryData)
head(SalaryData)

4.

# convert the categorical variables to factors if they are

SalaryData$SalaryGT50K=as.factor(SalaryData$SalaryGT50K)
str(SalaryData)


5.
# Checking and treating missing values

colSums(is.na(SalaryData))

# imputing missing values for categorical columns -central tendency is 
#measured by mode

#Workclass

FunctionMode=function(inpData){
  ModeValue=names(table(inpData)[table(inpData)==max(table(inpData))])
  return(ModeValue)
}

FunctionMode(SalaryData$workclass)

#Imputing Missing values for categorical columns

SalaryData$workclass[is.na(SalaryData$workclass)]="Private"

#Now, workclass will have 0 MV

colSums(is.na(SalaryData))

#Occupation

FunctionMode=function(inpData){
  ModeValue=names(table(inpData)[table(inpData)==max(table(inpData))])
  return(ModeValue)
}

FunctionMode(SalaryData$occupation)

#Imputing Missing values for categorical columns

SalaryData$occupation[is.na(SalaryData$occupation)]="Exec-managerial"

#Now, occupation will have 0 MV

colSums(is.na(SalaryData))


6.
# Clubbing different Name in similar Category of name  :

table(SalaryData$native_country)
south_america = c("Columbia", "Ecuador", "Peru")

SalaryData$native_country=as.character(SalaryData$native_country)
south_america = c("Columbia", "Ecuador", "Peru")
SalaryData$native_country[SalaryData$native_country %in% south_america] = "south_america"
table(SalaryData$native_country)

asia = c ("Cambodia", "China", "Hong", "India", "Iran", "Japan", "Laos",
             "Philippines", "Taiwan", "Thailand", "Vietnam")


SalaryData$native_country=as.character(SalaryData$native_country)
asia = c ("Cambodia", "China", "Hong", "India", "Iran", "Japan", "Laos",
             "Philippines", "Taiwan", "Thailand", "Vietnam")
SalaryData$native_country[SalaryData$native_country %in% asia] = "asia"


europe = c("England", "France", "Germany", "Greece","Hungary", "Ireland",
               "Italy", "Poland", "Portugal", "Scotland","Yugoslavia")
               

SalaryData$native_country=as.character(SalaryData$native_country)
europe = c("England", "France", "Germany", "Greece","Hungary", "Ireland",
           "Italy", "Poland", "Portugal", "Scotland","Yugoslavia")
SalaryData$native_country[SalaryData$native_country %in% europe] = "europe"


north_america = c("Canada", "Cuba", "Dominican-Republic", "El-Salvador", "Guatemala",
                     "Haiti", "Honduras", "Jamaica", "Mexico", "Nicaragua",
                  "Outlying-US(Guam-USVI-etc)", "Puerto-Rico", "Trinadad&Tobago",
                        "United-States")
  


table(SalaryData$native_country)
SalaryData$native_country=as.factor(SalaryData$native_country)

str(SalaryData)
colSums(is.na(SalaryData))

# Treating Missing value of Native_Country

FunctionMode=function(inpData){
  ModeValue=names(table(inpData)[table(inpData)==max(table(inpData))])
  return(ModeValue)
}

FunctionMode(SalaryData$native_country)

#Imputing Missing values for categorical columns

SalaryData$native_country[is.na(SalaryData$native_country)]="north_america"

colSums(is.na(SalaryData))

7.
# Target Variable - "Salary" categorical in nature ---- Logistic Regression 

#############################################################################
8.

# Explore each "Potential" predictor for distribution and Quality

#Univariate Analysis 

#Exploring Multiple continuous column -- Histogram

ColsForHist = c("age","hours_per_week")
  
par(mfrow=c(1,2))  
  
for (hist_cols in ColsForHist) {
  hist(SalaryData[ ,c(hist_cols)], main = paste('Histogram of:',hist_cols),
       col = brewer.pal(8,"Paired"))
} 

library(RColorBrewer)  
  
# Exploring Multiple Categorical column - Bar Plot  

ColsForBar = c('SalaryGT50K','workclass','education','marital_status','occupation','race',
               'sex','native_country')

par(mfrow=c(2,4))

for (bar_cols in ColsForBar) {
  barplot(table(SalaryData[ ,c(bar_cols)]), main = paste('Barplot of:' , bar_cols),
          col = brewer.pal(8,"Paired"))
}
  

##########################################################################
9.
# Bivariate Analysis
## Visual Relationship between predictors and target variable

# Categorical Vs Continuous  == Box Plot
ColsForBox = c("age","hours_per_week")
par(mfrow=c(1,2))

for (Box_cols in ColsForBox) {
   boxplot((SalaryData[ ,c(Box_cols)]~SalaryGT50K), data = SalaryData,
              main=paste('Box plot of:' ,Box_cols),
             Col=brewer.pal(8,"Paired"))
}
#################  OR ###################
par(mfrow=c(1,1))
boxplot(age~SalaryGT50K, data = SalaryData, col = brewer.pal(8,"Paired"))

par(mfrow=c(1,1))
boxplot(hours_per_week~SalaryGT50K, data = SalaryData, Col = brewer.pal(8,"Paired"))

# Categorical Vs Categorical Visual analysis: Grouped Bar chart

table(SalaryData$workclass)

par(mfrow=c(2,2))

crossTabResult = table(SalaryData[ ,c('SalaryGT50K','workclass')])
barplot(crossTabResult, beside = T, col = c("Red","Green"))
                                      
                                      
crossTabResult = table(SalaryData[ ,c('SalaryGT50K','education')])
barplot(crossTabResult, beside = T, col = c("Red","Green"))


crossTabResult = table(SalaryData[ ,c('SalaryGT50K','occupation')])
barplot(crossTabResult, beside = T, col = c("Red","Green"))

crossTabResult = table(SalaryData[ ,c('SalaryGT50K','native_country')])
barplot(crossTabResult, beside = T, col = c("Red","Green"))

#### Above plotting helps to understand that - Salary is highly affected by location
## where person stays and later on entity of work and then education and Position hold by him.


##############################################################################
10.
#strength of relationship between predictors and target variables 

### Continuous Vs Categorical relationship strength: ANOVA ######

# Null - Hypothesis : the variables are not correlated 

summary(aov(age ~ SalaryGT50K, data = SalaryData))

summary(aov(hours_per_week ~ SalaryGT50K, data = SalaryData))

## In both cases , we reject the P value and says that Age and hours_per_week
# is highly correlated with Target variable salary.

#(Note: unable to form loop as Target Variable is categorical in nature.)

######## Categorical Vs Categorical -- Chi-square test##############
  #H0:the two columns are not correlated

chisq.test(crossTabResult)

Chisqcols = c("workclass","education","marital_status",
                "occupation","race","sex","native_country")


for (Chi_Cols in Chisqcols) {
    crossTabResult=table(SalaryData[ ,c('SalaryGT50K',Chi_Cols)])
    ChisResult = chisq.test(crossTabResult)
    print(Chi_Cols)
    print(ChisResult)
}

# all above variable have very low p Value hence, we reject the null hypothesis
#and say all above variables are highly correlated with Target variable SalaryGT50K

###############################################################################
11.
#Generating data for Machine Learning

InputData = SalaryData
TargetVariableName = 'SalaryGT50K'
TargetVariableName

BestPredictorName=names(InputData[, !names(InputData) %in% TargetVariableName])
BestPredictorName

TargetVariable = InputData[ ,c(TargetVariableName)]
TargetVariable
str(TargetVariable)

#selecting all other columns as predictors apart from target variable
PredictorVariable=InputData[, BestPredictorName]
PredictorVariable
str(PredictorVariable)

##Creating the final data to be used for ML
DataForML=data.frame(TargetVariable,PredictorVariable) 
str(DataForML)
head(DataForML)

##################################################################################
12.
#Sampling| Splitting data into 70% for training 30% for testing 

set.seed(123)

TrainingSampleIndex=sample(1:nrow(DataForML), size = 0.7*nrow(DataForML))
length(TrainingSampleIndex)

DataForMLTrain = DataForML[TrainingSampleIndex, ]
DataForMLTrain

DataForMLTest = DataForML[ - TrainingSampleIndex, ]
DataForMLTest

dim(DataForMLTrain)
dim(DataForMLTest)

###############################################################################
13.
## Creating Predictive models on training data to check the accuracy of each algorithm
#Logistic Regression 

startTime=Sys.time()

LR_Model_1=glm(TargetVariable ~., data = DataForMLTrain, family ='binomial')
summary(LR_Model_1)

endTime=Sys.time()
endTime-startTime


#(occupation Armed-Forces - 0.992220)
LR_Model_2=glm(TargetVariable ~age+workclass+education+marital_status+race+
                 sex+hours_per_week+native_country+
                 I(occupation=='Craft-repair')+I(occupation=='Farming-fishing')+
                 I(occupation=='Handlers-cleaners')+I(occupation=='Machine-op-inspct')
               +I(occupation=='Other-service')+I(occupation=='Priv-house-serv')+
                 I(occupation=='Protective-serv')+I(occupation=='Sales')+
                 I(occupation=='Tech-support')+I(occupation=='Transport-moving')
               +I(occupation=='Exec-managerial'), data = DataForMLTrain, family ='binomial')
summary(LR_Model_2)


#(occupation - NA except Exec-managerial)
LR_Model_3=glm(TargetVariable ~age+workclass+education+marital_status+race+
                 sex+hours_per_week+native_country+
                 I(occupation=='Exec-managerial'), data = DataForMLTrain, family ='binomial')
summary(LR_Model_3)


#(workclass = Jobless (0.9883))
LR_Model_4=glm(TargetVariable ~age+education+marital_status+race+
                 sex+hours_per_week+native_country+
                 I(occupation=='Exec-managerial')+
                 I(workclass=='Private')+I(workclass=='SelfEmployed')+
                 I(workclass=='state-gov'), data = DataForMLTrain, family ='binomial')
summary(LR_Model_4)


#(education = preschool (0.95587))
LR_Model_5=glm(TargetVariable ~age+marital_status+race+
                 sex+hours_per_week+native_country+
                 I(occupation=='Exec-managerial')+
                 I(workclass=='Private')+I(workclass=='SelfEmployed')+
                 I(workclass=='state-gov')+
                 I(education=='11th')+I(education=='12th')+I(education=='1st-4th')+
                 I(education=='5th-6th')+I(education=='7th-8th')+I(education=='9th')+
                 I(education=='Bachelors')+I(education=='Doctorate')+I(education=='Masters')
               +I(education=='Prof-school'), data = DataForMLTrain, family ='binomial')
summary(LR_Model_5)

#(education)
LR_Model_6=glm(TargetVariable ~age+marital_status+race+
                 sex+hours_per_week+native_country+
                 I(occupation=='Exec-managerial')+
                 I(workclass=='Private')+I(workclass=='SelfEmployed')+
                 I(workclass=='state-gov')
                 , data = DataForMLTrain, family ='binomial')
summary(LR_Model_6)

#(native_Country = Europe)
LR_Model_7=glm(TargetVariable ~age+marital_status+race+
                 sex+hours_per_week+
                 I(occupation=='Exec-managerial')+
                 I(workclass=='Private')+I(workclass=='SelfEmployed')+
                 I(workclass=='state-gov')+I(native_country=='north_america')
               +I(native_country=='south_america')
               , data = DataForMLTrain, family ='binomial')
summary(LR_Model_7)


#(race = other)
LR_Model_8=glm(TargetVariable ~age+marital_status+I(race=='Asian-Pac-Islander')+
                 I(race=='Black')+I(race=='White')+
                 sex+hours_per_week+
                 I(occupation=='Exec-managerial')+
                 I(workclass=='Private')+I(workclass=='SelfEmployed')+
                 I(workclass=='state-gov')+I(native_country=='north_america')
               +I(native_country=='south_america')
               , data = DataForMLTrain, family ='binomial')
summary(LR_Model_8)

#(race = NA)
LR_Model_9=glm(TargetVariable ~age+marital_status+
                sex+hours_per_week+
                 I(occupation=='Exec-managerial')+
                 I(workclass=='Private')+I(workclass=='SelfEmployed')+
                 I(workclass=='state-gov')+I(native_country=='north_america')
               +I(native_country=='south_america')
               , data = DataForMLTrain, family ='binomial')
summary(LR_Model_9)


#(native_country = North_america )
LR_Model_10=glm(TargetVariable ~age+marital_status+
                 sex+hours_per_week+
                 I(occupation=='Exec-managerial')+
                 I(workclass=='Private')+I(workclass=='SelfEmployed')+
                 I(workclass=='state-gov')
               +I(native_country=='south_america')
               , data = DataForMLTrain, family ='binomial')
summary(LR_Model_10)


#(native_country = south_america )
LR_Model_11=glm(TargetVariable ~age+marital_status+
                  sex+hours_per_week+
                  I(occupation=='Exec-managerial')+
                  I(workclass=='Private')+I(workclass=='SelfEmployed')+
                  I(workclass=='state-gov')
                , data = DataForMLTrain, family ='binomial')
summary(LR_Model_11)


#(marital_status = widowed )
LR_Model_12=glm(TargetVariable ~age+I(marital_status=='Never-married')+
                  I(marital_status=='Separated')+I(marital_status=='Married')+
                  sex+hours_per_week+
                  I(occupation=='Exec-managerial')+
                  I(workclass=='Private')+I(workclass=='SelfEmployed')+
                  I(workclass=='state-gov')
                , data = DataForMLTrain, family ='binomial')
summary(LR_Model_12)


#(marital_status = NA Except Married  )
LR_Model_13=glm(TargetVariable ~age+
                  I(marital_status=='Married')+
                  sex+hours_per_week+
                  I(occupation=='Exec-managerial')+
                  I(workclass=='Private')+I(workclass=='SelfEmployed')+
                  I(workclass=='state-gov')
                , data = DataForMLTrain, family ='binomial')
summary(LR_Model_13)

##############################################################################
14.
# Checking Accuracy of model on Testing data

PredictionProb=predict(LR_Model_13,DataForMLTest, type = 'response')
PredictionProb

##############################################################################
15.
## Creating the Confusion Matrix to calculate overall accuracy, 
#precision and recall on TESTING data

library(caret)
#Iteration 

IterationData=data.frame(
  Threshold=numeric(0),
  Accuracy=numeric(0)
)

thresholds=seq(0.5,0.7,0.01)

for (i in thresholds) {
  
DataForMLTest$Prediction=ifelse(PredictionProb>i, 1, 0)
DataForMLTest$Prediction=as.factor(DataForMLTest$Prediction)
AccuracyResults=confusionMatrix(DataForMLTest$Prediction, DataForMLTest$TargetVariable,
                                mode = "prec_recall")
IterationData=rbind(IterationData,data.frame(
  Threshold=i,
  Accuracy=round(100* AccuracyResults[['overall']][1])))
}

IterationData

IterationData[IterationData$Accuracy==max(IterationData$Accuracy), ]

#considering a threshold of 0.58

DataForMLTest$Prediction=ifelse(PredictionProb>0.58, 1, 0)
DataForMLTest$Prediction=as.factor(DataForMLTest$Prediction)
head(DataForMLTest)

AccuracyResults=confusionMatrix(DataForMLTest$Prediction, DataForMLTest$TargetVariable,
                                mode = "prec_recall")

AccuracyResults[['table']]
AccuracyResults[['byClass']]
AccuracyResults[['overall']][1]

print(paste('### Overall Accuracy of Logistic Reg Model is:', round(100 * AccuracyResults[['overall']][1]), '%'))

##############################################################################
###################Decision Tree Model ######################

library(party)

DT_Model_1=ctree(TargetVariable~.,data = DataForMLTrain)
DT_Model_1
plot(DT_Model_1)

# Checking Accuracy of model on Testing data
DataForMLTest$Prediction=predict(DT_Model_1, DataForMLTest)
head(DataForMLTest)


# Creating the Confusion Matrix to calculate overall accuracy, precision and recall on TESTING data
library(caret)
AccuracyResults=confusionMatrix(DataForMLTest$Prediction, DataForMLTest$TargetVariable,
                                mode = "prec_recall")

# Since AccuracyResults is a list of multiple items, fetching useful components only
AccuracyResults[['table']]
AccuracyResults[['byClass']]

print(paste('### Accuracy of Logistic Reg Model is:' ,round(100* AccuracyResults[['overall']][1]), '%'))
