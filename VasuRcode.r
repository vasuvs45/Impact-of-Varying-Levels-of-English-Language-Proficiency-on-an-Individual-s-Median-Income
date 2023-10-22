dataframe = read.csv("Final_DataSet.csv")
dim(dataframe)  
colnames(dataframe)
dataframe <- subset(dataframe, select = -X)
# colnames(dataframe) <- c("PUMS person weight", "Income", "Age","White recode",
#                          "Black or African American recode","Marital status","SEX","Educational attainment",
#                          "State","English Speaking Ability","Pacific Islander recode","Native Hawaiian recode",
#                          "American Indiana Alaska Native",
#                          "other race recode","Asian recode","Standard Occupational Classification")
colnames(dataframe)

#Data Cleaning Part
dataframe<-subset(dataframe, POWSP!="N")
dim(dataframe)
dataframe <- subset(dataframe, POWSP < "072")
dim(dataframe)
dataframe<-subset(dataframe, SOCP!="N")
dim(dataframe)
dataframe <- subset(dataframe, PINCP >= 0)
dim(dataframe)

colnames(dataframe)
#Marrital Status
dataframe$Married=ifelse(dataframe$MAR==1,1,0)
dataframe$Widowed=ifelse(dataframe$MAR==2,1,0)
dataframe$Divorced=ifelse(dataframe$MAR==3,1,0)
dataframe$Separated=ifelse(dataframe$MAR==4,1,0)
dataframe$Single=ifelse(dataframe$MAR==5,1,0)#Never Married or under 15 yrs old
dim(dataframe) 
colnames(dataframe)
# Create a new data frame with values from 16 to 24 in the SCHL column
dataframe <- subset(dataframe, SCHL >= 15 & SCHL <= 24 & SCHL!=17)
dim(dataframe) 

dataframe$HighSchool <- ifelse(dataframe$SCHL %in% c(15, 16),1,0)
dataframe$CollegeDroput <-ifelse(dataframe$SCHL %in% c(18, 19),1,0)
dataframe$Associate <-ifelse(dataframe$SCHL %in% c(20),1,0)
dataframe$Bachelor <-ifelse(dataframe$SCHL %in% c(21),1,0)
dataframe$Master <-ifelse(dataframe$SCHL %in% c(22,23),1,0)
dataframe$Doctorate <-ifelse(dataframe$SCHL %in% c(24),1,0)

dim(dataframe)
colnames(dataframe)
#dataframe$SOCP_group_value <- as.numeric(dataframe$SOCP_group_value)

dataframe$Managers=     ifelse(grepl("^11", dataframe$SOCP),1,0)
dataframe$Business=     ifelse(grepl("^131", dataframe$SOCP),1,0)
dataframe$Financial=   	ifelse(grepl("^132", dataframe$SOCP),1,0)
dataframe$IT=          	ifelse(grepl("^15", dataframe$SOCP),1,0)
dataframe$Engineering= 	ifelse(grepl("^17", dataframe$SOCP), 1,0)
dataframe$Science=     	ifelse(grepl("^19", dataframe$SOCP), 1,0)
dataframe$Coucelling=  	ifelse(grepl("^21", dataframe$SOCP), 1,0)
dataframe$Legal=       	ifelse(grepl("^23", dataframe$SOCP), 1,0)
dataframe$Education=   	ifelse(grepl("^25", dataframe$SOCP), 1,0)
dataframe$Entertainment=	ifelse(grepl("^27", dataframe$SOCP), 1,0)
dataframe$Medical=     	ifelse(grepl("^29", dataframe$SOCP), 1,0)
dataframe$HealthCare=  	ifelse(grepl("^31", dataframe$SOCP), 1,0)
dataframe$PRT=         	ifelse(grepl("^33", dataframe$SOCP), 1,0)
dataframe$EATERY=      	ifelse(grepl("^35", dataframe$SOCP), 1,0)
dataframe$CLN=         	ifelse(grepl("^37", dataframe$SOCP), 1,0)
dataframe$PRS=         	ifelse(grepl("^39", dataframe$SOCP), 1,0)
dataframe$Sales=       	ifelse(grepl("^41", dataframe$SOCP), 1,0)
dataframe$Official=    	ifelse(grepl("^43", dataframe$SOCP), 1,0)
dataframe$FFF=         	ifelse(grepl("^45", dataframe$SOCP), 1,0)
dataframe$CON=         	ifelse(grepl("^474", dataframe$SOCP),1,0)
dataframe$EXT=         	ifelse(grepl("^475", dataframe$SOCP),1,0)
dataframe$RPR=         	ifelse(grepl("^49", dataframe$SOCP), 1,0)
dataframe$Production=  	ifelse(grepl("^51", dataframe$SOCP), 1,0)
dataframe$TRN=         	ifelse(grepl("^53", dataframe$SOCP), 1,0)
dataframe$MIL=         	ifelse(grepl("^55", dataframe$SOCP), 1,0)

dim(dataframe)
colnames(dataframe)
dataframe$OnlyEnglish=ifelse(dataframe$ENG==0,1,0)
dataframe$EnglishVeryWell=ifelse(dataframe$ENG==1,1,0)
dataframe$EnglishWell=ifelse(dataframe$ENG==2,1,0)
dataframe$EnglishNotWell=ifelse(dataframe$ENG==3,1,0)
dataframe$EnglishNotAtAll=ifelse(dataframe$ENG==4,1,0)
dim(dataframe)
colnames(dataframe)

#For analysis Perspective, better to go ahead with shorter Names only. Thats why not renamed.
# colnames(dataframe) <- c("PUMS person weight", "Income", "Age","White recode",
#                          "Black or African American recode","Marital status","SEX","Educational attainment",
#                          "State","English Speaking Ability","Pacific Islander recode","Native Hawaiian recode",
#                          "American Indiana Alaska Native",
#                          "other race recode","Asian recode","Standard Occupational Classification")

colnames(dataframe)
hist(dataframe$PINCP)
dataframe$transformed_income<-log(dataframe$PINCP+1)
hist(dataframe$transformed_income)
colnames(dataframe)

write.csv(dataframe,file = "tranformed.csv",row.names = FALSE)
