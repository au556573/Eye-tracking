#----- Load library ------

#install.packages("data.table")
#install.packages("zoo")

#install.packages("lmerTest")
#install.packages("caret")
#install.packages("ModelMetrics")
#install.packages("e1071")
#install.packages("pROC")
#install.packages("MuMIn")

library(dplyr)
library(tidyverse)
library(data.table)
library(ggplot2)
library(stringr)
library(zoo)
library(plyr)
library(lmerTest)
library(caret)
library(ModelMetrics)
library(readr)
library(stats)
library(e1071)
library(pROC) 
library(MuMIn)
library(jpeg)
library(grid)

setwd("C:/Users/Roland/Desktop/Experimental methods/4/Eye tracking")




#------ Read files ------
d_sample = read.csv('SamplesV1.csv', header = T)
d_fix = read.csv('FixationsV1.csv', header = T)
d_sac = read.csv('SaccadesV1.csv', header = T)

#------ Make conditions ------
#Fix
d_fix$VScondition = NA #Creates an empty column. VS stands for Visual Search
d_fix$VScondition[d_fix$SearchOrder==1 & d_fix$Trial < 6] = "Star" #For SO1, T1-T5 is the count cond., From T6 star cond.
d_fix$VScondition[d_fix$SearchOrder==1 & d_fix$Trial > 5] = "Count"
d_fix$VScondition[d_fix$SearchOrder==2 & d_fix$Trial < 6] = "Count"#For SO2 it is the opposite
d_fix$VScondition[d_fix$SearchOrder==2 & d_fix$Trial > 5] = "Star"

#Sac
d_sac$VScondition = NA
d_sac$VScondition[d_sac$SearchOrder==1 & d_sac$Trial < 6] = "Star"
d_sac$VScondition[d_sac$SearchOrder==1 & d_sac$Trial > 5] = "Count"
d_sac$VScondition[d_sac$SearchOrder==2 & d_sac$Trial < 6] = "Count"
d_sac$VScondition[d_sac$SearchOrder==2 & d_sac$Trial > 5] = "Star"

#Sample
d_sample$VScondition = NA
d_sample$VScondition[d_sample$SearchOrder==1 & d_sample$Trial < 6] = "Star"
d_sample$VScondition[d_sample$SearchOrder==1 & d_sample$Trial > 5] = "Count"
d_sample$VScondition[d_sample$SearchOrder==2 & d_sample$Trial < 6] = "Count"
d_sample$VScondition[d_sample$SearchOrder==2 & d_sample$Trial > 5] = "Star"


#------ Logs ------
#read all logs in the old fashioned manner. Renaming suject to participantID.
log1 = read.csv('PupilsLogs\\logfile_1_2_f.csv', header = T) #Read file
log1 = setnames(log1, c('X','subject'), c('Trial','ParticipantID')) #Rename variables to match the other data sets
log1$Trial = log1$Trial+1 #Add 1 to each trial number, as Python starts from 0

log2 = read.csv('PupilsLogs\\logfile_2_1_f.csv', header = T)
log2 = setnames(log2, c('X','subject'), c('Trial','ParticipantID'))
log2$Trial = log2$Trial+1

log3 = read.csv('PupilsLogs\\logfile_3_2_f.csv', header = T)
log3 = setnames(log3, c('X','subject'), c('Trial','ParticipantID'))
log3$Trial = log3$Trial+1

log4 = read.csv('PupilsLogs\\logfile_4_1_f.csv', header = T)
log4 = setnames(log4, c('X','subject'), c('Trial','ParticipantID'))
log4$Trial = log4$Trial+1

log5 = read.csv('PupilsLogs\\logfile_5_2_m.csv', header = T)
log5 = setnames(log5, c('X','subject'), c('Trial','ParticipantID'))
log5$Trial = log5$Trial+1

log6 = read.csv('PupilsLogs\\logfile_6_1_m.csv', header = T)
log6 = setnames(log6, c('X','subject'), c('Trial','ParticipantID'))
log6$Trial = log6$Trial+1

#Merge all log files into one
log_all = bind_rows(log1, log2, log3, log4, log5, log6)

#Get information from Video variable
#Gender
log_all$Gender = substring(log_all$video, first = 1, last = 1) #Gets digits from first to last, here f/m
log_all$Gender[log_all$Gender == 'f']= 'Female' #Rename levels
log_all$Gender[log_all$Gender == 'm']= 'Male'

#Direction
log_all$Direction = substring(log_all$video, first = 9, last = 11) #get 9th to 11th digit (dir, div)

#Ostensiveness
log_all$Ostensive = substring(log_all$video, first = 13, last = 13) #Get 13th digit
log_all$Ostensive[log_all$Ostensive == '+']= 'Ostensive' #Rename levels
log_all$Ostensive[log_all$Ostensive == '-']= 'Non-ostensive'


#------ Merge with Logs ------
#Merge with the three data frames
d_fix = merge(d_fix, log_all, by=c('Trial','ParticipantID'), all = T)
d_sac = merge(d_sac, log_all,by=c('Trial','ParticipantID'), all = T)
d_sample = merge(d_sample, log_all,by=c('Trial','ParticipantID'), all = T)


#------ Visualizations ------
ggplot(d_fix, aes(Duration, color = ParticipantID))+geom_density()+facet_wrap(~Task) #Not normally distributed, long tailed.
ggplot(d_sac, aes(Amplitude, color = ParticipantID))+geom_density()+facet_wrap(~Task)
ggplot(d_fix, aes(PositionX, PositionY))+scale_alpha()+scale_fill_gradient(colours=jet.colors(10),trans='sqrt')

#------ Model intuitions (Fix dur) ------

#lmer(Variable ~ VScondition +(1+VScondition|participantID), data = d_sac)

m=lmer(Duration ~ VScondition +(1+VScondition|ParticipantID), data = d_fix)
summary(m)

m=glmer(Duration ~ VScondition * Trial +(1+VScondition|ParticipantID), data = d_fix, family = gaussian(link = log))
summary(m)

#------ CV fix dur -------
#Fold data 
#Create fold with unique SUBJ
d_fix2 = subset(d_fix, Task == 'VisualSearch') #Only look at Visual search
d_fix2$ParticipantID=as.character(d_fix2$ParticipantID)%>%as.factor()%>%as.numeric() #make P_ID numeric
folds=createFolds(unique(d_fix2$ParticipantID), k = 3)

#Create list of models to try
mlist = c(glmer(Duration ~ VScondition * Trial +(1+VScondition|ParticipantID), data = data_train, family = gaussian(link = log)),
          glmer(Duration ~ VScondition + Trial +(1+VScondition|ParticipantID), data = data_train, family = gaussian(link = log)),
          glmer(Duration ~ VScondition +(1+VScondition|ParticipantID), data = data_train, family = gaussian(link = log)))


#Create null list for results
results = data.frame()
m_results3 = data.frame()

#The loop
for (mdl in mlist){
  #Empty data frame
  m_results = data.frame()
  
  for (k in folds){
    #Change the name of the ith variable in f to "f", e.g. range --> f
    #setnames(data, f, "f")
    #------ Split into training and test data ------ 
    #Create training dataset, data not in fold k
    data_train=subset(d_fix2,!(ParticipantID %in% k))
    #Create test dataset, data in fold k
    data_test=subset(d_fix2,ParticipantID %in% k)
    
    
    
    #------ train model - apply model to data_train ------
    #Model uses the variable renamed to f above
    m = mdl
    
    #Get predicted value for training data, and round to 0 or 1
    predict_train = predict(m, data_train, allow.new.levels = TRUE)
    
    #Get RMSE
    RMSE_train=rmse(data_train$Duration, predict_train)
    
    #------ test the model - test model on data_test (last quarter) ------
    #Make predictions based on modeVIS
    predict_test=predict(m, data_test, allow.new.levels = TRUE)
    
    RMSE_test = rmse(data_test$Duration, predict_test)
    
    #------ save the performance ------ 
    #Save all the variables in one row
    one_row = data.frame(rmse_test = RMSE_test, rmse_train = RMSE_train)
    
    #Add the row to the data set
    m_results = rbind(m_results, one_row)
    
  }
  
  #Take mean of the test stats
  m_results2 = data.frame(rmse_train_m = mean(m_results$rmse_train), rmse_train_sd = sd(m_results$rmse_train),
                          rmse_test_m = mean(m_results$rmse_test), rmse_test_sd = sd(m_results$rmse_test),
                          dif_m = (mean(m_results$rmse_test) - mean(m_results$rmse_train)), dif_sd = (sd(m_results$rmse_test)-sd(m_results$rmse_train)))
  
  m_results3 = rbind(m_results3, m_results2)
}

predict_train


#------ Heat maps and scan paths ------
#Define a list of colors to use in the heat map
jet.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))

#Get the image
img <- readJPEG('eyetrackingscripts/foraging/ng090ws.jpg')
g <- rasterGrob(img, interpolate=TRUE) #

#Heat maps
ggplot(subset(d_fix2, ParticipantID==6 & Trial==6), aes(x = PositionX, y = 1081 - PositionY)) +
  xlim(0,1920) + #xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)
  ylim(0, 1080) +
  annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-0, ymax=1080) + #Add picture
  stat_density2d(geom="raster", aes(fill=..density.., alpha=sqrt(sqrt(..density..))), contour=FALSE, n=1000) + #Last two lines add heat map
  scale_alpha(range = c(0.1, 0.6)) + scale_fill_gradientn(colours = jet.colors(10), trans='sqrt')

#Scan paths
d_sp = subset(d_fix2, ParticipantID==6 & Trial==6) #Make subset of participant and trial
d_sp = d_sp[order(d_sp$Fixation),] #Order the data set by fixation, so the order of the scan path is ok

ggplot(d_sp, aes(x = PositionX, y = 1081 - PositionY, label = Fixation)) +
  annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-0, ymax=1080) + #add picture
  geom_point(size = (d_sp$Duration/50), color = 'yellow', alpha = 0.5) + #Add duration dots
  geom_path(size = 1, alpha = 0.3, color = "green") + #Add path between dots
  geom_text(color = 'red', aes(label = Fixation, size = 5)) #Add the fixation number

#------ Pupil plots ------

d_pupil = d_sample[d_sample$Task=='SocialEngagement',] %>% select(TrialTime, PupilSize, Gender, Direction, Ostensive)

ggplot(d_pupil, aes(TrialTime, PupilSize, color = Gender)) +
  geom_smooth() +
  facet_grid(Ostensive ~ Direction) #each side of the ~ is an axis in the grid


