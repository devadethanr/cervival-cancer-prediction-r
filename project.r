
##########DATA PREPARATION#################################
# Libraries 
library(ggplot2)     #Data Visualization 
library(dplyr)       #Data Manipulation
library(caTools)     #To split the data

#==============================================================================

# Load the dataset
cervical_cancer = read.csv("kag_risk_factors_cervical_cancer.csv")
dim(cervical_cancer)

#===============================================================================
#  Explore the data 
glimpse(cervical_cancer)
unique(cervical_cancer$STDs..Time.since.first.diagnosis)

#===============================================================================
#Verify the data set integrity- NAs 
prop_NA  <- function(x) {mean(is.na(x))}                   #creating  the functions for missing values                             
missdata <- sapply(cervical_cancer, prop_NA)               #Apply the prop_Na function to the data set
missdata <- data.frame(Variables = names(missdata), Proportion= missdata, missdatanegval =1- missdata)

#===============================================================================
#Transform this into a data frame
missdata <- missdata[order(desc(missdata$Proportion)),]    #change the data into desc order according to the Proportion 

#===============================================================================
# Data Visualization: missdatanegval vs NAs
ggplot(missdata, aes(x = Variables, y = missdatanegval))+
  geom_bar(stat = "identity", fill = "cyan")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(title = "Porportion of non NA Values")+
  theme(plot.title = element_text(hjust = 0.5))

#===============================================================================
#Verify Data set Integrity - Blanks and Zeroes (Same method as above)
prop_NullZero <- function(x) { mean(x == "" | x == 0)}
nullzerodata <- sapply(cervical_cancer, prop_NullZero)
nullzerodata <- data.frame(Variables = names(nullzerodata), Proportion = nullzerodata, missdatanegval = 1 - nullzerodata)
nullzerodata <- nullzerodata[order(desc(nullzerodata$missdatanegval)),]

#===============================================================================
#Data Visualization: missdatanegval vs blanks and zeroes (Same method as above)
ggplot(nullzerodata, aes(x = Variables, y = missdatanegval))+
  geom_bar(stat = "identity", fill = "deepskyblue2")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(title = "Proportion of non Zero or Blank Values")+
  theme(plot.title = element_text(hjust = 0.5))

#===============================================================================
#=====DATA CHANGING FOR VALUES THAT ARE WITH ? AND BLANKS=====

#===============================================================================
# FUNCTION TO IDENTIFY THE SUM VALUES IF ? IS THE RETURN

find_cols = function(x){
  cols = vector()
  for (i in 1:ncol(x)){
    if (sum(x[,i] == "?") > 0){
      cols = c(cols,i)
    }  
  }
  return(cols)
}

#===============================================================================
# Create function to fix missing values
fix_columns = function(x,cols) {
  for (j in 1:length(cols)) {
    x[,cols[j]] = as.character(x[,cols[j]])
    x[which(x[,cols[j]] == "?"),cols[j]] = "-1.0"
    x[,cols[j]] = as.numeric(x[,cols[j]])
  }
  return(x)
}
#Replaced all the ? values with the -1 value  for  creating uniform operations(as my csv file contain null values)

#===============================================================================
#Apply fns created earlier 
cols_to_fix = find_cols(cervical_cancer)
cervical_cancer = fix_columns(cervical_cancer, cols_to_fix)

#==================================q============================================
#===============================================================================

# correlation between variables to reject unimportant variables

  #Q1.printing and plotting the correlation matrix for the cervical cancer data
    set.seed(123)
    correlationMatrix <- cor(cervical_cancer[,12:26])
    print(correlationMatrix)
    plot(correlationMatrix)

#===============================================================================
    
#variables to represent the cervical cancer 

cervical_cancer$CervicalCancer = cervical_cancer$Hinselmann + cervical_cancer$Schiller + cervical_cancer$Citology + cervical_cancer$Biopsy
print(cervical_cancer$CervicalCancer)
plot(cervical_cancer$CervicalCancer)

#===============================================================================

#adding all the columns to represent the cervical cancer exam result
#positive exams results show the cancer probability if multiple results become positive at same time 
cervical_cancer$CervicalCancer = factor(cervical_cancer$CervicalCancer
                                        , levels=c("0","1","2","3","4"))
print(cervical_cancer$CervicalCancer)




#=========EXPLORATORY DATA ANALYSIS of CERVICAL CANCER(EDA)=====================
#===============================================================================

  #Q2. to find  distribution of the cervical cancer in csv datasheet through grading
  
    round(prop.table(table(cervical_cancer$CervicalCancer)),2)     #percentage of data as the new grade assigned 
  
    # Plot target variable distribution
      ggplot(cervical_cancer,(aes(x = CervicalCancer, y = sum(as.integer(as.character(CervicalCancer))),fill = CervicalCancer)))+
        geom_bar(stat="identity")+
        scale_fill_manual(values=c("limegreen","gold","orangered","red2","purple"))+
        labs(title = "Quantity of CervicalCancer Classes")+
        theme(plot.title = element_text(hjust = 0.5))

#===============================================================================

    #Q3. occurrence of cervical cancer across age group
    
      #Density: Cervical Cancer across Age (to see how age affect the cervical cancer)
      ggplot(cervical_cancer, aes(x = Age, fill=CervicalCancer))+
        geom_density(alpha = 0.40, color=NA)+
        scale_fill_manual(values=c("limegreen","gold","orangered","red2","purple"))+
        labs(title = "Density of CervicalCancer across Age")+
        theme(plot.title = element_text(hjust = 0.5))+
        facet_grid(as.factor(CervicalCancer) ~ .)

#===============================================================================
      
    #Q4 calculating the probability of affecting cervical cancer based on age (regression analysis)
          
          #age to be calculated cerage variable
          cerage=25
          x=cervical_cancer$Age
          y=cervical_cancer$CervicalCancer
          
          print(y)
          #finding relation
          relation <- lm(y~x)
          
          #prediction.
          a <- data.frame(x = cerage)
          result <-  predict(relation,a)
          print(result)
          plot(result)
#===============================================================================
    
    #Q5 years of hormonal contraceptives of affects over the decrease of cervical cancer
            
        #Density: CervicalCancer across Hormornal Conctraceptive years (How can can years of hormonal contraceptives actually help)
        ggplot(cervical_cancer, aes(x = Hormonal.Contraceptives..years., fill=CervicalCancer))+
          geom_density(alpha = 0.40, color=NA)+
          scale_fill_manual(values=c("limegreen","gold","orangered","red2","purple"))+
          labs(title = "Density of CervicalCancer across Years of Hormonal Contraceptives")+
          theme(plot.title = element_text(hjust = 0.5))+
          facet_grid(as.factor(CervicalCancer) ~ .)
#===============================================================================
  
    #Q6 predicting the grade for cervical cancer occurrence by predictor variable smoke(packets/year)
            
            #no of smoke packets for finding the grade 
            cersmpkt=1200
            xpkt=cervical_cancer$Smokes..packs.year.
            ygrd=cervical_cancer$CervicalCancer
            
            #finding relation
            relation <- lm(ygrd~xpkt)
            
            #prediction.
            a <- data.frame(xpkt=cersmpkt)
            result <-  predict(relation,a)
            print(result)
            plot(result)
#===============================================================================
        
        
    
        
