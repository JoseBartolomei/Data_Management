
# Note ------------------------------------------------------------------------------
# This is the same 02_Data_Management_10.R
# I only find/replace from BRFSS.10 to BRFSS.13
###############################################################################

rm(list = ls())
gc()

# If in .csv to R
read.csv(file = "/.csv")

# In in .RData
  load (file = "/.RData")
sort(names(BRFSS.13))

### Script for data management and cleaning ###

## Recode BRFSS varibles ##


## 1. Month of interview (imonth) ##

table(BRFSS.13$imonth)
BRFSS.13$imonth <- as.numeric(as.character(BRFSS.13$imonth))
#Create new variable to keep the original. When imonth is labeled, during analysis
#the categories order is lost and could further affect analysis 
BRFSS.13$imonth2 <- BRFSS.13$imonth

table(BRFSS.13$imonth2)

## Factorize and label the month variable
 
BRFSS.13$imonth<-factor(BRFSS.13$imonth, labels=c("January","February","March","April",
                                                  "May","June",  "July", "August", 
                                                  "September","October","November",
                                                  "December"))
table(BRFSS.13$imonth)


## 2.Primary Sampling Unit, Equal to Annual Sequence Number (psu) ##


## 3. General health perception (genhlth)
table(BRFSS.13$genhlth)

BRFSS.13$genhlth<-factor(BRFSS.13$genhlth)
# Replace values with NA 
BRFSS.13$genhlth<-replace(BRFSS.13$genhlth, BRFSS.13$genhlth==7, NA)
BRFSS.13$genhlth<-replace(BRFSS.13$genhlth, BRFSS.13$genhlth==9, NA)

BRFSS.13$genhlth<-factor(BRFSS.13$genhlth)

#Create new variable to keep the original. When imonth is labeled, during analysis
#the categories order is lost.
BRFSS.13$genhlth2<-as.character(BRFSS.13$genhlth)

#For recoding into BRFSS.13 measure Fairpoor#
BRFSS.13$fairpoor<-BRFSS.13$genhlth #Is necesarry to create this variable before recoding genhlth 

label.gen<-list(BRFSS.13$genhlth, Excellent="1", Verygood="2", Good="3", Fair="4", 
		Poor="5")
levels(BRFSS.13$genhlth)<- label.gen

BRFSS.13$genhlth<-factor(BRFSS.13$genhlth)
                
summary(BRFSS.13$genhlth)


## 4. Physical health not good (physhlth) ##
table(BRFSS.13$physhlth)

#Convert 88 values to 0#
BRFSS.13$physhlth[BRFSS.13$physhlth==88]<-0

#Convert 77 values to blank#
BRFSS.13$physhlth[BRFSS.13$physhlth==77]<-" "

BRFSS.13$physhlth<-as.numeric(BRFSS.13$physhlth)
class(BRFSS.13$physhlth)
table(BRFSS.13$physhlth)
summary(BRFSS.13$physhlth)


## 5. Mental health not good (menthlth) ##
table(BRFSS.13$menthlth)
summary(BRFSS.13$menthlth)

#Convert 88 values to 0#
BRFSS.13$menthlth[BRFSS.13$menthlth==88]<-0

#Convert 77 values to blank#
BRFSS.13$menthlth[BRFSS.13$menthlth==77]<-" "

#Convert 99 values to blank#
BRFSS.13$menthlth[BRFSS.13$menthlth==99]<-" "

BRFSS.13$menthlth<-as.numeric(BRFSS.13$menthlth)
class(BRFSS.13$menthlth)
table(BRFSS.13$menthlth)
summary(BRFSS.13$menthlth)


## 6. Recent activity limitation days (poorhlth) ##

table(BRFSS.13$poorhlth)
summary(BRFSS.13$poorhlth)
class(BRFSS.13$poorhlth)

BRFSS.13$poorhlth<-as.character(BRFSS.13$poorhlth)

BRFSS.13$poorhlth<-replace(BRFSS.13$poorhlth, BRFSS.13$poorhlth==".", NA)

#Convert 88 values to 0#
BRFSS.13$poorhlth[BRFSS.13$poorhlth==88]<-0

#Convert 77 values to blank#
BRFSS.13$poorhlth[BRFSS.13$poorhlth==77]<-" "

# Convert 99 values to blank #
BRFSS.13$poorhlth[BRFSS.13$poorhlth==99]<-" "

BRFSS.13$poorhlth<-as.numeric(BRFSS.13$poorhlth)
class(BRFSS.13$poorhlth)
table(BRFSS.13$poorhlth)
summary(BRFSS.13$poorhlth)


## 7. Any kind of health care coverage (hlthplan) ##
table(BRFSS.13$hlthplan)
BRFSS.13$hlthplan<-factor(BRFSS.13$hlthplan)
BRFSS.13$hlthplan<-replace(BRFSS.13$hlthplan, BRFSS.13$hlthplan==7, NA)
BRFSS.13$hlthplan<-replace(BRFSS.13$hlthplan, BRFSS.13$hlthplan==9, NA)

label.hpl<-list(BRFSS.13$hlthplan, Yes="1", No="2")
levels(BRFSS.13$hlthplan)<- label.hpl
BRFSS.13$hlthplan<-factor(BRFSS.13$hlthplan)

class(BRFSS.13$hlthplan)
table(BRFSS.13$hlthplan)
summary(BRFSS.13$hlthplan)


## 8. Person you think of as your personal doctor or health care provider (persdoc2) ##
table(BRFSS.13$persdoc2)

#Change class#
BRFSS.13$persdoc2<-factor(BRFSS.13$persdoc2)

BRFSS.13$persdoc2<-replace(BRFSS.13$persdoc2, BRFSS.13$persdoc2==7, NA)
BRFSS.13$persdoc2<-replace(BRFSS.13$persdoc2, BRFSS.13$persdoc2==9, NA)

#For variable with values yes or no (perdocod)#
BRFSS.13$perdocod<-factor(BRFSS.13$persdoc2)

label.pdc<-list(BRFSS.13$persdoc2, Yes="1", Moreone="2", No="3")

levels(BRFSS.13$persdoc2)<- label.pdc

BRFSS.13$persdoc2<-factor(BRFSS.13$persdoc2)

table(BRFSS.13$persdoc2)
summary(BRFSS.13$persdoc2)

	#Categorized variables to obtain values yes on no(perdocod)#

	label.pc<-list(BRFSS.13$perdocod, Yes="1", Yes="2", No="3")

	levels(BRFSS.13$perdocod)<- label.pc

	BRFSS.13$perdocod<-factor(BRFSS.13$perdocod)

	table(BRFSS.13$perdocod)
	summary(BRFSS.13$perdocod)


## 9. Could not see a doctor because of cost (medcost) ##
table(BRFSS.13$medcost)
BRFSS.13$medcost<-replace(BRFSS.13$medcost, BRFSS.13$medcost==7, NA)
BRFSS.13$medcost<-factor(BRFSS.13$medcost)

levels(BRFSS.13$medcost)<- label.hpl
                BRFSS.13$medcost<-factor(BRFSS.13$medcost)

table(BRFSS.13$medcost)
summary(BRFSS.13$medcost)


## 10. Last visited a doctor for a routine checkup (checkup2) ##
table(BRFSS.13$checkup1)
summary(BRFSS.13$checkup1)

BRFSS.13$checkup1<-replace(BRFSS.13$checkup1, BRFSS.13$checkup1==7, NA)
BRFSS.13$checkup1<-replace(BRFSS.13$checkup1, BRFSS.13$checkup1==9, NA)

BRFSS.13$checkup1<-factor(BRFSS.13$checkup1)

#Line for maintaining the original values for further use#
BRFSS.13$checkup2<-as.character(BRFSS.13$checkup1)
BRFSS.13$checkup2<-replace(BRFSS.13$checkup2, BRFSS.13$checkup2==8, 0)

# Label checkup1

label.cup<-list(BRFSS.13$checkup1, Withinyear="1", Withintwoyrs="2", 
		Withinfiveyrs="3", Fivemoreyrs="4", Never="8")
levels(BRFSS.13$checkup1)<- label.cup

# refactor to eliminate deleted group remanet
BRFSS.13$checkup1<-factor(BRFSS.13$checkup1)
table(BRFSS.13$checkup1)
summary(BRFSS.13$checkup1)


## 11. Any physical activities or exercises during past month (exeranay2) ## 
table(BRFSS.13$exerany2)
BRFSS.13$exerany2<-replace(BRFSS.13$exerany2, BRFSS.13$exerany2==7, NA)
BRFSS.13$exerany2<-factor(BRFSS.13$exerany2)
levels(BRFSS.13$exerany2)<- label.hpl
BRFSS.13$exerany2<-factor(BRFSS.13$exerany2)

table(BRFSS.13$exerany2)
summary(BRFSS.13$exerany2)


## 13. Ever been told by a doctor that you have diabetes (diabetes2) ##

table (BRFSS.13$diabete2)
summary(BRFSS.13$diabete2)

#Line for maintaining the original values for further use#
BRFSS.13$diabetes<-as.character(BRFSS.13$diabete2)

# Replace values in diabetes
BRFSS.13$diabetes<-replace(BRFSS.13$diabetes, BRFSS.13$diabetes==7, NA)
BRFSS.13$diabetes<-replace(BRFSS.13$diabetes, BRFSS.13$diabetes==9, NA)

# Refactor to eliminate deleted group remanet
BRFSS.13$diabetes<-as.factor(BRFSS.13$diabetes)
# Label diabetes

label.dia<-list(Yes="1", No="2", No="3", No="4")
levels(BRFSS.13$diabetes)<- label.dia

BRFSS.13$diabetes <- relevel(BRFSS.13$diabetes, ref = "No")

table (BRFSS.13$diabetes)
summary(BRFSS.13$diabetes)


## 13. Ever told you had a heart attack (cvdinfr4) ##
table(BRFSS.13$cvdinfr4)
#To maintain the original values for further use#
BRFSS.13$cvdinfr<-as.character(BRFSS.13$cvdinfr4)

BRFSS.13$cvdinfr<-replace(BRFSS.13$cvdinfr, BRFSS.13$cvdinfr==7, NA)

# Factor
BRFSS.13$cvdinfr<-factor(BRFSS.13$cvdinfr)
#Recode#
levels(BRFSS.13$cvdinfr)<- label.hpl
# Re-factorize
BRFSS.13$cvdinfr<-factor(BRFSS.13$cvdinfr)

table(BRFSS.13$cvdinfr)
summary(BRFSS.13$cvdinfr)


## 14. Ever told you had angina or coronary heart disease (cvdcrhd4) ##
table(BRFSS.13$cvdcrhd4)

#Line for maintaining the original values for further use#
BRFSS.13$cvdcrhd<-as.factor(BRFSS.13$cvdcrhd4)

BRFSS.13$cvdcrhd<-replace(BRFSS.13$cvdcrhd, BRFSS.13$cvdcrhd==7, NA)

#Factorize
BRFSS.13$cvdcrhd<-factor(BRFSS.13$cvdcrhd)
levels(BRFSS.13$cvdcrhd)<- label.hpl

#Re-facotrize
BRFSS.13$cvdcrhd<-factor(BRFSS.13$cvdcrhd)

table(BRFSS.13$cvdcrhd)
summary(BRFSS.13$cvdcrhd)


## 15. Ever told you had a stroke (cvdstrk3) ##
table(BRFSS.13$cvdstrk3)

#To maintain the original values for further use#
BRFSS.13$cvdstrk<-as.factor(BRFSS.13$cvdstrk3)

# Put values to NA
BRFSS.13$cvdstrk<-replace(BRFSS.13$cvdstrk, BRFSS.13$cvdstrk==7, NA)

#lavels
levels(BRFSS.13$cvdstrk)<- label.hpl
#Re-factorieze
BRFSS.13$cvdstrk<-factor(BRFSS.13$cvdstrk)

table(BRFSS.13$cvdstrk)


## 16. Have you ever been told that you had asthma (asthma2) ##

table(BRFSS.13$asthma2)

BRFSS.13$asthma2<-factor(BRFSS.13$asthma2)

BRFSS.13$asthma2<-replace(BRFSS.13$asthma2, BRFSS.13$asthma2==7, NA)

levels(BRFSS.13$asthma2)<- label.hpl

BRFSS.13$asthma2<-factor(BRFSS.13$asthma2)
table(BRFSS.13$asthma2)


## 17. Still have asthma (asthnow) ##
## 17. Still have asthma (asthnow) ##

table(BRFSS.13$asthnow)
BRFSS.13$asthnow<-factor(BRFSS.13$asthnow)
BRFSS.13$asthnow<-replace(BRFSS.13$asthnow, BRFSS.13$asthnow==7, NA)
levels(BRFSS.13$asthnow)<- label.hpl
BRFSS.13$asthnow<-factor(BRFSS.13$asthnow)
table(BRFSS.13$asthnow)

## Former asthma patient (former) ## 
BRFSS.13$former <- BRFSS.13$asthnow
BRFSS.13$former <- ifelse(BRFSS.13$former == 1, "No", "Yes")
BRFSS.13$former <- factor(BRFSS.13$former)

table(BRFSS.13$former)

##18. Limited because of physical, mental, or emotional problems (qlactlm2) ##

table(BRFSS.13$qlactlm2)
BRFSS.13$qlactlm2<-factor(BRFSS.13$qlactlm2)
BRFSS.13$qlactlm2<-replace(BRFSS.13$qlactlm2, BRFSS.13$qlactlm2==7, NA)
BRFSS.13$qlactlm2<-replace(BRFSS.13$qlactlm2, BRFSS.13$qlactlm2==9, NA)

levels(BRFSS.13$qlactlm2)<- label.hpl
BRFSS.13$qlactlm2<-factor(BRFSS.13$qlactlm2)
summary(BRFSS.13$qlactlm2)


## 19. Age (age) ##

table(BRFSS.13$age)
BRFSS.13$age <- replace(BRFSS.13$age, BRFSS.13$age==7, NA)
BRFSS.13$age <- replace(BRFSS.13$age, BRFSS.13$age==9, NA)
table(BRFSS.13$age)
summary(BRFSS.13$age)


## 20. Marital status (marital) ##

table(BRFSS.13$marital)
BRFSS.13$marital<-as.character(BRFSS.13$marital)
BRFSS.13$marital<-replace(BRFSS.13$marital, BRFSS.13$marital==9, NA)

BRFSS.13$marital2<-factor(BRFSS.13$marital)
label.mt<-list(BRFSS.13$marital2, Married="1", Divorced="2", Widowed="3", Separate="4",
  		Nevermarried="5", Unmarriedcople="6")
levels(BRFSS.13$marital2)<- label.mt
table(BRFSS.13$marital2)          
BRFSS.13$marital2<-factor(BRFSS.13$marital2)

summary(BRFSS.13$marital2)

	#Living with a significant other yes or no(maritrec)##

	BRFSS.13$maritrec<-as.factor(BRFSS.13$marital)

	BRFSS.13$maritrec<-replace(BRFSS.13$maritrec, BRFSS.13$maritrec==9, NA)

	label.mtr<-list(Yes="1", No="2", No="3", No="4",
		No="5", Yes="6")
	levels(BRFSS.13$maritrec)<- label.mtr
                 
	BRFSS.13$maritrec<-factor(BRFSS.13$maritrec)

	summary(BRFSS.13$maritrec)


## 21. Educational level (educa) ##

table(BRFSS.13$educa)

BRFSS.13$educa<-as.factor(BRFSS.13$educa)

BRFSS.13$educa<-replace(BRFSS.13$educa, BRFSS.13$educa==9, NA)

label.ed<-list(BRFSS.13$educa, Never="1", Elementary="2", SomeHigh="3", HSGrad="4",
		SomeCollege="5", CollegeGrad="6")
levels(BRFSS.13$educa)<- label.ed

BRFSS.13$educa<-factor(BRFSS.13$educa)
summary(BRFSS.13$educa)


## 22. Employment status (employ2) ##

table(BRFSS.13$employ)
BRFSS.13$employ<-replace(BRFSS.13$employ, BRFSS.13$employ==9, NA)

BRFSS.13$employ2<-as.factor(BRFSS.13$employ)
               
label.em<-list(BRFSS.13$employ2, Forwage="1", Selfemploy="2", Outworkmoreyear="3",
 	Outworklessyear="4", Homemaker="5", Student="6", Retired="7", Unablework="8")
levels(BRFSS.13$employ2)<- label.em
               
BRFSS.13$employ2<-factor(BRFSS.13$employ2)

               summary(BRFSS.13$employ2)

	# Employ yes or no (emplrec)#

	BRFSS.13$emplrec<-as.factor(BRFSS.13$employ)
               
      BRFSS.13$emplrec<-replace(BRFSS.13$emplrec, BRFSS.13$emplrec==9, NA)

	label.emr<-list(BRFSS.13$emplrec, Yes="1", Yes="2", No="3", No="4", No="5", 
		No="6", No="7", No="8")
	levels(BRFSS.13$emplrec)<- label.emr
	
 	BRFSS.13$emplrec<-factor(BRFSS.13$emplrec)
               
 	summary(BRFSS.13$emplrec)

	# Employment combinig some cateqories forwage and selfemploy , out of work (emplrec2)#

	BRFSS.13$emplrec2<-as.factor(BRFSS.13$employ)
               
      BRFSS.13$emplrec2<-replace(BRFSS.13$emplrec2, BRFSS.13$emplrec2==9, NA)

	label.emr2<-list(BRFSS.13$emplrec2,Employ="1", Employ="2", Outwork="3",
 	Outwork="4", Homemaker="5", Student="6", Retired="7", Unablework="8")

	levels(BRFSS.13$emplrec2)<- label.emr2
	
 	BRFSS.13$emplrec2<-factor(BRFSS.13$emplrec2)
      table(BRFSS.13$emplrec2)         
 	summary(BRFSS.13$emplrec2)


## 23. Annual household income from all sources (income2) ##

table(BRFSS.13$income2)
# Refer to variable 37 incomg #


## 24. Weigh without shoes (weight2) ##


## 25. Tall without shoes (heigh3) ##


## 26. Gender (sex) ##

table(BRFSS.13$sex)
BRFSS.13$sex<-factor(BRFSS.13$sex, labels=c("Male", "Female")) 
table(BRFSS.13$sex)

## 28. Final weight assigned to each respondent (finalwt) ##

#BRFSS.13$finalwt <- BRFSS.13$llcpwt
nN_13 <- sum(BRFSS.13$finalwt)


## 29. Geographic region, Health region (geostr) ##

table(BRFSS.13$geostr)

BRFSS.13$geostr <- replace(BRFSS.13$geostr, BRFSS.13$geostr == 99, NA)

# BRFSS.13$geostr<- factor(BRFSS.13$geostr, labels=c("Aguadilla", "Arecibo", "Bayamon", "Metro",
# 			 "Fajardo", "Caguas", "Ponce", "Mayaguez"))
table(BRFSS.13$geostr)
 
## 30. Seven-level imputed age category(ageg) ##

table(BRFSS.13$ageg)
BRFSS.13$ageg <- factor(BRFSS.13$ageg,
                        labels=c("18-24", "25-34", "35-44", "45-54", "55-64", "65+"))
table(BRFSS.13$ageg)


## 31. Physical activity or exercise during the past 30 days other than their regular job (totinda) ##

table(BRFSS.13$totinda)
BRFSS.13$totinda<-replace(BRFSS.13$totinda, BRFSS.13$totinda==9, NA)

#Other form of recoding#
BRFSS.13$totinda<-factor(BRFSS.13$totinda, labels=c("Yes", "No")) 
  table(BRFSS.13$totinda)
  summary(BRFSS.13$totinda)


## 32. Asthma status (asthmst) ##

table(BRFSS.13$asthmst)

BRFSS.13$asthmst<-replace(BRFSS.13$asthmst, BRFSS.13$asthmst==9, NA)

  # variable for estimating current asthma (Yes or no in all ppopulation
    BRFSS.13$currasth<-BRFSS.13$asthmst
    BRFSS.13$currasth<-factor(BRFSS.13$currasth)
    table(BRFSS.13$currasth)
BRFSS.13$currasth <- ifelse(BRFSS.13$currasth == 1, "Yes", "No")    
BRFSS.13$currasth <- factor(BRFSS.13$currasth)
BRFSS.13$currasth <- relevel(BRFSS.13$currasth, ref = "No")
    
table(BRFSS.13$currasth)


BRFSS.13$asthmst<-factor(BRFSS.13$asthmst, labels=c("Current", "Former", "Never")) 
table(BRFSS.13$asthmst)
summary(BRFSS.13$asthmst)


## 33. Adults who are current smokers(rfsmok3) ##

table(BRFSS.13$rfsmok3)
BRFSS.13$rfsmok3<-replace(BRFSS.13$rfsmok3, BRFSS.13$rfsmok3==9, NA)
BRFSS.13$rfsmok3<-factor(BRFSS.13$rfsmok3, labels=c("No", "Yes")) 
table(BRFSS.13$rfsmok3)
summary(BRFSS.13$rfsmok3)

## 34. Body Mass Index (bmi4)
class(BRFSS.13$bmi)

BRFSS.13$bmi<-replace(BRFSS.13$bmi, BRFSS.13$bmi==9999, NA)

summary(BRFSS.13$bmi)

## 35. Three-categories of Body Mass Index (bmi4cat) ##

table(BRFSS.13$bmi4cat)

BRFSS.13$bmi4cat<-replace(BRFSS.13$bmi4cat, BRFSS.13$bmi4cat==4, NA)

#Create new variable to keep the original. When imonth is labeled, during analysis
#the categories order is lost and could further affect analysis 
BRFSS.13$bmi4cat2<-as.character(BRFSS.13$bmi4cat)

BRFSS.13$bmi4cat<-factor(BRFSS.13$bmi4cat, labels=c("Neither overweight nor obese",
 "Overweight","Obese")) 
table(BRFSS.13$bmi4cat)
summary(BRFSS.13$bmi4cat)


## 36. Education recoded (educag) ##

table(BRFSS.13$educag)

BRFSS.13$educag<-replace(BRFSS.13$educag, BRFSS.13$educag==9, NA)

#Create new variable to keep the original. When imonth is labeled, during analysis
#the categories order is lost and could afect analysis.
BRFSS.13$educag2<-as.character(BRFSS.13$educag)

BRFSS.13$educag<-factor(BRFSS.13$educag)
                 
label.edc<-list(BRFSS.13$educa, LessHS="1", HSGraduate="2", SomeCollege="3", CollegeGrad="4")
levels(BRFSS.13$educag)<- label.edc
                 
BRFSS.13$educag<-factor(BRFSS.13$educag)
table(BRFSS.13$educag)
summary(BRFSS.13$educag)

#lable to maintain the order of the educational levels#
BRFSS.13$educag3 <- factor(BRFSS.13$educag2)
label.edc2<-list(BRFSS.13$educag3, HS="1", HSGraduate="2", Univer="3", UniverGrad="4")
levels(BRFSS.13$educag3)<- label.edc2
BRFSS.13$educag3<-factor(BRFSS.13$educag3)              
table(BRFSS.13$educag3)
summary(BRFSS.13$educag3)

## 37. Annual household income from all sources (incomg)##

table(BRFSS.13$incomg)

BRFSS.13$incomg<-replace(BRFSS.13$incomg, BRFSS.13$incomg==9, NA)
BRFSS.13$incomg<-factor(BRFSS.13$incomg, labels=c("<15k", "15k-<25k", "25k-<35k", "35k-<50k",
  		 "50+k"))
table(BRFSS.13$incomg)
summary(BRFSS.13$incomg)


## 38. Six-level imputed age category (ageg2)##

table(BRFSS.13$ageg2)

BRFSS.13$ageg2<-factor(BRFSS.13$ageg2, labels=c("18-24", "25-34", "35-44", "45-54",
                                                "55-64", "65+"))
table(BRFSS.13$ageg2)
summary(BRFSS.13$ageg2)


## 39. How many children less than 18 years of age live in your household?
table(BRFSS.13$children)

# Convert 88 values to 0#
BRFSS.13$children[BRFSS.13$children==88]<-0
BRFSS.13$children<-replace(BRFSS.13$children, BRFSS.13$children==99, NA)

# Children yes or no #
BRFSS.13$child<-factor(BRFSS.13$children)
table(BRFSS.13$child)

label.child<-list(BRFSS.13$children, 
                  No="0", Yes="1", Yes="2", Yes="3", Yes="4", Yes="5", Yes="6",
                  Yes="7", Yes="8")

levels(BRFSS.13$child) <- label.child
table(BRFSS.13$child)
BRFSS.13$child<-factor(BRFSS.13$child)

# Children no, 1, 2, 2< #
BRFSS.13$child2 <- factor(BRFSS.13$children)
table(BRFSS.13$child2)

label.child2 <- list(No = "0", One = "1", Two = "2", TwoMore = "3", TwoMore = "4",
                     TwoMore = "5", TwoMore = "6", TwoMore = "7", TwoMore = "8")

levels(BRFSS.13$child2) <- label.child2
table(BRFSS.13$child2)
BRFSS.13$child2<-factor(BRFSS.13$child2)




## 41. To your knowledge, are you now pregnant?
table(BRFSS.13$pregnant)
BRFSS.13$pregnant<-replace(BRFSS.13$pregnant, BRFSS.13$pregnant==7, NA)

BRFSS.13$pregnant<-factor(BRFSS.13$pregnant)
levels(BRFSS.13$pregnant)<-label.hpl
BRFSS.13$pregnant<-factor(BRFSS.13$pregnant)
table(BRFSS.13$pregnant)


## 42. Year of interview
BRFSS.13$iyear <- 2013
table(BRFSS.13$iyear)

## 43. Health Region 

BRFSS.13$hregion <- factor(BRFSS.13$ststr)
table(BRFSS.13$hregion)

# This procedure generates a variable with the health region data using the variable citycode. The procedure use is as follows:

BRFSS.13$muni <- factor(BRFSS.13$citycode)
table(BRFSS.13$muni)
# This sections needs to be verified with
BRFSS.13$muni <- replace(BRFSS.13$muni, BRFSS.13$muni == 777, NA)
BRFSS.13$muni <- replace(BRFSS.13$muni, BRFSS.13$muni == 888, NA)
BRFSS.13$muni <- replace(BRFSS.13$muni, BRFSS.13$muni == 999, NA)
BRFSS.13$muni <- factor(BRFSS.13$muni)
table(BRFSS.13$muni)

#Conduct all the variable management procedures
# Land line management and changing number values to text values
levels(BRFSS.13$hregion)[levels(BRFSS.13$hregion) == 72011] <- "Aguadilla"
levels(BRFSS.13$hregion)[levels(BRFSS.13$hregion) == 72021] <- "Arecibo"
levels(BRFSS.13$hregion)[levels(BRFSS.13$hregion) == 72031] <- "Bayamon"
levels(BRFSS.13$hregion)[levels(BRFSS.13$hregion) == 72041] <- "Metro"
levels(BRFSS.13$hregion)[levels(BRFSS.13$hregion) == 72051] <- "Fajardo"
levels(BRFSS.13$hregion)[levels(BRFSS.13$hregion) == 72061] <- "Caguas"
levels(BRFSS.13$hregion)[levels(BRFSS.13$hregion) == 72071] <- "Ponce"
levels(BRFSS.13$hregion)[levels(BRFSS.13$hregion) == 72081] <- "Mayaguez"

# Cell phone management and changing number values to text values
BRFSS.13$hregion[which(BRFSS.13$state == 72 & BRFSS.13$muni %in%
  c("1","15","43","55","57","59","73","75","109","111","113","133","133","149",
   "153"))] <- "Ponce"

BRFSS.13$hregion[which(BRFSS.13$state == 72 & BRFSS.13$muni %in%
  c("3","5","71","99","131"))] <- "Aguadilla"

BRFSS.13$hregion[which(BRFSS.13$state == 72 & BRFSS.13$muni %in%
  c("7","9","25","35","41","63","69","77","85","95","103","139",
   "151"))] <- "Caguas"

BRFSS.13$hregion[which(BRFSS.13$state == 72 & BRFSS.13$muni %in%
  c("11","23","67","79","83","93","97","117","131","135"))] <- "Mayaguez"

BRFSS.13$hregion[which(BRFSS.13$state == 72 & BRFSS.13$muni %in%
  c("13","17","27","39","54","65","81","91","101","115","141",
   "145"))] <- "Arecibo"

BRFSS.13$hregion[which(BRFSS.13$state == 72 & BRFSS.13$muni %in%
  c("19","21","33","45","47","51","105","107","135","137",
   "143"))] <- "Bayamon"

BRFSS.13$hregion[which(BRFSS.13$state == 72 & BRFSS.13$muni %in%
  c("29","31","61","87","137","139"))] <- "Metro"

BRFSS.13$hregion[which(BRFSS.13$state == 72 & BRFSS.13$muni %in%
  c("37","49","53","89","119","147"))] <- "Fajardo"

BRFSS.13$hregion <- replace(BRFSS.13$hregion, BRFSS.13$hregion == 72999, NA)
#Check variable values after management.

BRFSS.13$hregion <- factor(BRFSS.13$hregion)
table(BRFSS.13$hregion)



# labels that idendify the municipalities and their BRFSS codes ---------------------

# this is usefull to know the respective municipality BRFSS code
# Do not needed for this script just to have it as knowldege
#label.MUNI <- list( Adjuntas="1", Aguada="3", Aguadilla="5", AguasBuenas="7", Aibonito="9",                
#  Anasco="11", Arecibo="13", Arroyo="15", Barceloneta="17", Barranquitas="19", Bayamon="21",
#  CaboRojo="23", Caguas="25", Camuy="27", Canovanas="29", Carolina="31", Catano="33", 
#  Cayey="35", Ceiba="37", Ciales="39",  Cidra="41", Coamo="43",  Comerio="45",Corozal="47",
#  Culebra="49", Dorado="51", Fajardo="53", Florida="54", Guanica="55" , Guayama="57",
#  Guayanilla="59", Guaynabo="61", Gurabo="63", Hatillo="65", Hormigueros="67", Humacao="69",    
#  Isabela="71", Jayuya="73", JuanaDiaz="75", Juncos="77", Lajas="79", Lares="81", LasMarias="83",
#  LasPiedras="85", Loiza="87", Luquillo="89", Manati="91", Maricao="93", Maunabo="95",               
#  Mayaguez="97", Moca="99", Morovis="101", Naguabo="103",    
#  Naranjito="105", Orocovis="107", Patillas="109" , Penuelas="111",  Ponce="113",  Quebradillas="115",       
#  Rincon="117", RioGrande="119", SabanaGrande="131" , Salinas="133", SanGerman="135", 
#  SanJuan="137",	SanLorenzo="139", SanSebastian="131",
#  SantaIsabel="133", ToaAlta="135", ToaBaja="137", TrujilloAlto="139", Utuado="141", VegaAlta="143",
#  VegaBaja="145", Vieques="147", Villalba="149", Yabucoa="151", Yauco="153")

#### BRFSS.13 variables based on the CDC measurements ###


## 44. Fairpoor from genhlth (fairpoor) ##

table(BRFSS.13$fairpoor)

label.fp<-list(BRFSS.13$fairpoor, EVGood="1",  EVGood="2",  EVGood="3",
 		Fairpoor="4", Fairpoor="5")
levels(BRFSS.13$fairpoor)<- label.fp

BRFSS.13$fairpoor<-factor(BRFSS.13$fairpoor)

table(BRFSS.13$fairpoor)
summary(BRFSS.13$fairpoor)


## 45. Computing unhealthy days by adding (unhlthy) ## 

#Create data frame with BRFSS.13 variables#
df<-data.frame(BRFSS.13$physhlth,BRFSS.13$menthlth)

#Sum variable#
sumdays<-rowSums(df, na.rm=T)

#Create variable total days#
BRFSS.13$sumdays<-sumdays
table(BRFSS.13$sumdays)

#Define unhealthy days groups here#
unhg<-c(0, 13,  Inf)

BRFSS.13$unhlthy<-cut(BRFSS.13$sumdays, unhg, include.lowest=TRUE,
		labels=c("0-13", "14+"),
		levels=c("0-13", "14+"))

table(BRFSS.13$unhlthy)

rm(df)

#Variable with the values >30 of sumdays transformed to 30#  

BRFSS.13$unhlthdays<-BRFSS.13$sumdays
table(BRFSS.13$unhlthdays)
BRFSS.13$unhlthdays[BRFSS.13$unhlthdays>30]=30
table(BRFSS.13$unhlthdays)
summary(BRFSS.13$unhlthdays)


## 46. Frequent physical distress (physhlcat) ##

BRFSS.13$physhlcat<-cut(BRFSS.13$physhlth, unhg, include.lowest=TRUE,
		labels=c("0-13", "14+"),
		levels=c("0-13", "14+"))

table(BRFSS.13$physhlcat)


## 47. Frequent mental distress (mentdist) ##

BRFSS.13$mentdist<-cut(BRFSS.13$menthlth, unhg, include.lowest=TRUE,
		labels=c("0-13", "14+"),
		levels=c("0-13", "14+"))

table(BRFSS.13$mentdist)


## 48. poor physical or mental health keep you from doing your usual activities, 
##     such as self-care, work, or recreation (poorhlcat)

BRFSS.13$poorhlcat<-cut(BRFSS.13$poorhlth, unhg, include.lowest=TRUE,
		labels=c("0-13", "14+"),
		levels=c("0-13", "14+"))

table(BRFSS.13$poorhlcat)

### Other covariates ###

## Any chronic disease such as diabetes, cadiovascular ##
## Constructing variables related to chronic disease presence ##


#Recoding variable diabete2#

table(BRFSS.13$diabete2)
BRFSS.13$diabe<-as.numeric(BRFSS.13$diabete2)

#Convert 2 values to 0#
BRFSS.13$diabe[BRFSS.13$diabe==2] <- 0

#Convert 3 values to 0#
BRFSS.13$diabe[BRFSS.13$diabe==3] <- 0

#Convert 4 values to 0#
BRFSS.13$diabe[BRFSS.13$diabe==4] <- 0

#Convert 7 values to blank#
BRFSS.13$diabe[BRFSS.13$diabe==7] <- " "

# Convert 9 values to blank #
BRFSS.13$diabe[BRFSS.13$diabe==9] <- " "

BRFSS.13$diabe<-as.numeric(BRFSS.13$diabe)
class(BRFSS.13$diabe)
table(BRFSS.13$diabe)
summary(BRFSS.13$diabe)


#Recoding variable cvdinfr4#

table(BRFSS.13$cvdinfr4)
BRFSS.13$cvdinfr2<-as.numeric(BRFSS.13$cvdinfr4)

#Convert 2 values to 0#
BRFSS.13$cvdinfr2[BRFSS.13$cvdinfr2==2]<-0

#Convert 7 values to blank#
BRFSS.13$cvdinfr2[BRFSS.13$cvdinfr2==7]<-" "

BRFSS.13$cvdinfr2<-as.numeric(BRFSS.13$cvdinfr2)
class(BRFSS.13$cvdinfr2)
table(BRFSS.13$cvdinfr2)
summary(BRFSS.13$cvdinfr2)


#Recoding variable cvdcrhd4#

table(BRFSS.13$cvdcrhd4)
BRFSS.13$cvdcrhd2<-as.numeric(BRFSS.13$cvdcrhd4)

#Convert 2 values to 0#
BRFSS.13$cvdcrhd2[BRFSS.13$cvdcrhd2==2]<-0

#Convert 7 values to blank#
BRFSS.13$cvdcrhd2[BRFSS.13$cvdcrhd2==7]<-" "

BRFSS.13$cvdcrhd2<-as.numeric(BRFSS.13$cvdcrhd2)
class(BRFSS.13$cvdcrhd2)
table(BRFSS.13$cvdcrhd2)
summary(BRFSS.13$cvdcrhd2)


#Recoding variable cvdstrk3#

table(BRFSS.13$cvdstrk3)
BRFSS.13$cvdstrk2<-as.numeric(BRFSS.13$cvdstrk3)

#Convert 2 values to 0#
BRFSS.13$cvdstrk2[BRFSS.13$cvdstrk2==2]<-0

#Convert 7 values to blank#
BRFSS.13$cvdstrk2[BRFSS.13$cvdstrk2==7]<-" "

BRFSS.13$cvdstrk2<-as.numeric(BRFSS.13$cvdstrk2)
class(BRFSS.13$cvdstrk2)
table(BRFSS.13$cvdstrk2)
summary(BRFSS.13$cvdstrk2)


#Create data frame with 
df2 <- data.frame(BRFSS.13$diabe, BRFSS.13$cvdinfr2, BRFSS.13$cvdcrhd2, BRFSS.13$cvdstrk2)
str(df2)


## 49. Total quantity of reported chronic diseases (chrotot)##

chrotot <- rowSums(df2, na.rm=T)
BRFSS.13$chrotot <- chrotot
table(BRFSS.13$chrotot)
summary(chrotot)

rm(df2)


## 50. Report of any chronic condition (anychronc) ##

BRFSS.13$anychronc <- as.factor(BRFSS.13$chrotot)


 BRFSS.13$anychronc <-  ifelse(test = BRFSS.13$anychronc == "0", yes = "No", 
                               no = "Oneormore" )

BRFSS.13$anychronc <- factor(BRFSS.13$anychronc)
table(BRFSS.13$anychronc)


# iyear -----------------------------------------------------------------------------

##############################################################
################### CHILD MANAGEMENT #########################
##############################################################

BRFSS.13$casthma <- factor(BRFSS.13$casthdx)
label.cla <- list(BRFSS.13$casthma, Yes="1", No="2")
levels(BRFSS.13$casthma)<- label.cla
BRFSS.13$casthma<-factor(BRFSS.13$casthma)
table(BRFSS.13$casthma)


## 3. Does the child Still have asthma (casthnow) ##

# Variable to Calculate current asthma in all population
BRFSS.13$ccurasth <- as.factor(BRFSS.13$casthnow)
summary(BRFSS.13$ccurasth)

BRFSS.13$ccurasth <- replace(BRFSS.13$ccurasth, BRFSS.13$ccurasth == "7", NA)
BRFSS.13$ccurasth[is.na(BRFSS.13$ccurasth)] <- 2
levels(BRFSS.13$ccurasth)<- label.cla
BRFSS.13$ccurasth<-factor(BRFSS.13$ccurasth)
table(BRFSS.13$ccurasth)

# Variable to claculate current asthma prevalence within those that reported lifetime asthma
BRFSS.13$casthnow <- factor(BRFSS.13$casthnow)
levels(BRFSS.13$casthnow) <- label.cla
BRFSS.13$casthnow <- factor(BRFSS.13$casthnow)
table(BRFSS.13$casthnow)

## 4. Child gender

BRFSS.13$csexg <- BRFSS.13$rcsgender
table(BRFSS.13$csexg)

BRFSS.13$csexg <- replace(BRFSS.13$csexg, BRFSS.13$csexg == 9, NA)  
BRFSS.13$csexg <- factor(BRFSS.13$csexg, labels = c("Male", "Female")) 
table(BRFSS.13$csexg)


## 5. Child age in categories

# calculate child age group ---------------------------------------------------------
table(BRFSS.13$cage)
# Values less than 0 was found in this data set. 

BRFSS.13$cageg <- BRFSS.13$cage
table(BRFSS.13$cageg)
class(BRFSS.13$cageg)
library(Hmisc)

# Convert 888 and 999 values to NA's ------------------------------------------------
# BRFSS.13$cageg <- replace(BRFSS.13$cageg, BRFSS.13$cageg == 777, NA)
# BRFSS.13$cageg <- replace(BRFSS.13$cageg, BRFSS.13$cageg == 999, NA)
# BRFSS.13$cageg <- replace(BRFSS.13$cageg, BRFSS.13$cageg >= 217, NA)
# 
# # The cut will be to create a variable with the follwing aggregation
# # 0-4; 5-9; 10-14; 15-17
# 
# # This cut procedure was carefully veryfy and it is correct
# BRFSS.13$cageg <- cut2(BRFSS.13$cageg, cuts = c(60, 130, 180, 216))
# #label cuts
BRFSS.13$cageg <- factor(BRFSS.13$cageg,
                        labels=c("00-04", "05-09", "10-14", "15-17"))

table(BRFSS.13$cageg)


#Remove objects that will not be use

rm(list= ls()[!(ls() %in% c('BRFSS.13', 'nN_13'))])
gc()

## Save image & source ()##

if (Sys.info()["sysname"] == "Linux"){
  save.image (file = "/.RData")
} else if (Sys.info()["sysname"] == "Windows"){
  save.image (file = "/.RData")
}


