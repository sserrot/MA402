# American Community Survey
library(data.table)
library(magrittr)
library(ggplot2)
library(dplyr)

# 1) Open the data dictionary
# 2) Download and find the data set 
# 3) Read and merge the data into a single dataset
# 4) Choose approx. 10 interesting variables from the dataset
#    See http://www2.census.gov/programs-surveys/acs/tech_docs/pums/data_dict/PUMSDataDict13.txt
# 5) Create a new dataset with only these 10 variables

setwd("~/csv_hus")
list.files()

system.time({
  # var.vec is a list of the variables 
  # you want to read from the file
  var.vec = c('ST','RT','SERIALNO','DIVISION','PUMA', 'ACCESS', 'REGION', 'VEH', 'HINCP', 'HHL', 'FS', 'NP','VALP')
  husa.df = fread("ss13husa.csv", 
                  stringsAsFactors=FALSE,
                  select=var.vec)
  husb.df = fread("ss13husb.csv", 
                  stringsAsFactors=FALSE,
                  select=var.vec)
  # you have created two data frames
})

# Create a single data frame
hus.df = bind_rows(husa.df, husb.df)

# Check the number of rows
nrow(hus.df)

# Check the number of rows and number of columns
dim(hus.df)

# Check the size of the data set
print(object.size(hus.df),unit="Gb")

# Look at the variables 
names(hus.df)
str(hus.df) # and their data types

# Many variables should used to create factor variables
#We will use 10 variables - 
#ST(State), DIVISION, ACCESS (access to internet), REGION, VEH (vehicles), 
#HINCP (household income), HHL(household language), FS(food stamps), NP(number of persons),VALP(property value)
# Variable "ST" (state) is one such variable

# Check values of the ST variable
table(hus.df$ST)

# Recode the ST variable into the ST.f factor variable
ST.values = c(01, 02, 04, 05, 06, 08, 09, 10, 11, 12, 13, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 44, 45, 46, 47, 48, 49, 50, 51, 53, 54, 55, 56)
ST.labels = c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY")
hus.df$ST.f = factor(hus.df$ST, levels=ST.values, labels=ST.labels)


# Compare the tables to ensure the recoding is correct
table(hus.df$ST)
table(hus.df$ST.f)


# Recode the DIVISION variable into the DIVISION.f factor variable
DIVISION.values = c(1, 2, 3, 4, 5, 6, 7, 8, 9)
DIVISION.labels = c("New England", "Middle Atlantic", "East North Central", "West North Central", "South Atlantic", "East South Central", "West South Central", "Mountain", "Pacific")
hus.df$DIVISION.f = factor(hus.df$DIVISION, levels = DIVISION.values, labels = DIVISION.labels)

# Compare the tables to ensure the recoding is correct
table(hus.df$DIVISION)
table(hus.df$DIVISION.f)

# Check values of the ACCESS variable
table(hus.df$ACCESS)

# replace NAs with 0
hus.df$ACCESS[is.na(hus.df$ACCESS)] = 0
summary(hus.df$ACCESS)

# Recode the ACCESS variable into the ACCESS.f factor variable
ACCESS.values = c(0, 1, 2, 3)
ACCESS.labels = c("N/A", "Yes, with subscription to an Internet Service", "Yes, without subscription to an Internet Service", "No Internet Access")
hus.df$ACCESS.f = factor(hus.df$ACCESS, levels = ACCESS.values, labels = ACCESS.labels)

# Compare the tables to ensure the recoding is correct
table(hus.df$ACCESS)
table(hus.df$ACCESS.f)

#Check values of the REGION variable
table(hus.df$REGION)

# Recode the REGION variables into REGION.f
REGION.values = c(1, 2, 3, 4)
REGION.labels = c("Northeast", "Midwest","South","West")
hus.df$REGION.f = factor(hus.df$REGION, levels = REGION.values, labels = REGION.labels)

#compare tables
table(hus.df$REGION)
table(hus.df$REGION.f)


#Check values of the VEH (Vehicle) variable
table(hus.df$VEH)

# Recode as factors
VEH.values = c(0,1,2,3,4,5,6)
VEH.labels = c("No vehicles","One vehicle", "Two vehicles", "Three vehicles","Four vehicles", "Five vehicles", "Six or more vehicles")
hus.df$VEH.f = factor(hus.df$VEH, levels = VEH.values, VEH.labels)

#compare tables --- note it has NA values
table(hus.df$VEH)
table(hus.df$VEH.f)
summary(hus.df$VEH)
summary(hus.df$VEH.f)


#Check values of the HHL (Household language) variable
table(hus.df$HHL)

# Recode as factors
HHL.values = c(1,2,3,4,5)
HHL.labels = c("English only", "Spanish", "Other Indo-European languages","Asian and Pacific Island languages", "Other language")
hus.df$HHL.f = factor(hus.df$HHL, levels = HHL.values, HHL.labels)

#compare tables --- note it has NA values
table(hus.df$HHL)
table(hus.df$HHL.f)
summary(hus.df$HHL)
summary(hus.df$HHL.f)


#Check values of the FS (Yearly Food Stamp) variable
table(hus.df$FS)

# Recode as factors
FS.values = c(1,2)
FS.labels = c("Yes", "No")
hus.df$FS.f = factor(hus.df$FS, levels = FS.values, FS.labels)

#compare tables --- note it has NA values
table(hus.df$FS)
table(hus.df$FS.f)
summary(hus.df$FS)
summary(hus.df$FS.f)

#Check values of the NP (Number of People) variable
table(hus.df$NP)

# Recode as factors
NP.values = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20)
NP.labels = c("Vacant","1 person", "2 people","3 people","4 people","5 people","6 people","7 people","8 people","9 people","10 people",
              "11 people","12 people","13 people","14 people","15 people","16 people","17 people","18 people","19 people","20 people")
hus.df$NP.f = factor(hus.df$NP, levels = NP.values, NP.labels)

#compare tables
table(hus.df$NP)
table(hus.df$NP.f)
summary(hus.df$NP)
summary(hus.df$NP.f)





#Check values of the HINCP (Income) variable
table(hus.df$HINCP)

#prevent scientific notation
options("scipen"=100, "digits"=4)

HINCquantile = cut(hus.df$HINCP,breaks = quantile(hus.df$HINCP, probs = seq(0, 1, 0.25), na.rm = TRUE,
                                            names = FALSE, type = 7), dig.lab = 7) 
summary(HINCquantile)
# Recode as factors by creating ranges
HINCP.values = levels(HINCquantile)
HINCP.labels = c("Loss of $20,000 to $26,700","$26,7000 to $53,300", "$53,300 to $95,000", "$95,000 to $2,090,000")
hus.df$HINCP.f = factor(HINCquantile, levels = HINCP.values, HINCP.labels)

#compare tables --- note it has NA values
table(hus.df$HINCP.f)

summary(hus.df$HINCP)
summary(hus.df$HINCP.f)


#Check values of the VALP (Property value) variable
table(hus.df$VALP)


#prevent scientific notation
options("scipen"=100, "digits"=4)

VALPquantile = cut(hus.df$VALP,breaks = quantile(hus.df$VALP, probs = seq(0, 1, 0.25), na.rm = TRUE,
                                               names = FALSE, type = 7), dig.lab = 7) 
summary(VALPquantile)
# Recode as factors by creating ranges
VALP.values = levels(VALPquantile)
VALP.labels = c("$100 to $90,000","$$90,000 to $160,000", "$160,000 to $300,000", "$300,000 to $4,775,000")
hus.df$VALP.f = factor(VALPquantile, VALP.values,  VALP.labels)
str(hus.df$VALP)
#compare tables --- note it has NA values
table(hus.df$VALP.f)

summary(hus.df$VALP)
summary(hus.df$VALP.f)

hus.df %>%
  ggplot  (aes(x   =HHL.f)) +
  geom_bar(aes(fill=HHL.f)) + scale_fill_brewer()