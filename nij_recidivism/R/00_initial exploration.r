
require(here)
require(psych)

################################################################################
# Read the datasets

full_data <- read.csv(here('data','NIJ_s_Recidivism_Challenge_Training_Dataset.csv'))

test_submit <- read.csv(here('data','NIJ_s_Recidivism_Challenge_Test_Dataset1.csv'))

colnames(full_data)

colnames(test_submit)
################################################################################

################# Review the variables #########################################

# ID

length(unique(full_data$ID))

length(unique(test_submit$ID))

# Gender

table(full_data$Gender)
table(full_data$Gender)/nrow(full_data)

table(test_submit$Gender)
table(test_submit$Gender)/nrow(test_submit)

# Race

table(full_data$Race)
table(full_data$Race)/nrow(full_data)

table(test_submit$Race)
table(test_submit$Race)/nrow(test_submit)

# Age at Release

table(full_data$Age_at_Release)
table(full_data$Age_at_Release)/nrow(full_data)

table(test_submit$Age_at_Release)
table(test_submit$Age_at_Release)/nrow(test_submit)

# Residence PUMA

# https://www.census.gov/programs-surveys/geography/guidance/geo-areas/pumas.html#:~:text=Contact%20Information-,PUMA%20Overview,and%20the%20U.S.%20Virgin%20Islands.

# https://www2.census.gov/geo/pdfs/reference/puma/2010_PUMA_Names.pdf

# Code	PUMAs included
# 1	1003, 4400
# 2	1008, 4300
# 3	1200, 1300
# 4	1400, 1500, 1600
# 5	1700, 1800
# 6	2001, 2002, 2003, 4005
# 7	100, 200, 500
# 8	4000, 4100, 4200
# 9	5001, 6001, 6002
# 10	2400, 5002
# 11	1001, 3004, 4600
# 12	1002, 1005, 3300, 3400, 4001, 4002, 4006
# 13	3101, 3102
# 14	1900, 3900, 4003, 4004
# 15	3001, 3002, 3003, 3005
# 16	2500, 4500
# 17	2800, 2900, 3200, 3500
# 18	600, 700, 800
# 19	900, 1100
# 20	300, 401, 402
# 21	1004, 2100
# 22	2200, 2300
# 23	1006, 1007, 2004
# 24	2600, 2700
# 25	3600, 3700, 3800

# Additional data from these geographic areas may be added.

# Gang Affiliated

table(full_data$Gang_Affiliated)
table(full_data$Gang_Affiliated)/nrow(full_data)

table(test_submit$Gang_Affiliated)
table(test_submit$Gang_Affiliated)/nrow(test_submit)

# Supervision_Risk_Score_First

table(full_data$Supervision_Risk_Score_First)

table(test_submit$Supervision_Risk_Score_First)

# Supervision_Risk_Score_First

table(full_data$Supervision_Level_First)

table(test_submit$Supervision_Level_First)


# Education Level

table(full_data$Education_Level)

table(test_submit$Supervision_Level_First)

# Dependents

table(full_data$Dependents)

table(test_submit$Dependents)


# Prison Offense

table(full_data$Prison_Offense)

table(test_submit$Prison_Offense)

# Prison Years

table(full_data$Prison_Years)

table(test_submit$Prison_Years)

# Prior_Arrest_Episodes_Felony

table(full_data$Prior_Arrest_Episodes_Felony)
      
table(test_submit$Prior_Arrest_Episodes_Felony)

# Prior_Arrest_Episodes_Misd

table(full_data$Prior_Arrest_Episodes_Misd)

table(test_submit$Prior_Arrest_Episodes_Misd)
                  
# Prior_Arrest_Episodes_Violent

table(full_data$Prior_Arrest_Episodes_Violent)

table(test_submit$Prior_Arrest_Episodes_Violent)
                  
# Prior_Arrest_Episodes_Property

table(full_data$Prior_Arrest_Episodes_Property)

table(test_submit$Prior_Arrest_Episodes_Property)
                  
# Prior_Arrest_Episodes_Drug       

table(full_data$Prior_Arrest_Episodes_Drug)

table(test_submit$Prior_Arrest_Episodes_Drug)
                  
# X_v1                            

table(full_data$X_v1)

table(test_submit$X_v1)
                  
# Prior_Arrest_Episodes_DVCharges

table(full_data$Prior_Arrest_Episodes_DVCharges)

table(test_submit$Prior_Arrest_Episodes_DVCharges)
                  
# Prior_Arrest_Episodes_GunCharges

table(full_data$Prior_Arrest_Episodes_GunCharges)
          
table(test_submit$Prior_Arrest_Episodes_GunCharges)
                  
# Prior_Conviction_Episodes_Felony

table(full_data$Prior_Conviction_Episodes_Felony)

table(test_submit$Prior_Conviction_Episodes_Felony)
                  
# Prior_Conviction_Episodes_Misd

table(full_data$Prior_Conviction_Episodes_Misd)

table(test_submit$Prior_Conviction_Episodes_Misd)
                  
# Prior_Conviction_Episodes_Viol

table(full_data$Prior_Conviction_Episodes_Viol)

table(test_submit$Prior_Conviction_Episodes_Viol)

# Prior_Conviction_Episodes_Prop

table(full_data$Prior_Conviction_Episodes_Prop)

table(test_submit$Prior_Conviction_Episodes_Prop)
                  
# Prior_Conviction_Episodes_Drug

table(full_data$Prior_Conviction_Episodes_Drug)

table(test_submit$Prior_Conviction_Episodes_Drug)
                  
                  
# X_v2

table(full_data$X_v2)
table(test_submit$X_v2)
                  
# X_v3                            
 
table(full_data$X_v3)
table(test_submit$X_v3)

# X_v4

table(full_data$X_v4)
table(test_submit$X_v4)

# Prior_Revocations_Parole

table(full_data$Prior_Revocations_Parole)
table(test_submit$Prior_Revocations_Parole)
                  
# Prior_Revocations_Probation     

table(full_data$Prior_Revocations_Probation)
table(test_submit$Prior_Revocations_Probation)
                  
# Condition_MH_SA                  

table(full_data$Condition_MH_SA)
table(test_submit$Condition_MH_SA)
                  
# Condition_Cog_Ed
  
table(full_data$Condition_Cog_Ed)
table(test_submit$Condition_Cog_Ed)

# Condition_Other
  
table(full_data$Condition_Other)
table(test_submit$Condition_Other)

# Violations_ElectronicMonitoring

table(full_data$Violations_ElectronicMonitoring)
  
  # table(test_submit$Violations_ElectronicMonitoring)
                  
# Violations_Instruction

table(full_data$Violations_Instruction)

  # table(test_submit$Violations_Instruction)
                  
# Violations_FailToReport

table(full_data$Violations_FailToReport)

  #table(test_submit$Violations_FailToReport)

# Violations_MoveWithoutPermission

table(full_data$Violations_MoveWithoutPermission)

  #table(test_submit$Violations_MoveWithoutPermission)
                  
# Delinquency_Reports

table(full_data$Delinquency_Reports)

  #table(test_submit$Delinquency_Reports)
                  
# Program_Attendances

table(full_data$Program_Attendances)

  # table(test_submit$Program_Attendances)
                  
# Program_UnexcusedAbsences

table(full_data$Program_UnexcusedAbsences)

  # table(test_submit$Program_UnexcusedAbsences)
                  
# Residence_Changes

table(full_data$Residence_Changes)

  #table(test_submit$Residence_Changes)
                  
# Avg_Days_per_DrugTest           

describe(full_data$Avg_Days_per_DrugTest)

  # describe(test_submit$Avg_Days_per_DrugTest)
                  
# DrugTests_THC_Positive

describe(full_data$DrugTests_THC_Positive)

  # describe(test_submit$DrugTests_THC_Positive)
                  
# DrugTests_Cocaine_Positive

describe(full_data$DrugTests_Cocaine_Positive)

  # describe(test_submit$DrugTests_Cocaine_Positive)

# DrugTests_Meth_Positive
  
describe(full_data$DrugTests_Meth_Positive)

  # describe(test_submit$DrugTests_Meth_Positive)

# DrugTests_Other_Positive
  
describe(full_data$DrugTests_Other_Positive)

  # describe(test_submit$DrugTests_Other_Positive)

# Percent_Days_Employed
  
describe(full_data$Percent_Days_Employed)

  #describe(test_submit$Percent_Days_Employed)

# Jobs_Per_Year
  
describe(full_data$Jobs_Per_Year)

  # describe(test_submit$Jobs_Per_Year)
                  
# Employment_Exempt
  
  table(full_data$Employment_Exempt)
  
    #table(test_submit$Employment_Exempt)

# Recidivism_Within_3years
  
table(full_data$Recidivism_Within_3years)
          
# Recidivism_Arrest_Year1 
  
table(full_data$Recidivism_Arrest_Year1)

# Recidivism_Arrest_Year2

table(full_data$Recidivism_Arrest_Year2)
                  
# Recidivism_Arrest_Year3

table(full_data$Recidivism_Arrest_Year3)










