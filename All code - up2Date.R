############### Final Outputs - SUMMARY ONLY ##############
# This code cannot be run, put at the top for easy access

# INTERVENTION 1A
scales::dollar(net_benefits_1A - costs_1A) # -$885,841 - Suggests costs out weight benefits.

# INTERVENTION 1B

scales::dollar(net_benefits_1B - costs_1B) # $137, 007 - Suggests benefits outweigh costs.

# INTERVENTION 2A

scales::dollar(net_benefits2A - costs2A) # -$1,259,800 - Suggests costs outweigh benefits 


# INTERVENTION 2B

scales::dollar(net_benefits2B - costs2B) # -$425,497 - Suggests costs outweigh benefits, slightly. 


# INTERVENTION 3B

scales::dollar(intervention3B_benefits - intervention3B_costs) #  $1,152,518 So Cost effective. 

# Interbventions 1a 2a combo

scales::dollar(net_benefits12A - costs12A) # $2,222,997 - So here benefits definitely outweigh costs so this would be a good intervention to run

# UNCERTAINTY ANALYSIS

scales::dollar(cba_80) 
# $2,222,997 - So here benefits definitely outweigh costs so this would be a good intervention to run

scales::dollar(cba_70) # $1,309,943

scales::dollar(cba_60) # $435,736

scales::dollar(cba_50) # - $399,622
############### Packages and Data ############
library(tidyverse)
library(ggplot2)
# Assume no Care workers have been trained.
Ghana <- read_csv("Ghana.csv")

# First need to change 2015 USDs to 2016 USDS

deflator_ratio <- 101.035 / 100

############### Interventions for Awareness ################## 

## Intervention A

# Cost Summary 
# training_cost # $868,193.8 cost of training all CHWs.
# total_pop # Total pop of 28 million
# avg_num_households # 5,725,945 households
# households_covered # 4,580, 756 Households covered by Health Workers
# cost_of_all_visits # 260,458.1 total Monthly Cost.  
# yearly_1A_costs # $3,125,498 yearly cost
# total_pop <- Ghana %>% summarise(Total_pop = sum(Population)) # Total pop of 28 million. 

# First Find total CHWs
# This report gives total chws in 2016: 
# https://public.tableau.com/profile/millennium.promise#!/vizhome/CHWCoverage/CHWDistributionDashboard?publish=yes


# We have 17k workers, that cover 80% of pop
# Do we do 80% of households? i.e. 5.7 mil * 0.8 for how many houses they visit? 

## INTERVENTION B

# 71% of total pop is Christian
# 90% of those people say they attend regularly. 

# 18,294,396 Regular ChurchGoers (18.2 mil)

# Next we need to find cost of installing stations 
# 14,000 churches & $100 cost for a station per church.

############### Intervention 1.A ###############################
chws <- read_csv("CHWs.csv") # Has number of health care workers.

one_time_training <- 50 * deflator_ratio # Cost of training one Health worker 

cost_per_vist <-15 * deflator_ratio  # Cost for one health worker to visit houses (per month)

CHW_coverage <- 0.8 # Percentage of Ghana all health workers cover. 

increase_in_careseeking_1A <- 0.8 # Percent of those covered who would seek healthcare under intervention

Num_CHWs <- sum(chws$Num) # Total healthcare workers

training_cost <- Num_CHWs * one_time_training # total cost of training all health workers


cost_of_all_visits <- cost_per_vist * (Num_CHWs)
yearly_1A_costs <- 12 * cost_of_all_visits # $3,125,498 yearly cost to do door to door visits. 



Ghana$Unaware_of_Hypertension <-
  Ghana$Hypertension_Prevalence - Ghana$Aware # Creates column detailing those with Hypertension but unaware

Ghana_1A <-
  Ghana %>% select(
    age,
    Population,
    Hypertension_Prevalence,
    Deaths,
    Unaware_of_Hypertension,
    Awareness_percent,
    Aware,
    Under_control_percent,
    Under_control,
    NonFatalCVD,
    FatalCVD
  )
head(Ghana_1A) # Created a subset, just for easier reading really. Not Necessary, just incase of mistakes too.


# newly aware population after intervention
Ghana_1A$intervention_Aware <-
  (Ghana_1A$Unaware_of_Hypertension * CHW_coverage * increase_in_careseeking_1A) + Ghana_1A$Aware



# So Here, after the intervention, 80% of the population who are unaware they have hypertension are covered
# Those who are covered, are visited by a health worker, and as such, 80% of them seek care.
# In other words they are now aware of their hypertension
# So We move this 80% into the "Aware of hypertension" group, and give them the same odds of adhering to treatment.
# So multiply this new population by the existing percentages we have for those who keep their hypertension under control.

# In short, 80% of unaware people become Aware and act like it (adhere at same rates as baseline)

# New aware population multiplied by existing Adherence percentages
Ghana_1A$intervention_control_pop <-
  Ghana_1A$intervention_Aware * Ghana_1A$Under_control_percent


# So now we have an intervention group with a number of: Unaware, Aware, Control.
# And a baseline group with a number of : Unaware, Aware, Control.
# Both of which should add up to population.

# So now: create a function that will calculate the costs and deaths of baseline vs intervention.

cost_per_patient_yr1 <- 76.92

# This function takes those who do not have their Hypertension under control 
# (unaware + aware - control) <- we minus control as Aware is all people who are aware, 
#                                even those who have it under control. 

# Then multiplies that by the chance of a non fatal CVD event * the yearly cost after a NonFatal CVD event

# THEN takes those who do not have it under control and calculates the death rate.alongside avg cfr for those who do
# multiplies each of these two groups by the VSL's to get the costs of deaths. 

# Finally sum both up to get the monetary value of all non-fatal and fatal cases in 2016.


calc_total <- function(prevelance, control) {
  nonfatal <-
    (prevelance - control) * Ghana$NonFatalCVD * cost_per_patient_yr1
  Total_nonfatal <- sum(nonfatal)
  
  fatal <-
    ((prevelance - control) * Ghana$FatalCVD * Ghana$VSL) + (control * Ghana$avg_cfr * Ghana$VSL)
  Total_fatal <- sum(fatal)
  
  total_costs <- sum(c(Total_nonfatal, Total_fatal))
  return(total_costs)
}


# This calculates above for the baseline
Baseline_overall <-
  calc_total(Ghana_1A$Hypertension_Prevalence,
             Ghana_1A$Under_control)



# and then for intervention
Intervention1A_overall <-
  calc_total(
    Ghana_1A$Hypertension_Prevalence,
    Ghana_1A$intervention_control_pop
  )


# Finally we compare the two to get a cost benefit analysis.

Baseline_overall # $16,054,320
Intervention1A_overall # $12,946,470

baseline_costs <- 0
intervention1A_costs <- (yearly_1A_costs + training_cost)


net_benefits_1A <- Baseline_overall - Intervention1A_overall
costs_1A <- intervention1A_costs - baseline_costs

results_1A <- net_benefits_1A - costs_1A

scales::dollar(net_benefits_1A) # $3,107,850
scales::dollar(costs_1A)    # $3,993,691

scales::dollar(net_benefits_1A - costs_1A) # -$885,841 - Suggests costs out weight benefits. 

# But this would become positive I'm sure if coupled with intervention 2A. 



############### Intervention 1.B ###############################

# BP Stations in Churches
# 14,000 Churches, $100 to set up BP station per church
# 50% increase in care seeking among those covered. 


christians_prop <- 0.71 # Percentage of Christians in Ghana

christian_churchgoer_prop <- 0.9 # Proportion of Christians who attend Church

churchgoer_prop <- christians_prop * christian_churchgoer_prop # Proportion of population that attends church. 

churches <- 14000 # Total Churches

cost_of_station <- 100 * deflator_ratio # Cost of a BP station (adjusted to 2016)

increase_in_careseeking_church <- 0.5 # Increase in careseeking as result of intervention

total_stations_cost <- churches * cost_of_station # total cost of $1,414,490 


Ghana_church <-
  Ghana %>% select(
    age,
    Population,
    Hypertension_Prevalence,
    Deaths,
    Aware,
    Unaware_of_Hypertension,
    Under_control_percent,
    Under_control,
    NonFatalCVD,
    FatalCVD
  )

head(Ghana_church)




# Assuming Churchgoers are evenly distributed by age
# There is an absence of any credible data to suggest otherwise 
# and most specific data we found was the 90% at population level.
# However further work should be done to determine if this is fair. 
# Uk for instance has higher proportions among older people. but also only like 30% on avg at population level. 

# This could affect results as: if higher proportion of older people do attend church then
# more people who have higher risk will become aware. 
# as is, age ranges with lower risk but higher proportions of being unaware will be modeled. 
# So may actually be okay trade off that gives similar results. 

Ghana_church$regular_churchgoers <- Ghana_church$Population * churchgoer_prop

# Now among this group we need to get unaware, aware and control sizes

# newly aware population after intervention
Ghana_church$church_intervention_aware <- 
  (Ghana_church$Unaware_of_Hypertension * churchgoer_prop * increase_in_careseeking_church) + Ghana_church$Aware

Ghana_church$church_intervention_control <-
  Ghana_church$church_intervention_aware * Ghana_church$Under_control_percent

# Now we have our Data correct. We use our functions to carry out the analysis. 

#Use same function as before. 
calc_total <- function(prevalence,control){
  nonfatal <- (prevalence - control) * Ghana$NonFatalCVD * cost_per_patient_yr1
  Total_nonfatal <- sum(nonfatal)
  
  fatal <- ((prevalence - control) * Ghana$FatalCVD * Ghana$VSL) + (control * Ghana$avg_cfr * Ghana$VSL)
  Total_fatal <- sum(fatal)
  
  total_costs <- sum(c(Total_nonfatal, Total_fatal))
  return(total_costs)
}

church_intervention_overall <-
  calc_total(
    Ghana_church$Hypertension_Prevalence,
    Ghana_church$church_intervention_control
  )

church_intervention_overall # $ 14,502,823


# Our baseline overall and costs remain the same from intervention 1. 
Baseline_overall # $16,054,401
baseline_costs <- 0 # There are no existing costs right? 

church_intervention_costs <- total_stations_cost


net_benefits_1B <- Baseline_overall - church_intervention_overall
costs_1B <- church_intervention_costs - baseline_costs

scales::dollar(net_benefits_1B) # $1,551,497
scales::dollar(costs_1B)    # $1,414,490

scales::dollar(net_benefits_1B - costs_1B) # $137, 007 - Suggests benefits outweigh costs.






############### Interventions for Adherence ##############

############### Intervention 2.A #####
#, is also monthly door to door visits by CHWs, we assume in 2a that the benefits are a 50% increase in those who have it under control
# and assume that the awareness level stays the same 
# Therefore the costs of 2a are similar to 1a, just the cost of annual door to door, not the cost of training



# Now calculate benefits of 50% increase in control 

#create new subset to avoid mistakes


Ghana_INT2A <-
  Ghana %>% select(
    age,
    Population,
    Hypertension_Prevalence,
    Deaths,
    Aware,
    Under_control,
    NonFatalCVD,
    FatalCVD
  )
CHW_coverage <- 0.8 # Percentage of Ghana all health workers cover. 

INT2a_adherenceIncrease <- 0.5 # Percent of those covered who now adhere to treatment under intervention

Ghana_INT2A$INT2a_control <- Ghana$Under_control + Ghana$Under_control*INT2a_adherenceIncrease*CHW_coverage    #column showing number of people who now have their hypertension under control


cost_per_patient_yr1 <- 76.92       # Both taken from Excel Sheet. 


calc_total <- function(prevalence,control){
  nonfatal <- (prevalence - control) * Ghana$NonFatalCVD * cost_per_patient_yr1
  Total_nonfatal <- sum(nonfatal)
  
  fatal <- ((prevalence - control) * Ghana$FatalCVD * Ghana$VSL) + (control * Ghana$avg_cfr * Ghana$VSL)
  Total_fatal <- sum(fatal)
  
  total_costs <- sum(c(Total_nonfatal, Total_fatal))
  return(total_costs)
}

# This calculates above for the baseline
Baseline2A_overall <-
  calc_total(
    Ghana_INT2A$Hypertension_Prevalence,
    Ghana_INT2A$Under_control
  )
# Baseline negative outcomes outcomes 16,054,401

# and then for intervention 
Intervention2A_overall <-
  calc_total(
    Ghana_INT2A$Hypertension_Prevalence,
    Ghana_INT2A$INT2a_control
  )
# Int 2A negative outcomes are 14,188,735






# Finally we compare the two to get a cost benefit analysis. 

Baseline2A_overall # $16,054,320, NOTE THIS IS SAME AS 1A BASELINE OBV
Intervention2A_overall # $14,188,622

baseline2A_costs <- 0 
intervention2A_costs <- yearly_1A_costs #$3125498 (2016 US dollars)


net_benefits2A <- Baseline2A_overall - Intervention2A_overall

costs2A <- intervention2A_costs - baseline2A_costs

scales::dollar(net_benefits2A) # $1,865,698
scales::dollar(costs2A)    # $3,125,498

scales::dollar(net_benefits2A - costs2A) # -$1,259,800 - Suggests costs outweigh benefits 


############### Intervention 2.B #####
#, is a national ad campaign to improve adherence to treatment,
# we assume in 2b that the benefits are a 20% increase in those who have it under control
# and assume that the awareness level stays the same 
# The costs of 2b are the costs of an ad campaign, which we found to be $1358346, taken from
# a similar ad campaign in Nigeria (chosen as similar economic capabilities as Ghana), and scaled down by population




#create new subset to avoid mistakes


Ghana_INT2B <-
  Ghana %>% select(
    age,
    Population,
    Hypertension_Prevalence,
    Deaths,
    Aware,
    Under_control,
    NonFatalCVD,
    FatalCVD
  )


INT2B_adherenceIncrease <- 0.2

Ghana_INT2B$INT2B_control <- Ghana$Under_control + Ghana$Under_control*INT2B_adherenceIncrease    #column showing number of people who now have their hypertension under control


cost_per_patient_yr1 <- 76.92       # Both taken from Excel Sheet. 


calc_total <- function(prevalence,control){
  nonfatal <- (prevalence - control) * Ghana$NonFatalCVD * cost_per_patient_yr1
  Total_nonfatal <- sum(nonfatal)
  
  fatal <- ((prevalence - control) * Ghana$FatalCVD * Ghana$VSL) + (control * Ghana$avg_cfr * Ghana$VSL)
  Total_fatal <- sum(fatal)
  
  total_costs <- sum(c(Total_nonfatal, Total_fatal))
  return(total_costs)
}

# This calculates above for the baseline
Baseline2B_overall <-
  calc_total(
    Ghana_INT2B$Hypertension_Prevalence,
    Ghana_INT2B$Under_control
  )
# Baseline negative outcomes outcomes 16,054,401, same as before of course

# and then for intervention 
Intervention2B_overall <-
  calc_total(
    Ghana_INT2B$Hypertension_Prevalence,
    Ghana_INT2B$INT2B_control
  )
# Int 2B negative outcomes are 15,121,568

Baseline2B_overall - Intervention2B_overall





# Finally we compare the two to get a cost benefit analysis. 

Baseline2B_overall # $16,054,401, NOTE THIS IS SAME AS 1A BASELINE OBV
Intervention2B_overall # $15,121,471

baseline2B_costs <- 0 
intervention2B_costs <- 1358346


net_benefits2B <- Baseline2B_overall - Intervention2B_overall

costs2B <- intervention2B_costs - baseline2B_costs

scales::dollar(net_benefits2B) # $932,849
scales::dollar(costs2B)    # $1,358,346

scales::dollar(net_benefits2B - costs2B) # -$425,497 - Suggests costs outweigh benefits, slightly. 







############### Interventions for LifeStyle ##############
############### Intervention 3.A #################

# A national program to reduce salt intake, similar to one run in the UK 
# that involved agreements with industry to reduce sodium in their products, 
# government monitoring industry compliance to agreements, and educating the public 
# through ad campaigns that aim to change consumer behaviour.

# !!!! This was deemed to be a bad option using more qualitative methods, see word Doc !!!!!



############### Intervention 3.B #################

# A national ad campaign to promote exercise 
# and developing and promoting a mobile application 
# that teaches individuals what exercises to conduct.

Ad_cost <- (7329084 * (101.035/83.483) )/6.53 # initial calculation of ad campaign. 

# Costs
Ad_cost <- 1358346 # $1,358,346 or $1.3 million - 1 Year
App_cost <- 700000  # $700,000 or $700k 

prevalence_reduction <- 0.8 # 20% reduction which means new prevalence is 80% of baseline. 

Awareness <- c(rep(0,3),0.38,rep(0.36,5),rep(0.479, 3), rep(0.492,3))
Ghana$Awareness_percent <- Awareness


Ghana_INT3B <- Ghana %>% select(
  age,
  Population,
  Hypertension_Prevalence,
  Deaths,
  Awareness_percent,
  Under_control_percent,
  NonFatalCVD,
  FatalCVD
)

# Here we want to create new columns which reflect the new levels of prevalence. 
# From here, multiply new prevalence to get new Aware,Unaware and control populations. 
# Conduct CBA on this. 

Ghana_INT3B$INT3B_prevalence <- Ghana_INT3B$Hypertension_Prevalence * prevalence_reduction
Ghana_INT3B$INT3B_aware <- Ghana_INT3B$INT3B_prevalence * Ghana_INT3B$Awareness_percent
Ghana_INT3B$INT3B_control <- Ghana_INT3B$INT3B_aware * Ghana_INT3B$Under_control_percent

calc_total <- function(prevelance, control) {
  nonfatal <-(prevelance - control) * Ghana$NonFatalCVD * cost_per_patient_yr1
  Total_nonfatal <- sum(nonfatal)
  
  fatal <-
    ((prevelance - control) * Ghana$FatalCVD * Ghana$VSL) + (control * Ghana$avg_cfr * Ghana$VSL)
  Total_fatal <- sum(fatal)
  
  total_costs <- sum(c(Total_nonfatal, Total_fatal))
  return(total_costs)
}

intervention3B_overall <-
  calc_total(
    Ghana_INT3B$INT3B_prevalence,
    Ghana_INT3B$INT3B_control
  )

Baseline_overall # $16,054,320
intervention3B_overall #  $ 12,843,456



baseline_costs <- 0 # There are no existing costs 
intervention3B_costs <- (Ad_cost + App_cost)
scales::dollar(intervention3B_costs) # $2,058,346


intervention3B_benefits <- Baseline_overall - intervention3B_overall
intervention3B_costs <- intervention3B_costs - baseline_costs

scales::dollar(intervention3B_benefits) # $3,210,864
scales::dollar(intervention3B_costs)# $2,058,346

scales::dollar(intervention3B_benefits - intervention3B_costs) #  $1,152,518 So Cost effective.  
                                                                



############### Combining intervention 1.A and 2.A ############



# Now, can we combine interventions 1A and 2A as they share a lot of the same costs

#### Intervention 12A, is a national ad campaign to improve adherenceto treatment,
# we assume in 12A that the benefits are an 80% increase in awareness for those covered
#####20% increase in those who have it under control
# and assume that the awareness level stays the same 
# The costs of 12A are the costs of an ad campaign, which we found o be $1358346, taken from
# a similar ad campaign in Nigeria (chosen as similar economic capabilities as Ghana), and scaled down by population




#create new subset to avoid mistakes


Ghana_INT12A <-
  Ghana %>% select(
    age,
    Population,
    Hypertension_Prevalence,
    Deaths,
    Aware,
    Unaware_of_Hypertension,
    Under_control_percent,
    Under_control,
    NonFatalCVD,
    FatalCVD
  )


increase_in_careseeking_INT12A <- 0.8 # Percent of those covered who would seek healthcare under intervention
# newly aware population after intervention
Ghana_INT12A$intervention_Aware<-
  (Ghana_INT12A$Unaware_of_Hypertension * CHW_coverage * increase_in_careseeking_INT12A) + Ghana_INT12A$Aware


# Population of those with hypertension but are unaware, in intervention.
Ghana_INT12A$Intervention_Unware<- Ghana_INT12A$Hypertension_Prevalence - Ghana_INT12A$intervention_Aware

# New aware population multiplied by existing Adherence percentages
Ghana_INT12A$intervention_control_pop <-
  Ghana_INT12A$intervention_Aware * Ghana_INT12A$Under_control_percent 



# Now increase the adherance by 50% as in Int 2a
Ghana_INT12A$intervention_control_pop <-
  Ghana_INT12A$intervention_control_pop  + Ghana_INT12A$intervention_control_pop*INT2a_adherenceIncrease*CHW_coverage



cost_per_patient_yr1 <- 76.92       # Both taken from Excel Sheet. 


calc_total <- function(prevalence,control){
  nonfatal <- (prevalence - control) * Ghana$NonFatalCVD * cost_per_patient_yr1
  Total_nonfatal <- sum(nonfatal)
  
  fatal <- ((prevalence - control) * Ghana$FatalCVD * Ghana$VSL) + (control * Ghana$avg_cfr * Ghana$VSL)
  Total_fatal <- sum(fatal)
  
  total_costs <- sum(c(Total_nonfatal, Total_fatal))
  return(total_costs)
}

# This calculates above for the baseline
Baseline12A_overall <-
  calc_total(
    Ghana_INT12A$Hypertension_Prevalence,
    Ghana_INT12A$Under_control
  )
# Baseline negative outcomes outcomes 16,054,401, same as before of course

# and then for intervention 
Intervention12A_overall <-
  calc_total(
    Ghana_INT12A$Hypertension_Prevalence,
    Ghana_INT12A$intervention_control_pop
  )
# Int 12A negative outcomes are 9,837,820

Baseline12A_overall - Intervention12A_overall





# Finally we compare the two to get a cost benefit analysis. 

Baseline12A_overall # $16,054,320 NOTE THIS IS SAME AS 1A BASELINE OBV
Intervention12A_overall # $9,837,632

baseline12A_costs <- 0 


# Here we make a big assumption that combining int 1A and 2A costs the same as just 1A
# At end I will run again but change CHW coverage to a smaller amount than 80%, 
# because we could maybe say that because they are now doing 2 interventions when going door to door it will take them longer
intervention12A_costs <- (yearly_1A_costs + training_cost)

net_benefits12A <- Baseline12A_overall - Intervention12A_overall

costs12A <- intervention12A_costs - baseline12A_costs

scales::dollar(net_benefits12A) # $6,216,689
scales::dollar(costs12A)    # $3,993,691

scales::dollar(net_benefits12A - costs12A) # $2,222,997 - So here benefits definitely outweigh costs so this would be a good intervention to run


############### uncertainty analysis for combo intervention #############
#Try combo again but swith CHW coverage to less than 80%

#create new subset to avoid mistakes


Ghana_INT12AA <-
  Ghana %>% select(
    age,
    Population,
    Hypertension_Prevalence,
    Deaths,
    Aware,
    Unaware_of_Hypertension,
    Under_control_percent,
    Under_control,
    NonFatalCVD,
    FatalCVD
  )

increase_in_careseeking_INT12AA <- 0.8 # Percent of those covered who would seek health care under intervention

INT2a_adherenceIncrease <- 0.5 # Percent of those covered who now adhere to treatment under intervention

cost_per_patient_yr1 <- 76.92       #  taken from Excel Sheet. 


##  Make this amount whatever you need to check different CHW coverage ranges
CHW_coverage_AA <- 0.6

uncertainty_INT12AA <- function(new_coverage = 0.8){
  uncertain_Ghana <- Ghana_INT12AA
  
  # Create new Aware population. 
  uncertain_Ghana$intervention_Aware<-
    (uncertain_Ghana$Unaware_of_Hypertension * new_coverage * increase_in_careseeking_INT12AA) + uncertain_Ghana$Aware
  
  # Population of those with hypertension but are unaware, for intervention.
  uncertain_Ghana$intervention_Unware<-  uncertain_Ghana$Hypertension_Prevalence -  uncertain_Ghana$intervention_Aware
  
  # New aware population multiplied by existing Adherence percentages
  uncertain_Ghana$intervention_control_pop <-
    uncertain_Ghana$intervention_Aware *  uncertain_Ghana$Under_control_percent 
  
  # Now increase the adherance by 50% as in Int 2a
  uncertain_Ghana$intervention_control_pop <-
    uncertain_Ghana$intervention_control_pop  + uncertain_Ghana$intervention_control_pop*INT2a_adherenceIncrease*new_coverage
  return(uncertain_Ghana)
}

calc_total <- function(prevalence,control){
  nonfatal <- (prevalence - control) * Ghana$NonFatalCVD * cost_per_patient_yr1
  Total_nonfatal <- sum(nonfatal)
  
  fatal <- ((prevalence - control) * Ghana$FatalCVD * Ghana$VSL) + (control * Ghana$avg_cfr * Ghana$VSL)
  Total_fatal <- sum(fatal)
  
  total_costs <- sum(c(Total_nonfatal, Total_fatal))
  return(total_costs)
}


# These costs will be consistent across all coverages
baseline12AA_costs <- 0 
intervention12AA_costs <- (yearly_1A_costs + training_cost) # Cost of all visits yearly + training CHWs 

Baseline12AA_overall <- Baseline12A_overall

#### Calc 80% coverage - Taken from above. 

cba_80 <- net_benefits12A - costs12A
scales::dollar(cba_80) 
# $2,222,997 - So here benefits definitely outweigh costs so this would be a good intervention to run


# Calc 70% coverage

cover_70 <- uncertainty_INT12AA(0.7)

cover_70_overall <-
  calc_total(
    cover_70$Hypertension_Prevalence,
    cover_70$intervention_control_pop
  )    

net_benefits_70_percent <- Baseline12A_overall - cover_70_overall

cba_70 <- net_benefits_70_percent - costs12A

scales::dollar(cba_70) # $1,309,943


# Calc 60% cover

cover_60 <- uncertainty_INT12AA(0.6)

cover_60_overall <-
  calc_total(
    cover_60$Hypertension_Prevalence,
    cover_60$intervention_control_pop
  )    

net_benefits_60_percent <- Baseline12A_overall - cover_60_overall

cba_60 <- net_benefits_60_percent - costs12A
scales::dollar(cba_60) # $435,736


# Calc 50% cover

cover_50 <- uncertainty_INT12AA(0.5)

cover_50_overall <-
  calc_total(
    cover_50$Hypertension_Prevalence,
    cover_50$intervention_control_pop
  )    

net_benefits_50_percent <- Baseline12A_overall - cover_50_overall

cba_50 <- net_benefits_50_percent - costs12A
scales::dollar(cba_50) # - $$399,622

all_combos <-
  data.frame( coverage_level = c("80%","70%","60%","50%"),CBA = c(cba_80,cba_70,cba_60,cba_50))

all_combos %>% ggplot + geom_col(aes(coverage_level,CBA,fill = coverage_level),colour="black") + 
  geom_hline(yintercept = 0, linetype ="dashed",colour = "red", size = 1.2) + 
  scale_x_discrete(name = "Health Worker coverage") +
  scale_y_continuous(name = "Overall Benefits",
    labels = scales::dollar) + 
  labs(title = "CBA of combining both door-to-door interventions", 
       subtitle = "Uncertainty analysis accounting for possible drop in coverage") + 
  theme_bw(base_size = 16) + 
  theme(legend.position = "") 
  

############### Graphs ###############

Ghana$age[Ghana$age == "5 to 9"] <- "05 to 09"
Ghana$age[Ghana$age == "1 to 4"] <- "01 to 04"

Ghana %>% ggplot+ geom_col(aes(age,Population),fill='blue') + 
  theme(legend.position = 'none') + 
  labs(title = "Population of each age group, Ghana 2016") +
  ylab("Population") + xlab("Age") + 
  scale_y_continuous(labels = scales::label_comma()) + theme_bw(base_size = 16) + 
  #theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold")) +
  coord_flip() 
# Graphs showing Aware/unaware for baseline then for each intervention. 
# Int 1: aware/unaware before/after
# Int 2: control before/after
# int 3: prevalence before/after

# Look to compare (non) fatal CVD events. Or get numbers. 

Ghana_maps <- Ghana

Ghana_maps <- Ghana_maps %>% 
  select(age,Hypertension_Prevalence,Unaware_of_Hypertension, Aware,Under_control) %>%
  pivot_longer(cols = Hypertension_Prevalence:Under_control,names_to = "Indicators")

Ghana_maps$age[Ghana_maps$age == "5 to 9"] <- "05 to 09"
Ghana_maps$age[Ghana_maps$age == "1 to 4"] <- "01 to 04"


indicators_prop <- c()

for(i in 1:nrow(Ghana_maps)) {
  indicator_p <-
    Ghana_maps$value[i] / sum(Ghana_maps$value[Ghana_maps$Indicators == Ghana_maps$Indicators[i]])
  indicators_prop <- c(indicators_prop, indicator_p)
  
}

Ghana_maps$Indicators_p <- indicators_prop


# Might Take out the "control" variable, It's on a different scale to the rest and might make it confusing for the reader. 
# Same with Prevelance actually. Might make it Aware/Unaware on one graph, then the other two below 
# Using Facet Wrap or something

# Whole Graph is kinda confusing actually. Might have to not do proportions for this? 
# Unless you read it as: "50% of those who are aware they are hypertensive are 70+"
# But should probably reduce variables then. 
Ghana_maps %>% ggplot(aes(x = age, y = Indicators_p, fill = Indicators)) + geom_col(position = "dodge") +
  theme(legend.position = "bottom")  + scale_x_discrete(name = "Ages") +
  scale_y_continuous(
    name = "Proportions attributable to Age Group",
    breaks = seq(0, 0.7, 0.1),
    labels = scales::percent
  ) +
  scale_fill_discrete(
    name = "",
    breaks = c(
      "Hypertension_Prevalence",
      "Unaware_of_Hypertension",
      "Aware",
      "Under_control"
    ),
    labels = c(
      "Hypertension \n prevalence",
      "Unaware they  are \nhypertensive",
      "Aware they are \nhypertensive",
      "Aware they are hypertensive \nand have it under control "
    )
  ) +
  coord_flip()

# Better graph.


  Ghana_maps %>% filter(Indicators == "Unaware_of_Hypertension" |
                          Indicators ==  "Aware") %>%
  ggplot(aes(x = age, y = Indicators_p, fill = Indicators)) + 
    geom_col(position = "dodge")  +
  scale_x_discrete(name = "Ages") +
  scale_y_continuous(
    name = "Proportions attributable to Age Group",
    breaks = seq(0, 0.7, 0.1),
    labels = scales::percent
  ) +
  scale_fill_discrete(
    name = "",
    breaks = c(
      "Unaware_of_Hypertension",
      "Aware"
    ),
    labels = c(
      "Unaware they  are \nhypertensive",
      "Aware they are \nhypertensive"
    )
  ) + 
  theme_bw(base_size = 16) +
  coord_flip() +
  theme(legend.position = "bottom") 

Ghana_baseline_aware_graph

# Intervention 1A graphs 

results_1A
Ghana_1A

Ghana_graph_1A <- Ghana_1A %>% select(age,Under_control,intervention_control_pop) %>%
  pivot_longer(cols = Under_control:intervention_control_pop,names_to = "Indicators")

Ghana_maps$age[Ghana_maps$age == "5 to 9"] <- "05 to 09"
Ghana_maps$age[Ghana_maps$age == "1 to 4"] <- "01 to 04"


indicators_prop <- c()

for(i in 1:nrow(Ghana_graph_1A)) {
  indicator_p <-
    Ghana_graph_1A$value[i] / sum(Ghana_graph_1A$value[Ghana_graph_1A$Indicators == Ghana_graph_1A$Indicators[i]])
  indicators_prop <- c(indicators_prop, indicator_p)
  
}

Ghana_graph_1A$Indicators_p <- indicators_prop

Ghana_graph_1A%>%
  ggplot(aes(x = age, y = Indicators_p, fill = Indicators)) + geom_col(position = "dodge") +
  theme(legend.position = "bottom")  + scale_x_discrete(name = "Ages") +
  scale_y_continuous(
    name = "Proportions attributable to Age Group",
    breaks = seq(0, 0.7, 0.1),
    labels = scales::percent
  ) +
  scale_fill_discrete(
    name = "",
    labels = c(
      "Unaware they  are \nhypertensive",
      "Aware they are \nhypertensive"
    )
  ) +
  coord_flip()
  
x<- 1
############### NOTES ON THESE FILES ########
############### Creating Complete Data Frame: if interested ##############

# File Notes
# Note: some columns were removed manually in Excel after they were deemed no longer necessary
# No 0-9 or 10-14 for deaths, so treated as 0. Reasonable assumption. - For Hypertension
# No over 70s for life expectancy, so took the 4 groups above 70 that are available (70-89) and took mean
# Source: http://ghdx.healthdata.org/gbd-results-tool?params=gbd-api-2019-permalink/2856f008b8cda61239b8d2d1ee1f83da
population <- read_csv("population.csv")
life_expectancy <- read_csv("Life_Expectancy.csv")
prevalence <- read_csv("Prevalence.csv")
all_cause_deaths <- read_csv("all_cause_deaths.csv")

# Population is the population of Ghana by age group for 2016
# Life expectancy is the expected life years for 2016
# Prevalence, deaths and DALYs from Hypertension in Ghana in 2016 
# All Cause death is the total deaths and DALYs in Ghana in 2016

head(life_expectancy)
head(prevalence)



# Fixing Life Expectancy 

over_70 = mean(c(11.14,8.74,6.7,5.23))

life_expectancy_adjusted <- life_expectancy %>% select(location,age,year,val) %>%
  rename(Life_Expectancy = val) %>% 
  add_row(location = "Ghana", age = "70+ years", year = 2016, Life_Expectancy = over_70)

life_expectancy_adjusted

# Fixing Prevalence

prevalence_wider <- prevalence %>% select(measure,location, age, year, val) %>% 
  pivot_wider(names_from = measure, values_from = val) %>% 
  rename(DALYS = `DALYs (Disability-Adjusted Life Years)`) %>% filter(age != '0 to 9') %>%
  mutate(Deaths = if_else(is.na(Deaths), 0, Deaths)) %>%
  add_row(location = "Ghana", age = "1 to 4", year = 2016, Prevalence = 0, DALYS = 0, Deaths = 0) %>%
  add_row(location = "Ghana", age = "5 to 9", year = 2016, Prevalence = 0, DALYS = 0, Deaths = 0) 

prevalence_wider

# Fixing death 

deaths_fixed <- all_cause_deaths %>% filter(cause == "All causes") %>%
  select(measure,age,year,val) %>%
  pivot_wider(names_from = "measure", values_from = "val") %>%
  rename(Deaths_All_Causes = Deaths, DALYS_All_Causes = `DALYs (Disability-Adjusted Life Years)`)

# Fixing Population 

population_fixed <- population %>% filter(location_name == "Ghana" & sex_name == "both") %>% 
  select(age_group_name,val) %>%
  rename(age = age_group_name, Population = val)

# Create final dataframe 

Ghana <- life_expectancy_adjusted %>% full_join(prevalence_wider) %>%
  full_join(deaths_fixed) %>% left_join(population_fixed, by = c("age"))

# Add VSL 

VSLY_Tan <- 9340 * deflator_ratio #2016 - Tanzania 
GNI_Gh <- 1820   #2016
GNI_Tan <- 970   #2016
n <- 1 #assumption? - See if way to find elasticity. 


VSLY_Gh <- VSLY_Tan * (GNI_Gh / GNI_Tan) ^ n


Ghana <- Ghana %>%
  mutate(VSL = VSLY_Gh * Life_Expectancy) # Value of a statistical life, by age group. 

# Add Fatal and non-fatal CVD events

Non_fatal_CVD_event <- c(rep(0.002,6),rep(0.009,4),rep(0.015,3),rep(0.05,2))
Fatal_CVD_event <- c(rep(0.0002,6),rep(0.0005,4),rep(0.003,3),rep(0.01,2))

Ghana$NonFatalCVD <- Non_fatal_CVD_event
Ghana$FatalCVD <- Fatal_CVD_event

