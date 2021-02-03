library(hypegrammaR)
library(dplyr)
library(reshape2)

#set working directory
# setwd("C:/Users/Vanessa Causemann/Desktop/REACH/RStuff/GitHub/mainAnalysisSOM20")

#import functions
source("re-formatting_hypegrammaR.R")

#import all necessary files (data set, dap, sampling frame, choices and questions from kobo-tool)
df<-read.csv(file="output/REACH_SOM2006_JMCNA_IV_Data-Set_with_indicators_scored_2021_Jan_28.csv", head=T, dec=".", sep=",", stringsAsFactors = F)
sf<-read.csv(file="input/sampling_frame.csv", head=T, dec=".", sep=",", stringsAsFactors = F)         #from population_patching_and_weighting.R
kobochoices <- read.csv("input/choices.csv", header = T, stringsAsFactors = F)
koboquestions <- read.csv("input/questions.csv", header = T, stringsAsFactors = F)

#create dap:
dependent.variable<- rep("MSNI", 10)
research.question<-rep(NA, 10)
sub.research.question<-rep(NA, 10)
repeat.for.variable<-rep(c("", "state", "settlement_type", "regional", "district"), 2)
independent.variable<-c(rep(c("breadwinner"), 5), rep(c("disp_why1_groups"),5))
independent.variable.type<-rep("categorical", 10)
dependent.variable.type<-rep("categorical", 10)
hypothesis.type<-rep("direct_reporting", 10)

dap_indicators<-data.frame(research.question,
                           sub.research.question,
                           repeat.for.variable,
                           independent.variable,
                           independent.variable.type,
                           dependent.variable,
                           dependent.variable.type,
                           hypothesis.type)



#adjust data set to sampling frame formatting
df$settlement_type[df$idp_settlement=="yes"]<-"IDP"
df$settlement_type[df$idp_settlement=="no"]<-"HC"

sf$stratum_id <- paste(sf$district, sf$settlement_type, sep = "_")
df$stratum_id <- paste(df$district, df$settlement_type, sep = "_")

#re-renaming (because of renaming made for Tableau output)
names(df)[which(names(df) == "weights")]<-"weights_tableau"
names(df)[which(names(df) == "hand_washing_times.cough_sneeze")]<-"hand_washing_times.coughing"
names(df)[which(names(df) == "nutrition_barriers.not_enough_prov")]<-"nutrition_barriers.not"
df$school_transport[df$school_transport=="bus"]<-"business"

#weighting and save weights as column
weight.function <- map_to_weighting(sampling.frame = sf, 
                                    data.stratum.column = "stratum_id",
                                    sampling.frame.population.column = "Population",
                                    sampling.frame.stratum.column = "stratum_id",
                                    data = df)

df$weights_dap <- weight.function(df)

#check difference in weights
#sum(df$weights_tableau-df$weights_dap)

#load questionnaire
questionnaire<-load_questionnaire(data = df,
                                  questions = koboquestions,
                                  choices = kobochoices,
                                  choices.label.column.to.use = "label::English")

#test if all variable names fit
#dap_indicators$dependent.variable %in% names(df)

#make sure to only have character variables
df[indicators]<-lapply(df[indicators],as.character)

#adapt naming to hypegrammaR-style
dap_indicators$dependent.variable<-hypegrammaR:::to_alphanumeric_lowercase(dap_indicators$dependent.variable)

#######################################
##########NEW VAR DAP        ##########
#######################################

dap_extra_var <-dap_indicators

#results-function (can take very long)
start_time <- Sys.time()                                                                                                                    #timer for fun
list_of_results_indicators_extra <-  from_analysisplan_map_to_output(df,
                                                                        analysisplan = dap_extra_var,
                                                                        weighting = weight.function,
                                                                        cluster_variable_name = NULL,
                                                                        questionnaire = questionnaire, confidence_level = 0.90)
end_time <- Sys.time()                                                                                                                      #timer
end_time - start_time                                                                                                                       #timer
#1h

#save results as R file and get it 
list_of_results_indicators_extra %>% saveRDS("output/list_of_results_indicators_extra.RDS")
#list_of_results_indicators_extra <- readRDS("output/list_of_results_indicators_extra.RDS")

#get long table of results 
# long_table_indicators_extra <- list_of_results_indicators_extra$results %>% lapply(function(x) x[["summary.statistic"]]) %>% do.call(rbind, .)
#export results as CSV files
write.csv(long_table_indicators_extra, file= "output/long_table_indicators_extra.csv", row.names=FALSE)
# write.csv(wide_table(long_table_indicators_extra), file= "output/wide_table_indicators_extra.csv", row.names=FALSE)
# write.csv(wide_perc(wide_table(long_table_indicators_extra)), file= "output/wide_table_perc_indicators_extra.csv", row.names=FALSE)

