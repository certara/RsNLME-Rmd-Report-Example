## Set up RsNLME R session
library(Certara.RsNLME)
library(Certara.RsNLME.ModelBuilder)
library(Certara.RsNLME.ModelExecutor)
library(Certara.Xpose.NLME)
library(Certara.ModelResults)
library(Certara.VPCResults)
library(tidyvpc)
library(xpose)
library(ggplot2)
library(ggquickeda)
library(egg)
library(ggh4x)
library(DescTools)
library(tidyr)
##==========================================================================================================
## Read the datafile into R and make some plots
##==========================================================================================================

pkdata <- read.csv("pkdata.csv") 
str(pkdata)

## Graph drug concentration vs. time
## This plot suggests that either a 1 or 2-compartment model
## with IV bolus is a good starting point

p1<-ggplot(subset(pkdata ,AMT==0), aes(x = TIME, y = DV, group = ID)) +  
  geom_point(size=.8) +
  geom_line(alpha=.2) + 
  scale_x_continuous(limits=c(0,24))  #limit plot to single dose rich sampling

p1 + scale_y_log10()  
p1 + facet_wrap_paginate(~ID,ncol=5,nrow=4,page=1) 
p1 + facet_grid(~SEX) 
p1 + facet_grid(~cut(WT,3))

#add line for mean
p1 + stat_summary(fun.y=mean,geom="line",aes(group=NULL),col='red',size=1)


p1 + facet_nested(CENTER + SETTING ~ SEX + STATUS,
             scales="free",switch="y",
             strip = strip_nested(size="variable"))

# ggquickeda is a Rshiny app interface to the ggplot2 package
# run_ggquickeda(pkdata)  
  
##==========================================================================================================
## Define Your PK Model
##==========================================================================================================
ModelName<-"TwoCptIV"

model <- pkmodel(numCompartments = 2,
                  data = pkdata, ID = "ID", Time = "TIME", A1 = "AMT", CObs = "DV",
                  modelName = ModelName)

## Use ?pkmodel to see options, also emaxmodel, pkemaxmodel, pkindirect model, linearmodel 

## View the model 
print(model)

## Set Initial Estimates and remove Random Effect terms from Cl2 and V2
model <- model %>%
  structuralParameter(paramName = "Cl2", hasRandomEffect = FALSE) %>%
  structuralParameter(paramName = "V2", hasRandomEffect = FALSE) %>%
  fixedEffect(effect = c("tvV", "tvCl", "tvV2", "tvCl2"), value = c(80, 6, 100, 9)) %>%
  randomEffect(effect = c("nV", "nCl"), value = c(0.1, 0.1)) %>%
  residualError(predName = "C", SD = 0.2)

## View the updated model 
print(model)

## Add covariates to your model, but don't assign to any parameters yet -- use for base diagnostics

model <- model %>%
  addCovariate(covariate = "WT") %>%
  addCovariate(covariate = "AGE") %>%
  addCovariate(covariate = "CRCL") %>%
  addCovariate(covariate = "SEX", type = "Categorical", levels = c(0, 1), 
               labels = c("male", "female"))

## View the updated model 
print(model)

## If user needs help with code, launch Model Builder shiny app
## modelui<-modelBuilderUI(pkdata,modelName="TwoCptIVui")

##print(modelui)



##==========================================================================================================
## Fit Your PK Model
##==========================================================================================================

## Fit the model
TwoCptIVfit <- fitmodel(model)

## If user needs help with engine settings/options, launch Model Executor Shiny app
## modelExecutorUI(modelui)

## View a summary of estimation results
print(TwoCptIVfit)


#Save as an RDS file so that we can open in RMarkdown and access results
saveRDS(model, file="basemodel.RDS")


##==========================================================================================================
## Create Diagnostic Plots using xpose
##==========================================================================================================

## xposeNlme imports results of an NLME run into xpose database to create commonly used diagnostic plots
xp <- xposeNlme(dir = model@modelInfo@workingDir, modelName = ModelName)

##  DV vs PRED/IPRED, Residuals, -- Many more diagnostics available in xpose package
dv_vs_pred(xp, type = "p", subtitle = "-2LL: @ofv, nSubj: @nind")
dv_vs_ipred(xp, type = "p")
res_vs_idv(xp, type = "ps")
res_vs_pred(xp, type = "ps")


##==========================================================================================================
## Run ModelResultsUI to create Diagnostic Plots and Run Summary Tables
##==========================================================================================================

resultsUI(model)

##==========================================================================================================
## Run a VPC and then use VPCResultsUI to create VPC plot
##==========================================================================================================
vpcmod <- copyModel(model, acceptAllEffects = TRUE, modelName = "vpc")

## Set up VPC arguments to have PRED outputted to simulation output dataset "predout.csv"
vpcSetup <- NlmeVpcParams(numReplicates=100)

## run the vpc using the vpcmodel function
vpcfit <- vpcmodel(model = vpcmod, vpcParams = vpcSetup)

## VPC simulation input dataset (obs data)
ObsData <- vpcfit$predcheck0

## VPC simulation output dataset
SimData <- vpcfit$predout

## launch VPCResultsUI

vpcResultsUI(ObsData,SimData)

vpc <- observed(ObsData, x = IVAR, y = DV) %>%
  simulated(SimData, y = DV) %>%
  binless(optimize = TRUE, interval = c(0L, 7L)) %>%
  vpcstats(qpred = c(0.05, 0.5, 0.95), conf.level = 0.95, quantile.type = 6)

vpcPlot <- ggplot(vpc$stats, aes(x = x)) +
  geom_ribbon(aes(ymin = lo, ymax = hi, fill = qname, col = qname, group = qname), alpha = 0.1, col = NA) +
  geom_line(aes(y = md, col = qname, group = qname)) +
  geom_line(aes(y = y, linetype = qname), size = 1) +
  geom_point(data = vpc$obs[!(blq | alq)], aes(x = x, y = y), color = "#757D8F", size = 2L, shape = 16, alpha = 0.8, show.legend = FALSE) +
  scale_colour_manual(name = "Simulated Percentiles\nMedian (lines) 95% CI (areas)", breaks = c("q0.05", "q0.5", "q0.95"), values = c("#D63636", "#3648D6", "#D63636"), labels = c("5%", "50%", "95%")) +
  scale_fill_manual(name = "Simulated Percentiles\nMedian (lines) 95% CI (areas)", breaks = c("q0.05", "q0.5", "q0.95"), values = c("#D63636", "#3648D6", "#D63636"), labels = c("5%", "50%", "95%")) +
  scale_linetype_manual(name = "Observed Percentiles", breaks = c("q0.05", "q0.5", "q0.95"), values = c("dashed", "solid", "dashed"), labels = c("5%", "50%", "95%")) +
  guides(fill = guide_legend(order = 2), colour = guide_legend(order = 2), linetype = guide_legend(order = 1)) +
  ylab(sprintf("Observed/Simulated percentiles and associated %s%% CI", 100 * vpc$conf.level)) +
  xlab("\nTIME") +
  theme_certara() +
  theme(legend.position = "top")

vpcPlot


##==========================================================================================================
## simulation and target attainment
##==========================================================================================================

pkdatasim <- pkdata %>%    #make a copy of original dataset
  filter(TIME==0) %>%    #keep only rows where time=0
  mutate(ADDL=6,II=24)   #add ADDL (additional doses) and II (interdose interval) variables

#Make a copy of our final model
modelTAsim <- copyModel(model, acceptAllEffects = TRUE, modelName = "modelTAsim")

#View the dataset that the model is currently mapped to
modelTAsim@inputData  #shows original dataset

#replace this dataset with dt_simdata using the dataMapping function
modelTAsim <- modelTAsim %>%
  dataMapping(pkdatasim)        

#Check mappings.  Note the presence of '?' for unmapped variables
print(modelTAsim)

#Map AMT and DV to A1 and CObs
modelTAsim <- modelTAsim %>%
  colMapping(c(A1="AMT", CObs="DV"))

#Re-check mappings.  Note that A1 and DV are now mapped
print(modelTAsim)

#Use addADDL to map multiple dose variables
modelTAsim <- modelTAsim %>%
  addADDL(ADDL="ADDL", II="II")

#Re-check mappings.  Note that ADDL and II are now mapped
print(modelTAsim)

#Define table we want as output from the simulation
SimTable <- NlmeSimTableDef(name="SimTable.csv",
                            timesList = seq(144,168,1),
                            variablesList = c("C", "CObs","WT","SEX"))
## Simulation setup
SimSetup <- NlmeSimulationParams(numReplicates = 50,
                                 seed = 1234,
                                 simulationTables = c(SimTable))

## Run the model
modelTAsimfit <- simmodel(modelTAsim, SimSetup)

## Plot simulation results
## Simulated drug concentration at the central compartment
## Read in the simulation output dataset specified in the call to NlmeSimTableDef

## can read the file in from the simulation directory
## SimTableout<-read.csv("<path>/SimTable.csv")

## Or, can extract it from the simulation fit object
SimTableout <- modelTAsimfit$SimTable %>%
  rename("Replicate"="# repl")  #rename repl column to Replicate

## Calculate Cmax and AUC (AUC function from DescTools library)

pkparms <- SimTableout %>% 
  group_by(Replicate, id5) %>% 
  mutate(Cmax = max(CObs), 
         AUC24 = AUC(time,CObs)) %>% 
  distinct(Replicate, id5, .keep_all = T) %>% 
  select(Replicate, id5, SEX, WT, Cmax, AUC24)

#Make Some Plots

targetCmax <- 1    #enter the threshold you'd like to use
targetAUC24 <- 15    #enter the threshold you'd like to use

overtargetAUC24<-length(which(pkparms$AUC24 > targetAUC24)) / length(pkparms$AUC24) * 100
overtargetCmax<-length(which(pkparms$Cmax > targetCmax)) / length(pkparms$Cmax) * 100

TAplotdat <- pkparms %>% pivot_longer(cols=c(Cmax,AUC24),names_to="Parameter") %>% 
mutate(target = ifelse(Parameter=="AUC24",targetAUC24,targetCmax)) 

p1<-ggplot(TAplotdat,aes(x = value, fill=Parameter)) + 
  labs(x="Parameter", y="Count",title="Histograms for AUC24 and Cmax Target Attainment") +
  geom_histogram(col="black") +                 #create the histogram
  geom_vline(aes(xintercept = target), lty=2, lwd=1) + #add a vertical line with our target
  facet_wrap(vars(Parameter),scales="free")         #wrap by Parameter (AUC and Cmax)


#we can use tag_facet from the egg package to add some custom text to the plots  

p1<-tag_facet(p1, x=Inf,y=Inf,                        
          open = "", 
          close = "",
          tag_pool=c(paste("%OverTarget = ",overtargetAUC24),   #this uses the % values we created earlier
                     paste("%OverTarget = ",overtargetCmax)),
          hjust = 1.1,
          vjust = 1.5) +
  theme(strip.text = element_text(),strip.background = element_rect())

p1

## Facet by WT Categories <50kg, 50-100kg, >100kg
TAplotdat <- TAplotdat %>% 
  mutate(WTcut = cut(WT,breaks=c(0,50,100,Inf)))

#calculate attainment by facets

TAsummary <- pkparms %>% 
  group_by(cut(WT,breaks=c(0,50,100,Inf))) %>% 
  summarize(pctovrCmax = round(length(which(Cmax > targetCmax)) / length(Cmax) * 100,0), 
            pctovrAUC24 = round(length(which(AUC24 > targetAUC24)) / length(AUC24) * 100,0))

#plot
p2<-ggplot(TAplotdat,aes(x = value, fill=Parameter)) + 
  labs(x="Parameter", y="Count",title="Histograms for AUC24 and Cmax Target Attainment") +
  geom_histogram(col="black") +                 
  geom_vline(aes(xintercept = target), lty=2, lwd=1) + 
  facet_wrap(Parameter ~ WTcut, scales="free") 

#we can use tag_facet from the egg package to add some custom text to the plots  

p2<-tag_facet(p2, x=Inf, y=Inf, open = "", close = "",
          tag_pool=c(paste("%OverTarget = ",TAsummary$pctovrAUC24[1]),
                     paste("%OverTarget = ",TAsummary$pctovrAUC24[2]),
                     paste("%OverTarget = ",TAsummary$pctovrAUC24[3]),
                     paste("%OverTarget = ",TAsummary$pctovrCmax[1]),
                     paste("%OverTarget = ",TAsummary$pctovrCmax[2]),
                     paste("%OverTarget = ",TAsummary$pctovrCmax[3])),
          hjust = 1.1,
          vjust = 1.5,
          size=3.5) +
theme(strip.text = element_text(),strip.background = element_rect())
  
p2