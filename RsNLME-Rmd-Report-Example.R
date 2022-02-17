## Set up RsNLME R session ----
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
library(gtsummary)
library(flextable)
library(DescTools)
library(tidyr)
library(dplyr)

## Exploratory Analysis/Dataset Summary ----

pkdata <- read.csv("pkdata.csv") 
head(pkdata)

# * Categorical Covariate Summary ----
catcovsumm <- pkdata %>% 
  filter(TIME == 0) %>% 
  mutate(SEX = factor(SEX, labels=c('Male','Female'))) %>% 
  select(SEX, STATUS, CENTER, SETTING) %>%
  tbl_summary(by=CENTER) %>% 
  modify_header(label = "**Variable**") %>% 
  bold_labels() %>% 
  modify_spanning_header(all_stat_cols() ~ "**Center**")

saveRDS(catcovsumm,file='catcovsumm.RDS')

# * Continuous Covariate Summary ----
contcovsumm <- pkdata %>% 
  filter(TIME == 0) %>% 
  select(WT, AGE, CRCL, CENTER) %>%
  tbl_summary(by=CENTER,
              statistic = list(all_continuous() ~ "{mean} ({sd}) [{min}-{max}]")) %>% 
  modify_header(label = "**Variable**") %>% 
  bold_labels() %>% 
  modify_spanning_header(all_stat_cols() ~ "**Center**")

saveRDS(contcovsumm,file='contcovsumm.RDS')

# * Mean Linear Plot ----
meanlinplot <- ggplot(subset(pkdata ,AMT==0), aes(x = TIME, y = DV, group = ID)) +  
  geom_point(size=.8) +
  geom_line(alpha=.2) + 
  scale_x_continuous(limits=c(0,24)) + #limit plot to first dose
  stat_summary(fun=mean,geom="line",aes(group=NULL),col='red',size=1) +
  theme_certara()

saveRDS(meanlinplot,file='meanlinplot.RDS')

# * Mean Log Plot ----
meanlogplot <- ggplot(subset(pkdata ,AMT==0), aes(x = TIME, y = DV, group = ID)) +  
  geom_point(size=.8) +
  geom_line(alpha=.2) + 
  scale_x_continuous(limits=c(0,24)) + #limit plot to first dose
  stat_summary(fun=mean,geom="line",aes(group=NULL),col='red',size=1) +
  scale_y_log10() +
  theme_certara()

saveRDS(meanlogplot,file='meanlogplot.RDS')

# * Mean Linear by SEX ----
meanlinplotbysex <- ggplot(subset(pkdata ,AMT==0), aes(x = TIME, y = DV, group = ID)) +  
  geom_point(size=.8) +
  geom_line(alpha=.2) + 
  scale_x_continuous(limits=c(0,24)) +
  stat_summary(fun=mean,geom="line",aes(group=NULL),col='red',size=1) +
  facet_grid(~SEX) +
  theme_certara()

saveRDS(meanlinplotbysex,file='meanlinplotbysex.RDS')

# * Mean Linear by WT ----
meanlinplotbywt <- ggplot(subset(pkdata ,AMT==0), aes(x = TIME, y = DV, group = ID)) +  
  geom_point(size=.8) +
  geom_line(alpha=.2) + 
  scale_x_continuous(limits=c(0,24)) +
  stat_summary(fun=mean,geom="line",aes(group=NULL),col='red',size=1) +
  facet_grid(~cut(WT,3)) +
  theme_certara()

saveRDS(meanlinplotbywt,file='meanlinplotbywt.RDS')

# ggquickeda is a Rshiny app interface to the ggplot2 package - 
# run_ggquickeda(pkdata)  
  
# Define Your PK Model ----

# * Main Structure ----
ModelName<-"TwoCptIV"
model <- pkmodel(numCompartments = 2,
                  data = pkdata, ID = "ID", Time = "TIME", A1 = "AMT", CObs = "DV",
                  modelName = ModelName)

## Use ?pkmodel to see options, also emaxmodel, pkemaxmodel, pkindirect model, linearmodel 

## View the model 
print(model)

# * Initial Estimates, Random Effects ----
# Here we provide initial estimates and remove eta terms from Cl2 and V2
model <- model %>%
  structuralParameter(paramName = "Cl2", hasRandomEffect = FALSE) %>%
  structuralParameter(paramName = "V2", hasRandomEffect = FALSE) %>%
  fixedEffect(effect = c("tvV", "tvCl", "tvV2", "tvCl2"), value = c(80, 6, 100, 9)) %>%
  randomEffect(effect = c("nV", "nCl"), value = c(0.1, 0.1)) %>%
  residualError(predName = "C", SD = 0.2)

## View the updated model 
print(model)

# * Covariates ----
#Add covariates to your model, here we include a WT covariate on Cl and V
model <- model %>%
  addCovariate(covariate = "WT", 
               center = "Value",
               centerValue=70, 
               effect = c("V", "Cl"))  %>%
  addCovariate(covariate = "AGE") %>%
  addCovariate(covariate = "CRCL") %>%
  addCovariate(covariate = "SEX", type = "Categorical", levels = c(0, 1), 
               labels = c("male", "female"))

# * View the model ---- 
print(model)

## If user needs help with code, launch Model Builder shiny app
## modelui<-modelBuilderUI(pkdata,modelName="TwoCptIVui")
## print(modelui)

# Fit PK Model ----

TwoCptIVfit <- fitmodel(model)

## If user needs help with engine settings/options, launch Model Executor Shiny app
## modelExecutorUI(model)

## View a summary of estimation results
print(TwoCptIVfit)

#Save as an RDS file so that we can open in RMarkdown and access results
saveRDS(model, file="model.RDS")

# Run Model Diagnostics ----

# * Create xpose database object ----

## xposeNlme imports results of an NLME run into xpose database to create commonly used diagnostic plots
xpdb <- xposeNlme(dir = model@modelInfo@workingDir, modelName = ModelName)

# * DV vs PRED/IPRED, CWRES, ETAs ---- 
# * * dv vs pred ----
dvpred <- 
  dv_vs_pred(xpdb, type = "p", subtitle = "-2LL: @ofv, nSubj: @nind")
saveRDS(dvpred,file="dvpred.RDS")

# * * dv vs ipred ----
dvipred <- 
  dv_vs_ipred(xpdb, type = "p", subtitle = "-2LL: @ofv, nSubj: @nind")
saveRDS(dvipred,file="dvipred.RDS")

# * * cwres vs idv ----
cwresidv <- 
  res_vs_idv(xpdb, type = "ps")
saveRDS(cwresidv,file="cwresidv.RDS")

# * * cwres vs pred ----
cwrespred <- 
  res_vs_pred(xpdb, type = "ps")
saveRDS(cwrespred,file="cwrespred.RDS")

# * * eta vs wt ----
etacovcont <- 
  eta_vs_cov(xpdb, 'WT', type = "ps", 
           guide=TRUE,
           guide_color = "black",
           guide_slope = 0,
           guide_intercept = 0)
saveRDS(etacovcont,file="etacovcont.RDS")

# * * eta vs sex ----
etacovcat <- 
  eta_vs_cov(xpdb, 'SEX') + geom_hline(yintercept=0,lty=2)
saveRDS(etacovcat,file="etacovcat.RDS")

##  Many more diagnostics available in xpose package

# Create Run Summary Tables ----
# We can use ModelResultsUI to get code for Run Summary Tables
# resultsUI(model)

# * Overall Run Summary Table ----
xpobj<-xpdb

tableoverall <- xpobj %>%
  get_overallNlme() %>%
  mutate(RetCode = as.integer(RetCode), nObs = as.integer(nObs), nSub = as.integer(nSub), nParm = as.integer(nParm)) %>%
  flextable(col_keys = c("RetCode", "Condition", "logLik", "-2LL", "AIC", "BIC", "nParm", "nObs", "nSub")) %>%
  set_header_labels(values = list(RetCode = "RetCode", Condition = "Condition", logLik = "LL", `-2LL` = "-2LL", AIC = "AIC", BIC = "BIC", nParm = "nParm", nObs = "nObs", nSub = "nSub")) %>%
  set_caption(caption = "Table Overall") %>%
  add_footer_row(values = "Source: script.R", colwidths = 9L) %>%
  colformat_double(digits = 2L) %>%
  align(align = "left", part = "all") %>%
  set_table_properties(layout = "autofit") %>%
  autofit() %>%
  fontsize(size = 10, part = "all") %>%
  font(fontname = "Times New Roman", part = "all") %>%
  bold(part = "header")

saveRDS(tableoverall, file="tableoverall.RDS")

# * Theta Table ----
tabletheta <- xpobj %>%
  get_prmNlme() %>%
  filter(type == "the") %>%
  select(-type, -diagonal, -n) %>%
  mutate(`rse%` = as.numeric(rse) * 100) %>%
  flextable(col_keys = c("name", "label", "value", "se", "rse%", "2.5% CI", "97.5% CI")) %>%
  set_header_labels(values = list(name = "Name", label = "Label", value = "Value", se = "SE", `rse%` = "RSE%", `2.5% CI` = "2.5% CI", `97.5% CI` = "97.5% CI")) %>%
  set_caption(caption = "Table Theta") %>%
  add_footer_row(values = "Source: script.R", colwidths = 7L) %>%
  colformat_double(digits = 2L) %>%
  align(align = "left", part = "all") %>%
  set_table_properties(layout = "autofit") %>%
  autofit() %>%
  fontsize(size = 10, part = "all") %>%
  font(fontname = "Times New Roman", part = "all") %>%
  bold(part = "header")

saveRDS(tabletheta, file="tabletheta.RDS")

# * Omega Table ----
tableomega <- xpobj %>%
  get_prmNlme() %>%
  filter(type == "ome") %>%
  select(-type) %>%
  mutate(`rse%` = as.numeric(rse) * 100) %>%
  left_join(get_eta_shk(xpobj), by = "label") %>%
  flextable(col_keys = c("name", "label", "value", "se", "rse%", "fixed", "diagonal", "2.5% CI", "97.5% CI", "shrinkage%")) %>%
  set_header_labels(values = list(name = "Name", label = "Label", value = "Value", se = "SE", `rse%` = "RSE%", fixed = "Fixed", diagonal = "Diagonal", `2.5% CI` = "2.5% CI", `97.5% CI` = "97.5% CI", `shrinkage%` = "Shrinkage%")) %>%
  set_caption(caption = "Table Omega") %>%
  add_footer_row(values = "Source: script.R", colwidths = 10L) %>%
  colformat_double(digits = 3L) %>%
  align(align = "left", part = "all") %>%
  set_table_properties(layout = "autofit") %>%
  autofit() %>%
  fontsize(size = 10, part = "all") %>%
  font(fontname = "Times New Roman", part = "all") %>%
  bold(part = "header")

saveRDS(tableomega, file="tableomega.RDS")

# * Sigma Table ----
tablesigma <- xpobj %>%
  get_prmNlme() %>%
  filter(type == "sig") %>%
  select(-type) %>%
  mutate(`rse%` = as.numeric(rse) * 100) %>%
  left_join(get_eps_shk(xpobj), by = "label") %>%
  flextable(col_keys = c("name", "label", "value", "se", "rse%", "fixed", "2.5% CI", "97.5% CI", "shrinkage%")) %>%
  set_header_labels(values = list(name = "Name", label = "Label", value = "Value", se = "SE", `rse%` = "RSE%", fixed = "Fixed", `2.5% CI` = "2.5% CI", `97.5% CI` = "97.5% CI", `shrinkage%` = "Shrinkage%")) %>%
  set_caption(caption = "Table Sigma") %>%
  add_footer_row(values = "Source: script.R", colwidths = 9L) %>%
  colformat_double(digits = 3L) %>%
  align(align = "left", part = "all") %>%
  set_table_properties(layout = "autofit") %>%
  autofit() %>%
  fontsize(size = 10, part = "all") %>%
  font(fontname = "Times New Roman", part = "all") %>%
  bold(part = "header")

saveRDS(tablesigma, file="tablesigma.RDS")

# Run VPC Simulation ----
## Run a VPC and then use VPCResultsUI to create VPC plot

vpcmod <- copyModel(model, acceptAllEffects = TRUE, modelName = "vpc")

## Set up VPC arguments to have PRED outputted to simulation output dataset "predout.csv"
vpcSetup <- NlmeVpcParams(numReplicates=100, stratifyColumns = "SEX")

## run the vpc using the vpcmodel function
vpcfit <- vpcmodel(model = vpcmod, vpcParams = vpcSetup)

# Plot VPC ----

## VPC simulation input dataset (obs data)
ObsData <- vpcfit$predcheck0

## VPC simulation output dataset
SimData <- vpcfit$predout

## launch VPCResultsUI
vpcResultsUI(ObsData,SimData)

vpc <- observed(ObsData, x = IVAR, y = DV) %>%
  simulated(SimData, y = DV) %>%
  stratify(~Strat1) %>%
  binless(optimize = TRUE, interval = c(0L, 7L)) %>%
  vpcstats(qpred = c(0.05, 0.5, 0.95), conf.level = 0.95, quantile.type = 6)

vpcPlot <- ggplot(vpc$stats, aes(x = x)) +
  geom_ribbon(aes(ymin = lo, ymax = hi, fill = qname, col = qname, group = qname), alpha = 0.1, col = NA) +
  geom_line(aes(y = md, col = qname, group = qname)) +
  geom_line(aes(y = y, linetype = qname), size = 1) +
  geom_point(data = vpc$obs[!(blq | alq)], aes(x = x, y = y), color = "#757D8F", size = 3L, shape = 16, alpha = 0.5, show.legend = FALSE) +
  scale_colour_manual(name = "Simulated Percentiles\nMedian (lines) 95% CI (areas)", breaks = c("q0.05", "q0.5", "q0.95"), values = c("#D63636", "#3648D6", "#D63636"), labels = c("5%", "50%", "95%")) +
  scale_fill_manual(name = "Simulated Percentiles\nMedian (lines) 95% CI (areas)", breaks = c("q0.05", "q0.5", "q0.95"), values = c("#D63636", "#3648D6", "#D63636"), labels = c("5%", "50%", "95%")) +
  scale_linetype_manual(name = "Observed Percentiles", breaks = c("q0.05", "q0.5", "q0.95"), values = c("dashed", "solid", "dashed"), labels = c("5%", "50%", "95%")) +
  guides(fill = guide_legend(order = 2), colour = guide_legend(order = 2), linetype = guide_legend(order = 1)) +
  facet_wrap(names(vpc$strat), scales = "free", labeller = label_both) +
  ylab(sprintf("Observed/Simulated percentiles and associated %s%% CI", 100 * vpc$conf.level)) +
  xlab("\nTIME") +
  theme_certara() +
  theme(legend.position = "top", legend.title=element_text(size=8), 
                                 legend.text=element_text(size=6),
                                 legend.key.width = unit(0.4, 'cm')) 

saveRDS(vpcPlot, file="vpcPlot.RDS")


# Simulation and Target Attainment ----

# * Setup and Run Simulation ----

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
SimSetup <- NlmeSimulationParams(numReplicates = 100,
                                 seed = 1234,
                                 simulationTables = c(SimTable))

## Run the model
modelTAsimfit <- simmodel(modelTAsim, SimSetup)

# * Create Target Attainment Plots ----

# * * Read in simulation output ----
# can read the file in from the simulation directory
# SimTableout<-read.csv("<path>/SimTable.csv")

## Or, can extract it from the simulation fit object
SimTableout <- modelTAsimfit$SimTable %>%
  rename("Replicate"="# repl")  #rename repl column to Replicate

# * * Calculate Cmax and AUC ----
#(AUC function from DescTools library)

pkparms <- SimTableout %>% 
  group_by(Replicate, id5) %>% 
  mutate(Cmax = max(CObs), 
         AUC24 = AUC(time,CObs)) %>% 
  distinct(Replicate, id5, .keep_all = T) %>% 
  select(Replicate, id5, SEX, WT, Cmax, AUC24)

# * * Overall TA Plot ----

targetCmax <- 1    #enter the threshold you'd like to use
targetAUC24 <- 10    #enter the threshold you'd like to use

overtargetAUC24<-length(which(pkparms$AUC24 > targetAUC24)) / length(pkparms$AUC24) * 100
overtargetCmax<-length(which(pkparms$Cmax > targetCmax)) / length(pkparms$Cmax) * 100

TAplotdat <- pkparms %>% pivot_longer(cols=c(Cmax,AUC24),names_to="Parameter") %>% 
mutate(target = ifelse(Parameter=="AUC24",targetAUC24,targetCmax)) 

TAplot<-ggplot(TAplotdat,aes(x = value, fill=Parameter)) + 
  labs(x="Parameter", y="Count",title="Histograms for AUC24 and Cmax Target Attainment") +
  geom_histogram(col="black") +                 #create the histogram
  geom_vline(aes(xintercept = target), lty=2, lwd=1) + #add a vertical line with our target
  facet_wrap(vars(Parameter),scales="free")         #wrap by Parameter (AUC and Cmax)


#we can use tag_facet from the egg package to add some custom text to the plots  

TAplot<-tag_facet(TAplot, x=Inf,y=Inf,                        
          open = "", 
          close = "",
          tag_pool=c(paste("%OverTarget = ",round(overtargetAUC24,0)),   #this uses the % values we created earlier
                     paste("%OverTarget = ",round(overtargetCmax))),
          hjust = 1.1,
          vjust = 1.5) +
  theme(strip.text = element_text(),strip.background = element_rect())

saveRDS(TAplot,file="TAplot.RDS")

# * * TA Facet by WT ----
## Facet by WT Categories <50kg, 50-100kg, >100kg

# First create a WT cut variable
TAplotdat <- TAplotdat %>% 
  mutate(WTcut = cut(WT,breaks=c(30,75,115,155)))

#calculate attainment by facets
TAsummary <- TAplotdat %>% 
  group_by(Parameter,WTcut) %>% 
  summarize(pctovr = round(length(which(value > target)) / length(value) * 100,0))
#plot
TAplotbyWT<-ggplot(TAplotdat,aes(x = value, fill=Parameter)) + 
  labs(x="Parameter", y="Count",title="Histograms for AUC24 and Cmax Target Attainment by WT Cuts") +
  geom_histogram(col="black") +                 
  geom_vline(aes(xintercept = target), lty=2, lwd=1) + 
  facet_wrap(Parameter ~ WTcut, scales="free")  

#we can use tag_facet from the egg package to add some custom text to the plots  

TAplotbyWT<-tag_facet(TAplotbyWT, x=Inf, y=Inf, open = "", close = "",
                      tag_pool=c(paste("%OverTarget = ",TAsummary$pctovr[1]),
                                 paste("%OverTarget = ",TAsummary$pctovr[2]),
                                 paste("%OverTarget = ",TAsummary$pctovr[3]),
                                 paste("%OverTarget = ",TAsummary$pctovr[4]),
                                 paste("%OverTarget = ",TAsummary$pctovr[5]),
                                 paste("%OverTarget = ",TAsummary$pctovr[6])),
                      hjust = 1.1,
                      vjust = 1.5,
                      size=3) +
  theme(strip.text = element_text(),strip.background = element_rect())

saveRDS(TAplotbyWT,file="TAplotbyWT.RDS")

