# SCRIPT TO RUN ALTERNATIVE FORECASTS USING THE FORECASTR PACKAGE

# NOTE: LOOPING THROUGH ALL THE ALTERNATIVE FORECASTS TAKE A WHILE.
# ONLY RUN THE WHOLE SCRIPT IF WANT TO REDO EVERYTHING FROM SCRATCH!

# install/load required packages

library(devtools) # Load the devtools package.
install_github("SOLV-Code/forecastR_package")

library(forecastR)

library(tidyverse)


################################################################################
# WORKED EXAMPLES ILLUSTRATING KEY FUNCTIONS
################################################################################

# For details, check this wiki page from the releases repo:
# https://github.com/SalmonForecastR/ForecastR-Releases/wiki/4-Using-the-ForecastR-package


#  load and prepare a sample input file


data.raw <- read.csv("DATA/3_ProcessedData/ForecastR_InputFiles/ForecastR_Input_Bonneville Lock & Dam_ForecastYear2025.csv") 
data.prepped <- prepData(data.raw ,out.labels="v2") # this pre-processes the inut file
names(data.prepped)

# check the components
data.prepped$data$`Age 4`
data.prepped$data$`Age 5`
data.prepped$output.pre
data.prepped$specs
data.prepped$sibreg.in

# fit a candidate model, plot the model fit, and calculate the 2025 forecast for the model

sample.fit.arima <- fitModel(model= "TimeSeriesArima", data = data.prepped$data, settings = 
                                list(BoxCox=FALSE),tracing=TRUE)

plotModelFit(sample.fit.arima,
             options = list(plot.which = "resid_ts", age.which = "Age 4", plot.add = FALSE))


sample.fc.out <- calcFC(fit.obj= sample.fit.arima,data = data.prepped$data, fc.yr= 2025,  
                   		 settings = list(BoxCox=FALSE), tracing=TRUE)	# IMPORTANT THAT SETTINGS ARE THE SAME!

print(sample.fc.out)

# run multiple candidate models and rank them based on retrospective performance
# multiFC() 	apply fitModel() and calcFC() using a list of alternative models
# NOTE: multiFC usesd the raw data (calls prepData fn inside)
# rankModels() 	do model ranking using the output of multiFC()



settings.use <- list( 			Naive4 = list(model.type="Naive",settings=list(avg.yrs=4)),
                     				Naive8 = list(model.type="Naive",settings=list(avg.yrs=8)),
                     				SibRegSimple = list(model.type="SibRegSimple",settings=NULL),
                     				SibRegLogPower =  list(model.type="SibRegLogPower",settings=NULL),
                     				SibRegKalman =  list(model.type="SibRegKalman",settings=NULL),
                     				TimeSeriesArimaBC = list(model.type="TimeSeriesArima",settings=list(BoxCox=TRUE)),
                     				TimeSeriesArimaNoBC = list(model.type="TimeSeriesArima",settings=list(BoxCox=FALSE)),
                     				TimeSeriesExpSmoothBC = list(model.type="TimeSeriesExpSmooth",settings=list(BoxCox=TRUE)),
                     				TimeSeriesExpSmoothNoBC = list(model.type="TimeSeriesExpSmooth",settings=list(BoxCox=FALSE))
                     				)

multiresults.retro <- multiFC(data.file=data.raw,
                              settings.list=settings.use,
                            	do.retro=TRUE,
                            	retro.min.yrs=15,
                              out.type="short",
                              int.type = "Retrospective", # generate forecast intervals based on retrospective residuals
                              int.n = 500,
                              boot.type = "meboot",
                              tracing=FALSE)

names(multiresults.retro)

# check the output
multiresults.retro$table.ptfc
multiresults.retro$retro.pm$retro.pm.bal[,,'Age 4']

# do the ranking
ranking.pm.use <- c("MRE", "MAE", "MPE", "MAPE", "MASE", "RMSE")

ranking.out <- rankModels(dat = multiresults.retro$retro.pm$retro.pm.bal,
                          columnToRank=ranking.pm.use, relative.bol=TRUE) # TRUE means scaled ranking
ranking.out$bestmodel
ranking.out$`Age 4`


################################################################################
# APPLY FULL SET OF 9 CANDIDATE MODELS TO ALL 14 STOCKS FOR FC YEARS 2020-2025

# TO DO
# - loop through full data set vs. trimmed ~ 2000


start.time <-  proc.time()

# get a list of all alternative input files
files.list <- list.files("DATA/3_ProcessedData/ForecastR_InputFiles")
files.list

files.paths <- list.files("DATA/3_ProcessedData/ForecastR_InputFiles",full.names = TRUE)
files.paths

# if want to keep a log for debugging, change to TRUE
do.log <- FALSE
if(do.log){sink("OUTPUT/ModelFitting_Log.txt")} # open the log file


if(exists("results.store")){rm(results.store)}

for(i in 1:length(files.list)){
  
file.name <- files.list[i]  
stk.label  <- str_split(file.name,pattern="_")[[1]][3]
fc.yr <- as.numeric(gsub(".csv","",gsub("ForecastYear","",str_split(file.name,pattern="_")[[1]][4])))
  
print("#########################################################")
print(file.name)
print(stk.label)
print(fc.yr)

print("#########################################################")

data.raw <- read.csv(files.paths[i])  
  
multiresults.retro <- multiFC(data.file=data.raw,
                              settings.list=settings.use,
                              do.retro=TRUE,
                              retro.min.yrs=15,
                              out.type="short",
                              int.type = "Retrospective", # generate forecast intervals based on retrospective residuals
                              int.n = 500,
                              boot.type = "meboot",
                              tracing=FALSE)

ranking.pm.use <- c("MRE", "MAE", "MPE", "MAPE", "MASE", "RMSE")

ranking.out <- rankModels(dat = multiresults.retro$retro.pm$retro.pm.bal,
                          columnToRank=ranking.pm.use, relative.bol=TRUE) # TRUE means scaled ranking




# extract key outputs for storage
multiresults.retro$table.ptfc
multiresults.retro$retro.pm$retro.pm.bal[,,'Age 4']
ranking.out$bestmodel
ranking.out$`Age 4`  
multiresults.retro$int.array  
  


  results.out <- multiresults.retro$table.ptfc %>% rownames_to_column("ModelLabel") %>%
    pivot_longer(cols = c(starts_with("Age"),"Total"),names_to = "Age",values_to = "PointFC") %>%
    mutate(Stock = stk.label,FC_Year = fc.yr) %>% select(Stock,FC_Year,everything())


if(exists("ranking.tmp")){rm(ranking.tmp)}

for(age.do in c("Age 3", "Age 4","Age 5","Age 6","Total")){

if(age.do %in% names(ranking.out)){
  
print(age.do)  
if(exists("ranking.tmp")){  
  ranking.tmp <- bind_rows(ranking.tmp,
            ranking.out[[age.do]] %>% select(rank.avg) %>% rownames_to_column("ModelLabel") %>% mutate(Age = age.do)
            )}  
if(!exists("ranking.tmp")){
ranking.tmp <- ranking.out[[age.do]] %>% select(rank.avg) %>% rownames_to_column("ModelLabel") %>% mutate(Age = age.do) }

} # end if age   
  
  
} # end looping through ages



results.out <- results.out %>% left_join(ranking.tmp %>% dplyr::rename(AvgRankByAge = rank.avg),
                                         by=c("ModelLabel","Age")) %>%  # add ranking info
 left_join(as.data.frame.table(multiresults.retro$int.array) %>% arrange(Var1) %>% 
  dplyr::rename(ModelLabel = Var1, Age = Var3) %>%
  pivot_wider(names_from = Var2, values_from = Freq),
  by=c("ModelLabel","Age"))  # add intervals

if(exists("results.store")){results.store <- bind_rows(results.store,results.out)}  
if(!exists("results.store")){results.store <- results.out }


} # end looping through input files

if(do.log){sink()} # close the log file


dim(results.store)

proc.time() - start.time




#############################################################################
# SUMMARIZE AND STORE THE RESULTS



results.store <- results.store %>% left_join(stk.lookup%>% dplyr::rename(Stock=River),by="Stock") %>% select(Stock,everything())

write_csv(results.store %>%  mutate(across(c(PointFC,starts_with("p")),\(x) round(x, 0))),
          "OUTPUT/FullOutputs_ForecastsAndRanks.csv")




results.top1 <- results.store %>% group_by(Stock,Age,FC_Year) %>% slice_min(AvgRankByAge,n=1) %>%
        arrange(System,Stock,Age,FC_Year,AvgRankByAge) %>% select(System,Stock,Age,FC_Year,AvgRankByAge,ModelLabel,everything()) %>%
        mutate(across(c(PointFC,starts_with("p")),\(x) round(x, 0)))


write_csv(results.top1,"OUTPUT/Top1_ForecastsAndRanks.csv")




results.top3 <- results.store %>% group_by(Stock,Age,FC_Year) %>% slice_min(AvgRankByAge,n=3) %>%
  arrange(System,Stock,Age,FC_Year,AvgRankByAge) %>% select(System,Stock,Age,FC_Year,AvgRankByAge,ModelLabel,everything()) %>%
  mutate(across(c(PointFC,starts_with("p")),\(x) round(x, 0)))

write_csv(results.top3,"OUTPUT/Top3_ForecastsAndRanks.csv")



results.topmodels.byage <- results.top1 %>% ungroup() %>% group_by(System,Stock,Age)%>% summarise(NumModels = length(unique(ModelLabel))) %>%
  left_join(results.top1 %>% ungroup() %>% group_by(System,Stock,Age)%>% 
              summarise(ModelLabel = paste(unique(ModelLabel), collapse = ' ')),
      by=c("System","Stock","Age")
)

write_csv(results.topmodels.byage ,"OUTPUT/TopModels_ListByAge.csv")



###########################################################
# STEP 7:  Reorg the selected model list and extract forecasts
# source file manually generated from results.topmodels.byage
# Details in Step 7: https://github.com/SOLV-Code/Team-forecastR-2025SockeyeInternational/tree/main/NOTES/7_Select_2025FC


selected.models <- read_csv("DATA/2_Lookup_Files/MANUAL_UPDATES_ModelSelection.csv")
selected.models 



selection.table.src <- selected.models %>% mutate(Selection = paste0(ModelLabel,": ",Notes)) %>%
  select(-ModelLabel,-Notes) %>% 
  pivot_wider(id_cols = c(System, Stock),names_from = Age, values_from = Selection)


write_csv(selection.table.src ,"OUTPUT/ModelSelection_Table.csv")


# extract corresponding forecasts

fc.details <- bind_rows(selected.models   %>% mutate(FC_Year = 2020),
                        selected.models   %>% mutate(FC_Year = 2021),
                        selected.models   %>% mutate(FC_Year = 2022),
                        selected.models   %>% mutate(FC_Year = 2023),
                        selected.models   %>% mutate(FC_Year = 2024),
                        selected.models   %>% mutate(FC_Year = 2025)
                        )




fc.details <- fc.details  %>% left_join(results.store,
                        by=c("System","Stock","Age","FC_Year","ModelLabel") )

write_csv(fc.details ,"OUTPUT/Forecast_Details.csv")


fc.SumOfMedians <- fc.details %>% group_by(System,Stock,FC_Year) %>% summarize(SumOfMedians=sum(p50)) %>%
  pivot_wider(id_cols=c(System,Stock),names_from = FC_Year,values_from = SumOfMedians)

write_csv(fc.SumOfMedians ,"OUTPUT/Forecast_Totals_ByYear_SumOfMedians.csv")




fc.SumOfPtFC <- fc.details %>% group_by(System,Stock,FC_Year) %>% summarize(SumOfPtFC=sum(PointFC)) %>%
  pivot_wider(id_cols=c(System,Stock),names_from = FC_Year,values_from = SumOfPtFC)

write_csv(fc.SumOfPtFC ,"OUTPUT/Forecast_Totals_ByYear_SumOfPtFC.csv")


##############################################################
# RETROSPECTIVE PLOTS

library(tidyverse)

# check object from script 1
names(full.data.long.df)



if(!dir.exists("OUTPUT/Retrospective_Diagnostics")){dir.create("OUTPUT/Retrospective_Diagnostics")}


png(filename = "OUTPUT/Retrospective_Diagnostics/AllStocks_Obs_vs_FC.PNG",
    width = 480*9, height = 480*7, units = "px",
    pointsize = 14*4.3,
    bg = "white",  res = NA)


par(mfrow = c(4,4),
    mai=c(2.8,2.8,2.5,1))  




for(stk.do in stk.list.all){


system.label <-  stk.lookup %>% dplyr::filter(River == stk.do)  %>% select(System) %>% unlist()
    
  

obs.ret.stk <-   full.data.long.df %>% dplyr::filter(River == stk.do, ReturnYear >=2010) %>%
                  select(ReturnYear, Total_Returns) %>% unique()

fc.stk <- fc.SumOfMedians %>% dplyr::filter(Stock == stk.do) %>%
                    pivot_longer(3:8,names_to = "ReturnYear", values_to = "SumOfMedians") %>%
          left_join(fc.SumOfPtFC %>% dplyr::filter(Stock == stk.do) %>%
                      pivot_longer(3:8,names_to = "ReturnYear", values_to = "SumOfPointFC"),
                    by = c("System","Stock","ReturnYear") )


y.max <- max(obs.ret.stk$Total_Returns,fc.stk$SumOfMedians,fc.stk$SumOfPointFC)
y.max

if(y.max >= 10^6){scalar.use <- 10^6 ; scalar.label <-" (Mill)"}
if(y.max < 10^6){scalar.use <- 10^5 ; scalar.label <-" (100k)"}
if(y.max < 10^5){scalar.use <- 10^4 ; scalar.label <-" (10k)"}
if(y.max < 10^4){scalar.use <- 10^3 ; scalar.label <-" (1k)"}
if(y.max < 10^3){scalar.use <- 1 ; scalar.label <-""}



plot(obs.ret.stk$ReturnYear,obs.ret.stk$Total_Returns/scalar.use,type="o",
     axes=FALSE,pch=21,col="darkblue",bg="lightgrey",lwd=3,cex=2,
     xlim=c(2010,2025), ylim = c(0,y.max/scalar.use),
     xlab="Return Year",ylab= paste0("Total Returns",scalar.label),
     main=paste0(stk.do," (",system.label,")") ,xpd=NA)
axis(1)
axis(2,las=1)

rect(2024.5,0,2025.5,y.max/scalar.use,col="azure2",border="azure2")

lines(fc.stk$ReturnYear,fc.stk$SumOfPointFC /scalar.use,type="o",
      col="red",bg="white",lwd=2,pch=21,cex=1.3)

lines(fc.stk$ReturnYear,fc.stk$SumOfMedians/scalar.use,type="o",
      col="red",bg="darkorange",lwd=2,pch=21,cex=1.3)



} # end looping through stocks

plot(1:5,1:5, type="n",axes=FALSE,xlab="",ylab="")

  legend("topleft",legend = c("Observed","Point Forecast","Median of Interval"),
         pch=21, col=c("darkblue","red","red"),
         pt.bg=c("lightblue","white", "darkorange"),
         pt.cex=c(3,2,2),bty="n",cex=2)



dev.off()



