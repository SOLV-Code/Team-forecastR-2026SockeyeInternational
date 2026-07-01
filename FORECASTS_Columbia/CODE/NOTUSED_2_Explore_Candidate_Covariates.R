# SCRIPT TO PROCESS AND REORGANIZE THE STOCK DATA

# install/load required packages

if(!"tidyverse"%in%installed.packages()[,"Package"]){install.packages("tidyverse")}
library(tidyverse)


var.list <- c("CTC_PDO_SummerMean","CTC_NPGO_SummerMean","MEIv2MeanJunToAug", "ONIAvgJanToJun", "Pacea_NPI_Anomaly")

psec.src <- read_csv("https://raw.githubusercontent.com/SOLV-Code/Open-Source-Env-Cov-PacSalmon/refs/heads/main/OUTPUT/Merged_CovariateSet.csv",
                     comment="#") %>% select(Year, all_of(var.list)) %>% dplyr::filter(Year>=1970)

head(psec.src)


lims.use <- range(psec.src %>% select(-Year),na.rm=TRUE)
lims.use

pairs.src <- as.data.frame(t(combn(var.list,2 )))
pairs.src 


# color fade
n.yrs <- dim(psec.src)[1]
n.yrs
n.fade <- 30
bg.fade <- c(rep(0,n.yrs-n.fade),seq(0.025,1, length.out=n.fade))
bg.col <- rgb(1, 0, 0,bg.fade)
bg.col <- tail(bg.col,n.yrs)


trim2000.idx <- psec.src$Year >= 2000


if(!dir.exists("OUTPUT/1_Exploratory_Data_Analyses/EnvCovarPreScreen")){
  dir.create("OUTPUT/1_Exploratory_Data_Analyses/EnvCovarPreScreen")}



png(filename = paste0("OUTPUT/1_Exploratory_Data_Analyses/EnvCovarPreScreen/Diagnostics_EnvCov_TimeSeries.png"),
    width = 480*5, height = 480*7.5, units = "px",
    pointsize = 14*3.6,
    bg = "white",  res = NA)


par(mfrow = c(5,2),
    mai=c(2.2,2.2,2,1))  


for(var.plot in var.list) {

print(var.plot)

lagged.var <- left_join(psec.src %>% select(all_of(c("Year",var.plot))) %>% rename(Orig = 2),
                          psec.src %>% select(all_of(c("Year",var.plot))) %>% mutate(Year = Year-1) %>% rename(Lagged = 2),
                          by="Year") %>% mutate(Lag1Diff = Lagged-Orig)
print(lagged.var)


plot(psec.src$Year,psec.src[,var.plot] %>% unlist(), bty="n", las=1,
     pch=21,col="darkblue",bg=bg.col,cex=1.4,type="o",main = var.plot,
     xlab="Year", ylab = var.plot)
abline(h=0,col="red",lwd=2)

plot(lagged.var$Year,lagged.var$Lag1Diff, bty="n", las=1,
     pch=21,col="darkblue",bg=bg.col,cex=1.4,type="o", 
     main = paste0("Lagged difference in ",var.plot) ,
     xlab="Year", ylab = paste0("Lagged Diff ",var.plot))
abline(h=0,col="red",lwd=2)

}



dev.off()

#####################################
# pairwise scatterplot


for(i in 1:dim(pairs.src )[1]){


  
  png(filename = paste0("OUTPUT/1_Exploratory_Data_Analyses/EnvCovarPreScreen/Diagnostics_EnvCov_PairwiseComparison_",i,".png"),
      width = 480*5, height = 480*4.5, units = "px",
      pointsize = 14*3.6,
      bg = "white",  res = NA)


par(mfrow = c(2,2),
    mai=c(2.7,2.7,2.2,1))  

  
  
var.plot.1 <- pairs.src[i,1]
var.plot.2 <- pairs.src[i,2]

var.plot.1
var.plot.2



lagged.var.1 <- left_join(psec.src %>% select(all_of(c("Year",var.plot.1))) %>% rename(Orig = 2),
                          psec.src %>% select(all_of(c("Year",var.plot.1))) %>% mutate(Year = Year-1) %>% rename(Lagged = 2),
                          by="Year") %>% mutate(Lag1Diff = Lagged-Orig)
lagged.var.1

lagged.var.2 <- left_join(psec.src %>% select(all_of(c("Year",var.plot.2))) %>% rename(Orig = 2),
                          psec.src %>% select(all_of(c("Year",var.plot.2))) %>% mutate(Year = Year-1) %>% rename(Lagged = 2),
                          by="Year") %>% mutate(Lag1Diff = Lagged-Orig)
lagged.var.2




plot(psec.src[,var.plot.1]  %>% unlist(), psec.src[,var.plot.2] %>% unlist(),
     pch=21,col="darkblue",bg=bg.col,cex=1.4, bty="n",las=1, 
     xlab= var.plot.1 , ylab=var.plot.2)
title(main =  "Annual Values - All Years",line=1)

plot(psec.src[trim2000.idx,var.plot.1]  %>% unlist(), psec.src[trim2000.idx,var.plot.2] %>% unlist(),
     pch=21,col="darkblue",bg=bg.col[trim2000.idx],cex=1.4, bty="n",las=1, 
     xlab= var.plot.1 , ylab=var.plot.2)
title(main = "Annual Values - 2000+",line=1)


plot(lagged.var.1$Lag1Diff, lagged.var.2$Lag1Diff,
     pch=21,col="darkblue",bg=bg.col,cex=1.4, bty="n",las=1, 
     xlab= var.plot.1 , ylab=var.plot.2)
title(main ="Lagged Differences - All Years",line=1)


plot(lagged.var.1$Lag1Diff[trim2000.idx], lagged.var.2$Lag1Diff[trim2000.idx],
     pch=21,col="darkblue",bg=bg.col[trim2000.idx],cex=1.4, bty="n",las=1, 
     xlab= var.plot.1 , ylab=var.plot.2)
title(main = "Lagged Differences - 2000+",line=1)



title( main = paste(var.plot.2 ,"vs.", var.plot.1),outer=TRUE,line=-1)

dev.off()


} # end looping through pairs




##############################################################################
#  Plot candidate env covars vs. brood year age class ratio

# start df for storing lagged values
covar.out <- psec.src %>% select(Year) %>% mutate(BroodYear = Year-2) # Because younger age in sib reg is either 4_2 or 5_2!


# Kvichak 5_3 vs. 4_2

kvichak.ratios <- left_join(
pooled.data.long %>% ungroup() %>% dplyr::filter(River=="Kvichak",Age==5) %>% select(BroodYear,ReturnsByAge) %>%
      dplyr::rename(Age5 = ReturnsByAge),
pooled.data.long %>% ungroup() %>% dplyr::filter(River=="Kvichak",Age==4) %>% select(BroodYear,ReturnsByAge) %>%
  dplyr::rename(Age4 = ReturnsByAge),
  by = "BroodYear") %>% mutate(KvichakRatio = Age5/Age4)


# Naknek 6_3 vs. 5_2

naknek.ratios <- left_join(
  pooled.data.long %>% ungroup() %>% dplyr::filter(River=="Naknek",Age==6) %>% select(BroodYear,ReturnsByAge) %>%
    dplyr::rename(Age6 = ReturnsByAge),
  pooled.data.long %>% ungroup() %>% dplyr::filter(River=="Naknek",Age==5) %>% select(BroodYear,ReturnsByAge) %>%
    dplyr::rename(Age5 = ReturnsByAge),
  by = "BroodYear") %>% mutate(NaknekRatio = Age6/Age5)



for(var.plot in var.list) {
  
  print(var.plot)
  
  
  png(filename = paste0("OUTPUT/1_Exploratory_Data_Analyses/EnvCovarPreScreen/Diagnostics_EnvCov_CompareToAgeRatio_",
                        var.plot,".png"),
      width = 480*5, height = 480*4.5, units = "px",
      pointsize = 14*3.3,
      bg = "white",  res = NA)
  
  
  par(mfrow = c(2,2),
      mai=c(2.7,2.7,2.2,1))  
  
  
  
  
  lagged.var <- left_join(psec.src %>% select(all_of(c("Year",var.plot))) %>% rename(Orig = 2),
                          psec.src %>% select(all_of(c("Year",var.plot))) %>% mutate(Year = Year-1) %>% rename(Lagged = 2),
                          by="Year") %>% mutate(Lag1Diff = Lagged-Orig) %>% mutate(BroodYear = Year -2) %>% # Because younger age in sib reg is either 4_2 or 5_2!
                        left_join(kvichak.ratios %>% select(-Age5, -Age4),by="BroodYear") %>%
                        left_join(naknek.ratios %>% select(-Age6, -Age5),by="BroodYear")
  
  lagged.var  

  
  covar.out <- left_join(covar.out, lagged.var %>% select(Year,Lag1Diff) %>% dplyr::rename(!!var.plot := Lag1Diff),
                         by = "Year")
  
  
x.lim <- range(lagged.var$Lag1Diff,na.rm = TRUE)  
  

tmp.2000.idx <-lagged.var  >= 2000  
  
plot(lagged.var$Lag1Diff,lagged.var$KvichakRatio,ylim=c(0,8),xlim=x.lim, xlab="Lagged Difference in Env. Covar", ylab= "Kvichak Age5/Age4",bty="n",las=1,
     main="Kvichak - All Years" ,pch=21,col="darkblue",bg=bg.col,cex=1.4)
abline(v=0,col="red",lwd=2)

plot(lagged.var$Lag1Diff[tmp.2000.idx],lagged.var$KvichakRatio[tmp.2000.idx],ylim=c(0,8),xlim=x.lim, xlab="Lagged Difference in Env. Covar", 
     ylab= "Kvichak Age5/Age4",bty="n",las=1,
     main="Kvichak - Starting 2000",pch=21,col="darkblue",bg=bg.col[trim2000.idx],cex=1.4)
abline(v=0,col="red",lwd=2)

plot(lagged.var$Lag1Diff,lagged.var$NaknekRatio,ylim=c(0,2),xlim=x.lim, xlab="Lagged Difference in Env. Covar", ylab= "Naknek Age6/Age5",bty="n",las=1,
     main="Naknek - All Years", pch=21,col="darkblue",bg=bg.col,cex=1.4)
abline(v=0,col="red",lwd=2)
plot(lagged.var$Lag1Diff[tmp.2000.idx],lagged.var$NaknekRatio[tmp.2000.idx],ylim=c(0,2),xlim=x.lim, xlab="Lagged Difference in Env. Covar", 
     ylab= "Naknek Age6/Age5",bty="n",las=1,
     main="Naknek - Starting 2000",pch=21,col="darkblue",bg=bg.col[trim2000.idx],cex=1.4)
abline(v=0,col="red",lwd=2)


title( main = var.plot,outer=TRUE,line=-1)



dev.off()
  
  
}


#############
# Write data file


covar.out

# as per https://stackoverflow.com/questions/72304594/added-commented-section-to-output-csv-with-write-csv
covar.out.filename <- "DATA/3_ProcessedData/GENERATED_EnvCovarSource_LaggedDifferences.csv"

comment.text1 <- paste("# CANDIDATE ENVIRONMENTAL COVARIATES")
comment.text2 <- paste("# Values are lagged differences")
comment.text3 <- paste("# Matched up to brood years for sibling regressions where younger cohort is X_2 and older cohort is X_3 (i.e. different ocean entry year)")
comment.text4 <- paste("# Generated by this script: https://github.com/SOLV-Code/Team-forecastR-2025SockeyeInternational/blob/main/CODE/2_Explore_Candidate_Covariates.R")
comment.text5 <- paste("# For rationale and variable details go to: https://github.com/SOLV-Code/Team-forecastR-2025SockeyeInternational/tree/main/NOTES/4_PreScreening_EnvCov")


write_lines(comment.text1, covar.out.filename)
write_lines(comment.text2, covar.out.filename, append = TRUE)
write_lines(comment.text3, covar.out.filename, append = TRUE)
write_lines(comment.text4, covar.out.filename, append = TRUE)
write_lines(comment.text5, covar.out.filename, append = TRUE)

covar.out |> colnames() |> paste0(collapse = ",") |> write_lines(covar.out.filename, append = TRUE)
write_csv(covar.out, covar.out.filename , append = TRUE)




#####################################################################
# GENERATE FORECASTR INPUT FILES (from the pooled data) WITH COVARIATES
#####################################################################


stk.list.covar <- c("Kvichak","Naknek")

covar.add <- covar.out %>% select(-Year) %>% 
  dplyr::rename(Brood_Year = BroodYear,
     Cov_PDOSummer =  CTC_PDO_SummerMean,
     Cov_NPGOSummer = CTC_NPGO_SummerMean,
     Cov_MEISummer = MEIv2MeanJunToAug, 
     Cov_ONIJanJun = ONIAvgJanToJun,
     Cov_NPIAnnual = Pacea_NPI_Anomaly)


covar.add

for(stk.do in stk.list.covar){
  
  
  print(stk.do)
  
  for(fc.yr.do in 2020:2025){
    
    print(fc.yr.do)
    
    pooled.data.sub <- pooled.data.long %>% ungroup() %>% 
      dplyr::filter(River == stk.do) %>%
      dplyr::rename(Run_Year = ReturnYear,
                    Brood_Year = BroodYear,
                    Ocean_Entry_Year = MarineEntryYear,
                    Age_Class = Age,
                    Stock_Name = River,
                    Average_Run = ReturnsByAge) %>%
      mutate(Stock_Species = "Sockeye", Stock_Abundance = "Run",
             Forecasting_Year = fc.yr.do) %>% 
      dplyr::filter(Run_Year < fc.yr.do+2) %>%
      select(Stock_Name,Stock_Species, Stock_Abundance,
             Forecasting_Year,Run_Year,Brood_Year,Age_Class,
             Average_Run) %>%
      mutate(Average_Run = round(Average_Run)) %>% # round to nearest integer. Some have small fractions of a fish?
      mutate(Average_Run = case_match(Average_Run,0 ~ 1,.default = Average_Run)) %>% # change any  0 to 1 fish for log power model
     left_join(covar.add,by="Brood_Year")
    
    # need to add more brood years with covar values to get forecast
    #if(stk.do == "Naknek"){
      
      
    add.df <- pooled.data.sub %>% dplyr::filter(Run_Year %in% c(fc.yr.do-c(1:2))) %>% select(Run_Year,Brood_Year,Age_Class) %>%
      mutate(Run_Year=Run_Year+2,Brood_Year=Brood_Year+2 ) %>% left_join(covar.add,by="Brood_Year")
      
    pooled.data.sub <- pooled.data.sub %>% bind_rows(add.df)
      
    #}
    
    
    
    
    
    
    
    num.rows <- dim(pooled.data.sub)[1]
    
    pooled.data.sub$Stock_Name[2:num.rows] <- NA
    pooled.data.sub$Stock_Species[2:num.rows] <- NA
    pooled.data.sub$Stock_Abundance[2:num.rows] <- NA
    pooled.data.sub$Forecasting_Year[2:num.rows] <- NA
    
    # fix so that these not-yet-available values (depending on fc year) are NA instead of 0 or values
    pooled.data.sub[pooled.data.sub$Run_Year >= fc.yr.do, "Average_Run"] <- NA
    
    
    specs.vec <- pooled.data.sub[1,1:4]
    
    
    
    head(pooled.data.sub)
    
    if(!dir.exists("DATA/3_ProcessedData/ForecastR_InputFiles_Covar")){dir.create("DATA/3_ProcessedData/ForecastR_InputFiles_Covar")}
   
    
   long.df <- pooled.data.sub %>% dplyr::filter(Brood_Year >= 1968) %>% select(-Cov_MEISummer,-Cov_ONIJanJun)
   long.df[1,1:4] <- specs.vec
   
   mid.df <- pooled.data.sub %>% dplyr::filter(Brood_Year >= 1977) %>% select(-Cov_ONIJanJun)
   mid.df[1,1:4] <- specs.vec
   
   short.df <-  pooled.data.sub %>% dplyr::filter(Brood_Year >= 1996) 
   short.df[1,1:4] <- specs.vec 
   
    write_csv(long.df,
              paste0("DATA/3_ProcessedData/ForecastR_InputFiles_Covar/ForecastR_Input_Covar_",
                     stk.do,"_ForecastYear", fc.yr.do,"StartBY1968.csv")     )
    
    write_csv(mid.df,
              paste0("DATA/3_ProcessedData/ForecastR_InputFiles_Covar/ForecastR_Input_Covar_",
                     stk.do,"_ForecastYear", fc.yr.do,"StartBY1977.csv")     )   
    
    write_csv(short.df,
              paste0("DATA/3_ProcessedData/ForecastR_InputFiles_Covar/ForecastR_Input_Covar_",
                     stk.do,"_ForecastYear", fc.yr.do,"StartBY1996.csv")     )     
    
    
  }} # end looping through stocks and years


