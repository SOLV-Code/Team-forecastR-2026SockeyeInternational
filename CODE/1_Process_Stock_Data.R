# SCRIPT TO PROCESS AND REORGANIZE THE STOCK DATA

# install/load required packages

if(!"tidyverse"%in%installed.packages()[,"Package"]){install.packages("tidyverse")}
library(tidyverse)


# specify competition year so that data fields for later years can be set to NA (vs. 0)
# This is a temporary patch for the pooled data set and needs a more robust solution.
# For competition year 2025, this sets values in return years 2025, 2026, and larger to NA, rather than 
# the 0 you get from the earlier steps.
competition.yr <- 2025



# read in source data files:
brood.df <- read_csv("DATA/1_SourceData/2025_Sk_Intl_Data/Combined_Brood_Bristol_Columbia_Fraser.csv")[-1]
head(brood.df)

entry.df <- read_csv("DATA/1_SourceData/2025_Sk_Intl_Data/Combined_First_Year_At_Sea.csv")[-1]
head(entry.df)

return.df <- read_csv("DATA/1_SourceData/2025_Sk_Intl_Data/Combined_Return_Bristol_Columbia_Fraser.csv")[-1]
head(return.df)

spn.df <- read_csv("DATA/1_SourceData/2025_Sk_Intl_Data/ManuallyExtracted_Spawners_Combined.csv",comment="#")
  head(spn.df)

  
# read_csv runs in to an error with some columns that have a lot of NA at the beginning, 
# read_csv assigns those as a logical variable, then encounters problems with text in later rows
# read.csv is a lot less smart, but doesn't trip over that particular issue
# keeping only the unique brood year EFS records
efs.src <- read.csv("DATA/1_SourceData/2025_Sk_Intl_Data/Production Data_Detailed Format_5_Summer_Stocks.csv") %>%
            select(production_stock_name,broodyr,total_broodyr_spawners,total_broodyr_EFS) %>%
            unique() %>% mutate(PercEFS = round(total_broodyr_EFS/total_broodyr_spawners*100,1) ) %>%
            dplyr::rename(Total_Spawners_BroodYear = total_broodyr_spawners,
                          Total_EFS_BroodYear = total_broodyr_EFS,
                          BroodYear = broodyr,
                          River = production_stock_name
                          )
            
  
head(efs.src)        
write_csv(efs.src, "DATA/3_ProcessedData/Generated_FRASER_EffectiveFemalesPawners_Summary.csv")



if(!dir.exists("OUTPUT")){dir.create("OUTPUT")}


age.classes.src <- data.frame(
  FW_Winters = str_split_i(gsub("AgeClass_","",names(brood.df)[-c(1,2,3,4)]),"\\.",1) %>% as.numeric(),
  Mar_Winters = str_split_i(gsub("AgeClass_","",names(brood.df)[-c(1,2,3,4)]),"\\.",2) %>% as.numeric() ) %>%
  mutate(Age = FW_Winters + Mar_Winters + 1) %>%
  mutate(Euro = paste0(FW_Winters,".",Mar_Winters),
         GR = paste0(Age,"_",FW_Winters+1)) %>%
  mutate(Label = paste0("AgeClass_",Euro)) %>%
  select(Age, everything())

age.classes.src

write_csv(age.classes.src,"DATA/2_Lookup_Files/GENERATED_AgeClass_Lookup.csv")


stk.lookup <-brood.df %>% select(System, River) %>% unique()
write_csv(stk.lookup,
          "DATA/2_Lookup_Files/GENERATED_Stock_Lookup.csv")



# revised to start from return file, because 2 issues with current brood year file:
# - 2024 Bristol Bay returns not showing up 
# - total recruits calc in source file seems to require all age classes up to age 8 or it is NA 
#      -> change to using only ages 3-6 to get some extra brood year recruit numbers
#      -> last brood year for Bristol Bay stocks gets a weird age comp
#      -> should tweak to do stock-specific age cutoffs

full.data.long.df <- return.df %>% pivot_longer(starts_with("AgeClass_"),names_to = "Label",values_to = "ReturnsByAgeClass") %>% 
  left_join(age.classes.src,by="Label") %>%
  mutate(BroodYear = ReturnYear - Age, RecruitsByAgeClass = ReturnsByAgeClass) %>%
    mutate(MarineEntryYear = BroodYear + FW_Winters + 1 )

total.rec.calc <- full.data.long.df  %>% 
  dplyr::filter(Age %in% 3:6) %>%
  group_by(System,River,BroodYear) %>% 
  summarize(Total_Recruits = sum(RecruitsByAgeClass)) %>% ungroup()

full.data.long.df <- full.data.long.df %>% left_join(total.rec.calc,by = c("System","River","BroodYear") ) %>% 
      mutate(PercOfBrdYr = round(RecruitsByAgeClass/Total_Recruits*100,2) )  %>%
      arrange(System, River, BroodYear, Age, Label) %>%
      left_join(spn.df %>% pivot_longer(2:dim(spn.df)[2],
                      names_to = "River",values_to = "Total_Spawners_BroodYear") ,
                by=c("BroodYear","River")) %>%
      mutate(TotRecPerTotSpn = Total_Recruits/Total_Spawners_BroodYear) %>%
      arrange(System,River,ReturnYear,BroodYear) %>%
      left_join(efs.src %>% select(-Total_Spawners_BroodYear),by=c("BroodYear","River"))


head(full.data.long.df)


# as per https://stackoverflow.com/questions/72304594/added-commented-section-to-output-csv-with-write-csv
full.data.filename <- "DATA/3_ProcessedData/GENERATED_FullDataSet_LongForm.csv"

comment.text1 <- paste("# FULL DATA LONG FORMAT FILE")
comment.text2 <- paste("# Generated from return table")
comment.text3 <- paste("# NOTE: total recruit calculations use ages 3-6. For last brood years of Bristol Bay stocks total recruits and age comp are incorrect. Need to fix/exclude.")
comment.text4 <- paste("# NOTE: Total_Spawners_BroodYear column includes total escapement for Bristol Bay stocks; total spawners for Fraser stocks; and Bonneville counts for the Columbia Aggregate")

write_lines(comment.text1, full.data.filename)
write_lines(comment.text2, full.data.filename, append = TRUE)
write_lines(comment.text3, full.data.filename, append = TRUE)
write_lines(comment.text4, full.data.filename, append = TRUE)

full.data.long.df  |> colnames() |> paste0(collapse = ",") |> write_lines(full.data.filename, append = TRUE)
write_csv(full.data.long.df, full.data.filename , append = TRUE)


# Subset SR data set  by brood year

sr.out <- spn.df %>% pivot_longer(2:dim(spn.df)[2], names_to = "River",values_to = "Total_Spawners_BroodYear") %>%
          left_join(stk.lookup %>% select(River,System), by="River") %>%
          left_join(full.data.long.df %>% select(River,BroodYear, Total_Recruits,TotRecPerTotSpn) %>% unique(),
            by=c("BroodYear","River"))  %>% arrange(System, River, BroodYear)  %>%
          select(System, River, BroodYear, everything())
head(sr.out)


# as per https://stackoverflow.com/questions/72304594/added-commented-section-to-output-csv-with-write-csv
sr.data.filename <- "DATA/3_ProcessedData/GENERATED_SRDataSet_LongForm.csv"

comment.text1 <- paste("# SR DATA LONG FORMAT FILE")
comment.text2 <- paste("# Generated from return table and spawner files")
comment.text3 <- paste("# NOTE: total recruit calculations use ages 3-6. For last brood years of Bristol Bay stocks total recruits and age comp are incorrect. Need to fix/exclude.")
comment.text4 <- paste("# NOTE: Total_Spawners_BroodYear column includes total escapement for Bristol Bay stocks; total spawners for Fraser stocks; and Bonneville counts for the Columbia Aggregate")

write_lines(comment.text1, sr.data.filename)
write_lines(comment.text2, sr.data.filename, append = TRUE)
write_lines(comment.text3, sr.data.filename, append = TRUE)

sr.out |> colnames() |> paste0(collapse = ",") |> write_lines(sr.data.filename, append = TRUE)
write_csv(sr.out, sr.data.filename , append = TRUE)



# get range of % age class by brood year

age.comp.details <- full.data.long.df %>% group_by(System,River,Label) %>% 
      summarize(
        Perc_Min = quantile(PercOfBrdYr,0,na.rm=TRUE),
        Perc_p10 = quantile(PercOfBrdYr,0.10,na.rm=TRUE),
        Perc_p25 = quantile(PercOfBrdYr,0.25,na.rm=TRUE),
        Perc_Med = quantile(PercOfBrdYr,0.5,na.rm=TRUE),
        Perc_p75 = quantile(PercOfBrdYr,0.75,na.rm=TRUE),
        Perc_p90 = quantile(PercOfBrdYr,0.90,na.rm=TRUE),
        Perc_Max = quantile(PercOfBrdYr,1,na.rm=TRUE),
        ) %>% left_join(age.classes.src,by="Label")
head(age.comp.details)
write_csv(age.comp.details,"DATA/3_ProcessedData/GENERATED_AgeComp_Details.csv")


# get top 4 age classes #, filter out any that have median < 2%
age.comp.top4 <- age.comp.details %>% group_by(System, River) %>%
                  arrange(desc(Perc_Med), .by_group = TRUE ) %>%
                  slice_head(n=4) #%>%
                  #ungroup() %>%
                  #dplyr::filter(Perc_Med >=2)
head(age.comp.top4)



# get main age class by stock, then get main cohort for 4 main ages
# main-2, main-1, main, main+1

main.age.lookup <- age.comp.top4 %>% group_by(System, River) %>%
  arrange(desc(Perc_Med), .by_group = TRUE ) %>%
  slice_head(n=1) %>% select(System,River,Label, Perc_Med, Age, Euro,GR) %>%
  dplyr::rename(MainAgeClass = Label)
head(main.age.lookup )


top.cohort.byage.lookup <-  age.comp.details %>% group_by(System, River,Age) %>%
  arrange(desc(Perc_Med), .by_group = TRUE ) %>%
  slice_head(n=1) %>% select(System, River,Age,Label,Euro,GR,Perc_Med)
head(top.cohort.byage)


tmp.src <- main.age.lookup %>% select(System,River, Age)

age.lookup <- tmp.src  %>% bind_rows(tmp.src %>% mutate(Age=Age-2)) %>% 
  bind_rows(tmp.src %>% mutate(Age=Age-1)) %>% 
  bind_rows(tmp.src %>% mutate(Age=Age+1)) %>%
  dplyr::filter(Age>2) %>% arrange(System, River, Age) %>%
  left_join(top.cohort.byage.lookup, by = c("System","River","Age")) %>%
  left_join(age.classes.src %>% select(Label,FW_Winters, Mar_Winters),by="Label") %>%
  mutate(MarineEntryOffset_BrYr = FW_Winters+1,
         MarineEntryOffset_RetYr = -Mar_Winters)
head(age.lookup)

# as per https://stackoverflow.com/questions/72304594/added-commented-section-to-output-csv-with-write-csv
age.lookup.filename <- "DATA/3_ProcessedData/GENERATED_AgeComp_MainAgeClasses.csv"

comment.text1 <- paste("# AGE CLASS LOOKUP FILE")
comment.text2 <- paste("# Generated from return table")
comment.text3 <- paste("# Step 1: Find the age class that accounts for most recruits by brood year (median % over all years)")
comment.text4 <- paste("# Step 2: get main cohort for each age from MainAge-2 to MainAge+1 (excluding any 2-yr olds)")
comment.text5 <- paste("# Step 3: Calculate ocean-entry offset from brood year and from return year for each age based on the dominant age class for that age")

write_lines(comment.text1, age.lookup.filename)
write_lines(comment.text2, age.lookup.filename, append = TRUE)
write_lines(comment.text3, age.lookup.filename, append = TRUE)
write_lines(comment.text4, age.lookup.filename, append = TRUE)
write_lines(comment.text5, age.lookup.filename, append = TRUE)

age.lookup  |> colnames() |> paste0(collapse = ",") |> write_lines(age.lookup.filename, append = TRUE)
write_csv(age.lookup, age.lookup.filename , append = TRUE)




# LONG FORM DATA - POOLED BY AGE
# switched to pool by return year!
pooled.data.long <- full.data.long.df %>% group_by(System,River,ReturnYear,Age) %>%
            summarize(ReturnsByAge = sum(ReturnsByAgeClass,na.rm=TRUE)) %>%
            left_join(age.lookup,by=c("System","River","Age") ) %>%
            dplyr::filter(!is.na(Label)) %>%
            mutate(MarineEntryYear = ReturnYear + MarineEntryOffset_RetYr,
                   BroodYear = ReturnYear - Age) %>%
            select(System,River,BroodYear,Age,MarineEntryYear,ReturnYear,everything()
                   )
             
  
head(pooled.data.long)

# fix so that these not-yet-available values are NA instead of 0
pooled.data.long[pooled.data.long$ReturnYear >= competition.yr, "ReturnsByAge"] <- NA

write_csv(pooled.data.long,"DATA/3_ProcessedData/GENERATED_PooledByAge_LongForm.csv")



#####################################################################
# GENERATE FORECASTR INPUT FILES (from the pooled data)
#####################################################################


stk.list.all <- sort(unique(age.lookup %>% ungroup() %>% 
                              select(River) %>% unlist()))
stk.list.all


for(stk.do in stk.list.all){

  
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
  mutate(Average_Run = case_match(Average_Run,0 ~ 1,.default = Average_Run)) # change any  0 to 1 fish for log power model
  
  # Briefly tested return rate models with TotSpn or EFS as predictors, but crashed in the app.
  # skipped it for this year, but flagged it for closer look next year!
  # left_join(efs.src %>% dplyr::filter(River == stk.do) %>% select(-River, -PercEFS) %>%
  #            dplyr::rename(Brood_Year = BroodYear, Pred_TotalSpn = Total_Spawners_BroodYear,
  #                          Pred_TotalEFS = Total_EFS_BroodYear), by="Brood_Year")
  



num.rows <- dim(pooled.data.sub)[1]


pooled.data.sub$Stock_Name[2:num.rows] <- NA
pooled.data.sub$Stock_Species[2:num.rows] <- NA
pooled.data.sub$Stock_Abundance[2:num.rows] <- NA
pooled.data.sub$Forecasting_Year[2:num.rows] <- NA


# fix so that these not-yet-available values (depending on fc year) are NA instead of 0 or values
pooled.data.sub[pooled.data.sub$Run_Year >= fc.yr.do, "Average_Run"] <- NA





head(pooled.data.sub)

if(!dir.exists("DATA/3_ProcessedData/ForecastR_InputFiles")){dir.create("DATA/3_ProcessedData/ForecastR_InputFiles")}

write_csv(pooled.data.sub,
          paste0("DATA/3_ProcessedData/ForecastR_InputFiles/ForecastR_Input_",
                 stk.do,"_ForecastYear", fc.yr.do,".csv")     )

}} # end looping through stocks and years



#####################################################################
# AGE COMP DIAGNOSTIC PLOTS
#####################################################################

#full.data.long.df
#age.lookup 

if(!dir.exists("OUTPUT/1_Exploratory_Data_Analyses")){dir.create("OUTPUT/1_Exploratory_Data_Analyses")}

brd.yrs.range <- range(full.data.long.df$BroodYear)
brd.yrs.range

system.list <- sort(unique(age.lookup$System))
system.list


for(fig.do in c("BristolBay","Other")){


if(fig.do == "BristolBay"){system.sub.list <- "Bristol Bay"}
if(fig.do == "Other"){system.sub.list <- system.list[system.list!="Bristol Bay"]}

  print("---------------------")
  print(paste("start : ",fig.do))
  
  
png(filename = paste0("OUTPUT/1_Exploratory_Data_Analyses/Diagnostics_AgeComp_",
                        fig.do, ".png"),
      width = 480*6, height = 480*8, units = "px",
      pointsize = 14*4.5,
      bg = "white",  res = NA)
  
  
  par(mfrow = c(4,2),
      mai=c(2.5,2.5,2.5,1))  
  
  
for(system.do in system.sub.list){

  print("---------------------")
  print(paste("start : ",system.do))
  
stk.list <- sort(unique(age.lookup %>% ungroup() %>% dplyr::filter(System == system.do) %>% select(River) %>% unlist()))
stk.list


# get axis limits in 5 year intervals that include all the years with data
year.lims <- c(5*floor(min(brd.yrs.range)/5),5*ceiling(max(brd.yrs.range)/5))
year.lims 


for(stk.do in stk.list){

print("---------------------")
print(paste("start age comp plot for: ",stk.do))

age.lookup.sub <- age.lookup %>% dplyr::filter(River == stk.do) %>%
                    arrange(-Perc_Med)
age.lookup.sub



plot(1:5,1:5,type="n",
     xlim = year.lims ,
     ylim=c(0,100),
     main = paste0(stk.do, " (",unique(age.lookup.sub$System),")\nLargest Age Class = ",
                   age.lookup.sub$Euro[1]," (",age.lookup.sub$GR[1],")"),
     col.main="darkblue",
     bty="n",
     xlab="",ylab="",axes = FALSE
     )
axis(2,las=1)
axis(1,seq(year.lims[1],year.lims[2],by=5))
abline(h=seq(0,100,by=20),col="darkgrey",lty=2)
title(ylab = "% of Brood Year Recruits",line=2.5)
title(xlab = "Brood Year",line=2.5)


for(i in 2:1){
 
cohort.plot <- age.lookup.sub$Label[i]
bg.use <- c("darkblue","white")[i] 
 
data.plot <- full.data.long.df  %>% dplyr::filter(River == stk.do,Label == cohort.plot)

lines(data.plot$BroodYear,
      data.plot$PercOfBrdYr,
      type="o",col="darkblue",pch=21,bg=bg.use)  
}

legend(year.lims[1],115,legend = paste0(age.lookup.sub$Euro[1:2]," (",age.lookup.sub$GR[1:2],")"),
       lty=1,col="darkblue", pch=21,pt.bg = c("darkblue","white"),bty="n",xpd=NA,ncol=2)

}} # end looping through stocks and systems
  

dev.off()  
  
} # end looping through figures
  
  



#####################################################################
# RETURNS BY AGE DIAGNOSTIC PLOTS - Full Panels
#####################################################################

#full.data.long.df
#age.lookup 

if(!dir.exists("OUTPUT/1_Exploratory_Data_Analyses/ReturnsByAge")){dir.create("OUTPUT/1_Exploratory_Data_Analyses/ReturnsByAge")}




for(stk.do in stk.list.all){
  
  
  print(stk.do)

  system.label <- stk.lookup %>% dplyr::filter(River == stk.do) %>% select(System) %>% unique()
  
  png(filename = paste0("OUTPUT/1_Exploratory_Data_Analyses/ReturnsByAge/Diagnostics_ReturnsByAge_",
                        system.label,"_",stk.do, ".png"),
      width = 480*5, height = 480*4, units = "px",
      pointsize = 14*3.3,
      bg = "white",  res = NA)
  
  
  par(mfrow = c(2,2),
      mai=c(2.5,2.5,2.5,1))  
  
  
  
  
  
 stk.ages.lookup <- age.lookup %>% ungroup() %>% dplyr::filter(River == stk.do)
 print(stk.ages.lookup) 
 
  stk.ages.list <- sort(stk.ages.lookup %>% select(Age) %>% unlist())
 print(stk.ages.list)
  
 
 for(age.do in stk.ages.list){
 
 
  pooled.data.sub <- pooled.data.long %>% ungroup() %>% 
    dplyr::filter(River == stk.do, Age == age.do) 
  
  stk.age.lookup.sub <-  stk.ages.lookup %>% dplyr::filter(Age == age.do)
  
  ylim.use <- c(0, max(pooled.data.sub$ReturnsByAge, na.rm=TRUE) )
  
  if(ylim.use[2] >= 10^6){scalar.use <- 10^6 ; scalar.label <-" (Mill)"}
  if(ylim.use[2] < 10^6){scalar.use <- 10^5 ; scalar.label <-" (100k)"}
  if(ylim.use[2] < 10^5){scalar.use <- 10^4 ; scalar.label <-" (10k)"}
  if(ylim.use[2] < 10^4){scalar.use <- 10^3 ; scalar.label <-" (1k)"}
  if(ylim.use[2] < 10^3){scalar.use <- 1 ; scalar.label <-""}
  
  plot(pooled.data.sub$ReturnYear,pooled.data.sub$ReturnsByAge/scalar.use,
       bty="n",
       xlim=c(1945,2025), xlab="Return Year",
       ylim = ylim.use/scalar.use,
       ylab = paste0("Total Age ",age.do," Returns ",scalar.label),
       las=1,pch=19, col="darkblue",type="o",axes=FALSE, 
       col.main="darkblue",cex.main=0.8,
       main =  paste0("Age ",age.do,"\nMostly ",stk.age.lookup.sub$Euro," (",stk.age.lookup.sub$GR,")",
                      "\nBrood Year = ",age.do," years earlier",
                      "\nOcean Entry Year = ",-stk.age.lookup.sub$MarineEntryOffset_RetYr," years earlier")
       )
  axis(1,at = seq(1950,2020,by=10))
  axis(2,las=1)
  
  legend("topleft",
         legend = paste0(age.do,"-yr running avg"),
         lty=1,col="red",bty="n",lwd=4,text.col="red")
  
  
  lines(pooled.data.sub$ReturnYear,
        stats::filter(pooled.data.sub$ReturnsByAge/scalar.use,filter=rep(1/age.do,age.do),side=1),
        col="red",lwd=5)
  
  
  
} # end looping through ages

title(main = paste0(system.label,":\n",stk.do),outer=TRUE,
      col.main="darkblue",line=-2) 
 
dev.off()
 
} # end looping through stocks


#####################################################################
# SIBLING REGRESSION PRE-SCREEN
#####################################################################

if(!dir.exists("OUTPUT/1_Exploratory_Data_Analyses/SibRegPreScreen")){dir.create("OUTPUT/1_Exploratory_Data_Analyses/SibRegPreScreen")}



for(stk.do in stk.list.all){
  
  
  print(stk.do)
  
  system.label <- stk.lookup %>% dplyr::filter(River == stk.do) %>% select(System) %>% unique()
  
  png(filename = paste0("OUTPUT/1_Exploratory_Data_Analyses/SibRegPreScreen/Diagnostics_SibRegPreScreen_",
                        system.label,"_",stk.do, ".png"),
      width = 480*5, height = 480*4, units = "px",
      pointsize = 14*3.3,
      bg = "white",  res = NA)
  
  
  par(mfrow = c(2,2),
      mai=c(2.5,2.5,2.5,1))  
  

  for(age.do in c(4,5)){
    
    
    younger.df <- pooled.data.long %>% ungroup() %>%  dplyr::filter(River == stk.do, Age == age.do-1) 
    stk.age.lookup.younger <-  stk.ages.lookup %>% dplyr::filter(Age == age.do-1)
    
    older.df <- pooled.data.long %>% ungroup() %>%  dplyr::filter(River == stk.do, Age == age.do) 
    stk.age.lookup.older <-  stk.ages.lookup %>% dplyr::filter(Age == age.do)
    
    
    xlim.use <- c(0, max(younger.df$ReturnsByAge, na.rm=TRUE) )
    ylim.use <- c(0, max(older.df$ReturnsByAge, na.rm=TRUE) )
    
    
    if(ylim.use[2] >= 10^6){scalar.use.y <- 10^6 ; scalar.label.y <-" (Mill)"}
    if(ylim.use[2] < 10^6){scalar.use.y <- 10^5 ; scalar.label.y <-" (100k)"}
    if(ylim.use[2] < 10^5){scalar.use.y <- 10^4 ; scalar.label.y <-" (10k)"}
    if(ylim.use[2] < 10^4){scalar.use.y <- 10^3 ; scalar.label.y <-" (1k)"}
    if(ylim.use[2] < 10^3){scalar.use.y <- 1 ; scalar.label.y <-""}
    
    if(xlim.use[2] >= 10^6){scalar.use.x <- 10^6 ; scalar.label.x <-" (Mill)"}
    if(xlim.use[2] < 10^6){scalar.use.x <- 10^5 ; scalar.label.x <-" (100k)"}
    if(xlim.use[2] < 10^5){scalar.use.x <- 10^4 ; scalar.label.x <-" (10k)"}
    if(xlim.use[2] < 10^4){scalar.use.x <- 10^3 ; scalar.label.x <-" (1k)"}
    if(xlim.use[2] < 10^3){scalar.use.x <- 1 ; scalar.label.x <-""}    
    
    
    
    plot.df <- younger.df %>% select(BroodYear, ReturnsByAge) %>% dplyr::rename(ReturnsYounger = ReturnsByAge) %>%
              left_join(older.df %>% select(BroodYear, ReturnsByAge) %>% dplyr::rename(ReturnsOlder = ReturnsByAge), 
                        by = "BroodYear")
    last.brdyr <- max(plot.df$BroodYear)
    
    last5.idx <- plot.df$BroodYear %in% (last.brdyr-5):(last.brdyr-1)
    prev10.idx <- plot.df$BroodYear %in% (last.brdyr-15):(last.brdyr-6)
    prevprev10.idx <- plot.df$BroodYear %in% (last.brdyr-25):(last.brdyr-16)
    
    plot(plot.df$ReturnsYounger/scalar.use.x ,
         plot.df$ReturnsOlder/scalar.use.y,
         bty="n",
         xlab=paste0("Total Age ",age.do-1," Returns ",scalar.label.x),
         ylab = paste0("Total Age ",age.do," Returns ",scalar.label.y),
         xlim = xlim.use/scalar.use.x,
         ylim = ylim.use/scalar.use.y,
         cex=1.4, pch=21,col = "darkblue", bg="lightgray",
         main = paste0("Age ",age.do, " vs. Age",age.do-1)
    )
         
    points(plot.df$ReturnsYounger[prev10.idx]/scalar.use.x ,
            plot.df$ReturnsOlder[prev10.idx]/scalar.use.y,
           cex=1.4, pch=21,col = "darkblue", bg="orange"
           )
    
    points(plot.df$ReturnsYounger[last5.idx]/scalar.use.x ,
           plot.df$ReturnsOlder[last5.idx]/scalar.use.y,
           cex=1.4, pch=21,col = "darkblue", bg="red"
    )
    
    abline(v = tail(plot.df$ReturnsYounger/scalar.use.x,1),col="red")
    
    text(tail(plot.df$ReturnsYounger/scalar.use.x,1),par("usr")[4],
      paste0("Age ", age.do-1," returns in ", last.brdyr +age.do-1),
      col="red",xpd=NA,adj=c(0.5,-0.2))
    
    legend("bottomright",
           legend = c(paste0(last.brdyr-16, " and earlier"),
                      paste0((last.brdyr-15),"-",(last.brdyr-6)),
                      paste0((last.brdyr-5),"-",(last.brdyr-1)) ),
           bty="n",pch=21, col="darkblue",pt.bg=c("lightgray","orange", "red"),
           pt.cex=1.4,title = "Brood Years",title.cex=1,title.font=2,cex=0.8
           )
    
    
    
    plot(log(plot.df$ReturnsYounger) ,
         log(plot.df$ReturnsOlder),
         bty="n",
         xlab=paste0("log(Total Age ",age.do-1," Returns)"),
         ylab = paste0("log(Total Age ",age.do," Returns)"),
         main = paste0("Log(Age ",age.do, ") vs. log(Age",age.do-1,")"),
         cex=1.4, pch=21,col = "darkblue", bg="white",
         )
    
    
    points(log(plot.df$ReturnsYounger[prevprev10.idx]) ,
           log(plot.df$ReturnsOlder[prevprev10.idx]),
           cex=1.4, pch=21,col = "darkblue", bg="darkgrey"
    )
    
    
    points(log(plot.df$ReturnsYounger[prev10.idx]) ,
               log(plot.df$ReturnsOlder[prev10.idx]),
           cex=1.4, pch=21,col = "darkblue", bg="orange"
    )
    
    points(log(plot.df$ReturnsYounger[last5.idx]) ,
               log(plot.df$ReturnsOlder[last5.idx]),
           cex=1.4, pch=21,col = "darkblue", bg="red"
    )  
         

    abline(v = log(tail(plot.df$ReturnsYounger,1)),col="red")
    
    text(log(tail(plot.df$ReturnsYounger,1)),par("usr")[4],
         paste0("Age ", age.do-1," returns in ", last.brdyr +age.do-1),
         col="red",xpd=NA,adj=c(0.5,-0.2))
    
    

  } # end looping through ages
  
  title(main = paste0(system.label,":\n",stk.do),outer=TRUE,
        col.main="darkblue",line=-2) 
  
  dev.off()
  
} # end looping through stocks




# repeate with trimmed data set, starting brood year 2000


for(stk.do in stk.list.all){
  
  
  print(stk.do)
  
  system.label <- stk.lookup %>% dplyr::filter(River == stk.do) %>% select(System) %>% unique()
  
  png(filename = paste0("OUTPUT/1_Exploratory_Data_Analyses/SibRegPreScreen/Diagnostics_SibRegPreScreen_",
                        system.label,"_",stk.do, "_trimmed2000.png"),
      width = 480*5, height = 480*4, units = "px",
      pointsize = 14*3.3,
      bg = "white",  res = NA)
  
  
  par(mfrow = c(2,2),
      mai=c(2.5,2.5,2.5,1))  
  
  
  for(age.do in c(4,5)){
    
    
    younger.df <- pooled.data.long %>% ungroup() %>%  dplyr::filter(River == stk.do, Age == age.do-1, BroodYear>=2000) 
    stk.age.lookup.younger <-  stk.ages.lookup %>% dplyr::filter(Age == age.do-1)
    
    older.df <- pooled.data.long %>% ungroup() %>%  dplyr::filter(River == stk.do, Age == age.do, BroodYear>=2000) 
    stk.age.lookup.older <-  stk.ages.lookup %>% dplyr::filter(Age == age.do)
    
    
    xlim.use <- c(0, max(younger.df$ReturnsByAge, na.rm=TRUE) )
    ylim.use <- c(0, max(older.df$ReturnsByAge, na.rm=TRUE) )
    
    
    if(ylim.use[2] >= 10^6){scalar.use.y <- 10^6 ; scalar.label.y <-" (Mill)"}
    if(ylim.use[2] < 10^6){scalar.use.y <- 10^5 ; scalar.label.y <-" (100k)"}
    if(ylim.use[2] < 10^5){scalar.use.y <- 10^4 ; scalar.label.y <-" (10k)"}
    if(ylim.use[2] < 10^4){scalar.use.y <- 10^3 ; scalar.label.y <-" (1k)"}
    if(ylim.use[2] < 10^3){scalar.use.y <- 1 ; scalar.label.y <-""}
    
    if(xlim.use[2] >= 10^6){scalar.use.x <- 10^6 ; scalar.label.x <-" (Mill)"}
    if(xlim.use[2] < 10^6){scalar.use.x <- 10^5 ; scalar.label.x <-" (100k)"}
    if(xlim.use[2] < 10^5){scalar.use.x <- 10^4 ; scalar.label.x <-" (10k)"}
    if(xlim.use[2] < 10^4){scalar.use.x <- 10^3 ; scalar.label.x <-" (1k)"}
    if(xlim.use[2] < 10^3){scalar.use.x <- 1 ; scalar.label.x <-""}    
    
    
    
    plot.df <- younger.df %>% select(BroodYear, ReturnsByAge) %>% dplyr::rename(ReturnsYounger = ReturnsByAge) %>%
      left_join(older.df %>% select(BroodYear, ReturnsByAge) %>% dplyr::rename(ReturnsOlder = ReturnsByAge), 
                by = "BroodYear")
    last.brdyr <- max(plot.df$BroodYear)
    
    last5.idx <- plot.df$BroodYear %in% (last.brdyr-5):(last.brdyr-1)
    prev10.idx <- plot.df$BroodYear %in% (last.brdyr-15):(last.brdyr-6)
    prevprev10.idx <- plot.df$BroodYear %in% (last.brdyr-25):(last.brdyr-16)
    
    plot(plot.df$ReturnsYounger/scalar.use.x ,
         plot.df$ReturnsOlder/scalar.use.y,
         bty="n",
         xlab=paste0("Total Age ",age.do-1," Returns ",scalar.label.x),
         ylab = paste0("Total Age ",age.do," Returns ",scalar.label.y),
         xlim = xlim.use/scalar.use.x,
         ylim = ylim.use/scalar.use.y,
         cex=1.4, pch=21,col = "darkblue", bg="lightgray",
         main = paste0("Age ",age.do, " vs. Age",age.do-1)
    )
    
    points(plot.df$ReturnsYounger[prev10.idx]/scalar.use.x ,
           plot.df$ReturnsOlder[prev10.idx]/scalar.use.y,
           cex=1.4, pch=21,col = "darkblue", bg="orange"
    )
    
    points(plot.df$ReturnsYounger[last5.idx]/scalar.use.x ,
           plot.df$ReturnsOlder[last5.idx]/scalar.use.y,
           cex=1.4, pch=21,col = "darkblue", bg="red"
    )
    
    abline(v = tail(plot.df$ReturnsYounger/scalar.use.x,1),col="red")
    
    text(tail(plot.df$ReturnsYounger/scalar.use.x,1),par("usr")[4],
         paste0("Age ", age.do-1," returns in ", last.brdyr +age.do-1),
         col="red",xpd=NA,adj=c(0.5,-0.2))
    
    legend("bottomright",
           legend = c(paste0(last.brdyr-16, " and earlier"),
                      paste0((last.brdyr-15),"-",(last.brdyr-6)),
                      paste0((last.brdyr-5),"-",(last.brdyr-1)) ),
           bty="n",pch=21, col="darkblue",pt.bg=c("lightgray","orange", "red"),
           pt.cex=1.4,title = "Brood Years",title.cex=1,title.font=2,cex=0.8
    )
    
    
    
    plot(log(plot.df$ReturnsYounger) ,
         log(plot.df$ReturnsOlder),
         bty="n",
         xlab=paste0("log(Total Age ",age.do-1," Returns)"),
         ylab = paste0("log(Total Age ",age.do," Returns)"),
         main = paste0("Log(Age ",age.do, ") vs. log(Age",age.do-1,")"),
         cex=1.4, pch=21,col = "darkblue", bg="white",
    )
    
    
    points(log(plot.df$ReturnsYounger[prevprev10.idx]) ,
           log(plot.df$ReturnsOlder[prevprev10.idx]),
           cex=1.4, pch=21,col = "darkblue", bg="darkgrey"
    )
    
    
    points(log(plot.df$ReturnsYounger[prev10.idx]) ,
           log(plot.df$ReturnsOlder[prev10.idx]),
           cex=1.4, pch=21,col = "darkblue", bg="orange"
    )
    
    points(log(plot.df$ReturnsYounger[last5.idx]) ,
           log(plot.df$ReturnsOlder[last5.idx]),
           cex=1.4, pch=21,col = "darkblue", bg="red"
    )  
    
    
    abline(v = log(tail(plot.df$ReturnsYounger,1)),col="red")
    
    text(log(tail(plot.df$ReturnsYounger,1)),par("usr")[4],
         paste0("Age ", age.do-1," returns in ", last.brdyr +age.do-1),
         col="red",xpd=NA,adj=c(0.5,-0.2))
    
    
    
  } # end looping through ages
  
  title(main = paste0(system.label,":\n",stk.do),outer=TRUE,
        col.main="darkblue",line=-2) 
  
  dev.off()
  
} # end looping through stocks









