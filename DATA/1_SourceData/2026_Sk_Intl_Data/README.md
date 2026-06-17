# 2026_Sk_Intl_Data


**NEED TO UPDATE THE TEXT BELOW AND INCLUDE THE LINKS FOR THE DATA PACKS AND THE DATA MANAGEMENT REPO**



## Adult Returns/Recruits

Three versions of the combined dataset are provided for the event:

* Combined_Brood_Bristol_Columbia_Fraser.csv
* Combined_First_Year_At_Sea.csv
* Combined_Return_Bristol_Columbia_Fraser.csv

They are available
for download [here](https://drive.google.com/file/d/14Wn7BXlfkJua5Wmr5o1ZKLRm7jCwp2Ca/view?usp=sharing). A map of the stock locations is available [here](https://salmonprize.com/competitions/2025-sockeye-international).


The following summary for the data is included with the download:

```
To assist Salmon Prize teams in rapidly developing their estimates, we have pre-packaged brood and return tables for the fourteen runs across three systems. This includes:

1.	All 14 runs across the three systems 
a.	Brood table
b.	Return table
c.	First year at sea table

2.	Bristol Bay System — covering eight runs ()
a.	Brood table
b.	Return Table

3.	Fraser River — covering five runs ()
a.	Brood table
b.	Return table

4.	Columbia River — one run covering the entire system
a.	Brood table
b.	Return table


These tables have been prepared by Alejandro Yanez. If anyone has any questions please reach out to Alejandro by email at a.yanez@oceans.ubc.ca.
```

**Notes**

All 3 tables include the same number. They are just mapped onto different years based on the cohort. For example, for the Alagnak stock in Bristol Bay the brood file shows 4,468 Age 1.1 adult recruits for the 1961 brood year. The First Year At Sea file shows the same number of Age 1.1 recruits for the  1963 ocean entry year, and the same number of Age 1.1 as returns in 1964. These are 3 year old fish who entered the ocean in their second year. In *European notation* these are designated as 1.1 for 1 winter rearing in freshwater and 1 winter rearing in the ocean. To get the total age from this notation, sum the numbers and add 1. An alternative notation, called *Gilbert-Rich* labels the same fish as 3<sub>2</sub> where the main number is the total age and subscript is the year they entered the ocean. Throughout this repo we denote these as ```3_2``` to reduce markdown formatting requirements.

These notations and year matches regularly cause confusion and data wrangling errors, especially when trying to match up environmental covariates,  so we created an [age class lookup file](https://github.com/SOLV-Code/Team-forecastR-2025SockeyeInternational/blob/main/DATA/2_Lookup_Files/GENERATED_AgeClass_Lookup.csv) using [this script](https://github.com/SOLV-Code/Team-forecastR-2025SockeyeInternational/blob/main/CODE/1_Process_Stock_Data.R).

Here's a quick reference:

European | Gilbert-Rich
 -- | --
 0.2 | 3_1
 0.3 | 4_1
 0.4 | 5_1
 1.1 | 3_2
 1.2 | 4_2
 1.3 | 5_2
 1.4 | 6_2
 2.1 | 4_3
 2.2 | 5_3
 2.3 | 6_3


 ## Spawners

During the competition, some participants requested spawner data as well.

The file ```ManuallyExtracted_Spawners_Combined.csv``` is a reorganized csv version of the 3 spawner data files available for download [here](https://drive.google.com/file/d/14Wn7BXlfkJua5Wmr5o1ZKLRm7jCwp2Ca/view?usp=sharing).

**Note:** These values were extracted "as-is" from source files provided by the agencies. The values are consistent within each system, but variable definitions differ between agencies.

* Columbia Aggregate (Bonneville Lock & Dam) : Count of returning adults at Bonneville
* Fraser Stocks (Chilko, Late Stuart, Quesnel, Raft, Stellako): total brood year spawners 
* Bristol Bay Stocks (Rest): total brood year escapement

The file ```Production Data_Detailed Format_5_Summer_Stocks.csv``` is the source for the Fraser sockeye spawner data used in the file above. It is used here to extract effective female spawners ('total_broodyrEFS').





