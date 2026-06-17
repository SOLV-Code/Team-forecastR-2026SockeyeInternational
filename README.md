# Team-forecastR-2026SockeyeInternational

Data processing and forecasting for the 2025 Sockeye International forecasting competition

Team ForecastR includes:

* [Gottfried Pestal, SOLV Consulting](https://github.com/SOLV-Code)
* [Charmaine Carr-Harris, DFO](https://github.com/charmainecarrharris)
* [Michael Folkes, DFO](https://github.com/MichaelFolkes)
* [L. Antonio Vélez-Espino, DFO](https://github.com/avelez-espino)

We are developers and active users of the [**forecastR**](https://github.com/SalmonForecastR)  toolkit, which consists of an [R package](https://github.com/SalmonForecastR/ForecastR-Package) and a shiny app ([repo](https://github.com/SalmonForecastR/ForecastR-App), [SOLV server](https://solv-code.shinyapps.io/forecastr/), [PSC server](https://psc1.shinyapps.io/ForecastR/)).

## Introduction


The [Salmon Prize](https://salmonprize.com/) organizes annual forecasting competitions. The [2026 Sockeye International](https://salmonprize.com/competitions) includes 16 sockeye stocks from Bristol Bay in Alaska, the Fraser River in British Columbia, and the Columbia River in Washington.Presentations from the 2024 Sockeye competition are now available on [youtube](https://www.youtube.com/@SalmonPrize/videos).


**forecastR** is designed to streamline the exploration of fundamentally different model types within an iterative working group process. Initial development has focused on the suite of models routinely used for Chinook salmon forecasts by the [Chinook Technical Committee of the Pacific Salmon Commission](https://www.psc.org/about-us/structure/committees/technical/chinook/). For many Chinook stocks there is currently no spawner-recruit data set available, only time series of run size (a.k.a adult returns) or escapement, either total or broken out by age. forecastR therefore includes naive (running average) models, time series models (ARIMA, exponential smoothing), and a suite of sibling regression models (e.g., forecasting age 5 returns this year based on age 4 returns last year), including versions with time-varying parameters or environmental covariates. forecastR also includes forecasts based on return rate for cases where predictor variables are available (e.g., juvenile outmigration estimate, hatchery releases). forecastR has emphasized rapid retrospective screening of many alternative model types within a shiny app, so model types are implemented with standard frequentist methods rather than Bayesian methods that are much more computing intensive. forecastR does not currently include spawner-recruit models.

The R package includes functions to complete retrospective evaluations and rank a suite of alternative models. The shiny app allows users to explore retrospective model rankings and vary specifications in real time (e.g., add another model, revise the ranking criteria, and rerun). For details, check out the latest **[ForecastR Report](https://www.google.com/url?sa=t&rct=j&q=&esrc=s&source=web&cd=&ved=2ahUKEwiMi47T1rrvAhVVJjQIHQ-nCNYQFjAGegQIChAD&url=https%3A%2F%2Fwww.psc.org%2Fdownload%2F585%2Fvery-high-priority-chinook%2F11704%2Fs18-vhp15a-forecastr-tools-to-automate-forecasting-procedures-for-salmonid-terminal-run-and-escapement.pdf&usg=AOvVaw2ZHMiJb0dBhjytGgM8lgvZ)**.

The Salmon Prize competitions offer a great opportunity to showcase our tool kit, test package capabilities and app features on a new group of stocks, prioritize extensions for the next phase of development, and build a detailed worked example to assist the forecastR user community. 

## Overview of Approach

We're approaching this as an illustration of the full workflow we've designed the **forecastR** tool kit for. 

For our entry in the [2025 competition](https://github.com/SOLV-Code/Team-forecastR-2025SockeyeInternational) we restricted the analyses to model forms that were already included in the package and built detailed notes for each step in the workflow. Refer to the [2025 Notes](https://github.com/SOLV-Code/Team-forecastR-2025SockeyeInternational/tree/main/NOTES) for details.

For the 2026 competition, we are approaching the 3 systems differently.

### Columbia

*Approach*

Apply same set of short-listed model types as in 2025, but (1) revisit model selection considerations in light of 2025 performance of alternative models, and (2) apply the same models to the newly added component stocks (Okanagan, Wenatchee).
   
* In 2025 for "Bonneville Lock & Dam", Simple SibReg for all ages would have been closest to observed, but model ranking pointed to age-specific SibReg variations (LogPower, Kalman). 

* For 2026, the total forecasts for the 3 SibReg variations differ a lot for all three stocks, with Simple SibReg much lower than the other two for all three stocks. For Wenatchee, for example, the total forecasts by sib reg type are: SibRegSimple = 35,231, SibRegLogPower = 80,281, SibRegKalman	= 125,462). 

* Retrospective ranking generally favors either Log-Power or Kalman sib reg, but 2025 experience points to Simple SibReg for Okanagan and Bonneville Lock & Dam. 

* Need to develop a better selection rationale, but for now just go with simple sib reg for all 3 stocks, based on 2025 experience.

*Model Selections By Stock*


System | 	Stock | 	Age 3	 | Age 4 | 	Age 5
-- | -- | -- | -- | -- 
Columbia River|	Bonneville Lock & Dam|		Naive8: Basic|		SibRegSimple: Based on 2025 experience|		SibRegSimple: Based on 2025 experience
Columbia River|		Wenatchee|		Naive8: Basic|		SibRegLogPower: Based on retrospective plot	|	SibRegLogPower: Based on retrospective plot
Columbia River|		Okanagan|		Naive8: Basic|		SibRegSimple: Based on 2025 experience|		SibRegSimple: Based on 2025 experience


*2026 Forecasts and Retrospective 2021-2025 Forecasts

Results below reproduced from the file *Forecast_Totals_ByYear_SumOfMedians.csv.csv*, which shows the sum of age-specific median forecasts from the residual bootstrap intervals. An alternative version in the file *Forecast_Totals_ByYear_SumOfPtFC.csv* shows to sum age-specific point forecasts. For age-specific point forecasts and intervals, check *Forecast_Details.csv*. All 3 files are in the [FORECASTS_Columbia/OUTPUT folder](https://github.com/SOLV-Code/Team-forecastR-2026SockeyeInternational/tree/main/FORECASTS_Columbia/OUTPUT). Retrospective diagnostic plots are in the [OUTPUT/Retrospective_Diagnostics folder](https://github.com/SOLV-Code/Team-forecastR-2026SockeyeInternational/tree/main/FORECASTS_Columbia/OUTPUT/Retrospective_Diagnostics). 


System|	Stock|		2021|	2022|		2023|		2024|		2025|		2026|	
-- | -- | -- | -- | -- | -- | -- | -- 
Columbia River|		Bonneville Lock & Dam|		60465|		341277|		162629|		273111|		172674|		59134
Columbia River|		Okanagan|		46854|		191551|		114523|		221489|		122606|		44167
Columbia River|		Wenatchee|		42784|		43716|		57100|		201619|		70789|		53657


<img src="https://github.com/SOLV-Code/Team-forecastR-2026SockeyeInternational/blob/main/FORECASTS_Columbia/OUTPUT/Retrospective_Diagnostics/AllStocks_Obs_vs_FC.PNG" width="800">


### Bristol Bay
Apply same set of short-listed model types as in 2025, plus test the spawner-recruit models being developed for the next *forecastR* upgrade.




### Fraser

Apply same set of short-listed model types as in 2025, plus test the spawner-recruit models being developed for the next *forecastR* upgrade, plus explore some new Fraser-specific environmental covariates.


