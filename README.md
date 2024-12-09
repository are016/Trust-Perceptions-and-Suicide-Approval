# Trust-Perceptions-and-Suicide-Approval

R code for reproducing analyses from the PhD project: 

> Examining the Association between Trust Perceptions and Suicide Approval: A Sociological Analysis

These files contain all the R code necessary to rerun the analyses for the above PhD project. The documents do not include any of the data used in the analyses. Please visit the [World Values Survey](https://www.worldvaluessurvey.org/WVSContents.jsp) (WVS) and/or [European Values Study](https://europeanvaluesstudy.eu) (EVS) websites to download the appropriate data files. The analysis is specifically based on waves 6 (Inglehart et al. 2018) and 7 (EVS/WVS 2022b) of the WVS/EVS. 

## Folders

The files have been organised into the following folders:

_Data_ - Contains R scripts for importing and preparing data.

_Factor_Analysis_ - Contains R scripts for factor analysing trust expressions.

_Multilevel_Analysis_ - Contains R scripts for MCMC multilevel analysis of trust expressions and suicide approval.

_Functions_ - Contains documentation of all custom functions created for the analysis ('Function_Notes') as well as R scripts containing these functions. The R scripts are organised according to the purpose of the functions – e.g., recoding, factor analysis, MCMC.

## Necessary Packages

A number of R packages are required to reproduce the analyses. 

The `here` package (Müller 2020) is used to source data files and functions across folders.

For the purposes of cleaning and recoding certain parts of the data, the `tidyverse` (Wickham _et al._ 2019) and `car` (Fox 2019) packages are used. 

The `psych` package (Revelle 2024) is used for calculating polychoric correlations, Cronbach's alpha and running the factor analyses. In turn, `GPArotation` (Bernaards and Jennrich 2005) is needed to apply OBLIMIN rotations to factor solutions. 

For the multilevel analyses, model fitting is carried out under a Bayesian framework using the package `MCMCglmm` (Hadfield 2010). Specifically, the core `MCMCglmm` function is incorporated into a custom function that allows for running multiple chains (see __Custom Functions__). The package `rstan` (Stan Development Team 2024) is then required for calculating _R-hat_ values.

Finally, the `lme4` package (Bates _et al._ 2015) is used to perform diagnostic tests under a frequentist perspective.

For more details on R packages used in the analyses, please see __Cited Packages__.

## Custom Functions

To ease the analyses and reduce repetition, a number of custom functions were built. Details on these functions are provided in the __Function_Notes__ and accompanying R scripts. 

For the most part, the custom functions are designed to streamline the recoding of variables (e.g., `dummy_out`) or rearrange the output from statistical models so that they can be presented in neat tables with the `kable` package (e.g., `fa_table`). 

The MCMC custom functions, on the other hand, are used to run `MCMCglmm` with multiple chains. Specifically, the custom function `mcmc_chains` is used to run `MCMCglmm` _n_ many times to produce _n_ many chains and organise the posterior draws from these chains into a single dataset. The accompanying MCMC custom functions (e.g., `mcmc_summary`, `mcmc_trace`) are then largely dedicated to extracting useful information from the resulting dataset, such as posterior statistics and convergence diagnostics. It should be emphasised that none of these custom functions involved making changes to the source code of the MCMCglmm package - rather, the unaltered `MCMCglmm` function is slotted into `mcmc_chains` to generate and organise multiple chains. 

## Cited Data sources

EVS/WVS (2022b) _European Values Study and World Values Survey: Joint EVS/WVS 2017-2022 Dataset (Joint EVS/WVS)_. Version 4.0.0: JD Systems Institute & WVSA, available: https://doi.org/10.4232/1.14023 [accessed 18/03/2024].

Inglehart, R., Haerpfer, C., Moreno, A., Welzel, C., Kizilova, K., J., D.-M., Lagos, M., Norris, P., Ponarin, E. and Puranen, B. (2018) _World Values Survey: Round Six - Country Pooled Datafile_. Version 20201117, Madrid, Spain & Vienna, Austria: JD Systems Institute & WVSA Secretariat, available: https://doi.org/10.14281/18241.8 [accessed 18/03/2024].

## Cited Packages

Bates D, Maechler M, Bolker B, Walker S (2015). Fitting Linear Mixed-Effects Models Using lme4. _Journal of Statistical Software_, 67(1), 1-48. doi:10.18637/jss.v067.i01.

Bernaards CA, Jennrich RI (2005). “Gradient Projection Algorithms and Software for Arbitrary Rotation Criteria in Factor Analysis.” _Educational
and Psychological Measurement_, *65*, 676-696. doi:10.1177/0013164404272507 <https://doi.org/10.1177/0013164404272507>

Fox J, Weisberg S (2019). _An R Companion to Applied Regression_, Third edition. Sage, Thousand Oaks CA. <https://socialsciences.mcmaster.ca/jfox/Books/Companion/>

Hadfield, Jarrod, D. (2010). MCMC Methods for Multi-Response Generalized Linear Mixed Models: The MCMCglmm R Package. _Journal of Statistical
Software_, 33(2), 1-22. URL: <https://www.jstatsoft.org/v33/i02/>

Müller K (2020). _here: A Simpler Way to Find Your Files_. R package version 1.0.1, <https://CRAN.R-project.org/package=here>

Revelle, William (2024). _psych: Procedures for Psychological, Psychometric, and Personality Research_. Northwestern University, Evanston, Illinois. R package version 2.4.3, <https://CRAN.R-project.org/package=psych>
  
Stan Development Team (2024). RStan: the R interface to Stan. R package version 2.32.6. <https://mc-stan.org/>

Wickham H, Averick M, Bryan J, Chang W, McGowan LD, François R, Grolemund G, Hayes A, Henry L, Hester J, Kuhn M, Pedersen TL, Miller E, Bache SM, Müller K, Ooms J, Robinson D, Seidel DP, Spinu V, Takahashi K, Vaughan D, Wilke C, Woo K, Yutani H (2019). “Welcome to the tidyverse.” _Journal of Open Source Software_, *4*(43), 1686. doi:10.21105/joss.01686 <https://doi.org/10.21105/joss.01686>
