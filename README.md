# Observational Study of Haloperidol in Hospitalized Patients with COVID-19

The present repository contains a coding example of the analyses performed for the manuscript titled *"Observational Study of Haloperidol in Hospitalized Patients with COVID-19"*, currently under revision in PLOS ONE. 

**Contents**: 

* `crude_analysis.R`: required packages and coding for crude Cox regression analyses, checking of PH assumption, and outliers and Kaplan-Meier curves. 
* `multivariable_analysis.R`: required packages and coding for multivariable Cox regression analyses, frequency tables, and SMD for comparing exposed vs. non-esposed groups. 
* `ps_analysis.R`: required packages and coding for the propensity-score model, SMD for balance, and Kaplan-Meier curves. 
* `optimal_matching_analysis.R`: required packages and coding for the computation of matched analytic samples, crude Cox regression analysis in the matched analytic sample, frequency tables, SMD check for balance, and Kaplan-Meier curves. 

**Note**:

All scripts included in this repository are examples of the coding used to perform the main analyses in the aforementioned manuscript. This is because all analyses were performed in a private server from APHP, with Jupiter notebook (R extension, version 2.4.3). Because these are examples, it is not identified every outcome and sample that it was applied on and I have tried to repeat code as little as possible in order to be as clear as possible. This means, for example, that even the code for outlier detection is only presented in `crude_analysis.R`, this was used in all subsequent analyses.  
