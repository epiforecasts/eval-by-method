# Skeleton

## Abstract

_Background_
_Methods_
_Results_
_Interpretation_

## Introduction

For public health,

- Forecasting aims to be accurate to observed data: minimising error and maximising calibration.
- There are many forecasters and forecasting methods
- Identifying which methods are most accurate, when, and why can help prioritise model development/interpretation

However,

- Accuracy is difficult to compare because
  - there is an unbalanced set of modellers forecasting across different targets
  - there is different expected difficulty, or predictability, of each target, influencing both modelling approach and resulting score: i.e. confounding factors
- The issue of an unbalanced sample can be handled by either restricting the sample to ensure consistency, or scoring with pairwise comparison
- The issue of confounding is typically handled by stratifying scores
  - For example, age is almost universally considered as a confounding variable in epidemiological analysis and adjusted for
  - Analogously, we might include forecast horizon as inherently changing the system we are predicting

We suggest that,

- Both of these issues can be handled together by fitting a model to forecast scores
- This explicitly identifies the analyst's view of relationships affecting forecast performance
- In this work, we explore the effects of model structure on forecast performance

_Approach_

We consider the effect of forecasting model structure (mechanistic, statistical, ensemble, etc.) on per-target forecast accuracy (weekly, 1:4 week horizon, 32 countries), measured as WIS.

We hypothesise that mechanistic structures perform better than others because of the ability to incorporate domain expertise to account for epidemiological mechanisms. 

We consider this effect could be modified by whether a forecasting team chooses to forecast for one or many different locations as affecting resources for incorporating this domain expertise.

We adjust for confounding factors affecting both the overall predictability of the forecast target (outcome) and the choice of model structure (exposure).

### Methods

_Descriptive analysis_

We describe overall distribution of the outcome WIS

We describe the overall characteristics of the participating models: total sample size, participant-time, sample variation per week, distribution of follow up time, sample availability per target

We consider variation in WIS by model structure
We use the intra-class correlation coefficient to demonstrate heterogeneity within vs between categories of model structure

_Model building_

We use a DAG to demonstrate our hypothetical model setting

We use a GAMM to account for a multi-model, multi-target forecast setting.

- _Outcome_: (continuous) Weighted Interval Score (WIS) for incident count on log scale
- _Exposure_: (categorical) Model structure
- _Adjustment set_:
  - Epidemiological mechanism
    - Epidemiological target (cases or deaths)
    - ? Trend in incidence
    - Dominant variant phase in each location (random effect)
    - ? Location-specific (random effect) - representing policy/behaviour
  - Forecasting properties
    - Horizon by model (factor smooth interaction)
  - Model-specific random effect to account for repeated measures per model
- _Interactions_:
  - CountryTargets (geographic specificity: single vs multi-location)

_Model fitting_

We first fit crude associations between the outcome, exposure, and each considered confounding factor (the minimal adjustment set).

We explore possible effect modification (interaction) by testing whether CountryTargets has a crude association with both exposure and outcome.

We estimate the total effect of model structure on WIS with the minimal adjustment set.

We then include an interaction with CountryTargets.

_Sensitivity analysis_

We repeated the same procedure with the outcome on the natural scale (absolute incidence).

### Results

Outcome description

Cohort description

Table 1. Cohort description

Model fitting

Results on the log-transformed scores

Figure 1. Adjusted partial effect of model structure on WIS (log transformed)

Table 2

Results on the natural scale

### Discussion

_Summary_


_Interpretation_


_Limitations_

* no account of temporal correlation structure in the outcome - could this be addressed with a variogram score?
* misclassification or change in exposure status i.e. model categorisation
* differential nonresponse / loss to followup
* Residual confounding: e.g. location-specific effect to represent mechanisms operating at country-unit level doesn't capture heterogeneity within or correlation among countries; variant phase identification as a proxy for different outbreak regimes
* table 2 fallacy - we've constructed this model adjustment set to account for effects on forecasting model structure, can't interpret e.g. effects of variant phase on accuracy

_Further work_
Using classical statistical methods to more accurately capture, summarise, and understand underlying features ofo forecast skill in a multi-model, multi-target setting. 
This could include exploring feature/dimensionality reduction eg PCA to suggest latent (unobserved) influences. 
The overall aim is to help us understand the dynamics of relationships between model and target in forecasting work.
This helps suggest where to prioritise investment between advancing specific modelling techniques compared to improved understanding of the target (eg priorities for surveillance data collection).

## Supplement

Figure: lollipop cohort plot of model follow up time

Figure: Model-specific random effects showing variability across models is greater than within model type

Results on the natural scale
