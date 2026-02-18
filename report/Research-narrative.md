## Overall narrative

This gives a brief overview of the research problem and how this work intends to approach it. 

For public health,

- Forecasting aims to be accurate to observed data: minimising error and maximising calibration.
- There are many forecasters and forecasting methods 
- Identifying which methods are most accurate, when, and why can help prioritise model development/interpretation

However,

- Accuracy is difficult to compare when there is an unbalanced set of modellers forecasting across different targets with many possible confounding factors (different expected difficulty, or predictability, of each target, influencing both modelling approach and resulting score)
- The issue of an unbalanced sample can be handled by restricting the sample to ensure consistency, or scoring with pairwise comparison
- The issue of confounding is typically handled by stratifying scores
   - For example, age is almost universally considered as a confounding variable in epidemiological analysis and adjusted for
   - Analogously, we might include forecast horizon an inherently changing the system we are predicting

We suggest that,

- Both of these issues can be handled together by fitting a model to forecast scores
- This explicitly identifies the analyst's view of relationships affecting forecast performance
- In this work, we explore the effects of model structure, and geographic specificity, on forecast performance
- We use a GAMM to account for a multi-model, multi-target forecast setting

Specifically, our analysis uses a Generalized Additive Mixed Model (GAMM):

- **Response**: Weighted Interval Score (WIS) on log scale
- **Fixed effects of interest**:
  - Method (model structure: mechanistic, statistical, ensemble, etc.)
  - CountryTargets (geographic specificity: single vs multi-location)
- **Adjustment factors**:
  - Trend in incidence (cases or deaths)
  - Dominant variant phase in each location (random effect)
  - Horizon by model (factor smooth interaction)
  - Location-specific (random effect)
  - Model-specific random effects

## Full manuscript summary

