### Relating forecast performance to methodology: the influence of model structure and target specificity on the performance of European COVID-19 forecasts

Kath Sherratt (1), Rok Grah (2), Bastian Prasse (2), The European COVID-19 Forecast Hub, Sam Abbott (1), Sebastian Funk (1)

(1) London School of Hygiene & Tropical Medicine
(2) European Centre for Disease Prevention and Control

#### Abstract

Background: The performance of forecasts in capturing observed data often varies in time and space with no overall "best" forecast. Two varying features of forecasting methods that could help explain this are: the forecaster’s approach to model structure; and whether the forecaster specifically tunes their model to each target. We investigated short-term forecasts of weekly incident deaths for 32 countries submitted to the European COVID-19 Forecast Hub between March 2021 and March 2023. 

Methods: We categorised 39 models by their structure (mechanistic, semi-mechanistic, statistical), and by their specificity (the number of locations each team targeted; and whether the target location was the same as the modeller’s institutional location, as a proxy for model adaptation to local conditions). We assessed forecast performance using the weighted interval score. First, we compared performance relatively against a median ensemble of all models. Next, we used a generalised additive model to explore performance among different model structures and specificity, while accounting for the level and trend of observed incidence, the forecast horizon, and random variation among models. 

Results: We evaluated forecasts of COVID-19 deaths up to 4 weeks ahead for up to 32 countries over 104 weeks. No one model structure consistently outperformed the ensemble. Mechanistic models saw the widest range in performance, with the range of scores including both beating the ensemble and performing up to three times worse. Models that targeted only one or two countries appeared to perform on average better and more consistently compared to multi-country models. 

Interpretation: Based on this study we would recommend that mechanistic models should be part of predictive ensembles, with an emphasis on using information from the local context of where they are applied. Multi-model comparisons should encourage methodological diversity while ensuring that detailed information on methodology is collated alongside the numerical predictions.

Current working draft:
- <https://docs.google.com/document/d/1OOVUHR_BGWcviSNxvaHvbXD16Bb3Y_zhw--7gAGBqMk/edit#>

View results:

- [Results](./output/results.pdf)
- [Supplement](./output/supplement/supplement.pdf)

Reproduce results:

- [results.Rmd](./output/results.Rmd)
- [supplement.Rmd](./output/supplement/supplement.Rmd)

### Change log

##### April 2024

- Remove code that removes anomalies from the dataset of forecasts to be scored. 
  - Rationale: we should include forecasts around anomalous data as the aim is to assess real-time forecast performance; removing anomalies is likely to moderate the effect of any domain-specific knowledge that is the target of this study. One aim is to explore whether teams with local-country knowledge produce more accurate forecasts, and predicting/adjusting to anomalies in real time is, in theory, a time at which this difference might be observed. Removing anomalies removes the possibility of observing this effect.
  - Impact: including forecasts for anomalous data allowed for an extra 9,864 forecast scores (88,141 with versus 78,277 without). Comparing results before and after the change, there was a slight impact on the analysis of single- versus multi-country forecasting models. There was no substantial difference in results from the final GAMM (showing no effect of method structure or single/multi-country targeting). There was an impact in the sensitivity analysis that removed the random effect on individual models. In that GAMM, the effect of single or multi-country models became somewhat more substantial (including anomalies, p=0.02; when anomalies were excluded this was p=0.39).

- Add categorisation of models by location of modelling teams' institutional affiliations.
  - TODO: associate with scores by location (same/different as institutional affiliation)

