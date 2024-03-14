### Methodological factors in forecast performance: the influence of model structure and target specificity on the performance of real time COVID-19 forecasts (provisional title)
Kath Sherratt*, others TBC, and Sebastian Funk*

*London School of Hygiene and Tropical Medicine

Performance of forecasts in capturing observed data varies in time and space with no overall "best" forecast. Two varying features of forecasting methods include the forecaster’s approach to model structure; and whether the forecaster tunes their model to each target. We investigate forecasts of weekly incident deaths for 32 countries submitted to the European COVID-19 Forecast Hub between March 2021 and March 2023. 
We use teams’ provided metadata to categorise models by their structure (mechanistic, semi-mechanistic, statistical), and by their specificity (the number of locations each team targets; and whether the target location is the same as the team’s institutional location, as a proxy for adaptation of model parameters to local conditions). We evaluate forecasts using the weighted interval score, scaled relative to a baseline of the median ensemble of all models.

See analysis plan: 

- https://docs.google.com/document/d/1k3rhId4HqNl-acDaK9OVSX7Id27ELa7F4h4BkBGAXjo/edit

Reproduce results:

- Descriptive: [draft.Rmd](./output/draft.Rmd)
- GAMM (draft): [model-wis.R](./R/model-wis.R)
