Rules/tips for a workflow for forecast evaluation/model comparison

- Submit to plos cb  
- Create scoringutils vignette 

Next steps:

- Iterate on this document with ideas   
- Week of august 10th– make a plan for how to structure this 

Adding some notes on meta perspective, not thought through, needs rethink but adding while fresh in mind

1. Starting points for an evaluation workflow  
   1. Model development (generate the model, and generate (or choose) the target data)  
   2. Single model evaluation (generate the model, fixed target data)  
   3. Multiple model comparison (own neither model nor data)  
   4. Multiple model and multiple target comparison  
2. Defining the depth of evaluation  
   1. Describing accuracy  
   2. Describing variation in accuracy  
   3. Tracing causes of varying accuracy

(as a side note we could frame this as often facing a breadth/depth trade-off with forecast evaluation)

In no particular order so I am just going to use bullet points for now:

* If evaluating retrospectively, data used to produce forecasts should be as close to what was available real-time as possible (i.e. use data vintages as oppose to truncating data before the forecast date to account for real-time data revisions and reporting lags)  
* Consider your forecast set – does each model produce forecast for the same set (e.g. horizons, forecast dates, locations)? If the answer is no, than naive averages of WIS/CRPS/coverage are not directly comparable without introducing bias. Consider using scaled relative skill scores and/or model-based assessments of forecast performance  
* Overall average performance should be assessed in some way e.g. average WIS/CRPS across all strata  
* Performance along each strata should be assessed by some overall metric (e.g. horizon, location, forecast date, year, etc.)  
* Both model calibration and model accuracy should be assessed (e.g. coverage and WIS/CRPS)  
* Performance should be compared to a baseline, and the baseline should be chosen in an informed manner (point to Manuel’s paper). Depending on the answer to above question about the forecast set, rWIS/rCRPS/SRSS should be presented alongside absolute magnitude of scores  
* Look to analysis of cohort studies for framing   
* Treating forecasts/models/errors as data in its own right – hierarchical modelling, accounting for bias in your sample, population normalisation, etc.   
  * Strobe checklist – map what these correspond to in forecast evaluation \- specifically for retrospective analysis of this kind of data   
    * participants  
  * EPIFORGE is focused on forecasting process  
* Distinguish between evaluating difficulty of forecasting process and difficulty of the target   
* 

Open questions:

- Should we be encouraging the use of statistical tests for model comparison? Recently have had reviewers asking for this and we have chosen to use bootstrapping of the performance of individual forecast date-location-models to produce CIs but tried to avoid producing p-values  
- Another option is suggesting using model confidence sets. I think both of these are on the “nice to have” or “consider if relevant for your audience/context” to be honest  
- 

How to choose subsets of forecasts to show without cherry-picking

- Policy-relevant points  
- Periods of rapid change  
- Timepoints that can show performance of specific assumptions  
- Median, max/min performance

Kath (jotting down key points without elaboration for now, in no order)

- Assess the relevance of the forecast target. Forecasts are ultimately operational products \- they are not really useful for scientific explanation. Check your target aligns with what’s needed  
  - Where possible align with already-existing performance metrics/framework for the target forecast user/consumer. E.g. outbreak detection: [7-1-7 framework](https://www.thelancet.com/journals/langlo/article/PIIS2214-109X\(23\)00133-X/fulltext)  
- Then use that to develop a relevant metric based on suitability to meet the end user goals. Is reliable coverage more important, or assessing risk at the extremes?  
- Visualise and interpret that metric in a way that makes sense to the user (without losing the message)  
- Consider multiple metrics and compare by ordinal rank, not just continuous score; there might not be that much difference between rank in practice (Li et al 2017\)  
- Use probabilistic scores. And then focus on the forecasting paradigm: maximise sharpness subject to calibration  
- Always stratify by horizon…  
- Pre-specify the strata you will use in evaluation and justify why that might matter to forecast performance. Report additional analyses but clarify that these are post-hoc choices.  
- Probably should have something on evaluation relative to ensemble but we don’t have a very systematic way to do this yet (... I stopped working on it with S)

- Evaluate temporal coherence \- stability of forecasts created successively over time for the same target \- e.g. by the Cramer distance; this is complementary to accuracy, suggests calibration of the model with its own uncertainty (eg does the median at t-1 fall within the 95% CI of forecast at t-4)

[https://journals.plos.org/ploscompbiol/s/other-article-types](https://journals.plos.org/ploscompbiol/s/other-article-types)  
