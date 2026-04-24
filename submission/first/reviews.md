PCOMPBIOL-D-25-00717
The influence of model structure and geographic specificity on predictive accuracy among European COVID-19 forecasts
PLOS Computational Biology

Dear Dr. Sherratt,

Thank you for submitting your manuscript to PLOS Computational Biology. After careful consideration, we feel that it has merit but does not fully meet PLOS Computational Biology's publication criteria as it currently stands. Therefore, we invite you to submit a revised version of the manuscript that addresses the points raised during the review process.

?Please submit your revised manuscript within 60 days Aug 15 2025 11:59PM. If you will need more time than this to complete your revisions, please reply to this message or contact the journal office at ploscompbiol@plos.org. When you're ready to submit your revision, log on to https://www.editorialmanager.com/pcompbiol/ and select the 'Submissions Needing Revision' folder to locate your manuscript file.

Please include the following items when submitting your revised manuscript:

* A rebuttal letter that responds to each point raised by the editor and reviewer(s). You should upload this letter as a separate file labeled 'Response to Reviewers'. This file does not need to include responses to formatting updates and technical items listed in the 'Journal Requirements' section below.
* A marked-up copy of your manuscript that highlights changes made to the original version. You should upload this as a separate file labeled 'Revised Manuscript with Track Changes'.
* An unmarked version of your revised paper without tracked changes. You should upload this as a separate file labeled 'Manuscript'.

If you would like to make changes to your financial disclosure, competing interests statement, or data availability statement, please make these updates within the submission form at the time of resubmission. Guidelines for resubmitting your figure files are available below the reviewer comments at the end of this letter

We look forward to receiving your revised manuscript.

Kind regards,

Michael J Plank
Academic Editor
PLOS Computational Biology

Jennifer Flegg
Section Editor
PLOS Computational Biology

Additional Editor Comments:

The reviewers are largely supportive of the study but raise a number of points that will need to be carefully addressed before publication.


Journal Requirements:
1) We ask that a manuscript source file is provided at Revision. Please upload your manuscript file as a .doc, .docx, .rtf or .tex. If you are providing a .tex file, please upload it under the item type u2018LaTeX Source Fileu2019 and leave your .pdf version as the item type u2018Manuscriptu2019.

2) Please upload all main figures as separate Figure files in .tif or .eps format. For more information about how to convert and format your figure files please see our guidelines:
https://journals.plos.org/ploscompbiol/s/figures

3) Please upload a copy of Figure figures S4 and S5 which you refer to in your text on pages 8, and 24. Or, if the figure is no longer to be included as part of the submission please remove all reference to it within the text.


Reviewers' comments:


Reviewer's Responses to Questions
Comments to the Authors:
Please note here if the review is uploaded as an attachment.

Reviewer #1: Dear authors,

Thank you for the opportunity to review the manuscript titled ‘The influence of model structure and geographic specificity on predictive accuracy among European COVID-19 forecasts’. This manuscript is clearly written, well-articulated, and demonstrates maturity and thoroughness with which you have carried out the presented scientific queries. I believe this work to be of value to readers of PLOS Computational Biology, and I recommend for its acceptance. Nonetheless, please find below suggestions which I believe would improve the quality of this manuscript, particularly in improving its value to a broader audience.

I think the overall presentation is clear and concise, however there are redundancies and ambiguities across the methods and results section. The results section has repetitive texts that repeat justifications for methodological choices (eg paragraph on line 200). Results paragraphs are also loosely organised, flowing across different aspects of the results. I think you should make better use of the result figures and tables, and incorporate subheadings to better guide readers to different aspects of the results (general characteristics of the participating models, overall WIS trends, effect sizes in the GAMM etc). Your description of the GAMM in methods also does not set up clear expectation of what results you would present, so a reader has to go back and forth to match the results presented to the questions you posed. I suggest including the regression formula for GAMM in the main text, and highlight the motivation of probing how these fixed and random effects affect WIS. I think you should also do another round on the language used for fixed/random effects/explanatory variables/covariates and standardise the terms throughout the text for ease of reading.

I have a number of queries over some methodological choices, which I think could be better explained and justified in text:

• Wouldn’t random effect for epidemic epoch (increasing, stable, decreasing) be partially correlated with random effect of location+time? I think a location+time random effect would be soaking up too much signal in the data. While your other random effects attempt to mechanistically unpack factors that would influence predictability, this one is effectively “it’s always hard/easy at this place at this time”, but provides no further insight to what makes it hard or easy. Maybe instead of a spline of time with fixed knots = 40, you could discretely divide time into phases dominated by different circulating strains (effectively different epidemic seasons).
• I’m in general curious why using an additive model with smoothing splines. Intuitively I would expect most of the modelled effects to have fairly simple relationships with predictability, I’m not sure if you need smoothing splines to capture the right shape.
• Shouldn’t the effect of model and methods be hierarchical, since each model is assigned to a class of methods?
• Upon seeing Table 1, I have concerns whether the unadjusted results are worth reporting at all. The forecasts mean for the only 3 agent-based models is substantially lower than others, probably because they are only used in countries/times with low epidemic activities. This made me realise forecast targets look vastly different across different countries and times, and importantly you do not always have paired comparison across methods for the same situation. This made me suspect any methods effect on WIS (without adjusting for confounding factors) is likely spurious. We indeed see a suspicious pattern of agent-based models seemingly having an effect on WIS scores while also having very small sample size. I think a better narrative is to say you must adjust for confounding factors to be able to compare forecast performance of your sampled models, thus the GAMM approach is necessary.
• Are there any models with sub-national forecast targets? Related, did you find any effect of country population size on WIS among single-country models?
• I think you need to more thoroughly justify why only evaluate with WIS as opposed to or in addition to eg CRPS.
• Presumably all forecast intervals are for negatively binomial distributed count data (cases or death)? Please explicitly state this.
• Why log link in the GAMM? Since WIS is already calculated on log scale, it’s not intuitive to me why the modelled effects are expected to be multiplicative.
• In the QQ plot in your supp results it looks like dispersion of WIS score in the fitted GAMM is a bit off?

Please find below more minor comments:

Lines 16-17: it is vague that by model structure you mean mechanistic vs statistical etc, and by forecast target you mean single vs multi country. I think you need to word this sentence to more accurately reflect the language used later on.

Paragraph on line 69: I think it’s worth expanding this paragraph to also point out that different models are by design meant to be good at different things. For example, statistical models are more sensitive to subtle changes in short term trends, but cannot turn around in forecasts (unless trained on previous seasons). I think it is also a good idea to remind readers that often epidemiological models can be designed with a range of goals and features in mind (scenario projection, counterfactuals, etc), and forecast is not the only use case.

Line 81: I’m not fully convinced that target-specificity is necessarily linked to how many target countries the model predicts to. It is true in practice that many ‘in-house’ models are designed with specific data streams and epidemiological context of a country in mind, but they can be in principle applied to a different country as long as relevant data exists. Conversely, one could also easily build models targeting a single country with off-the-shelf machine learning methods and country-filtered data from aggregation resources like JHU. So single country target doesn’t necessarily mean more context-specific.

Line 98-101: if you analyse forecast performance only among models submitted to the same round of forecast for the same target, you do not need to model confounding factors (and this has been done by some Hubs). I think a stronger narrative is that post-hoc modelling of forecast performance enables you to compare across targets and time periods, to interrogate effects of interest.

Line 126: are all “others” model expert judgement based? Why not just call them expert-judged?

Line 171: give package and R version numbers

Table 1: I find forecastsMean column unnecessary, it doesn’t show anything useful (and the sum doesn’t make sense). You also need to explicitly explain what the numbers in brackets are.

Figure 1: I have concerns if readers will get the right message from the figure. If someone has just seen the figure without thoroughly reading the text, they might arrive at the conclusion agent-based models are generally better, or than single country models are better, which are not the case. I wonder if these descriptive visualisations of WIS are necessary. Regardless, I also suggest making the colour scheme more distinct between panels A and B. I also think panel C is unnecessary, we cannot interpret much from patterns aggregated across so many countries each with their own epidemic trends.

Line 291: I think you need to highlight the observation about ensembling more. Your results do not consistently highlight any class of methods as superior, which is a strong piece of evidence in favour of building ensembles across different models. I think this is an important conclusion that readers should more easily spot.

Paragraph on line 295: or maybe single-country models perform better because multi-country models are fitted to data from different ongoing epidemics, and their shared or hierarchical parameters may simply be not as well calibrated for a specific country. i.e., the advantage may simply be conferred by the choice of training data, not by bespoke approaches taken by modellers. I do not think you have shown evidence that modellers of single-country models have put in more bespoke features in their models.

Line 303: an additional interesting point in this paragraph is that there is value in evaluating forecasts on the go as soon as you have data, so that you can retrospectively evaluate again in the future and understand the impact of reporting errors and corrections in the data.

323-236: I actually think the focus on just WIS over 4-weeks is a strength of this work. Exploring different ways to evaluate forecasts (e.g., over longer time horizons) requires us to have a question in mind (e.g., are we forecasting farther into the future to figure out peak timing and size?). I think you need to establish the relevant questions first before expanding this work to include longer horizons and different target quantities.

334: as suggested earlier, you can look into this if you fitted discrete time effect of different epidemic phases dominated by different variants.

Reviewer #2: In this study, the authors assess the impact of model structure and geographic specificity on epidemic forecasting performance. They evaluate a range of models using the Weighted Interval Score (WIS), incorporating adjustments for key factors such as forecast horizon, model type, and epidemiological context. The manuscript is interesting. However, it represents an incremental rather than a substantial advancement in the field. Therefore, I recommend acceptance after major revisions, with the following points addressed:

1. Model could be included within the main manuscript (As Plos Computational Biology format allows this), that would help the reader to follow the manuscript. A detailed description is also required. How was the “Model adjustment” performed? Make a detailed discussion about adjusted and non-adjusted scenarios with the corresponding model.
2. Figure 3 is not properly described in the manuscript. Probably, “Multi-country” is missing in the legend.
3. Most importantly, “This indicates that factors beyond our structural categorisation may have played a role in driving performance.” The statement says the authors found that the considered features may not be enough to capture the performance. This is also clear from the QQ plot. It is advisable to improve the model or consider additional features.
4. Authors are advised to include ensemble model within the study as it often gives better results in epidemic forecasting.

Reviewer #3: Overall the paper is quite complete with an analysis looking at whether model structure effects the outcome. I only have a few comments. However, all of these should only slightly alter the manuscript over all.
1) In the author summary perhaps a rewording about it being a large dataset. I agree it is large however, it was not large in the way needed to show the differences desired (this is described very well in the discussion at lines 309-). I think the summary should make note of this.
2) It is not clear from the method of deciding the model classification whether both KS and SF processed all the files on the first pass. As where a disagreement existed, they agreed (this would be the case if they were that majority).
3) It would be of interest to know where the disagreements existed was it spread across different classifications or was it mainly between a select two e.g. mechanistic vs semi mechanistic or statistical vs semi mechanistic.
4) It is stated in the discussion: We observed that the log scale moderated scores overall (line 339). I wonder if in the supplement it could be included the results on the natural scale given it appears some of the work has been done.
5) On line 137 when describing single country vs multi country, I wonder whether any models modelled multiple countries at once or whether all models that were multi country modelled them independently. This distinction seems of interest (it may be that this cannot be none but a mention of this would be of interest).
6) Given the experiment and there was some disagreement (in model classification) is there something that can be learned or improved in the metadata files. I think it would be nice if the authors could describe the current issues and where effort can be made (this will assist in other hubs or groups adding this to their metadata which should assist in making future studies like this easier).

Have the authors made all data and (if applicable) computational code underlying the findings in their manuscript fully available?
The PLOS Data policy requires authors to make all data and code underlying the findings described in their manuscript fully available without restriction, with rare exception (please refer to the Data Availability Statement in the manuscript PDF file). The data and code should be provided as part of the manuscript or its supporting information, or deposited to a public repository. For example, in addition to summary statistics, the data points behind means, medians and variance measures should be available. If there are restrictions on publicly sharing data or code —e.g. participant privacy or use of data from a third party—those must be specified.

Reviewer #1: Yes

Reviewer #2: Yes

Reviewer #3: Yes

PLOS authors have the option to publish the peer review history of their article (what does this mean?). If published, this will include your full peer review and any attached files.

If you choose “no”, your identity will remain anonymous but your review may still be made public.

Do you want your identity to be public for this peer review? For information about this choice, including consent withdrawal, please see our Privacy Policy.

Reviewer #1: No

Reviewer #2: No

Reviewer #3: No

Figure resubmission:

While revising your submission, please upload your figure files to the Preflight Analysis and Conversion Engine (PACE) digital diagnostic tool, https://pacev2.apexcovantage.com/. PACE helps ensure that figures meet PLOS requirements. To use PACE, you must first register as a user. Registration is free. Then, login and navigate to the UPLOAD tab, where you will find detailed instructions on how to use the tool. If you encounter any issues or have any questions when using PACE, please email PLOS at figures@plos.org. Please note that Supporting Information files do not need this step. If there are other versions of figure files still present in your submission file inventory at resubmission, please replace them with the PACE-processed versions.

Reproducibility:

To enhance the reproducibility of your results, we recommend that authors of applicable studies deposit laboratory protocols in protocols.io, where a protocol can be assigned its own identifier (DOI) such that it can be cited independently in the future. Additionally, PLOS ONE offers an option to publish peer-reviewed clinical study protocols. Read more information on sharing protocols at https://plos.org/protocols?utm_medium=editorial-email&utm_source=authorletters&utm_campaign=protocols



