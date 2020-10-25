# Assessment of Caret Methods on Medical Data

The designed shiny app demonstrates the issue of candidate model selection and has been designed around the caret  package. This demonstrates the idea of "no-free lunch" - there is no method that works optimally in all situations.

The caret package is a framework for best-practice in regression and classification. It enables you to apply each of 237 different methods from Data Science to your data and optimally generate any hyper-parameters through resampling. The candidate models can then be fairly compared in order to select several top models that should then (and only then) be assessed against the official test  data.

The source code is present in server.R, ui.R, global.R

Actions Performed

Learning the caret style of thinking about model optimisation and selection.
Choosing appropriate feature engineering (in the right order).
Performing hyper-parameter optimisation through resampling.
Using parallel processing for methods that support this.
Producing a set of candidate models appropriate to the data.
Assessing candidate models relative to a base (null) model.
Selecting the best model using an unbiased measure.
Quantifying the performance of the best model.
A Report has been provided by assessing different models on the dataset. 
