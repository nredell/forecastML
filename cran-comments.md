## Test environments

* local Windows install, R 4.0.0 and 3.6.2
* ubuntu 16.04 (on travis-ci), R 4.0.0 and 3.6.2

## R CMD check results

0 errors | 0 warnings | 0 notes

## Submission history

* **05-01-2020**
* This is a minor version update, 0.8.0 to 0.9.0, for an existing CRAN package.
    + User-visible new features
        + New function `create_skeleton()` for analysis with big data.
        + New argument in `create_lagged_df(..., predict_future = NULL)` for predicting dynamic features.
        + Added the rmsse error metric from the M5 forecasting competition.
        + Forecast combination is clearer and now supports custom aggregation with 
        `combine_forecasts(..., aggregate = stats::median)`.
        + Added a new vignette for forecast combinations.
        + Continued improvement of plot aesthetics.
    + Bug fixes
        + Multiple grouping columns now works with `create_lagged_df()`.
        + Support outcome column positions other than 1 in `create_lagged_df()`.
        + Plotting single time series predictions with non-contiguous windows removes connecting line.
    + Internals
        + DESCRIPTION: works with dplyr < v1.0.0 and v1.0.0.
        + Reduced build time of existing vignettes.
        + Unit test code coverage = 83%.
<br>

* **02-28-2020**
* This is a minor version update, 0.7.0 to 0.8.0, for an existing CRAN package.
    + User-visible new features
        + Support forecasting with multi-output models (e.g., multi-output neural networks).
        + Refined plot API including support for pivoting results with formulas.
        + Added a new vignette for forecasting grouped time series with factors.
    + Bug fixes
        + Forecast combinations with grouped time series and factors now works.
        + Set required R version to 3.5.0
    + Internals
        + Substantial code refactor
        + Unit test code coverage = 82%.
<br>

* **01-06-2020**
* This is a minor version update, 0.6.0 to 0.7.0, for an existing CRAN package.
    + User-visible new features
        + Support forecasting factor outcome levels and probabilities.
        + Add function `combine_forecasts()` to produce final h-step-ahead forecasts from ensemble models.
        + Reworked vignettes for style and to cover the most common use cases.
    + Bug fixes
        + None
    + Internals
        + Refactored `return_error()` to support the addition of new error metrics in zzz.R
        + Unit test code coverage = 83%.
<br>

* **11-22-2019**
* This is a minor version update, 0.5.0 to 0.6.0, for an existing CRAN package.
    + User-visible new features
        + Support sub-daily forecasting with the POSIXt datetime class.
        + Support a vector of custom validation windows in `create_lagged_df()` with `window_start` and `window_stop`.
        + The `create_lagged_df(..., keep_rows = TRUE)` argument allows retaining the first *N* rows of an ungrouped training dataset.
        + Simplified plots by removing window numbers from the legened with grouped time series.
        + Updated documentation.
    + Bug fixes
        + MAPE error metrics were incorrectly set to median MAPE metrics for error results by window.
        + `return_hyper()` now returns the correct window number.
        + Printed forcats warning message suppressed when plotting empty data.frames which is the desired behavior.
        + Support plotting numerical and categorical hyperparameters in plot.forecast_model_hyper()
    + Internals
        + Add dtplyr and data.table to the DESCRIPTION file and remove stringr.
        + Unit test code coverage increased from ~40% to 83%.
<br>

* **10-8-2019**
* This is a resubmission of a new-to-CRAN release. Changes include:
    + Reduced DESCRIPTION from 103 to 74 words.
    + Added a number of stop("message") checks on the input arguments for all functions and 
    methods (~15 additional). The checks include various missing(), is.null(), and argument type checks.
    + Minor changes in help docs for language consistency and to clarify required arguments and 
    argument types.
    + Added new unit tests; package has ~40% code coverage.

* **10-2-2019**
    + Reduced build time by several minutes (< 5 min w/ devtools::check())
    + Removed donttest{} in all examples
    + Fixed copyright format
