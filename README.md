# **Analysis Description** 

This repository contains the code for the main analysis conducted to estimate the serial interval of different SARS-CoV-2 variants while accounting for negative serial intervals. The analysis utilises a Bayesian framework to infer transmission pairs and explores all possible transmission trees based on observed symptom dates ([outbreaker2](https://github.com/reconhub/outbreaker2)). The code in this repository performs various tasks related to the analysis, including generating Latin Hypercube Samples, reconstructing transmission chains, fitting distributions, comparing trends, computing posterior cumulative distributions, generating epidemic curves, and more.

# Reproducibility

For a reproducible example of our methods, we recommend visiting the [**SI_simulation repository**](https://github.com/CyGei/SI_simulation). This repository contains code that utilises simulated data, which allows for a complete replication of our analysis. Please note that due to restrictions on sharing patient data, we are unable to publicly provide the actual patient data used in our study.

# File Descriptions 

`compare_fits.R`: Fits normal, log-normal, and gamma distributions to the serial interval data.

`compare_trends.R`: Calculates pairwise differences in the mean serial interval by variants of concern (VOCs).

`ecdf.R`: Computes the posterior cumulative distribution of the serial interval.

`epicurve.R`: Generates an epidemic curve, cumulative incidence, and vaccine intake plot for the Virus Watch cohort.

`fira_theme.R`: Applies the "fira_theme" ggplot style from the "firatheme" R package.

`fit_disc.R`: Fits discrete distributions to non-zero observations.

`fitted_SI.R`: Computes distribution parameters for the serial interval.

`functions.R`: Contains helper functions used in the analysis.

`gamma_fits.R`: Computes fitted gamma distribution parameters.

`LHS.R`: Includes Latin Hypercube Sampling functions.

`LHS_plot.R`: Plots the input distributions for Latin Hypercube Sampling.

`LHS_sanity_check.R`: Compares the mean serial interval by VOCs for a single Latin Hypercube Sampling input.

`libraries.R`: Loads required R packages.

`load.R:` Loads the VirusWatch data (note that for data privacy and security reasons we cannot publicly share the data, please contact the authors).

`master.R`: Serves as the main file that runs the entire analysis.

`misc.R`: Loads outputs from the analysis (note that for data privacy and security reasons we cannot publicly share the data, please contact the authors).

`naive_models.R`: Compares theoretical, pairwise, and outbreaker2 models.

`outbreaker_future_groups2.R`: Runs outbreaker2 for households in parallel using the "future" R package.

`pairwise.R`: Implements the pairwise model to compute the serial interval solely from the data.

`plot_densities.R`: Plots the densities for the mean estimate and 95% credible interval.

`posterior_mean_sd.R`: Estimates the mean serial interval (and standard deviation) including observations at zero.

`results.R`: Prints key results from the analysis.

`same_date_si.R`: Computes the posterior sample for households reporting the same symptom onset date.

`save_input_data.R`: Saves input data for the model.

`serial_interval.Rproj`: Rproject file for the analysis.

`SI.R`: Extracts the serial interval.

`table_demographics.R`: Produces a descriptive table.

`truncation.R`: Computes the percentage of values lost from truncating the serial interval.
