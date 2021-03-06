## Code for my thesis, submitted at the ENS DEC, titled: Understanding individual differences in reward-guided learning as an efficient adaptation to task uncertainty and early life adversity: a pre-registered study in a large online sample

This repository contains Matlab and R scripts, required to reproduce all results presented in the thesis. The scripts should be run in a predefined order, described below, along with a brief description of what each file does.

## Mostly Matlab scripts for preprocessing
collect_fits.m should be run first. It collects and saves the fitting results for all subjects.

compare_exclusions next. It calculates and saves the exclusion criteria.

match_outliers.R next. It checks and matches the IDs of the questionnaire outliers and the task based ones. It also plots their correspondence across participants.

add_exc.m next. It adds the final exclusion criteria to to the data.

collect_behav.m next. It calculates indices for the analyses of task behaviour.

choice_sensitivity.m next. It preprocesses data for the choice sensitivity analyses.

Q_diff_rt.m next. It preprocesses data for the Q value difference analyses.

switch_prob.m next. It preprocesses data for the switch probability analyses.

mut_info.m next. It preprocesses data for the mutual information analyses.

model_comparison.m next. It performs and saves model comparisons.

model_recovery.m next. It collects the simulated and recovered analyses.

cor_params.m next. It creates a database for the model parameter analyses.


## Mostly R scripts for stats and plotting

behav.R performs the model free analyses of task behaviour.

model_comparison.R plots the fitting indices.

mut_info.R performs the analyses related to mutual information.

choice_sensitivity.R performs the choice sensitivity prediction error analyses.

cor_explore.R performs the correlational analyses of model parameters.

pca.R performs the PCA analysis of model parameters.

sim_pca.m performs the PCA simulations.

sim_pca_analysis.R performs the analyses of simulated data.

par_recovery.R plots the parameter recovery and performs the simulations of measurement noise and the correlation correction.
