# Code for my thesis, submitted at the ENS DEC, titled: Understanding individual differences in reward-guided learning as an efficient adaptation to task uncertainty and early life adversity: a pre-registered study in a large online sample

## Matlab scripts for preprocessing
collect_fits.m should be run first. It collects and saves the fitting results for all subjects.

compare_exclusions next. It calculates and saves the exclusion criteria.

match_outliers.R next. It checks and matches the IDs of the questionnaire outliers and the task based ones. It also plots their correspondence across participants.

add_exc.m next. It adds the final exclusion criteria to to the data.

collect_behav.m next. It calculates indices for the analyses of task behaviour.

Q_diff_rt.m next. It preprocesses data for the Q value difference analysis.

switch_prob.m next. It preprocesses data for the switch probability analysis.

mut_info.m next. It preprocesses data for the mutual information analyses.

model_comparison.m next. It performs and saves model comparisons.

model_recovery.m next. It collects the simulated and recovered analyses.

cor_params.m next. It creates a database for the model parameter analyses.


## R scripts for stats and plotting

behav.R performs the model free analyses of task behaviour.

model_comparison.R plots the fitting indices.

mut_info.R performs the analyses related to mutual information.

choice_sensitivity.R performs the choice sensitivity prediction error analyses.

cor_explore.R performs the correlational analyses of model parameters.

pca.R performs the PCA analysis of model parameters.

par_recovery.R plots the parameter recovery and performs the simulations of measurement noise and the correlation correction.
