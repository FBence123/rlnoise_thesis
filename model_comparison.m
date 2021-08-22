%% This script performs fixed (Log likelihood, AIC and BIC) and random effects model comparisons (BMS function).

% load fits
fbtype = 2; % Set to 1 for partial, 2 for complete feedback condition
if fbtype == 1
    mod01 = load('taskdat_fb1_01.mat');
    mod20 = load('taskdat_fb1_20.mat');
    mod21 = load('taskdat_fb1_21.mat');
    load('behavdat_fb1_21.mat');
else
    mod01 = load('taskdat_fb2_01.mat');
    mod20 = load('taskdat_fb2_20.mat');
    mod21 = load('taskdat_fb2_21.mat');
    load('behavdat_fb2_21.mat');
end

mods_name = {'exact_softmax' 'weber_argmax' 'weber_softmax'};
nmod = size(mods_name,2);
nsubj = sum(behavdat.exc);

ll_subj = zeros(nsubj,nmod);
lp_subj = zeros(nsubj,nmod);
elbo_subj = zeros(nsubj,nmod);

ll_subj(:,1) = mod01.fits.ll(behavdat.exc);
ll_subj(:,2) = mod20.fits.ll(behavdat.exc);
ll_subj(:,3) = mod21.fits.ll(behavdat.exc);

lp_subj(:,1) = mod01.fits.lp(behavdat.exc);
lp_subj(:,2) = mod20.fits.lp(behavdat.exc);
lp_subj(:,3) = mod21.fits.lp(behavdat.exc);

elbo_subj(:,1) = mod01.fits.elbo(behavdat.exc);
elbo_subj(:,2) = mod20.fits.elbo(behavdat.exc);
elbo_subj(:,3) = mod21.fits.elbo(behavdat.exc);

% Fixed effects comparisons
ll = sum(ll_subj) %log likelihood
[V, I] = max(ll) %log likelihood and number of best model
lp = sum(lp_subj) %log prior
[V, I] = max(lp) %log prior and number of best model
elbo = sum(elbo_subj) %elbo
[V, I] = max(elbo) %elbo and number of best model

bar(ll)
title('Log likelihood')
xlabel('Model')
figure
bar(lp)
title('Log prior')
xlabel('Model')
figure
bar(elbo)
title('ELBO')
xlabel('Model')

% Random effects comparisons
% for info on how to interpret this see:
% Rigoux, L, Stephan, KE, Friston, KJ and Daunizeau, J. (2014)
% Bayesian model selection for group studies—Revisited. 
% NeuroImage 84:971-85. doi: 10.1016/j.neuroimage.2013.08.065

[alpha, exp_r, xp, pxp, bor] = spm_BMS(elbo_subj)
figure
bar(pxp)
xlabel('Model')
title('Exceedance probabilities')

metrics = [ll' lp' elbo' alpha' pxp'];
if fbtype == 1
    save('metrics_fb1.mat','metrics')
else
    save('metrics_fb2.mat','metrics')
end
