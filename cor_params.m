%% This script collects parameters across subjects and plots the correlation structure
%
% for each subjects the parameters we explore are:
% alpha : learning rate
% delta : decay rate (only in partial)
% zeta : computational noise
% tau : policy temperature
% vd : drift variance
% switches : nr of reversals (switching to the other option)
% nongreedy : fraction of nongreedy decisions (those that are different than a no
% noise argmax model)
% frac_noise : fraction of nongreedy decisions explained by noise
%
% load fits

partial_b = load('behavdat_fb1_21.mat');
partial_t = load('taskdat_fb1_21.mat');
complete_b = load('behavdat_fb2_21.mat');
complete_t = load('taskdat_fb2_21.mat');

nsubj = size(partial_b.behavdat.actions,1);
ntrial = size(partial_b.behavdat.actions,2);
params = zeros(nsubj,21);
params_name = {'p_alpha', 'p_delta', 'p_zeta', 'p_tau', 'p_switches', 'p_nongreedy', 'p_frac_noise', 'c_alpha', 'c_zeta', 'c_tau', 'c_switches', 'c_nongreedy', 'c_frac_noise', 'p_alpha_std', 'c_alpha_std', 'p_delta_std', 'p_zeta_std', 'c_zeta_std', 'p_tau_std', 'c_tau_std', 'idx'};

% Loop over all subjects
for i = 1:nsubj
    params(i,2) = partial_t.fits.params(i,2);
    
    params(i,1) = partial_t.fits.params(i,1);
    params(i,8) = complete_t.fits.params(i,1);
    
    params(i,3) = partial_t.fits.params(i,3);
    params(i,9) = complete_t.fits.params(i,2);
    
    params(i,4) = partial_t.fits.params(i,4);
    params(i,10) = complete_t.fits.params(i,3);
    
    params(i,5) = sum(partial_b.behavdat.switches(i,:))/ntrial;
    params(i,11) = sum(complete_b.behavdat.switches(i,:))/ntrial;
    
    params(i,6) = sum(partial_b.behavdat.nongreedy(i,:))/ntrial;
    params(i,12) = sum(complete_b.behavdat.nongreedy(i,:))/ntrial;
    
    params(i,7) = partial_b.behavdat.noisy(i);
    params(i,13) = complete_b.behavdat.noisy(i);
    
    params(i,14) = partial_t.fits.std(i,1);
    params(i,15) = complete_t.fits.std(i,1);

    params(i,16) = partial_t.fits.std(i,2);
    
    params(i,17) = partial_t.fits.std(i,3);
    params(i,18) = complete_t.fits.std(i,2);
    
    params(i,19) = partial_t.fits.std(i,4);
    params(i,20) = complete_t.fits.std(i,3);
end

% Visualize variances and correlations
%corrplot(params,'varNames',params_name)

params(:,21) = partial_t.fits.exc;

% save to mat file
save('params.mat','params')
