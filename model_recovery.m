%% Script to collect simulated and recovered parameters

partial = load('fits_sim/rnd_noisyKF_fb1_fit21_vbmc.mat');
complete = load('fits_sim/rnd_noisyKF_fb2_fit21_vbmc.mat');

nsubj = size(partial.pid,1);
simparams = zeros(nsubj,15);
any_outliers = readtable('Bence_list_outliers.csv');

pids = table2cell(any_outliers(:,3));
ll_outlier = table2array(any_outliers(:,14));

for i = 1:nsubj
    if any(strcmp(partial.pid(i),pids)) && ll_outlier(i) == 0
        simparams(i,15) = 1;
    end
    simparams(i,1) = partial.alpha(i);  % simulated partial alpha
    simparams(i,2) = partial.delta(i);  % simulated partial delta
    simparams(i,3) = partial.zeta(i);   % simulated partial zeta
    simparams(i,4) = partial.tau(i);    % simulated partial tau
    simparams(i,5) = complete.alpha(i); % simulated complete alpha
    simparams(i,6) = complete.zeta(i);  % simulated complete zeta
    simparams(i,7) = complete.tau(i);   % simulated complete tau

    simparams(i,8) = partial.out_fit{i,1}.xavg(1);   % recovered partial alpha
    simparams(i,9) = partial.out_fit{i,1}.xavg(2);   % recovered partial delta
    simparams(i,10) = partial.out_fit{i,1}.xavg(3);  % recovered partial zeta
    simparams(i,11) = partial.out_fit{i,1}.xavg(4);  % recovered partial tau
    simparams(i,12) = complete.out_fit{i,1}.xavg(1); % recovered complete alpha
    simparams(i,13) = complete.out_fit{i,1}.xavg(2); % recovered complete zeta
    simparams(i,14) = complete.out_fit{i,1}.xavg(3); % recovered complete tau       
end

save('simparams.mat','simparams')
