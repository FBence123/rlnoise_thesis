%% This script simulates models with low and high values on the 2 PCs

partial = load('behavdat_fb1_21.mat');
complete = load('behavdat_fb2_21.mat');
params_full = readtable('PCA_pars.csv');
partial.behavdat.pid = partial.behavdat.pid(partial.behavdat.exc);
complete.behavdat.pid = complete.behavdat.pid(complete.behavdat.exc);
nsubj = size(partial.behavdat.pid,1);
ntrl = size(partial.behavdat.actions,2);

parset = {'Dim1_high', 'Dim1_low', 'Dim2_high', 'Dim2_low'};

% Partial condition

for j = 1:size(params_full,2) %loop through parameter sets to test
    params = table2array(params_full(1:4,j));
    
    pca_simdat.nongreedy = zeros(nsubj,ntrl);
    pca_simdat.noisy = zeros(1,nsubj);
    pca_simdat.actions = zeros(nsubj,ntrl);
    pca_simdat.reward = zeros(nsubj,ntrl);
    pca_simdat.switches = zeros(nsubj,ntrl);

    for i = 1:nsubj %Loop through 'subjects'
        fname = sprintf('/data/fulldata_213/fulldata_complete0_%s.mat', ...
            partial.behavdat.pid{i});
        dat = load(fname);

        [~,~,~,~,pca_simdat.actions(i,:),~,~] = simulate_agents('weber_softmax',dat,1,params);
        
        for t = 1:ntrl
            if pca_simdat.actions(i,t) == 1
                pca_simdat.reward(i,t) = dat.reward_1(t);
            elseif pca_simdat.actions(i,t) == 2
                pca_simdat.reward(i,t) = dat.reward_2(t);
            end
        end
        
        for t = 2:ntrl
            if pca_simdat.actions(i,t) ~= pca_simdat.actions(i,t-1)
                pca_simdat.switches(i,t) = 1;
            else
                pca_simdat.switches(i,t) = 0;
            end
        end
        
        [~,~,~,~,corresp,~,~] = simulate_agents('optim',dat,1);
        pca_simdat.nongreedy(i,:) = pca_simdat.actions(i,:) ~= corresp';
        
        % set fitting parameters
        nb = max(dat.blocks); % number of blocks
        nt = nnz(dat.blocks == 1); % number of trials per block
        cfg.nstype = 'weber'; % noise type (set to weber for our candidate model)
        cfg.chrule = 'softm'; % choice rule (set to softmax for our candidate model)
        cfg.fitalgo = 'vbmc'; % fitting algorithm
        cfg.isnoisy = 1; % noisy
        cfg.alpha = params(1); % learning rate
        cfg.delta = params(2); % drift variance
        cfg.zeta = params(end-1); % learning noise
        cfg.tau = params(end); % policy temperature
        cfg.nsmp = 1e3; % number of samples used by particle filter
        cfg.nres = 1e2; % number of bootstrap/validation resamples
        cfg.nrun = 10;  % number of random starting points for initialization fit
        cfg.trl  = repmat((1:nt)',[nb,1]); % trial number in current block
        cfg.resp = pca_simdat.actions(i,:)'; % response
        cfg.rt   = cat(2,dat.reward_1(:),dat.reward_2(:))/100; % reward samples
        cfg.fbtype  = 1;
        cfg.verbose = 0;
        cfg.noprior = 0;
        %cfg.run_bs = 1;

        out = fit_noisyKF(cfg);
        pca_simdat.noisy(i) = out.pngx;
        disp(i)
    end
    
    fname = strcat(parset{j},'_partial');
    save(fname,'pca_simdat')
end
