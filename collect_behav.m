%% Collect behavioural data and calculate smoothing distributions of Q values for all subjects

fbtype = 1; % Set to 1 for partial, 2 for complete feedback condition
% load list of Prolifics identifiers
fname = sprintf('taskdat_fb%d_21.mat',fbtype);
load(fname)

nsubj = size(fits.pid,1);
ntrl = fits.ntrl;

behavdat.pid = fits.pid;
behavdat.QA = zeros(nsubj,ntrl); % Q(A)
behavdat.QB = zeros(nsubj,ntrl); % Q(B)
behavdat.QA_wn = zeros(nsubj,ntrl); % noiseless Q(A)
behavdat.QB_wn = zeros(nsubj,ntrl); % noiseless Q(B)
behavdat.actions = zeros(nsubj,ntrl); % actions
behavdat.switches = zeros(nsubj,ntrl); % switches
behavdat.rt = zeros(nsubj,ntrl); % reaction times
behavdat.reward_1 = zeros(nsubj,ntrl); % reward of option A
behavdat.reward_2 = zeros(nsubj,ntrl); % reward of option B
behavdat.reward = zeros(nsubj,ntrl); % reward received
behavdat.correct = zeros(nsubj,ntrl); % 'correct' actions
behavdat.nongreedy = zeros(nsubj,ntrl); % 'nongreedy' actions
behavdat.noisy = zeros(1,nsubj); % nongreedy actions explained by noise
behavdat.optQA = zeros(nsubj,ntrl); % optimal Q(A)
behavdat.optQB = zeros(nsubj,ntrl); % optimal Q(B)
if fbtype == 1
    behavdat.PE = zeros(nsubj,ntrl); % prediction errors from optimal model
else
    behavdat.PEA = zeros(nsubj,ntrl);
    behavdat.PEB = zeros(nsubj,ntrl);
end
behavdat.llr = zeros(nsubj,ntrl); % log-likelihood ratios from optimal model

% loop over all subjects
for isubj = 1:nsubj
    % locate and load experiment data and fit
    fname = sprintf( ...
        'data/fulldata_213/fulldata_complete%d_%s.mat', ...
        fbtype == 2,behavdat.pid{isubj});
    dat = load(fname);
    
    %%% Get best fitting trajectories
    
    nb = max(dat.blocks); % number of blocks
    nt = nnz(dat.blocks == 1); % number of trials per block
    
    params = fits.params(isubj,:);
    
    % set fitting parameters
    cfg.nstype = 'weber'; % noise type (set to weber for our candidate model)
    cfg.chrule = 'softm'; % choice rule (set to softmax for our candidate model)
    cfg.fitalgo = 'vbmc'; % fitting algorithm
    cfg.isnoisy = 1; % noisy
    cfg.alpha = params(1); % learning rate
    if fbtype == 1
        cfg.delta = params(2); % drift variance
    end
    cfg.zeta = params(end-1); % learning noise
    cfg.tau = params(end); % policy temperature
    cfg.nsmp = 1e3; % number of samples used by particle filter
    cfg.nres = 1e2; % number of bootstrap/validation resamples
    cfg.nrun = 10;  % number of random starting points for initialization fit
    cfg.trl  = repmat((1:nt)',[nb,1]); % trial number in current block
    cfg.resp = dat.actions(:); % response
    cfg.rt   = cat(2,dat.reward_1(:),dat.reward_2(:))/100; % reward samples
    cfg.fbtype  = fbtype;
    cfg.verbose = 0;
    cfg.noprior = 0;
    %cfg.run_bs = 1;
    
    out = fit_noisyKF(cfg);
    behavdat.QA(isubj,:) = out.mt(:,1);
    behavdat.QB(isubj,:) = out.mt(:,2);
    behavdat.QA_wn(isubj,:) = out.ut(:,1);
    behavdat.QB_wn(isubj,:) = out.ut(:,2);
    
    %%% Get other behavioural indices
    for j = 1:fits.ntrl
        if dat.actions(j) == 1
            behavdat.reward(isubj,j) = dat.reward_1(j);
        elseif dat.actions(j) == 2
            behavdat.reward(isubj,j) = dat.reward_2(j);
        end
    end
    
    for j = 2:ntrl
        if dat.actions(j) ~= dat.actions(j-1)
            behavdat.switches(isubj,j) = 1;
        else
            behavdat.switches(isubj,j) = 0;
        end
    end
        
    [~,~,optQ,~,corresp,behavdat.llr(isubj,:),PE] = simulate_agents('optim',dat,fbtype); % simulate an optimal agent to get 'maximizing actions' to which to compare 'correct actions'
    behavdat.correct(isubj,:) = dat.actions == corresp';
    behavdat.optQA(isubj,:) = optQ(:,1);
    behavdat.optQB(isubj,:) = optQ(:,2);
    if fbtype == 1
        behavdat.PE(isubj,:) = PE;
    else
        behavdat.PEA(isubj,:) = PE(:,1);
        behavdat.PEB(isubj,:) = PE(:,2);
    end
    
    %[~,~,maxQ,~,maxresp] = simulate_agents('nn_argmax',dat,fbtype,params); % simulate a no noise argmax agent to get 'maximizing actions' to which to compare 'nongreedy actions'
    behavdat.nongreedy(isubj,:) = dat.actions ~= corresp';
    
    behavdat.noisy(isubj) = out.pngx;
    
    behavdat.rt(isubj,:) = dat.rt;
    behavdat.reward_1(isubj,:) = dat.reward_1;
    behavdat.reward_2(isubj,:) = dat.reward_2;
    behavdat.actions(isubj,:) = dat.actions;
    
    behavdat.exc = fits.exc;

    disp(isubj)
    
end

% save
fname = sprintf('behavdat_fb%d_21.mat',fbtype);
save(fname,'behavdat')
