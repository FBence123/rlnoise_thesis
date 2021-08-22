%% Script to create data for choice sensitivity analysis

fbtype = 1; % Set to 1 for partial, 2 for complete feedback condition
if fbtype == 1
    load('behavdat_fb1_21.mat');
    mod01 = load('taskdat_fb1_01.mat');
    mod20 = load('taskdat_fb1_20.mat');
    mod21 = load('taskdat_fb1_21.mat');
else
    load('behavdat_fb2_21.mat');
    mod01 = load('taskdat_fb2_01.mat');
    mod20 = load('taskdat_fb2_20.mat');
    mod21 = load('taskdat_fb2_21.mat');
end

behavdat.pid = behavdat.pid(behavdat.exc,:);
mod01.fits.params = mod01.fits.params(behavdat.exc,:);
mod20.fits.params = mod20.fits.params(behavdat.exc,:);
mod21.fits.params = mod21.fits.params(behavdat.exc,:);

% calculate and save relevant quantities for all subjects and models
sens_data.human.actions = behavdat.actions(behavdat.exc,:);    % actions
nsubj = size(sens_data.human.actions,1);
ntrial = size(sens_data.human.actions,2);
sens_data.human.PE = behavdat.PE(behavdat.exc,:);              % prediction errors (calculated from the noisy Q values)
sens_data.human.llr = behavdat.llr(behavdat.exc,:);            % log likelihood ratio

sens_data.mod01.actions = zeros(nsubj,ntrial);
sens_data.mod01.PE = zeros(nsubj,ntrial);
sens_data.mod01.llr = zeros(nsubj,ntrial);

sens_data.mod20.actions = zeros(nsubj,ntrial);
sens_data.mod20.PE = zeros(nsubj,ntrial);
sens_data.mod20.llr = zeros(nsubj,ntrial);

sens_data.mod21.actions = zeros(nsubj,ntrial);
sens_data.mod21.PE = zeros(nsubj,ntrial);
sens_data.mod21.llr = zeros(nsubj,ntrial);

% loop over all subjects
for i = 1:nsubj
    fname = sprintf('/data/fulldata_213/fulldata_complete%d_%s.mat', ...
                fbtype == 2, behavdat.pid{i});
    dat = load(fname);
    [~,~,~,~,sens_data.mod01.actions(i,:),~,~] = simulate_agents('exact_softmax',dat,fbtype,mod01.fits.params(i,:));
    [~,~,~,~,sens_data.mod20.actions(i,:),~,~] = simulate_agents('weber_argmax',dat,fbtype,mod20.fits.params(i,:));
    [~,~,~,~,sens_data.mod21.actions(i,:),~,~] = simulate_agents('weber_softmax',dat,fbtype,mod21.fits.params(i,:));
    
    % Mod 01
    nb = max(dat.blocks); % number of blocks
    nt = nnz(dat.blocks == 1); % number of trials per block
    cfg1.nstype = 'weber'; % noise type (set to weber for our candidate model)
    cfg1.chrule = 'softm'; % choice rule (set to softmax for our candidate model)
    cfg1.alpha = mod01.fits.params(i,1); % learning rate
    cfg1.delta = mod01.fits.params(i,2); % drift variance
    cfg1.zeta = 0; % learning noise
    cfg1.tau = mod01.fits.params(i,3); % policy temperature
    cfg1.nsmp = 1e3; % number of samples used by particle filter
    cfg1.nres = 1e2; % number of bootstrap/validation resamples
    cfg1.nrun = 10;  % number of random starting points for initialization fit
    cfg1.trl  = repmat((1:nt)',[nb,1]); % trial number in current block
    cfg1.resp = sens_data.mod01.actions(i,:)'; % response
    cfg1.rt   = cat(2,dat.reward_1(:),dat.reward_2(:))/100; % reward samples
    cfg1.fbtype  = fbtype;
    cfg1.verbose = 0;
    cfg1.noprior = 0;
    %cfg1.run_bs = 1;
    
    out = fit_noisyKF(cfg1);
    et = sqrt(nansum(out.et.^2,2));
    sens_data.mod01.PE(i,:) = et;
    sens_data.mod01.llr(i,:) = out.llr;
    sens_data.mod01.actions(i,:) = cfg1.resp;
    
    % Mod 20
    nb = max(dat.blocks); % number of blocks
    nt = nnz(dat.blocks == 1); % number of trials per block
    cfg2.nstype = 'weber'; % noise type (set to weber for our candidate model)
    cfg2.chrule = 'softm'; % choice rule (set to softmax for our candidate model)
    cfg2.alpha = mod20.fits.params(i,1); % learning rate
    cfg2.delta = mod20.fits.params(i,2); % drift variance
    cfg2.zeta = mod20.fits.params(i,3); % learning noise
    cfg2.tau = 0; % policy temperature
    cfg2.nsmp = 1e3; % number of samples used by particle filter
    cfg2.nres = 1e2; % number of bootstrap/validation resamples
    cfg2.nrun = 10;  % number of random starting points for initialization fit
    cfg2.trl  = repmat((1:nt)',[nb,1]); % trial number in current block
    cfg2.resp = sens_data.mod20.actions(i,:)'; % response
    cfg2.rt   = cat(2,dat.reward_1(:),dat.reward_2(:))/100; % reward samples
    cfg2.fbtype  = fbtype;
    cfg2.verbose = 0;
    cfg2.noprior = 0;
    %cfg2.run_bs = 1;
    
    out = fit_noisyKF(cfg2);
    et = sqrt(nansum(out.et.^2,2));
    sens_data.mod20.PE(i,:) = et;
    sens_data.mod20.llr(i,:) = out.llr;
    sens_data.mod20.actions(i,:) = cfg2.resp;
    
    % Mod 21
    nb = max(dat.blocks); % number of blocks
    nt = nnz(dat.blocks == 1); % number of trials per block
    cfg3.nstype = 'weber'; % noise type (set to weber for our candidate model)
    cfg3.chrule = 'softm'; % choice rule (set to softmax for our candidate model)
    cfg3.alpha = mod21.fits.params(i,1); % learning rate
    cfg3.delta = mod21.fits.params(i,2); % drift variance
    cfg3.zeta = mod21.fits.params(i,3); % learning noise
    cfg3.tau = mod21.fits.params(i,4); % policy temperature
    cfg3.nsmp = 1e3; % number of samples used by particle filter
    cfg3.nres = 1e2; % number of bootstrap/validation resamples
    cfg3.nrun = 10;  % number of random starting points for initialization fit
    cfg3.trl  = repmat((1:nt)',[nb,1]); % trial number in current block
    cfg3.resp = sens_data.mod21.actions(i,:)'; % response
    cfg3.rt   = cat(2,dat.reward_1(:),dat.reward_2(:))/100; % reward samples
    cfg3.fbtype  = fbtype;
    cfg3.verbose = 0;
    cfg3.noprior = 0;
    %cfg3.run_bs = 1;
    
    out = fit_noisyKF(cfg3);
    et = sqrt(nansum(out.et.^2,2));
    sens_data.mod21.PE(i,:) = et;
    sens_data.mod21.llr(i,:) = out.llr;
    sens_data.mod21.actions(i,:) = cfg3.resp;
    disp(i)
end


% save to mat file
if fbtype == 1
    save('partial_sens_data.mat','sens_data')
else
    save('complete_sens_data.mat','sens_data')
end
