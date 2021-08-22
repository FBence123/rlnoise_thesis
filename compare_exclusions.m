%% List different task based exclusion criteria

% load data
fbtype = 2; % Set to 1 for partial, 2 for complete feedback condition
load('/data/fulldata_213_pid.mat','idx');
pid = idx;
clear('idx');
nsubj = size(pid,1); % total number of subjects

fname = sprintf('taskdat_fb%d_21.mat',fbtype);
load(fname)

ll_chance = zeros(nsubj,1);
cor_chance = zeros(nsubj,1);
var = zeros(nsubj,1);
rew = zeros(nsubj,1);

% loop over all subjects
rew_trl = zeros(1,144);
for isubj = 1:nsubj

    fname = sprintf('/data/fulldata_213/fulldata_complete%d_%s.mat', ...
        fbtype == 2,pid(isubj,:));
    bdat = load(fname);
    
    % chance level log likelihood
    lnul = log(0.5)*fits.ntrl;
    pnul = 1./(1+exp(-(lnul-fits.ll(isubj))));
    ll_chance(isubj) = pnul > 0.05;
    
    % chance level correct response
    [~,~,~,~,maxresp] = simulate_agents('optim',bdat,fbtype); % simulate an optimal agent to get 'maximizing actions' to which to compare 'exploratory actions'
    corr = sum(bdat.actions == maxresp');
    cor_chance(isubj) = corr < 83; % for a task with 144 trials, a one tailed binomial test would reveal performance better than chance at 83 or above correct trials
    
    % choice variability too low
    freq = sum(bdat.actions == mode(bdat.actions))/fits.ntrl; % fraction of times the most frequent response was used
    var(isubj) = freq > 0.7;
    
    % obtained rewards
    for j = 1:144
        if bdat.actions(j) == 1
            rew_trl(j) = bdat.reward_1(j);
        elseif bdat.actions(j) == 2
            rew_trl(j) = bdat.reward_2(j);
        end
    end
    rew(isubj) = sum(rew_trl);
    disp(isubj)
end

rew_mad = mad(rew,1);
rew_bad = rew <= (median(rew) - 3*rew_mad);

param_mad = mad(fits.params,1);
zeta_bad = fits.params(:,end-1) >= (median(fits.params(:,end-1)) + 3*param_mad(end-1));
tau_bad = fits.params(:,end) >= (median(fits.params(:,end)) + 3*param_mad(end));
param_bad = zeta_bad | tau_bad;

list_exc = table(pid,ll_chance,cor_chance,var,rew_bad,param_bad);

% save files
if fbtype == 1
    writetable(list_exc,'partial_task_exc.csv')
else
    writetable(list_exc,'complete_task_exc.csv')
end