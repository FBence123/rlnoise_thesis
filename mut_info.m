%% Script to calculate mutual information of successive actions in human data and the noisy and exact models.

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
behavdat.actions = behavdat.actions(behavdat.exc,:);
mod01.fits.params = mod01.fits.params(behavdat.exc,:);
mod20.fits.params = mod20.fits.params(behavdat.exc,:);
mod21.fits.params = mod21.fits.params(behavdat.exc,:);
nsubj = size(behavdat.actions,1);
ntrial = size(behavdat.actions,2);

% Partial condition
human = zeros(nsubj,ntrial);
bmod01 = zeros(nsubj,ntrial);
bmod20 = zeros(nsubj,ntrial);
bmod21 = zeros(nsubj,ntrial);
maxresp = zeros(nsubj,ntrial);

for i = 1:nsubj
    fname = sprintf('/data/fulldata_213/fulldata_complete%d_%s.mat', ...
                fbtype == 2, behavdat.pid{i});
    dat = load(fname);
    human(i,:) = dat.actions;
    [~,~,~,~,bmod01(i,:)] = simulate_agents('exact_softmax',dat,fbtype,mod01.fits.params(i,:));
    [~,~,~,~,bmod20(i,:)] = simulate_agents('weber_argmax',dat,fbtype,mod20.fits.params(i,:));
    [~,~,~,~,bmod21(i,:)] = simulate_agents('weber_softmax',dat,fbtype,mod21.fits.params(i,:));
    [~,~,~,~,maxresp(i,:)] = simulate_agents('optim',dat,fbtype);
    disp(i)
end

frac_nongreedy = zeros(nsubj,4);
mi = zeros(nsubj,4);

for i = 1:nsubj
    mi(i,1) = calc_mi(human(i,:));
    mi(i,2) = calc_mi(bmod01(i,:));
    mi(i,3) = calc_mi(bmod20(i,:));
    mi(i,4) = calc_mi(bmod21(i,:));
    frac_nongreedy(i,1) = sum(human(i,:) ~= maxresp(i,:))/ntrial;
    frac_nongreedy(i,2) = sum(bmod01(i,:) ~= maxresp(i,:))/ntrial;
    frac_nongreedy(i,3) = sum(bmod20(i,:) ~= maxresp(i,:))/ntrial;
    frac_nongreedy(i,4) = sum(bmod21(i,:) ~= maxresp(i,:))/ntrial;
end

if fbtype == 1
    save('partial_mi.mat','mi')
    save('partial_frac_nongreedy.mat','frac_nongreedy')
else
    save('complete_mi.mat','mi')
    save('complete_frac_nongreedy.mat','frac_nongreedy')
end

%[~,~,Q,~,resp] = simulate_agents('weber_thompson',dat,fbtype,mod21.fits.params);
%[~,~,maxQ,~,mresp] = simulate_agents('nn_argmax',dat,fbtype,mod21.fits.params);