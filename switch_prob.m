%% This script sets up and preprocesses the behavioural data for further analysis
fbtype = 2; % Set to 1 for partial, 2 for complete feedback condition
if fbtype == 1
    load('behavdat_fb1_21.mat')
else
    load('behavdat_fb2_21.mat')
end

excdat.reward = behavdat.reward(behavdat.exc,:);
excdat.actions = behavdat.actions(behavdat.exc,:);
nsubj = size(excdat.actions,1);
ntrl = size(excdat.actions,2);

% calculate switch probabilities
excdat.reward = (3-2*excdat.actions).*(excdat.reward - 50);
excdat.actions = excdat.actions == 1;
B = quantile(excdat.reward(:),4);
[N,~,idx] = histcounts(excdat.reward,[-Inf B Inf]);
V = accumarray(idx(:),excdat.reward(:),[],@mean);

switch_prob_t_1 = zeros(nsubj,5);
idx_1 = zeros(size(idx));
idx_1(:,2:end) = idx(:,1:end-1);
for i = 1:nsubj
    n_i = histcounts(idx_1(i,[2:72 74:end]));
    switch_prob_t_1(i,:) = (accumarray(idx_1(i,[2:72 74:end])',excdat.actions(i,[2:72 74:end]),[],@(x) sum(x)))';
    switch_prob_t_1(i,:) = switch_prob_t_1(i,:)./n_i;
end

switch_prob_t_2 = zeros(nsubj,5);
idx_2 = zeros(size(idx));
idx_2(:,3:end) = idx(:,1:end-2);
for i = 1:nsubj
    n_i = histcounts(idx_1(i,[3:72 75:end]));
    switch_prob_t_2(i,:) = (accumarray(idx_2(i,[3:72 75:end])',excdat.actions(i,[3:72 75:end]),[],@(x) sum(x)))';
    switch_prob_t_2(i,:) = switch_prob_t_2(i,:)./n_i;
end

switch_prob_t_3 = zeros(nsubj,5);
idx_3 = zeros(size(idx));
idx_3(:,4:end) = idx(:,1:end-3);
for i = 1:nsubj
    n_i = histcounts(idx_1(i,[4:72 76:end]));
    switch_prob_t_3(i,:) = (accumarray(idx_3(i,[4:72 76:end])',excdat.actions(i,[4:72 76:end]),[],@(x) sum(x)))';
    switch_prob_t_3(i,:) = switch_prob_t_3(i,:)./n_i;
end

switch_prob_t_4 = zeros(nsubj,5);
idx_4 = zeros(size(idx));
idx_4(:,5:end) = idx(:,1:end-4);
for i = 1:nsubj
    n_i = histcounts(idx_1(i,[5:72 77:end]));
    switch_prob_t_4(i,:) = (accumarray(idx_4(i,[5:72 77:end])',excdat.actions(i,[5:72 77:end]),[],@(x) sum(x)))';
    switch_prob_t_4(i,:) = switch_prob_t_4(i,:)./n_i;
end

switch_prob_t_5 = zeros(nsubj,5);
idx_5 = zeros(size(idx));
idx_5(:,6:end) = idx(:,1:end-5);
for i = 1:nsubj
    n_i = histcounts(idx_1(i,[6:72 78:end]));
    switch_prob_t_5(i,:) = (accumarray(idx_5(i,[6:72 78:end])',excdat.actions(i,[6:72 78:end]),[],@(x) sum(x)))';
    switch_prob_t_5(i,:) = switch_prob_t_5(i,:)./n_i;
end

switch_prob_t_6 = zeros(nsubj,5);
idx_6 = zeros(size(idx));
idx_6(:,7:end) = idx(:,1:end-6);
for i = 1:nsubj
    n_i = histcounts(idx_1(i,[7:72 79:end]));
    switch_prob_t_6(i,:) = (accumarray(idx_6(i,[7:72 79:end])',excdat.actions(i,[7:72 79:end]),[],@(x) sum(x)))';
    switch_prob_t_6(i,:) = switch_prob_t_6(i,:)./n_i;
end

% save to mat files
if fbtype == 1
    save('partial_switch_prob_t_1.mat','switch_prob_t_1')
    save('partial_switch_prob_t_2.mat','switch_prob_t_2')
    save('partial_switch_prob_t_3.mat','switch_prob_t_3')
    save('partial_switch_prob_t_4.mat','switch_prob_t_4')
    save('partial_switch_prob_t_5.mat','switch_prob_t_5')
    save('partial_switch_prob_t_6.mat','switch_prob_t_6')
else
    save('complete_switch_prob_t_1.mat','switch_prob_t_1')
    save('complete_switch_prob_t_2.mat','switch_prob_t_2')
    save('complete_switch_prob_t_3.mat','switch_prob_t_3')
    save('complete_switch_prob_t_4.mat','switch_prob_t_4')
    save('complete_switch_prob_t_5.mat','switch_prob_t_5')
    save('complete_switch_prob_t_6.mat','switch_prob_t_6')
end
