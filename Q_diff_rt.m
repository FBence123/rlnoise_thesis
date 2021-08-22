%% Calculate RTs and PCorr for 5 bins of Q value differences

fbtype = 1; % Set to 1 for partial, 2 for complete feedback condition
if fbtype == 1
    load('behavdat_fb1_21.mat')
else
    load('behavdat_fb2_21.mat')
end

excdat.QA = behavdat.QA_wn(behavdat.exc,[2:72,74:end]);
excdat.QB = behavdat.QB_wn(behavdat.exc,[2:72,74:end]);
excdat.rt = behavdat.rt(behavdat.exc,[2:72,74:end]);
excdat.corr = behavdat.correct(behavdat.exc,[2:72,74:end]);
nsubj = size(excdat.rt,1);
ntrl = size(excdat.rt,2);

Q_diff = excdat.QA - excdat.QB;

B = quantile(Q_diff(:),6);
[N,~,idx] = histcounts(Q_diff,[-Inf B Inf]);
Q_vals = accumarray(idx(:),Q_diff(:),[],@mean);

Q_rt = zeros(nsubj,size(Q_vals,1));
for i = 1:nsubj
    for j = 1:size(N,2)
        Q_rt(i,j) = mean(excdat.rt(i,idx(i,:) == j));
    end
end

Q_corr = zeros(nsubj,size(Q_vals,1));
for i = 1:nsubj
    for j = 1:size(N,2)
        Q_corr(i,j) = sum(excdat.corr(i,idx(i,:) == j));
    end
    Q_corr(i,:) = Q_corr(i,:)./histcounts(idx(i,:),[0.5 1.5 2.5 3.5 4.5 5.5 6.5 7.5]);
end

% save mat file
if fbtype == 1
    save('partial_Q_rts','Q_rt')
    save('partial_Q_corrs','Q_corr')
else
    save('complete_Q_rts','Q_rt')
    save('complete_Q_corrs','Q_corr')
end
