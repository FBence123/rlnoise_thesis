%% This script collects the fitted parameters and fit indices of all subjects

% define the different fitted models
mods = {'01' '20' '21'};
nmod = size(mods,2);
mods_name = {'exact_softmax' 'noisy_argmax' 'noisy_softmax'};
fbtype = 2; % Set to 1 for partial, 2 for complete feedback condition

% load fits
for imod = 1:nmod
    fname = sprintf('/fits/fit_noisyKF_fb%d_fit%s_vbmc.mat',fbtype,mods{imod});
    load(fname);
    nsubj = size(out_fit,1);
    if fbtype == 1 && imod == 1
        fits.params = zeros(nsubj,3);
        fits.std = zeros(nsubj,3);
    elseif fbtype == 1 && imod == 2
        fits.params = zeros(nsubj,3);
        fits.std = zeros(nsubj,3);
    elseif fbtype == 1 && imod == 3
        fits.params = zeros(nsubj,4);
        fits.std = zeros(nsubj,4);
    elseif fbtype == 2 && imod == 1
        fits.params = zeros(nsubj,2);
        fits.std = zeros(nsubj,2);
    elseif fbtype == 2 && imod == 2
        fits.params = zeros(nsubj,2);
        fits.std = zeros(nsubj,2);
    else
        fits.params = zeros(nsubj,3);
        fits.std = zeros(nsubj,3);
    end
    fits.ll = zeros(nsubj,1);
    fits.lp = zeros(nsubj,1);
    fits.elbo = zeros(nsubj,1);
    fits.pid = pid;
    for isubj = 1:nsubj
        fits.params(isubj,:) = out_fit{isubj}.xavg;
        fits.std(isubj,:) = out_fit{isubj}.xstd;
        fits.ll(isubj) = out_fit{isubj}.ll;
        fits.lp(isubj) = out_fit{isubj}.lp;
        fits.elbo(isubj) = out_fit{isubj}.elbo;
    end
    fits.ntrl = out_fit{1}.ntrl;
    % save fits
    outname = sprintf('taskdat_fb%d_%s.mat',fbtype,mods{imod});
    save(outname,'fits')
end
