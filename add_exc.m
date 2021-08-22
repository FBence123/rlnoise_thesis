%% Add exclusion indices to database

fbtype = 2;
mods = {'21'};
nmod = size(mods,2);
mods_name = {'noisy_softmax'};

for imod = 1:nmod
    fname = sprintf('taskdat_fb%d_%s.mat',fbtype,mods{imod});
    load(fname)

    % exclude subjects based on task and questionnaire criteria
    any_outliers = readtable('Bence_list_outliers.csv');
    any_outliers = table2array(any_outliers(:,18));
    any_outliers = any_outliers == 0;

    fits.exc = any_outliers;
    outname = sprintf('taskdat_fb%d_%s.mat',fbtype,mods{imod});
    save(outname,'fits')
end