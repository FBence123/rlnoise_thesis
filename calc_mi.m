function [mi] = calc_mi(actions)
% function to calculate mutual information from a sequence of actions in
% the task
n = size(actions,2);
A_prob = sum(actions == 1)/n;
B_prob = sum(actions == 2)/n;
AA_count = 0;
BB_count = 0;
AB_count = 0;
BA_count = 0;
for t = 1:(n-1)
    if actions(t) == 1 && actions(t) == actions(t+1)
        AA_count = AA_count+1;
    elseif actions(t) == 2 && actions(t) == actions(t+1)
        BB_count = BB_count+1;
    elseif actions(t) == 1 && actions(t) ~= actions(t+1)
        AB_count = AB_count+1;
    elseif actions(t) == 2 && actions(t) ~= actions(t+1)
        BA_count = BA_count+1;
    end
end

AA_prob = AA_count/(n-1);
BB_prob = BB_count/(n-1);
AB_prob = AB_count/(n-1);
BA_prob = BA_count/(n-1);

AA_mi = AA_prob*log(AA_prob/(A_prob*A_prob));
BB_mi = BB_prob*log(BB_prob/(B_prob*B_prob));
AB_mi = AB_prob*log(AB_prob/(A_prob*B_prob));
BA_mi = BA_prob*log(BA_prob/(B_prob*A_prob));

mi = AA_mi + BB_mi + AB_mi + BA_mi;
end