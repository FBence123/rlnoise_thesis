function [a,b] = betapar(m,v)
% get parameters of beta distribution with mean m and variance v
v = min(max(v,1e-12),m.*(1-m));
c = m.*(1-m)./v-1;
a = max(c.*m,1);
b = max(c.*(1-m),1);
end