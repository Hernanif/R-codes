## Function to estimate if network is hierarchical
## by Borja Esteve-Altava, April 2015
## Requires package "igraph"
# Input: an igraph object, cumulative Pk TRUE/FALSE
# Outputs: TRUE (r > 0.7) / FALSE, alpha and r

is.hierarchy = function(graph, cumulative = c(TRUE, FALSE)){
        d <- degree(graph, mode = "all")
        dd = degree.distribution(graph, mode = "all", cumulative = cumulative)
        
        # initialize vectors
        probability = dd[-1]
        degree = 1:max(d)
        # delete blank values
        nonzero.position = which(probability != 0)
        probability = probability[nonzero.position]
        degree = degree[nonzero.position]
        # linear fit of logarithms
        reg = lm(log(probability) ~ log(degree))
        cozf = coef(reg)
        power.law.fit = function(x) exp(cozf[[1]] + cozf[[2]] * log(x))
        
        alpha1 = -cozf[[2]]
        R.square1 = summary(reg)$r.squared
        
        # Fit C(k) to power-law
        cluster = transitivity(graph, type = "local", isolates = "NaN")
        for (i in 1:max(d)) {probability[i] = mean(cluster[d==i], na.rm=TRUE)}	
        # delete blank values
        nonzero.position = which(probability != 0)
        probability = probability[nonzero.position]
        degree = degree[nonzero.position]
        # linear fit of logarithms
        reg = lm(log(probability) ~ log(degree))
        cozf = coef(reg)
        power.law.fit = function(x) exp(cozf[[1]] + cozf[[2]] * log(x))
        
        alpha2 = -cozf[[2]]
        R.square2 = summary(reg)$r.squared
        
        
        print(paste("Is hierarchical?", (R.square1>0.7)&&(R.square2>0.7)))
        
        print("P(k) fit to Power-Law")
        print(paste("Alpha =", round(alpha1, 3)))
        print(paste("R square =", round(R.square1, 3)))
        
        print("C(k) fit to Power-Law")
        print(paste("Alpha =", round(alpha2, 3)))
        print(paste("R square =", round(R.square2, 3)))

}