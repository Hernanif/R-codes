## Function to estimate if network is scale-free
## by Borja Esteve-Altava, April 2015
## Requires package "igraph"
# Input: an igraph object, cumulative TRUE/FALSE
# Outputs: TRUE (r > 0.7) / FALSE, alpha and r

is.scalefree = function(graph, cumulative = c(TRUE, FALSE)){
        d <- degree(graph, mode = "all")
        # Fit P(k) to power-law
        # initialize vectors
        dd = degree.distribution(graph, mode = "all", cumulative = cumulative)
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
        alpha = -cozf[[2]]
        R.square = summary(reg)$r.squared
        # output
        print(paste("Is scale-free?", R.square>0.7))
        print(paste("Alpha =", round(alpha, 3)))
        print(paste("R square =", round(R.square, 3)))
        dev.new()
        plot(probability ~ degree, log = "xy", xlab = "Connections (log)", ylab = "Probability (log)", col = 1, main = "Connectivity Distribution P(k)")
        curve(power.law.fit, col = "red", add = T, n = length(d)) 
}