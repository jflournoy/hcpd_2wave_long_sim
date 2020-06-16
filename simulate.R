## @knitr simulate_data

#Simulate data with correlated intercepts and slopes across two linear growth
#models. We first simulate a dense growth model and then sample from it to match
#the data we have.

# y1 ~ 1 + age + (1 + age | id)
# y2 ~ 1 + age + (1 + age | id)
#
# y1 = p0 + p1 * age + sigma1
# y2 = q0 + q2 * age + sigma2
#
# p0 = b00 + I_y1
# p1 = b10 + S_y1
# q0 = g00 + I_y2
# q1 = g10 + S_y2
#
#       I_y1 S_y1 I_y2 S_y2 
# I_y1 [i1i1               ]
# S_y1 [i1s1 s1s1          ]
# I_y2 [i1i2 s1i2 i2i2     ]
# S_y2 [i1s2 s1s2 i2s2 s2s2]
#

if(!file.exists('sim_data.rds')){
    set.seed(393) #random.org
    #Number of subjects
    N = 200
    #Number of timepoints
    nT = 50
    #Age range
    AgeRange = c(9,14)
    b00 <- 0
    b10 <- .3
    g00 <- 0
    g10 <- .3
    sigma1 <- sigma2 <- 1
    sigma.i1 = 3
    sigma.s1 = 1 
    sigma.i2 = 3
    sigma.s2 = 1
    #         i1s1  i1i2  i1s2  s1i2  s1s2  i2s2
    rho.v = c(-.20,   .2, -.05, -.05,  .30, -.20)
    rho.v.mat <- matrix(c(1, rho.v[1:3], rho.v[1], 1, rho.v[4:5], rho.v[c(2,4)], 1, rho.v[6], rho.v[c(3,5,6)], 1),
                        ncol = 4)
    
    #Ages at timepoints
    age = seq(AgeRange[[1]], AgeRange[[2]], length.out = nT)
    #Introduce jitter
    ageN <- lapply(1:N, function(i) age + runif(nT, -diff(AgeRange)/(nT-1)/3, diff(AgeRange)/(nT-1)/3))
    d <- data.frame(id = rep(1:N, each = nT), age = unlist(ageN))
    d$age_c <- d$age - 11.5
    
    # library(ggplot2)
    # ggplot(d, aes(x = age, y = id, group = id))+
    #     geom_line(alpha = .5) +
    #     geom_point(size = .25)
    
    #Level-1 error, no autoregressive effect
    E1 = rnorm(N*nT, 0, sigma1)
    E1 = rnorm(N*nT, 0, sigma2)
    
    # Simulate error level-2
    # Simulate between-subject random effect
    Sigma.v = diag(c(sigma.i1, sigma.s1, sigma.i2, sigma.s2))
    dimnames(Sigma.v) <- list(c('sigma.i1',
                                'sigma.s1',
                                'sigma.i2',
                                'sigma.s2'),
                              c('sigma.i1',
                                'sigma.s1',
                                'sigma.i2',
                                'sigma.s2'))
    Sigma.v <- Sigma.v %*% rho.v.mat %*% Sigma.v
    
    #positive definite?
    all(eigen(rho.v.mat)$values > 0)
    all(eigen(Sigma.v)$values > 0)
    
    V.i = MASS::mvrnorm(N, rep(0,ncol(Sigma.v)), Sigma.v)
    V = matrix(0, nT*N, ncol(Sigma.v))
    for (i in 1:N){
        V[which(d$id==i),1] = V.i[i,1]
        V[which(d$id==i),2] = V.i[i,2]
        V[which(d$id==i),3] = V.i[i,3]
        V[which(d$id==i),4] = V.i[i,4]
    }
    
    #Set parameters
    B00 <- rep(b00, nrow(d))
    B10 <- rep(b10, nrow(d))
    G00 <- rep(g00, nrow(d))
    G10 <- rep(g10, nrow(d))
    
    d$y1 <- B00 + B10 * d$age_c + V[,1] + V[,2] * d$age_c + E1
    d$y2 <- G00 + G10 * d$age_c + V[,3] + V[,4] * d$age_c + E1
    
    # library(ggplot2)
    # ggplot(d, aes(x = age, y = y1, group = id))+
    #     # geom_line(stat = 'smooth', method = 'lm', alpha = .25) +
    #     geom_line(alpha = .25) +
    #     geom_point(size = .25)
    # ggplot(d, aes(x = age, y = y2, group = id))+
    #     # geom_line(stat = 'smooth', method = 'lm', alpha = .25) +
    #     geom_line(alpha = .25) +
    #     geom_point(size = .25)
    
    loadings <- runif(6, .7, .85)
    #Create 3 indicators for y1 and y2
    d$y1_a <- loadings[[1]] * d$y1 + rnorm(dim(d)[1])
    d$y1_b <- loadings[[2]] * d$y1 + rnorm(dim(d)[1])
    d$y1_c <- loadings[[3]] * d$y1 + rnorm(dim(d)[1])
    d$y2_a <- loadings[[4]] * d$y2 + rnorm(dim(d)[1])
    d$y2_b <- loadings[[5]] * d$y2 + rnorm(dim(d)[1])
    d$y2_c <- loadings[[6]] * d$y2 + rnorm(dim(d)[1])
    
    saveRDS(d, file = 'sim_data.rds')
} else {
    d <- readRDS('sim_data.rds')
}
