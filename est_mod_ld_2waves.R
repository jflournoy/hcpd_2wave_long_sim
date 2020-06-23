## @knitr latent_differences_2wave

seed <- 319 #random.org

library(lavaan)
library(dplyr)
library(tidyr)
d <- readRDS('sim_data.rds')
fname <- 'fit_2waves'

if(!file.exists('d2wave.rds')){
    d2wave <- do(group_by(d, id), 
                 {
                     i <- sample(1:dim(.)[1], size = 1)
                     if( (i + 10) > dim(.)[1]){
                         i2 <- i - 10
                     } else {
                         i2 <- i + 10
                     }
                     .[c(i, i2), ]
                 })
    saveRDS(d2wave, 'd2wave.rds')
} else {
    d2wave <- readRDS('d2wave.rds')
}

d2wave_w <- arrange(d2wave, id, age) %>%
    group_by(id) %>%
    mutate(wave = paste0('w', 1:2)) %>%
    gather(key = 'key', value = 'value', -id, -wave) %>%
    unite(key, key, wave) %>%
    spread(key, value)

measurement_model <- '
#factors
y1_w1 =~ 1*y1_a_w1 + y1b*y1_b_w1 + y1c*y1_c_w1
y1_w2 =~ 1*y1_a_w2 + y1b*y1_b_w2 + y1c*y1_c_w2
y2_w1 =~ 1*y2_a_w1 + y2b*y2_b_w1 + y2c*y2_c_w1
y2_w2 =~ 1*y2_a_w2 + y2b*y2_b_w2 + y2c*y2_c_w2

#residuals and residual covariance
y1_a_w1 ~~ ry1a*y1_a_w1 + y1_a_w2
y1_b_w1 ~~ ry1b*y1_b_w1 + y1_b_w2
y1_c_w1 ~~ ry1c*y1_c_w1 + y1_c_w2
y1_a_w2 ~~ ry1a*y1_a_w2
y1_b_w2 ~~ ry1b*y1_b_w2
y1_c_w2 ~~ ry1c*y1_c_w2
y2_a_w1 ~~ ry2a*y2_a_w1 + y2_a_w2
y2_b_w1 ~~ ry2b*y2_b_w1 + y2_b_w2
y2_c_w1 ~~ ry2c*y2_c_w1 + y2_c_w2
y2_a_w2 ~~ ry2a*y2_a_w2
y2_b_w2 ~~ ry2b*y2_b_w2
y2_c_w2 ~~ ry2c*y2_c_w2

#intercepts
y1_a_w1 ~ 0*1
y1_b_w1 ~ iy1b*1
y1_c_w1 ~ iy1c*1
y1_a_w2 ~ 0*1
y1_b_w2 ~ iy1b*1
y1_c_w2 ~ iy1c*1
y2_a_w1 ~ 0*1
y2_b_w1 ~ iy2b*1
y2_c_w1 ~ iy2c*1
y2_a_w2 ~ 0*1
y2_b_w2 ~ iy2b*1
y2_c_w2 ~ iy2c*1
'

measurement_only_extras <- '
#intercepts
y1_w1 ~ 1
y1_w2 ~ 1
y2_w1 ~ 1
y2_w2 ~ 1

# lv variances
y1_w1 ~~ y1_w1
y1_w2 ~~ y1_w2
y2_w1 ~~ y2_w1
y2_w2 ~~ y2_w2

# lv covvariances
y1_w1 ~~ y1_w2 + y2_w1 + y2_w2 
y1_w2 ~~ y2_w1 + y2_w2 
y2_w1 ~~ y2_w2 
'

ld_model <- '
#fix regression of y_w2 on y_w1
y1_w2 ~ 1*y1_w1
y2_w2 ~ 1*y2_w1

#fix regression of delta_y on y_w2
delta_y1 =~ 1*y1_w2
delta_y2 =~ 1*y2_w2

#constrain intercept of y_w2 to 0
y1_w2 ~ 0*1
y2_w2 ~ 0*1

#fix variance of y_w2 to 0
y1_w2 ~~ 0*y1_w2
y2_w2 ~~ 0*y2_w2

#estimate intercept for delta_y
delta_y1 ~ 1
delta_y2 ~ 1

#estimate intercept of y_w1
y1_w1 ~ 1
y2_w1 ~ 1

#estimate variance of delta_y
delta_y1 ~~ delta_y1 
delta_y2 ~~ delta_y2

#estimate variance of y_w1
y1_w1 ~~ y1_w1
y2_w1 ~~ y2_w1

#estimate self-feedback parameter
delta_y1 ~ y1_w1
delta_y2 ~ y2_w1

#Bivariate parameters
## coupling parameters
delta_y1 ~ y2_w1
delta_y2 ~ y1_w1

## covariances
y1_w1 ~~ y2_w1
delta_y1 ~~ delta_y2
'

ld_df <- as.data.frame(select(d2wave_w, -y1_w1, -y1_w2, -y2_w1, -y2_w2))

measurement_only <- paste(c(measurement_model, measurement_only_extras), collapse = '\n')
fitMeasurement <- lavaan(measurement_only, data = ld_df, 
                         estimator = 'mlr', missing = 'fiml',
                         fixed.x = FALSE)

LCS <- paste(c(measurement_model, ld_model), collapse = '\n')

fitLCS <- lavaan(LCS, data = ld_df, 
                 estimator = 'mlr', missing = 'fiml',
                 fixed.x = FALSE)
summary(fitLCS, stan = TRUE)


fitLCS75 <- lavaan(LCS, data = ld_df[sample(1:dim(ld_df)[1], size = 75), ], 
                 estimator = 'mlr', missing = 'fiml',
                 fixed.x = FALSE)
summary(fitLCS75, stan = TRUE)
