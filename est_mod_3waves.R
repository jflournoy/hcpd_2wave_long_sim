## @knitr slurm_stuff3
model_obj_dir <- 'model_obj'
model_text_dir <- 'model_text'
if(!dir.exists(model_obj_dir)){
    message('Creating directory: ', file.path(model_obj_dir))
    dir.create(model_obj_dir)
}
if(!dir.exists(model_text_dir)){
    message('Creating directory: ', file.path(model_text_dir))
    dir.create(model_text_dir)
}
if(Sys.getenv('HOME') != '/users/jflournoy'){
    cpus_per_task <- 4
    message('Not running on SLURM system')
    warmup <- 200
    iter <- 500
} else {
    cpus_per_task <- as.numeric(Sys.getenv('SLURM_CPUS_PER_TASK'))
    warmup <- 5000
    iter <- 10000
    message('Running on SLURM system')
}

seed <- 648 #random.org

## @knitr est_mod_3waves
library(brms)
library(dplyr)
d <- readRDS('sim_data.rds')
fname <- 'fit_3waves'

d2wave <- do(group_by(d, id), 
             {
                 i <- sample(1:dim(.)[1], size = 1)
                 if( (i + 10) > dim(.)[1] ){
                     i2 <- i - 10
		     i3 <- i - 20
                 } else {
		     if( (i + 20) > dim(.)[1] ) {
		         i3 <- i - 10
                     } else {
	                 i3 <- i + 20
                     }		     
                     i2 <- i + 10
                 }
                 .[c(i, i2, i3), ]
             })

# library(ggplot2)
# d2wave_id <- do(group_by(d2wave, id),
#                 {
#                     .[which(.$age == min(.$age)),]
#                 })
# d2wave$id_fac <- factor(d2wave$id, levels = d2wave_id$id[order(d2wave_id$age)])
# ggplot(d2wave, aes(x = age, y = id_fac, group = id))+
#     geom_line(alpha = .5) +
#     geom_point(size = .25)

priors <- c(set_prior('normal(0,2.5)', class = 'b', resp = 'y1', coef = 'age_c'),
            set_prior('normal(0,2.5)', class = 'b', resp = 'y2', coef = 'age_c'),
            set_prior("normal(0, 1)", class = "Intercept", resp = "y1"),
            set_prior("normal(0, 1)", class = "Intercept", resp = "y2"),
            set_prior("weibull(2, 1)", class = "sigma", resp = "y1"),
            set_prior("weibull(2, 1)", class = "sigma", resp = "y2"),
            set_prior("lkj(1)", class = "cor"),
            set_prior("lkj(1)", class = "rescor"),
            set_prior("cauchy(0,2.5)", class = "sd", resp = "y1"),
            set_prior("cauchy(0,2.5)", class = "sd", resp = "y2"))


models <- bf(y1 ~ 1 + age_c + (1 + age_c | ID1 | id)) + 
    bf(y2 ~ 1 + age_c + (1 + age_c | ID1 | id))

fit <- brm(models, data = d2wave, 
           family = gaussian(), 
           prior = priors, 
           chains = cpus_per_task, cores = cpus_per_task,
           seed = seed,
           warmup = warmup, iter = iter,
           control = list(adapt_delta = 0.999,max_treedepth = 20),
           save_model = file.path(model_text_dir, paste0(fname, '.stan')),
           save_dso = TRUE,
           file = file.path(model_obj_dir, fname),
           silent = TRUE, open_progress = FALSE)
