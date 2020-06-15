## @knitr slurm_stuff1
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
    task_id <- 1
    cpus_per_task <- 4
    message('Not running on SLURM system')
    warmup <- 200
    iter <- 500
} else {
    task_id <- as.numeric(Sys.getenv('SLURM_ARRAY_TASK_ID'))
    cpus_per_task <- as.numeric(Sys.getenv('SLURM_CPUS_PER_TASK'))
    warmup <- 2000
    iter <- 7500
    message('Running on SLURM system')
}

seed <- 188 + task_id #random.org

## @knitr est_mod_full_data
library(brms)
d <- readRDS('sim_data.rds')
fname <- 'fit_full_data'

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

fit <- brm(models, data = d, 
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
