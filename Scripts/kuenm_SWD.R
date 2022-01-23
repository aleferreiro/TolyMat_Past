####### KUENM for SWD format data  ########
#uNA VEZ PREPALAS CARPETAS COMO INDICA EL TUTORIAL, seteo el working directory en project folder del tutorial

setwd("~/Doctorado/ENM/TolyMat_Past/kuenm/WorldClim7_FinalOccData/3_PooledNiches")

library(kuenm)
library(sp)
library(raster)
library(Rcpp)

#Prepare SWD (NOT for multi-time niches)--------------------------------------------------------
# create csv files containing occurrence records (all, train, and test records) and background coordinates, 
# together with values of predictor variables, that later can be used to run model calibration in Maxent 
# using the SWD format.
occ_joint <- "Sp_joint.csv"
paths_capas <- list.files(path = "M_variables/Set_1",
                          pattern = "*.asc$",full.names = TRUE)
raster_layers <- raster::stack(paths_capas)
name_occ <- "Prueba"
background_folder <- "BG_Prueba"

prepare_swd(occ = occ_joint, species= "sp_name", longitude = "x", latitude = "y", data.split.method = "random",
            train.proportion = 0.5, raster.layers = raster_layers, sample.size = 10000,
            var.sets = NULL, min.number = 2, save = TRUE, name.occ = name_occ,
            back.folder = background_folder, set.seed = 1)

# Model calibration --------------------------------------------------------------------------------
# Variables with information to be used as arguments. Change "YOUR/DIRECTORY" by your actual directory.
occ_joint <- "Sp_joint.csv"
occ_tra <- "Sp_train.csv"
occ_test <- "Sp_test.csv"
back_dir <- "Background_Set2"
batch_cal <- "Candidate_models_Set2"
out_dir <- "Candidate_Models_Set2"
reg_mult <- c(0.1,0.25,0.5,1,2,5,8,10)
f_clas <- c("l", "q","lq", "lp", "qp", "lqp") # alternativamente "l", "q", "p", 
#"t", "h", "lq", "lp", "lt", "lh", "qp", "qt", "qh", "pt", "ph", "th", "lqp", "lqt", "lqh", "lpt", "lph",
#"lth", "qpt", "qph", "qth", "pth", "lqpt", "lqph", "lqth", "lpth", "qpth","lqpth", OR "all"
arguments <-  "maximumbackground=10300"# e.g., "maximumbackground=20000" for increasing the number of pixels in the bacground or
# note that some arguments are fixed in the function and should not be changed
maxent_path <- "C:/Users/ale_f/OneDrive/Documentos/Doctorado/ENM/Maxent/." # path de maxent
out_eval <- "Calibration_results_Set2"
threshold <- 5
rand_percent <- 50
iterations <- 500
kept <- TRUE
selection <- "OR_AICc"
wait <- FALSE
run <- TRUE

#Inicio el proceso de calibracion de los modelos
kuenm_cal_swd(occ.joint = occ_joint, occ.tra = occ_tra, occ.test = occ_test, back.dir = back_dir, 
              batch = batch_cal,out.dir.models = out_dir, reg.mult = reg_mult, f.clas = f_clas,
              max.memory = 1000, args = arguments, maxent.path = maxent_path,
              selection = selection, threshold = 5,
              rand.percent = rand_percent, iterations = iterations,
              kept = kept, out.dir.eval = out_eval)

getwd()
# Final models run -------------------------------------------------------------------------------------------
#proyecto los mejores modelos
batch_fin <- "Final_models"
mod_dir <- "Final_Models"
rep_n <- 10
rep_type <- "Bootstrap"
jackknife <- TRUE
out_format <- "logistic"#buscar paper que hable de que salida es mas correcta
project <- TRUE
G_var_dir <- "G_variables"
extrapolation_type <- "ext_clam" # para extrapolacion "all" PORQUE ME LO PIDE AHORA 
write_mess <- TRUE
write_clamp <- TRUE
wait1 <- FALSE
run1 <- TRUE
arguments <- "maximumbackground=10200" 

kuenm_mod_swd(occ.joint = occ_joint, back.dir = back_dir, out.eval = out_eval, batch = batch_fin, 
              rep.n = rep_n, rep.type = rep_type, jackknife = jackknife,
              max.memory = 1000, out.format = out_format,
              project = project, G.var.dir = G_var_dir, ext.type = extrapolation_type, 
              write.mess = write_mess, write.clamp = write_clamp, maxent.path = maxent_path,
              args = arguments, out.dir = mod_dir, wait = FALSE, run = TRUE)

warnings()
#PARA MODELOS QUE SERAN TRANSFERIDOS PUEDO REALIZAR EL MOP 

sets_var <- "Set_1" # a vector of various sets can be used
out_mop <- "MOP_results"
percent <- 5
paral <- F # make this true to perform MOP calculations in parallel, recommended
# only if a powerfull computer is used (see function's help)
# Two of the variables used here as arguments were already created for previous functions
kuenm_mmop(G.var.dir = G_var_dir, M.var.dir = M_var_dir, sets.var = sets_var, out.mop = out_mop,
           percent = percent, parallel = F)


###  Summary of results -------------------------------------------------------------------
## descriptive statistics of results (complete the code)
help(kuenm_modstats_swd)

spname <- "Tolypeutes_matacus"
fmod_dir <- "Final_Models"
statistics <- c("med", "min", "max", "range")
proj_scenarios <- c("Actual", "HTMccsm", "LGMccsm", "LIG")
exttype <- "EC"
modstats <- "Final_Model_Stats"

kuenm_modstats_swd(sp.name, fmod.dir, format = "asc", statistics,
                   proj.scenarios = proj_scenarios, ext.type = exttype, out.dir = modstats)


## detecting changes in projections (complete the code)
help(kuenm_projchanges)

curpatt <- "Actual"
time_periods <- c("HTM", "LGM", "LIG")
umbral <-  10
clim_models <- c("CCSM", "MIROC", "MPI")

kuenm_projchanges(occ = occ_joint, fmod.stats = modstats, threshold = umbral, 
                  time.periods = time_periods,  
                  current = curpatt,
                  ext.type = "EC", out.dir = "Projection_changes")


## variability in results coming from distinct sources (complete the code)
help(kuenm_modvar)

kuenm_modvar(sp.name = spname, fmod.dir = fmod_dir, replicated = TRUE, 
             format = "asc", project = TRUE, time.periods = time_periods,  
             current = curpatt,
             ext.type = exttype,
             split.length = 100, out.dir = "Variation_from_sources")


## partici?n jer?rquica de la varianza
help("kuenm_hierpart")
replicated <- TRUE
clim_models <- "ccsm"

kuenm_hierpart(sp.name = spname, fmod.dir = fmod_dir, replicated, format = "asc", 
             project = TRUE, current = curpatt, clim.models = clim_models, 
             time.periods = time_periods, ext.type = exttype,
             iterations = 100, sample.size = 1000, keep.tables = FALSE,
             out.dir = "Hierarchical_partitioning")

## agreement of extrapolative conditions (complete the code)
help(kuenm_mopagree)

kuenm_mopagree(mop.dir = outmop, in.format = "GTiff", out.format = "GTiff", 
               current = curpatt, emi.scenarios = emscepatt, 
               out.dir = "MOP_agremment")

#-------------------------------------------------------------------------------
