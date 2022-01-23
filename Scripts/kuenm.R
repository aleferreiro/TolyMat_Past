#Instalacion
if(!require(devtools)){
  install.packages("devtools")
}

if(!require(kuenm)){
  devtools::install_github("marlonecobos/kuenm")
}


### Correr el kuenm ------

#uNA VEZ PREPALAS CARPETAS COMO INDICA EL TUTORIAL, seteo el working directory en project folder del tutorial

setwd("D:/MEGAsync/Modelado/KUENM/Tolypeutes_matacus/WorldClim5/1_Actual_RandomPart")
setwd("D:/MEGAsync/Modelado/KUENM/Tolypeutes_matacus/WorldClim5/5_HolocenoHistorico_RandomPart/MPI")
setwd("D:/MEGAsync/Modelado/KUENM/Tolypeutes_matacus/WorldClim5/6_Pleistoceno_RandomPart/CCSM")
setwd("D:/MEGAsync/Modelado/KUENM/Tolypeutes_matacus/WorldClim5/6_Pleistoceno_RandomPart/CCSM_Climatic")
setwd("D:/MEGAsync/Modelado/KUENM/Tolypeutes_matacus/WorldClim7_FinalOccData/1_CurrentNiche")

library(kuenm)
# Variables with information to be used as arguments. Change "YOUR/DIRECTORY" by your actual directory.
occ_joint <- "Sp_joint.csv"
occ_tra <- "Sp_train.csv"
M_var_dir <- "M_variables"
batch_cal <- "Candidate_models"
out_dir <- "Candidate_Models"
reg_mult <- c(0.1,0.5,1,2,5,8,10)
f_clas <- c("no.t.h") # alternativamente "all"
args <- "maximumbackground=10000"  # e.g., "maximumbackground=20000" for increasing the number of pixels in the bacground or
# note that some arguments are fixed in the function and should not be changed
maxent_path <- "D:/MEGAsync/Modelado/KUENM/." # path de maxent
wait <- FALSE
run <- TRUE

#Inicio el proceso de calibracion de los modelos
help(kuenm_cal)
kuenm_cal(occ.joint = occ_joint, occ.tra = occ_tra, M.var.dir = M_var_dir, batch = batch_cal,
          out.dir = out_dir, reg.mult = reg_mult, f.clas = f_clas, args = args,
          maxent.path = maxent_path, wait = wait, run = run)


#Realizo la evaluacion de los mismos
occ_test <- "Sp_test.csv"
out_eval <- "Calibration_results"
threshold <- 5
rand_percent <- 50
iterations <- 500
kept <- TRUE
selection <- "OR_AICc"
paral_proc <- FALSE 

cal_eval <- kuenm_ceval(path = out_dir, occ.joint = occ_joint, occ.tra = occ_tra, occ.test = occ_test, batch = batch_cal,
                        out.eval = out_eval, threshold = threshold, rand.percent = rand_percent, iterations = iterations,
                        kept = kept, selection = selection, parallel.proc = paral_proc)


warnings()
#proyecto los mejores modelos
batch_fin <- "Final_models"
mod_dir <- "Final_Models"
rep_n <- 10
rep_type <- "Bootstrap"
jackknife <- TRUE
out_format <- "logistic"#buscar paper que hable de que salida es mas correcta
project <- TRUE
G_var_dir <- "G_variables"
ext_type <- "ext_clam" # para extrapolacion "all" PORQUE ME LO PIDE AHORA 
write_mess <- TRUE
write_clamp <- TRUE
wait1 <- FALSE
run1 <- TRUE
args <- "maximumbackground=10000"  

kuenm_mod(occ.joint = occ_joint, M.var.dir = M_var_dir, out.eval = out_eval, batch = batch_fin, rep.n = rep_n,
          rep.type = rep_type, jackknife = jackknife, out.dir = mod_dir, out.format = out_format, project = project,
          G.var.dir =G_var_dir, ext.type = ext_type, write.mess = write_mess, write.clamp = write_clamp, 
          maxent.path = maxent_path, args = args, wait = wait1, run = run1)

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
help(kuenm_modstats)

spname <- "Tolypeutes_matacus"
modstats <- "Final_Model_Stats"

kuenm_modstats(sp.name = spname, fmod.dir = mod_dir, format = "asc", 
               project = TRUE, statistics = c("med", "range"), replicated = TRUE,
               proj.scenarios = c("Current", "HTM_CCSM", "HTM_MIROC", "HTM_MPI", "LGM_CCSM", "LGM_MIROC", "LGM_MPI", "LIG"), 
               ext.type = "EC", out.dir = modstats)


## detecting changes in projections (complete the code)
help(kuenm_projchanges)

curpatt <- "Current"
time_periods <- c("HTM", "LGM", "LIG")
clim_models <- c("CCSM", "MIROC", "MPI")

kuenm_projchanges(occ = occ_joint, fmod.stats = modstats, threshold = 5, 
                  time.periods = time_periods,  
                  current = curpatt,
                  clim.models = clim_models,
                  ext.type = "EC", out.dir = "Projection_changes")


## variability in results coming from distinct sources (complete the code)
help(kuenm_modvar)

kuenm_modvar(sp.name = spname, fmod.dir = mod_dir, replicated = TRUE, 
             format = "asc", project = TRUE, time.periods = time_periods,  
             current = curpatt,
             clim.models = clim_models,
             ext.type = "E",
             split.length = 100, out.dir = "Variation_from_sources")


## partición jerárquica de la varianza


kuenm_hierpart(sp.name, fmod.dir, replicated, format = "asc", project, current,
               time.periods, emi.scenarios, clim.models, ext.type,
               iterations = 100, sample.size = 1000, keep.tables = FALSE,
               factors.col, out.dir = "Hierarchical_partitioning")

## agreement of extrapolative conditions (complete the code)
help(kuenm_mopagree)

kuenm_mopagree(mop.dir = outmop, in.format = "GTiff", out.format = "GTiff", 
               current = curpatt, emi.scenarios = emscepatt, 
               out.dir = "MOP_agremment")

#-------------------------------------------------------------------------------
