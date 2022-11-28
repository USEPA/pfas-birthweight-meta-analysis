## Sensitivity Analysis: log base for Kwon, 2016

#The study Kwon et al., 2016 reported results on the log-scale, but did not 
# specify the base of the log. For the main analysis, we assume base e (i.e., 
# natural log or ln). In this senstivity analysis, we run the overall 
# meta-analysis model as if Kwon et al., 2016 were run with a log base of 2 or 
# 10, commonly used log bases in addition to base e. 

pfas_k2     <-  read.csv(paste0(params$pfas_type, "_MetaAnalysis_Input_k2.csv"))
pfas_k10    <- read.csv(paste0(params$pfas_type, "_MetaAnalysis_Input_k10.csv"))
rma_overall_k2  <- rma(yi = BetaLnNgml, sei = BetaLnNgmlSE, slab = Study, data = pfas_k2)
rma_overall_k10 <- rma(yi = BetaLnNgml, sei = BetaLnNgmlSE, slab = Study, data = pfas_k10)


rbind(print_results("Log Base e", rma_overall, betas, dig1 = 3, dig2 = 4),
      print_results("Log Base 2", rma_overall_k2, pfas_k2, dig1 = 3, dig2 = 4),
      print_results("Log Base 10", rma_overall_k10, pfas_k10, dig1 = 3, dig2 = 4)) %>% 
  kable(format = 'pandoc')