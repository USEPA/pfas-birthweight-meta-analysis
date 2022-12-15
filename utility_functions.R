print_results <- function(model_nm, out, dat, dig1 = 1, dig2 = 2) {
  # Description: Select relevant variables from model output and organize into a table.
  #   
  # Args:
  #   model_nm (chr):   short description of the model
  #   out (rma):        output from rma call
  #   dat (data.frame): full data set
  
  with(out, 
       data.frame(Model = model_nm, 
                  N = nrow(dat), 
                  Estimate = b, 
                  SE = se, 
                  LCL = ci.lb, 
                  UCL = ci.ub, 
                  PVal = pval, 
                  I2 = I2, 
                  Q_PVal = QEp)) %>% 
    mutate_at(c(3:6), ~round(., dig1)) %>%
    mutate_at(c(7:9), ~round(., dig2)) %>%
    remove_rownames()
}

modify_label <- function(out, lab) {
  # Description: Create forest plot label.
  #   
  # Args:
  #   out (rma):  output from rma call
  #   lab (chr):  text for label
  
  a <- metafor:::.pval(out$QEp, digits=2, showeq=TRUE, sep=" ")
  b <- formatC(out$I2, digits=1, format="f")
  c <- paste0("(Q-pval ", a, ", I2 = ", b, "%)")
  paste(lab, c)
}

pool_studies <- function(dat, author_nm) {
  
  # Description: Pool male/female data and output data set with separate data 
  #   replaced with pooled estimates.
  #
  # Args:  
  #   author_nm (chr):  Author column of data
  #   dat (data.frame): pfas data
  
  study_nms <- as.character(unlist(c(dat[dat$Author == author_nm, c("Study")])))
  yr_new    <- substr(study_nms[1], nchar(study_nms[1]) - 4, nchar(study_nms[1]) - 1)
  out_ln    <- summary(rma(yi = BetaLnNgml, sei = BetaLnNgmlSE, data = dat %>% filter(Author == author_nm)))
  out       <- summary(rma(yi = BetaNgml, sei = BetaNgmlSE, data = dat %>% filter(Author == author_nm)))
  data_new  <- dat %>% filter(Study != study_nms[2])
  data_new[data_new$Author == author_nm, ] <- data_new %>%
    filter(Study == study_nms[1]) %>%
    mutate(Year = yr_new, 
           Study = paste(author_nm, yr_new, sep = ", "), 
           BetaNgml = as.numeric(round(out$b, 2)), 
           BetaNgmlSE = as.numeric(round(out$se, 2)), 
           BetaNgmlLCL = as.numeric(round(out$ci.lb, 2)), 
           BetaNgmlUCL = as.numeric(round(out$ci.ub, 2)), 
           BetaLnNgml = as.numeric(round(out_ln$b, 2)), 
           BetaLnNgmlSE = as.numeric(round(out_ln$se, 2)), 
           BetaLnNgmlLCL = as.numeric(round(out_ln$ci.lb, 2)), 
           BetaLnNgmlUCL = as.numeric(round(out_ln$ci.ub, 2)), 
           `Sample Size` = as.numeric(sum(dat[dat$Author == author_nm, "Sample Size"])))
  return(data_new)
  
}

assign_strata <- function(x) {
  
  out <- NULL
  
  for (i in 1:length(x)) {
    
    if (x[i] %in% c("T1", "T1-T2")) {
      
      out[i] <- "Early"
      
    } else if (x[i] %in% c("T2", "T2-T3", "T3")) {
      
      out[i] <- "Late"
      
    } else {
      
      out[i] <- "Post"
      
    }
  }
  
  return(out)
  
}

backcalc_se <- function(l, u) {
  # https://handbook-5-1.cochrane.org/chapter_7/7_7_7_2_obtaining_standard_errors_from_confidence_intervals_and.htm
  # l: lower confidence bound
  # u: upper confidence bound
  
  return((u - l) / 3.92) 
  
}
