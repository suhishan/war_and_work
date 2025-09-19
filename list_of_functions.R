
# 1.  Output Doubly Robust DiD Estimates for an outcome and for 
#     a given dataframe.
dr <- function(outcome, data){
  a <- drdid(yname = outcome, tname = "post", idname = "id",
             dname = "treatment_s",
             xformla = ~ hindu + brahmin_chhetri + 
               urbrur + sex + poverty_rate,
             data = data, panel = FALSE, boot = F, nboot = 199)
  
  out =  round(data.frame(a[1:4]), 3)
  return (out)
  
}

# 2. Take a lot of outcome variables and compute DrDiD for each, and
#    return the results in a data.frame

#    The Outcome variables in question.

vars <- c("usually_emp", "currently_emp_out",
          "currently_selfemp_out",
          "work_hours_outside", "work_hours_outside_std",
          "selfemp_hours_outside","selfemp_hours_outside_std")


output_dr <- function(vars, data) {
  b <- sapply(vars, dr, data)
  return (data.frame(t(b)))
}


# 3. Take a DrDid model output's model name, and produce a .tex file.

# Pretty variable names for outcome variables
outcome_var_names <- c(
  usually_emp = "Usually Employed",
  currently_emp_out = "Currently Employed",
  currently_selfemp_out = "Currently Self-employed",
  work_hours_outside = "Work Hours",
  work_hours_outside_std = "Work Hours (std)",
  selfemp_hours_outside = "Self Employment Hours",
  selfemp_hours_outside_std = "Self Employment Hours (std)"
  )

model_to_tex <- function(model_name, caption, file_name) {
  model_name <- model_name %>% 
  mutate(variables = outcome_var_names[variables]) %>% 
  column_to_rownames("variables")
 
  print(xtable(model_name, 
               caption = paste(
                 "Doubly Robust DiD estimates of ATT (",
                  caption, ")", sep = "" )
               ), 
      type = "latex",
      file = paste0("Analysis files/", file_name,".tex"),
      booktabs = T,
      )
  
  return (model_name)
}

