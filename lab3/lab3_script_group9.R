library(rmarkdown)


amount_of_code_model <- function(N, X_lang, X_ide, X_experience, likert=T) { 
  
  ref <- 100 #Intercept/reference group. This is java, intelij and junior

  #main effects
  lang_cpp <- 0.2
  lang_python <- 1.5
  
  ide_visual_studio <- 1.2 
  
  xp_senior <- 2.0 
  
  # second order interactions
  lang_cpp_ide_visual_studio <- 1.0  
  lang_python_ide_visual_studio <- 1.0  
  lang_python_xp_senior <- 0.0 
  lang_cpp_xp_senior <- 2.0
  ide_visual_studio_xp_senior <- 1.5

  
  #third order interactions
  lang_cpp_xp_senior_ide_visual_studio <- 1.0 
  lang_python_xp_senior_ide_visual_studio <- 0.0 

  #input
  x_lang_cpp <- X_lang[1]
  x_lang_python <- X_lang[2]
  
  x_ide_visual_studio <- X_ide[1] 
  
  x_senior <- X_experience[1]

  response_std <- 1.0

  #This is the linear model that controls the response variable

  y <- ref +
    lang_cpp * x_lang_cpp +
    lang_python * x_lang_python +
    ide_visual_studio  * x_ide_visual_studio +
    xp_senior * x_senior +
    
    lang_cpp_ide_visual_studio * x_lang_cpp * x_ide_visual_studio +
    lang_python_ide_visual_studio * x_lang_python *x_ide_visual_studio +
    lang_python_xp_senior * x_lang_python * x_senior +
    lang_cpp_xp_senior * x_lang_cpp * x_senior +
    ide_visual_studio_xp_senior * x_senior * x_ide_visual_studio +
    
    lang_cpp_xp_senior_ide_visual_studio * x_lang_cpp * x_ide_visual_studio * x_senior +
    lang_python_xp_senior_ide_visual_studio * x_lang_python * x_ide_visual_studio * x_senior 
    
    y_out<- rnorm(N, mean=y, sd = response_std) # This generates a normal distribution 
  
  #if(likert)
  #  y_out<- findInterval(y_out,vec=c(-Inf,-2.5,-1, 1,2.5,Inf)+ref) 
  
  # this is fairly unbiased for standard deviation of 1 in the responses and 0 mean. 
  # That is why we add the value of the reference group.
  # Here the reference will have a mean of 3 in likert scale
  return(y_out)
}