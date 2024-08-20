




# ChatGPT code
# Adjust the Z matrix to assume one state for all systems
ZZ <- matrix(1, 9, 1)

# Modify the model specifications
mod.list <- list(
  B = "identity",  
  U = "zero",          
  Q = "diagonal and equal",         
  Z = ZZ,
  A = "scaling",    
  R = "diagonal and unequal",
  x0 = matrix("mu", nrow = 1, ncol = 1),  # Single initial state
  tinitx = 0             
)

# Fit PHE model 
PHE.fit <- MARSS(all.PHE, model = mod.list)
autoplot(PHE.fit)
