library(readr)
ROI_data <- read_csv("ROI_data.csv")
View(ROI_data)

library(gurobi)

# 1-3
obj = as.numeric(ROI_data[1, 2:11]) # objective vector...don't call it c in R, c is used for something else!!!!
A = matrix(0,3,10)
A[1,] = c(1,1,0,0,-1,0,0,0,0,-1) # a constraint
A[2,] = c(0,0,-2,-2,1,1,1,1,1,0) # b constraint
A[3,] = c(1,1,1,1,1,1,1,1,1,1) # c constraint
A

ub = c(rep(3, 10))

b = c(0,0,10) # limits on production, storage, and demand
sense = c('<','>','<') # all constraints are less than or equal constraints

model = list()
model$modelsense = 'max'
model$obj = obj
model$A = A
model$rhs = b
model$sense = sense
model$ub = ub

params = list()
params$outputflag = 0 # tell gurobi to shut the hell up!
# gurobi automatically assumes all variables should be non-negative...
# it's easy to change this assumption...add an lb entry to the model list
solOJ1 = gurobi(model,params=params)

print(solOJ1$x)
print(solOJ1$objval)

# 4
obj2 = as.numeric(ROI_data[2, 2:11]) # objective vector...don't call it c in R, c is used for something else!!!!
A = matrix(0,3,10)
A[1,] = c(1,1,0,0,-1,0,0,0,0,-1) # a constraint
A[2,] = c(0,0,-2,-2,1,1,1,1,1,0) # b constraint
A[3,] = c(1,1,1,1,1,1,1,1,1,1) # c constraint
A

ub = c(rep(3, 10))

b = c(0,0,10) # limits on production, storage, and demand
sense = c('<','>','<') # all constraints are less than or equal constraints

model2 = list()
model2$modelsense = 'max'
model2$obj = obj2
model2$A = A
model2$rhs = b
model2$sense = sense
model2$ub = ub

params = list()
params$outputflag = 0 
solOJ2 = gurobi(model2,params=params)

print(solOJ2$x)
print(solOJ2$objval)

# 5.
# a)
solOJ1$objval - solOJ2$x %*% as.numeric(ROI_data[1, 2:11])

# b)
solOJ2$objval - solOJ1$x %*% as.numeric(ROI_data[2, 2:11])

# c)
# NO

# 6.
Platforms = colnames(ROI_data[2:11])

#How much can we change each platform's ROI before the optimal solution changes?

#decrease
for(i in 1:10)
{
  for (j in seq(0.005, 0.30, by = 0.005))
  {
    model3 = model
    model3$obj[i] = model3$obj[i]-j
    sol2 = gurobi(model3,params = params)
    if (all(sol2$x == solOJ1$x)){
      if (j ==.30){
        print(paste0("Solution didn't change after " ,j, " decrease in ", Platforms[i] ))
      }
      next
    }
    else{print(paste0("Solution changed after " ,j, " decrease in ", Platforms[i] ))
      break
    }
  }
}

#increase
for(i in 1:10)
{
  for (j in seq(0.005, 0.30, by = 0.005))
  {
    model3 = model
    model3$obj[i] = model3$obj[i]+j
    sol2 = gurobi(model3,params = params)
    if (all(sol2$x == solOJ1$x)){
      if (j ==.30){
        print(paste0("Solution didn't change after " ,j, " increase in ", Platforms[i] ))
      }
      next
    }
    else{print(paste0("Solution changed after " ,j, " increase in ", Platforms[i] ))
      break
    }
  }
}

# 7.
load('Project1nonFA.Rdata')
budget = 10
for (i in c(1:12)){
  
  obj = as.numeric(ROI_mat[i,]/100)
  #print(obj)
  A = matrix(0,3,10)
  A[1,] = c(1,1,0,0,-1,0,0,0,0,-1) # a constraint
  A[2,] = c(0,0,-2,-2,1,1,1,1,1,0) # b constraint
  A[3,] = c(1,1,1,1,1,1,1,1,1,1) # c constraint
  A
  
  ub = c(rep(3, 10))
  
  b = c(0,0,budget)
  sense = c('<','>','<')
  
  model = list()
  model$modelsense = 'max'
  model$obj = obj
  model$A = A
  model$rhs = b
  model$sense = sense
  model$ub = ub
  
  params = list()
  params$outputflag = 0 
  solOJ3 = gurobi(model,params=params)
  
  #print(solOJ3$x)
  #print(solOJ3$objval)
  
  return = solOJ3$objval/budget
  #print(solOJ3$objval)
  budget = budget + budget*return*0.5
  print(paste0('Month ', i))
  print(solOJ3$x)
}

# 8.

# Our budget is not stable, as some of the allocations change more than $1M from period to period.
# To force a stable budget, we could add a constraint to the optimization problem that 
# limits the change in spend from period to period. 








