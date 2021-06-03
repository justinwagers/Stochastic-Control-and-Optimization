library(readr)
library(gurobi)

#Reading in 2019 data
stocks2019 <- read_csv("stocks2019.csv")


#Creating a return matrix to get returns of stocks using stock prices
df <- subset(stocks2019, select = -c(X1))

n = ncol(df)
d = nrow(df)-1

returnMat_full = matrix(NA, d, n)

for (i in 1: d) {
    for (j in 1: n){
      returnMat_full[i,j] = ((df[i+1,j] - df [i,j])/df[i,j])[[1]]
    }
  }

colnames(returnMat_full) = colnames(stocks2019)[2:ncol(stocks2019)]

#Stock returns
returnMat<- returnMat_full[,-c(1)]
#index returns
index_returns = returnMat_full[,c(1)]

#correlation matrix for stocks
rho = cor(returnMat)
colnames(rho) = colnames(stocks2019)[3:ncol(stocks2019)]
rownames(rho) = colnames(stocks2019)[3:ncol(stocks2019)]

# Importing 2020 data
stocks2020 <- read_csv("stocks2020.csv")

#Creating a return matrix to get returns of stocks using stock prices

df <- subset(stocks2020, select = -c(X1))

n = ncol(df)
d = nrow(df)-1

returnMat2020_full = matrix(NA, d, n) #d-1 by n matrix for daily returns

for (i in 1: d) {
  for (j in 1: n){
    returnMat2020_full[i,j] = ((df[i+1,j] - df [i,j])/df[i,j])[[1]]
  }
}

colnames(returnMat2020_full) = colnames(stocks2020)[2:ncol(stocks2020)]
#stock returns
returnMat2020<- returnMat2020_full[,-c(1)]
#index returns
index_returns2020 = returnMat2020_full[,c(1)]


##Question 2 - First stock selection, then weight calculation

#Set dimension values
n = ncol(returnMat)
d = nrow(returnMat)

#objective
obj = c(as.vector(rho),rep(0,n))

#Initialize A
A = matrix(0, n^2+n+1, n^2+n)
#Constraint 1
A[1,(n^2+1):(n^2+n)] = rep(1,n)
#Contstraint 2
for (i in 1:n){
  A[(i+1), (n*(i-1)+1):(n*i)] = rep(1,n)
}
#Constraint 3
A[(n+2):(n^2+n+1), 1:n^2] = diag(1,n^2)
A[(n+2):(n^2+n+1), (n^2+1):(n^2+n)] = matrix(rep(diag(-1,n),n), nrow=n^2, byrow=T)

#right hand side
b = c(5, rep(1,n), rep(0,n^2))
#signs
sense = c(rep('=',(n+1)),rep('<',n^2))

#Prep for gurobi

#binary variable types
vartype = rep('B',n^2+n)
model = list()
model$modelsense = 'max'
model$obj = obj
model$A = A
model$rhs = b
model$sense = sense
model$vtype = vartype

params = list()
params$outputflag = 0 # mute
solOJ = gurobi(model,params=params)

# pull out the optimal stocks
stocks = solOJ$x[-(1:(n^2))]
returnMat2 <- rbind(returnMat,stocks)
returnMat2[nrow(returnMat)+1,]

#Grabbing the returns for the 5 stocks
subset_returns = returnMat2[, returnMat2[nrow(returnMat2), ] ==1]
subset_returns = subset_returns[-c(251),]

#Printing the chosen 5 stocks
print(colnames(subset_returns))


## Weight linear program

m=5

#objective - minimize sum of y's
obj = c(rep(0,m),rep(1, d))

#initialize A
A = matrix(0,m+1+2*d,m+d)

#greater than zero constraint
A[1:m,1:m] = diag(m)

#weights sum to 1 constraint
A[m+1, 1:m] = 1

#positive half of the absolute value constraing
f = m+2
for (j in 1:(d)) {
  A[f,1:m] = subset_returns[j,]
  A[f,m+j] = 1
  f = f+1
}
#negative half of the absolute value constraint
h = m+d+2
for (i in 1:(d)) {
  A[h,1:m] = -subset_returns[i,]
  A[h,m+i] = 1
  h = h+1
}

#right hand side
b = c(rep(0,m),1,index_returns,-index_returns)
#signs
sense = c(rep('>',m),'=', rep('>',(d)*2))

#Prep for gurobi
model = list()
model$modelsense = 'min'
model$obj = obj
model$A = A
model$rhs = b
model$sense = sense
model$vtype = c(rep('C',length(obj)))

params = list()
params$outputflag = 0 # mute
solOJ = gurobi(model,params=params)

#2019 error
solOJ$objval
#2019 stock selection with 2020 returns

selected_stocks = colnames(subset_returns)
daily_diff = c(rep(0,d))

#taking the absolute value of each index return - portfolio return
for (i in 1:d){
    diff = abs(index_returns2020[[i]] - sum(solOJ$x[1] * returnMat2020[,selected_stocks[1]][[i]] + solOJ$x[2] * returnMat2020[,selected_stocks[2]][[i]] +solOJ$x[3] * returnMat2020[,selected_stocks[3]][[i]] +solOJ$x[4] * returnMat2020[,selected_stocks[4]][[i]] +solOJ$x[5] * returnMat2020[,selected_stocks[5]][[i]]))
    daily_diff[i] = diff
}
#2020 error - adding all of the absolute values together (higher than 2019 error)
sum(daily_diff)

print(selected_stocks)
solOJ$x[1:5]

### Question 3

##Same thing for different values of m

#This part is uncommented - same comments as above. 

steps <- seq(10, 100, by=10)
solutions = matrix(0,10,2)

for (m in steps){
  n = ncol(returnMat)
  d = nrow(returnMat)
  
  obj = c(as.vector(rho),rep(0,n))
  
  A = matrix(0, n^2+n+1, n^2+n)
  A[1,(n^2+1):(n^2+n)] = rep(1,n)
  for (i in 1:n){
    A[(i+1), (n*(i-1)+1):(n*i)] = rep(1,n)
  }
  A[(n+2):(n^2+n+1), 1:n^2] = diag(1,n^2)
  A[(n+2):(n^2+n+1), (n^2+1):(n^2+n)] = matrix(rep(diag(-1,n),n), nrow=n^2, byrow=T)
  b = c(m, rep(1,n), rep(0,n^2))
  sense = c(rep('=',(n+1)),rep('<',n^2))
  
  #Prep for gurobi
  vartype = rep('B',n^2+n)
  model = list()
  model$modelsense = 'max'
  model$obj = obj
  model$A = A
  model$rhs = b
  model$sense = sense
  model$vtype = vartype
  
  params = list()
  params$outputflag = 0 # mute
  solOJ = gurobi(model,params=params)
  
  # print the optimal x
  print(solOJ$objval)
  
  stocks = solOJ$x[-(1:(n^2))]
  returnMat2 <- rbind(returnMat,stocks)
  returnMat2[nrow(returnMat)+1,]
  subset_returns = returnMat2[, returnMat2[nrow(returnMat2), ] ==1]
  subset_returns = subset_returns[-c(nrow(returnMat2)),]
  
  ## Weight linear program
  obj = c(rep(0,m),rep(1, d))
  
  A = matrix(0,m+1+2*d,m+d)
  
  A[1:m,1:m] = diag(m)
  #sum(w) = 1
  A[m+1, 1:m] = 1
  #absolute
  # g=f
  # a = j
  f = m+2
  for (j in 1:(d)) {
    A[f,1:m] = subset_returns[j,]
    A[f,m+j] = 1
    f = f+1
  }
  h = m+d+2
  for (i in 1:(d)) {
    A[h,1:m] = -subset_returns[i,]
    A[h,m+i] = 1
    h = h+1
  }
  
  b = c(rep(0,m),1,index_returns,-index_returns)
  sense = c(rep('>',m),'=', rep('>',(d)*2))
  
  #Prep for gurobi
  model = list()
  model$modelsense = 'min'
  model$obj = obj
  model$A = A
  model$rhs = b
  model$sense = sense
  model$vtype = c(rep('C',length(obj)))
  
  params = list()
  params$outputflag = 0 # mute
  solOJ = gurobi(model,params=params)
  
  # print the optimal x
  solutions[(m/10),1] = (solOJ$objva)
  print(solOJ$objva)
  
  
  n = ncol(returnMat2020)
  d = nrow(returnMat2020)
  
  #2019 stock selection with 2020 returns
  
  selected_stocks = colnames(subset_returns)
  daily_diff = c(rep(0,d))
  for (i in 1:d){
    sum = 0
    for (j in 1:m){
      sum = sum +solOJ$x[j] * returnMat2020[,selected_stocks[j]][[i]]
    }
    diff = abs(index_returns2020[[i]] - sum)
    daily_diff[i] = diff
  }
  solutions[(m/10),2] = sum(daily_diff)
  
  print(sum(daily_diff))
  

}
colnames(solutions) <-c('2019','2020')
View(solutions)

y2020 <- solutions[,2]
x = c(10,20,30,40,50,60,70,80,90,100)
par(mar=c(1,1,1,1))
plot(x,y2020,type="l",col="green")

#Question 4


steps <- seq(10, 100, by=10)
solutions = matrix(0,10,2)
time_limit = 3600

for (m in steps){
  #set dimensions
  n= ncol(returnMat)
  d= nrow(returnMat)
  
  #setting big M - 1 because no single weight can be greater than 1.
  M = 1
  
  #objective
  obj = c(rep(0,2*n),rep(1, d))
  #initialize A matrix
  A = matrix(0,n+1+2*d +n+1,n+d+n)
  
  #Must be greater than or equal to zero constraint
  A[1:n,1:n] = diag(n)
  #sum of weights must be equal to 1
  A[n+1, 1:n] = 1
  #positive absolute value constraint
  f = n+2
  for (j in 1:(d)) {
    A[f,1:n] = returnMat[j,]
    A[f,2*n+j] = 1
    f = f+1
  }
  #negative absolute value constraint
  h = n+d+2
  for (i in 1:(d)) {
    A[h,1:n] = -returnMat[i,]
    A[h,2*n+i] = 1
    h = h+1
  }
  #Big M constraint
  A[(n+d+d+2):(n+1+d+d +n),1:n] = diag(n)
  A[(n+2*d+2):(n+1+2*d +n),n:(2*n-1)] = (-M)*diag(n)
  
  #binary variables must add to m constraint
  A[(2*n+2*d+2),(n+1):(2*n)] = 1
  
  #right hand side
  b = c(rep(0,n),1,index_returns,-index_returns,rep(0,n),m)
  
  #signs
  sense = c(rep('>',n),'=', rep('>',(d)*2),rep('<',n),'=')
  #Prep for gurobi
  model = list()
  model$modelsense = 'min'
  model$obj = obj
  model$A = A
  model$rhs = b
  model$sense = sense
  #y's must be binary
  model$vtype = c(rep('C',n),rep('B',n),rep('C',d))
  
  params = list()
  params$outputflag = 0 # mute
  params$TimeLimit = time_limit
  solOJ = gurobi(model,params=params)
  
  solutions[(m/10),1] = (solOJ$objva)
  print(solOJ$objva)
  
  #reset dimensions to 2020 data
  n = ncol(returnMat2020)
  d = nrow(returnMat2020)
  
  #2019 stock selection with 2020 returns
  
  selected_stocks = colnames(returnMat2020)
  daily_diff = c(rep(0,d))
  
  #here we have generalized the sum of the absolute values to work for any number
  #m of stocks
  for (i in 1:d){
    sum = 0
    for (j in 1:m){
      sum = sum +solOJ$x[j] * returnMat2020[,selected_stocks[j]][[i]]
    }
    diff = abs(index_returns2020[[i]] - sum)
    daily_diff[i] = diff
  }
  solutions[(m/10),2] = sum(daily_diff)
  
  print(sum(daily_diff))
  
  
}
colnames(solutions) <-c('2019','2020')
View(solutions)
write.csv(solutions,'solutions1.csv')


#plotting 2020 error versus size of m
y2020 <- solutions[,2]
x = c(10,20,30,40,50,60,70,80,90,100)
par(mar=c(1,1,1,1))
plot(x,y2020,type="l",col="green")
