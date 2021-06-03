# Project 3

#Direct Variable Selection - MIQP
library(gurobi)

time_limit = 2

train.data = read.csv("training_data.csv")
test.data = read.csv("test_data.csv")

#Generate the constraint matrix
require(caret)
set.seed(42)
y = train.data[,1]
flds <- createFolds(y, k = 10, list = TRUE, returnTrain = FALSE)

results = matrix(0,100,3)
M <- 4

for (i in seq(1,10,by=1)){
  train <- train.data[-unlist(flds[i]),]
  X_mat = model.matrix(y~., data = train)
  y_train = train[,1]
  print(i)
  
  for (j in seq(5, 50, by=5)){
    m = ncol(X_mat)-1
    zeros =  matrix(0,m,m)
    k=j

    print(j)
    
    constraints = function(a, b){ 
      A = matrix(0,2*m+1,2*m)
      #k constraint
      A[1,(m+1):(2*m)] = rep(1,m)
      #big M constraint
      A[2:(m+1), 1:m] = diag(1,m,m)
      A[2:(m+1), (m+1):(2*m)] = diag(-M,m,m)
      A[(m+2):(2*m +1), 1:m] = diag(-1,m,m)
      A[(m+2):(2*m +1), (m+1):(2*m)] = diag(-M,m,m)
      B_not = matrix(0,2*m+1,1)
      A = cbind(B_not,A)
      b = c(k,rep(0, (2*m)))
      return(list("A" = A, 
                  "b" = b))
    }
    
    model = list()
    #linear part
    model$obj = c(-2*(t(y_train) %*% X_mat), rep(0, m))
    
    #Quadratic part
    Q = matrix(0,(2*m+1),(2*m+1))
    Q[1:(m+1),1:(m+1)] = t(X_mat) %*% X_mat
    model$Q = Q
    
    model$sense = rep("<=", (2*m) +1)
    model$vtype = c(rep("C", m+1), rep("B", m))
    
    
    cons = constraints(M, k)
    model$A = cons$A
    model$modelsense = 'min'
    model$rhs = cons$b
    model$lb = -M
    params = list()
    params$outputflag = 0 # mute
    params$TimeLimit = time_limit
    MIQP_sol = gurobi(model, params=params)
    
    
    betas_final = MIQP_sol$x[1:(m+1)]
    betas_final
    
    #make sure M isn't binding
    if (any(betas_final == M)){
      print('M in betas')
    }

    validate <- train.data[unlist(flds[1]),]
    validate_mat = model.matrix(y~., data = validate)
    y_validate = validate[,1]
    
    sum_sq = sum((validate_mat%*% betas_final - y_validate)^2)
    results[(10*(i-1) + (j/5)), 1] = i
    results[(10*(i-1) + (j/5)), 2] = j
    results[(10*(i-1) + (j/5)), 3] = sum_sq
  }
}


res = as.data.frame(results)
colnames(res)[1] <- "Cv fold"
colnames(res)[2] <- "k"
colnames(res)[3] <- "sum of squares"
results1 = aggregate(res[, 3], list(res$k), mean)
write.csv(res,'results.csv')

results1 = as.data.frame(results1)
colnames(results1)[1] <- "k"
colnames(results1)[2] <- "average sum of squared error"

results1


## Fit on entire training set, predict y

k=25
train <- train.data
X_mat = model.matrix(y~., data = train)
y_train = train[,1]
model = list()
#linear part
model$obj = c(-2*t(X_mat) %*% y_train, rep(0, m))

#Quadratic part
Q = matrix(0,(2*m+1),(2*m+1))
Q[1:(m+1),1:(m+1)] = t(X_mat) %*% X_mat
model$Q = Q

model$sense = rep("<=", (2*m) +1)
model$vtype = c(rep("C", m+1), rep("B", m))


cons = constraints(M, k)
model$A = cons$A
model$modelsense = 'min'
model$rhs = cons$b
model$lb = -M
params = list()
params$outputflag = 0 # mute
params$TimeLimit = time_limit
MIQP_sol = gurobi(model, params=params)


betas_final = MIQP_sol$x[1:(m+1)]

betas_final
write.csv(betas_final,'betas.csv')


## Error on test set

test.data_mat = model.matrix(y~., data = test.data)
y_test = test.data[,1]

sum_sq = sum((test.data_mat%*% betas_final - y_test)^2)

sum_sq





# Question 4
library(glmnet)
X.train = as.matrix(train.data[,-1])
y.train = as.matrix(train.data[1])
X.test = as.matrix(test.data[,-1])
y.test = as.matrix(test.data[1])

# Visualize coefficients
fit <- glmnet(X.train, y.train)
plot(fit, label = TRUE)

# cross-fold
set.seed (123)
cv.fit = cv.glmnet(X.train, y.train,nfolds = 10)
plot(cv.fit)

# lambda
bestLambda = cv.fit$lambda.min
bestLambda

# fit to train
new.cv =glmnet(X.train,y.train ,lambda=bestLambda)


# predict
lasso.pred = predict(new.cv, newx=X.test, s=bestLambda)

# MSE
sum((y.test- lasso.pred)^2)

# looking at which columns go to zero
cv.fit = glmnet(X.train, y.train, alpha=1)
predict(cv.fit, s=bestLambda, type="coefficients")
                            
                            