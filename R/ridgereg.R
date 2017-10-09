#'A RC class for ridgereg regression
#'
#'@field formula The formula object containing depedent and independent variables
#'@field data A data frame object to apply the multiple linear regression to
#'@field lambda numeric value which is default set as Zer0
#'a<-ridgereg$new(formula=Petal.Length ~ Sepal.Width + Sepal.Length,data=iris)
#'plot(ridgereg$new(formula=Petal.Length ~ Sepal.Width + Sepal.Length,data=iris))
#'@name ridgereg
#'@exportClass ridgereg
#'@export ridgereg

ridgereg <- setRefClass("ridgereg", 
                        fields = list(formula="formula",data="data.frame",lambda="numeric",
                                      reg_coeff="numeric", fitted_reg = "numeric"),
                        methods = list(
                          initialize= function(formula,data,lambda=0)
                          {
                            #assigne values to class variables
                            formula <<- formula
                            data <<- data
                            lambda <<- lambda
                            
                            #generating X and Y
                            X <- model.matrix(formula,data)
                            Y <- data[[(all.vars(formula)[1])]]
                            
                            
                            for(i in 2:ncol(X))
                            {
                              X[,i] <- ( X[,i] - mean(X[,i] )) / sd(X[,i] )
                            }
                            
                             
                            I_mat <- matrix(c(0),nrow = ncol(X),ncol = ncol(X))
                            beta_hat <- solve(( (t(X) %*% X) + I_mat)) %*% (t(X) %*% Y )
                            
                            
                            
                          }
                         )
                        ) 


data(iris)
a<-ridgereg$new(formula=Petal.Length ~ Species ,data=iris)