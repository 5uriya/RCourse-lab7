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
                        fields = list(formula="formula",data="data.frame",
                                      lambda="numeric",datasetName="character", 
                                      beta_ridge="matrix", y_hat = "numeric",
                                      ridge_coef="numeric"),
                        methods = list(
                          initialize= function(formula,data,lambda = 0, normalize = FALSE)
                          {
                            "This function acts as constructor"
                            
                            formula <<- formula #formula assign to class formula variable
                            data <<- data #data assign to class data variable
                            lambda <<- lambda #lambda 
                            
                            X <- model.matrix(formula,data)
                            Y <- data[[(all.vars(formula)[1])]]
                            
                            #normalize the data if normiliz param is TRUE
                            if(normalize == TRUE)
                              for(i in 2:ncol(X))
                                X[,i] <- ( X[,i] - mean(X[,i] )) / sd(X[,i] )
                      
                            I_mat <- matrix(c(0),nrow = ncol(X),ncol = ncol(X)) #create a identity Matrix
                            diag(I_mat) <- sqrt(lambda) #update diagnal of lambda matrix of I_mat
                            mat_Y <- as.matrix(Y) #convert into martix
                            beta_ridge <<- solve(( (t(X) %*% X) + I_mat)) %*% (t(X) %*% mat_Y ) #calculate beta Ridge
      
                            y_hat <<- as.numeric(X %*% beta_ridge)    #y_hat calculate
                            
                            ridge_coef <<- as.numeric(beta_ridge)
                            names(ridge_coef) <<- rownames(beta_ridge)
                            #extract dataset name 
                            datasetName <<-  deparse(substitute(data))  
    
                          },
                          predict =  function()
                          {
                            "This function returns the vector of calculated fitted values"
                            return(y_hat)
                          },
                          coef = function()
                          {
                            "This function returns the vector of beta coefficients  "
                            return(ridge_coef)
                          },
                          
                          print = function()
                          {
                            "This function prints the formula and dataset name as well as the calculated coefficients"
                            r_name <- rownames(as.data.frame(ridge_coef))
                            cat("Call:")
                            cat("\n")
                            formula_print<- paste0("ridgereg(","formula = ",formula[2]," ",formula[1]," ",formula[3],", ","data = ",datasetName,")",sep="")
                            cat(formula_print)
                            cat("\n")
                            cat("\n")
                            cat("Coefficients:")
                            cat("\n")
                            cat(" ")
                            cat(r_name)
                            cat(" ")
                            cat("\n")
                            cat(ridge_coef)
                            cat("\n")
                            cat("\n")

                          }
                          
                          
                         )
                        ) 

# 
# library(MASS)
# cc<- lm.ridge(Petal.Length~Sepal.Width+Sepal.Length, data=iris)
# cc
# data(iris)
# a<-ridgereg$new(Petal.Length~Sepal.Width+Sepal.Length, data=iris)
# a$print()