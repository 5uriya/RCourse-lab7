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
                                      beta_ridge="list", y_hat = "list",
                                      char_lamb = "character",
                                      beta_coff_numeric="list"
                                      ),
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
                      
                            #QR decompostion
                            x_qr <- qr(X)
                            QR_R <- qr.R(x_qr)
                            ###################
                            mat_Y <- as.matrix(Y)
                            # tranpose_x <- t(X)
                            
                            #loop for lambda 
                            for(k in 1:length(lambda))
                            {
                              I_mat <- matrix(c(0),nrow = ncol(X),ncol = ncol(X))
                              diag(I_mat) <- lambda[k]
                              beta_ridge_hat <- solve(( (t(QR_R) %*% QR_R) + I_mat)) %*% (t(X) %*% mat_Y )
                              y_fit <- as.numeric(X %*% beta_ridge_hat)
                            

                              ridge_coef <- as.numeric(beta_ridge_hat)
                              beta_coff_numeric[[k]] <<- ridge_coef
                              names(ridge_coef) <- rownames(beta_ridge_hat)
                              beta_ridge[[k]] <<- ridge_coef #assign values to class variable
                              y_hat[[k]] <<- y_fit #assign values to class variable  
                            }
                            
                            # y_hat <<- tmp_fit_list
                            # beta_ridge <<- tmp_beta_list
                            # ridge_coef <<- as.numeric(beta_ridge)
                            # names(ridge_coef) <<- rownames(beta_ridge)
                            # print(y_hat)
                            # print(beta_ridge)
                            # print(r_name)
                            
                            # y_hat <<- as.numeric(X %*% beta_ridge)    #y_hat calculate
                            # 
                            # ridge_coef <<- as.numeric(beta_ridge)
                            # names(ridge_coef) <<- rownames(beta_ridge)
                            # #extract dataset name 
                            datasetName <<-  deparse(substitute(data))  
                            char_lamb <<- deparse(substitute(lambda))  
                            
                
                          },
                          predict =  function()
                          {
                            "This function returns the vector of calculated fitted values"
                            return(y_hat)
                          },
                          coef = function()
                          {
                            "This function returns the vector of beta coefficients  "
                            return(beta_ridge)
                          },
                          print = function()
                          {
                            "This function prints the formula and dataset name as well as the calculated coefficients"
                            
                            r_name <- c("",rownames(as.data.frame(beta_ridge)))
                            langd_namn<-nchar(r_name)
                            cat(langd_namn)
                            cat("Call:")
                            cat("\n")
                            formula_print<- paste0("ridgereg(","formula = ",formula[2]," ",formula[1]," ",formula[3],", ","data = ",datasetName,",lambda = ", char_lamb,")",sep="")
                            cat(formula_print)
                            cat("\n")
                            cat("\n")
                            cat("Coefficients:")
                            cat("\n")
                            cat(" ")
                            cat(paste(r_name,collapse = "  "),sep="",collapse="\n")
                            for(i in 1:length(beta_coff_numeric)){
                              cat(paste(lambda[[i]],collapse = "  "),sep="",collapse="\t")
                              cat(paste(round(beta_coff_numeric[[i]],5),collapse = "  "),sep="",collapse="\n")
                            }
                            cat("\n")
                            cat("\n")

                          }
                          
                          
                         )
                        ) 
a<- ridgereg(Petal.Length ~ Sepal.Width + Sepal.Length,data=iris, lambda= c(0.5,1))