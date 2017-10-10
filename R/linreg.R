#'A RC class for multiple linear regression
#'
#'@field formula The formula object containing depedent and independent variables
#'@field data A data frame object to apply the multiple linear regression to
#'
#'a<-linreg$new(formula=Petal.Length ~ Sepal.Width + Sepal.Length,data=iris)
#'plot(linreg$new(formula=Petal.Length ~ Sepal.Width + Sepal.Length,data=iris))
#'@name linreg
#'@exportClass linreg
#'@export linreg
require(ggplot2)

linreg <- setRefClass("linreg",
           fields = list(formula= "formula", data = "data.frame", betaCoff="numeric", fittedValues="numeric",
                         residualsVal="numeric",residualsVariance="matrix",
                         dataSetName="character",tValues="numeric",pValue="numeric",regression_coff="numeric",
                         degree_f="numeric"
                         ),
           methods = list(
             initialize = function(formula, data)
             {
                #This function acts as constructor
                formula <<- formula
                data <<- data

                X <- model.matrix(formula,data)
                Y <- data[[(all.vars(formula)[1])]]

                QR <- qr(X)
                betaCoff <<- qr.coef(QR,Y) #This variable stores the calculated value of beta coefficient
                fittedValues <<- qr.fitted(QR,Y) #This variable stores the  calculated value of fitted value
                residualsVal <<- qr.resid(QR,Y) #This variable store the calculated calculated value residual value

                degree_f <<- nrow(X) - ncol(X) #This variable stores the calculated value of degree of freedom
                residualsVariance <<- ( t(residualsVal) %*% residualsVal) / (degree_f) #This variable stores the calculated value of residual variance

                regression_coff <<- as.numeric(residualsVariance) *  diag(solve((t(X) %*% X))) #This variable stores the calculated value of variance of the regression coefficients

                #tvalues
                tValues <<- diag(betaCoff / sqrt(diag(regression_coff))) #Calculates and stores and t-value
                #p values
                pValue <<-  2*pt(-abs(tValues),df=nrow(X)-ncol(X)) #Calculates and stores P-Value
                dataSetName <<- deparse(substitute(data)) #Stores the name of dataset being used


             },

             print = function()
             {
               "This function prints the formula and dataset name as well as the calculated coefficients"
               r_name <- rownames(as.data.frame(betaCoff))
               cat("Call:")
               cat("\n")
               formula_print<- paste0("linreg(","formula = ",formula[2]," ",formula[1]," ",formula[3],", ","data = ",dataSetName,")",sep="")
               cat(formula_print)
               cat("\n")
               cat("\n")
               cat("Coefficients:")
               cat("\n")
               cat(" ")
               cat(r_name)
               cat(" ")
               cat("\n")
               cat(betaCoff)
               cat("\n")
               cat("\n")
             },

             plot = function()
             {
               library(magick)
               library(grid)
               library(ggplot2)
               logo <- image_read('http://www.ida.liu.se/mall11/images/logo-sv.png')
               img<- rasterGrob(logo,interpolate=TRUE)

               #liu-theme
               theme_liu <- function()
               {  theme_bw() +
                   theme(plot.background = element_rect(size = 1, color = "#ccf0fa", fill = "#0cc7d3"),
                         text=element_text(size = 15, family = "serif", color = "white",face = "bold"),

                         plot.title = element_text(size=20,margin = margin(t=5,b=10),hjust = 0.5),

                         axis.text.y = element_text(colour = "white"),
                         axis.text.x = element_text(colour = "white"),

                         panel.background = element_rect(fill = "#ccf5f0")

                         )
               }



               "This function plots the graphs for fitted values"
               phras<- paste("lm(",format(formula),")")

               stand_res <- sqrt(abs((residualsVal-mean(residualsVal))/sqrt(var(residualsVal))))


              plot1<-ggplot(data.frame(fittedValues,residualsVal),aes(y=residualsVal,x=fittedValues))+geom_point(shape=1,size=3)+xlab(paste("Fitted values", phras, sep = "\n"))+ ylab("Residuals")+ggtitle("Residuals vs Fitted") +geom_hline(yintercept=0, linetype="dashed")+geom_smooth(span = 1.5,colour="red",method="loess",se=FALSE)+annotation_custom(img, xmin = 1.4, xmax = 2.4, ymin = 0.8, ymax = 1.4)+theme_liu()

              plot2<-ggplot(data.frame(fittedValues,stand_res),aes(y=stand_res,x=fittedValues))+geom_point(shape=1)+xlab(paste("Fitted values",phras, sep = "\n"))+ ylab(expression(sqrt(abs("Standardized residuals")))) + ggtitle("Scale-Location")+geom_smooth(span = 1.5,colour="red",method="loess",se=FALSE)+annotation_custom(img, xmin = 1.4, xmax = 2.4, ymin = 1.4, ymax = 1.9)+theme_liu()

              return(list(plot1,plot2))
             },
             resid = function()
             {
               "This function returns the vector of calculted residual values"
               return(residualsVal)
             },
             pred = function()
             {
               "This function returns the vector of calculated fitted values"
              return(fittedValues)
             },
             coef = function()
             {
               "This function returns the vector of beta coefficients  "
              return (betaCoff)
             },
             summary = function()
             {
               "This function returns the summary of linear regression"

               r_name <- rownames(as.data.frame(betaCoff))
               cat("Call:")
               cat("\n")
               formula_print<- paste0("linreg(","formula = ",formula[2]," ",formula[1]," ",formula[3],", ","data = ",dataSetName,")",sep="")
               cat(formula_print)
               cat("\n")
               cat("\n")
               cat("Coefficients:")
               cat("\n")
               for(j in 1:length(r_name))
               {
                 cat(paste( r_name[j] ,as.numeric(round(betaCoff[j],4) ) ,round(sqrt(as.numeric(regression_coff[j])),3), round(tValues[j],3) , round(pValue[j],3), "***"   ) )
                 cat("\n")
               }
               cat("\n")
               cat(paste("Residual standard error: ",round(sqrt(residualsVariance),5) ," on " ,degree_f, " degrees of freedom",sep=""))
               cat("\n")
               cat("\n")
             }


           )



        )
data(iris)
a<-linreg$new(formula=Petal.Length ~ Sepal.Width + Sepal.Length,data=iris)
