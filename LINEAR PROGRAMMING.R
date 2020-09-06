##########################################################################################
###>>>>>>>>>>>>>>>>>>>>>>>>>>>>THE DISCUSSION>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
###The objective function in the above problem will be:
##max(Sales) = max(25X1 + 20X2)
##Where
##X1 is the units of product A produced
##X2 is the units of product B produced
##X1 and X2 are also called the decision variables
##The constraints (resource and time) in the problem
      ##20X1+12X2<=1800 (Resource Constraint)
##4X1+4X2<=8*60 (Time Constraint)

###############################################################################################
###load the package ("lpsolve")
library(lpSolve)
lp(direction = "max",objective.in, const.mat,const.dir,const.rhs)

##Set the coefficients of the decision variables
objective.in = c(25,20)

##Create constraint matrix 
const.mat = matrix(c(20,12,4,4),nrow = 2,byrow = TRUE)

##Create constraint direction
time_constraint = (8*60)
resource_constraint = 1800

##RHS for the constraint 
const.rhs = c(resource_constraint,time_constraint)

##Constraint direction
const.dir = c("<=","<=")

##Obtaining the optimal solution of the model
Optimum = lp(direction = "max",objective.in,const.mat,const.dir,const.rhs)

##Summary of the optimum value
summary(Optimum)

##Display the optimum values for X1 and X2
Optimum$solution

##check the value of objective function at the optimum point
Optimum$objval

##Interpretation of the model:
##from the above output, the company should produce 45 Units of product A and 75 Units of product B
##which would produce a maximum sales of $2625
##hence this will produce a maximum sales of the company.

