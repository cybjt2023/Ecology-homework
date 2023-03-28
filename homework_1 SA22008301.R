# Ecology Homework 1, Author: caoying, Student ID: SA22008301
#1.
#Loading libraries of tidyverse and ade4, 
#as well as the doubs data into R.
install.packages("ade4")   #install package ade4
install.packages("tidyverse")   #install package tidyverse
library(ade4)              #load library of the package ade4
library(tidyverse)         #load library of the package tidyverse
help(package="tidyverse")
data(doubs,package="ade4") #extract and load "doubs" data from the ade4 package
?doubs
#checking what the data looks like and the class of the data. 
str(doubs)        #checking what the data looks like
class(doubs)      #checking the class of the data     
#2.
#extracting data and checking it.
env<-doubs$env    #Extract env data from doubs
class(env)        #checking the class of env data
env_row <- nrow(env)
env_col <- ncol(env)
#Turning the row names into a column called site
site<-row.names(env)
env_dataframe<-data.frame(site,env) 
#converting the data frame to a tibble, named it env_tb.
env_tb<-as_tibble(env_dataframe)    
class(env_tb)     #checking the class of new env data
#3.
#Concatenating several steps with %>% pipe,
#and name the final variable as env_final.
#3.1 
#Extract and remain the data of the dfs with more than 1000 km.
dfs<-env_tb$dfs
env_tb[dfs>1000,]   #3.1
env_final<-env_tb[env_tb$dfs>1000,] %>%   #3.
#3.2
#Selecting columns for further analysis.
#columns:site, dfs, slo, flo, pH, nit, oxy.
select(site, dfs, slo, flo, pH, nit, oxy)%>% 
#3.3
#Renaming Some column names as follows:
#dfs to distsour, slo to slope,flo to flowrate, nit to nitrogen, oxy to oxygen.
rename(distsour = dfs, slope =slo , flowrate =flo ,  nitrogen =nit , oxygen =oxy )%>%  
#3.4
#Arranging the data first by slope in ascending order, 
#and then by pH in descending order.
arrange(slope,desc(pH))  
