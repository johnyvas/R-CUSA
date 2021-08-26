vars <- scale(microvan[,3:32]) #standardizing the content 
cor <- cor(vars) #Analyse the correlation matrix 
upper<-round(cor,3) # we round the results to the 3d digit after comma 
upper[upper.tri(cor)]<-"" 
upper<-as.data.frame(upper) 
upper 

#As the correlation matrix is rather large (30 dimensions), 
#we will visualize with the heatmap

library(corrplot) #plot correlation matrix 
corrplot(cor,  
         
         method = "number",  
         type = "upper",  
         order = "hclust", # reorder by the size of the correlation coefficients 
         tl.cex = 0.8, # font size of the variable labels 
         tl.col = "black", # color of the variable labels 
         tl.srt = 45, # rotation angle for the variable labels 
         number.cex = 0.5 # font size of the coefficients 
) 



#find the INITIAL eigenvalues

EV = eigen(cor)$values # Individual percentages of variance explained by each factor 
EV
#all eigenvalues sum to 1 the last one is zero
EV/length(EV) #dividing eigenvalue of the dimension (in our case, 30).
# explains the individual percentage of variance 
# by a dimension eg. the first dimension EV=8.27 accounts for the 27,5% of variance

cumsum(EV/length(EV)) #cummulative variance explained 
# how much of variance the account for.

library(psych) 
scree(cor, pc = TRUE, factors = FALSE) #scree plot 
#This suggests that 5 dimensions may be sufficient to capture the variability in the data
#The 5 account for 69,58% 


# Shares for the cumulative variance explained 

plot(cumsum(EV/length(EV)),  
     
     type = "o", # type of plot: "o" for points and lines 'overplotted' 
     col = "darkblue", 
     pch = 16, # plot symbol: 16 = filled circle 
     cex = 1, # size of plot symbols 
     xlab = "Number of factors", # a title for the x axis 
     ylab = "Cumulative variance explained", # a title for the y axis 
     lwd = 2) # line width 
abline(v = 4, lwd = 2, col = "grey") # draw a vertical line at v = 4 



#Exploratory factor analysis (EFA) - factor rotation


#Unrotated factor solution 

EFA1 <- fa(r = cor,  
           nfactors = 5,  #number of factors to extract
           fm = "pa",  #specifies the factor estimation method 
           rotate = "none")  # indicates the rotation method 

#plot Unrotated factor solution 

print(EFA1,  
      digits = 3, # to round numbers to the third digit  
      cut = 0.35, # to show only values > 0.35 
      sort = TRUE # to sort rows by loading size 
) 

                    #  PA1   PA2   PA3   PA4   PA5
#SS loadings           7.915 4.603 2.801 2.439 1.445
#Proportion Var        0.264 0.153 0.093 0.081 0.048 the first factor explains 26.4% of the variation
#Cumulative Var        0.264 0.417 0.511 0.592 0.640 those 5 factors make all 64.0%

#proportion of variance in each of the original variables accounted for by the first 4 factors 

sort(EFA1$communality) # communalities in increasing order from 0.43-0.92 
# eg. variable "pricqual" has a communality value (1-0.5591637)= 0.44 implies that 44% of the variation
#  is unique to "pricqual" and is not explained by the factors 

# Factor loadings store
L <- unclass(EFA1$loadings)  
round(L, 3) 


#ROTATED factor solution 
#analysis varimax 

EFA2 <- fa(r = cor,  
           nfactors = 5,  
           fm = "pa",  
           rotate = "varimax")  

#print factor analysis 


print(EFA2,  
      digits = 3, # to round numbers to the third digit  
      cut = 0.35, # to show only values > 0.35 
      sort = TRUE # to sort rows by loading size 
) 



EFA2$weights 
# We take the coefficients to calculate factor scores 
# for each of the observation 

# extract rotated factor scores  
EFA2.scores = factor.scores(vars, unclass(EFA2$loadings))$scores 
head(EFA2.scores) # to show the first 6 observations 

#[1,] -0.830170151  0.2294461  0.7153434 -0.9273900 -1.6251184
#[2,]  0.023738569  1.2563808 -1.0902308  0.8797055  0.2968818
#[3,]  0.020409382 -1.7668302  1.5286434  0.8019823  0.2180403
#[4,] -0.002497453  0.2535720 -1.5180369 -1.4110212  0.6299620
#[5,] -0.283949325 -1.6921567 -0.2582154  0.6368648 -0.1865315
#[6,]  0.444262014 -2.4334538 -0.6837478  1.5975919  0.1237065

microvan.scores <- cbind(microvan, EFA2.scores)
head(microvan.scores)
