library(data.table) 

microvan <- as.data.table(lapply(microvan, as.numeric)) 

str(microvan) 
psych::describe(microvan) 
hist(microvan$mvliking,  

     xlab = "Concept liking (mvliking)", # x axis label 
     main = "Histogram",                 # plot title 
     col = "grey")          

library(tidyverse) 

microvan %>% 
  select(-"subjnumb") %>%  

  gather() %>%  
  ggplot(aes(value)) + 
  facet_wrap(~ key, scales = "free") + 
  geom_histogram() + 
  theme_minimal() 


# Scatterplot matrix for the variables 2 to 11 

pairs(microvan[, 2:11], lower.panel = NULL)  

library(corrplot) 

cor <- cor(microvan[,2:32]) 

corrplot(cor,  

         method="number",  
         type="upper",  
         order = "hclust", # reorder by the size of the correlation coefficients 
         tl.cex = 1.3,     # font size of the variable labels 
         tl.col = "black", # color of the variable labels 
         tl.srt = 45,      # rotation angle for the variable labels 
        number.cex = 1    # font size of the coefficients 
) 

regfit <- lm(mvliking ~ ., data = microvan[,2:32]) 

summary(regfit) 
microvan$residuals <- residuals(regfit) # to save the residual values 
plot(y = microvan$residuals, x = microvan$accesfun,  

     xlab = "Predictor: accesfun", ylab = "Residuals",  
     main = "Residuals vs. Predictor")  

library(olsrr) 

ols_step_both_p(regfit, pent = 0.1, prem = 0.3, details = FALSE) 

vars <- scale(microvan[,2:32]) #standartizing the content 
cor <- cor(vars) #correlation matrix 
upper<-round(cor,3) # we round the results to the 3d digit after comma 
upper[upper.tri(cor)]<-"" 
upper<-as.data.frame(upper) 
upper 

 
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


library(psych) 
scree(cor, pc = TRUE, factors = FALSE) #scree plot 


EV = eigen(cor)$values # Individual percentages of variance explained by each factor 
EV/length(EV) 
cumsum(EV/length(EV)) #cummulative variance explained 


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

 

#Unrotated factor solution 


EFA1 <- fa(r = cor,  
           nfactors = 5,  
           fm = "pa",  
           rotate = "none")  

#plot factor solution 

print(EFA1,  
      digits = 3, # to round numbers to the third digit  
      cut = 0.35, # to show only values > 0.35 
      sort = TRUE # to sort rows by loading size 
) 

#proportion of variance in each of the original variables accounted for by the first 5 factors 
sort(EFA1$communality) 
# Factor loadings 
L <- unclass(EFA1$loadings)  
round(L, 3) 

 

#factor analysis varimax 

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

 

# extract rotated factor scores  

EFA2.scores = factor.scores(vars, unclass(EFA2$loadings))$scores 

head(EFA2.scores) # to show the first 6 observations 

 

# 2. Run a cluster analysis on a distance matrix and using the Ward method 

microvan_ward <- hclust(dist(microvan[,2:32]), method="ward.D2")  

 

# Scree plot 

plot(rev(microvan_ward$height), # rev is used to plot from low to high values on Y axis 

     type = "b",           # to display both the points and lines 
     ylab = "Dissimilarity measure", 
     xlab = "Number of clusters", 
     main = "Scree plot", 
     col = "darkblue", 
     pch = 16)             # specify the plot symbol: 16 = filled circle 
abline(v = 5, lty = 2, col = "darkred") # draw a vertical line at v = 5  

 

# Dendrogram  

library(dendextend) 
plot(set(as.dendrogram(microvan_ward),   

         "branches_k_color", # to highlight the cluster solution with a color 
         k = 5), 

     ylab = "Distance", 
     main = "Dendrogram", 
     cex = 0.2)             # Size of labels 

rect.hclust(microvan_ward, k = 2, border = "darkblue")  # draw red borders around 2 clusters 
rect.hclust(microvan_ward, k = 5, border = "darkred")  # draw red borders around 5 clusters 

 

memb <- cutree(microvan_ward, k = 4) #cut the tree 4 clusters 
table(memb) #number of members per cluster 

 

cent <- NULL 
for(k in 1:4){ 
  cent <- rbind(cent, colMeans(microvan[memb == k, , drop = FALSE])) 

} 

round(cent[, 2:32], 3)# finding cetroid starting points 

 

set.seed(1) 
microvan_kmeans <- kmeans(microvan[, 2:32], centers = cent[, 2:32], iter.max = 10) 
microvan_kmeans 
str(microvan_kmeans)  

 
microvan_kmeans$size #size of final solution 
microvan_kmeans$centers #centers from the final solutio' 

 

#change in each kluster from the k-means clustering 

change <- NULL 

for (i in 1:4){ 
  change <- rbind(change, microvan_kmeans$centers[i,]-cent[i,2:32]) 
} 

 #difference between original cluster centers and wards methode 

round(change, 3) 

 

#matrix of pairwise distance btween cluster centers 

dist(microvan_kmeans$centers) 

 

#result interpretation 

#store cluster solution in the dataset 

microvan <- cbind(microvan, cluster = microvan_kmeans$cluster) 


#overall average in the sample 

round(colMeans(microvan[, 2:32]), 3) 

#average for cluster 1 

round(colMeans(microvan[microvan$cluster == 1, 2:32]), 3) 

# Average for each cluster with one step 

aggregate(microvan[, 2:32], 

          by = list(cluster = microvan$cluster),  
          FUN = mean) 

library(factoextra) 
fviz_cluster(microvan_kmeans, data = microvan) +  
 theme_bw() 
