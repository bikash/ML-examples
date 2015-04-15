# Load data
data(iris)
head(iris, 3)

# log transform 
log.ir <- log(iris[, 1:4])
ir.species <- iris[, 5]

# apply PCA - scale. = TRUE is highly 
# advisable, but default is FALSE. 
ir.pca <- prcomp(log.ir,
                 center = TRUE,
                 scale. = TRUE)
# print method
print(ir.pca)

# plot method
plot(ir.pca, type = "l")


# Predict PCs
predict(ir.pca,  newdata=tail(log.ir, 2))

library(devtools)
install_github("ggbiplot", "vqv")

library(ggbiplot)
g <- ggbiplot(ir.pca, obs.scale = 1, var.scale = 1, 
              groups = ir.species, ellipse = TRUE, 
              circle = TRUE)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', 
               legend.position = 'top')
print(g)



## example 2
library(FactoMineR)
data(decathlon)

res.pca = PCA(decathlon[,1:10], scale.unit=TRUE, ncp=5, graph=T)

#decathlon: the data set used
#scale.unit: to choose whether to scale the data or not 
#ncp: number of dimensions kept in the result
#graph: to choose whether to plot the graphs or not


