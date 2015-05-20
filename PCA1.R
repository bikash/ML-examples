bumpus <- read.table("http://www.ndsu.nodak.edu/ndsu/doetkott/introsas/rawdata/bumpus.html", 
                     skip=20, nrows=49, 
                     col.names=c("id","total","alar","head","humerus","sternum"))

boxplot(bumpus, main="Boxplot of Bumpus' data") ## in this step it is showing the ERROR

# we first standardize the data:
bumpus.scaled <- data.frame( apply(bumpus,2,scale) )

library(corrplot)
corMatMy <- cor(bumpus.scaled)
#compute the correlation matrix

corrplot(corMatMy, order = "hclust")
#visualize the matrix, clustering features by correlation index.

boxplot(bumpus.scaled, main="Boxplot of standardized Bumpus' data")

pca.res <- prcomp(bumpus.scaled, retx=TRUE)
pca.res
# note:
# PC.1 is some kind of average of all the measurements 
#    => measure of size of the bird
# PC.2 has a negative weight for 'sternum' 
#    and positive weights for 'alar', 'head' and 'humerus'
#    => measure of shape of the bird

# first two principal components:
pca.res$x[,1:2]
plot(pca.res$x[,1:2], pch="", main="PC.1 and PC.2 for Bumpus' data (blue=survived, red=died)")
text(pca.res$x[,1:2], labels=c(1:49), col=c(rep("blue",21),rep("red",28)))
abline(v=0, lty=2)
abline(h=0, lty=2)

# compare to segment plot:
windows()
palette(rainbow(12, s = 0.6, v = 0.75)) 
stars(bumpus, labels=c(1:49), nrow=6, key.loc=c(20,-1), 
      main="Segment plot of Bumpus' data", draw.segment=TRUE) 

# compare to biplot:
windows()
biplot(pca.res, scale=0)
# what do the arrows mean?
# consider the arrow for sternum:
abline(0, pca.res$rotation[5,2]/pca.res$rotation[5,1])
# consider the arrow for head:
abline(0, pca.res$rotation[3,2]/pca.res$rotation[3,1])


# Different solution


pca.reduced <- function(X, center=TRUE, retX=TRUE) {
  # Note that the data must first be centered on the *original* dimensions
  # because the centering of the 'transposed covariance' is meaningless for
  # the dataset. This is also why Sigma must be computed dependent on N
  # instead of simply using cov().
  if (center) {
    mu <- colMeans(X)
    X <- sweep(X, 2, mu, `-`)
  }
  # From now on we're looking at the transpose of X:
  Xt <- t(X)
  aux <- svd(Xt)
  V <- Xt %*% aux$v
  # Normalize the columns of V.
  V <- apply(V, 2, function(x) x / sqrt(sum(x^2)))
  # Done.
  list(X = if (retX) X %*% V else NULL,
       V = V,
       sd = aux$d / sqrt(nrow(X)-1),
       mean = if (center) mu else NULL)
}

# Example data (low-dimensional, but sufficient for this example):
X <- cbind(rnorm(1000), rnorm(1000) * 5, rnorm(1000) * 3)

boxplot(X, main="Boxplot of Bumpus' data") ## in this step it is showing the ERROR


original   <- prcomp(X, scale=FALSE)
transposed <- pca.reduced(X)


