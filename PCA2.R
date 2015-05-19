# R-code for principal component analysis (PCA)

###########################
# Example in 2 dimensions #
###########################


# generate some data:
a1 <- rnorm(100)
a2 <- rnorm(100)
x1 <- a1 
x2 <- -sqrt(2/3)*a1 + sqrt(1/3)*a2

# standardize the data to have mean 0 and variance 1:
x1 <- (x1 - mean(x1))/sd(x1)
x2 <- (x2 - mean(x2))/sd(x2)
data <- data.frame(x1,x2)

# plot the data in a scatterplot:
#    pty="s" gives a square plotting region 
#   (type '?par' to get the help file on plotting options)
plot(data, xlim=c(-4,4), ylim=c(-4,4), pty="s", 
     main="Scatterplot of the data")

# scatterplot where the numbers of the observations are indicated:
#    pch="" suppresses the plotting of points 
plot(data, xlim=c(-4,4), ylim=c(-4,4), pty="s", pch="",
     main="Scatterplot of the data")
text(data, labels=c(1:100))

# apply PCA
pca.res <- prcomp(data, retx=TRUE)
pca.res
summary(pca.res)

# detailed output of PCA:
pca.res$sdev     # standard deviations of the principal components
pca.res$rotation # matrix of variable loadings
pca.res$x        # if scores=TRUE, scores of the data on the 
# principal components

# rotate data by hand: [y1,y2] = [x1,x2] %*% pca.res$rotation 
rot <- pca.res$rotation
y1 <- rot[1,1]*x1 + rot[2,1]*x2
y2 <- rot[1,2]*x1 + rot[2,2]*x2

# note that this matches up with pca.res$x:
round(pca.res$x - cbind(y1,y2), 10)

# also:
round(pca.res$x - as.matrix(data) %*% pca.res$rotation, 10)

# plot rotated axes:
abline(0,rot[2,1]/rot[1,1], col="red", lwd=2)    # first PCA axis, largest variance
abline(0,rot[2,2]/rot[1,2], col="blue", lwd=2)   # second PCA axis, smallest variance
legend(2,0.5, c("PC.1","PC.2"), lty=c(1,1), col=c("red","blue"), lwd=c(2,2))

# does the direction of the first principal component 
# correspond to the linear regression of x2 on x1?
fit <- lm(x2 ~ x1)
abline(fit, lty=2)

# check that standard devations and variances of y1 and y2 match up
# with summary(pca.res)
summary(pca.res)
sd(y1)
sd(y2)
# total variance in original data:
total.var <- cov(data)[1,1] + cov(data)[2,2]
total.var
# proportional variance:
var(y1)/total.var
var(y2)/total.var

# check that correlation between y1 and y2 is zero:
round(cor(y1,y2), 10)

# plot transformed data in new plotting window:
windows()    			# open new plotting window
plot(pca.res$x, pch="", xlim=c(-4,4), ylim=c(-4,4))
text(pca.res$x, labels=c(1:100))
abline(h=0, col="red", lwd=2)
abline(v=0, col="blue", lwd=2)
# note that data might flip... 

# to unflip the data:
if (rot[1,1]<0) rot[,1] <- rot[,1]*-1
if (rot[1,2]<0) rot[,2] <- rot[,2]*-1
rot
y1 <- rot[1,1]*x1 + rot[2,1]*x2
y2 <- rot[1,2]*x1 + rot[2,2]*x2
windows()
plot(y1,y2, pch="", xlim=c(-4,4), ylim=c(-4,4))
text(y1,y2, labels=c(1:100))
abline(h=0, col="red", lwd=2)
abline(v=0, col="blue", lwd=2)

# compare to biplot with scale=0:
windows()
biplot(pca.res, scale=0, xlim=c(-4,4), ylim=c(-4,4))

# default biplot is similar, but axes are scaled differently:
windows()
biplot(pca.res)


###########################
# Example on Bumpus' data #
###########################

bumpus <- read.table("http://www.ndsu.nodak.edu/ndsu/doetkott/introsas/rawdata/bumpus.html", 
                     skip=20, nrows=49, 
                     col.names=c("id","total","alar","head","humerus","sternum"))
bumpus <- bumpus[,-1]
boxplot(bumpus, main="Boxplot of Bumpus' data")

# we first standardize the data:
bumpus.scaled <- data.frame( apply(bumpus,2,scale) )
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


##############################
# PCA is not scale invariant #
##############################

# only center Bumpus' data, don't force variance 1:
bumpus.centered <- data.frame(apply(bumpus, 2, scale, scale=FALSE))
boxplot(bumpus.centered)

# apply PCA:
pca.res.2 <- prcomp(bumpus.centered, retx=TRUE)
pca.res.2

# compare to PCA on standardized data that is scaled back to original scale:
# pca on standardized data:
pca.res
# y1 = 0.451 * total/sd(total) + 0.461 * alar/sd(alar) + ...
# y1 = 0.451/sd(total) * total + 0.461/sd(alar) * alar + ...
# y1 = 0.124 * total + 0.0911 * alar + ...
a <- pca.res$rotation[,1]/apply(bumpus,2,sd)
# scale coefficients by factor c, so that sum of squared coefficients = 1:
c <- sqrt(sum(a^2))
a <- a/c
sum(a^2)
# y1 = 0.113*total + 0.083*alar + 0.517*head + 0.761*humerus + 0.366*sternum
# very different from results in pca.res.2: 
# y1* = 0.536*total + 0.829*alar + 0.096*head + 0.074*humerus + 0.100*sternum

# also proportion of the variance explained is different:
summary(pca.res)
summary(pca.res.2)


#########################################
# How many principal components to use? #
#########################################

# use a screeplot:
plot(pca.res)
screeplot(pca.res)
screeplot(pca.res, type="lines")

# check that it simply plots the variances of the principal components:
apply(pca.res$x, 2, var)

# correlation between original variables and principal components
corr <- cor(bumpus.scaled, pca.res$x)
round(corr, 2)

# correlation^2: amount of variation of each of the variables that
# is explained by the principal components:
round(corr^2, 2)

# note the rows sum to 1:
apply(corr^2, 1, sum)


##################
# USJudgeRatings #
##################

pca.res <- prcomp(USJudgeRatings, center=TRUE, retx=TRUE)
pca.res
screeplot(pca.res)
biplot(pca.res, scale=0, xlim=c(-6,9), ylim=c(-6,9))
windows()
plot(pca.res$x[,1:2], xlim=c(-6,9), ylim=c(-6,9), pch="")
text(pca.res$x[,1:2], labels=abbreviate(row.names(USJudgeRatings)), col="blue")

# compare to star plot:
windows()
USJudge <- apply(USJudgeRatings, 2, function(x) x/max(x))
loc <- stars(USJudge, labels = NULL, scale = FALSE,
             radius = FALSE, frame.plot = TRUE,
             key.loc = c(13, 1.5), main = "Judge data", 
             len = 1.2)
text(loc, abbreviate(row.names(USJudgeRatings)), 
     col = "blue", cex = 0.8, xpd = TRUE)

# correlation between original variables and principal components
corr <- cor(USJudgeRatings, pca.res$x)
round(corr, 2)

# correlation^2: amount of variation of each of the variables that
# is explained by the principal components:
round(corr^2, 2)

# note the rows sum to 1:
apply(corr^2, 1, sum)


### IRIS data example

###pca - calculated for the first 4 columns of the data set that correspond to biometric measurements ("Sepal.Length" "Sepal.Width"  "Petal.Length" "Petal.Width")
data(iris)
dat <- as.matrix(iris[,-5])
pca <- prcomp(dat, retx=TRUE, center=TRUE, scale=TRUE)

###Create new data sets for each of the three species. 
#Biometric values are based on the distributions of the original data means
#and the covariances between these parameters. 
setosa.mean <- apply(iris[iris$Species=="setosa",-5], 2, mean)
setosa.cov <- cov(iris[iris$Species=="setosa",-5])

versicolor.mean <- apply(iris[iris$Species=="versicolor",-5], 2, mean)
versicolor.cov <- cov(iris[iris$Species=="versicolor",-5])

virginica.mean <- apply(iris[iris$Species=="virginica",-5], 2, mean)
virginica.cov <- cov(iris[iris$Species=="virginica",-5])

#Make new random data based on the calculated biometry info. each species
#The MASS package allows for the calculation of correlated/covarying random 
#numbers using this information.
require(MASS)
set.seed(1)
n <- 30
new.setosa <- mvrnorm(n, setosa.mean, setosa.cov)
new.versicolor <- mvrnorm(n, versicolor.mean, versicolor.cov)
new.virginica <- mvrnorm(n, virginica.mean, virginica.cov)

###Predict PCs by projecting the new data using the predict.prcomp function
pred.setosa <- predict(pca, new.setosa)
pred.versicolor <- predict(pca, new.versicolor)
pred.virginica <- predict(pca, new.virginica)

###Plot result
SPP <- iris$Species
COLOR <- c(2:4)
PCH <- c(1,16)

pc <- c(1,2)
plot(pca$x[,pc[1]], pca$x[,pc[2]], col=COLOR[SPP], cex=PCH[1], xlab=paste0("PC ", pc[1], " (", round(pca$sdev[pc[1]]/sum(pca$sdev)*100,0), "%)"), ylab=paste0("PC ", pc[2], " (", round(pca$sdev[pc[2]]/sum(pca$sdev)*100,0), "%)"))
points(pred.setosa[,pc[1]], pred.setosa[,pc[2]], col=COLOR[levels(SPP)=="setosa"], pch=PCH[2])
points(pred.versicolor[,pc[1]], pred.versicolor[,pc[2]], col=COLOR[levels(SPP)=="versicolor"], pch=PCH[2])
points(pred.virginica[,pc[1]], pred.virginica[,pc[2]], col=COLOR[levels(SPP)=="virginica"], pch=PCH[2])
legend("topright", legend=levels(iris$Species), col=COLOR, pch=17)
legend("topleft", legend=c("Original data", "New data"), col=1, pch=PCH)
