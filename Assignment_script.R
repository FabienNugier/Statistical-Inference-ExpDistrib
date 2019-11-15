# Part I - Simulations of the Exponential Distribution

lambda = 0.2  # rate parameter
n = 40        # population within one sample
Nsim = 1000   # number of samples (simulations)

# For comparison
# hist(runif(1000))
# mns = NULL
# for (i in 1 : 1000) mns = c(mns, mean(runif(40)))
# hist(mns)

# Testing one sampling of the Exp Distrib:
set.seed(12345)
hist(rexp(n,lambda), xlab="x", main="Histogram of x ~ Exp(0.2)", col="blue")

# Sampling 1000 times and showing the distribution of the sample mean:
X = NULL
set.seed(123)
for (i in 1:Nsim) X = c(X, mean(rexp(n,lambda)))
m <- mean(X) # We take the average of the sample means
s <- sd(X)   # We take the standard deviation of the sample means
print(paste("mean:",round(m,3),", standard error:",round(s,3)))

# Plotting:
# Initializing
new.mai <- old.mai <- par("mai")
new.mai[4] <- old.mai[2]
par(mai = new.mai)
# Plot 1 preparation
X <- mns
h <- hist(X, breaks=50, plot=FALSE)
pos <- pretty(h$density, n = 5)
freq <- round(pos * length(X) * with(h, breaks[2] - breaks[1]))
# Plot 2 preparation
xseq <- seq(0,10,0.2)
mth <- 1/lambda # theoretical mean
sth <- (1/lambda)/sqrt(n) # theoretical standard error of the mean
# Plot 1
graphics:::plot.histogram(h, freq = FALSE, col="light blue", main="Histogram of the Sample Mean",
                          xlab = "sample mean", ylab="Frequency",
                          border="black", yaxt='n')
Axis(side = 2, at = pos, labels = freq)
Axis(side = 4, at = pos, labels = pos)
mtext("Density", side = 4, line = 3)
# Plot 2
lines(xseq, dnorm(xseq,mth,sth), type='l', lwd=2)
# Finalization
par(mai = old.mai)





# Part II - Inferential Data Analysis on the ToothGrowth dataset

# Basic infos:
data(ToothGrowth)
head(ToothGrowth,n=3)
dim(ToothGrowth)
str(ToothGrowth)
summary(ToothGrowth$supp)
unique(ToothGrowth$dose)














