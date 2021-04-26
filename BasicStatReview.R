
# Review of Basic Btatistics

library(tidyverse)
#install.packages("Sleuth3")
library(Sleuth3)
library(broom)

# This is not an attempt to cover all important concepts in a standard statistics
# course, we will only focus on and feature key elements of statistics needed for
# our course

# Let's generate a normal curve

x <- seq(-4, 4, length=1000)
y <- dnorm(x, mean=0, sd=1)
plot(x, y, type="l", lwd= 1)


# 1 Find the height of the normal curve for an x value of -2

qnorm(p = 0.65, mean = 75, sd = 3.25) # the function dnorm produces the height of the
                                # the normal curve at a specified value.

# note that the symmetric property of the normal curve will give the same
# answer for an x value of 2.

dnorm(x = 2, mean = 0, sd = 1)

# 2 The rnorm function will randomly produce specified number of normally distri-
# buted values that are randomly selected, given a mean and a standard deviation.

rnorm(n = 550, mean = 1, sd = 1) 

rnorm(n = 200, mean = 3, sd = 2.75)

# 3 The pnorm function will produce the area under the normal curve that is to the
# left of a given value.  This area is also a probability designation.

pnorm(q = 2, mean = 1, sd = 1)

# 3a For a normal distribution with a mean of 40 and a standard deviation of 5,
#    Find the area under the curve the that is less than or equal to 43.
pnorm(q = 43, mean = 40, sd = 5)

# 3b Test scores are normally distributed with a mean of 70 and a standard
# deviation of 4.75. Find the probability that a student will scored above 79 on
# the test.
pnorm(q = 79, mean = 70, sd = 4.75) # this will give the probability of getting a
# score that is below 79.

# Now subtract your answer from 1

1 - 0.9709364

# 3c Test scores are normally distributed with a mean of 70 and a standard
# deviation of 4.75. Find the probability that a student will scored between 65 and
# 85 on the test.

#  Step 1  Find the probability for getting less than the upper bound
pnorm(q = 85, mean = 70, sd = 4.75)     # 0.9992054

#  Step 2  Find the probability for getting less than the lower bound
pnorm(q = 65, mean = 70, sd = 4.75)    # 0.1462549

#  Step 3  Subtract!!  Step 1 answer -  Step 2 answer  0.9992054 - 0.1462549

0.9992054 - 0.1462549


# 4 qnorm is called the quantile function. Given a mean and a standard deviation,
# it will give a value that an indicated probability (area) is to the left of.
qnorm(p = 0.8413, mean = 60, sd = 5)

#4a  Test scores are normally distributed with a mean of 75 and a standard 
# deviation of 3.25. 65% of the scores are less than what score?
qnorm(p = 0.65, mean = 75, sd = 3.25)

# #4b  Test scores are normally distributed with a mean of 75 and a standard 
# deviation of 3.25. 65% of the scores are greater than what score?

 qnorm(p = 0.35, mean = 75, sd = 3.25)


# T Distribution

 # The t-distribution shows up a lot in Statistics.
 
 # It is also bell-curved but has "thicker tails" (more extreme observations are 
 # more likely).
 # It is always centered at 0.
 # It only has one parameter, called the "degrees of freedom", which determines
 # how thick the tails are.
 # Smaller degrees of freedom mean thicker tails, larger degrees of freedom means
 # thinner tails.
 # If the degrees of freedom is large enough, the t-distribution is 
 # approximately the same as a normal distribution with mean 0 and variance 1.

# Plotting a T distribution
 
 t.values <- seq(-4,4,.1)
 plot(x = t.values,y = dt(t.values, 2), type = "l", lty = "dotted", 
      ylim = c(0,.4), xlab = "t", ylab = "f(t)")
 
 
 dt(x = -2, df = 2)
 
 
 dt(x = -.5, df =2)
 
 # Hypothesis Testing (T test)
 
 
 #One sample t test example
 # A random sample from a population has been obtain. Should we reject the
 # Null hypothesis that the mean of the population is 100?

 
 c(119,131,115,107,125,96,128,99,103,103,105,109)->k
 k
 
 # Lets check common conditions for using a t test for Hypothesis Testing
 # Check for outliers in the sample by looking at the boxplot.
 boxplot(k)
 
 # Check for normality
 qqnorm(k)
 qqline(k)
 
 # H(0) :  population mean = 100
 # H(A) :  population mean does not equal 100
 
 t.test(k,mu=100, alternative = "two.sided", conf.level = .95)
 
 #Two Sample T test
 
 
 #One sample t test example
 # Two random samples from different populations has been obtain.Should we 
 # reject the Null hypothesis that the means of the population are the equal?
 
 # Assume that the required conditions for executing this test have been met.
 
 # We have two populations  L and D
 
 # H(O) : population mean for L = population mean for D  (Null hypothesis)
 # H(A) : population mean for L does not equal the population mean for D
 #        (Alternative Hypothesis)
 
 # The two samples are given below
 
 c(119,131,115,107,125,96,128,99,103,103,105,109)->L
 L
 
 c(120,140,112,109,114,116,99,108,109,111,109,131,117,101)->D
 D
 
 t.test(L,D, mu =0, var.equal=FALSE,)
 
 # If your p value is > than .05 you cannot reject the Null hypothesis
 # If 0 is in your confidence interval you cannot reject the Null hypothesis.
 


q()
y
