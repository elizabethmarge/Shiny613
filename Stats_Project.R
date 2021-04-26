## Reading into the csv 

diet = read.csv("diet_1.csv")
diet
diet$weight.loss = diet$initial.weight - diet$final.weight 
diet$gender      = factor(diet$gender,levels=c("Female","Male"))
boxplot(weight.loss~diet.type,data=diet,col="light gray",
        ylab = "Weight loss (kg)", xlab = "Diet type")
abline(h=0,col="red")

## Fisher
## Welch
## Diet Kruskal 

diet.fisher  = aov(weight.loss~diet.type,data=diet)
diet.welch   = oneway.test(weight.loss~diet.type,data=diet)
diet.kruskal = kruskal.test(weight.loss~diet.type,data=diet)

summary(diet.fisher)
print(diet.welch)
print(diet.kruskal)

## Summary anova for diet 2 

summary(aov(weight.loss~diet.type,data=diet))
## there are no differences-null
# < .05 reject the null  


diet$diet.type   = factor(diet$diet.type,levels=c("1","2","3"))
col_group = rainbow(nlevels(diet$diet.type))
qqnorm(diet$weight.loss,col=col_group[as.numeric(diet$diet.type)])
qqline(diet$weight.loss)
legend("top",legend=levels(diet$diet.type),col=col_group,ncol=3,box.lwd=NA)

## t test for combining  for diet 2 
t.test(weight.loss~diet.type,data=diet[diet$diet.type!="1",],var.equal = TRUE)
t.test(weight.loss~diet.type,data=diet[diet$diet.type!="2",],var.equal = TRUE)
t.test(weight.loss~diet.type,data=diet[diet$diet.type!="3",],var.equal = TRUE)

## mean, median, 

mean  = tapply(diet$weight.loss,diet$diet.type,mean)
median = tapply(diet$weight.loss,diet$diet.type,median)
mean
median

## residual mean 
## residuals median

diet$residual.mean   = (diet$weight.loss - mean[as.numeric(diet$diet.type)])
diet$residual.median = (diet$weight.loss - median[as.numeric(diet$diet.type)])
diet[1:15,]

diet$residual.mean
## shapiro test 

shapiro.test(diet$residual.mean)


## anova test in looking at height
anova(lm(weight.loss~diet.type*Height,data=diet))

## boxplot in comparing residual means 

boxplot(residual.mean~diet.type,data=diet,main="Residual boxplot ",col="light gray",xlab="Diet type",ylab="Residuals")
abline(h=0,col="blue")

## boxplot in comparing height to weight loss and data type
plot(weight.loss~diet.type*Height,data=diet,main="Residual boxplot ",col="blue",xlab="Diet type",ylab="blah")
abline(h=3,col="blue")


## qqnorm
## qqline 

col_group = rainbow(nlevels(diet$diet.type))
qqnorm(diet$residual.mean,col=col_group[as.numeric(diet$diet.type)])
qqline(diet$residual.mean)
legend("top",legend=levels(diet$diet.type),col=col_group,pch=21,ncol=3,box.lwd=NA)

