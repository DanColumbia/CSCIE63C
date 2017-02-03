# Problem 1
p <- c(0.3, 0.5, 0.8)

old.par <- par(mfrow=c(1,3),ps=16)
for ( iTry in 1:3 ) {
  x=rbinom(1000,60,p[iTry])
  hist(x,breaks=10,col='lightgreen',main=paste("P =",p[iTry]), xlab = "# of successful trials")
}
par(old.par)

result <- data.frame()
for(iTry in 1:3){
  x = rbinom(1000, 60, p[iTry])
  vector <- c(quantile(x, 0.25), 
              quantile(x, 0.5),
              mean(x),
              sd(x),
              quantile(x, 0.75)
  )
  result_now <- as.data.frame(vector)
  result_now$p <- p[iTry]
  result <- rbind(result, result_now)
}
boxplot(formula=vector~p, data=result)


boxplot(x)

# Problem 2
model <- lm(formula = waiting ~ eruptions, data = faithful)
plot(faithful)
abline(model, col='red', lwd=5)

# Problem 3
faithful$type <- ifelse(faithful$eruptions<3.1,'short', 'long')
boxplot(formula=eruptions~type, data=faithful, main="eruptions vs type")
boxplot(formula=waiting~type, data=faithful, main="waiting vs type")

# Problem 4
p4vector <- runif(1000, min=-1, max=2)
hist(p4vector, breaks = 20)
plot(ecdf(p4vector))

# Problem 5
p5matrix <- matrix(data=runif(100*40, min=-1, max=2), nrow = 100, ncol = 40)
y1 <- p5matrix[,1]
y2 <- p5matrix[,2]
plot(p4vector[1:100], y1, main = 'random variable vs random variable', xlab = 'x', ylab = 'y1 - in dot, y2 - in line')
lines(p4vector[1:100], y2, col='red')

# Problem 6
p5matrix <- data.frame(p5matrix)
p5matrix$sum <- rowSums(p5matrix)
hist(p5matrix$sum, breaks=20, freq = F, main = 'empirical & true density', xlab = 'random variable value')
lines(dnorm(0:40, mean=20, sd=sqrt(0.75*40)))
