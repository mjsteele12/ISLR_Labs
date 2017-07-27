#Create truth function
f = function(x) {
 x ^ 3 
}

#Function to generate data
data = function(f, n = 100) {
    x = runif(n = n, min = -1, max = 1)
    y = f(x) + rnorm(n = n, mean = 0, sd = 0.2)
    data.frame(x, y)
}

#Create sample data, separate x,y required for spline
data = data(f, n = 100)
x = data$x
y = data$y

#Train linear regression, second & third degree polynomial, and spline
linear = lm(y ~x, data = data)
poly2 = lm(y ~ poly(x, degree = 2), data = data)
poly3 = lm(y ~ poly(x, degree = 3), data = data)
spline = smooth.spline(x=x,y=y, spar = 0.3)

#To save a higher resolution output, uncomment below and at the end of the script. The image will
#be stored in your current working directory
#png("Bias_Variance_Plot.png", width = 3700, height = 3500, res = 1000)

#Set seed for random data, plot datapoints
set.seed(100)
plot(y ~ x, data = data)

#New data to predict on 
newdata = seq(from = -10, to = 10, by = 0.2)

#Predict on newdata, plot as line on chart
lines(newdata, predict(linear, newdata = data.frame(x = newdata)), 
      col = "midnightblue", lwd = 3, lty = 1)

lines(newdata, predict(poly2, newdata = data.frame(x = newdata)),
      col = "red", lwd = 3, lty = 1)

lines(newdata, predict(poly3, newdata = data.frame(x = newdata)), 
      col = "yellow2", lwd = 3, lty = 1)

lines(predict(spline, newdata=data.frame(x = newdata)),
      col = "grey40", lwd = 3, lty = 1)

#Plot ground truth using our defined function
lines(newdata, f(newdata), col = "black", lwd = 5)

#Plot aesthetics
legend('bottomright', c("linear", "poly ^ 2", "poly ^ 3", "spline", "truth"), 
       col = c("midnightblue", "red", "yellow2", "grey20", "black"), lty = c(1, 1, 1, 1, 1), 
       lwd = 3, cex=.8,text.width=.3, bty='n', y.intersp = .75)

title("Model Flexibility and Bias-Variance", line = -2)

#For a higher resolution output, uncomment below
dev.off()