observed <- c(.22, .83, -.12, .89, -.23, -1.3, -0.15, -1.4, .62, .99, 
              -.18, .32, .34, -.30, .04, -.87, .55, -1.3, -1.15, .20)
predicted <- c(.24, .78, -.66, .53, .7, -.75, -.41, -.43, .49, .79,
               -1.19, .06, .75, -.07, .43, -.42, -.25, -.64, -1.26, -.07)
length(observed)
length(predicted)

library(caret)
residualValues <- observed - predicted
summary(residualValues)

plot(observed, predicted)
abline(0, 1, col = "darkgrey", lty = 2)

plot(predicted, residualValues, ylab = "residual")
abline(h = 0, col = "darkgrey", lty = 2)

R2(predicted, observed)
RMSE(predicted, observed)

cor(predicted, observed)^2

cor(predicted, observed, method = "spearman")
