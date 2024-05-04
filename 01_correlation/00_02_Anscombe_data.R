# setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\00_basics\\02_correlation")
setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\00_basics\\02_correlation")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)




# ------------------------------------------------------------------------------
# Read Anscombe's data
# ------------------------------------------------------------------------------


data <- read.csv("Anscombe_data.txt", header = T, stringsAsFactors = FALSE, sep = "\t")


str(data)


data




# ------------------------------------------------------------------------------
# data explorationo
# ------------------------------------------------------------------------------


summary(data)					





# ------------------------------------------------------------------------------
# plot data
# ------------------------------------------------------------------------------

result1 <- lm(y1~x1, data=data)					

r2_1 <- round(1 - var(resid(result1))/var(data$y1), 2)					



result2 <- lm(y2~x2, data=data)					

r2_2 <- round(1 - var(resid(result2))/var(data$y2), 2)					



result3 <- lm(y3~x3, data=data)					

r2_3 <- round(1 - var(resid(result3))/var(data$y3), 2)					



result4 <- lm(y4~x4, data=data)					

r2_4 <- round(1 - var(resid(result4))/var(data$y4), 2)					



# ----------
op <- par(mfrow=c(2,2), mar=c(4,4,1,1), oma=c(0,0,2,0))					


for(i in 1:4){					
  xname <- paste("x", i, sep="");  yname <- paste("y", i, sep="")
  
  plot(data[,xname], data[,yname], xlim=c(0,20), ylim=c(0,20), xlab=xname, ylab=yname, pch = 20, cex = 2, col = "blue")
  
  abline(eval(parse(text=paste("result", i, sep=""))), lty = 2)

  mtext(paste("R^2=", eval(parse(text=paste("r2_", i, sep="")))), cex=1.5)
}

par(op)					




# ----------
# Note that all models are significant ..

summary(result1)

summary(result2)

summary(result3)

summary(result4)




# ------------------------------------------------------------------------------
# Pearson's product moment correlation
# ------------------------------------------------------------------------------

# Note that all correlation coefficients are 0.816 (or 0.817)

round(cor(data$x1, data$y1), 3)


round(cor(data$x2, data$y2), 3)


round(cor(data$x3, data$y3), 3)


round(cor(data$x4, data$y4), 3)




# ------------------------------------------------------------------------------
# Spearman's correlation coefficients
# ------------------------------------------------------------------------------


round(cor(data$x1, data$y1, method="spearman"), 3)


round(cor(data$x2, data$y2, method="spearman"), 3)


round(cor(data$x3, data$y3, method="spearman"), 3)


round(cor(data$x4, data$y4, method="spearman"), 3)



