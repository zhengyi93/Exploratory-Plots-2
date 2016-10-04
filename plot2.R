library(RColorBrewer)

mydata <- read.csv("_e143dff6e844c7af8da2a4e71d7c054d_payments.csv", stringsAsFactors = F)
data2 <- log10(mydata[, c("Average.Covered.Charges", "Average.Total.Payments")])

conditionTypes <- unique(mydata$DRG.Definition)
color <- sapply(brewer.pal(6, "Spectral"), function(col){
    rgb.val <- col2rgb(col)
    rgb(rgb.val[1], rgb.val[2], rgb.val[3], 81, maxColorValue = 255)
})

par(mfrow=c(4, 2), oma = c(0, 0, 2, 0))
for (state in unique(mydata$Provider.State)){
    plot(data2[, 1][mydata$Provider.State == state], data2[, 2][mydata$Provider.State == state], 
         xlab = "log( Mean Covered Charges ($) )", ylab = "log( Mean Total Payments ($) )",
         main = state, pch = 16, col = rgb(1, 0, 0.2, 0.2), type = "n")
    
    for(i in seq(length(conditionTypes)))
        points(data2[, 1][mydata$DRG.Definition == conditionTypes[i] & mydata$Provider.State == state], data2[, 2][mydata$DRG.Definition == conditionTypes[i] & mydata$Provider.State == state], col = color[i] , pch = 16)
}

mtext("Plots of each state with varying medical conditions", outer = TRUE, cex = 1.5)
plot(1, 2, xaxt = 'n', yaxt = 'n', ann = F, bty = "n")
legend("bottomleft", inset=c(-0.04, -0.25), legend=unique(mydata$DRG.Definition), pch=16, 
       title="Medical Conditions Color Legend", col = color, cex=0.8, bty = "n")
