mydata <- read.csv("_e143dff6e844c7af8da2a4e71d7c054d_payments.csv", stringsAsFactors = F)

plot(log10(mydata[, c("Average.Covered.Charges", "Average.Total.Payments")]),
    xlab = "log( Mean Covered Charges ($) )", ylab = "log( Mean Total Payments ($) )",
    main = "Plot of log( Mean Covered Charges ($) ) against log( Mean Total Payments ($) )",
    pch = 16, col = rgb(1, 0, 0.2, 0.2))

model <- lm(log10(Average.Total.Payments) ~ log10(Average.Covered.Charges), mydata)

abline(model, lwd = 2, col = rgb(0, 0, 0.5, 0.5))
mtext("Appoximate Relationship: Mean Total Payments = 100 * (Mean Covered Charges) ^ 0.4")
