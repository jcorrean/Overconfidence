makePlot <- function(data, worse, persp, stats = TRUE, line = T, cex.axes = 1.05) {
    d <- data
    par(las = 1)
    sel <- d[d$other.worse == worse & d$other.persp == persp,]
    plot(sel$perc, 
         sel$X13 + runif(length(sel$X13), -2, 2), 
         ylim = c(0,100),
         pch = 16, col = rgb(0,0,0,0.3), cex = 0.5,
         ylab = "", xlab = ""
    )
    if(line) {
        m.loe <- loess(sel$X13 ~ sel$perc)
        xs <-  seq(min(sel$perc), max(sel$perc), 0.01)
        pred.loe <- predict(m.loe, xs, se = T)
        polygon(c(xs, xs[length(xs):1]), 
                c(pred.loe$fit + 1.96*pred.loe$se.fit, 
                  (pred.loe$fit - 1.96*pred.loe$se.fit)[length(xs):1]),
                col = rgb(0.5, 0.5, 0.5, alpha = 0.4), border = NA
        )
        lines(xs, pred.loe$fit, col = "black", lwd = 2)
    }
    mtext("Result", side = 1, line = 2.5, at = 50, cex = cex.axes)
    mtext("Prediction", side = 2, line = 2.5, at = 50, cex = cex.axes, las = 3)
    lines(c(0,100), c(0,100), lty = 2)
    if(stats) {
        text(80, 25, sprintf("r = %.2f", cor(sel$perc, sel$X13)), cex = 1.35, pos = 4)
        text(80, 15, sprintf("B = %.1f", mean(sel$X13 - sel$perc)), cex = 1.35, pos = 4)
        text(80, 5, sprintf("D = %.1f", mean(abs(sel$X13 - sel$perc))), cex = 1.35, pos = 4)
    }
}


makeLine <- function(sel, color) {
    m.loe <- loess(sel$X13 ~ sel$perc)
    xs <-  seq(min(sel$perc), max(sel$perc), 0.01)
    pred.loe <- predict(m.loe, xs, se = T)
    polygon(c(xs, xs[length(xs):1]), 
            c(pred.loe$fit + 1.96*pred.loe$se.fit, 
              (pred.loe$fit - 1.96*pred.loe$se.fit)[length(xs):1]),
            col = rgb(color[1], color[2], color[3], alpha = 0.2), border = NA)
    lines(xs, pred.loe$fit, col = rgb(color[1], color[2], color[3]), lwd = 2)   
}

