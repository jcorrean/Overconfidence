setwd("/home/juan/Documents/GitHub/Overconfidence")
# Let's load the raw data
load("~/Documents/GitHub/Overconfidence/d.RData")
# Let's run some preliminary ad-hoc functions for reporting
# results and creating graphs
source("helper.R")
source("helper2.R")
source("graphs.R")

# WHILE THE DATASET IS MOSTLY ANONYMIZED, DO NOT SHARE THE DATA IN THIS FORM ANYWHERE
# WE WILL POST THE DATA ON OSF, BUT SOME OTHER INFORMATION SHOULD BE REMOVED FROM THE DATASET BEFOREHAND

# we use only the osp variables as a measure of performance (those are z-scores)
# osp is from the third administration where our questions were administered
# osp1 and osp2 are from first two administrations
# the personality measures are computed in the code below, the names should be self-expalantory
# other.worse obtained below is whether they were asked how many participant will be worse than they
# (the other condition being how many pariticipants will be better)
# other.persp obtained below is whether they took perspective of others
# (in the other condition they were asked from their own perspective)

# the rest should not be important

####################################################################


# exclusion
#-------------
# missing measure
remain <- apply(d[,c("X12a", "X12b", "X12c", "X12d", "X13", "OSP_v_terminu")], 1, 
                function(x) all(!is.na(x)))
d <- d[remain,]
n.excluded <- 5989 - nrow(d)
n.excluded
sum(remain)
n.excluded/5989

m0 <- lm(X13 ~ perc.cent, data = d)
plotty <- plot(d$perc.cent, d$X13)
abline(m0)
summary(m0)
m1 <- lm(I(X13-mean(d$perc[d$zkousek.OSP.vykonanych == 1])) ~ perc.cent*other.worse*other.persp, 
         data = d[d$zkousek.OSP.vykonanych == 1,])
cor(d$X13[d$zkousek.OSP.vykonanych == 1 & d$other.worse == 0.5 & d$other.persp == -0.5],
    d$perc[d$zkousek.OSP.vykonanych == 1 & d$other.worse == 0.5 & d$other.persp == -0.5])

# Let's see general results from this first model
summary(m1)
# and now let's decompose these general results as follows:
# First, the results of the Intercept are estimated as follows
results(m1, 1, family = "linear") 
# Then, we proceed with the estimation of
# the relationship between the participant's 
# self-prediction (i.e., "perc.cent" in our data set)
# and his/her GAP test score (i.e., "X13" in our data set)
results(m1, 2, family = "linear", 3)
# Now, let's see the association between
# participant's perception on how many participant will be worse than they
# (the other condition being how many pariticipants will be better)

results(m1, 3, family = "linear")

results(m1, 4, family = "linear")
results(m1, 5, family = "linear", 3)
results(m1, 6, family = "linear", 3)
results(m1, 7, family = "linear")
results(m1, 8, family = "linear", 3)



png("Fig1.png", width = 1400, height = 1000, pointsize = 24)
par(mfrow = c(2,2), mar = c(4.1, 4.1, 0.2, 0.2), oma = c(0,5,5,0))
makePlot(data = d[d$zkousek.OSP.vykonanych == 1,], 0.5, 0.5)
makePlot(data = d[d$zkousek.OSP.vykonanych == 1,], 0.5, -0.5)
makePlot(data = d[d$zkousek.OSP.vykonanych == 1,], -0.5, 0.5)
makePlot(data = d[d$zkousek.OSP.vykonanych == 1,], -0.5, -0.5)
mtext("Comparison", side = 2, outer = T, line = 3, at = 0.55, las = 3, cex = 1.8)
mtext("Others worse", side = 2, outer = T, line = 1, at = 0.8, las = 3, cex = 1.5)
mtext("Others better", side = 2, outer = T, line = 1, at = 0.3, las = 3, cex = 1.5)
mtext("Perspective", side = 3, outer = T, line = 3, at = 0.55, cex = 1.8)
mtext("Family and friends", side = 3, outer = T, line = 1, at = 0.3, cex = 1.5)
mtext("Self", side = 3, outer = T, line = 1, at = 0.8, cex = 1.5)
dev.off()


# experience
#-----------
m2 <- lm(I(X13-mean(d$perc)) ~ perc.cent*other.worse*other.persp + perc.cent*previous.cent, 
         data = d)
summary(m2)
results(m2, 1, family = "linear") 
results(m2, 2, family = "linear", 3)
results(m2, 3, family = "linear")
results(m2, 4, family = "linear")
results(m2, 5, family = "linear")
results(m2, 6, family = "linear", 3)
results(m2, 7, family = "linear", 3)
results(m2, 8, family = "linear")
results(m2, 9, family = "linear", 3)
results(m2, 10, family = "linear", 3)




# personality
#------------
round(cor(d[,c("mindset", "narcissism", "selfesteem", "osp")]), 2)

# narcissism
m.nar <- lm(I(X13-mean(d$perc[d$zkousek.OSP.vykonanych == 1])) ~ perc.cent*other.worse*other.persp*narcissism.cent,
            data = d[d$zkousek.OSP.vykonanych == 1,])
summary(m.nar)
results(m.nar, 5, family = "linear")
results(m.nar, 9, family = "linear", 3)
results(m.nar, 15, family = "linear")

png("Fig2.png", width = 1200, height = 825, pointsize = 24)
set.seed(200)
makePlot(data = d[d$zkousek.OSP.vykonanych == 1,], 0.5, 0.5, stats = F, line = F, cex.axes = 1.35)
mtext("Narcissism", side = 3, cex = 1.8, line = 1)
sel <- d[d$zkousek.OSP.vykonanych == 1 & d$other.worse == 0.5 & d$other.persp == -0.5 & d$narcissism %in% 1:2,]
makeLine(sel, c(1,0,0))
text(60, 8, sprintf("r = %.2f", cor(sel$perc, sel$X13)), cex = 1.35, pos = 4, col = "red")
text(60, 2, sprintf("B = %.1f", mean(sel$X13 - sel$perc)), cex = 1.35, pos = 4, col = "red")
sel <- d[d$zkousek.OSP.vykonanych == 1 & d$other.worse == 0.5 & d$other.persp == -0.5 & d$narcissism %in% 3:4,]
makeLine(sel, c(0,1,0))
text(75, 8, sprintf("r = %.2f", cor(sel$perc, sel$X13)), cex = 1.35, pos = 4, col = "green")
text(75, 2, sprintf("B = %.1f", mean(sel$X13 - sel$perc)), cex = 1.35, pos = 4, col = "green")
sel <- d[d$zkousek.OSP.vykonanych == 1 & d$other.worse == 0.5 & d$other.persp == -0.5 & d$narcissism %in% 5:7,]
makeLine(sel, c(0,0,1))
text(90, 8, sprintf("r = %.2f", cor(sel$perc, sel$X13)), cex = 1.35, pos = 4, col = "blue")
text(90, 2, sprintf("B = %.1f", mean(sel$X13 - sel$perc)), cex = 1.35, pos = 4, col = "blue")
text(60, 14, "low", cex = 1.35, pos = 4, col = "red")
text(75, 14, "middle", cex = 1.35, pos = 4, col = "green")
text(90, 14, "high", cex = 1.35, pos = 4, col = "blue")
dev.off()


# self-esteem
m.sel <- lm(I(X13-mean(d$perc[d$zkousek.OSP.vykonanych == 1])) ~ perc.cent*other.worse*other.persp*selfesteem.cent,
            data = d[d$zkousek.OSP.vykonanych == 1,])
summary(m.sel)
results(m.sel, 5, family = "linear")
results(m.sel, 9, family = "linear", 3)
results(m.sel, 10, family = "linear")
results(m.sel, 11, family = "linear")
results(m.sel, 13, family = "linear", 3)

png("Fig3.png", width = 1200, height = 825, pointsize = 24)
set.seed(200)
makePlot(data = d[d$zkousek.OSP.vykonanych == 1,], 0.5, 0.5, stats = F, line = F, cex.axes = 1.35)
mtext("Self-esteem", side = 3, cex = 1.8, line = 1)
sel <- d[d$zkousek.OSP.vykonanych == 1 & d$other.worse == 0.5 & d$other.persp == -0.5 & d$selfesteem %in% 1:2,]
makeLine(sel, c(1,0,0))
text(60, 8, sprintf("r = %.2f", cor(sel$perc, sel$X13)), cex = 1.35, pos = 4, col = "red")
text(60, 2, sprintf("B = %.1f", mean(sel$X13 - sel$perc)), cex = 1.35, pos = 4, col = "red")
sel <- d[d$zkousek.OSP.vykonanych == 1 & d$other.worse == 0.5 & d$other.persp == -0.5 & d$selfesteem %in% 3:5,]
makeLine(sel, c(0,1,0))
text(75, 8, sprintf("r = %.2f", cor(sel$perc, sel$X13)), cex = 1.35, pos = 4, col = "green")
text(75, 2, sprintf("B = %.1f", mean(sel$X13 - sel$perc)), cex = 1.35, pos = 4, col = "green")
sel <- d[d$zkousek.OSP.vykonanych == 1 & d$other.worse == 0.5 & d$other.persp == -0.5 & d$selfesteem %in% 6:7,]
makeLine(sel, c(0,0,1))
text(90, 8, sprintf("r = %.2f", cor(sel$perc, sel$X13)), cex = 1.35, pos = 4, col = "blue")
text(90, 2, sprintf("B = %.1f", mean(sel$X13 - sel$perc)), cex = 1.35, pos = 4, col = "blue")
text(60, 14, "low", cex = 1.35, pos = 4, col = "red")
text(75, 14, "middle", cex = 1.35, pos = 4, col = "green")
text(90, 14, "high", cex = 1.35, pos = 4, col = "blue")
dev.off()


# mindset
m.min <- lm(I(X13-mean(d$perc[d$zkousek.OSP.vykonanych == 1])) ~ perc.cent*other.worse*other.persp*mindset.cent,
            data = d[d$zkousek.OSP.vykonanych == 1,])
summary(m.min)
results(m.min, 5, family = "linear")
results(m.min, 9, family = "linear", 3)

png("Fig4.png", width = 1200, height = 825, pointsize = 24)
set.seed(200)
makePlot(data = d[d$zkousek.OSP.vykonanych == 1,], 0.5, 0.5, stats = F, line = F, cex.axes = 1.35)
mtext("Mindset", side = 3, cex = 1.8, line = 1)
sel <- d[d$zkousek.OSP.vykonanych == 1 & d$other.worse == 0.5 & d$other.persp == -0.5 & d$mindset %in% seq(1, 2.5, 0.5),]
makeLine(sel, c(1,0,0))
text(60, 8, sprintf("r = %.2f", cor(sel$perc, sel$X13)), cex = 1.35, pos = 4, col = "red")
text(60, 2, sprintf("B = %.1f", mean(sel$X13 - sel$perc)), cex = 1.35, pos = 4, col = "red")
sel <- d[d$zkousek.OSP.vykonanych == 1 & d$other.worse == 0.5 & d$other.persp == -0.5 & d$mindset %in% seq(3, 5, 0.5),]
makeLine(sel, c(0,1,0))
text(75, 8, sprintf("r = %.2f", cor(sel$perc, sel$X13)), cex = 1.35, pos = 4, col = "green")
text(75, 2, sprintf("B = %.1f", mean(sel$X13 - sel$perc)), cex = 1.35, pos = 4, col = "green")
sel <- d[d$zkousek.OSP.vykonanych == 1 & d$other.worse == 0.5 & d$other.persp == -0.5 & d$mindset %in% seq(5.5, 7, 0.5),]
makeLine(sel, c(0,0,1))
text(90, 8, sprintf("r = %.2f", cor(sel$perc, sel$X13)), cex = 1.35, pos = 4, col = "blue")
text(90, 2, sprintf("B = %.1f", mean(sel$X13 - sel$perc)), cex = 1.35, pos = 4, col = "blue")
text(60, 14, "low", cex = 1.35, pos = 4, col = "red")
text(75, 14, "middle", cex = 1.35, pos = 4, col = "green")
text(90, 14, "high", cex = 1.35, pos = 4, col = "blue")
dev.off()