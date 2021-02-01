###########################################
# This file contains R code for recreating 
# the analyses in Carlson and Zorn (2021)
# "Toward Better Hiring Practices" PS:
# Political Science and Politics, Vol.
# 54: forthcoming.
#
# This file created February 1, 2021.
###########################################

# Uncomment and fix as necessary: 
#
# setwd("~/Stuff/")

# Load packages (install as necessary):

library(RCurl) 
library(plyr)    
library(ggplot2)
library(car)
library(randomNames)
library(compositions)

###########################################
# Read data:

url <- getURL("https://raw.githubusercontent.com/PrisonRodeo/TBHP-git/master/TBHP.csv")
df <- read.csv(text = url)
rm(url)

#####################################################
# Unstandardized means by item and rater:

df2<-ddply(df,.(Candidate),summarise,
           Fit=mean(Fit,na.rm=TRUE),
           Pubs=mean(PubsGrants,na.rm=TRUE),
           Agenda=mean(Agenda,na.rm=TRUE),
           Climate=mean(Climate,na.rm=TRUE),
           Female=median(Female))

# Binary fit:

df2$BinaryFit<-ifelse(df2$Fit>=6,1,0)
with(df2, xtabs(~Female+BinaryFit))

# Unstandardized standard deviations by item and rater:

ddply(df,.(CommitteeMember),summarise,
      Fit=sd(Fit,na.rm=TRUE),
      Pubs=sd(PubsGrants,na.rm=TRUE),
      Agenda=sd(Agenda,na.rm=TRUE),
      Climate=sd(Climate,na.rm=TRUE))

# Standardize:

dfS <- ddply(df, .(CommitteeMember), transform,
             FitS = (Fit - mean(Fit,na.rm=TRUE)) / sd(Fit,na.rm=TRUE),
             PubS = (PubsGrants - mean(PubsGrants,na.rm=TRUE)) / sd(Fit,na.rm=TRUE),
             AgendaS = (Agenda - mean(Agenda,na.rm=TRUE)) / sd(Fit,na.rm=TRUE),
             ClimateS = (Climate - mean(Climate,na.rm=TRUE)) / sd(Fit,na.rm=TRUE))

# Calculate unweighted sum of standardized scores:

dfS$SumScoreS <- with(dfS, ((FitS+PubS+AgendaS+ClimateS) / 4))

# Candidate averages of unweighted SumScores:

cc <- ddply(dfS, .(Candidate), summarise,
            SumScoreS = mean(SumScoreS),
            Female = mean(Female))

# A t-test:

U.S.ttest <- with(cc, t.test(SumScoreS~Female))
U.S.ttest

# Criteria-specific tests:

Fit.ttest <- with(dfS, t.test(FitS~Female))
Pub.ttest <- with(dfS, t.test(PubS~Female))
Agenda.ttest <- with(dfS, t.test(AgendaS~Female))
Climate.ttest <- with(dfS, t.test(ClimateS~Female))

# A plot of the means / t-tests:

F.means <- c(Fit.ttest$estimate[2],Pub.ttest$estimate[2],
             Agenda.ttest$estimate[2],Climate.ttest$estimate[2])

M.means <- c(Fit.ttest$estimate[1],Pub.ttest$estimate[1],
             Agenda.ttest$estimate[1],Climate.ttest$estimate[1])

symbols<-c(4,19)
colors<-c("navy","orange2")
criteria <- c("Fit","Publications","Research Agenda","Climate")
T.labels <- c(paste("Fit      \n","(t=",
                    round(Fit.ttest$statistic,2),")",sep=""),
              paste("Publications\n","(t=",
                    round(Pub.ttest$statistic,2),")   ",sep=""),
              paste("Research Agenda\n","(t=",
                    round(Agenda.ttest$statistic,2),")     ",sep=""),
              paste("Climate  \n","(t=",
                    round(Climate.ttest$statistic,2),")",sep=""))

pdf("PS/Plots/PS-Std-Scores-Means.pdf",5,5.5)
par(mar=c(7,4,1,1))
pssm<-plot(seq(1:4),F.means,ylim=c(-0.5,1.1),pch=19,col="orange2",
           xaxt="n",cex=1.2,xlab=" ",
           ylab="Mean Standardized Ratings (Unweighted)")
points(seq(1:4),M.means,pch=4,col="navy")
text(seq(1:4),par("usr")[3]-0.04,labels=T.labels,srt=45,
     adj=c(1.1,1.1),xpd=TRUE)
axis(1,at=seq(1:4),labels=FALSE,tick=TRUE)
abline(h=0,lty=2,lwd=1)
legend("topleft",bty="n",pch=symbols,col=colors,
       legend=c("Male Candidates","Female Candidates"))
dev.off()

##################################################
# Weighted sum scores...
#
# Enter vector of weights for Fit, Publications, 
# Agenda, and Climate, respectively

W <- c(0.4,0.4,0.1,0.1) # Search weights

dfS$WSumScore <- with(dfS, (W[1]*FitS+W[2]*PubS+
                              W[3]*AgendaS+W[4]*ClimateS))

cc2 <- ddply(dfS, .(Candidate), summarise,
             WSumScore = mean(WSumScore),
             Female = mean(Female))
cc2 <- cc2[order(cc2$WSumScore),]     # sort...

# Dotplot, because why not?
#
# pdf("PS/Plots/PS-WeightedDotPlot.pdf",6,8)
# with(cc2, dotchart(WSumScore,labels=Candidate,cex=0.75,
#                    col=colors[cc2$Female+1],pch=symbols[cc2$Female+1],
#                    main="Weighted Standardized Sum Scores"))
# abline(v=c(-1,-0.5,0,0.5,1),lty=3)
# dev.off()

# Overall t-test: Weighted sum scores:

W.S.ttest <- with(cc2, t.test(WSumScore~Female))

# PCA using candidate-level data (unweighted
# averages of standardized scores):

PIX <- ddply(dfS, .(Candidate), summarise,
             Fit = mean(FitS),
             Pubs = mean(PubS),
             Agenda = mean(AgendaS),
             Climate = mean(ClimateS),
             Female = mean(Female)) 

# PCA:

pca <- with(PIX, princomp(~Fit+Pubs+Agenda+Climate,
                          cor=TRUE))

# Biplot:

pdf("PS/Plots/PS-Biplot.pdf",6,6)
coloredBiplot(pca,xlabs=PIX$Candidate,cex=0.6,
              xlab="First PCA Component",
              ylab="Second PCA Component",
              col=c("black","black"),
              xlabs.col=colors[PIX$Female+1])
dev.off()

# Finally, create the Figure 1 in the paper (by
# combining the two above):

pdf("PS/Plots/Figure1.pdf",12,6)
par(mfrow=c(1,2))
par(mar=c(7,4,1,1))
pssm<-plot(seq(1:4),F.means,ylim=c(-0.5,1.1),pch=19,col="orange2",
           xaxt="n",cex=1.2,xlab=" ",
           ylab="Mean Standardized Ratings (Unweighted)")
points(seq(1:4),M.means,pch=4,col="navy")
text(seq(1:4),par("usr")[3]-0.04,labels=T.labels,srt=45,
     adj=c(1.1,1.1),xpd=TRUE)
axis(1,at=seq(1:4),labels=FALSE,tick=TRUE)
abline(h=0,lty=2,lwd=1)
legend("topleft",bty="n",pch=symbols,col=colors,
       legend=c("Male Candidates","Female Candidates"))
coloredBiplot(pca,xlabs=PIX$Candidate,cex=0.6,
              xlab="First PCA Component",
              ylab="Second PCA Component",
              col=c("black","black"),
              xlabs.col=colors[PIX$Female+1])
dev.off()

# Aaaand, a TIFF, for the good people at CUP:

tiff("PS/Plots/Figure1.tiff",width=12,height=6,
     units="in",res=300)
par(mfrow=c(1,2))
par(mar=c(7,4,1,1))
pssm<-plot(seq(1:4),F.means,ylim=c(-0.5,1.1),pch=19,col="orange2",
           xaxt="n",cex=1.2,xlab=" ",
           ylab="Mean Standardized Ratings (Unweighted)")
points(seq(1:4),M.means,pch=4,col="navy")
text(seq(1:4),par("usr")[3]-0.04,labels=T.labels,srt=45,
     adj=c(1.1,1.1),xpd=TRUE)
axis(1,at=seq(1:4),labels=FALSE,tick=TRUE)
abline(h=0,lty=2,lwd=1)
legend("topleft",bty="n",pch=symbols,col=colors,
       legend=c("Male Candidates","Female Candidates"))
coloredBiplot(pca,xlabs=PIX$Candidate,cex=0.6,
              xlab="First PCA Component",
              ylab="Second PCA Component",
              col=c("black","black"),
              xlabs.col=colors[PIX$Female+1])
dev.off()

# fin