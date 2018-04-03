source("limma.ggplot.R")
#simulated data, first 5 with differential expression between groups
m1<-5 #genes with differential expression
m2<-95 #genes with no differential expression
m<-m1+m2
n1<-5 #samples in control
n2<-5 #samples in case
n<-n1+n2

set.seed(7)
sd <- 0.3*sqrt(4/rchisq(m,df=4))
y <- matrix(rnorm(m*n,sd=sd),m,n)
rownames(y) <- paste("Gene",1:m)

#add effect with a constant coefficient to first group
y[1:m1,1:n1] <- y[1:m1,1:n1] + 2 
design <- cbind(Grp1=1,Grp2vs1=c(rep(1, n1), rep(0, n2)))

library(limma)
library(ggplot2)
# Ordinary fit
fit <- lmFit(y,design)
fit <- eBayes(fit)
tab<-topTable(fit,coef=2, sort.by = "none", number = nrow(fit))
tab$highlightcol<-tab$adj.P.Val<0.01

mypng<-function(p, dirout, header, width = 10, height = 10, res = 300){
	png(paste0(dirout, "/", header, ".png"), width = width, height = height, units = 'in', res = res)
	print(p)
	dev.off()
}

# Volcano plot
dirout<-"../example_plots"
dir.create(dirout)

#volcanoplot(fit,coef=2,highlight=5)
p<-volcano.ggplot(tab, highlight = "highlightcol")
mypng(p, dirout, "volcano")

#plotMD(fit,column=2)
p<-md.ggplot(tab, highlight = "highlightcol")
mypng(p, dirout, header = "md")


