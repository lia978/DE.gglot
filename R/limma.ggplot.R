##accessory functions for plotting Differential Expression results from limma
library(limma)
library(ggplot2)
library(ggrepel)

#volcano plot of log2 fold change vs -log10(P-value)
volcano.ggplot<-function(tab, x = "logFC", y = "P.Value", xlab = "Log2 Fold Change", 
ylab = "-log10(P-value)", highlight = NA, ...){
	tab$neglogp<-(-1)*log(tab[, y], base = 10)
	tab$ID<-rownames(tab)

	y<-"neglogp"

	p<-ggplot(tab, aes_string(x=x, y=y)) + 
	geom_point() + 
	theme_bw() +
	xlab(xlab) + 
	ylab(ylab)

	if(!is.na(highlight))
	p<-p+geom_text_repel(data=subset(tab, tab[,highlight] == TRUE),
		aes(label = ID), size = 2, nudge_x = 0.02, colour = "red")

    return(p)
}

#mean difference plot
md.ggplot<-function(tab, x = "AveExpr", y = "logFC", xlab = "Average log-expression", 
ylab = "log-fold-change", highlight = NA, ...){

	tab$ID<-rownames(tab)
	p<-ggplot(tab, aes_string(x=x, y=y)) + 
	geom_point() + 
	theme_bw() +
	xlab(xlab) + 
	ylab(ylab)

	if(!is.na(highlight))
	p<-p+geom_text_repel(data=subset(tab, tab[,highlight] == TRUE),
		aes(label = ID), size = 2, nudge_x = 0.02, colour = "red")
	
    return(p)
}
