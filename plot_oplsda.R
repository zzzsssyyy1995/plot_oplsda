plot.myself <- function(x,y,label=F){
  xxx <- as.data.frame(cbind(getScoreMN(x),getScoreMN(x,orthoL=T)[,1]))
  colnames(xxx) <- c("t1","ot1")
  xxx %<>% rownames_to_column("Label") %>%
    dplyr::mutate(Groups=factor(y))
  # xxx$Groups <- factor(xxx$Groups) 
  # library(ggfortify)
  # library(tidyverse)
  # library(yyplot)
  # library(plyr)
  p1 <- ggplot(xxx,aes(x=t1,y=ot1,label=Label,color=Groups,fill=Groups))+
    # geom_rug(outside = TRUE)+
    stat_ellipse(show.legend = F,geom="polygon",segments = 51,
                 alpha=0.2,size=0.8,level=0.95,type="norm")+
    geom_point(size=3,position = position_jitter(height = 0.5,width = 1,seed = 2))+
    geom_hline(yintercept = 0,linetype=2)+
    geom_vline(xintercept = 0,linetype=2)+
    scale_color_manual(values = c("#2b3d4f","#18bc9c"))+
    scale_fill_manual(values = c("#2b3d4f","#18bc9c"))+
    labs(caption=paste0("R2X    R2Y    Q2    pR2Y    pQ2\n",
                        x@summaryDF$`R2X(cum)`,"    ",
                        x@summaryDF$`R2Y(cum)`,"    ",
                        x@summaryDF$`Q2(cum)`,"    ",
                        x@summaryDF$pR2Y,"    ",
                        x@summaryDF$pQ2))+
    # ggtitle("Scores (OPLS-DA)")+
    xlab(paste0("T score[1] (",x@modelDF$R2X[1]*100,"%)"))+
    ylab("Orthogonal T score[1]")+
    guides(color=guide_legend(override.aes=list(size=3)))+
    # gglayer::geom_ord_ellipse(ellipse_pro = 0.95)+
    theme_minimal()+
    theme(axis.text=element_text(size=12,face = "bold",colour = "black"),
          axis.title=element_text(size=12,face = "bold",colour = "black"),
          axis.ticks.length=unit(0, "cm"),
          # axis.line = element_line(colour = "black", size = 1.1),
          # axis.ticks = element_line(colour = "black", size = 1.1),
          # legend.background =element_blank(),
          # legend.position = "bottom",
          plot.caption=element_text(size=12,face = "bold",colour = "black"),
          legend.title = element_blank(),
          legend.text=element_text(size=12,face = "bold",colour = "black"),
          plot.title = element_text(hjust = 0.5,size = 14,face = "bold",colour = "black"),
          plot.margin = unit(rep(1,4),"lines"),
    )
  if(label==T){
    p1<- p1+geom_text(show.legend = F)
  }
  library(aplot)
  library(ggpubr)
  xdensity <- ggplot(xxx, aes(t1, fill=Groups)) +
    geom_density(alpha=.5,adjust=2) +
    scale_fill_manual(values = c("#2b3d4f","#18bc9c"))+
    theme(legend.position = "none")+
    theme_nothing()
  xdensity
  # Marginal density plot of y (right panel)
  ydensity <- ggplot(xxx, aes(ot1, fill=Groups)) +
    geom_density(alpha=.5,adjust=2) +
    scale_fill_manual(values = c("#2b3d4f","#18bc9c"))+
    theme(legend.position = "none")+
    coord_flip()+
    theme_nothing()
  ydensity
  
  p1 %<>%
    insert_top(xdensity,height = 0.2) %>%
    insert_right(ydensity,width = 0.2)
  p1
  # library(patchwork)
  # xdensity+plot_spacer()+p1+ydensity
  # library(cowplot)
  # plot_grid(xdensity,NULL,p1,ydensity,align = "hv")
  
}