rob.summary<-function(data){
  rob.vars<-data.frame(data)
  rob.vars$Author<-NULL
  ncol.rob.vars<-ncol(rob.vars)
  last<-colnames(rob.vars[ncol.rob.vars])
  first<-colnames(rob.vars[1])
  rob.long <- gather(data,
                     condition, measurement,
                     first:last,
                     factor_key=TRUE)
  rob.long$measurement<-as.factor(rob.long$measurement)
  rob.long$measurement<-factor(rob.long$measurement,
                               levels(rob.long$measurement)[c(1,3,2)])
  rob.plot<-ggplot(data=rob.long)+
    geom_bar(mapping=aes(x=condition,fill=measurement),
             width=0.7,
             position = "fill",
             color="black")+
    coord_flip(ylim = c(0,1))+
    guides(fill = guide_legend(reverse = TRUE))+
    scale_fill_manual("Risk of Bias",
                      labels = c("    High risk of bias          ",
                                 "    Unclear risk of bias       ",
                                 "    Low risk of bias  "),
                      values = c("Unclear" = "#E2DF07",
                                 "High" = "#BF0000",
                                 "Low" = "#02C100"))+
    scale_y_continuous(labels = scales::percent)+
    theme(axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.text.y = element_text(size=18, color = "black"),
          axis.line.x = element_line(colour = "black",
                                     size = 0.5, linetype = "solid"),
          legend.position = "bottom",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          legend.background = element_rect(linetype="solid",
                                           colour ="black"),
          legend.title = element_blank(),
          legend.key.size = unit(0.75,"cm"),
          legend.text=element_text(size=14))
  return(rob.plot)
}

