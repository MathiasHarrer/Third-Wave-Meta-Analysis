pcurve_dataprep<-function(data){
  data<-data
  Me<-as.numeric(data$Me)
  Se<-as.numeric(data$Se)
  Ne<-as.numeric(data$Ne)
  Mc<-as.numeric(data$Mc)
  Sc<-as.numeric(data$Sc)
  Nc<-as.numeric(data$Nc)
  
  esc<-esc_mean_sd(grp1m=Me, 
                   grp1sd=Se, 
                   grp1n=Ne, 
                   grp2m=Mc, 
                   grp2sd=Sc, 
                   grp2n=Nc, 
                   es.type = "d")
  
  output<-des(d=esc$es,n.1=Ne,n.2=Nc, verbose = FALSE)
  output$r<-abs(output$r)
  tot<-data.frame(paste("r(",output$N.total-2,")=",output$r))
  colnames(tot)<-c("output")
  tot$output<-gsub(" ", "", tot$output, fixed = TRUE)
  totoutput<-as.character(tot$output)
  print(tot, row.names = FALSE)
  write(totoutput,ncolumns=1, file="input.txt")
}
