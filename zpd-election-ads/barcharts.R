#setwd("/home/kalvis/workspace/ddgatve-stat/zpd-election-ads")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


df <- read.table(
  file="declaration-data.csv", 
  sep=",",
  na.strings="NN",
  header=TRUE,
  row.names=NULL,
  fileEncoding="UTF-8")

# Cik reklamu naudas vajag, lai dabutu 1 balsi
df$priceOfVote <- df$Izdevumi/df$Votes

# Cik reklamu naudas vajag, lai dabutu 1 deputatu
df$priceOfMP <- df$Izdevumi/df$Mps

# Nomet uz 0 tas partijas, kas neparsniedz 5% barjeru
df$priceOfMP[which(is.infinite(df$priceOfMP))] <- 0

df$priceOfMPlab <- sprintf("%.1fK",0.001*df$priceOfMP)
df$priceOfVoteLab <- sprintf("%.2f",df$priceOfVote)

#df$bcolor <- levels(droplevels(df$bcolor))

df$overBarrier <- (df$Mps > 0)

library(ggplot2)
p<-ggplot(data=df, aes(x=reorder(ShortName, priceOfVote), 
                       y=priceOfVote, width=0.7, 
                       fill=overBarrier, 
                       col=overBarrier)) +
  geom_bar(stat="identity") + 
  geom_text(aes(label=priceOfVoteLab), hjust=1.05, vjust=0.5, color="black", size=2)+
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(face="plain", color="#0066CC", 
                                   size=7),
        axis.text.y = element_text(face="plain", color="#0066CC", 
                                   size=7),
        panel.grid.major.x = 
          element_line(size = 0.2, colour="black", linetype="dashed"), 
        panel.grid.major.y=
          element_line(size = 0.1, colour="#cccccc"))+
  scale_x_discrete(name ="Partija")+
  scale_y_continuous(name="Izdevumi (EUR) par 1 balsi", 
                   breaks = 0:15, minor_breaks = NULL,
                   labels=sprintf("%d",0:15)) + 
  scale_fill_manual(values = c("white", "#f8766d"))+
  scale_colour_manual(values=c("gray", "#f8766d"))+
  coord_flip()
p

ggsave(p, filename = "declarations/barchart1.png", dpi = 300, type = "cairo",
       width = 4, height = 2.5, units = "in")


df2 <- df[df$Mps>0,]

p<-ggplot(data=df2, aes(x=reorder(ShortName, priceOfMP), y=priceOfMP, width=0.7, fill="pink")) +
  geom_bar(stat="identity") +
  geom_text(aes(label=priceOfMPlab), hjust=1, vjust=0.5, color="black", size=2)+
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid.major.x = 
          element_line(size = 0.2, colour="black", linetype="dashed"), 
        panel.grid.minor.x = 
          element_line(size = 0.1, colour="black", linetype="dotted"), 
        panel.grid.major.y=
          element_line(size = 0.1, colour="#cccccc"),
        axis.text.x = element_text(face="plain", color="#0066CC",
                                   size=8),
        axis.text.y = element_text(face="plain", color="#0066CC",
                                   size=8))+
  scale_x_discrete(name ="Partija", 
                   labels=sprintf("%s (%d)",df2$ShortName,df2$Mps)[order(df2$priceOfMP)])+
  scale_y_continuous(name="Izdevumi (EUR) par 1 deput\u0101ta vietu", 
                     breaks = 20000*c(0,1,2,3,4,5), 
                     labels=c("0K", "20K", "40K", "60K", "80K", "100K")) + 
  coord_flip()
p

ggsave(p, filename = "declarations/barchart2.png", dpi = 300, type = "cairo",
       width = 4, height = 2, units = "in")



