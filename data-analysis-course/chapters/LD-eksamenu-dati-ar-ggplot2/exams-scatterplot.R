require(ggplot2)
require(RColorBrewer)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
df <- read.table(file="matematikas-centralizetais.csv", 
                 sep=",", header=TRUE)
df$Part23 = 0.50*df$Part2 + (0.1875)*df$Part3
p <- ggplot(df, aes(x=Part1, y=Part23, color=as.factor(Class))) + 
  geom_point() + 
  scale_color_manual(values=brewer.pal(4,"Set1")) + 
  geom_smooth(method='lm', se = FALSE)
p
ggsave(p, filename = "scatterplot.png", dpi = 200, type = "cairo",
       width = 4.25, height = 3.25, units = "in")

