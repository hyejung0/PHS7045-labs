library(netplot)
library(igraph)
library(igraphdata)

data("UKfaculty")

col<-V(UKfaculty)$Group


set.seed(889)

ans<-nplot(
  UKfaculty,
  vertex.color=col,
  edge.line.breaks=10)

png("netplotfig.png", width=1024, height=780)
print(ans)
dev.off()

saveRDS(ans,file="netplotfig.rds")
