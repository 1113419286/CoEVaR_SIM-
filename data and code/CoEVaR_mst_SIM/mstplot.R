# based on data.frame-----------------------------------------
rm(list = ls(all = TRUE))
graphics.off()

setwd('~/COEVAR//')

############################################
#MST plot 图6 and 图8


library(Cairo)
library(showtext) #pdf chinese

library(readxl)
library('igraph')
load('SIM_Results/sim_co.Rdata')


name=as.data.frame(read_excel('name.xlsx'))
name.fi=as.vector(name[,1])


timelist = list('201911'=c(499:501),'202003'=c(516:519),
                '202201'=c(611:614),'202205'=c(628:632))
mon = c('2019_11','2020_03','2022_01','2022_05')

# vertex shape
mytriangle <- function(coords, v=NULL, params) {
  vertex.color <- params("vertex", "color")
  if (length(vertex.color) != 1 && !is.null(v)) {
    vertex.color <- vertex.color[v]
  }
  vertex.size <- 1/200 * params("vertex", "size")
  if (length(vertex.size) != 1 && !is.null(v)) {
    vertex.size <- vertex.size[v]
  }
  
  symbols(x=coords[,1], y=coords[,2], bg=vertex.color,
          stars=cbind(vertex.size, vertex.size, vertex.size),
          add=TRUE, inches=FALSE)
}
add_shape("triangle", clip=shapes("circle")$clip,plot=mytriangle)

t=1
for (t in 1:length(timelist)) {
  
  m = mon[t]
  t_con = matrix(0,30,30)
  for(i in timelist[[t]]){
    
    t_con = t_con+abs(con[,,i]) 
  }
  pm = -t_con/length(timelist) #maximum spanning tree #pm=abs(p) #minimum spanning tree
  
  colnames(pm) = name.fi
  
  h2 = graph_from_adjacency_matrix(as.matrix(pm),weighted = TRUE,mode='directed')
  edges = get.data.frame(h2,what='edges') #get edges' dataframe from adjacency
  
  nodes<-data.frame("labels"=name.fi,
                    "location"=rep(c("Americas","Europes","Asia"),times=1,each=10))
  
  h22 = graph_from_data_frame(edges,vertices = nodes,directed = T) #new 
  h22mst = mst(h22,weights = graph_attr(h22,'weight')) #mst
  edgemst = get.data.frame(h22mst,what='edges')
  edgemst[,3]= -get.data.frame(h22mst,what='edges')[,3] #maximum spanning tree
  
  deg=degree(h22mst,mode = 'all') #degree
  vcolor<-c("#ff8f19","#c72228","#00b0eb") 
  
  V(h22mst)$color=rep(vcolor,times=1,each=10)#color based on different area
  #E(h22mst)$width=E(h22mst)$weight*100  #edge width with weight
  E(h22mst)$width = 0.6*(edgemst$weight)*3.5^(1/max(edgemst$weight)) #maximum
  E(h22mst)$color = V(h22mst)$color[match(edgemst[,1],name.fi)]
  
  set.seed(125)
  l<-layout_with_lgl(h22mst)  #points arrangement way
  
  svg(paste0("CoEVaR_mst_SIM/p", m, "mst_sim.svg"),width = 12, height = 12,family = 'GB1')
  
  par(mai=c(0,0,0,0))
  plot(h22mst, vertex.size=sqrt(deg)*3,vertex.label.cex = 1,vertex.label.dist=1,
       vertex.label.color = 'gray10',
       vertex.shape = rep(c('circle','square',"triangle"),each = 10),
       edge.curved=0.1,
       edge.arrow.size = 1.3,
       layout=l)
  
  legend(x=0.8,y=-0.61,legend=c("美洲","欧洲","亚洲"),
         pch=c(16,15,17),col = vcolor,
         pt.bg="#777777",cex = 1.5)
  
  dev.off()  
  
 
  ### pdf
  CairoPDF(paste0("CoEVaR_mst_SIM/pdf/p", m, "mst_sim.pdf"),width = 12, height = 12)
  showtext_begin()
  
  par(mai=c(0,0,0,0))
  plot(h22mst, vertex.size=sqrt(deg)*3,vertex.label.cex=1,vertex.label.dist=1,
       vertex.label.color = 'gray10',
       vertex.shape = rep(c('circle','square',"triangle"),each = 10),
       edge.curved=0.1,
       edge.arrow.size=1.3,layout=l)
  
  legend(x=0.8,y=-0.61,legend=c("美洲","欧洲","亚洲"),
         pch=c(16,15,17),col = vcolor,
         pt.bg="#777777",cex = 1.5)
  
  showtext_end()
  dev.off() 
}




#####################################3
# 附录 图 13

#covid transform
timelist = list('202002'=c(512:515),'202003'=c(516:519),'202004'=c(520:523),'202005'=c(524:528))
mon = c('2020_02','2020_03','2020_04','2020_05')


# vertex shape
# vertex shape
mytriangle <- function(coords, v=NULL, params) {
  vertex.color <- params("vertex", "color")
  if (length(vertex.color) != 1 && !is.null(v)) {
    vertex.color <- vertex.color[v]
  }
  vertex.size <- 1/200 * params("vertex", "size")
  if (length(vertex.size) != 1 && !is.null(v)) {
    vertex.size <- vertex.size[v]
  }
  
  symbols(x=coords[,1], y=coords[,2], bg=vertex.color,
          stars=cbind(vertex.size, vertex.size, vertex.size),
          add=TRUE, inches=FALSE)
}
add_shape("triangle", clip=shapes("circle")$clip,plot=mytriangle)

t=1
for (t in 1:length(timelist)) {
  
  m = mon[t]
  t_con = matrix(0,30,30)
  for(i in timelist[[t]]){
    
    t_con = t_con+abs(con[,,i]) 
  }
  pm = -t_con/length(timelist) #maximum spanning tree #pm=abs(p) #minimum spanning tree
  
  colnames(pm) = name.fi
  
  h2 = graph_from_adjacency_matrix(as.matrix(pm),weighted = TRUE,mode='directed')
  edges = get.data.frame(h2,what='edges') #get edges' dataframe from adjacency
  
  nodes<-data.frame("labels"=name.fi,
                    "location"=rep(c("Americas","Europes","Asia"),times=1,each=10))
  
  h22 = graph_from_data_frame(edges,vertices = nodes,directed = T) #new 
  h22mst = mst(h22,weights = graph_attr(h22,'weight')) #mst
  edgemst = get.data.frame(h22mst,what='edges')
  edgemst[,3]= -get.data.frame(h22mst,what='edges')[,3] #maximum spanning tree
  
  deg=degree(h22mst,mode = 'all') #degree
  vcolor<-c("#ff8f19","#c72228","#00b0eb") 
  
  V(h22mst)$color=rep(vcolor,times=1,each=10)#color based on different area
  #E(h22mst)$width=E(h22mst)$weight*100  #edge width with weight
  E(h22mst)$width = 0.6*(edgemst$weight)*3.5^(1/max(edgemst$weight)) #maximum
  E(h22mst)$color = V(h22mst)$color[match(edgemst[,1],name.fi)]
  
  set.seed(135)
  l<-layout_with_lgl(h22mst)  #points arrangement way
  
  svg(paste0("CoEVaR_mst_SIM/covid02-05/p", m, "mst_sim.svg"),width = 12, height = 12,family = 'GB1')
  
  par(mai=c(0,0,0,0))
  plot(h22mst, vertex.size=sqrt(deg)*3,vertex.label.cex = 1,vertex.label.dist=1,
       vertex.label.color = 'gray10',
       vertex.shape = rep(c('circle','square',"triangle"),each = 10),
       edge.curved=0.1,
       edge.arrow.size = 1.3,
       layout=l)
  
  legend(x=0.8,y=-0.61,legend=c("美洲","欧洲","亚洲"),
         pch=c(16,15,17),col = vcolor,
         pt.bg="#777777",cex = 1.5)
  
  dev.off()  
  
  
  ### pdf
  CairoPDF(paste0("CoEVaR_mst_SIM/covid02-05/p", m, "mst_sim.pdf"),width = 12, height = 12)
  showtext_begin()
  
  par(mai=c(0,0,0,0))
  plot(h22mst, vertex.size=sqrt(deg)*3,vertex.label.cex=1,vertex.label.dist=1,
       vertex.label.color = 'gray10',
       vertex.shape = rep(c('circle','square',"triangle"),each = 10),
       edge.curved=0.1,
       edge.arrow.size=1.3,layout=l)
  
  legend(x=0.8,y=-0.61,legend=c("美洲","欧洲","亚洲"),
         pch=c(16,15,17),col = vcolor,
         pt.bg="#777777",cex = 1.5)
  
  showtext_end()
  dev.off() 
}

