Gender Representation in Romance Movies using Network Analysis
================
Ryan and Dina

``` r
#Clear
rm(list = ls())
```

\#0 Load Node and Edge list (Check Alignment of Node and Edge List)

Nest Node and EdgeList in dataframe

``` r
for (i in 1:no_films){
  all[i,]$edges <- nest(all_edges[[i]])
  nod <- all_nodes[[i]]
  nod <- nod %>% 
    select(nid,entity,freq,scene_count,gender) %>%
    mutate(main = if_else(scene_count > 2, TRUE, FALSE))
  
  all[i,]$nodes <- nest(nod)
}

all = all %>% arrange(year)
```

Check Dataframe

``` r
print (all %>% select(n,year,movie))
```

    ##     n year                                 movie
    ## 1   2 1977                            Annie Hall
    ## 2   6 1987                        Princess Bride
    ## 3   7 1996                        Romeo & Juliet
    ## 4  11 1997                               Titanic
    ## 5   8 1999                         Runaway Bride
    ## 6   4 2004 Eternal Sunshine of the Spotless Mind
    ## 7   1 2009                    500 Days of Summer
    ## 8   3 2011                   Crazy, Stupid, Love
    ## 9   9 2012               Silver Linings Playbook
    ## 10 10 2014                  Theory of Everything
    ## 11  5 2016                            La La Land

\#1 Data Exploration: Raw Network Statistics Stats Table

``` r
compare_df <- all %>% 
  select(n, year, movie) %>%
  mutate(modularity = 0,
         den = 0,
         trn = 0,
         apl = 0,
         dia = 0)

#Network Statistics

for (x in 1:no_films){
  
node_list = data.frame(all[x,]$nodes)
edge_list = data.frame(all[x,]$edges)

nodelist <- node_list
edgelist <- edge_list %>% select(node_i, node_j)

colnames(edgelist) = c('from','to')

edgelist

#Create igraph graph
g <- graph_from_edgelist(as.matrix(edgelist), directed = FALSE)
g <- simplify(g,remove.loops = TRUE, remove.multiple = TRUE)

#Check Modularity
com <- edge.betweenness.community(g, directed = F)
compare_df[x,]$modularity <- round(max(com$modularity),3)

#density
compare_df[x,]$den <- mean(degree(g))/(vcount(g)-1)

#transitivity (probability that the adjacent vertices of a vertex are connected)
compare_df[x,]$trn <- transitivity(g, 'global')


#Average Path Length (length of all the shortest paths from or to the vertices in the network)
compare_df[x,]$apl <- mean_distance(g, unconnected = TRUE)

#Diameter
compare_df[x,]$dia <- diameter(g, weights = NA)
}

compare_df
```

    ##     n year                                 movie modularity       den       trn
    ## 1   2 1977                            Annie Hall      0.276 0.1012195 0.1743421
    ## 2   6 1987                        Princess Bride      0.031 0.3450292 0.5645514
    ## 3   7 1996                        Romeo & Juliet      0.083 0.1763441 0.4036697
    ## 4  11 1997                               Titanic      0.128 0.1090395 0.3076596
    ## 5   8 1999                         Runaway Bride      0.094 0.1763441 0.3328823
    ## 6   4 2004 Eternal Sunshine of the Spotless Mind      0.185 0.2213439 0.4580838
    ## 7   1 2009                    500 Days of Summer      0.037 0.2573099 0.3559322
    ## 8   3 2011                   Crazy, Stupid, Love      0.019 0.1849462 0.3667820
    ## 9   9 2012               Silver Linings Playbook      0.080 0.1592742 0.3136531
    ## 10 10 2014                  Theory of Everything      0.318 0.1566952 0.3000000
    ## 11  5 2016                            La La Land      0.244 0.1118280 0.1720698
    ##         apl dia
    ## 1  2.084146   4
    ## 2  1.771930   3
    ## 3  2.182796   4
    ## 4  2.300000   5
    ## 5  1.961290   3
    ## 6  2.193676   5
    ## 7  1.777778   3
    ## 8  1.929032   3
    ## 9  1.901210   3
    ## 10 2.122507   3
    ## 11 2.255914   5

Comparing with Year \*\*Lower Transitivity over the Years

``` r
par(mfrow = c(2,3))
plot(compare_df$year, compare_df$modularity, main = "modularity")
plot(compare_df$year, compare_df$den, main = "density")
plot(compare_df$year, compare_df$trn, main = "trn")
plot(compare_df$year, compare_df$apl, main = "apl")
plot(compare_df$year, compare_df$dia, main = "dia")
```

![](SNA_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

\#2 Data Cleaning: Filtering Nodes 2a/ Initial Plots (undirected, self
loops, WEIGHTED) - Problems with lack of labels, networks also very
messy and unnecessary convulated - Black nodes, seem to be unimportant,
thought we were able to remove it

``` r
for (x in 1:no_films){
  node_list <- data.frame(all[x,]$nodes)
  edge_list <- data.frame(all[x,]$edges)
  nodelist <- node_list
  edgelist <- edge_list %>% select(node_i, node_j)
  colnames(edgelist) <- c('from','to')
  
  #Create igraph graph
  el <- as.matrix(get.adjacency(graph.data.frame(edgelist)))
  g <- graph_from_adjacency_matrix(el, weighted = T, mode = "undirected",diag = F)
  
  #Order Vertices
  order <- match(V(g)$name, nodelist$nid)
  nodelist <- nodelist[order,]
  
  #Centrality
  btw <- betweenness(g)#betweenness
  cls <- closeness(g)#closeness
  k     <- degree(g, mode = 'all')#degree
  evc <- eigen_centrality(g, directed = T)$vector#eigenvector
  pgr <- page_rank(g)$vector#pagerank
  
  #Color by Gender
  gen <- nodelist$gender
  col_gen <- rep('black',length(gen))
  col_gen[gen == 'F'] <- 'pink'
  col_gen[gen == 'M'] <- 'skyblue'
  
  #plot based on degree centrality
  plot (g,
       layout = layout_with_fr,
       edge.width = (E(g)$weight/2),
       vertex.size = 1.5*nodelist$freq^0.5, #based on speaking
       #vertex.size = 25*(btw/max(btw))^0.5, #based on btw
       #vertex.size = 25*(evc)^0.5, #based on evc
       vertex.color = col_gen,
       vertex.label = nodelist$entity,
       vertex.label.cex = 0.1*(nodelist$freq^0.5),
       #edge.arrow.size = 0.2,
       main = paste(all[x,]$n," - (", all[x,]$year, ") ",all[x,]$movie, sep = ""),
       add = F)
  
  nodelist$gender[is.na(nodelist$gender)] <- "O"
  text(-1.2, 1.2, paste("Gender Assortativity =", assortativity_nominal(g, as.factor(nodelist$gender))))

}
```

![](SNA_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->![](SNA_files/figure-gfm/unnamed-chunk-7-2.png)<!-- -->![](SNA_files/figure-gfm/unnamed-chunk-7-3.png)<!-- -->![](SNA_files/figure-gfm/unnamed-chunk-7-4.png)<!-- -->![](SNA_files/figure-gfm/unnamed-chunk-7-5.png)<!-- -->![](SNA_files/figure-gfm/unnamed-chunk-7-6.png)<!-- -->![](SNA_files/figure-gfm/unnamed-chunk-7-7.png)<!-- -->![](SNA_files/figure-gfm/unnamed-chunk-7-8.png)<!-- -->![](SNA_files/figure-gfm/unnamed-chunk-7-9.png)<!-- -->![](SNA_files/figure-gfm/unnamed-chunk-7-10.png)<!-- -->![](SNA_files/figure-gfm/unnamed-chunk-7-11.png)<!-- -->

2b/ Filtering “Un-important” People? by Frequency Count \[Scene Count 2
and Below\] - Learnt that “importance” is hard to define theoretically -
Low scene count actors might have high betweeness (disconnected the
node) - Tititanic and Eternal Sunshine \*\*The utterance count/ scene
count might not have a direct rls with importance (centrality) \[Search
further\]

``` r
for (x in 1:no_films){
    
  node_list <- data.frame(all[x,]$nodes)
  edge_list <- data.frame(all[x,]$edges)
  nodelist <- node_list
  edgelist <- edge_list %>% select(node_i, node_j)
  colnames(edgelist) = c('from','to')
  
  #Create igraph graph
  el <- as.matrix(get.adjacency(graph.data.frame(edgelist)))
  #g <- graph_from_edgelist(as.matrix(edgelist), directed = FALSE)
  #g <- simplify(g,remove.loops = TRUE, remove.multiple = TRUE)
  g = graph_from_adjacency_matrix(el, weighted = T, mode = "undirected",diag = F)
  
  #Delete Vertex
  #del_gen = nodelist[is.na(nodelist$gender),]$nid
  #del = del_gen
  del_main = nodelist[(nodelist$main==FALSE),]$nid
  #del = c(del_gen,del_main)
  
  del_o <- match(del_main, V(g)$name)
  del_o <- del_o[!is.na(del_o)]
  g = delete_vertices(g, del_o)
  
  #Order Vertices
  order <- match(V(g)$name, nodelist$nid)
  nodelist = nodelist[order,]
  
  #Centrality
  btw <- betweenness(g)#betweenness
  cls <- closeness(g)#closeness
  k     <- degree(g, mode = 'all')#degree
  evc <- eigen_centrality(g, directed = T)$vector#eigenvector
  pgr <- page_rank(g)$vector#pagerank
  
  #Color by Gender
  gen <- nodelist$gender
  col_gen <- rep('black',length(gen))
  col_gen[gen == 'F'] <- 'pink'
  col_gen[gen == 'M'] <- 'skyblue'
  
  #plot based on degree centrality
  plot (g,
       layout = layout_with_fr,
       edge.width = (E(g)$weight/2),
       vertex.size = 1.5*nodelist$freq^0.5, #based on speaking
       #vertex.size = 25*(btw/max(btw))^0.5, #based on btw
       #vertex.size = 25*(evc)^0.5, #based on evc
       vertex.color = col_gen,
       vertex.label = nodelist$entity,
       vertex.label.cex = 0.1*(nodelist$freq^0.5),
       #edge.arrow.size = 0.2,
       main = paste(all[x,]$n," - (", all[x,]$year, ") ",all[x,]$movie, sep = ""),
       add = F)
  
  nodelist$gender[is.na(nodelist$gender)] <- "O"
  text(-1.2, 1.2, paste("Gender Assortativity =", assortativity_nominal(g, as.factor(nodelist$gender))))

}
```

![](SNA_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->![](SNA_files/figure-gfm/unnamed-chunk-8-2.png)<!-- -->![](SNA_files/figure-gfm/unnamed-chunk-8-3.png)<!-- -->![](SNA_files/figure-gfm/unnamed-chunk-8-4.png)<!-- -->![](SNA_files/figure-gfm/unnamed-chunk-8-5.png)<!-- -->![](SNA_files/figure-gfm/unnamed-chunk-8-6.png)<!-- -->![](SNA_files/figure-gfm/unnamed-chunk-8-7.png)<!-- -->![](SNA_files/figure-gfm/unnamed-chunk-8-8.png)<!-- -->![](SNA_files/figure-gfm/unnamed-chunk-8-9.png)<!-- -->![](SNA_files/figure-gfm/unnamed-chunk-8-10.png)<!-- -->![](SNA_files/figure-gfm/unnamed-chunk-8-11.png)<!-- -->

2c/ Final Plotting by Gender

``` r
#x=1
compare_df$asst = 0

for (x in 1:no_films){

  #Call nodes and edges
  node_list = data.frame(all[x,]$nodes)
  edge_list = data.frame(all[x,]$edges)
  nodelist <- node_list
  edgelist <- edge_list %>% select(node_i, node_j)
  colnames(edgelist) = c('from','to')
  
  
  #Create igraph graph (undirected/unweighted)
  g <- graph_from_edgelist(as.matrix(edgelist), directed = FALSE)
  g <- simplify(g,remove.loops = TRUE, remove.multiple = TRUE)
  
  #Delete Vertex (by gender)
  del_gen <- nodelist[is.na(nodelist$gender),]$nid
  del <- del_gen
  del_o <- match(del, V(g)$name)
  del_o <- del_o[!is.na(del_o)]
  g <- delete_vertices(g, del_o)
  
  #Order Vertices
  order <- match(V(g)$name, nodelist$nid)
  nodelist <- nodelist[order,]
  
  #Centrality
  btw <- betweenness(g)#betweenness
  cls <- closeness(g)#closeness
  k     <- degree(g, mode = 'all')#degree
  evc <- eigen_centrality(g, directed = T)$vector#eigenvector
  pgr <- page_rank(g)$vector#pagerank
  
  
  
  #Color by Gender
  gen <- nodelist$gender
  col_gen <- rep('black',length(gen))
  col_gen[gen == 'F'] <- 'pink'
  col_gen[gen == 'M'] <- 'skyblue'
  
  #plot based on degree centrality
  plot (g,
       layout = layout_with_fr,
       #edge.width = (E(g)$weight/2),
       vertex.size = (nodelist$freq^0.5)+3, #based on speaking
       #vertex.size = 25*(btw/max(btw))^0.5, #based on btw
       #vertex.size = 25*(evc)^0.5, #based on evc
       vertex.color = col_gen,
       vertex.label = nodelist$entity,
       vertex.label.cex = 0.1*(nodelist$freq^0.5),
       #edge.arrow.size = 0.2,
       main = paste(all[x,]$n," - (", all[x,]$year, ") ",all[x,]$movie, sep = ""),
       add = F)
  
  nodelist$gender[is.na(nodelist$gender)] <- "O"
  text(-1.2, 1.2, paste("Gender Assortativity =", assortativity_nominal(g, as.factor(nodelist$gender))))
  
  #plot(x = cent_df$freq, y = cent_df$btw, col = as.factor(cent_df$gender) )
  compare_df[x,]$asst = assortativity_nominal(g, as.factor(nodelist$gender))
}
```

![](SNA_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->![](SNA_files/figure-gfm/unnamed-chunk-9-2.png)<!-- -->![](SNA_files/figure-gfm/unnamed-chunk-9-3.png)<!-- -->![](SNA_files/figure-gfm/unnamed-chunk-9-4.png)<!-- -->![](SNA_files/figure-gfm/unnamed-chunk-9-5.png)<!-- -->![](SNA_files/figure-gfm/unnamed-chunk-9-6.png)<!-- -->![](SNA_files/figure-gfm/unnamed-chunk-9-7.png)<!-- -->![](SNA_files/figure-gfm/unnamed-chunk-9-8.png)<!-- -->![](SNA_files/figure-gfm/unnamed-chunk-9-9.png)<!-- -->![](SNA_files/figure-gfm/unnamed-chunk-9-10.png)<!-- -->![](SNA_files/figure-gfm/unnamed-chunk-9-11.png)<!-- -->

\#3 ASSORTATIVE MIXING \[Macro\] 3a/ Gender Assortative over Time? -
Seems like “Gender dynamics” are more POLARIZED - Gender differences
seems to play greater roles

``` r
#par(mfrow = c(2,1))
plot(x=compare_df$year, 
     y=compare_df$asst,
     main = "Gender Assortativity",
     ylim = c(-0.15,0.15),
     xlab = "Year", 
     ylab = "Asst")
abline(h = 0, lwd = 2, col = 'red')
```

![](SNA_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
plot(x=compare_df$year, 
     y=abs(compare_df$asst),
     col= as.factor(compare_df$asst>0),
     main = "Gender Assortativity (Absolute)",
     xlab = "Year", 
     ylab = "abs(Asst)")
abline(lm(abs(compare_df$asst)~compare_df$year), col="green") # regression line (y~x)
```

![](SNA_files/figure-gfm/unnamed-chunk-10-2.png)<!-- -->

``` r
#compare_df
```

3b/ Plotting by Communities -
<INSERT HOW COMMUNITIES ARE DECIDED IN ROMANCE MOVIES>

``` r
#x=2
for (x in 1:no_films){
  
node_list = data.frame(all[x,]$nodes)
edge_list = data.frame(all[x,]$edges)

nodelist <- node_list
edgelist <- edge_list %>% select(node_i, node_j)

colnames(edgelist) = c('from','to')

#Create igraph graph
g <- graph_from_edgelist(as.matrix(edgelist), directed = FALSE)
g <- simplify(g,remove.loops = TRUE, remove.multiple = TRUE)

del = nodelist[is.na(nodelist$gender),]$nid
del_o <- match(del, V(g)$name)
del_o <- del_o[!is.na(del_o)]
delete_vertices(g, del_o)

order <- match(V(g)$name, nodelist$nid)
nodelist = nodelist[order,]

#Community Structure
com <- edge.betweenness.community(g, directed = F)
com_no = max(com$membership)
C <- split(nodelist, com$membership)
library(colorRamps)
names(C) <- primary.colors(com_no, steps = 2, no.white = TRUE)

#plot based on degree centrality
plot (g,
     layout = layout_with_fr,
     #edge.width = (E(g)$weight/2),
     vertex.size = (nodelist$freq^0.5)+3,
     vertex.color = com$membership + 1,
     vertex.label = nodelist$entity,
     #vertex.label.cex = 0.07*(nodelist$freq^0.5),
     #edge.arrow.size = 0.2,
     main = paste(all[x,]$n," - (", all[x,]$year, ") ",all[x,]$movie, sep = ""),
     add = F)
text(-1.2, 1.2, paste("Modularity, Q =", round(max(com$modularity),3)))
}
```

![](SNA_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->![](SNA_files/figure-gfm/unnamed-chunk-11-2.png)<!-- -->![](SNA_files/figure-gfm/unnamed-chunk-11-3.png)<!-- -->![](SNA_files/figure-gfm/unnamed-chunk-11-4.png)<!-- -->![](SNA_files/figure-gfm/unnamed-chunk-11-5.png)<!-- -->![](SNA_files/figure-gfm/unnamed-chunk-11-6.png)<!-- -->![](SNA_files/figure-gfm/unnamed-chunk-11-7.png)<!-- -->![](SNA_files/figure-gfm/unnamed-chunk-11-8.png)<!-- -->![](SNA_files/figure-gfm/unnamed-chunk-11-9.png)<!-- -->![](SNA_files/figure-gfm/unnamed-chunk-11-10.png)<!-- -->![](SNA_files/figure-gfm/unnamed-chunk-11-11.png)<!-- -->

\#4 CENTRALITY \[Micro\] How does good representation look like?

4a/ Understanding Main Characters - <OBSERVE>

``` r
#x=1

for (x in 1:no_films){

#Call nodes and edges
node_list = data.frame(all[x,]$nodes)
edge_list = data.frame(all[x,]$edges)
nodelist <- node_list
edgelist <- edge_list %>% select(node_i, node_j)
colnames(edgelist) = c('from','to')


#Create igraph graph (undirected/unweighted)
g <- graph_from_edgelist(as.matrix(edgelist), directed = FALSE)
g <- simplify(g,remove.loops = TRUE, remove.multiple = TRUE)

#Delete Vertex (by gender)
del_gen <- nodelist[is.na(nodelist$gender),]$nid
del <- del_gen
del_o <- match(del, V(g)$name)
del_o <- del_o[!is.na(del_o)]
g <- delete_vertices(g, del_o)

#Order Vertices
order <- match(V(g)$name, nodelist$nid)
nodelist <- nodelist[order,]

#Centrality
btw <- betweenness(g)#betweenness
cls <- closeness(g)#closeness
k     <- degree(g, mode = 'all')#degree
evc <- eigen_centrality(g, directed = T)$vector#eigenvector
pgr <- page_rank(g)$vector#pagerank

#Color by Gender
gen <- nodelist$gender
col_gen <- rep('black',length(gen))
col_gen[gen == 'F'] <- 'pink'
col_gen[gen == 'M'] <- 'skyblue'

#plot based on degree centrality
par(mfrow = c(1, 1))
plot (g,
     layout = layout_with_fr,
     #edge.width = (E(g)$weight/2),
     vertex.size = 1.5*nodelist$freq^0.5, #based on speaking
     #vertex.size = 25*(btw/max(btw))^0.5, #based on btw
     #vertex.size = 25*(evc)^0.5, #based on evc
     vertex.color = col_gen,
     vertex.label = nodelist$entity,
     vertex.label.cex = 0.1*(nodelist$freq^0.5),
     #edge.arrow.size = 0.2,
     main = paste(all[x,]$n," - (", all[x,]$year, ") ",all[x,]$movie, sep = ""),
     add = F)

text(-1.2, 1.2, paste("Gender Assortativity =", assortativity_nominal(g, as.factor(nodelist$gender))))
#}

#which(nodelist$entity == "ANNIE")
#charA_i = which(nodelist$freq == max(nodelist$freq)) # Highest
#charB_i = which(nodelist$freq == max( nodelist$freq[nodelist$freq!=max(nodelist$freq)])) # Second Highest

#Male Protag
Mlist <- nodelist[nodelist$gender == "M",]
charA_nid <- Mlist$nid[which(Mlist$freq == max(Mlist$freq))]
#Femal Protag
Flist <- nodelist[nodelist$gender == "F",]
charB_nid <- Flist$nid[which(Flist$freq == max(Flist$freq))]

charA = which(nodelist$nid == charA_nid)
charB = which(nodelist$nid == charB_nid)
#charB
#charA = nodelist[nodelist$gender == "M",]
#nodelist[nodelist$gender == "F",]


charA_name = paste(nodelist$entity[charA], " (", nodelist$gender[charA] , ")", sep = "") #guy protag
charB_name = paste(nodelist$entity[charB], " (", nodelist$gender[charB] , ")", sep = "") #girl protag

# INDIVIDUAL NODE LEVEL
deg <- degree(g, mode = 'all')
btw <- betweenness(g)
eig <- eigen_centrality(g)$vector


BTW <- NULL
EIG <- NULL
done <- 0

#charA <- 5
#charB <- 24
sample_size <- 5000

while (done < sample_size)
{
        r <- sample_degseq(deg, method = 'vl')
        V(r)$name <- V(g)$name
        
        BTW <- rbind(BTW, betweenness(r))
        EIG <- rbind(EIG, eigen_centrality(r)$vector)
        done <- done + 1
}

BTW <- as.data.frame(BTW)
EIG <- as.data.frame(EIG)

par(mfrow = c(2, 2))
# specific individual (e.g., Jace, ID = 23)
den <- density(BTW[,charA])
pval <- mean(BTW[,charA] > btw[charA])
plot(den, main = paste ('Betweenness - ', charA_name, sep = ""))
abline(v = median(BTW[,charA]), lwd = 2)
abline(v = btw[charA], col = 'red', lwd = 2)
text(btw[charA], max(den$y) * 6/7, paste("TOP", round(pval * 100, 1), "%"), pos = 4, col = 'red')

den <- density(BTW[,charB])
pval <- mean(BTW[,charB] > btw[charB])
plot(den, main = paste ('Betweenness - ', charB_name, sep = ""))
abline(v = median(BTW[,charB]), lwd = 2)
abline(v = btw[charB], col = 'blue', lwd = 2)
text(btw[charB], max(den$y) * 6/7, paste("TOP", round(pval * 100, 1), "%"), pos = 4, col = 'blue')


den <- density(EIG[,charA])
pval <- mean(EIG[,charA] > eig[charA])
plot(den, main = paste ('Eigenvector - ', charA_name, sep = ""))
abline(v = median(EIG[,charA]), lwd = 2)
abline(v = eig[charA], col = 'red', lwd = 2)
text(eig[charA], max(den$y) * 6/7, paste("TOP", round(pval * 100, 1), "%"), pos = 4, col = 'red')

den <- density(EIG[,charB])
pval <- mean(EIG[,charB] > eig[charB])
plot(den, main = paste ('Eigenvector - ', charB_name, sep = ""))
abline(v = median(EIG[,charB]), lwd = 2)
abline(v = eig[charB], col = 'blue', lwd = 2)
text(eig[charB], max(den$y) * 6/7, paste("TOP", round(pval * 100, 1), "%"), pos = 4, col = 'blue')
}
```

![](SNA_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->![](SNA_files/figure-gfm/unnamed-chunk-12-2.png)<!-- -->![](SNA_files/figure-gfm/unnamed-chunk-12-3.png)<!-- -->![](SNA_files/figure-gfm/unnamed-chunk-12-4.png)<!-- -->![](SNA_files/figure-gfm/unnamed-chunk-12-5.png)<!-- -->![](SNA_files/figure-gfm/unnamed-chunk-12-6.png)<!-- -->![](SNA_files/figure-gfm/unnamed-chunk-12-7.png)<!-- -->![](SNA_files/figure-gfm/unnamed-chunk-12-8.png)<!-- -->![](SNA_files/figure-gfm/unnamed-chunk-12-9.png)<!-- -->![](SNA_files/figure-gfm/unnamed-chunk-12-10.png)<!-- -->![](SNA_files/figure-gfm/unnamed-chunk-12-11.png)<!-- -->![](SNA_files/figure-gfm/unnamed-chunk-12-12.png)<!-- -->![](SNA_files/figure-gfm/unnamed-chunk-12-13.png)<!-- -->![](SNA_files/figure-gfm/unnamed-chunk-12-14.png)<!-- -->![](SNA_files/figure-gfm/unnamed-chunk-12-15.png)<!-- -->![](SNA_files/figure-gfm/unnamed-chunk-12-16.png)<!-- -->![](SNA_files/figure-gfm/unnamed-chunk-12-17.png)<!-- -->![](SNA_files/figure-gfm/unnamed-chunk-12-18.png)<!-- -->![](SNA_files/figure-gfm/unnamed-chunk-12-19.png)<!-- -->![](SNA_files/figure-gfm/unnamed-chunk-12-20.png)<!-- -->![](SNA_files/figure-gfm/unnamed-chunk-12-21.png)<!-- -->![](SNA_files/figure-gfm/unnamed-chunk-12-22.png)<!-- -->

4b/ Understanding Overall Roles of Characters - <OBSERVE>

``` r
for (x in 1:no_films){

  #Call nodes and edges
  node_list = data.frame(all[x,]$nodes)
  edge_list = data.frame(all[x,]$edges)
  nodelist <- node_list
  edgelist <- edge_list %>% select(node_i, node_j)
  colnames(edgelist) = c('from','to')
  
  #Create igraph graph (undirected/unweighted)
  g <- graph_from_edgelist(as.matrix(edgelist), directed = FALSE)
  g <- simplify(g,remove.loops = TRUE, remove.multiple = TRUE)
  
  #Delete Vertex (by gender)
  del_gen <- nodelist[is.na(nodelist$gender),]$nid
  del <- del_gen
  del_o <- match(del, V(g)$name)
  del_o <- del_o[!is.na(del_o)]
  g <- delete_vertices(g, del_o)
  
  #Order Vertices
  order <- match(V(g)$name, nodelist$nid)
  nodelist <- nodelist[order,]
  
  #Color by Gender
  gen <- nodelist$gender
  col_gen <- rep('black',length(gen))
  col_gen[gen == 'F'] <- 'pink'
  col_gen[gen == 'M'] <- 'skyblue'
  
  #plot based on degree centrality
  par(mfrow = c(1, 1))
  plot (g,
       layout = layout_with_fr,
       #edge.width = (E(g)$weight/2),
       vertex.size = 1.5*nodelist$freq^0.5, #based on speaking
       #vertex.size = 25*(btw/max(btw))^0.5, #based on btw
       #vertex.size = 25*(evc)^0.5, #based on evc
       vertex.color = col_gen,
       vertex.label = nodelist$entity,
       vertex.label.cex = 0.1*(nodelist$freq^0.5),
       #edge.arrow.size = 0.2,
       main = paste(all[x,]$n," - (", all[x,]$year, ") ",all[x,]$movie, sep = ""),
       add = F)
  
  text(-1.2, 1.2, paste("Gender Assortativity =", assortativity_nominal(g, as.factor(nodelist$gender))))
  #}
  
  # INDIVIDUAL NODE LEVEL
  deg <- degree(g, mode = 'all')
  btw <- betweenness(g)
  eig <- eigen_centrality(g)$vector
  
  BTW <- NULL
  EIG <- NULL
  done <- 0
  
  sample_size <- 5000
  
  while (done < sample_size)
  {
          r <- sample_degseq(deg, method = 'vl')
          V(r)$name <- V(g)$name
          
          BTW <- rbind(BTW, betweenness(r))
          EIG <- rbind(EIG, eigen_centrality(r)$vector)
          done <- done + 1
  }
  
  BTW <- as.data.frame(BTW)
  EIG <- as.data.frame(EIG)
  
  # ------------------------------------- #
  peig <- NULL
  pbtw <- NULL
  
  n <- vcount(g)
  for (i in 1:n)
  {
          peig <- c(peig, mean(EIG[,i] >= eig[i]))
          pbtw <- c(pbtw, mean(BTW[,i] >= btw[i]))
  }
  
  nodelist$deg <- deg
  nodelist$eig <- eig
  nodelist$btw <- btw
  
  nodelist$peig <- peig
  nodelist$pbtw <- pbtw
  nodelist$col <- 'white'
  nodelist$col[nodelist$pbtw < .025] <- 'pink'
  nodelist$col[nodelist$pbtw > .975] <- 'skyblue'
  
  nodelist$gcol <- 'red'
  nodelist$gcol[nodelist$gender == 'M'] <- 'blue'
  
  #---------------------------
  par(mfrow = c(3, 2))
  #---------------------------
  plot(-1, -1, xlim = c(0, max(nodelist$deg)), ylim = c(1, 0), main = 'Betweenness',
       ylab = 'p_value', xlab = 'degree')
  
  text(nodelist$deg, nodelist$pbtw, nodelist$gender, col = nodelist$gcol)
  
  abline(h = c(.025, .5, .975), lty = 2)
  
  plot(-1, -1, xlim = c(0, max(nodelist$deg)), ylim = c(1, 0), main = 'Eigenvector',
       ylab = 'p_value', xlab = 'degree')
  text(nodelist$deg, nodelist$peig, nodelist$gender, col = nodelist$gcol)
  abline(h = c(.025, .5, .975), lty = 2)
  #---------------------------
  plot(-1, -1, xlim = c(0, max(nodelist$scene_count)), ylim = c(1, 0), main = 'Betweenness',
       ylab = 'p_value', xlab = 'freq')
  
  text(nodelist$scene_count, nodelist$pbtw, nodelist$gender, col = nodelist$gcol)
  
  abline(h = c(.025, .5, .975), lty = 2)
  
  plot(-1, -1, xlim = c(0, max(nodelist$scene_count)), ylim = c(1, 0), main = 'Eigenvector',
       ylab = 'p_value', xlab = 'freq')
  text(nodelist$scene_count, nodelist$peig, nodelist$gender, col = nodelist$gcol)
  abline(h = c(.025, .5, .975), lty = 2)
  #---------------------------
  boxplot(pbtw ~ gender, data = nodelist, ylim = c(1, 0), main = 'Betweenness')
  abline(h = .5, lty = 2)
  
  boxplot(peig ~ gender, data = nodelist, ylim = c(1, 0), main = 'Eigenvector')
  abline(h = .5, lty = 2)

}
```

![](SNA_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->![](SNA_files/figure-gfm/unnamed-chunk-13-2.png)<!-- -->![](SNA_files/figure-gfm/unnamed-chunk-13-3.png)<!-- -->![](SNA_files/figure-gfm/unnamed-chunk-13-4.png)<!-- -->![](SNA_files/figure-gfm/unnamed-chunk-13-5.png)<!-- -->![](SNA_files/figure-gfm/unnamed-chunk-13-6.png)<!-- -->![](SNA_files/figure-gfm/unnamed-chunk-13-7.png)<!-- -->![](SNA_files/figure-gfm/unnamed-chunk-13-8.png)<!-- -->![](SNA_files/figure-gfm/unnamed-chunk-13-9.png)<!-- -->![](SNA_files/figure-gfm/unnamed-chunk-13-10.png)<!-- -->![](SNA_files/figure-gfm/unnamed-chunk-13-11.png)<!-- -->![](SNA_files/figure-gfm/unnamed-chunk-13-12.png)<!-- -->![](SNA_files/figure-gfm/unnamed-chunk-13-13.png)<!-- -->![](SNA_files/figure-gfm/unnamed-chunk-13-14.png)<!-- -->![](SNA_files/figure-gfm/unnamed-chunk-13-15.png)<!-- -->![](SNA_files/figure-gfm/unnamed-chunk-13-16.png)<!-- -->![](SNA_files/figure-gfm/unnamed-chunk-13-17.png)<!-- -->![](SNA_files/figure-gfm/unnamed-chunk-13-18.png)<!-- -->![](SNA_files/figure-gfm/unnamed-chunk-13-19.png)<!-- -->![](SNA_files/figure-gfm/unnamed-chunk-13-20.png)<!-- -->![](SNA_files/figure-gfm/unnamed-chunk-13-21.png)<!-- -->![](SNA_files/figure-gfm/unnamed-chunk-13-22.png)<!-- -->
