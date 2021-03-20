Collapse Multilevel Network
================
JC
20/02/2021

# Globals

``` r
#-----------------------------------
# load libraries
library(tidyverse)
library(igraph)
library(scales)
library(reshape2)
```

    ## Warning: package 'reshape2' was built under R version 4.0.4

``` r
#-----------------------------------
# globals
# function to plot
plotNet <- function(g, ecolor = "black", mycoords = coords, title = ""){
  plot(g, 
       layout=mycoords,
       vertex.color = get.vertex.attribute(graph = g, name = "color"),
       # vertex.frame.color= get.vertex.attribute(graph = g, name = "color"),
       vertex.size = 19,
       vertex.frame.color = "transparent",
       main = title
       # ,vertex.label=NA
       )
}

# ac.dist to edge list format:
gen.mat.to.edge.list<-function(mat,symmetric=TRUE,diagonal=FALSE,text=FALSE){
  #create edge list from matrix
  # if symmetric duplicates are removed
  mat<-as.matrix(mat)
  id<-is.na(mat) # used to allow missing
  mat[id]<-"nna"
  if(symmetric){mat[lower.tri(mat)]<-"na"} # use to allow missing values
  if(!diagonal){diag(mat)<-"na"}
  obj<-melt(mat)
  colnames(obj)<-c("from","to","value")
  obj<-obj[!obj$value=="na",]
  obj$value[obj$value=="nna"]<-NA
  if(!text){obj$value<-as.numeric(as.character(obj$value))}
  return(obj)
  # remove duplicates
}
```

## Create a multilevel network

``` r
# create network
#vertices:
v.df <- data.frame(id = c("F1","F2","F3","F4","F5","a","b","c", "d"),
                   type = c(rep("facility", times = 5),
                            rep("staff", times = 4)))
v.df$color <- ifelse(v.df$type == "facility", alpha("blue",.5), alpha("red",.5))

#edges
e.df <- data.frame(source = c("F1","F2","F2","F3","F4","F4","F5","a","c"),
                    target = c("a","a","c","c","c","d","b","b","d"))

# create igraph object
g <- graph.data.frame(d = e.df, directed = FALSE, v = v.df)

# define layout for raw data
coords.r <- layout.fruchterman.reingold(g)

plotNet(g, mycoords = coords.r, title = "raw net")
```

![](collapseNet_files/figure-gfm/raw%20net-1.png)<!-- -->

## Collapse at facility-level using minimum path-length 2

``` r
# calculate the shortest distance between all aged cre nodes:
ac.dist <- igraph::distances(g, v = V(g)$type == "facility", 
                               to = V(g)$type == "facility",
                     mode = "all")

# from matrix to edge list
pl.edg <- gen.mat.to.edge.list(ac.dist, symmetric = TRUE,
                                 diagonal = FALSE, text = FALSE)
# subset mpl2
mpl2.edf <- subset(pl.edg, value == 2)

# create network object
g.mpl2 <- graph.data.frame(d = mpl2.edf,
                           directed = FALSE,
                           v = v.df)
plotNet(g.mpl2, mycoords = coords.r, title = "MPL2")
```

![](collapseNet_files/figure-gfm/MPL2-1.png)<!-- -->

## Collapse at facility-level using minimum path-length 3

``` r
# subset mpl3
mpl3.edf <- subset(pl.edg, value == 3)

# create network object
g.mpl3 <- graph.data.frame(d = mpl3.edf,
                           directed = FALSE,
                           v = v.df)
plotNet(g.mpl3, mycoords = coords.r, title = "MPL3")
```

![](collapseNet_files/figure-gfm/MPL3-1.png)<!-- -->

## Collapse at facility-level using path-length 3

### Logan’s version

Begin with the central staff-staff edges and table join staff-facility
edges onto either side.

``` r
staff.edges = e.df %>%
  filter(!str_detect(source, "F"))
employment.edges = e.df %>%
  filter(str_detect(source, "F")) %>%
  rename(facility = source,
         staff = target)
collapsed.edges = staff.edges %>%
  inner_join(employment.edges, by=c("source"="staff")) %>%
  inner_join(employment.edges, by=c("target"="staff"), suffix=c(".source", ".target")) %>%
  filter(facility.source != facility.target) %>%
  select(facility.source, facility.target)
g3 = graph.data.frame(collapsed.edges, directed = F)
plot(g3, main="Facilities linked via two staff in series")
```

![](collapseNet_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

### Jose’s version

``` r
# vector of facility id
all.facilities <- v.df$id[which(v.df$type == 'facility')] 
  
# subset nodes in neighborhood of facility[i]
vids <- neighborhood(graph = g, order = 3,
                     nodes = all.facilities, mode = "all")

vapply(vids, FUN = function(vx){}  )

# create subgraph with nodes from previous step
ret.g <- induced.subgraph(graph = g, vids = unlist(vids[[1]]),
                             impl = "create_from_scratch")

# return all simple paths for the subgraph
ret.p <- all_simple_paths(graph = ret.g, from = all.facilities[1] , 
                 to = V(ret.g), mode = c("all"))

# subset simple paths of length 3
ret.p <- ret.p[sapply(ret.p, length) == 4] 

# create a function for checking ends
check.ends <- function(x){all(c(as_ids(x[1]), as_ids(x[4])) %in% all.facilities)}

# subset paths with start and end in facility
ret.p <- ret.p[sapply(ret.p, check.ends)]

# keep only first and last node in path (RACFS) to make edge list 
r.edf <- t(sapply(ret.p, function(x){as_ids(x[c(1,4)])}))


# if ret.p > 1 add to edge list


# selegoV <- ego(g, order=1, nodes = selnodes, mode = "all", mindist = 0)
#     
#     extend arm to nodes that are at 3 PL distance from RACFi
#     subset nodes from step 2 to retain RACF nodes only
#     list simple paths between RACFi and nodes from step 3
#     keep paths from step 4 that don’t go through RACFs
#     collapse simple paths to 1st and last node of the path (i.e. RACF-RACF edge)

# }
# https://gist.github.com/JoshuaTPierce/b919168421b40e06481080eb53c3fb2f
```
