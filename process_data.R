
### Setup -------------------- {{{
options(scipen=999)
library(rmarkdown)
library(flexdashboard)
library(pipeR)
library(tidyverse)
library(rio)
library(ggraph)
library(tidygraph)
library(DT)
library(visNetwork)
library(igraph)
library(highcharter)
library(htmlwidgets)
library(printr)
library(shiny)
library(patchwork)
library(bibliometrix)
# }}}

### Growth -------------------- {{{

# graphics
import('data/shelf_life_growth.txt') %>>% 
    as_tibble %>>% 
    rename(PY = V1, publications = V2 ) %>>% 
    dplyr::filter(PY %in% c(1980:2019)) %>>% 
    dplyr::arrange(PY) %>>% 
    dplyr::mutate(trend=1:n()) %>>% 
    (. -> d)

d$lnp <- log(d$publications)

# ajustar parametros via mqo 
m1 <- lm(lnp ~ trend, data=d)

# summary(m1)

beta0 <- m1$coefficients[[1]]
beta1 <- m1$coefficients[[2]]

# modelo não linear
# 1980 é o primeiro ano da série
m2 <- nls(publications ~ b0*exp(b1*(PY-1980)), start = list(b0=beta0, b1=beta1), data=d)

# publications estimado
d$predicted <- 12.159638*exp(0.121922*(d$PY-1980))

d %>>% 
    mutate(Publications=publications, Year=PY) %>>% 
    mutate(predicted=round(predicted,0)) %>>% 
    (export(.,'data/growth_shelf_life.rds'))

# }}}

### Groups Attributes -------------------- {{{

netcit <- import('data/netcit.rds')
a <- import('data/netcit_grupos.rds')

netcit %>>% 
    activate(nodes) %>>% 
    as_tibble %>>% 
    dplyr::filter(!is.na(grupo)) %>>% 
    group_by(PY,grupo) %>>% 
    tally(sort=TRUE) %>>% 
    arrange(grupo,desc(PY)) %>>% 
    ungroup %>>% 
    dplyr::filter(PY %in% c(2000:2019)) %>>% 
    dplyr::mutate(Group=grupo,Publications = n, Year = PY) %>>% 
    (. -> grupoAno)

grupos <- sort(unique(grupoAno$Group))

res <- vector('double', length(grupos))

for(i in seq_along(grupos)){ 

    grupoAno %>>% 
        dplyr::select(PY,n,Group) %>>% 
        dplyr::rename(publications = n) %>>% 
        dplyr::filter(PY >= 2000) %>>% 
        dplyr::arrange(PY) %>>%  
        dplyr::filter(Group==grupos[[i]]) %>>%
        dplyr::mutate(trend=1:n()) %>>%
        dplyr::mutate(lnp=log(publications)) %>>%
        (. -> d) 

    # ajustar parametros via mqo 
    m1 <- lm(lnp ~ trend, data=d)
    beta0 <- m1$coefficients[[1]]
    beta1 <- m1$coefficients[[2]]
    
    # modelo não linear
    m2 <- nls(publications ~ b0*exp(b1*(PY-2000)), start = list(b0=beta0, b1=beta1), data=d)
    res[[i]] <- coef(m2)[2]

}

# print(xtable(grupoAnoCrescimento, type = "latex"))

data.frame(Groups=grupos,Coef=res) %>>% 
    as_tibble %>>% 
    mutate(GrowthRateYear=(exp(Coef)-1)*100) %>>% 
    dplyr::select(-Coef) %>>% 
    left_join(import('data/netcit_grupos.rds') %>>% select(nname,qtde.papers,PY.m) %>>% rename(Groups = nname)) %>>% 
    dplyr::arrange(Groups) %>>% 
    (. -> grupoAnoCrescimento) %>>%     
    dplyr::rename(AverageAge = PY.m) %>>% 
    dplyr::rename(TotalPapers = qtde.papers) %>>% 
    mutate(AverageAge = round(AverageAge,1)) %>>% 
    left_join(import('data/netcit_ZiPi.rds') %>>% mutate(Groups=grupo) %>>% select(Groups,Hubs)) %>>% 
    rename(Group = Groups) %>>% 
    (export(.,'data/groups_attributes.rds'))
# }}}

### Segmented Growth -------------------- {{{

# graphics
import('data/shelf_life_growth.txt') %>>% 
    as_tibble %>>% 
    rename(PY = V1, publications = V2 ) %>>% 
    dplyr::filter(PY %in% c(1980:2019)) %>>% 
    dplyr::arrange(PY) %>>% 
    dplyr::mutate(trend=1:n()) %>>% 
    (. -> d)

d$lnp <- log(d$publications)

PY <- d$PY

d$est <- ifelse(PY <= 1986.0, -441.3+(0.2239)*PY,
                ifelse(PY<=1992.0, -441.3 + (0.2239)*1986.0 + 0.0511*(PY-1986.0), 
                       ifelse(PY<=2004.8, -441.3 + (0.2239)*1986.0 + 0.0511*(1992.0-1986.0) + 0.1510*(PY-1992.0), 
                              -441.3 +  (0.2239)*1986.0 + 0.0511*(1992.0-1986.0) + 0.1510*(2004.8-1992.0) + 0.1186*(PY-2004.8)
                    )))

d %>>% 
    mutate(ln_Publications=lnp, Year=PY) %>>% 
    mutate(ln_Publications=round(ln_Publications,2), est=round(est,2)) %>>% 
    (export(.,'data/segmented_growth.rds'))

# }}}

### Networks -------------------- {{{

netcit <- import('data/netcit.rds') 
hubs <- import('data/netcit_hubs.rds') 

hubs %>>% 
    select(SR,Ki) %>>% 
    (. -> hubs2)

netcit %>>% 
    activate(nodes) %>>% 
    left_join(hubs2) %>>% 
    (. -> netcit)

# ALTERAR AQUI
ano <- 1990 

netcit %>>% 
    as_tbl_graph() %>>% 
    activate(nodes) %>>% 
    mutate(label=name) %>>% 
    mutate(label=paste( gsub(' .*$','',label), gsub('.*\\.','',label), sep='' )) %>>% 
    dplyr::filter(!is.na(grupo)) %>>% 
    dplyr::filter(PY <= ano) %>>%  
    (. -> netcit2)

tibble(id=1:length(V(netcit2)),
       label= V(netcit2)$label,
       group=V(netcit2)$grupo,
       year=V(netcit2)$PY
       ) %>>% 
(. -> nodes)

tibble(from = netcit2 %>>% activate(edges) %>>% as_tibble %>>%  pull(from),
       to = netcit2 %>>% activate(edges) %>>% as_tibble %>>%  pull(to)
       ) %>>% 
(. -> edges)

list(nodes=nodes,edges=edges) %>>% 
    (export(.,'data/networks.rds'))

# }}}

### Groups Growth -------------------- {{{

netcit <- import('data/netcit.rds')
a <- import('data/netcit_grupos.rds')

netcit %>>% 
    activate(nodes) %>>% 
    as_tibble %>>% 
    dplyr::filter(!is.na(grupo)) %>>% 
    group_by(PY,grupo) %>>% 
    tally(sort=TRUE) %>>% 
    arrange(grupo,desc(PY)) %>>% 
    ungroup %>>% 
    dplyr::filter(PY %in% c(2000:2019)) %>>% 
    dplyr::mutate(Group=grupo,Publications = n, Year = PY) %>>% 
    (export(.,'data/groups_growth.rds'))

# }}}

### Network -------------------- {{{

netcit <- import('data/netcit.rds') 
hubs <- import('data/netcit_hubs.rds') 

hubs %>>% 
    select(SR,Ki) %>>% 
    (. -> hubs2)

netcit %>>% 
    activate(nodes) %>>% 
    left_join(hubs2) %>>% 
    mutate(label=name) %>>% 
    mutate(label=paste( gsub(' .*$','',label), gsub('.*\\.','',label), sep='' )) %>>% 
    dplyr::filter(!is.na(grupo)) %>>% 
    (. -> netcit2)

## 1985
netcit2 %>>% 
    dplyr::filter(PY < 1985) %>>% 
    (. -> netcit3)

ggraph(netcit3, layout = 'fr') +
    geom_edge_fan(aes(alpha = stat(index)), show.legend = F) +
    geom_node_point(aes(size = degree(netcit3)), show.legend = F)  

ggsave('img/rede1.png')

## 1990
netcit2 %>>% 
    dplyr::filter(PY < 1990) %>>% 
    (. -> netcit3)

ggraph(netcit3, layout = 'fr') +
    geom_edge_fan(aes(alpha = stat(index)), show.legend = F) +
    geom_node_point(aes(size = degree(netcit3)), show.legend = F)  

ggsave('img/rede2.png')


## 1995
netcit2 %>>% 
    dplyr::filter(PY < 1995) %>>% 
    (. -> netcit3)

ggraph(netcit3, layout = 'fr') +
    geom_edge_fan(aes(alpha = stat(index)), show.legend = F) +
    geom_node_point(aes(size = degree(netcit3)), show.legend = F)  

ggsave('img/rede3.png')

## 2000
netcit2 %>>% 
    dplyr::filter(PY < 2000) %>>% 
    (. -> netcit3)

ggraph(netcit3, layout = 'kk') +
    geom_edge_fan(show.legend = F) +
    geom_node_point(aes(size = degree(netcit3)/20), show.legend = F)  

ggsave('img/rede4.png')

# plot(simplify(g), 
# vertex.size= 0.01,
# edge.arrow.size=0.001,
# vertex.label.cex = 0.75,
# vertex.label.color = "black",
# vertex.frame.color = adjustcolor("white", alpha.f = 0),
# vertex.color = adjustcolor("white", alpha.f = 0),
# edge.color=adjustcolor(1, alpha.f = 0.15),
# display.isolates=FALSE,
# vertex.label=ifelse(page_rank(g)$vector > 0.1 , "important nodes", NA))

# }}}

### Group g16 autores -------------------- {{{

M <- import('data/M.rds')
netcit <- import('data/netcit.rds')

netcit %>>% 
    activate(nodes) %>>% 
    as_tibble %>>% 
    right_join(M) %>>% 
    dplyr::relocate(name,grupo,qtde.papers,PY.m) %>>% 
    (metaTagExtraction(., Field = "AU_CO", sep = ";")) %>>% 
    (. -> M2)

grupo_analisado <- 'g16'

M2 %>>% 
    dplyr::filter(grupo==grupo_analisado) %>>% 
    (bibliometrix::biblioAnalysis(., sep = ";")) %>>% 
    (. -> results)

authors <- gsub(","," ",names(results$Authors))[1:200]
indices <- Hindex(M2, field = "author", elements=authors, sep = ";", years = 50)$H

export(indices, paste0('data/indices_',grupo_analisado,'.rds'))

M2 %>>% 
    dplyr::filter(grupo==grupo_analisado) %>>% 
    (authorProdOverTime(., k = 75, graph = F)) %>>% 
    (. -> authorProd)

export(authorProd, paste0('data/authorProd_',grupo_analisado,'.rds'))

M2 %>>% 
    dplyr::filter(grupo==grupo_analisado) %>>% 
    (authorProdOverTime(., k = 20, graph = T))  
ggsave(paste0('img/top_authors_',grupo_analisado,'.png'))

#----
# rede de coautoria
M2 %>>% 
    dplyr::filter(grupo==grupo_analisado) %>>% 
    as.data.frame(.) %>>% 
    (biblioNetwork(., analysis = "collaboration", network = "authors", sep = ";")) %>>% 
    (~ . -> NetMatrix) %>>% 
    networkStat %>>% 
    (. -> netcoau)

netcoau$graph %>>%  (. -> net)

authorProd$dfAU %>>% 
    tibble %>>% 
    group_by(Author) %>>% 
    summarise(Papers=sum(freq), TC=sum(TC), TCpY=mean(TCpY), firstPaper=min(year)) %>>% 
    ungroup  %>>% 
    arrange(desc(TCpY)) %>>% 
    rename(name = Author ) %>>% 
    (. -> a)

net %>>% 
    as_tbl_graph() %>>% 
    activate(nodes) %>>% 
    left_join(a) %>>% 
    dplyr::filter(!is.na(TC)) %>>% 
    mutate(label=name) %>>% 
    mutate(title=paste(name,paste0('TC = ',TC), paste0('TCpY = ', round(TCpY),2), sep='; ')) %>>% 
    (. -> net2)

wc <- cluster_louvain(net2)
V(net2)$comm <- membership(wc)
V(net2)$color <- colorize(membership(wc))
V(net2)$label.color <- colorize(membership(wc))
V(net2)$size <- V(net2)$TC/5

nodes <- as_data_frame(net2, what='vertices') %>>% as_tibble
edges <- as_data_frame(net2, what='edges') %>>% as_tibble

export(list(nodes=nodes,edges=edges), paste0('data/authorNet_',grupo_analisado,'.rds'))

visNetwork(nodes, edges, height = "750px", width='500px') %>>% 
    visIgraphLayout(layout = "layout_with_kk")  %>>% 
    visOptions(highlightNearest = TRUE)

#----
## gggraph
# utilizado para pre-visualizar a rede de coautoria
# ggraph(net2, layout = 'fr') +
#     geom_edge_link() +
#     geom_node_point(aes(size = grau), show.legend = F)  
#
# ggsave(paste0('img/authors_collaboration_',grupo_analisado,'.png'))


#----
# colaboracao entre paises

NetMatrix <- biblioNetwork(as.data.frame(M2[M2$grupo==grupo_analisado,]), analysis = "collaboration", network = "countries", sep = ";")

# Plot the network
net=networkPlot(NetMatrix, 
                n = 30, 
                Title = "Country Collaboration", 
                type = "circle", 
                size=TRUE, 
                remove.multiple=FALSE,
                labelsize=0.7,
                cluster="none",
                verbose = F)


net2 <- net$graph

wc <- cluster_louvain(net2)
V(net2)$comm <- membership(wc)
V(net2)$color <- colorize(membership(wc))
V(net2)$label.color <- colorize(membership(wc))
V(net2)$size <- V(net2)$deg*0.6

as_data_frame(net2, what='vertices') %>>% 
    as_tibble %>>% 
    mutate(id=name) %>>% 
    select(id,deg,size,color,comm,label) %>>% 
    mutate(title=id) %>>% 
    (. -> nodes)

as_data_frame(net2, what='edges') %>>% 
    as_tibble %>>% 
    select(from,to,num,width) %>>% 
    group_by(from,to) %>>% 
    summarise(width=sum(width)) %>>% 
    (. -> edges)

export(list(nodes=nodes,edges=edges), paste0('data/countryNet_',grupo_analisado,'.rds'))

visNetwork(nodes, edges, height = "750px", width='500px') %>>% 
    visIgraphLayout(layout = "layout_with_fr")  %>>% 
    visOptions(highlightNearest = TRUE)

# }}}

### Group g15 autores -------------------- {{{

M <- import('data/M.rds')
netcit <- import('data/netcit.rds')

netcit %>>% 
    activate(nodes) %>>% 
    as_tibble %>>% 
    right_join(M) %>>% 
    dplyr::relocate(name,grupo,qtde.papers,PY.m) %>>% 
    (metaTagExtraction(., Field = "AU_CO", sep = ";")) %>>% 
    (. -> M2)

grupo_analisado <- 'g15'

M2 %>>% 
    dplyr::filter(grupo==grupo_analisado) %>>% 
    (bibliometrix::biblioAnalysis(., sep = ";")) %>>% 
    (. -> results)

authors <- gsub(","," ",names(results$Authors))[1:200]
indices <- Hindex(M2, field = "author", elements=authors, sep = ";", years = 50)$H

export(indices, paste0('data/indices_',grupo_analisado,'.rds'))

M2 %>>% 
    dplyr::filter(grupo==grupo_analisado) %>>% 
    (authorProdOverTime(., k = 66, graph = F)) %>>% 
    (. -> authorProd)

export(authorProd, paste0('data/authorProd_',grupo_analisado,'.rds'))

M2 %>>% 
    dplyr::filter(grupo==grupo_analisado) %>>% 
    (authorProdOverTime(., k = 20, graph = T))  
ggsave(paste0('img/top_authors_',grupo_analisado,'.png'))

#----
# rede de coautoria
M2 %>>% 
    dplyr::filter(grupo==grupo_analisado) %>>% 
    as.data.frame(.) %>>% 
    (biblioNetwork(., analysis = "collaboration", network = "authors", sep = ";")) %>>% 
    (~ . -> NetMatrix) %>>% 
    networkStat %>>% 
    (. -> netcoau)

netcoau$graph %>>%  (. -> net)

authorProd$dfAU %>>% 
    tibble %>>% 
    group_by(Author) %>>% 
    summarise(Papers=sum(freq), TC=sum(TC), TCpY=mean(TCpY), firstPaper=min(year)) %>>% 
    ungroup  %>>% 
    arrange(desc(TCpY)) %>>% 
    rename(name = Author ) %>>% 
    (. -> a)

net %>>% 
    as_tbl_graph() %>>% 
    activate(nodes) %>>% 
    left_join(a) %>>% 
    dplyr::filter(!is.na(TC)) %>>% 
    mutate(label=name) %>>% 
    mutate(title=paste(name,paste0('TC = ',TC), paste0('TCpY = ', round(TCpY),2), sep='; ')) %>>% 
    (. -> net2)

wc <- cluster_louvain(net2)
V(net2)$comm <- membership(wc)
V(net2)$color <- colorize(membership(wc))
V(net2)$label.color <- colorize(membership(wc))
V(net2)$size <- V(net2)$TC/5

nodes <- as_data_frame(net2, what='vertices') %>>% as_tibble
edges <- as_data_frame(net2, what='edges') %>>% as_tibble

export(list(nodes=nodes,edges=edges), paste0('data/authorNet_',grupo_analisado,'.rds'))

visNetwork(nodes, edges, height = "750px", width='500px') %>>% 
    visIgraphLayout(layout = "layout_with_kk")  %>>% 
    visOptions(highlightNearest = TRUE)

#----
## gggraph
# utilizado para pre-visualizar a rede de coautoria
# ggraph(net2, layout = 'fr') +
#     geom_edge_link() +
#     geom_node_point(aes(size = grau), show.legend = F)  
#
# ggsave(paste0('img/authors_collaboration_',grupo_analisado,'.png'))


#----
# colaboracao entre paises

NetMatrix <- biblioNetwork(as.data.frame(M2[M2$grupo==grupo_analisado,]), analysis = "collaboration", network = "countries", sep = ";")

# Plot the network
net=networkPlot(NetMatrix, 
                n = 30, 
                Title = "Country Collaboration", 
                type = "circle", 
                size=TRUE, 
                remove.multiple=FALSE,
                labelsize=0.7,
                cluster="none",
                verbose = F)


net2 <- net$graph

wc <- cluster_louvain(net2)
V(net2)$comm <- membership(wc)
V(net2)$color <- colorize(membership(wc))
V(net2)$label.color <- colorize(membership(wc))
V(net2)$size <- V(net2)$deg

as_data_frame(net2, what='vertices') %>>% 
    as_tibble %>>% 
    mutate(id=name) %>>% 
    select(id,deg,size,color,comm,label) %>>% 
    mutate(title=id) %>>% 
    (. -> nodes)

as_data_frame(net2, what='edges') %>>% 
    as_tibble %>>% 
    select(from,to,num,width) %>>% 
    group_by(from,to) %>>% 
    summarise(width=sum(width)) %>>% 
    (. -> edges)

export(list(nodes=nodes,edges=edges), paste0('data/countryNet_',grupo_analisado,'.rds'))

visNetwork(nodes, edges, height = "750px", width='500px') %>>% 
    visIgraphLayout(layout = "layout_with_fr")  %>>% 
    visOptions(highlightNearest = TRUE)

# }}}

### Group g03 autores -------------------- {{{

M <- import('data/M.rds')
netcit <- import('data/netcit.rds')

netcit %>>% 
    activate(nodes) %>>% 
    as_tibble %>>% 
    right_join(M) %>>% 
    dplyr::relocate(name,grupo,qtde.papers,PY.m) %>>% 
    (metaTagExtraction(., Field = "AU_CO", sep = ";")) %>>% 
    (. -> M2)

grupo_analisado <- 'g03'

M2 %>>% 
    dplyr::filter(grupo==grupo_analisado) %>>% 
    (bibliometrix::biblioAnalysis(., sep = ";")) %>>% 
    (. -> results)

authors <- gsub(","," ",names(results$Authors))[1:200]
indices <- Hindex(M2, field = "author", elements=authors, sep = ";", years = 50)$H

export(indices, paste0('data/indices_',grupo_analisado,'.rds'))

M2 %>>% 
    dplyr::filter(grupo==grupo_analisado) %>>% 
    (authorProdOverTime(., k = 66, graph = F)) %>>% 
    (. -> authorProd)

export(authorProd, paste0('data/authorProd_',grupo_analisado,'.rds'))

M2 %>>% 
    dplyr::filter(grupo==grupo_analisado) %>>% 
    (authorProdOverTime(., k = 20, graph = T))  
ggsave(paste0('img/top_authors_',grupo_analisado,'.png'))

#----
# rede de coautoria
M2 %>>% 
    dplyr::filter(grupo==grupo_analisado) %>>% 
    as.data.frame(.) %>>% 
    (biblioNetwork(., analysis = "collaboration", network = "authors", sep = ";")) %>>% 
    (~ . -> NetMatrix) %>>% 
    networkStat %>>% 
    (. -> netcoau)

netcoau$graph %>>%  (. -> net)

authorProd$dfAU %>>% 
    tibble %>>% 
    group_by(Author) %>>% 
    summarise(Papers=sum(freq), TC=sum(TC), TCpY=mean(TCpY), firstPaper=min(year)) %>>% 
    ungroup  %>>% 
    arrange(desc(TCpY)) %>>% 
    rename(name = Author ) %>>% 
    (. -> a)

net %>>% 
    as_tbl_graph() %>>% 
    activate(nodes) %>>% 
    left_join(a) %>>% 
    dplyr::filter(!is.na(TC)) %>>% 
    mutate(label=name) %>>% 
    mutate(title=paste(name,paste0('TC = ',TC), paste0('TCpY = ', round(TCpY),2), sep='; ')) %>>% 
    (. -> net2)

wc <- cluster_louvain(net2)
V(net2)$comm <- membership(wc)
V(net2)$color <- colorize(membership(wc))
V(net2)$label.color <- colorize(membership(wc))
V(net2)$size <- V(net2)$TC/15

nodes <- as_data_frame(net2, what='vertices') %>>% as_tibble
edges <- as_data_frame(net2, what='edges') %>>% as_tibble

export(list(nodes=nodes,edges=edges), paste0('data/authorNet_',grupo_analisado,'.rds'))

visNetwork(nodes, edges, height = "750px", width='500px') %>>% 
    visIgraphLayout(layout = "layout_with_kk")  %>>% 
    visOptions(highlightNearest = TRUE)

#----
## gggraph
# utilizado para pre-visualizar a rede de coautoria
# ggraph(net2, layout = 'fr') +
#     geom_edge_link() +
#     geom_node_point(aes(size = grau), show.legend = F)  
#
# ggsave(paste0('img/authors_collaboration_',grupo_analisado,'.png'))


#----
# colaboracao entre paises

NetMatrix <- biblioNetwork(as.data.frame(M2[M2$grupo==grupo_analisado,]), analysis = "collaboration", network = "countries", sep = ";")

# Plot the network
net=networkPlot(NetMatrix, 
                n = 30, 
                Title = "Country Collaboration", 
                type = "circle", 
                size=TRUE, 
                remove.multiple=FALSE,
                labelsize=0.7,
                cluster="none",
                verbose = F)


net2 <- net$graph

wc <- cluster_louvain(net2)
V(net2)$comm <- membership(wc)
V(net2)$color <- colorize(membership(wc))
V(net2)$label.color <- colorize(membership(wc))
V(net2)$size <- V(net2)$deg/10

as_data_frame(net2, what='vertices') %>>% 
    as_tibble %>>% 
    mutate(id=name) %>>% 
    select(id,deg,size,color,comm,label) %>>% 
    mutate(title=id) %>>% 
    (. -> nodes)

as_data_frame(net2, what='edges') %>>% 
    as_tibble %>>% 
    select(from,to,num,width) %>>% 
    group_by(from,to) %>>% 
    summarise(width=sum(width)) %>>% 
    (. -> edges)

export(list(nodes=nodes,edges=edges), paste0('data/countryNet_',grupo_analisado,'.rds'))

visNetwork(nodes, edges, height = "750px", width='500px') %>>% 
    visIgraphLayout(layout = "layout_with_fr")  %>>% 
    visOptions(highlightNearest = TRUE)

# }}}

### Group g09 autores -------------------- {{{

M <- import('data/M.rds')
netcit <- import('data/netcit.rds')

netcit %>>% 
    activate(nodes) %>>% 
    as_tibble %>>% 
    right_join(M) %>>% 
    dplyr::relocate(name,grupo,qtde.papers,PY.m) %>>% 
    (metaTagExtraction(., Field = "AU_CO", sep = ";")) %>>% 
    (. -> M2)

grupo_analisado <- 'g09'

M2 %>>% 
    dplyr::filter(grupo==grupo_analisado) %>>% 
    (bibliometrix::biblioAnalysis(., sep = ";")) %>>% 
    (. -> results)

authors <- gsub(","," ",names(results$Authors))[1:200]
indices <- Hindex(M2, field = "author", elements=authors, sep = ";", years = 50)$H

export(indices, paste0('data/indices_',grupo_analisado,'.rds'))

M2 %>>% 
    dplyr::filter(grupo==grupo_analisado) %>>% 
    (authorProdOverTime(., k = 100, graph = F)) %>>% 
    (. -> authorProd)

export(authorProd, paste0('data/authorProd_',grupo_analisado,'.rds'))

M2 %>>% 
    dplyr::filter(grupo==grupo_analisado) %>>% 
    (authorProdOverTime(., k = 20, graph = T))  
ggsave(paste0('img/top_authors_',grupo_analisado,'.png'))

#----
# rede de coautoria
M2 %>>% 
    dplyr::filter(grupo==grupo_analisado) %>>% 
    as.data.frame(.) %>>% 
    (biblioNetwork(., analysis = "collaboration", network = "authors", sep = ";")) %>>% 
    (~ . -> NetMatrix) %>>% 
    networkStat %>>% 
    (. -> netcoau)

netcoau$graph %>>%  (. -> net)

authorProd$dfAU %>>% 
    tibble %>>% 
    group_by(Author) %>>% 
    summarise(Papers=sum(freq), TC=sum(TC), TCpY=mean(TCpY), firstPaper=min(year)) %>>% 
    ungroup  %>>% 
    arrange(desc(TCpY)) %>>% 
    rename(name = Author ) %>>% 
    (. -> a)

net %>>% 
    as_tbl_graph() %>>% 
    activate(nodes) %>>% 
    left_join(a) %>>% 
    dplyr::filter(!is.na(TC)) %>>% 
    mutate(label=name) %>>% 
    mutate(title=paste(name,paste0('TC = ',TC), paste0('TCpY = ', round(TCpY),2), sep='; ')) %>>% 
    (. -> net2)

wc <- cluster_louvain(net2)
V(net2)$comm <- membership(wc)
V(net2)$color <- colorize(membership(wc))
V(net2)$label.color <- colorize(membership(wc))
V(net2)$size <- V(net2)$TC/4

nodes <- as_data_frame(net2, what='vertices') %>>% as_tibble
edges <- as_data_frame(net2, what='edges') %>>% as_tibble

export(list(nodes=nodes,edges=edges), paste0('data/authorNet_',grupo_analisado,'.rds'))

visNetwork(nodes, edges, height = "750px", width='500px') %>>% 
    visIgraphLayout(layout = "layout_with_kk")  %>>% 
    visOptions(highlightNearest = TRUE)

#----
## gggraph
# utilizado para pre-visualizar a rede de coautoria
# ggraph(net2, layout = 'fr') +
#     geom_edge_link() +
#     geom_node_point(aes(size = grau), show.legend = F)  
#
# ggsave(paste0('img/authors_collaboration_',grupo_analisado,'.png'))


#----
# colaboracao entre paises

NetMatrix <- biblioNetwork(as.data.frame(M2[M2$grupo==grupo_analisado,]), analysis = "collaboration", network = "countries", sep = ";")

# Plot the network
net=networkPlot(NetMatrix, 
                n = 30, 
                Title = "Country Collaboration", 
                type = "circle", 
                size=TRUE, 
                remove.multiple=FALSE,
                labelsize=0.7,
                cluster="none",
                verbose = F)


net2 <- net$graph

wc <- cluster_louvain(net2)
V(net2)$comm <- membership(wc)
V(net2)$color <- colorize(membership(wc))
V(net2)$label.color <- colorize(membership(wc))
V(net2)$size <- V(net2)$deg/4

as_data_frame(net2, what='vertices') %>>% 
    as_tibble %>>% 
    mutate(id=name) %>>% 
    select(id,deg,size,color,comm,label) %>>% 
    mutate(title=id) %>>% 
    (. -> nodes)

as_data_frame(net2, what='edges') %>>% 
    as_tibble %>>% 
    select(from,to,num,width) %>>% 
    group_by(from,to) %>>% 
    summarise(width=sum(width)) %>>% 
    (. -> edges)

export(list(nodes=nodes,edges=edges), paste0('data/countryNet_',grupo_analisado,'.rds'))

visNetwork(nodes, edges, height = "750px", width='500px') %>>% 
    visIgraphLayout(layout = "layout_with_fr")  %>>% 
    visOptions(highlightNearest = TRUE)

# }}}

# vim: fdm=marker nowrap nospell
