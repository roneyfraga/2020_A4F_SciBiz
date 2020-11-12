
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

netcoup <- import('data/netcoup.rds')
a <- import('data/netcoup_grupos.rds')

netcoup %>>% 
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
    m2 <- nls(publications ~ b0*exp(b1*(PY-2010)), start = list(b0=beta0, b1=beta1), data=d)
    res[[i]] <- coef(m2)[2]

}

# print(xtable(grupoAnoCrescimento, type = "latex"))

data.frame(Groups=grupos,Coef=res) %>>% 
    as_tibble %>>% 
    mutate(GrowthRateYear=(exp(Coef)-1)*100) %>>% 
    dplyr::select(-Coef) %>>% 
    left_join(import('data/netcoup_grupos.rds') %>>% select(nname,qtde.papers,PY.m) %>>% rename(Groups = nname)) %>>% 
    dplyr::arrange(Groups) %>>% 
    (. -> grupoAnoCrescimento) %>>%     
    dplyr::rename(AverageAge = PY.m) %>>% 
    dplyr::rename(TotalPapers = qtde.papers) %>>% 
    mutate(AverageAge = round(AverageAge,1)) %>>% 
    left_join(import('data/ZiPi.rds') %>>% mutate(Groups=grupo) %>>% select(Groups,Hubs)) %>>% 
    mutate(Description='Adicionar a descrição do grupo. Manter um texto o mais explicativo possível.') %>>% 
    relocate(Description, .after=Groups) %>>% 
    select(-Description) %>>% 
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

netcoup <- import('data/netcoup.rds') 
hubs <- import('data/netcoup_hubs.rds') 

hubs %>>% 
    select(SR,Ki) %>>% 
    (. -> hubs2)

netcoup %>>% 
    activate(nodes) %>>% 
    left_join(hubs2) %>>% 
    (. -> netcoup)

# ALTERAR AQUI
ano <- 1990 

netcoup %>>% 
    as_tbl_graph() %>>% 
    activate(nodes) %>>% 
    mutate(label=name) %>>% 
    mutate(label=paste( gsub(' .*$','',label), gsub('.*\\.','',label), sep='' )) %>>% 
    dplyr::filter(!is.na(grupo)) %>>% 
    dplyr::filter(PY <= ano) %>>%  
    (. -> netcoup2)

tibble(id=1:length(V(netcoup2)),
       label= V(netcoup2)$label,
       group=V(netcoup2)$grupo,
       year=V(netcoup2)$PY
       ) %>>% 
(. -> nodes)

tibble(from = netcoup2 %>>% activate(edges) %>>% as_tibble %>>%  pull(from),
       to = netcoup2 %>>% activate(edges) %>>% as_tibble %>>%  pull(to)
       ) %>>% 
(. -> edges)

list(nodes=nodes,edges=edges) %>>% 
    (export(.,'data/networks.rds'))

# }}}

### Groups Growth -------------------- {{{

netcoup <- import('data/netcoup.rds')
a <- import('data/netcoup_grupos.rds')

netcoup %>>% 
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

netcoup <- import('data/netcoup.rds') 
hubs <- import('data/netcoup_hubs.rds') 

hubs %>>% 
    select(SR,Ki) %>>% 
    (. -> hubs2)

netcoup %>>% 
    activate(nodes) %>>% 
    left_join(hubs2) %>>% 
    mutate(label=name) %>>% 
    mutate(label=paste( gsub(' .*$','',label), gsub('.*\\.','',label), sep='' )) %>>% 
    dplyr::filter(!is.na(grupo)) %>>% 
    (. -> netcoup2)

## 1985
netcoup2 %>>% 
    dplyr::filter(PY < 1985) %>>% 
    (. -> netcoup3)

ggraph(netcoup3, layout = 'fr') +
    geom_edge_fan(aes(alpha = stat(index)), show.legend = F) +
    geom_node_point(aes(size = degree(netcoup3)), show.legend = F)  

ggsave('img/rede1.png')

## 1990
netcoup2 %>>% 
    dplyr::filter(PY < 1990) %>>% 
    (. -> netcoup3)

ggraph(netcoup3, layout = 'fr') +
    geom_edge_fan(aes(alpha = stat(index)), show.legend = F) +
    geom_node_point(aes(size = degree(netcoup3)), show.legend = F)  

ggsave('img/rede2.png')


## 1995
netcoup2 %>>% 
    dplyr::filter(PY < 1995) %>>% 
    (. -> netcoup3)

ggraph(netcoup3, layout = 'fr') +
    geom_edge_fan(aes(alpha = stat(index)), show.legend = F) +
    geom_node_point(aes(size = degree(netcoup3)), show.legend = F)  

ggsave('img/rede3.png')

## 2000
netcoup2 %>>% 
    dplyr::filter(PY < 2000) %>>% 
    (. -> netcoup3)

ggraph(netcoup3, layout = 'kk') +
    geom_edge_fan(show.legend = F) +
    geom_node_point(aes(size = degree(netcoup3)/20), show.legend = F)  

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

### Group g05 autores -------------------- {{{

M <- import('data/M.rds')
M$name <- paste(M$SR, M$PY, sep='. ')
netcoup <- import('data/netcoup.rds')

netcoup %>>% 
    activate(nodes) %>>% 
    as_tibble %>>% 
    right_join(M) %>>% 
    dplyr::relocate(name,grupo,qtde.papers,PY.m) %>>% 
    (. -> M2)

M2 <- metaTagExtraction(M2, Field = "AU_CO", sep = ";")

grupo_analisado <- 'g05'

M2 %>>% 
    dplyr::filter(grupo==grupo_analisado) %>>% 
    (bibliometrix::biblioAnalysis(., sep = ";")) %>>% 
    (. -> results)

authors <- gsub(","," ",names(results$Authors))[1:200]
indices <- Hindex(M2, field = "author", elements=authors, sep = ";", years = 50)$H

export(indices,'data/indices_g05.rds')

M2 %>>% 
    dplyr::filter(grupo==grupo_analisado) %>>% 
    (authorProdOverTime(., k = 15, graph = F)) %>>% 
    (. -> authorProd)

export(authorProd,'data/authorProd_g05.rds')

authorProd$graph
# ggsave('img/top_authors_g05.png')

M2 %>>% 
    dplyr::filter(grupo==grupo_analisado) %>>% 
    as.data.frame(.) %>>% 
    (biblioNetwork(., analysis = "collaboration", network = "authors", sep = ";")) %>>% 
    (~ . -> NetMatrix) %>>% 
    networkStat %>>% 
    (. -> netcoau)

netcoau$network

netcoau$graph %>>%  (. -> net)

V(net)$grau <- degree(net)

net %>>% 
    as_tbl_graph() %>>% 
    activate(nodes) %>>% 
    dplyr::filter(grau>18) %>>% 
    (. -> net2)

ggraph(net2, layout = 'fr') +
    geom_edge_link() +
    geom_node_point(aes(size = grau), show.legend = F)  
# ggsave('img/authors_collaboration_g05.png')

# }}}

### Group g05 countries -------------------- {{{

M2 <- metaTagExtraction(M2, Field = "AU_CO", sep = ";")

M2 %>>% 
    select(name,grupo,AU_CO) %>>% 
    dplyr::filter(grupo=='g05') %>>% 
    separate_rows(AU_CO, sep=';') %>>% 
    distinct(name,AU_CO, .keep_all=T) %>>% 
    dplyr::filter(!is.na(grupo)) %>>% 
    group_by(AU_CO) %>>% 
    count(sort=T, name='autores') %>>% 
    ungroup %>>% 
    slice_head(n=10) %>>% 
    rmarkdown::paged_table()

png("img/countries_collaboration_g05.png", width = 16, height = 16, units = 'cm', res = 300)
M2 %>>% 
    dplyr::filter(grupo==grupo_analisado) %>>% 
    as.data.frame() %>>% 
    (biblioNetwork(., analysis = "collaboration", network = "countries", sep = ";")) %>>% 
    (networkPlot(., n = 30, Title = "Country Collaboration", type = "kamada", 
                 size=TRUE, remove.multiple=FALSE,labelsize=0.7,cluster="none", verbose =T)) 
dev.off()

# ggsave('img/countries_collaboration_g05.png')


# ---
library("treemap")
library("viridis")

data(GNI2014)
head(GNI2014)

tm <- treemap(GNI2014,
  index = c("continent", "iso3"),
  vSize = "population", vColor = "GNI",
  type = "comp", palette = rev(viridis(6)),
  draw = FALSE
)

hctreemap(tm, allowDrillToNode = TRUE, layoutAlgorithm = "squarified") %>%
  hc_title(text = "Gross National Income World Data") %>%
  hc_tooltip(pointFormat = )

#
install.packages(c('d3treeR'), dependencies=TRUE) 

library(d3treeR)
data(GNI2014)
treex2<- treemap(GNI2014,
        index=c("continent", "iso3"),
        vSize="population",
        vColor="GNI",
        type="value",
        format.legend = list(scientific = FALSE, big.mark = " "))
d3tree2(treex2)

# }}}

# vim: fdm=marker nowrap
