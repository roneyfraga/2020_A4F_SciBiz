
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

# vim: fdm=marker nowrap
