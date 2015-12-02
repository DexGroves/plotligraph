# plotligraph
3D graphs in R powered by `plotly` and `igraph`.

Hacks with `plotly` to trick it into producing a rotatable, pannable and zoomable 3D render of an `igraph` graph
[that will look something like this.](https://rawgit.com/dexgroves/plotligraph/html/index.html)

## Disclaimer
OpenGL seems to render badly in Linux, and even worse inside VMs.

Due to forces beyond control, `plotligraph` will generate one `brewer.pal` warning per edge. Don't worry about it.

## How to plot
```R
# devtools::install_github("DexGroves/plotligraph")
library("plotly")
library("igraph")
library("plotligraph")

set.seed(1234)
graph <- barabasi.game(35)
V(graph)$name <- sample(letters, length(E(graph)), TRUE)
plotligraph(graph)
```

