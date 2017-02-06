
library(stringr)
library(ggplot2)
library(rjson)

graph.data = function(json) {
  name = json$slug
  d = json$series[[1]]
  df = data.frame(
    size=unlist(lapply(d$results, function(x) x$size)),
    mean=unlist(lapply(d$results, function(x) x$stats$mean)),
    sd=unlist(lapply(d$results, function(x) x$stats$deviation))
  )
  names(df) = c("size", str_c(name, ".mean"), str_c(name, ".sd"))
  df
}

ggplot.data = function(orig) {
  t = melt(orig, id.vars="size")
  t = cbind(t$size, colsplit(t$variable, "\\.", names=c("operation", "type")), t$value)
  names(t) = c("size", "operation", "type", "value")
  dcast(t, size + operation ~ ...)
}

graph = function(data) {
  ggd = ggplot.data(data)
  ggplot(ggd, aes(x=size, y=mean, colour=operation)) +
    geom_point() +
    geom_line() +
    geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd)) +
    ylab("seconds") +
    theme_bw()
}

files = Sys.glob("tmp/*.json")
df = NULL
for(file in files) {
  df2 = graph.data(fromJSON(file=file))
  if(!is.null(df)) {
    df = merge(df, df2, by="size", all=TRUE)
  } else {
    df = df2
  }
}


