#################################################################
# All of these visuals depend on the data from process.R
#

script.dir <- dirname(sys.frame(1)$ofile)
source(file.path(script.dir, "process.R"))
library(ggplot2)

D.pop <- D[D$Topic == "PopCulture",]
D.science <- D[D$Topic == "Science",]
D$Group <- paste(D$Topic, " & ", D$Font)

start.image <- function(name) {
  png(file.path(script.dir, "results", "charts", name))
}
end.image <- function() {
  dev.off()
}


start.image("Participants - Sample size.png")
plot <- ggplot(D, aes(Group))
plot <- plot + geom_bar()
print(plot)
end.image()

start.image("Participants - Age.png")
plot <- ggplot(D, aes(Group, Age))
plot <- plot + geom_boxplot()
print(plot)
end.image()

start.image("Participants - Sex.png")
plot <- ggplot(D, aes(Group))
plot <- plot + geom_bar(aes(fill = Sex))
plot <- plot + theme(legend.position = "bottom")
print(plot)
end.image()

plot.best.normal.fit <- function(data, name) {
  start.image(paste("Analysis -", name, "- Best normal fit.png"))
  plot <- ggplot(data, aes(x=Agreement))
  plot <- plot + geom_histogram(bins=15, aes(y=..density..))
  plot <- plot + stat_function(fun=dnorm, args=list(
    mean=mean(data$Agreement, na.rm = TRUE),
    sd=sd(data$Agreement, na.rm = TRUE)))
  print(plot)
  end.image()  
}

plot.best.normal.fit(D.pop, "Pop culture")
plot.best.normal.fit(D.science, "Science")

plot.comparison <- function(data, name, x) {
  start.image(paste("Analysis -", name, "- Comparison of means.png"))
  plot <- ggplot(data, aes(x, data$Agreement))
  plot <- plot + geom_boxplot()
  print(plot)
  end.image()  
}

plot.comparison(D.pop, "Pop culture - Font", D.pop$Font)
plot.comparison(D.science, "Science - Font", D.science$Font)

plot.comparison(D.pop, "Pop culture - Sex", D.pop$Sex)
plot.comparison(D.science, "Science - Sex", D.science$Sex)






