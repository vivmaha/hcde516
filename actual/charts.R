#################################################################
# All of these visuals depend on the data from process.R
#

script.dir <- dirname(sys.frame(1)$ofile)
source(file.path(script.dir, "process.R"))
library(ggplot2)


D$Credibility <- D$Agreement

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
plot <- plot + geom_bar(aes(fill = Sex), position = "dodge")
plot <- plot + theme(legend.position = "bottom")
print(plot)
end.image()

plot.best.normal.fit <- function(data, name) {
  start.image(paste("Analysis -", name, "- Best normal fit.png"))
  n = length(data$Credibility)
  binwidth <- 0.15
  mean <- mean(data$Credibility, na.rm = TRUE)
  plot <- ggplot(data, aes(x=Credibility))
  plot <- plot + geom_histogram(binwidth=binwidth, aes(y=..count..))
  plot <- plot + stat_function(fun = function(x) dnorm(x, 
                                                       mean = mean(data$Credibility, na.rm = TRUE),
                                                       sd = sd(data$Credibility, na.rm = TRUE)
                                                       ) * n * binwidth)
  plot <- plot + xlim(1,5)
  print(plot)
  end.image()  
}

plot.best.normal.fit(D.pop, "Pop culture")
plot.best.normal.fit(D.science, "Science")

plot.comparison <- function(data, name, x, label.x) {
  start.image(paste("Analysis -", name, "- Comparison of means.png"))
  plot <- ggplot(data, aes(x, data$Credibility))
  plot <- plot + geom_boxplot()
  plot <- plot + labs(x = label.x, y = "Credibility")
  print(plot)
  end.image()  
}

plot.comparison(D.pop, "Pop culture - Font", D.pop$Font, "Font")
plot.comparison(D.science, "Science - Font", D.science$Font, "Font")

plot.comparison(D.pop, "Moderators - Pop culture - Sex", D.pop$Sex, "Sex")
plot.comparison(D.science, "Moderators - Science - Sex", D.science$Sex, "Sex")

plot.cor <- function(data, name) {
  start.image(paste("Analysis - Moderators - ", name, " - Age.png"))
  plot <- ggplot(data, aes(Age, Credibility))
  plot <- plot + geom_point();
  print(plot)
  end.image()
}

plot.cor(D.pop, "Pop culture")
plot.cor(D.science, " Science")
