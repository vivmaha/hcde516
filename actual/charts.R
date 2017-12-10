#################################################################
# All of these visuals depend on the data from process.R
#

script.dir <- dirname(sys.frame(1)$ofile)
source(file.path(script.dir, "process.R"))
library(ggplot2)


D$Credibility <- D$Agreement
D$Gender <- D$Sex

D.pop <- D[D$Topic == "PopCulture",]
D.science <- D[D$Topic == "Science",]
D$Group <- paste(D$Topic, " & ", D$Font)

start.image <- function(name) {
  png(
    filename = file.path(script.dir, "results", "charts", name),
    width = 288,
    height = 288,
    units = "px"
  )
}
end.image <- function() {
  dev.off()
}

#theme_update()
theme_set(
  theme_classic() +
    theme(text = element_text(size=14))
)

start.image("Participants - Sample size.png")
plot <- ggplot(D, aes(Group))
plot <- plot + geom_bar()
print(plot)
end.image()

plot.participants.age <- function(data, name) {
  start.image(paste("Participants -", name, "- Age.png"))
  plot <- ggplot(data, aes(Font, Age))
  plot <- plot + geom_boxplot()
  plot <- plot + labs(title=name)
  print(plot)
  end.image()
}

plot.participants.age(D.pop, "Popular Culture")
plot.participants.age(D.science, "Science")

plot.participants.gender <- function(data, name) {
  start.image(paste("Participants -", name, "- Gender.png"))
  plot <- ggplot(data, aes(Font))
  plot <- plot + geom_bar(aes(fill = Gender), position = "dodge")
  plot <- plot + labs(title=name, y = "Count")
  print(plot)
  end.image()
}

plot.participants.gender(D.pop, "Popular Culture")
plot.participants.gender(D.science, "Science")

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
  plot <- plot + xlim(2,5)
  plot <- plot + labs(title=name, y = "Count")
  print(plot)
  end.image()  
}

plot.best.normal.fit(D.pop, "Pop culture")
plot.best.normal.fit(D.science, "Science")

plot.comparison <- function(data, name, x, label.x) {
  start.image(paste("Analysis -", name, "-", label.x, "- Comparison of means.png"))
  plot <- ggplot(data, aes(x, data$Credibility))
  plot <- plot + geom_boxplot()
  plot <- plot + labs(x = label.x, y = "Credibility", title = name)
  plot <- plot + theme(text = element_text(size=16))
  plot <- plot + ylim(2,5)
  print(plot)
  end.image()  
}

plot.comparison(D.pop, "Pop culture", D.pop$Font, "Font")
plot.comparison(D.science, "Science", D.science$Font, "Font")

plot.comparison(D.pop, "Pop culture", D.pop$Gender, "Gender")
plot.comparison(D.science, "Science", D.science$Gender, "Gender")

plot.cor <- function(data, name) {
  start.image(paste("Analysis - Moderators - ", name, " - Age.png"))
  plot <- ggplot(data, aes(Age, Credibility))
  plot <- plot + geom_point();
  print(plot)
  end.image()
}

plot.cor(D.pop, "Pop culture")
plot.cor(D.science, " Science")
