library(pwr)
library(Hmisc)

print.line <- function(s) {
  cat(paste(s, "\n"))
}

print.header <- function(s) {
  print.line("=====================================================")
  print.line(s)
  print.line("=====================================================")
}

D.Pop = D[D$Topic == "PopCulture",]
D.Pop.Serif <- D.Pop[D.Pop$Font == "serif", ]
D.Pop.Sans <- D.Pop[D.Pop$Font == "sans-serif", ]
D.Pop.Male <- D.Pop[D.Pop$Sex == "Male", ]
D.Pop.Female <- D.Pop[D.Pop$Sex == "Female", ]

D.Science = D[D$Topic == "Science",]
D.Science.Serif <- D.Science[D.Science$Font == "serif", ]
D.Science.Sans <- D.Science[D.Science$Font == "sans-serif", ]
D.Science.Male <- D.Science[D.Science$Sex == "Male", ]
D.Science.Female <- D.Science[D.Science$Sex == "Female", ]

print.t.test <- function(x, y, s) {
  print.header(paste("Comparison of means -", s))
  print(t.test(x, y))
}

print.t.test(D.Pop.Serif$Agreement, D.Pop.Sans$Agreement, "Pop")
print.t.test(D.Science.Serif$Agreement, D.Science.Sans$Agreement, "Science")

print.t.test(D.Pop.Male$Agreement, D.Pop.Female$Agreement, "Pop - Sex")
print.t.test(D.Science.Male$Agreement, D.Science.Female$Agreement, "Science - Sex")

print.cor <- function(x, y, s) {
  print.header(paste("Pearson correlation test - ", s))
  print(rcorr(x, y))
}

print.cor(D.Pop$Age, D.Pop$Agreement, "Pop")
print.cor(D.Science$Age, D.Science$Agreement, "Science")

effectSize <- function(x, y) {
  mean.x <- mean(x)
  mean.y <- mean(y)
  sd <- sd(x)
  abs(mean.x - mean.y) / sd
}

print.pwr <- function(x, y, s) {
  n1 = length(x)
  n2 = length(y)
  d <- effectSize(x, y)
  print.header(paste("Statistical Power -", s))
  print(pwr.t2n.test(n1, n2, d, sig.level=.05, alternative="two.sided"))
}

print.pwr(D.Pop.Serif$Agreement, D.Pop.Sans$Agreement, "Pop")
print.pwr(D.Science.Serif$Agreement, D.Science.Sans$Agreement, "Science")
