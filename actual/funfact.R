questions.columns = c(questions.pop.columns, questions.science.columns)
D.questions = rawD[,questions.columns]

questions = {}
credibility = {}

for (question.index in 1 : dim(D.questions)[2]) {
  answers <- D.questions[,question.index]
  answers <- answers[answers!=""]
  questions = c(questions, names(D.questions)[question.index])
  credibility <- c(credibility, mean(likert.to.numerical(answers)))
}

fun <- data.frame(questions, credibility)