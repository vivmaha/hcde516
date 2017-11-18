# Input data
D <- read.csv("C:/Users/vivma/Desktop/Pilot Study - V2_November 16, 2017_20.28.csv", stringsAsFactors=FALSE)
agreementQuestions = list(D$Q2, D$Q11, D$Q14, D$Q17, D$Q20, D$Q23, D$Q29, D$Q32, D$Q35, D$Q38, D$Q41, D$Q44, D$Q47, D$Q53, D$Q56, D$Q59)
categoryQuestions = list(D$Q1, D$Q12, D$Q15, D$Q18, D$Q21, D$Q24, D$Q30, D$Q33, D$Q36, D$Q39, D$Q42, D$Q45, D$Q48, D$Q54, D$Q57, D$Q60)

# Convert the names on the likert scale (eg. "Strongly Disagree") to numbers (eg. 1)
likert.to.numerical <- function (x) {
  values <- c(1,2,3,4,5)
  names(values)=c("Strongly Disagree", "Disagree", "Undecided", "Agree", "Strongly Agree")
  values[x]
}

# Determines if a question should be rejected
verify.question <- function(agreementData, categoryData) {
  
  question <- substr(agreementData[1],nchar("I believe the following statement:\n") + 1, 10000)
  
  cat(paste("Q.", question, "\n"))
  
  # The first to rows are header information, not real sample points from participants
  agreementData <- tail(agreementData, -2)
  categoryData <- tail(categoryData, -2)
  
  agreementData <- likert.to.numerical(agreementData)
  
  verify.threshold.check <- function(threshold, predicate) {
    sum(predicate) > threshold * length(predicate)
  }
  
  verify.reject.message <- function(threshold, message) {
    cat(paste("! REJECT the above question because over", threshold, "of participants", message, "\n"))
  }
  
  verify <- function(threshold, predicate, message) {
    if (verify.threshold.check(threshold, predicate)) {
      verify.reject.message(threshold, message)
    }
  }
  
  # Reject questions where 75% of folks answered "strongly disagree", "undecided", or "strongly agree"
  extremeValues <- c(1,3,5)
  for (extremeValue in extremeValues) {
    verify(0.75, agreementData == extremeValue, paste("responded with a value of", extremeValue))
  }
  
  # Reject questions where 90% of folks agreed, or 90% of folks disagreed
  verify(0.9, agreementData > 3, "agreed")
  verify(0.9, agreementData < 3, "disagreed")
  
  # Reject questions unless over 80% of folks can agree on a category (science vs. pop culture)
  if (!verify.threshold.check(0.8, categoryData == "Science") &&
      !verify.threshold.check(0.8, categoryData == "Pop Culture")) {
    verify.reject.message(0.8, "did not agree on a category (science vs. pop culture)")
  }
}

for (i in 1 : length(agreementQuestions)) {
  verify.question(agreementQuestions[[i]], categoryQuestions[[i]])
}