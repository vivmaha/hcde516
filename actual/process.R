script.dir <- dirname(sys.frame(1)$ofile)

# Get approval survey codes from m turk data
data.mturk.file <- file.path(script.dir, "data-mturk.csv")
M <- read.csv(data.mturk.file, stringsAsFactors = FALSE)
cat(paste("Total entries:", length(M$AssignmentStatus), "\n"))
cat(paste("Rejected entries:", sum(M$AssignmentStatus == "Rejected") + 1, "\n"))
cat(paste("Approved entries:", sum(M$AssignmentStatus == "Approved") - 1, "\n"))
# NOTE: The +1 and -1 is because I realized I should reject an entry (failed attention test) AFTER finalizing the mTurk batch, 
# so I had to throw away the entry and was unable to get another HIT from mTurk. I removed the entry from qualtrics directly so
# it would not show up there. Retrospectively, I should have left the entry in qualtrics, and removed it in the code here.
approvedCodes <- M[M$AssignmentStatus == "Approved", "Answer.surveycode"]
approvedCodes <- c(approvedCodes, "CompletionCode")

read.data.qualtrics <- function(x) {
  # Get survey result data from qualtrics (and filter to the ones approved in m turk)
  data.qualtrics.file <- file.path(script.dir, x)
  D <- read.csv(data.qualtrics.file , stringsAsFactors = FALSE)
  D <- D[D$CompletionCode %in% approvedCodes,]
}

D.serif <- read.data.qualtrics("data-serif-qualtrics.csv")
D.sans <- read.data.qualtrics("data-sans-qualtrics.csv")
D <- rbind(D.serif, D.sans)

questions.science = list(D$S01, D$S02, D$S03, D$S04, D$S05, D$S06, D$S07, D$S08, D$S09, D$S10, D$S11, D$S12, D$S13, D$S14, D$S15, D$S16, D$S17, D$S18, D$S19, D$S20)
questions.pop = list(D$P01, D$Q82, D$Q83, D$Q84, D$Q85, D$Q87, D$Q88, D$Q89, D$Q90, D$Q91, D$Q92, D$Q93, D$Q94, D$Q95, D$Q96, D$Q97, D$Q98, D$Q99, D$Q100)
# NOTE: One of the pop culture questions was blank by mistake (D$Q86) so data for this question was discarded.

stop()

# Convert the names on the likert scale (eg. "Strongly Disagree") to numbers (eg. 1)
likert.to.numerical <- function (x) {
  values <- c(1,2,3,4,5)
  names(values)=c("Strongly Disagree", "Disagree", "Undecided", "Agree", "Strongly Agree")
  values[x]
}


# Determines if a question should be rejected
verify.question <- function(agreementData, categoryData) {
  
  question <- substr(agreementData[1],nchar("I believe the following statement:\n") + 1, 10000)
  question <- trimws(gsub("[\r\n]", "", question))
  
  cat(paste("Q.", question, "\n"))
  
  # The first to rows are header information, not real sample points from participants
  agreementData <- tail(agreementData, -1)
  categoryData <- tail(categoryData, -1)
  
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