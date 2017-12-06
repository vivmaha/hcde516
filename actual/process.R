script.dir <- dirname(sys.frame(1)$ofile)

#################################################################
# Get approval survey codes from mTurk data
#

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

#################################################################
# Get response data from qualtrics data
#

read.data.qualtrics <- function(filename, fontname) {
  # Get survey result data from qualtrics (and filter to the ones approved in m turk)
  data.qualtrics.file <- file.path(script.dir, filename)
  D <- read.csv(data.qualtrics.file , stringsAsFactors = FALSE)
  # Remove the first row, which is a header
  D <- tail(D,-1)
  D <- D[D$CompletionCode %in% approvedCodes,]
  D$Font <- fontname
  names(D)[names(D) == 'D1'] <- 'YearOfBirth'
  names(D)[names(D) == 'D2'] <- 'Sex'
  D
}

D.serif <- read.data.qualtrics("data-serif-qualtrics.csv", "serif")
D.sans <- read.data.qualtrics("data-sans-qualtrics.csv", "sans-serif")
D <- rbind(D.serif, D.sans)

#################################################################
# Convert likert strings to numerical values and average them per-participant to create an "Agreement" column
#

# Convert the names on the likert scale (eg. "Strongly Disagree") to numbers (eg. 1)
likert.to.numerical <- function (x) {
  values <- c(1,2,3,4,5)
  names(values)=c("Strongly Disagree", "Disagree", "Undecided", "Agree", "Strongly Agree")
  values[x]
}

questions.science.columns <- c("S01", "S02", "S03", "S04", "S05", "S06", "S07", "S08", "S09", "S10", "S11", "S12", "S13", "S14", "S15", "S16", "S17", "S18", "S19", "S20")
questions.pop.columns <- c("P01", "Q82", "Q83", "Q84", "Q85", "Q87", "Q88", "Q89", "Q90", "Q91", "Q92", "Q93", "Q94", "Q95", "Q96", "Q97", "Q98", "Q99", "Q100")

get.questions.columns <- function(row) {
  switch(row$Topic,
         Science = row[,questions.science.columns],
         # NOTE: One of the pop culture questions was blank by mistake (D$Q86) so data for this question was discarded.
         PopCulture = row[,questions.pop.columns] 
  )
}

for (row.index in 1 : dim(D)[1]) {
  row <- D[row.index,]
  responses.likert <- get.questions.columns(row)
  responses.numerical = sapply(responses.likert, likert.to.numerical)
  D$Agreement[row.index] = mean(responses.numerical)
}


#################################################################
# Convert YearOfBirth to Age
#

D$Age <- 2017 - as.numeric(D$YearOfBirth)

#################################################################
# Get rid of all the columns we don't care about
#
rawD <- D
D <- D[,c("Age", "Sex", "Topic", "Font", "Agreement")]