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

for (row.index in 1 : dim(D)[1]) {
  row <- D[row.index,]
  responses.likert <- switch(row$Topic,
         Science = list(row$S01, row$S02, row$S03, row$S04, row$S05, row$S06, row$S07, row$S08, row$S09, row$S10, row$S11, row$S12, row$S13, row$S14, row$S15, row$S16, row$S17, row$S18, row$S19, row$S20),
         # NOTE: One of the pop culture questions was blank by mistake (D$Q86) so data for this question was discarded.
         PopCulture = list(row$P01, row$Q82, row$Q83, row$Q84, row$Q85, row$Q87, row$Q88, row$Q89, row$Q90, row$Q91, row$Q92, row$Q93, row$Q94, row$Q95, row$Q96, row$Q97, row$Q98, row$Q99, row$Q100)
  )
  responses.numerical = sapply(responses.likert, likert.to.numerical)
  
  D$Agreement[row.index] = mean(responses.numerical)
}

#################################################################
# Get rid of all the columns we don't care about
#

D <- D[,c("YearOfBirth", "Sex", "Topic", "Font", "Agreement")]