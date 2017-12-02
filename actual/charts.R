#################################################################
# All of these visuals depend on the data from process.R
#

script.dir <- dirname(sys.frame(1)$ofile)
source(file.path(script.dir, "process.R"))

D.pop <- D[D$Topic == "PopCulture",]
D.science <- D[D$Topic == "Science",]
D$Group <- paste(D$Topic, " & ", D$Font)


#################################################################
# Sample size across each group
#

plot <- ggplot(D, aes(Group))
plot <- plot + geom_bar()
print(plot)


#################################################################
# Age across all groups
#

plot <- ggplot(D, aes(Group, Age))
plot <- plot + geom_boxplot()
print(plot)

