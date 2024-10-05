###########
# Dataset #
###########

stop <- read.csv("D:/Coding/R Storage/M748/Project/Data/MplsStops.csv")
summary(stop)
dim(stop)

problem <- stop$problem
summary(as.numeric(problem))

# scale_fill_manual(values = c("firebrick4", "darkorange2", "goldenrod1", "seagreen4", "dodgerblue3", "slateblue2", "darkviolet") ) +
