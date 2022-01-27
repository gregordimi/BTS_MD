#############################
######    MAxDiff      ######
#############################


library(choiceDes)

des <- tradeoff.des(items =  12, shown = 4, vers = 10, tasks = 15, Rd = 50, Rc = 50)

typeof(des$balance)


as.data.frame(des$balance$two.way$tbl)
