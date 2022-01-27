#############################
######    ChoiceDes    ######
#############################

library(choiceDes)
library(xtable)
library(plyr)


# Attributes:

attributes <- cbind(
  c("Desktop", "Mobile", "Desktop/Mobile"),
  c("Internet usage", "Internet usage/Apps usage", "Internet usage/Apps usage/Location"),
  c("8E", "10E", "12E")
)

colnames(attributes) <- c("Device", "Extent", "Reimbursement")
rownames(attributes) <- c("Level 1", "Level 2", "Level 3")

print( xtable( attributes ), include.rownames = TRUE)

# Conjoint design

library(choiceDes)

set.seed(999)

factor_levels <- c(3,3,3)
design <- dcm.design(cand = factor_levels, nb = 1, sets = 12, alts = 2 , Rd = 50, fname = "design_CA.txt" ,print = FALSE)

design$d.eff$V

design_CA <- design$levels
design_CA$X1 <- factor(x = design_CA$X1, levels = c(1:3), labels = c("Desktop", "Mobile", "Desktop/Mobile"))
design_CA$X2 <- factor(x = design_CA$X2, levels = c(1:3), labels = c("Internet usage", "Internet usage/Apps usage", "Internet usage/Apps usage/Location"))
design_CA$X3 <- factor(x = design_CA$X3, levels = c(1:3), labels = c("8E", "10E", "12E"))

colnames(design_CA)[4:6] <- c("Device", "Extent", "Reimbursement")


############################
#### Prepare for import ####
############################

#LOAD LIMESURVEY TEMPLATE

limesurvey_str <- read.delim(file = "limesurvey/limesurvey_survey_366397.txt", stringsAsFactors = FALSE)

design_import <- design_CA[3:6]

design_import$task <- rep(1:2, 12)
design_import$Device <- unlist( lapply(design_import$Device, FUN = function(x) paste0("<div id='attr' class='att1'>",x,"</div>")  ) )
design_import$Extent <- unlist( lapply(design_import$Extent, FUN = function(x) paste0("<div id='attr' class='att2'>",x,"</div>")  ) )
design_import$Reimbursement <- unlist( lapply(design_import$Reimbursement, FUN = function(x) paste0("<div id='attr' class='att3'>",x,"</div>")  ) )

#GET QUESTIONNAIRE



################################
#### EXPORT QUESTIONNAIRE   ####
################################

blocks <- list()

for(i in 1:12){
  
  r1 <- seq(1,24,2)
  r2 <- seq(1,24,2)+1
  
  blocks[[i]] <- as.data.frame(limesurvey_str[63:78,])
  
  blocks[[i]][1,2] <- paste0("G",i-1)
  
  blocks[[i]][1,3] <- paste0("TASK",i)
  
  blocks[[i]][2,3] <- paste0("CBC",i)
  
  blocks[[i]][5,3] <- paste0("BTS",i)
  
  blocks[[i]][3,5] <- paste0(design_import$Device[r1[i]], design_import$Extent[r1[i]], design_import$Reimbursement[r1[i]] )
    
  blocks[[i]][4,5] <- paste0(design_import$Device[r2[i]], design_import$Extent[r2[i]], design_import$Reimbursement[r2[i]] )
  
  
}

View(blocks[2])

design_lsa <- rbind(limesurvey_str[1:62,] ,rbind.fill(blocks))


write.table(x = design_lsa, file = "limesurvey/design_lsa.txt", quote = FALSE, row.names = FALSE, col.names = TRUE, sep = "\t", na = "", fileEncoding =  "UTF-8")

