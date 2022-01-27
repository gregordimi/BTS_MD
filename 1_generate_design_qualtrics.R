#############################
######    ChoiceDes    ######
#############################

library(choiceDes)
library(xtable)
library(plyr)


# Attributes:

attributes <- cbind(
  c("Desktop", "Mobile", "Desktop + Mobile"),
  c("Internet usage", "Internet usage + Apps usage", "Internet usage + Apps usage + Location"),
  c("8E", "10E", "12E")
)

colnames(attributes) <- c("Device", "Extent", "Reimbursement")
rownames(attributes) <- c("Level 1", "Level 2", "Level 3")

print( xtable( attributes ), include.rownames = TRUE)

# Conjoint design

set.seed(999)

factor_levels <- c(3,3,3)
design <- dcm.design(cand = factor_levels, nb = 1, sets = 12, alts = 2 , Rd = 50, fname = "design_CA.txt" ,print = FALSE)

saveRDS(object = design, file = "data/design.rds")


design$d.eff$V

design_CA <- design$levels
design_CA$X1 <- factor(x = design_CA$X1, levels = c(1:3), labels = c("Desktop data", "Mobile data", "Desktop data <br /> Mobile data"))
design_CA$X2 <- factor(x = design_CA$X2, levels = c(1:3), labels = c("Internet usage", "Internet usage <br /> Applications usage", "Internet usage <br /> Apps usage <br /> Location"))
design_CA$X3 <- factor(x = design_CA$X3, levels = c(1:3), labels = c("8 Euro", "10 Euro", "12 Euro"))

colnames(design_CA)[4:6] <- c("Device", "Extent", "Reimbursement")


############################
#### Prepare for import ####
############################

#LOAD LIMESURVEY TEMPLATE

limesurvey_str <- read.delim(file = "limesurvey/limesurvey_survey_366397.txt", stringsAsFactors = FALSE)

design_import <- design_CA[3:6]

design_import$task <- rep(1:2, 12)


design_import$Device <- sapply(design_import$Device, FUN = function(x) paste0("<div id='attr' class='att1'>",x,"</div>")  ) 
design_import$Extent <- sapply(design_import$Extent, FUN = function(x) paste0("<div id='attr' class='att2'>",x,"</div>")  )
design_import$Reimbursement <- sapply(design_import$Reimbursement, FUN = function(x) paste0("<div id='attr' class='att3'>",x,"</div>")  )


################################
#### EXPORT QUESTIONNAIRE   ####
################################

blocks <- list()
head <- as.data.frame(matrix(NA,nrow = 3, ncol = 1))
head [1,1] <- "[[AdvancedFormat]]"
head [3,1] <- "[[Block:CJNT Tasks]]"

for(i in 1:12){
  
  r1 <- seq(1,24,2)
  r2 <- seq(1,24,2)+1
  
  blocks[[i]] <- as.data.frame(matrix(NA,nrow = 10, ncol = 1))
  
  blocks[[i]][1,1] <- paste0("[[Question:MC:SingleAnswer:Vertical]]")
  blocks[[i]][2,1] <- paste0("[[ID:CJNT",i,"]]")
  blocks[[i]][3,1] <- "If the Datatron offers you these two products, which one would you prefer."
  
  blocks[[i]][5,1] <- "[[Choices]]"
  
  blocks[[i]][6,1] <- paste0( design_import$Reimbursement[r1[i]], design_import$Device[r1[i]], design_import$Extent[r1[i]] )
  
  blocks[[i]][7,1] <- paste0( design_import$Reimbursement[r2[i]], design_import$Device[r2[i]], design_import$Extent[r2[i]] )
  
  blocks[[i]][9,1] <- "[[PageBreak]]" 
  
  
}




blocks2 <- list()
head2 <- as.data.frame(matrix(NA,nrow = 3, ncol = 1))
head2 [1,1] <- ""
head2 [3,1] <- "[[Block:BTS CJNT Tasks]]"

for(i in 1:12){
  
  r1 <- seq(1,24,2)
  r2 <- seq(1,24,2)+1
  
  blocks2[[i]] <- as.data.frame(matrix(NA,nrow = 25, ncol = 1))
  
  blocks2[[i]][1,1] <- "[[Question:MC:SingleAnswer:Vertical]]"
  blocks2[[i]][2,1] <- paste0("[[ID:BCJNT",i,"]]")
  blocks2[[i]][3,1] <- "If the company offers you these two products, which one would you prefer."
  blocks2[[i]][5,1] <- "[[Choices]]"
  blocks2[[i]][6,1] <- paste0(design_import$Device[r1[i]], design_import$Extent[r1[i]], design_import$Reimbursement[r1[i]] )
  blocks2[[i]][7,1] <- paste0(design_import$Device[r2[i]], design_import$Extent[r2[i]], design_import$Reimbursement[r2[i]] )
  
  blocks2[[i]][9,1]  <- "[[Question:MC:SingleAnswer:Vertical]]"
  blocks2[[i]][10,1] <- paste0("[[ID:BTS",i,"]]")
  blocks2[[i]][11,1] <- "Think about the rest of the respondents participating in the survey. What percentage of them do you feel are going to select the same option as you just did?"
  blocks2[[i]][12,1] <- "[[Choices]]"
  
  for(n in 1:10){
    blocks2[[i]][12+n,1] <- paste0(seq(1,100,10)[n],""," - ",seq(10,100,10)[n],"%")
  }
  
  blocks2[[i]][24,1] <- "[[PageBreak]]" 
  
  
}

design_txt <- rbind(head, rbind.fill(blocks), head2,rbind.fill(blocks2))
design_txt[is.na(design_txt)]<-""

write.table(x = design_CA, file = "data/design_txt.txt", quote = FALSE, row.names = FALSE, col.names = FALSE, sep = "\t", na = "", fileEncoding =  "UTF-8")
write.table(x = design_txt, file = "data/design_txt.txt", quote = FALSE, row.names = FALSE, col.names = FALSE, sep = "\t", na = "", fileEncoding =  "UTF-8")

