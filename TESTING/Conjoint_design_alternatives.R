#############################
######    support.CEs   #####
#############################

library(support.CEs)

design_CA <- Lma.design(candidate.array = NULL, 
                        attribute.names = list( device = c("1","2","3"), 
                                                extent = c("1", "2", "3"),
                                                reimb = c("1","2","3")),
                        nalternatives = 2, 
                        nblocks = 1, 
                        row.renames = FALSE, 
                        #randomize = FALSE, 
                        seed = 4)

design_CA <- rotation.design(candidate.array = NULL, 
                             attribute.names = list( device = c("1","2","3"), 
                                                     extent = c("1", "2", "3"),
                                                     reimb = c("1","2","3")),
                             nalternatives = 2, 
                             nblocks = 1, 
                             row.renames = FALSE, 
                             randomize = TRUE, 
                             seed = 4)

questionnaire(choice.experiment.design = design_CA, quote = FALSE)

design_matrix_CA <- make.design.matrix( choice.experiment.design = design_CA,
                                        optout = FALSE, 
                                        categorical.attributes = NULL,
                                        continuous.attributes =  c("device", "extent","reimb"),
                                        unlabeled = TRUE, 
                                        binary = FALSE)








#############################
######    ChoiceDes    ######
#############################

library(choiceDes)

levs1 <- c(3,3,3)
des <- dcm.design(cand = levs1, nb = 10, sets = 1, alts = 2 ,print = TRUE)

des <- dcm.design(cand = levs1, nb = 2, sets = 12, alts = 2 , Rd = 999, print = TRUE)


levs2 <- list(c(3,3), c(5,4))
des <- dcm.design(levs2, nb = 1, sets = 12, alts = 2, Rd = 999, print = TRUE)




#############################
######    AlgDesign    ######
#############################

library(AlgDesign)

full_design_CA = expand.grid(device = c("L","M","LM"), 
                             extent = c("H", "HA", "HAL"),
                             reimb = c("8","10","12"))

set.seed(666)

optFederov( ~ ., data = full_design_CA, nTrials = 13)

## TEST

# Blocking the design for a quadratic polynomial in three variables into two 
# seven trial blocks:

# FACTORS
#full_design_CA2 <-gen.factorial(levels = c(3,3,3), nVars = 3, varNames = c("D","E","R"), factors = "all")
# NON-FACTORS
full_design_CA2 <-gen.factorial(levels = c(3,3,3), nVars = 3, varNames = c("D","E","R"))

# Choose an optimum 14 trail design.
design_CA2 <- optFederov( frml = ~quad(.), data = full_design_CA2, nTrials=10, eval=TRUE ) 

# Letting optBlock() search the dat candidate list instead of first choosing a 
# 14 trial design.
design_CA3 <- optBlock( frml = ~quad(.), withinData = design_CA2$design, blocksizes = c(10,10))

design_CA4 <- optBlock( frml = ~quad(.), withinData = full_design_CA2, blocksizes = c(10,10))







# A block design for 7 treatments in 7 blocks of size 3. Note how withinData is recycled to fill out the blocksize requirements.

BIB <- optBlock( frml = ~. , withinData = factor(1:7), blocksizes=rep(3,7) )

# This is a balanced incomplete block design as may be seen from:

crossprod(table(BIB$rows,c(rep(1:7, rep(3,7)))))

# A partially balanced incomplete block design with two associate classes:

tr<-factor(1:9)
PBIB<-optBlock(~.,withinData=tr,blocksizes=rep(3,9))

crossprod(table(PBIB$rows,c(rep(1:9, rep(3,9)))))



# Two fractions of a 2^(4-1).

dat <- gen.factorial( levels = c(2,3,4,5), nVars =  4, varNames = c("v1","v2","v3","v4"), center = TRUE)
od <-optBlock( frml =  ~., withinData = dat, blocksizes = c(8,8), nRepeats = 50, criterion = "D" ) 

# The blocks are not themselves orthogonal even though the entire design is optimal.

bk<-data.matrix(od$Blocks$B1)
t(bk)%*%bk 

# Better blocks may be obtained as follows, but note that they are not generally 
# the fractions that would be obtained by confounding the third order interaction.

od <- optBlock( frml = ~., withinData = dat, blocksizes = c(8,8), criterion = "Dpc", nRepeats =10)
bk <- data.matrix(od$Blocks$B1)
t(bk) %*% bk

# Blocking with whole plot factors. Note that the 27 rows of within are recycled 
# to make the 54 trial blocked design. 

within <- expand.grid( A = c(-1,0,1), B = c(-1,0,1), C = c(-1,0,1) )
whole <- expand.grid( D = factor(1:3), E = factor(1:3) )
od <- optBlock( frml = ~D+E*(quad(A,B,C)), withinData = within, blocksizes = rep(6,9), wholeBlockData = whole )

# Either withinData, or wholeBlockData may be an approximate theory optimial design 
# produced by optFederov() for nTrials. The first column in the optFederov() output 
# design, named "Rep..", is used to replicate the trials. 

within <- optFederov( frml =  ~quad(A,B,C), data = within, nTrials =54, approx=TRUE )
od <- optBlock( frml = ~D+E*(quad(A,B,C)), withinData=within$design, blocksizes=rep(6,9) , wholeBlockData=whole)


