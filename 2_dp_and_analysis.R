library(reshape2)
library(mlogit)
library(effects)
library(stargazer)
library(mfx)
library(ggplot2)
library(tidyr)
library(ROCR)
library(pscl)
library(caret)


# Reading in the survey data ----------------------------------------------


setwd(getwd())

s_data <- read.csv("data/20170816_data.csv", header = T, stringsAsFactors = F, na.strings = "")
s_data <- s_data[c(3:nrow(s_data)),]
s_data <- s_data[-which(is.na(s_data$Q46)),]

# Reading in the desing of the choice experiment --------------------------
design <- readRDS(file =  "data/design.rds")

full_design <- design$levels

names(full_design)[names(full_design)=="X1"] <- "device"
names(full_design)[names(full_design)=="X2"] <- "extent"
names(full_design)[names(full_design)=="X3"] <- "price"

full_design$vers <- NULL
full_design$task <- rep(x = 1:2,6)
full_design <- cbind(setNames( full_design[full_design$task==1,c("card","device","extent","price")],c("card",paste0(c("device","extent","price"),1))),
                     setNames( full_design[full_design$task==2,c("device","extent","price")], paste0(c("device","extent","price"),2)))  
full_design$card <- paste0("CJ",full_design$card)

#table(full_design$device1, full_design$device2)
#table(full_design$extent1, full_design$extent2)
#table(full_design$price1, full_design$price2)
#table(full_design$price1, full_design$extent1)
#table(full_design$price1, full_design$extent2)
#table(full_design$price2, full_design$extent1)
#table(full_design[,c("price1","price2")], full_design[,c("extent1","extent2")])
#table(design$levels$X1, design$levels$X2)
#table(design$levels$X1, design$levels$X3)
#table(design$levels$X2, design$levels$X3)

# Data processing ---------------------------------------------------------

s_data$group <- NA
#s_data$group <- apply(s_data[,c("CJNT1","BCJNT1")], 1, FUN = function(x) ifelse(is.na(x),1,2))
s_data$group[which(s_data$CJNT12==1 | s_data$CJNT12==2)] <- 1
#s_data[which(s_data$CJNT1==1),"Q46"]
s_data$group[which(s_data$BCJNT12==1 | s_data$BCJNT12==2)] <- 2
#s_data[which(s_data$BCJNT1==1),"Q46"]

t_data <- as.data.frame(matrix(data = NA, nrow = 0, ncol = 14))
names(t_data) <- c("ResponseId","group", paste0("CJ",1:12))

demo <- s_data[, c("ResponseId","Q5","Q44","Q45","Q46")]

temp_c <- s_data[ s_data$group==1, c("ResponseId", "group",paste0("CJNT",1:12))]
temp_b <- s_data[ s_data$group==2, c("ResponseId", "group",paste0("BCJNT",1:12))]

t_data <- rbind(t_data, setNames(object = temp_c, nm = names(t_data)))
t_data <- rbind(t_data, setNames(object = temp_b, nm = names(t_data)))


prop.table(table(t_data$CJ1, t_data$group), 2)

prop.table(table(t_data$CJ4, t_data$group), 2)

prop.table(table(t_data$CJ7, t_data$group), 2)

prop.table(table(t_data$CJ9, t_data$group), 2)

t.test()
#head(t_data)


m_data <- melt(t_data, id.vars = c("ResponseId","group"))
names(m_data)[names(m_data)=="value"] <- "choice"
names(m_data)[names(m_data)=="variable"] <- "card"
m_data$choice <- paste0("choice",m_data$choice)

#remove NAs
m_data <- m_data[m_data$choice!="choiceNA",]

freq_result <- dcast(data = m_data, formula = card + group ~ choice, fun.aggregate = length)

freq_result <- cbind(freq_result, setNames(object = data.frame(round(t(apply(freq_result[,c("choice1","choice2")], 1, FUN = function(x) x/sum(x))),2)), nm = paste0(c("choice1","choice2"), "_freq")))


final_all <- merge(m_data, full_design, by = "card")

names(final_all)[names(final_all)=="card"] <- "choiceid"
names(final_all)[names(final_all)=="ResponseId"] <- "id"

# Data preparation --------------------------------------------------------

final_all[apply(expand.grid(c("device","extent","price"), 1:2), 1, paste, collapse="")] <- 
                  lapply(final_all[apply(expand.grid(c("device","extent","price"), 1:2), 1, paste, collapse="")], function(x) as.numeric(as.character(x)) )


final_all <- merge(final_all, demo, by.x = "id", by.y = "ResponseId")
final_all[, c("Q5", "Q44", "Q45", "Q46")] <- apply(final_all[, c("Q5", "Q44", "Q45", "Q46")], 2, as.numeric)

final_all <- final_all[final_all$choice!="choiceNA",]


# Chen implementation -----------------------------------------------------
altvar <- list()

altvar$final_chen <- setNames(as.data.frame(matrix(data = NA, nrow = nrow(final_all), ncol = 4)), c("choice","device", "extent", "price", "group"))

#altvar$final_chen$device <- final_all$device1 - final_all$device2
#altvar$final_chen$extent <- final_all$extent1 - final_all$extent2
altvar$final_chen$price <- final_all$price1 - final_all$price2

altvar$final_chen$device_1 <- final_all$device1
altvar$final_chen$extent_1 <- final_all$extent1
altvar$final_chen$price_1  <- final_all$price1

altvar$final_chen$device_2 <- final_all$device2
altvar$final_chen$extent_2 <- final_all$extent2
altvar$final_chen$price_2  <- final_all$price2


#altvar$final_chen <- cbind(altvar$final_chen,
#setNames( data.frame(sapply(1:3, function(x) as.integer(x == final_all$device1))), paste0("device_1_",1:3)),
#setNames( data.frame(sapply(1:3, function(x) as.integer(x == final_all$device2))), paste0("device_2_",1:3)),
#setNames( data.frame(sapply(1:3, function(x) as.integer(x == final_all$extent1))), paste0("extent_1_",1:3)),
#setNames( data.frame(sapply(1:3, function(x) as.integer(x == final_all$extent2))), paste0("extent_2_",1:3))
#)

altvar$final_chen <- cbind(altvar$final_chen,
setNames( data.frame(sapply(1:3, function(x) as.integer(x == final_all$device1)) - sapply(1:3, function(x) as.integer(x == final_all$device2))), paste0("device",1:3)),
setNames( data.frame(sapply(1:3, function(x) as.integer(x == final_all$extent1)) - sapply(1:3, function(x) as.integer(x == final_all$extent2))), paste0("extent",1:3)))


View(altvar$final_chen)

altvar$final_chen$choice <- ifelse(final_all$choice=="choice1", 1, 0)

altvar$final_chen$group <- final_all$group

altvar$final_chen$age <- final_all$Q44
altvar$final_chen$gender <- final_all$Q45
altvar$final_chen$stat <- final_all$Q46

head(altvar$final_chen)

#altvar$model_chen <- formula( choice ~  device + extent + price + group + group*(device + extent + price))
altvar$model_full <- formula( choice ~  device1 + device2 + device3 + extent1 + extent2 + extent3 + price)
altvar$model_bts <- formula( choice ~  device1 + device2 + device3 + extent1 + extent2 + extent3 + price + group + group*(device1 + device2 + device3 + extent1 + extent2 + extent3 + price))

altvar$results_all     <- glm( altvar$model_full, data = altvar$final_chen, family = binomial(link = "logit"))
altvar$results_allbts  <- glm( altvar$model_bts, data = altvar$final_chen,  family = binomial(link = "logit"))
altvar$results_control <- glm( altvar$model_full, data = altvar$final_chen,  family = binomial(link = "logit"), subset = group==1)
altvar$results_bts     <- glm( altvar$model_full, data = altvar$final_chen,  family = binomial(link = "logit"), subset = group==2)


stargazer(altvar$results_all , altvar$results_control, altvar$results_bts, type = "text", column.labels = c("All", "Control", "BTS"))

stargazer(
  altvar$results_all,   
  altvar$results_allbts,
  altvar$results_control,
  altvar$results_bts, 
  type = "text", column.labels = c("All", "Control", "BTS"))


# Chen implementation -----------------------------------------------------

final_all_mlogit <- mlogit.data(final_all,
                                choice = "choice", 
                                shape = "wide", 
                                varying = 5:10, 
                                alt.levels = c("choice1", "choice2"), 
                                #opposite = c("device","extent","price"),
                                sep = "")

#final_all_mlogit$device <- factor(final_all_mlogit$device, labels = c("d","m","d+m"))
#final_all_mlogit$extent <- factor(final_all_mlogit$extent, labels = c("b","b+a","b+a+l"))
#final_all_mlogit$price <- factor(final_all_mlogit$price, labels = c("8","10","12"))

final_all_mlogit$price_8  <- as.logical(ifelse(final_all_mlogit$price==1,1,0))
final_all_mlogit$price_10 <- as.logical(ifelse(final_all_mlogit$price==2,1,0))
final_all_mlogit$price_12 <- as.logical(ifelse(final_all_mlogit$price==3,1,0))



final_all_mlogit$price_N <- ifelse(final_all_mlogit$price == 1,8,
                                   ifelse(final_all_mlogit$price == 2,10,
                                          ifelse(final_all_mlogit$price == 3,12,NA)))



final_all_mlogit$extent_1 <- as.logical(ifelse(final_all_mlogit$extent==1,1,0))
final_all_mlogit$extent_2 <- as.logical(ifelse(final_all_mlogit$extent==2,1,0))
final_all_mlogit$extent_3 <- as.logical(ifelse(final_all_mlogit$extent==3,1,0))

final_all_mlogit$device_1 <- as.logical(ifelse(final_all_mlogit$device==1,1,0))
final_all_mlogit$device_2 <- as.logical(ifelse(final_all_mlogit$device==2,1,0))
final_all_mlogit$device_3 <- as.logical(ifelse(final_all_mlogit$device==3,1,0))


#Effect coding
final_all_mlogit$device_e <- ifelse(final_all_mlogit$device == 1,1,
                                    ifelse(final_all_mlogit$device == 2,2,
                                           ifelse(final_all_mlogit$device == 3,3,NA)))
final_all_mlogit$extent_e <- ifelse(final_all_mlogit$extent == 1,1,
                                    ifelse(final_all_mlogit$extent == 2,0,
                                           ifelse(final_all_mlogit$extent == 3,-1,NA)))
final_all_mlogit$price_e <- ifelse(final_all_mlogit$price == 1,-1,
                                   ifelse(final_all_mlogit$price == 2,0,
                                          ifelse(final_all_mlogit$price == 3,1,NA)))


final_all_mlogit_g1 <- final_all_mlogit[final_all_mlogit$group==1,]
final_all_mlogit_g2 <- final_all_mlogit[final_all_mlogit$group==2,]

#use this one to compare with alt ver
#model_formula <- formula( choice ~ device + extent + price )


#model_formula <- formula( choice ~  device_1 + device_2 + device_3 + extent_1 + extent_2 + extent_3 + price_8 + price_10 + price_12 )
#model_formula <- formula( choice ~  device_1 + device_2 + device_3 + extent_1 + extent_2 + extent_3 + price_N )
#model_formula <- formula( choice ~  device_1 + device_2 + extent_1 + extent_2 + price_N )
#model_formula <- formula( choice ~  device_1 + device_2 + device_3 + extent_1 + extent_2 + extent_3 + price_N )
#model_formula <- formula( choice ~  device_1 + device_2 + device_1*device_2 + extent_1 + extent_2 + extent_3 + price_N )
#model_formula <- formula( choice ~  device_e + extent_e + price_e )

#compute the diffeercen between the actual choice and the prob and square
 
model_formula <- formula( choice ~  device_1 + device_2 + device_3 + extent_e + price_N )

ref_labels_ormx <- c("Desktop (B: Both)", "Mobile (B: Both)", "Browsing History (B: BH + AU + LI)", "Browsing History + Apps (B: BH + AU + LI)", "Price (Euro)")
ref_labels <- c("Desktop (D)", "Mobile (M)", "Base: D + M", "Browsing History (BH)", "BH + Apps Usage (AU)", "Base: BH + AU + Location", "Price (Euro)")
ref_notes <- "*Both: Desktop + Mobile; BH: Browsing History, AU: Application Usage, LI: Location Information"
ref_notes_l <- "\\newline Both: \"Desktop + Mobile\";\\newline  BH: \"Browsing History\",\\newline  AU: \"Application Usage \",\\newline  LI: \"Location Information\""

results_models <- list()

results_models[["all"]] <- glm( model_formula, data = final_all_mlogit, family = binomial(link = "logit"))
results_models[["g1"]]  <- glm( model_formula, data = final_all_mlogit, family = binomial(link = "logit"), subset = group==1)
results_models[["g2"]]  <- glm( model_formula, data = final_all_mlogit, family = binomial(link = "logit"), subset = group==2)


#result_mlogit[["alt_mlogit"]] <- mlogit(formula = model_formula, data = final_all_mlogit)

# Results coefficients
stargazer(results_models[["all"]] , results_models[["g1"]], results_models[["g2"]] , type = "text", column.labels = c("All", "Control", "BTS")
          #covariate.labels = ref_labels
          #notes = ref_notes
          )




varImp(results_models[["g1"]], scale = FALSE, class = "glm")
varImp(results_models[["g2"]], scale = FALSE, class = "glm")

# tests -------------------------------------------------------------------


#logLik(results_models[["g1"]])
#logLik(results_models[["g2"]])

anova(results_models[["g1"]], test="Chisq")
anova(results_models[["g2"]], test="Chisq")

pR2(results_models[["g1"]])
pR2(results_models[["g2"]])


# ODDs ratios
results_models[["all_or"]] <- logitor( model_formula, data = final_all_mlogit)
results_models[["g1_or"]]  <- logitor( model_formula, data = final_all_mlogit_g1)
results_models[["g2_or"]] <- logitor( model_formula, data = final_all_mlogit_g2)
results_models[["table_or"]] <- cbind(setNames(as.data.frame(results_models[["all_or"]]$oddsratio[1:5]), "All"),
                                      setNames(as.data.frame(results_models[["g1_or"]]$oddsratio[1:5]), "Control"),
                                      setNames(as.data.frame(results_models[["g2_or"]]$oddsratio[1:5]), "BTS"))

rownames(results_models[["table_or"]]) <- ref_labels_ormx
stargazer(results_models[["table_or"]], type = "text", summary = FALSE, notes = ref_notes)


# Marginal effects
results_models[["all_mfx"]] <- logitmfx( model_formula, data = final_all_mlogit)
results_models[["g1_mfx"]]  <- logitmfx( model_formula, data = final_all_mlogit_g1)
results_models[["g2_mfx"]]  <- logitmfx( model_formula, data = final_all_mlogit_g2)
results_models[["table_mfx"]] <- cbind(setNames(as.data.frame(results_models[["all_mfx"]]$mfxest[1:5]), "All"),
                                       setNames(as.data.frame(results_models[["g1_mfx"]]$mfxest[1:5]), "Control"),
                                       setNames(as.data.frame(results_models[["g2_mfx"]]$mfxest[1:5]), "BTS"))

rownames(results_models[["table_mfx"]]) <- ref_labels_ormx
stargazer(results_models[["table_mfx"]], type = "text", summary = FALSE, notes = ref_notes)

# in sample predictions ---------------------------------------------------

fitresults <- list()

fitresults[["g1"]] <- predict(results_models[["g1"]], newdata=subset(final_all_mlogit ,subset = group==1, select=c( "price_N", "extent_1", "extent_2", "extent_3", "device_1", "device_2", "device_3" )), type='response')
fitresults[["g1_p"]] <- fitresults[["g1"]]
fitresults[["g1"]] <- ifelse(fitresults[["g1"]] > 0.5,1,0)
fitresults[["g1_result"]] <- mean(fitresults$g1 != final_all_mlogit[final_all_mlogit$group==1, "choice"])

print(paste('Accuracy',1-fitresults$g1_result))


fitresults[["g2"]]<- predict(results_models[["g2"]], newdata=subset(final_all_mlogit ,subset = group==2, select=c( "price_N", "extent_1", "extent_2", "extent_3", "device_1", "device_2", "device_3" )), type='response')
fitresults[["g2_p"]] <- fitresults[["g2"]]
fitresults[["g2"]] <- ifelse(fitresults[["g2"]] > 0.5,1,0)
fitresults[["g2_result"]] <- mean(fitresults[["g2"]] != final_all_mlogit[final_all_mlogit$group==2, "choice"])

print(paste('Accuracy',1-fitresults$g2_result))

round(prop.table(table(as.logical(fitresults$g1), final_all_mlogit[final_all_mlogit$group==1, "choice"])),2)
round(prop.table(table(as.logical(fitresults$g2), final_all_mlogit[final_all_mlogit$group==2, "choice"])),2)


# out of sample predictions -----------------------------------------------

final_os <- list()


final_os[["g1_obs"]] <- sample(1:nrow(final_all_mlogit_g1), nrow(final_all_mlogit_g1)*0.3)
final_os[["g2_obs"]] <- sample(1:nrow(final_all_mlogit_g2), nrow(final_all_mlogit_g2)*0.3)

final_os[["g1_train"]] <- final_all_mlogit_g1[-final_os$g1_obs,]
final_os[["g1_test"]]  <- final_all_mlogit_g1[ final_os$g1_obs,]
final_os[["g2_train"]] <- final_all_mlogit_g2[-final_os$g2_obs,]
final_os[["g2_test"]]  <- final_all_mlogit_g2[ final_os$g2_obs,]

results_models[["g1_os"]]  <- glm( model_formula, data = final_os$g1_train, family = binomial(link = "logit"), subset = group==1)
results_models[["g2_os"]]  <- glm( model_formula, data = final_os$g2_train, family = binomial(link = "logit"), subset = group==2)

fitresults[["g1_os"]] <- predict(results_models[["g1_os"]], newdata=final_os$g1_test, type='response')
fitresults[["g1_os_p"]] <- fitresults[["g1_os"]]
fitresults[["g1_os"]] <- ifelse(fitresults[["g1_os"]] > 0.5,1,0)
fitresults[["g1_os_result"]] <- mean(fitresults$g1_os != final_os$g1_test$choice)
fitresults[["g2_os"]] <- predict(results_models[["g2_os"]], newdata=final_os$g2_test, type='response')
fitresults[["g2_os_p"]] <- fitresults[["g2_os"]]
fitresults[["g2_os"]] <- ifelse(fitresults[["g2_os"]] > 0.5,1,0)
fitresults[["g2_os_result"]] <- mean(fitresults$g2_os != final_os$g2_test$choice)

print(paste('Accuracy',1-fitresults$g1_os_result))
print(paste('Accuracy',1-fitresults$g2_os_result))

round(prop.table(table(as.logical(fitresults$g1_os), final_os$g1_test$choice)),2)
round(prop.table(table(as.logical(fitresults$g2_os), final_os$g2_test$choice)),2)

xtable(round(prop.table(table(as.logical(fitresults$g1_os), final_os$g1_test$choice)),2))
xtable(round(prop.table(table(as.logical(fitresults$g2_os), final_os$g2_test$choice)),2))



# ROC Curve ---------------------------------------------------------------
#
##TO DO
#
#ROCRpred <- prediction(fitresults$g1_os_p, as.numeric(final_os$g1_test$choice))
#ROCRperf <- performance(ROCRpred, 'tpr','fpr')
#plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))
#
#
#ROCRpred <- prediction(fitresults$g2_os_p, as.numeric(final_os$g2_test$choice))
#ROCRperf <- performance(ROCRpred, 'tpr','fpr')
#plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))
#
#
#g <- roc(admit ~ prob, data = mydata)
#plot(g) 
#
#
#
#library(ggplot2)
#library(plotROC)
#
#set.seed(2529)
#D.ex <- rbinom(200, size = 1, prob = .5)
#M1 <- rnorm(200, mean = D.ex, sd = .65)
#M2 <- rnorm(200, mean = D.ex, sd = 1.5)
#
#test <- data.frame(D = D.ex, D.str = c("Healthy", "Ill")[D.ex + 1], 
#                   M1 = M1, M2 = M2, stringsAsFactors = FALSE)
#
#ggplot(melt_roc(test, "D", c("M1", "M2")), 
#       aes(d = D, m = M, color = name)) + 
#  geom_roc() + 
#  style_roc()
#
#ggplot(melt_roc(setNames(data.frame(cbind(as.numeric(final_os$g2_test$choice),fitresults$g2_os_p,fitresults$g2_os_p)), c("Choice","Predict1","Predict2")),d = "Choice",m = c("Predict1","Predict2")), 
#       aes(d = D, m = M, color = name)) + 
#  geom_roc() + 
#  style_roc()
#
# Plot time ---------------------------------------------------------------

bts_c <- list()

bts_c[["c_b0"]] <- results_models[["g1"]]$coefficients[1]
bts_c[["c_d1"]] <- results_models[["g1"]]$coefficients[2]
bts_c[["c_d2"]] <- results_models[["g1"]]$coefficients[3]
bts_c[["c_e1"]] <- results_models[["g1"]]$coefficients[4]
bts_c[["c_e2"]] <- results_models[["g1"]]$coefficients[5]
bts_c[["c_p"]]  <- results_models[["g1"]]$coefficients[6]

bts_c[["t_b0"]] <- results_models[["g2"]]$coefficients[1]
bts_c[["t_d1"]] <- results_models[["g2"]]$coefficients[2]
bts_c[["t_d2"]] <- results_models[["g2"]]$coefficients[3]
bts_c[["t_e1"]] <- results_models[["g2"]]$coefficients[4]
bts_c[["t_e2"]] <- results_models[["g2"]]$coefficients[5]
bts_c[["t_p"]]  <- results_models[["g2"]]$coefficients[6]

p_range <- seq(from=min(final_all_mlogit$price_N), to=max(final_all_mlogit$price_N), by=.01)

a_logits <- bts_c[["c_b0"]] +
            bts_c[["c_d1"]]*1 +
            bts_c[["c_d2"]]*0 +
            bts_c[["c_e1"]]*1 +
            bts_c[["c_e2"]]*0 +
            bts_c[["c_p"]] *p_range


b_logits <- bts_c[["t_b0"]] +
            bts_c[["t_d1"]]*1 +
            bts_c[["t_d2"]]*0 +
            bts_c[["t_e1"]]*1 +
            bts_c[["t_e2"]]*0 +
            bts_c[["t_p"]] *p_range
  
a_probs <- exp(a_logits)/(1 + exp(a_logits))
b_probs <- exp(b_logits)/(1 + exp(b_logits))



#plot(p_range, a_probs, 
#     ylim=c(0,1),
#     type="l", 
#     lwd=3, 
#     lty=2, 
#     col="gold", 
#     xlab="Price", ylab="P(choice)", main="Probability of a choice")
#
#
## Add the line for people who are in the b group
#lines(p_range, b_probs, 
#      type="l", 
#      lwd=3, 
#      lty=3, 
#      col="turquoise2")
#
## Add the line for people who are in the c group
#lines(p_range, c_probs, 
#      type="l", 
#      lwd=3, 
#      lty=4, 
#      col="orangered")
#
## add a horizontal line at p=.5
#abline(h=.5, lty=2)
  


bts_c[["plot.data_p1"]] <- data.frame(control=a_probs, bts=b_probs, price=p_range)
bts_c[["plot.data_p1"]] <- gather(bts_c[["plot.data_p1"]], key=group, value=prob, c("control","bts"))

#head(plot.data)


ggplot(bts_c[["plot.data_p1"]], aes(x=price, y=prob, color=group)) + 
  geom_line(lwd=2) + 
  labs(x="Price", y="P(choice)", title="Probability of product selection on different levels of price \nkeeping other factors constant on \ndevice: Desktop\nextent:  Internet History", col = "Series")+
  theme_gray()+
  scale_color_manual(labels = c("BTS", "Control"), values = c("#E69F00", "#56B4E9"))



c_logits <- bts_c[["c_b0"]] +
  bts_c[["c_d1"]]*0 +
  bts_c[["c_d2"]]*0 +
  bts_c[["c_e1"]]*0 +
  bts_c[["c_e2"]]*0 +
  bts_c[["c_p"]] *p_range


d_logits <- bts_c[["t_b0"]] +
  bts_c[["t_d1"]]*0 +
  bts_c[["t_d2"]]*0 +
  bts_c[["t_e1"]]*0 +
  bts_c[["t_e2"]]*0 +
  bts_c[["t_p"]] *p_range

c_probs <- exp(c_logits)/(1 + exp(c_logits))
d_probs <- exp(d_logits)/(1 + exp(d_logits))


bts_c[["plot.data_p2"]] <- data.frame(control=c_probs, bts=d_probs, price=p_range)
bts_c[["plot.data_p2"]] <- gather(bts_c[["plot.data_p2"]], key=group, value=prob, c("control","bts"))


ggplot(bts_c[["plot.data_p2"]], aes(x=price, y=prob, color=group)) + 
  geom_line(lwd=2) + 
  labs(x="Price", y="P(choice)", title="Probability of product selection on different levels of price \nkeeping other factors constant on \ndevice: Desktop + Mobile\nextent:  Internet History + App + Location", col = "Series")+
  theme_gray()+
  scale_color_manual(labels = c("BTS", "Control"), values = c("#E69F00", "#56B4E9"))








## Create a temporary data frame of hypothetical values
#temp.data <- data.frame(price_N =  seq(from=min(final_all_mlogit$price_N), to=max(final_all_mlogit$price_N), by=.01),
#                        device_1 = TRUE, device_2 = FALSE, device_3 = FALSE,
#                        extent_1 = TRUE, extent_2 = FALSE, extent_3 = FALSE)
#
## Predict the fitted values given the model and hypothetical data
#predicted.data <- as.data.frame(predict(results_models[["g2"]], newdata = temp.data, 
#                                        type="link", se=TRUE))
#
## Combine the hypothetical data and predicted values
#new.data <- cbind(temp.data, predicted.data)
#
## Calculate confidence intervals
#std <- qnorm(0.95 / 2 + 0.5)
#new.data$ymin <- results_models[["g2"]]$family$linkinv(new.data$fit - std * new.data$se)
#new.data$ymax <- results_models[["g2"]]$family$linkinv(new.data$fit + std * new.data$se)
#new.data$fit <- new.data$family$linkinv(new.data$fit)  # Rescale to 0-1



z <- with(final_all_mlogit, data.frame(device = tapply(device, index(final_all_mlogit)$alt, mean),
                                       extent = tapply(extent, index(final_all_mlogit)$alt, mean),
                                       price = tapply(price, index(final_all_mlogit)$alt, mean)))
                                       
glm(formula = choice ~ device + extent + price + device, data = final_all_mlogit)
glm(formula = choice ~ device + extent + price + device, data = final_all_mlogit_g1)
glm(formula = choice ~ device + extent + price + device, data = final_all_mlogit_g2)

effects(object = all, covariate = "device", data = z)
effects(object = all, covariate = "extent", data = z)
effects(object = all, covariate = "price",  data = z)

