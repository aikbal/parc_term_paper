#devtools::install_github("dswatson/cpi")

library(ggplot2)
library(ggsci)
library(mlr)
library(data.table)
library(cpi)
#install.packages ("e1071", dependencies = TRUE)
library(e1071)


seed <- 100

# Convert 2-level factor to binary
 mytask <- makeClassifTask(data = iris, target = "Species")


cpi_rf_log_iris<-cpi(task = mytask, 
                   learner = makeLearner("classif.ctree"),
                   resampling = makeResampleDesc("CV", iters = 5), 
                   measure = "mmce", test = "t")


cpi_svm_log_iris<-cpi(task = mytask, 
                    learner = makeLearner("classif.svm",kernel = "radial"),
                    resampling = makeResampleDesc("CV", iters = 5), 
                    measure = "mmce", test = "t")

# Combine for plotting
res <- rbind(data.table(Learner = "Decesion Tree", Log = "Multiplicative CPI", cpi_rf_log_iris[, c("Variable", "CPI", "SE", "p.value")]),
             data.table(Learner = "Support vector machine", Log = "Multiplicative CPI", cpi_svm_log_iris[, c("Variable", "CPI", "SE", "p.value")]))#, 
res[, p.adj := p.adjust(p.value, "holm")]
res[, signif := ifelse(p.adj <= .05, 1, 0)]
Variable<- cpi_svm_log_iris$Variable
levels <- res[Learner == "Support vector machine", as.character(Variable)[order(CPI)]]


labels <- levels
labels[labels == "chas.1"] <- "chas"
res[, Variable := factor(Variable, levels = levels, labels = labels)]

# Plot
ggplot(res, aes(x = Variable, fill = Learner, y = CPI, alpha = signif)) + 
  geom_bar(stat = "identity", position = position_dodge(-.9)) + 
  geom_errorbar(aes(ymin = CPI - SE, ymax = CPI + SE), position = position_dodge(-.9)) +
  #facet_wrap(~ Log, scales = "free") + 
  scale_fill_npg() +
  scale_alpha_continuous(range = c(.4, 1)) + 
  coord_flip() + 
  theme_bw() + 
  theme(legend.position = "top") + 
  guides(alpha = "none") + 
  ylab("CPI")
ggsave("../img/iris.png", width = 6, height = 8)



