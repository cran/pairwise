### R code from vignette source 'DEU_PISA2012_description.Rnw'

###################################################
### code chunk number 1: DEU_PISA2012_description.Rnw:26-27
###################################################
library(pairwise)


###################################################
### code chunk number 2: DEU_PISA2012_description.Rnw:32-33
###################################################
data(DEU_PISA2012)


###################################################
### code chunk number 3: DEU_PISA2012_description.Rnw:42-43
###################################################
names(DEU_PISA2012)


###################################################
### code chunk number 4: DEU_PISA2012_description.Rnw:48-49
###################################################
lapply(DEU_PISA2012,class)


###################################################
### code chunk number 5: DEU_PISA2012_description.Rnw:55-56
###################################################
names(DEU_PISA2012$id)


###################################################
### code chunk number 6: DEU_PISA2012_description.Rnw:63-64
###################################################
table(DEU_PISA2012$id$BOOKID)


###################################################
### code chunk number 7: DEU_PISA2012_description.Rnw:68-69
###################################################
table(DEU_PISA2012$id$QuestID)


###################################################
### code chunk number 8: DEU_PISA2012_description.Rnw:76-77
###################################################
names(DEU_PISA2012$covariate)


###################################################
### code chunk number 9: DEU_PISA2012_description.Rnw:92-93
###################################################
names(DEU_PISA2012$cog)


###################################################
### code chunk number 10: DEU_PISA2012_description.Rnw:99-100
###################################################
names(DEU_PISA2012$cog$pv)


###################################################
### code chunk number 11: DEU_PISA2012_description.Rnw:105-106
###################################################
names(DEU_PISA2012$cog$pv$MATH)


###################################################
### code chunk number 12: DEU_PISA2012_description.Rnw:109-110
###################################################
names(DEU_PISA2012$cog$pv$READ)


###################################################
### code chunk number 13: DEU_PISA2012_description.Rnw:113-114
###################################################
names(DEU_PISA2012$cog$pv$SCIE)


###################################################
### code chunk number 14: DEU_PISA2012_description.Rnw:118-119
###################################################
length(DEU_PISA2012$cog$pv$MATH$PV1MATH)


###################################################
### code chunk number 15: DEU_PISA2012_description.Rnw:124-125
###################################################
names(DEU_PISA2012$cog$dat)


###################################################
### code chunk number 16: DEU_PISA2012_description.Rnw:130-131
###################################################
rapply(DEU_PISA2012$cog$dat,names,classes = "list",how="list")


###################################################
### code chunk number 17: DEU_PISA2012_description.Rnw:138-139
###################################################
names(DEU_PISA2012$cog$dat$MATH)


###################################################
### code chunk number 18: DEU_PISA2012_description.Rnw:144-147
###################################################
dim(DEU_PISA2012$cog$dat$MATH$resp)
dim(DEU_PISA2012$cog$dat$MATH$inc7)
dim(DEU_PISA2012$cog$dat$MATH$inc8)


###################################################
### code chunk number 19: DEU_PISA2012_description.Rnw:162-163
###################################################
names(DEU_PISA2012$ncog)


###################################################
### code chunk number 20: DEU_PISA2012_description.Rnw:169-170
###################################################
names(DEU_PISA2012$ncog$wle)


###################################################
### code chunk number 21: DEU_PISA2012_description.Rnw:176-177
###################################################
names(DEU_PISA2012$ncog$dat)


###################################################
### code chunk number 22: DEU_PISA2012_description.Rnw:184-185
###################################################
names(DEU_PISA2012$ncog$dat$CLSMAN)


###################################################
### code chunk number 23: DEU_PISA2012_description.Rnw:190-191
###################################################
lapply(DEU_PISA2012$ncog$dat$CLSMAN,dim)


###################################################
### code chunk number 24: DEU_PISA2012_description.Rnw:196-197
###################################################
colnames(DEU_PISA2012$ncog$dat$CLSMAN$resp)


###################################################
### code chunk number 25: DEU_PISA2012_description.Rnw:208-209
###################################################
names(DEU_PISA2012$weights)


###################################################
### code chunk number 26: DEU_PISA2012_description.Rnw:218-219
###################################################
(sum(DEU_PISA2012$cog$dat$MATH$inc7))/(prod(dim(DEU_PISA2012$cog$dat$MATH$inc7)))*100


###################################################
### code chunk number 27: DEU_PISA2012_description.Rnw:224-225
###################################################
(sum(DEU_PISA2012$cog$dat$READ$inc7))/(prod(dim(DEU_PISA2012$cog$dat$READ$inc7)))*100


###################################################
### code chunk number 28: DEU_PISA2012_description.Rnw:230-231
###################################################
(sum(DEU_PISA2012$cog$dat$SCIE$inc7))/(prod(dim(DEU_PISA2012$cog$dat$SCIE$inc7)))*100


