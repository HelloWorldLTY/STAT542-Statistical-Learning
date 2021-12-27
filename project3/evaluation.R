library(pROC)
source(mymain.R)
#5. Evaluation
test.y = read.table(testypath, header = TRUE)
pred = read.table("mysubmission.txt", header = TRUE)
pred = merge(pred, test.y, by="id")
roc_obj = roc(pred$sentiment, pred$prob)
tmp = pROC::auc(roc_obj)
print(tmp)