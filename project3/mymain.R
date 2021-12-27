library(pROC)
library(glmnet)
# Read in the vocab 
myvocab = scan(file= "myvocab.txt", what = character())  
set.seed(3402)

  #1. Prepare training dataset
  trainpath = "train.tsv"
  testpath = "test.tsv"
  testypath = "test_y.tsv"
  train = read.table(trainpath,
                     stringsAsFactors = FALSE,
                     header = TRUE)
  
  #2. Prepare training vocabulary
  train$review <- gsub('<.*?>', ' ', train$review)
  it_train = itoken(train$review,
                    preprocessor = tolower, 
                    tokenizer = word_tokenizer)
  vectorizer = vocab_vectorizer(create_vocabulary(myvocab, 
                                                  ngram = c(1L, 2L)))
  dtm_train = create_dtm(it_train, vectorizer)
  
  #3. Train the Ridge regression model
  mylogit.cv = cv.glmnet(x = dtm_train, 
                         y = train$sentiment, 
                         alpha = 0,
                         family='binomial', 
                         type.measure = "auc")
  mylogit.fit = glmnet(x = dtm_train, 
                       y = train$sentiment, 
                       alpha = 0,
                       lambda = mylogit.cv$lambda.min, 
                       family='binomial')
  
  #4. Load the testing dataset
  test = read.table(testpath,
                    stringsAsFactors = FALSE,
                    header = TRUE)
  test$review <- gsub('<.*?>', ' ', test$review)
  it_test = itoken(test$review,
                   preprocessor = tolower, 
                   tokenizer = word_tokenizer)
  dtm_test = create_dtm(it_test, vectorizer)
  mypred = predict(mylogit.fit, dtm_test, type = "response")
  output = data.frame(id = test$id, prob = as.vector(mypred))
  write.table(output, file = "mysubmission.txt", 
              row.names = FALSE, sep='\t')

