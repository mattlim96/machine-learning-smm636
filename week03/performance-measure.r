####################################################
#### This function calculates two performance measures:
#### specificity and sensitivity
#### Input: pred: predicted labels (factor)
####        truth: true labels (factor)
####        pos: positive level
####        neg: negative level
#### Output: a list containing sensitivity and specificity
####################################################
performance.measure<-function(pred,truth,pos,neg){
  #### get confusion table
  confusion=table(pred,truth)
  #### get tn, tp, fn, fp
  tn=confusion[neg,neg]
  tp=confusion[pos,pos]
  fn=confusion[neg,pos]
  fp=confusion[pos,neg]
  #### calculate sensitivity
  sens=tp/(tp+fn)
  #### calculate specificity
  spec=tn/(tn+fp)
  #### calculate accuracy
  acc=(tn+tp)/length(pred)
  #### put the two values in a list
  measures=list(sensitivity=sens,specificity=spec,accuracy=acc)
  #### return the list as function output
  return(measures)
} 
