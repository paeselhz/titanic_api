
#* @apiTitle API sobrevivência TITANIC

#* Echo back the input
#* @param pclass_ Classe do passageiro _numeric_
#* @param sex_ Sexo do passageiro _'male'/'female'_
#* @param age_ Idade do passageiro _numeric_
#* @param sibsp_ Número de irmãos/cônjuges a bordo _numeric_
#* @param parch_ Número de pais/filhos a bordo _numeric_
#* @get /predict_titanic
function(pclass_, sex_, age_, sibsp_, parch_) {
  
  model_load <-
    readRDS('data/model_titanic.rds')
  
  pclass_ = as.numeric(pclass_)
  age_    = as.numeric(age_)
  sibsp_  = as.numeric(sibsp_)
  parch_  = as.numeric(parch_)
  
  newdata_ <- data.frame(
    pclass = pclass_, 
    sex = sex_, 
    age = age_, 
    sibsp = sibsp_, 
    parch = parch_
  )
  
  pred <- predict(
    object = model_load, 
    newdata = newdata_,
    type = 'response'
  )
  
  list(response = paste0("A predição de sobrevivência no titanic é de : '", round(pred, 4), "'"))
}

