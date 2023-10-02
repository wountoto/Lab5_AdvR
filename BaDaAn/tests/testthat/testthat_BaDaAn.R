data('iris')
df <- data_dl()

test_that('Check that the data is correct', { 

  expected_true(c('kpi','municipality','period','values') %in% colnames(df))        
  
  
  })


test_hat('Checking inputs for muniLines',{
  expect_error(p <- muniLines('Götebörg', df))
  expect_error(p <- muniLines('Göteborg',iris))
})


test_hat('Checking inputs for muniCompare',{
  expect_error(p <- muniLines('Götebörg', df,'care_spend' ))
  expect_error(p <- muniLines('Göteborg',iris,'care_spend'))
  expect_error(p <- muniLines('Göteborg',df,'care_spenddrups'))
})


test_hat('Checking inputs for muniCorr',{
  expect_error(p <- muniCorr(iris))

})














