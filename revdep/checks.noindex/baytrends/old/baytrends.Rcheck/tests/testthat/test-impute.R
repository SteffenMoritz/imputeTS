# Test impute function
#
test_that("impute, lower", {
  # Example
  x  <- dataCensored[1:20,"tdp"]
  calc_impute <- impute(x,'lower')
  
  qc_impute <- c(0.0360, 0.0170, 0.0080, 0.0255, 0.0000, 0.0040, 0.0320, 0.0640
                 , 0.0370, 0.0140, 0.0160, 0.0260, 0.0270, 0.0400, 0.0180, 0.0190
                 , 0.0150, 0.0470, 0.0430, 0.0145)
  
  # Test
  expect_equal(qc_impute, calc_impute)
})
#
test_that("impute, mid", {
  x  <- dataCensored[1:20,"tdp"]
  calc_impute <- impute(x,'mid')
  
  qc_impute <- c(0.03600, 0.01700, 0.00800, 0.02550, 0.00250, 0.00525, 0.03200
                 , 0.06400, 0.03700, 0.01400, 0.01600, 0.02600,0.02700, 0.04000
                 , 0.01800, 0.01900, 0.01500, 0.04700, 0.04300, 0.01450)

  # Test
  expect_equal(qc_impute, calc_impute)
})
#
test_that("impute, upper", {
  x  <- dataCensored[1:20,"tdp"]
  calc_impute <- impute(x,'upper')
  
  qc_impute <- c(0.0360, 0.0170, 0.0080, 0.0255, 0.0050, 0.0065, 0.0320, 0.0640
                 , 0.0370, 0.0140, 0.0160, 0.0260, 0.0270, 0.0400, 0.0180, 0.0190
                 , 0.0150, 0.0470, 0.0430, 0.0145)
  
  # Test
  expect_equal(qc_impute, calc_impute)
})
#
test_that("impute, norm", {
  x  <- dataCensored[1:20,"tdp"]
  calc_impute <- impute(x,'norm')
  
  qc_impute <- c(0.036000000, 0.017000000, 0.008000000, 0.025500000, 0.002705021
                 , 0.005295357, 0.032000000, 0.064000000, 0.037000000, 0.014000000
                 , 0.016000000, 0.026000000, 0.027000000, 0.040000000, 0.018000000
                 , 0.019000000, 0.015000000, 0.047000000, 0.043000000, 0.014500000)
  
  # Test
  expect_equal(qc_impute, calc_impute)
})
#
test_that("impute, lnorm", {
  x  <- dataCensored[1:20,"tdp"]
  calc_impute <- impute(x,'lnorm')
  
  qc_impute <- c(0.036000000, 0.017000000, 0.008000000, 0.025500000, 0.003934377
                 , 0.005418384, 0.032000000, 0.064000000,0.037000000, 0.014000000
                 , 0.016000000, 0.026000000, 0.027000000, 0.040000000, 0.018000000
                 , 0.019000000, 0.015000000, 0.047000000, 0.043000000, 0.014500000)
  
  # Test
  expect_equal(qc_impute, calc_impute)
})