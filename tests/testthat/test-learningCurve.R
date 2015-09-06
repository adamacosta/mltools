context('test learning curve')

test_that('test learningCurve generates data', {
    data(iris)
    res <- learningCurve(iris, 'classif.rpart')
    expect_is(res, 'data.frame')
})
