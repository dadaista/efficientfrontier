library('RUnit')

source('util.R')

test.suite <- defineTestSuite("portfolio",
                              dirs = file.path("test"),
                              testFileRegexp = '^\\d+\\.R')

test.result <- runTestSuite(test.suite)

printTextProtocol(test.result)

