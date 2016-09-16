library('RUnit')

#source('util.R')
source("loader.R")
source("frontier.R")
test.suite <- defineTestSuite("portfolio",
                              dirs = file.path("test"),
                              testFileRegexp = '\\.R')
                              #testFileRegexp = '^\\d+\\.R')

test.result <- runTestSuite(test.suite)

printTextProtocol(test.result)

