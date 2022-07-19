test_that("wiki download version change", {
  expect_s3_class(readLipd("http://wiki.linked.earth/wiki/index.php/Special:WTLiPD?op=export&lipdid=BJ8-03-70GGC.Linsley.2010"),"lipd")
})
