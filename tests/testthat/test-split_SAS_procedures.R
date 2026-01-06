test_that("split diverse procedures", {
  expect_equal(
    split_SAS_procedures(
      text =
        "proc contents data = table;
    run;
    proc means data= table;
    var age;
    run;
    proc freq data=table;
    table sexe;
    run;"
    ),
    c(
      "proc contents data = table; run;",
      "proc means data= table; var age; run;",
      "proc freq data=table; table sexe; run;"
    )
  )
})


test_that("split similar procedures", {
  expect_equal(
    split_SAS_procedures(
      text =     "proc contents data = table;
  run;
  proc contents data= table2;
  run;
  proc contents data=table3;
  run;"
    ),
    c(
      "proc contents data = table; run;",
      "proc contents data= table2; run;",
      "proc contents data=table3; run;"
    )
  )
})

test_that("split everything", {
  expect_equal(
    split_SAS_procedures(
      text =
        "data test;
  set table;
  run;
  /* un commentaire ici*/
  proc contents data = table;
  run;
  proc means data= table;
  var age; /*et ici*/
  run;
  /*un commentaire là*/
  proc freq data=table;
  table sexe;
  run;"
    ),
    c(
      "data test; set table; run;",
      "/* un commentaire ici*/",
      "proc contents data = table; run;",
      "proc means data= table; var age; /*et ici*/ run;",
      "/*un commentaire là*/",
      "proc freq data=table; table sexe; run;"
    )
  )
})

test_that("complicated graph", {
  expect_equal(
    split_SAS_procedures(
      text =
        "data totals;
length dept $ 7 site $ 8;
input dept site quarter sales;
datalines;
Parts Sydney 1 7043.97
Parts Atlanta 1 8225.26
Parts Paris 1 5543.97
Tools Sydney 4 1775.74
Tools Atlanta 4 3424.19
Tools Paris 4 6914.25
run;
  title 'Total Sales';
footnote j=r 'GCHBKSUM ';

proc gchart data=totals;
format sales dollar8.;
block site / sumvar=sales;
run;
quit;
"
    ),
    c(
      "data totals; length dept $ 7 site $ 8; input dept site quarter sales; datalines; parts sydney 1 7043.97 parts atlanta 1 8225.26 parts paris 1 5543.97 tools sydney 4 1775.74 tools atlanta 4 3424.19 tools paris 4 6914.25 run;",
      "title 'total sales'; footnote j=r 'gchbksum ';",
      "proc gchart data=totals; format sales dollar8.; block site / sumvar=sales; run;"
    )
  )
})
