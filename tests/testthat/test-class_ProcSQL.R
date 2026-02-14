
# utils SQL -------------------------------------------------------------
test_that("extract_table_name", {
  t1 <- extract_table_name("table1 as t1")
  expect_equal(as.vector(t1), "table1")
  expect_equal(attributes(t1)$alias, "t1")

  t2 <- extract_table_name("table2 t2")
  expect_equal(as.vector(t2), "table2")
  expect_equal(attributes(t2)$alias, "t2")

  t3 <- extract_table_name("table3")
  expect_equal(as.vector(t3), "table3")
})

test_that("read_join", {
  expect_equal(mutate_join("t1", "t2", "t1.id = t2.fk_id"), "\"id\" = \"fk_id\"")
  expect_equal(mutate_join("t1", "t2", "t2.fk_id = t1.id"), "\"id\" = \"fk_id\"")
})



# Clause FROM ------------------------------------------------------------

test_that("sql_to_dplyr : clause from", {
  code_sql <-
    "PROC SQL;
    select *
     from table;
  QUIT;"
  test <- ProcSQL(code_sql)
  expect_equal(transpile(test), "table")
})


# Clause SELECT -----------------------------------------------------------


test_that("sql_dplyr_select : selection simple", {
  code_sql <-
    "PROC SQL;
    select var1, var2, var3
     from table;
  QUIT;"
  test <- ProcSQL(code_sql)
  expect_equal(transpile(test),
               "table %>%\n\tselect(var1, var2, var3)")
})

test_that("sql_dplyr_select : creation variable 1", {
  code_sql <-
    "PROC SQL;
    select old as new
     from table;
  QUIT;"
  test <- ProcSQL(code_sql)
  expect_equal(transpile(test),
               "table %>%\n\ttransmute(new = old)")
})

test_that("sql_dplyr_select : creation variable 2", {
  code_sql <-
    "PROC SQL;
    select *, old as new
     from table;
  QUIT;"
  test <- ProcSQL(code_sql)
  expect_equal(transpile(test),
               "table %>%\n\tmutate(new = old)")
})

test_that("sql_dplyr_select : creation variable 3", {
  code_sql <-
    "PROC SQL;
    select var1, old as new
     from table;
  QUIT;"
  test <- ProcSQL(code_sql)
  expect_equal(transpile(test),
               "table %>%\n\tmutate(new = old) %>%\n\tselect(var1, new)")
})

test_that("sql_dplyr_select : calcul ", {
  code_sql <-
    "PROC SQL;
    select avg(age)
     from table;
  QUIT;"
  test <- ProcSQL(code_sql)
  expect_equal(transpile(test),
               "table %>%\n\tsummarize(mean(age))")
})

test_that("sql_dplyr_select : calcul avec nouvele var", {
  code_sql <-
    "PROC SQL; select avg(age) as moy
     from table;
  QUIT;"
  test <- ProcSQL(code_sql)
  expect_equal(transpile(test),
               "table %>%\n\tsummarize(moy = mean(age))")
})


# Clause WHERE ------------------------------------------------------------

test_that("select all + from + where", {
  code_sql <-
    "proc sql;
    select * from iris where Species=\"setosa\";
  quit;"
  test <- ProcSQL(code_sql)
  expect_equal(transpile(test), "iris %>%\n\tfilter(Species == \"setosa\")")

})

# TODO
# test_that("select + from + where between + order by", {
#   code_sql = "select rB010, rB030, RB080, RB0808F
#   from table
#   where (RB080 not between 1890 and 2018 and RB080 not = .)
#   order by rb010, rB030;"
#   transpile(test)
# })

# Clause ORDER BY ---------------------------------------------------------

test_that("order by", {
  code_sql <- "PROC SQL;select * from tbl1 order by var1, var2 desc; quit;"
  test <- ProcSQL(code_sql)
  expect_equal(transpile(test), "tbl1 %>%\n\tarrange(var1, -var2)")

})


# Clause GROUP BY ---------------------------------------------------------
test_that("group by", {
  code_sql <- "PROC SQL; select var1, max(var2) as max from tbl1 group by var1; quit;"
  test <- ProcSQL(code_sql)
  expect_equal(transpile(test),
               "tbl1 %>%\n\tgroup_by(var1) %>%\n\tsummarize(max = max(var2))")

})

test_that("group by + having", {
  code_sql <- "PROC SQL; select var1, count(*) as nb from tbl1 group by var1 having nb>1; quit;"
  test <- ProcSQL(code_sql)
  expect_equal(transpile(test),
               "tbl1 %>%\n\tgroup_by(var1) %>%\n\tsummarize(nb = n()) %>%\n\tfilter(nb>1)")

})



# Create Table ------------------------------------------------------------

test_that("create table as", {
  code_sql = "PROC SQL;
  create table new_table as
  select * from old_table where var1 = 1;
  QUIT;"
  test <- ProcSQL(code_sql)
  expect_equal(transpile(test),
               "new_table <- old_table %>%\n\tfilter(var1 == 1)")

})

test_that("create new table and new var", {
  code_sql = "PROC SQL;
  CREATE TABLE LIB.MY_IRIS AS
  SELECT *, SepalLength*SepalWidth as result
  FROM SASHELP.IRIS;
  QUIT;
  "
  test <- ProcSQL(code_sql)
  expect_equal(transpile(test),
               "LIB.MY_IRIS <- SASHELP.IRIS %>%\n\tmutate(result = SepalLength*SepalWidth)")

})




# Jointures ---------------------------------------------------------------
#source : https://sql.sh/cours/jointures/inner-join
# https://www.w3schools.com/sql/sql_join_left.asp

test_that("Jointure simple avec ON", {
  code_sql = "PROC SQL;
  SELECT *
FROM table1
INNER JOIN table2 ON table1.id = table2.fk_id;
  QUIT;"
test <- ProcSQL(code_sql)
expect_equal(test@source, code_sql)
  expect_equal(
    transpile(test),
    "table1 %>%\n\tinner_join(table2, by = c(\"id\" = \"fk_id\"))"
  )
})

test_that("Double jointure simple avec ON", {
  code_sql = "PROC SQL;
  SELECT *
FROM table1
INNER JOIN table2 ON table1.id = table2.fk_id
INNER JOIN table3 ON table3.nom = table1.nom;
  QUIT;"
  test <- ProcSQL(code_sql)
  expect_equal(
    transpile(test),
    "table1 %>%\n\tinner_join(table2, by = c(\"id\" = \"fk_id\")) %>%\n\tinner_join(table3, by = c(\"nom\" = \"nom\"))"
  )
})

test_that("Jointure simple avec WHERE", {
  code_sql = "PROC SQL;
  SELECT *
FROM table1
INNER JOIN table2
WHERE table1.id = table2.fk_id;
  QUIT;"
  test <- ProcSQL(code_sql)
  expect_equal(
    transpile(test),
    "table1 %>%\n\tinner_join(table2, by = c(\"id\" = \"fk_id\"))"
  )
})

test_that("Jointure simple avec ON et selection de variables", {
  code_sql = "PROC SQL;
  SELECT id, prenom, nom, date_achat, num_facture, prix_total
FROM utilisateur
INNER JOIN commande ON utilisateur.id = commande.utilisateur_id;
  QUIT;"
  test <- ProcSQL(code_sql)
  expect_equal(
    transpile(test),
    "utilisateur %>%\n\tinner_join(commande, by = c(\"id\" = \"utilisateur_id\")) %>%\n\tselect(id, prenom, nom, date_achat, num_facture, prix_total)"
  )
})

test_that("Jointure simple avec ON, selection de variables et filtre", {
  code_sql = "PROC SQL;
  SELECT id, prenom, nom, utilisateur_id
FROM utilisateur
LEFT JOIN commande ON utilisateur.id = commande.utilisateur_id
WHERE utilisateur_id IS NOT NULL;
  QUIT;"
  test <- ProcSQL(code_sql)
  expect_equal(transpile(test),
               "utilisateur %>%\n\tleft_join(commande, by = c(\"id\" = \"utilisateur_id\")) %>%\n\tselect(id, prenom, nom, utilisateur_id) %>%\n\tfilter(!is.na(utilisateur_id))")
})

test_that("Jointure multiple", {
  code_sql = "
  PROC SQL;
  SELECT Orders.OrderID, Customers.CustomerName, Shippers.ShipperName
FROM Orders
INNER JOIN Customers ON Orders.CustomerID = Customers.CustomerID
INNER JOIN Shippers ON Orders.ShipperID = Shippers.ShipperID;
  QUIT;"
  test <- ProcSQL(code_sql)
  expect_equal(transpile(test),
               "Orders %>%\n\tinner_join(Customers, by = c(\"CustomerID\" = \"CustomerID\")) %>%\n\tinner_join(Shippers, by = c(\"ShipperID\" = \"ShipperID\")) %>%\n\tselect(OrderID, CustomerName, ShipperName)"
  )
})

# TODO: Real cases tests -----------
#
# test_that("Jointure impropre", {
#   code_sql = "SELECT Orders.OrderID, Orders.OrderDate, Customers.CustomerName
# FROM Customers
# INNER JOIN Orders ON Customers.CustomerID=Orders.CustomerID
# WHERE Customers.CustomerName='Around the Horn'"
#   transpile(test)
#
#   "SELECT Orders.OrderID, Orders.OrderDate, Customers.CustomerName
# FROM Customers, Orders
# WHERE Customers.CustomerName='Around the Horn' AND Customers.CustomerID=Orders.CustomerID;"
#
# })
#
# test_that("query Sylvain 2", {
#   code_sql = "CREATE TABLE LIB.JOINTURE AS
#   SELECT DISTINCT a.CUSTUMER_ID, a.DATE, sum(b.price) as total_price
#   FROM LIB2.TABLE1 as a
#   LEFT JOIN LIB3.TABLE2 b
#   ON a.CUSTUMER_ID = b.CUSTUMER_ID and a.var1 != b.var2
#   WHERE a.DATE < mdy(1, 1, 2020)
#   GROUP BY a.CUSTUMER_ID, a.DATE
#   HAVING calculated total_price>0"
#
#   "LIB2.TABLE1 %>%
#   left_join(LIB3.TABLE2, by = c(\"CUSTUMER_ID\"=\"CUSTUMER_ID\")) %>%
#   filter(var1 != var2 & DATE < mdy(1, 1, 2020)) %>%
#   group_by(CUSTUMER_ID, DATE) %>%
#   summarize(total_price = sum(price)) %>%
#   filter(total_price > 0)"
# })


