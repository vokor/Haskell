# SQLite
main'

0) команда выхода: exit

1)
CREATE TABLE table_name (Int a,String b,Int c)

2)
INSERT INTO table_name (column1,column2,column3) VALUES ('data1','data2','data3')
INSERT INTO table_name VALUES (0, 'sava', 6)

3)
UPDATE table_name SET (Units=148, Money=23680) WHERE ID = 4
UPDATE table_name SET (Units=148, Money=23680) WHERE ID <> 4   // не равен
UPDATE table_name SET (Units=148, Money=23680)                 // заполнить все столбцы

4)
REMOVE FROM table_name WHERE Age > 18
REMOVE FROM table_name                      // удалить все

5)
SELECT * FROM table_name                                           // * для всех
SELECT (DEPT, NAME, JOB) FROM table_name                           // вывести все столбцы по названию: DEPT, NAME, JOB
SELECT (FirstName,LastName) FROM table_name WHERE Age > 18

6)
SELECT (a, tableX.id, tableY.id) FROM tableX JOIN tableY ON table1.id = table2.somekey
SELECT * FROM A JOIN (B JOIN C ON B.fkC = C.pk) ON A.optionalfkB = B.pk
SELECT * FROM A JOIN (B JOIN (B JOIN C ON B.fkC = C.pk) ON B.fkC = C.pk) ON A.optionalfkB = B.pk

7)
LOAD * IN FILE 'data.txt'
LOAD table_name IN FILE 'data.txt'

8)
UPLOAD FILE 'in.txt'
