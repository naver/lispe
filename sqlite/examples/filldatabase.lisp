(use 'lispe_sqlite)

(setq mysql (sqlite))


(sqlite_open mysql "test.db")

(sqlite_insert mysql "people" "name" "Pierre" "age" 40)
(sqlite_insert mysql "people" "name" "Jean-Pierre" "age" 43)
(sqlite_insert mysql "people" "name" "Marie" "age" 20)
(sqlite_insert mysql "people" "name" "Jérôme" "age" 34)
(sqlite_insert mysql "people" "name" "Alphonse" "age" 72)
(sqlite_insert mysql "people" "name" "Anne" "age" 28)
(sqlite_insert mysql "people" "name" "Roseline" "age" 61)
(sqlite_insert mysql "people" "name" "Thierry" "age" 50)
(sqlite_insert mysql "people" "name" "Hugo" "age" 17)
(sqlite_insert mysql "people" "name" "Amandine" "age" 38)
(sqlite_insert mysql "people" "name" "Christine" "age" 40)


(sqlite_close mysql)







