(use 'lispe_sqlite)

(setq mysql (sqlite))


(sqlite_open mysql "test.db")
(sqlite_create mysql "people" "name TEXT PRIMARY KEY" "age INTEGER")
(sqlite_close mysql)
 




