(use 'lispe_sqlite)

(setq mysql (sqlite))


(sqlite_open mysql "test.db")

; We use sqlite_process to enable a loop on the database

(sqlite_process mysql "select * from people where age >= 30")

(loop x mysql (println x))

; This is another way to handle a SQLite command
(setq v (sqlite_run mysql "select * from people where age < 40"))

(println v)

(sqlite_close mysql)







