; Creation of a server
(setq server (socket_create 2654 10))

; (socket_settimeout server 1)

(println (socket_gethostname server))

(println 'Waiting 'for 'a 'connection)
; We wait for a connection
(setq idclient (socket_wait server))


;# We then read on this idclient

(println 'Reading (socket_read server idclient))











