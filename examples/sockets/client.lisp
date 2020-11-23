
# we create a connection

(setq client (socket_connect "mulakus-MacBook-Pro.local" 2654))

# We then write something on the socket
(socket_write client "Ceci est un test")





