
library(Rserve)
library(RSclient)

Rserve(port = 6311)
# Starting Rserve...
# "C:\PROGRA~1\R\R-33~1.1\library\Rserve\libs\x64\Rserve.exe" --RS-port 6311 

rsc <- RSconnect(port = 6311)

system('tasklist /FI "IMAGENAME eq Rserve.exe"')

RSshutdown(rsc)
