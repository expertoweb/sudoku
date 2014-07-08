#! /bin/tcsh
#  _________
# |   FILE  |_______________________________________
# |                 _________                       |
# | sdkpar.tcsh    |  AUTHOR |_______________________________________
# |   _________    |                                                 |
# |  | RESUME  |___| Rubén Muñoz <rmunoz@cbm.uam.es> CBM-SO (CSIC)  
# |__|             |_________________________________________________|
#    | 19-07-2006:                                              |            
#    |                                                          | 
#    |   Send to MN with:                                       |
#    |            llsubmit sdkpar.tcsh                          |
#    |__________________________________________________________|
#
# @ job_type = parallel
# @ class = debug 
# @ group = uam51
# @ initialdir = /home/uam51/uam51409/labo/bin
# @ output = sdkpar.$(jobid).out
# @ error = sdkpar.$(jobid).err
# @ restart = no
# @ requirements = (Feature == "myrinet")
# @ blocking = unlimited
# @ total_tasks = 12
# @ wall_clock_limit = 00:10:00
# @ queue
setenv MP_EUILIB gm
setenv OBJECT_MODE 64
setenv MP_RSH ssh

#set NPROCS=`cat $LL_MACHINE_LIST |wc -l`
set NPROCS=4


#time mpirun -s -np ${NPROCS} -machinefile $LL_MACHINE_LIST ./sdkpar < ../data/difficult.in
time mpirun -s -np ${NPROCS} -machinefile machinelocal ./sdkpar < $1

