#! /bin/tcsh
#  _________
# |   FILE  |_______________________________________
# |                 _________                       |
# | master.tcsh    |  AUTHOR |_______________________________________
# |   _________    |                                                 |
# |  | RESUME  |___| Rubén Muñoz <rmunoz@cbm.uam.es> CBM-SO (CSIC)  
# |__|             |_________________________________________________|
#    | 19-07-2006:                                              |            
#    |                                                          | 
#    |   Send to MN with:                                       |
#    |            llsubmit master.tcsh                          |
#    |__________________________________________________________|
#
# @ job_type = parallel
# @ class = class_a 
# @ group = uam51
# @ initialdir = /home/uam51/uam51409/labo/bin
# @ output = master.$(jobid).out
# @ error = master.$(jobid).err
# @ restart = no
# @ requirements = (Feature == "myrinet")
# @ blocking = unlimited
# @ total_tasks = 1
# @ node_usage = not_shared
# @ wall_clock_limit = 01:00:00
# @ queue
setenv MP_EUILIB gm
setenv OBJECT_MODE 64
setenv MP_RSH ssh

#set NPROCS=`cat $LL_MACHINE_LIST |wc -l`
set NPROCS=1

./master < ../data/master.in
#time mpirun -s -np ${NPROCS} -machinefile $LL_MACHINE_LIST ./master < ../data/master.in
#time mpirun -s -np 1 -machinefile machinelocal ./master < ../data/master.in

