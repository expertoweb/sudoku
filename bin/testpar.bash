#! /bin/bash
#  _________
# |   FILE  |_______________________________________
# |                 _________                       |
# | testpar.bash   |  AUTHOR |_______________________________________
# |   _________    |                                                 |
# |  | RESUME  |___| Rubén Muñoz <rmunoz@cbm.uam.es> CBM-SO (CSIC)  
# |__|             |_________________________________________________|
#    | 04-08-2006:                                              |            
#    |                                                          | 
#    |   Send to MN with:                                       |
#    |            llsubmit testpar.tcsh                         |
#    |__________________________________________________________|
#
# @ job_type = parallel
# @ class = debug 
# @ group = uam51
# @ initialdir = /home/uam51/uam51409/labo/bin
# @ output = testpar.$(jobid).out
# @ error = testpar.$(jobid).err
# @ restart = no
# @ requirements = (Feature == "myrinet")
# @ node = 4
# @ total_tasks = 8
# @ wall_clock_limit = 00:10:00
# @ queue
export MP_EUILIB=gm
export OBJECT_MODE=64
export MP_RSH=ssh

NPROCS=`cat $LL_MACHINE_LIST |wc -l`

time mpirun -s -np ${NPROCS} -machinefile $LL_MACHINE_LIST ./testpar < ../data/easy.in > output.out
#time mpirun -s -np 8 -machinefile machinelocal ./testpar < ../data/easy.in

