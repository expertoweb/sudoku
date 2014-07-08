#! /bin/tcsh
#  _________
# |   FILE  |_______________________________________
# |                 _________                       |
# | hellompi.tcsh  |  AUTHOR |_______________________________________
# |   _________    |                                                 |
# |  | RESUME  |___| Rubén Muñoz <rmunoz@cbm.uam.es> CBM-SO (CSIC)  
# |__|             |_________________________________________________|
#    | 19-07-2006:                                              |            
#    |                                                          | 
#    |   First example of MPICH tutorial submittion script      | 
#    |   Send to MN with:                                       |
#    |            llsubmit hellompi.tcsh                        |
#    |__________________________________________________________|
#
# @ job_type = parallel
# @ class = debug 
# @ group = uam51
# @ initialdir = /home/uam51/uam51409/labo/bin
# @ output = hellompi.$(jobid).out
# @ error = hellompi.$(jobid).err
# @ restart = no
# @ requirements = (Feature == "myrinet")
# @ node = 1
# @ total_tasks = 2
# @ wall_clock_limit = 00:10:00
# @ queue
setenv MP_EUILIB gm
setenv OBJECT_MODE 64
setenv MP_RSH ssh

set NPROCS = `wc -l $LL_MACHINE_LIST`

#mpirun -s -np ${NPROCS} -machinefile $MLIST ./hellompi
mpirun -s -np ${NPROCS} -machinefile $LL_MACHINE_LIST ./prueba.tcsh

