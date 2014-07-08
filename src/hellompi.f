c  _________
c |   FILE  |_______________________________________
c |                 _________                       |
c | hellompi.f     |  AUTHOR |_______________________________________
c |   _________    |                                                 |
c |  | RESUME  |___| Rubén Muñoz <rmunoz@cbm.uam.es> CBM-SO (CSIC)   |
c |__|             |_________________________________________________|
c    | 19-07-2006:                                              |            
c    |                                                          | 
c    |   First example of MPICH tutorial.                       | 
c    |   One processor sends an array and other prints it.      | 
c    |   Compile with:                                          |    
c    |   -I/home/rmunoz/software/mpich2-1.0.3/src/include       |
c    |   -L/home/rmunoz/software/mpich2-1.0.3/lib               |
c    |__________________________________________________________|

        PROGRAM p2p
c           Run with two processes
            INCLUDE 'mpif.h'
            INTEGER err, rank, size
            real data(100), value(200)
            integer status(MPI_STATUS_SIZE)
            integer count
            CALL MPI_INIT(err)
            CALL MPI_COMM_RANK(MPI_COMM_WORLD,rank,err)
            CALL MPI_COMM_SIZE(MPI_COMM_WORLD,size,err)
            if (rank.eq.1) then
                data=3.0
                print *, "P:",rank," This is not always outputed"
                call MPI_SEND(data,100,MPI_REAL,0,55,MPI_COMM_WORLD,err)
            else
                call MPI_RECV(value,200,MPI_REAL,MPI_ANY_SOURCE,55,
     &          MPI_COMM_WORLD,status,err)
                print *, "P:",rank," got data from processor ",
     &          status(MPI_SOURCE)
                call MPI_GET_COUNT(status,MPI_REAL,count,err)
                print *, "P:",rank," got ",count," elements"
                print *, "P:",rank," value(5)=",value(5)
            end if
            CALL MPI_FINALIZE(err)
        END
