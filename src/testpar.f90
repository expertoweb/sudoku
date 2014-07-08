!  _________
! |   FILE  |_______________________________________
! |                 _________                       |
! | testpar.f90    |  AUTHOR |_______________________________________
! |   _________    |                                                 |
! |  | RESUME  |___| Rubén Muñoz <rmunoz@cbm.uam.es> CBM-SO (CSIC)   |
! |__|             |_________________________________________________|
!    | 04-08-2006:                                              |            
!    | - MPI test.                                              | 
!    |__________________________________________________________|

        PROGRAM TESTPAR

        IMPLICIT NONE
       
!============MPI CODE============== 
        INCLUDE 'mpif.h'
!================================== 
    
        INTEGER, DIMENSION(9,9) :: matrix
    
!============MPI CODE============== 
        INTEGER :: number, err, rank, size, nfree, i
        INTEGER, DIMENSION(9) :: valid
        INTEGER, DIMENSION(81) :: data
        INTEGER, DIMENSION(MPI_STATUS_SIZE) :: status

        CALL MPI_INIT(err)
        CALL MPI_COMM_RANK(MPI_COMM_WORLD,rank,err)
        CALL MPI_COMM_SIZE(MPI_COMM_WORLD,size,err)
        
        IF (rank.EQ.0) THEN
            ! Read matrix from stdin
            READ *, matrix
            matrix=TRANSPOSE(matrix)
            PRINT *, "P:", rank, "sending matrix..." ! Sometimes this is not printed
!           CALL FLUSH_()
            CALL PRINTMATRIX(matrix)
            data=RESHAPE(matrix, SHAPE=(/81/))
        ELSE
            PRINT *, "P:", rank, "waiting for matrix..." ! Sometimes this is not printed
!           CALL FLUSH_()
        END IF
        
        DO i=1,2
! 1 10 19 28 37 46 55 64 73
! 2 11 20 29
! 3 12 21 30 
! 4 13 22 31 
! 5 14 23 .. ..
! 6 15 24 ..    ..
! 7 16 25 ..       ..
! 8 17 26 ..          ..
! 9 18 27 36 45 54 63 72 81

            CALL MPI_BCAST(data,81,MPI_INTEGER,0,MPI_COMM_WORLD,err)

            PRINT *, "P:", rank, "matrix received!" ! This is always printed
!           CALL FLUSH_()
            matrix=RESHAPE(data,SHAPE=(/9,9/))
!            CALL PRINTMATRIX(matrix)
            
            CALL MPI_FINALIZE(err)

        END DO

!================================== 
    
        CONTAINS
            
            SUBROUTINE PRINTMATRIX(matrix)

                IMPLICIT NONE
    
                INTEGER, DIMENSION(9,9), INTENT(IN) :: matrix
                INTEGER :: i,j
    
                DO i=1,9
                    WRITE(*,*) matrix(i,1), matrix(i,2), matrix(i,3),&
                               matrix(i,4), matrix(i,5), matrix(i,6),&
                               matrix(i,7), matrix(i,8), matrix(i,9) 
                END DO
            END SUBROUTINE
                
       END PROGRAM     

