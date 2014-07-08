!  _________
! |   FILE  |_______________________________________
! |                 _________                       |
! | sdkpar.f       |  AUTHOR |_______________________________________
! |   _________    |                                                 |
! |  | RESUME  |___| Rubén Muñoz <rmunoz@cbm.uam.es> CBM-SO (CSIC)   |
! |__|             |_________________________________________________|
!    | 30-07-2006:                                              |            
!    | - Sdk solver in f90...                                   | 
!    | 02-08-2006:                                              |            
!    | - Paralellized version with MPI.                         | 
!    |__________________________________________________________|
!  _____________                                                                      
! |  TRASH BIN  |_____________________________________________________________________
! |           INTEGER, PARAMETER :: SDK_MSG_ASSIGN = 55                               |
! |           PRINT *, "P:",rank," initial board: "                                   |
! |           call PRINTBOARD(board)                                                  |
! |           CALL MPI_SEND(data,81,MPI_INTEGER,1,SDK_MSG_BOARD,MPI_COMM_WORLD,err)   |
! |       else                                                                        |
! |           CALL SEARCHDEPTH(board,i,j)                                             |
! |           CALL MPI_RECV(data,81,MPI_INTEGER,MPI_ANY_SOURCE,SDK_MSG_BOARD,&        |
! |                         MPI_COMM_WORLD,status,err)                                |
! |           PRINT *, "P:",rank," got board from processor ",&                       |
! |                    status(MPI_SOURCE)                                             |
! |           CALL MPI_GET_COUNT(status,MPI_INTEGER,number,err)                       |
! |           PRINT *, "P:",rank," got ",number," elements"                           |
! |           board=RESHAPE(data, SHAPE=(/9,9/))                                      |
! |           PRINT *, "P:",rank," my initial board: "                                |
! |           call PRINTBOARD(board)                                                  |
! |___________________________________________________________________________________|
        PROGRAM SDK

        IMPLICIT NONE
       
!============MPI CODE============== 
        INCLUDE 'mpif.h'
!================================== 
    
        INTEGER, DIMENSION(9,9) :: board
    
!============MPI CODE============== 
        INTEGER :: number, err, rank, size, nfree, i, j, mine, nvalid, checkstat, npossible, step
        INTEGER, DIMENSION(9) :: valid
        INTEGER, DIMENSION(1) :: single
        INTEGER, DIMENSION(:), ALLOCATABLE :: allnvalid, allvalid, possible
        INTEGER, DIMENSION(81) :: data, free 
        INTEGER, DIMENSION(MPI_STATUS_SIZE) :: status

        CALL MPI_INIT(err)
        CALL MPI_COMM_RANK(MPI_COMM_WORLD,rank,err)
        CALL MPI_COMM_SIZE(MPI_COMM_WORLD,size,err)
        
        ALLOCATE(allnvalid(0:size-1))
        ALLOCATE(allvalid(0:size*9-1))

        IF (rank.EQ.0) THEN
            ! Read board from stdin
            READ *, board
            board=TRANSPOSE(board)
            data=RESHAPE(board, SHAPE=(/81/))
        END IF
        
        step=0
        ! Each board modification takes here
        DO
            IF (rank == 0) THEN            
                nfree=FIND_FREE_CELLS(data,free)
                PRINT *, "P:",rank,"initial board free cells:", nfree
                board=RESHAPE(data, SHAPE=(/9,9/))
                CALL PRINTBOARD(board)
            END IF
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
            CALL MPI_BCAST(free,81,MPI_INTEGER,0,MPI_COMM_WORLD,err)
            CALL MPI_BCAST(nfree,1,MPI_INTEGER,0,MPI_COMM_WORLD,err)

            ! All processes end if the board is empty or full
            IF ( nfree==0 .OR. nfree==81 ) THEN
                CALL MPI_FINALIZE(err)
                IF (rank == 0) THEN
                    PRINT *, "Result:"
                    board=RESHAPE(data, SHAPE=(/9,9/))
                    CALL PRINTBOARD(board)
                END IF
                STOP
            END IF
        
            IF (rank > 0) THEN
                mine=free(rank)
                DO i=1, size-1
                    free(i)=0
                END DO
        
                ! Start with the free cell corresponding with the proccess rank
                IF ( mine /= 0 ) THEN    
                    ! mine contains the number of cell assigned to this proccess
                    nfree=size-1
                    ! Obtain coordinates for mine 
                    i=mod(mine-1,9)+1
                    j=(mine-1)/9+1
!!!                    PRINT *, "P:",rank,"is assigned board(",i,",",j,")"
                    ! Reconstruct board                           
                    board=RESHAPE(data, SHAPE=(/9,9/))

                    ! Obtaing valid positions
                    nvalid=FIND_VALID_VALUES(board, i, j, valid)
!!!                    print *, "P:",rank,"found",nvalid,"valid values", valid
                    ! Prepare for allgather
                    CALL MPI_GATHER(nvalid, 1, MPI_INTEGER, allnvalid, 1, MPI_INTEGER, 0,MPI_COMM_WORLD, err)
                    CALL MPI_GATHER(valid, 9, MPI_INTEGER, allvalid, 9, MPI_INTEGER, 0, MPI_COMM_WORLD, err)
                ELSE
                    ! Nothing to do but wait for some order
                    PRINT *, "P:",rank,"jobless"
                    nvalid=0
                    valid=0
                    CALL MPI_GATHER(nvalid, 1, MPI_INTEGER, allnvalid, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, err)
                    CALL MPI_GATHER(valid, 9, MPI_INTEGER, allvalid, 9, MPI_INTEGER, 0, MPI_COMM_WORLD, err)
                END IF
            ELSE
                ! This is process 0
!               DO i=1, size-1
!                   free(i)=0
!               END DO
            
                nvalid=0
                valid=0

                CALL MPI_GATHER(nvalid, 1, MPI_INTEGER, allnvalid, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, err)
                CALL MPI_GATHER(valid, 9, MPI_INTEGER, allvalid, 9, MPI_INTEGER, 0, MPI_COMM_WORLD, err)

                PRINT *, "P:",rank,"all nvalids are",allnvalid
                PRINT *, "P:",rank," valids:", allvalid

! 0 1 2 2 2 1 2 3 4 ...
                DO WHILE (ANY(allnvalid == 1))
                    single=MINLOC(allnvalid, MASK=(allnvalid==1))
                    PRINT *, "Cell:",free(single-1), "has value:", allvalid((single-1)*9)
                    allnvalid(single-1)=0                  
                    data(free(single-1))=allvalid((single-1)*9)
                END DO
            
                ! TODO: Built a list of boards to play with 
                ! The size of the list is the product of all the nvalids
                IF (ANY(allnvalid>0)) THEN                
                    npossible=PRODUCT(allnvalid, MASK=(allnvalid>0))
                    PRINT *, "Step", step, "number of possible boards: ", npossible
                END IF
                ! Maximun memory allocated = 4*81*9^nprocs bytes
                ! With 8 processes and an empty board this is 13Gb
!               ALLOCATE(possible(npossible)*81)

!               print *, "P:",rank,"free positions:",free
            END IF
            step=step+1
        END DO

!================================== 
    
        CONTAINS
            
            FUNCTION FIND_FREE_CELLS (data, free)
                IMPLICIT NONE

                INTEGER, DIMENSION(81), INTENT(IN) :: data
                INTEGER, DIMENSION(81), INTENT(OUT) :: free
                INTEGER :: find_free_cells
                INTEGER :: i
            
                free=0
                find_free_cells=0

                DO i=1,81
                    IF (data(i)==0) THEN
                        free(find_free_cells+1) = i
                        find_free_cells=find_free_cells+1
                    END IF
                END DO

            END FUNCTION

            FUNCTION FIND_VALID_VALUES(board, i, j, valid)
            
                IMPLICIT NONE

                INTEGER, DIMENSION(9,9), TARGET, INTENT(INOUT) :: board
                INTEGER, DIMENSION(9), INTENT(OUT) :: valid
                INTEGER, INTENT(IN) :: i,j
                INTEGER, DIMENSION(:), POINTER :: col,row
                INTEGER, DIMENSION(:,:), POINTER :: box
                INTEGER :: find_valid_values, ibox,jbox,digit

                ! Initialy no valid values
                find_valid_values=0
                valid=0            

                ibox=i-MOD(i-1,3)
                jbox=j-MOD(j-1,3)
                box=>board(ibox:ibox+2,jbox:jbox+2)
!                    DO dummy=1,3
!                        WRITE(*,*) box(dummy,1), box(dummy,2), box(dummy,3)
!                    END DO
                col=>board(:,j)
!                    DO dummy=1,9
!                        WRITE(*,*) col(dummy)
!                    END DO
                row=>board(i,:)
!                    WRITE(*,*) row(1), row(2), row(3),&
!                               row(4), row(5), row(6),&
!                               row(7), row(8), row(9)
    
                DO digit=1,9
                    IF (ALL(box /= digit)) THEN
                        board(i,j)=digit
!                        WRITE(*,*) 'board(',i,',',j,')=', board(i,j)
                        IF(CHECKROW(row).AND.CHECKCOL(col)) THEN
                            valid(find_valid_values+1)=digit
                            find_valid_values=find_valid_values+1
                        END IF
                        board(i,j)=0
                    END IF
                END DO
            END FUNCTION

            RECURSIVE SUBROUTINE SEARCHDEPTH (board, i, j)
            
                IMPLICIT NONE

                INTEGER, DIMENSION(9,9), TARGET, INTENT(INOUT) :: board
                INTEGER, INTENT(IN) :: i,j
                INTEGER, DIMENSION(:), POINTER :: col,row
                INTEGER, DIMENSION(:,:), POINTER :: box
                INTEGER :: ibox,jbox,digit,dummy

                IF ( board(i,j) /= 0 ) THEN
                    IF ( j<9 ) CALL SEARCHDEPTH (board,i,j+1)
                    IF ( j==9 .AND. i<9 ) CALL SEARCHDEPTH (board,i+1,1)
                    IF ( j==9 .AND. i==9 ) THEN
                        CALL PRINTBOARD (board)
                        STOP
                    END IF
                ELSE

                    ibox=i-MOD(i-1,3)
                    jbox=j-MOD(j-1,3)
                    box=>board(ibox:ibox+2,jbox:jbox+2)
!                    DO dummy=1,3
!                        WRITE(*,*) box(dummy,1), box(dummy,2), box(dummy,3)
!                    END DO
                    col=>board(:,j)
!                    DO dummy=1,9
!                        WRITE(*,*) col(dummy)
!                    END DO
                    row=>board(i,:)
!                    WRITE(*,*) row(1), row(2), row(3),&
!                               row(4), row(5), row(6),&
!                               row(7), row(8), row(9)
    
                    DO digit=1,9
                        IF (ALL(box /= digit)) THEN
                            board(i,j)=digit
!                            WRITE(*,*) 'board(',i,',',j,')=', board(i,j)
                            IF(CHECKBOX(box).AND.CHECKROW(row).AND.&
                                                 CHECKCOL(col)) THEN
                                IF ( j<9 ) THEN
                                    CALL SEARCHDEPTH (board,i,j+1)
                                END IF
                                IF ( j==9 .AND. i<9 ) THEN
                                    CALL SEARCHDEPTH (board,i+1,1)
                                END IF
                                IF ( j==9 .AND. i==9 ) THEN
                                    CALL PRINTBOARD (board)
                                    STOP
                                END IF
                            END IF
                        END IF
                    END DO
                    board(i,j)=0
                END IF
            END SUBROUTINE
    
            FUNCTION CHECKBOX(box)
    
                IMPLICIT NONE
    
                INTEGER, DIMENSION(3,3),INTENT(IN) :: box
                INTEGER :: digit,n, i

                LOGICAL :: checkbox
                
                DO digit=1,9
                    n=COUNT(MASK=(box==digit))
                    IF ( n>1 ) THEN
                        checkbox=.FALSE.
                        RETURN
                    END IF
                END DO
                checkbox=.TRUE.
                RETURN
            END FUNCTION
    
            FUNCTION CHECKCOL(col)
    
                IMPLICIT NONE
    
                INTEGER, DIMENSION(9),INTENT(IN) :: col
                INTEGER :: digit,n
                LOGICAL :: checkcol
                
                DO digit=1,9
                    n=COUNT(MASK=(col==digit))
                    IF ( n>1 ) THEN
                        checkcol=.FALSE.
                        RETURN
                    END IF
                END DO
                checkcol=.TRUE.
                RETURN
            END FUNCTION
            

            FUNCTION CHECKROW(row)
    
                IMPLICIT NONE
    
                INTEGER, DIMENSION(9),INTENT(IN) :: row
                INTEGER :: digit,n
                LOGICAL :: checkrow
                
                DO digit=1,9
                    n=COUNT(MASK=(row==digit))
                    IF ( n>1 ) THEN
                        checkrow=.FALSE.
                        RETURN
                    END IF
                END DO
                checkrow=.TRUE.
                RETURN
            END FUNCTION
    
            SUBROUTINE PRINTBOARD(board)

                IMPLICIT NONE
    
                INTEGER, DIMENSION(9,9), INTENT(IN) :: board
                INTEGER :: i,j
    
                DO i=1,9
                    WRITE(*,*) board(i,1), board(i,2), board(i,3),&
                               board(i,4), board(i,5), board(i,6),&
                               board(i,7), board(i,8), board(i,9) 
                END DO
            END SUBROUTINE
                
       END PROGRAM     

