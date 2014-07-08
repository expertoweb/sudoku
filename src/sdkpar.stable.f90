!  _________
! |   FILE  |_______________________________________
! |                 _________                       |
! | sdkpar.f       |  AUTHOR |_______________________________________
! |   _________    |                                                 |
! |  | RESUME  |___| Rubén Muñoz <rmunoz@cbm.uam.es> CBM-SO (CSIC)   |
! |__|             |_________________________________________________|
!    | 30-07-2006:                                              |            
!    | - Sdk solver in f90...                                   | 
!    | 02-07-2006:                                              |            
!    | - Paralellized version with MPI.                         | 
!    |__________________________________________________________|
        PROGRAM SDKPAR

        IMPLICIT NONE
       
!============MPI CODE============== 
        INCLUDE 'mpif.h'
!================================== 
    
        INTEGER, DIMENSION(9,9) :: board
    
!============MPI CODE============== 
        INTEGER, PARAMETER :: SDK_MSG_POSSIBLE = 55
        INTEGER, PARAMETER :: SDK_MSG_ASSIGN = 55
        INTEGER :: number, err, rank, size, nfree, i, j, mine, nvalid, checkstat, npossible, step, n, p, v, nfreebis
        INTEGER :: ibox, jbox
        INTEGER, DIMENSION(9) :: valid
        INTEGER, DIMENSION(1) :: single
        INTEGER, DIMENSION(9,9,9), TARGET :: possible

        ! Trabajo con cajas, filas y columnas de possibles
        INTEGER, DIMENSION(:,:), POINTER :: col,row
        INTEGER, DIMENSION(:,:,:), POINTER :: box


        INTEGER, DIMENSION(81) :: data, free 
        INTEGER, DIMENSION(729) :: flatpossible
        INTEGER, DIMENSION(MPI_STATUS_SIZE) :: status

        CALL MPI_INIT(err)
        CALL MPI_COMM_RANK(MPI_COMM_WORLD,rank,err)
        CALL MPI_COMM_SIZE(MPI_COMM_WORLD,size,err)
        

        IF (rank == 0) THEN
            ! Read board from stdin
            READ *, board
            board=TRANSPOSE(board)
            data=RESHAPE(board, SHAPE=(/81/))
            PRINT *, "P:",rank,"initial board:"
            board=RESHAPE(data, SHAPE=(/9,9/))
            CALL PRINTBOARD(board)
        END IF
        
        step=1
        ! Each board modification takes here
        DO
            IF (rank == 0) THEN            
                nfree=FIND_FREE_CELLS(data,free)
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
            ! This could take to infinite loop
            IF ( nfree==81 .OR. nfree==0 ) THEN
                CALL MPI_FINALIZE(err)
            END IF
        
            IF (rank > 0) THEN
                ! Start with the free cell corresponding with the proccess rank
                mine=free(rank)
        
                n=1
                DO WHILE ( mine /= 0 )
                    ! Obtain coordinates for mine 
                    i=mod(mine-1,9)+1
                    j=(mine-1)/9+1
!                   PRINT *, "P:",rank,"is assigned board(",i,",",j,")"
                    ! Reconstruct board                           
                    board=RESHAPE(data, SHAPE=(/9,9/))

                    ! Obtaing valid positions
                    nvalid=FIND_VALID_VALUES(board, i, j, valid)
!                    PRINT *, "P:",rank,"found",nvalid,"valid values", valid
                    CALL MPI_SEND(valid,9,MPI_INTEGER,0,SDK_MSG_POSSIBLE,MPI_COMM_WORLD,err)

                    ! Check if the cell can be assigned
!                    IF (nvalid==1) THEN
                           ! Assigned. No free any more
!                           data(mine)=valid(1)                           
!                    END IF

                    IF (rank+n*(size-1) > 81) EXIT
                    ! Take next free cell
                    mine=free(rank+n*(size-1))
                    n=n+1
                END DO

                CALL MPI_BCAST(flatpossible,729,MPI_INTEGER,0,MPI_COMM_WORLD,err)
                possible=RESHAPE(flatpossible,SHAPE=(/9,9,9/))

                ! Segunda vuelta teniendo en cuenta los possible de toda la caja y fila 
                mine=free(rank)
                n=1
                DO WHILE ( mine /= 0 )
                    i=mod(mine-1,9)+1
                    j=(mine-1)/9+1
!                    PRINT *, "P:",rank,"Estudiando board(",i,",",j,")=[",possible(i,j,:),"]"
                    nvalid=COUNT(MASK=(possible(i,j,:) /= 0))
                    
                    ibox=i-MOD(i-1,3)
                    jbox=j-MOD(j-1,3)
                    box=>possible(ibox:ibox+2,jbox:jbox+2,:)
                    col=>possible(:,j,:)
                    row=>possible(i,:,:)

                    v=0
                    
                    IF (nvalid == 1) THEN
                        v=MAXVAL(possible(i,j,:))
                    ELSE
                        p=0
                        DO WHILE (nvalid > 0)               
                            p=p+1
                            v=possible(i,j,p)
                            IF (v==0) CYCLE
                            nvalid=nvalid-1
!                            PRINT *, "P:",rank,"probando board(", i, ",",j,")=",v,"..."
                            IF (COUNT(MASK=(box==v))==1) THEN
                                EXIT
                            ELSE
                                v=0
                            END IF
                            IF (COUNT(MASK=(col==v))==1) THEN
                                EXIT
                            ELSE
                                v=0
                            END IF
                            IF (COUNT(MASK=(row==v))==1) THEN
                                EXIT
                            ELSE
                                v=0
                            END IF
                        END DO
                    END IF
!                    PRINT *, "P:",rank,"Enviando board(",i,",",j,")=",v
                    CALL MPI_SEND(v,1,MPI_INTEGER,0,SDK_MSG_ASSIGN,MPI_COMM_WORLD,err)
!                       Actualiza los possibles de la caja y la col y la row
                    WHERE (box == v) box=0
                    WHERE (col == v) col=0
                    WHERE (row == v) row=0
                    possible(i,j,:)=0
                    possible(i,j,1)=v

                    IF (rank+n*(size-1) > 81) EXIT
                    ! Take next free cell
                    mine=free(rank+n*(size-1))
                    n=n+1
                END DO
                
!                PRINT *, "P:",rank,"ended"
            ELSE
                ! P: 0 Maestro
                possible=0
                possible(:,:,1)=board(:,:)
                n=0
                nfreebis=nfree
                DO WHILE(nfreebis>0)
                    DO p=1,MIN(size-1,nfreebis)
                        v=free(p+n*(size-1))
                        i=mod(v-1,9)+1
                        j=(v-1)/9+1
                        CALL MPI_RECV(possible(i,j,:),9,MPI_INTEGER,p,SDK_MSG_POSSIBLE,MPI_COMM_WORLD,status,err)
                        nfreebis=nfreebis-1
                    END DO
                    n=n+1
                END DO
                ! Publicar possible para el calculo de cajas
                flatpossible=RESHAPE(possible,SHAPE=(/729/))
                CALL MPI_BCAST(flatpossible,729,MPI_INTEGER,0,MPI_COMM_WORLD,err)

                ! Recibir un dato que puede ser 0 si no se ha encontrado valor
                ! o un valor para la posición.
                n=0
                nfreebis=nfree
                DO WHILE(nfreebis>0)
                    DO p=1,MIN(size-1,nfreebis)
                        v=free(p+n*(size-1))
                        CALL MPI_RECV(data(v),1,MPI_INTEGER,p,SDK_MSG_ASSIGN,MPI_COMM_WORLD,status,err)
                        IF (data(v) /= 0) THEN
                            ! Se ha encontrado un valor valido para data(free(p+n*(size-1))
                            i=mod(v-1,9)+1
                            j=(v-1)/9+1
                            
!                            PRINT *, "Asignado data(",v,")=",data(v)
                            nfree=nfree-1
!                            PRINT *, "nfree=", nfree

                            IF (nfree==0) THEN
                                PRINT *, "Result in ",step,"steps :"
                                board=RESHAPE(data, SHAPE=(/9,9/))
                                CALL PRINTBOARD(board)
                            END IF
                        END IF
                        nfreebis=nfreebis-1
                    END DO
                    n=n+1
                END DO

            END IF
            step=step+1


        END DO
        CALL MPI_FINALIZE(err)

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

