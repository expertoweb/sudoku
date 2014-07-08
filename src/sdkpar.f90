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
        INTEGER :: number, err, rank, size, nfree, i, j, mine, nvalid, checkstat, npossible, step, n, p, v, nfreebis, nvalidbis
        INTEGER, DIMENSION(9) :: valid
        INTEGER, DIMENSION(1) :: single
        INTEGER, DIMENSION(9,9,9), TARGET :: possible

        ! Trabajo con cajas, filas y columnas de possibles
        INTEGER :: ibox, jbox
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
            READ (*,*,END=666) board
            board=TRANSPOSE(board)
            data=RESHAPE(board, SHAPE=(/81/))
            PRINT *, "P:",rank,"initial board:"
            board=RESHAPE(data, SHAPE=(/9,9/))
            CALL PRINTBOARD(board)
        END IF
        
        
        ! Each board modification takes here
        DO step=1,2
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
                EXIT
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
                    PRINT *, "P:",rank,"Estudiando board(",i,",",j,")=[",possible(i,j,:),"]"
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
                        nvalidbis=nvalid
                        DO WHILE (nvalidbis > 0)               
                            p=p+1
                            v=possible(i,j,p)
                            IF (v==0) CYCLE
                            nvalidbis=nvalidbis-1
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
                        ! If the cell could not be assigned the program 
                        ! tries to get into a conflict using all the possible 
                        ! values. This can erase some possibles.                    
!                        IF (v==0) THEN
!                            p=0
!                            nvalidbis=nvalid
!                            DO WHILE (nvalidbis > 0)
!                                p=p+1
!                                v=possible(i,j,p)
!                                IF (v==0) CYCLE
!!     PRINT *, "P:",rank,"Checking conflicts board(", i, ",",j,")=",v
!                                nvalidbis=nvalidbis-1
!      PRINT *,"P:", rank, "Suposicion board(",i,",",j,")=",v
!                                IF (CHECK_CONSISTENCY(possible,i,j,v)) THEN
!                                    ! Aqui se puede borrar el valor v de la 
!                                    ! lista de possibles para i,j
!      PRINT *,"P:", rank, "Conflicto!!!!!!! possible(",i,",",j,")=",v
!                                    possible(i,j,p)=0
!                                    nvalid=nvalid-1
!                                    ! Al tener una posibilidad menos puedo asignar si   
!                                    ! solo tenia 2 elementos possibles
!!      PRINT *, "P:",rank,"Actualizado tras conflicto board(",i,",",j,")=[",possible(i,j,:),"]"
!                                END IF
!                            END DO
!                            IF (nvalid==1) THEN
!                                v=MAXVAL(possible(i,j,:))
!      PRINT *,"P:", rank, "Por reduccion board(",i,",",j,")=",v
!                            ELSE
!                                v=0
!                            END IF
!                        END IF
                    END IF
                    PRINT *, "P:",rank,"Enviando board(",i,",",j,")=",v
                    CALL MPI_SEND(v,1,MPI_INTEGER,0,SDK_MSG_ASSIGN,MPI_COMM_WORLD,err)
!                       Actualiza los possibles de la caja y la col y la row
                    IF (v > 0) THEN
                        WHERE (box == v) box=0
                        WHERE (col == v) col=0
                        WHERE (row == v) row=0
                        possible(i,j,:)=0
                        possible(i,j,1)=v
                    END IF

                    IF (rank+n*(size-1) > 81) EXIT
                    ! Take next free cell
                    mine=free(rank+n*(size-1))
                    n=n+1
                END DO
                
                PRINT *, "P:",rank,"ended"
            ELSE
                ! P: 0 Maestro
                PRINT *, "Step:", step
                board=RESHAPE(data, SHAPE=(/9,9/))
                CALL PRINTBOARD(board)
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
                            
                            PRINT *, "Asignado data(",v,")=",data(v)
                            nfree=nfree-1
                            PRINT *, "nfree=", nfree

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

        END DO
666     CALL MPI_FINALIZE(err)

!================================== 
    
        CONTAINS
                        
            
!            RECURSIVE FUNCTION CHECK_CONSISTENCY(possible, i, j, value) RESULT (conflict)
!                IMPLICIT NONE
!
!                INTEGER, DIMENSION(9,9,9), INTENT(IN) :: possible
!                INTEGER, INTENT(IN) :: i,j,value
!
!                INTEGER, DIMENSION(9,9,9), TARGET:: possiblebis
!                LOGICAL :: conflict
!                LOGICAL, DIMENSION(9,9) :: countone, newcountone
!
!                INTEGER :: ibox, jbox, ione, jone, onlyvalue
!
!                INTEGER, DIMENSION(:,:), POINTER :: col,row
!                INTEGER, DIMENSION(:,:,:), POINTER :: box
!
!                conflict=.FALSE.
!
!                possiblebis=possible 
!                ! Supossition board
!!                PRINT *, "Supossition board"
!!                CALL PRINTBOARD(possiblebis(:,:,1))
!                
!                possiblebis(i,j,:)=0
!                possiblebis(i,j,1)=value
!                
!                ibox=i-MOD(i-1,3)
!                jbox=j-MOD(j-1,3)
!                box=>possiblebis(ibox:ibox+2,jbox:jbox+2,:)
!                col=>possiblebis(:,j,:)
!                row=>possiblebis(i,:,:)
!
!
!                countone = COUNT(MASK=(possiblebis /= 0),DIM=3) == 1
!                ! Estudiar las implicaciones que tiene fijar este valor
!                ! Devolver .TRUE. si genera un conflicto y .FALSE. si no
!                WHERE (box == value) box=0
!                WHERE (col == value) col=0
!                WHERE (row == value) row=0
!                possiblebis(i,j,1)=value
!
!                ! Comprobar que no hay ninguna celda sin posibilidades de asignación
!                ! En caso de que haya podemos decir que ha habido un conflicto
!!               PRINT *, ALL(MASK=(possiblebis==0),DIM=3)
!!               PRINT *, ANY(ALL(MASK=(possiblebis==0),DIM=3))                
!
!                IF (ANY(ALL(MASK=(possiblebis==0),DIM=3))) THEN
!                    PRINT *, ALL(MASK=(possiblebis==0),DIM=3)
!                    conflict=.TRUE.
!                    RETURN
!                ELSE
!                    newcountone = COUNT(MASK=(possiblebis /= 0),DIM=3) == 1                    
!                    
!                    DO ione=1,9
!                        DO jone=1,9
!                            IF (newcountone(ione,jone).AND.(.NOT.countone(ione,jone))) THEN
!                                PRINT *, "P:",rank,"implicacion: board(",ione,",",jone,")=", MAXVAL(possiblebis(ione,jone,:))
!                                IF ((i /= ione) .AND. (j /= jone) .AND. (board(ione,jone)==0)) THEN
!                                    ! Find the only possible
!                                    onlyvalue=MAXVAL(possiblebis(ione,jone,:))
!                                    PRINT *, "Recursivamente con board(",ione,",",jone,")=",onlyvalue
! 
!                                    conflict=check_consistency(possiblebis,ione,jone,onlyvalue)
!                                    IF (conflict) RETURN
!                                END IF
!                            END IF
!                        END DO
!                    END DO
!                END IF
!
!                RETURN
!
!            END FUNCTION


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
                col=>board(:,j)
                row=>board(i,:)
    
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

