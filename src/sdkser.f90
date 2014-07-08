!  _________
! |   FILE  |_______________________________________
! |                 _________                       |
! | sdkser.f90     |  AUTHOR |_______________________________________
! |   _________    |                                                 |
! |  | RESUME  |___| Rubén Muñoz <rmunoz@cbm.uam.es> CBM-SO (CSIC)   |
! |__|             |_________________________________________________|
!    | 30-07-2006:                                              |            
!    |                                                          | 
!    |   Sdk solver in f90...                                   | 
!    |__________________________________________________________|

        PROGRAM SDK

        IMPLICIT NONE
    
        INTEGER, DIMENSION(9,9), TARGET :: board
        INTEGER :: i,j
    
!        DO i=1,9 ! Read initial board
!            DO j=1,9
                READ *, board
                board=TRANSPOSE(board)
!                DO i=1,9
!                    WRITE(*,*) board(i,1), board(i,2), board(i,3), board(i,4), board(i,5), board(i,6),&
!                               board(i,7), board(i,8), board(i,9) 
!                END DO
!            END DO
!        END DO
    
        i=1; j=1
    
        CALL SEARCHDEPTH(board,i,j)
    
        CONTAINS
    
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
                        CALL PRINTRESULT (board)
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
                            IF(CHECKBOX(box).AND.CHECKROW(row).AND.CHECKCOL(col)) THEN
                                IF ( j<9 ) CALL SEARCHDEPTH (board,i,j+1)
                                IF ( j==9 .AND. i<9 ) CALL SEARCHDEPTH (board,i+1,1)
                                IF ( j==9 .AND. i==9 ) THEN
                                    CALL PRINTRESULT (board)
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
    
            SUBROUTINE PRINTRESULT(board)

                IMPLICIT NONE
    
                INTEGER, DIMENSION(9,9), INTENT(IN) :: board
                INTEGER :: i,j
    
                DO i=1,9
                    WRITE(*,*) board(i,1), board(i,2), board(i,3), board(i,4), board(i,5), board(i,6),&
                               board(i,7), board(i,8), board(i,9) 
                END DO
            END SUBROUTINE
                
       END PROGRAM     

