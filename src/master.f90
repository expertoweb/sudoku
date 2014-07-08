!  _________
! |   FILE  |_______________________________________
! |                 _________                       |
! | master.f90     |  AUTHOR |_______________________________________
! |   _________    |                                                 |
! |  | RESUME  |___| Rubén Muñoz <rmunoz@cbm.uam.es> CBM-SO (CSIC)   |
! |__|             |_________________________________________________|
!    | 30-07-2006:                                              |            
!    |                                                          | 
!    |   Master solver in f90...                                | 
!    |__________________________________________________________|
    
    MODULE stacks
        IMPLICIT NONE
        
        TYPE :: stackt
            INTEGER :: ssize
            TYPE(node), POINTER :: nodep
        END TYPE stackt

        TYPE :: node
            INTEGER, DIMENSION(:,:), POINTER :: data
            TYPE(node), POINTER :: nextp
        END TYPE node

    CONTAINS
        FUNCTION INIT(stack)
            TYPE(stackt), INTENT(INOUT) :: stack
            LOGICAL :: init

            init=.TRUE.
            stack%ssize=0
        END FUNCTION

        FUNCTION DESTROY(stack)
            TYPE(stackt), INTENT(INOUT) :: stack
            LOGICAL :: destroy
            INTEGER, DIMENSION(:,:), ALLOCATABLE :: board
            INTEGER :: h,w
            
            IF(ASSOCIATED(stack%nodep)) THEN
                h=SIZE(stack%nodep%data,1)
                w=SIZE(stack%nodep%data,2)
                ALLOCATE(board(h,w))
                DO WHILE(POP(stack,board))
                END DO
                DEALLOCATE(board)
            END IF
            destroy=.TRUE.
            
        END FUNCTION

        FUNCTION PUSH(stack, board)
            TYPE(stackt), INTENT(INOUT) :: stack
            TYPE(node), POINTER :: new
            INTEGER, DIMENSION(:,:), INTENT(IN) :: board
            INTEGER, DIMENSION(:,:), TARGET, ALLOCATABLE :: data
            INTEGER :: i,ssize
            LOGICAL :: push
    
            ALLOCATE(new)
            ALLOCATE(new%data(SIZE(board,1),SIZE(board,2)))
            new%data=board
            IF (ASSOCIATED(stack%nodep)) THEN
                new%nextp=>stack%nodep
            END IF
            stack%nodep=>new

            stack%ssize=stack%ssize+1
            push=.TRUE.
        END FUNCTION

        FUNCTION POP(stack,board)
            TYPE(stackt), INTENT(INOUT) :: stack
            TYPE(node), POINTER :: tmp
            INTEGER, DIMENSION(:,:), INTENT(OUT) :: board
            INTEGER, DIMENSION(:,:), POINTER :: auxp
            INTEGER :: i,ssize
            LOGICAL :: pop

            pop=.FALSE.
            IF (stack%ssize == 0) RETURN
                
            IF (ASSOCIATED(stack%nodep%nextp)) THEN
                tmp=>stack%nodep%nextp
            END IF
            board=stack%nodep%data
            DEALLOCATE(stack%nodep%data)
            DEALLOCATE(stack%nodep)
            stack%nodep=>tmp
            stack%ssize=stack%ssize-1
            pop=.TRUE.
        END FUNCTION
        
        FUNCTION SEARCH(stack,board)
            TYPE(stackt), INTENT(INOUT) :: stack
            TYPE(node), POINTER :: tmp
            INTEGER, DIMENSION(:,:), INTENT(IN) :: board
            LOGICAL :: search

            search=.FALSE.

            IF (ASSOCIATED(stack%nodep)) THEN
                tmp=>stack%nodep
                IF (ALL(tmp%data == board)) THEN
                    search=.TRUE.
                    RETURN
                END IF
                DO WHILE(ASSOCIATED(tmp%nextp))
                    tmp=>tmp%nextp
                    IF (ALL(tmp%data == board)) THEN
                        search=.TRUE.
                        RETURN
                    END IF
                END DO
            END IF
        END FUNCTION

    END MODULE stacks
    
    PROGRAM MASTER
        USE stacks
        IMPLICIT NONE
    
        INTEGER :: h,w
        INTEGER, DIMENSION(:,:), ALLOCATABLE :: initial, final, actual
        TYPE(stackt) :: stack
        LOGICAL :: success
        
        READ (*,*) h, w 
        WRITE(*,*) 'h=',h,'w=',w
        
        ALLOCATE (initial(h,w))
        ALLOCATE (final(h,w))

        CALL READ_BOARD(initial, h, w)
        CALL READ_BOARD(final, h, w)

        success=INIT(stack)

        success=DEPTH(stack, initial, final, h, w)

        IF (success) THEN
            WRITE(*,*) 'Success!'
        ELSE 
            WRITE(*,*) 'Sorry!'
        END IF

        success=DESTROY(stack)

        DEALLOCATE (initial)
        DEALLOCATE (final)

    CONTAINS
        SUBROUTINE READ_BOARD(board, height, width)
            IMPLICIT NONE
            INTEGER, INTENT(IN) :: height,width
            INTEGER, DIMENSION(width,height) :: tmp
            INTEGER, DIMENSION(:,:), INTENT(out) :: board

            READ *, tmp
            board=TRANSPOSE(tmp)
        END SUBROUTINE

        SUBROUTINE PRINT_BOARD(board)
            IMPLICIT NONE
   
            INTEGER, DIMENSION(:,:), INTENT(IN) :: board
            INTEGER :: i,j,maxi
            
            maxi=SIZE(board,1) 
            DO i=1,maxi
                WRITE(*,*) board(i,:)
            END DO
        END SUBROUTINE

        FUNCTION CHOOSE(pieces)
            INTEGER :: choose,i
            INTEGER, DIMENSION(:) :: pieces

            choose=0
            DO i=1,SIZE(pieces,1)
                IF (pieces(i) /= 0) THEN
                    choose=pieces(i)
                    WHERE (pieces == choose) pieces=0
                    RETURN
                END IF
            END DO
            
        END FUNCTION

        RECURSIVE FUNCTION EXPLORE(stack, board, final, piece, height, width)
            TYPE(stackt) :: stack
            INTEGER :: piece,i,j
            INTEGER, INTENT(IN) :: height,width
            INTEGER, DIMENSION(:,:), INTENT(IN) :: board,final
            INTEGER, DIMENSION(height,width) :: new
            LOGICAL, DIMENSION(height,width) :: mover,movel,moveu,moved,mask 
            LOGICAL :: possr, possl, possu, possd, success, explore
            
            explore=.FALSE.
             
            mask = board == 0 .OR. board == piece
            mover = .FALSE.
            movel = .FALSE.
            moveu = .FALSE.
            moved = .FALSE.
            possr = .TRUE.
            possl = .TRUE.
            possu = .TRUE.
            possd = .TRUE.

            DO i = 1,height
                DO j = 1,width
                    IF (board(i,j)==piece) THEN
                        IF (i < h .AND. mask(i+1,j)) THEN
                            moved(i+1,j) = .TRUE.
                        ELSE
                            possd = .FALSE.
                        END IF
                        IF (i > 1 .AND. mask(i-1,j)) THEN
                            moveu(i-1,j) = .TRUE.
                        ELSE
                            possu = .FALSE.
                        END IF
                        IF (j < w .AND. mask(i,j+1)) THEN
                            mover(i,j+1) = .TRUE. 
                        ELSE
                            possr = .FALSE.
                        END IF
                        IF (j > 1 .AND. mask(i,j-1)) THEN
                            movel(i,j-1) = .TRUE.
                        ELSE
                            possl = .FALSE.
                        END IF
                    END IF
                END DO
            END DO
            
            IF (possl) THEN
                new = board
                WHERE (new==piece) new=0
                WHERE (movel) new = piece
                IF (DEPTH(stack,new,final,height,width)) THEN
!                    WRITE(*,*) 'Left:'
!                    CALL PRINT_BOARD (new)
                    explore = .TRUE.
                    RETURN
                END IF
            END IF

            IF (possr) THEN
                new = board
                WHERE (new==piece) new=0
                WHERE (mover) new = piece
                IF (DEPTH(stack,new,final,height,width)) THEN
!                    WRITE(*,*) 'Right:'
!                    CALL PRINT_BOARD (new)
                    explore = .TRUE.
                    RETURN
                END IF
            END IF

            IF (possu) THEN
                new = board
                WHERE (new==piece) new=0
                WHERE (moveu) new = piece
                IF (DEPTH(stack,new,final,height,width)) THEN
!                    WRITE(*,*) 'Up:'
!                    CALL PRINT_BOARD (new)
                    explore = .TRUE.
                    RETURN
                END IF
            END IF

            IF (possd) THEN
                new = board
                WHERE (new==piece) new=0
                WHERE (moved) new = piece
                IF (DEPTH(stack,new,final,height,width)) THEN
!                    WRITE(*,*) 'Down:'
!                    CALL PRINT_BOARD (new) 
                    explore = .TRUE.
                    RETURN
                END IF
            END IF
        END FUNCTION

        RECURSIVE FUNCTION DEPTH(stack, initial, final, height, width)
            IMPLICIT NONE
            TYPE(stackt), INTENT(INOUT) :: stack
            INTEGER :: piece
            INTEGER, INTENT(IN) :: height,width
            INTEGER, DIMENSION(:,:), INTENT(IN) :: final, initial 
            INTEGER, DIMENSION(height,width) :: test
            INTEGER, DIMENSION(height*width):: packed
            LOGICAL :: success,depth

            depth=.FALSE.

            packed=0

            IF (SEARCH(stack,initial)) RETURN

            test=final
            WHERE(test /= 0) test=test-initial
    
            IF (ALL (test == 0)) THEN
                CALL PRINT_BOARD(initial)
                depth=.TRUE.
                RETURN 
            END IF

            success=PUSH(stack,initial)
            
            packed=PACK(initial,MASK=(initial /= 0))

            piece=CHOOSE(packed)
            DO WHILE(piece /= 0)
                success=EXPLORE(stack,initial,final,piece,height,width)
                IF (success) THEN
                    WRITE(*,*) '^^^^^^^^^^'
                    CALL PRINT_BOARD(initial)
                    depth=.TRUE.
                    EXIT
                END IF
                piece=CHOOSE(packed)
            END DO

        END FUNCTION


    END PROGRAM
