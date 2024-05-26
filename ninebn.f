      PROGRAM NINEBN
      CHARACTER*1 ALPHA(13)
      INTEGER*8 TOTAL
      COMMON /NINEB/ TOTAL,ALPHA
      DATA ALPHA/'A','B','C','D','E','F','G','H','I','J','K','L','M'/
      TOTAL = 0
      CALL GEN1
      WRITE(*,*)
      WRITE(*,*) TOTAL," names generated"
      WRITE(*,*)
      CALL GEN2
      WRITE(*,*)
      WRITE(*,*) TOTAL," names generated"
      WRITE(*,*)
      CALL GEN3
      WRITE(*,*)
      WRITE(*,*) TOTAL," names generated"
      WRITE(*,*)
      CALL GEN4
      WRITE(*,*)
      WRITE(*,*) TOTAL," names generated"
      WRITE(*,*)
      CALL GEN5
      WRITE(*,*)
      WRITE(*,*) TOTAL," names generated"
      WRITE(*,*)
      CALL GEN6
      WRITE(*,*)
      WRITE(*,*) TOTAL," names generated"
      WRITE(*,*)
      CALL GEN7
      WRITE(*,*)
      WRITE(*,*) TOTAL," names generated"
      WRITE(*,*)
      CALL GEN8
      WRITE(*,*)
      WRITE(*,*) TOTAL," names generated"
      WRITE(*,*)
      CALL GEN9
      WRITE(*,*)
      WRITE(*,*) TOTAL," names generated"
      WRITE(*,*)
      END
C
      SUBROUTINE GEN1
      CHARACTER*1 ALPHA(13)
      INTEGER*8 TOTAL
      COMMON /NINEB/ TOTAL,ALPHA
      INTEGER L1
      DO 100, L1 = 1,13
      WRITE(*,*) ALPHA(L1)
      TOTAL=TOTAL+1
  100 CONTINUE
      RETURN
      END
C
      SUBROUTINE GEN2
      CHARACTER*1 ALPHA(13)
      INTEGER*8 TOTAL
      COMMON /NINEB/ TOTAL,ALPHA
      INTEGER L1,L2
      DO 100, L1 = 1,13
      DO 200, L2 = 1,13
      WRITE(*,*) ALPHA(L1),ALPHA(L2)
      TOTAL=TOTAL+1
  200 CONTINUE
  100 CONTINUE
      RETURN
      END
C
      SUBROUTINE GEN3
      CHARACTER*1 ALPHA(13)
      INTEGER*8 TOTAL
      COMMON /NINEB/ TOTAL,ALPHA
      INTEGER L1,L2,L3
      DO 100, L1 = 1,13
      DO 200, L2 = 1,13
      DO 300, L3 = 1,13
      WRITE(*,*) ALPHA(L1),ALPHA(L2),ALPHA(L3)
      TOTAL=TOTAL+1
  300 CONTINUE
  200 CONTINUE
  100 CONTINUE
      RETURN
      END
C
      SUBROUTINE GEN4
      CHARACTER*1 ALPHA(13)
      INTEGER*8 TOTAL
      COMMON /NINEB/ TOTAL,ALPHA
      INTEGER L1,L2,L3,L4
      DO 100, L1 = 1,13
      DO 200, L2 = 1,13
      DO 300, L3 = 1,13
      DO 400, L4 = 1,13
      IF (.NOT.((L1.EQ.L2) .AND. (L2.EQ.L3) .AND. (L3.EQ.L4))) THEN
         WRITE(*,*) ALPHA(L1),ALPHA(L2),ALPHA(L3),ALPHA(L4)
         TOTAL=TOTAL+1
      END IF
  400 CONTINUE
  300 CONTINUE
  200 CONTINUE
  100 CONTINUE
      RETURN
      END
C
      SUBROUTINE GEN5
      CHARACTER*1 ALPHA(13)
      INTEGER*8 TOTAL
      COMMON /NINEB/ TOTAL,ALPHA
      INTEGER L1,L2,L3,L4,L5
      DO 100, L1 = 1,13
      DO 200, L2 = 1,13
      DO 300, L3 = 1,13
      DO 400, L4 = 1,13
      DO 500, L5 = 1,13
      IF ((.NOT.((L1.EQ.L2) .AND. (L2.EQ.L3) .AND. (L3.EQ.L4))) .AND.
     +    (.NOT.((L2.EQ.L3) .AND. (L3.EQ.L4) .AND. (L4.EQ.L5)))) THEN
         WRITE(*,*) ALPHA(L1),ALPHA(L2),ALPHA(L3),ALPHA(L4),ALPHA(L5)
         TOTAL=TOTAL+1
      END IF
  500 CONTINUE
  400 CONTINUE
  300 CONTINUE
  200 CONTINUE
  100 CONTINUE
      RETURN
      END
C
      SUBROUTINE GEN6
      CHARACTER*1 ALPHA(13)
      INTEGER*8 TOTAL
      COMMON /NINEB/ TOTAL,ALPHA
      INTEGER L1,L2,L3,L4,L5,L6
      DO 100, L1 = 1,13
      DO 200, L2 = 1,13
      DO 300, L3 = 1,13
      DO 400, L4 = 1,13
      DO 500, L5 = 1,13
      DO 600, L6 = 1,13
      IF ((.NOT.((L1.EQ.L2) .AND. (L2.EQ.L3) .AND. (L3.EQ.L4))) .AND.
     +    (.NOT.((L2.EQ.L3) .AND. (L3.EQ.L4) .AND. (L4.EQ.L5))) .AND.
     +    (.NOT.((L3.EQ.L4) .AND. (L4.EQ.L5) .AND. (L5.EQ.L6)))) THEN 
         WRITE(*,*) ALPHA(L1),ALPHA(L2),ALPHA(L3),ALPHA(L4),ALPHA(L5),
     +              ALPHA(L6)
         TOTAL=TOTAL+1
      END IF
  600 CONTINUE
  500 CONTINUE
  400 CONTINUE
  300 CONTINUE
  200 CONTINUE
  100 CONTINUE
      RETURN
      END
C
      SUBROUTINE GEN7
      CHARACTER*1 ALPHA(13)
      INTEGER*8 TOTAL
      COMMON /NINEB/ TOTAL,ALPHA
      INTEGER L1,L2,L3,L4,L5,L6,L7
      DO 100, L1 = 1,13
      DO 200, L2 = 1,13
      DO 300, L3 = 1,13
      DO 400, L4 = 1,13
      DO 500, L5 = 1,13
      DO 600, L6 = 1,13
      DO 700, L7 = 1,13
      IF ((.NOT.((L1.EQ.L2) .AND. (L2.EQ.L3) .AND. (L3.EQ.L4))) .AND.
     +    (.NOT.((L2.EQ.L3) .AND. (L3.EQ.L4) .AND. (L4.EQ.L5))) .AND.
     +    (.NOT.((L3.EQ.L4) .AND. (L4.EQ.L5) .AND. (L5.EQ.L6))) .AND.
     +    (.NOT.((L4.EQ.L5) .AND. (L5.EQ.L6) .AND. (L6.EQ.L7)))) THEN 
         WRITE(*,*) ALPHA(L1),ALPHA(L2),ALPHA(L3),ALPHA(L4),ALPHA(L5),
     +              ALPHA(L6),ALPHA(L7)
         TOTAL=TOTAL+1
      END IF
  700 CONTINUE
  600 CONTINUE
  500 CONTINUE
  400 CONTINUE
  300 CONTINUE
  200 CONTINUE
  100 CONTINUE
      RETURN
      END
C
      SUBROUTINE GEN8
      CHARACTER*1 ALPHA(13)
      INTEGER*8 TOTAL
      COMMON /NINEB/ TOTAL,ALPHA
      INTEGER L1,L2,L3,L4,L5,L6,L7,L8
      DO 100, L1 = 1,13
      DO 200, L2 = 1,13
      DO 300, L3 = 1,13
      DO 400, L4 = 1,13
      DO 500, L5 = 1,13
      DO 600, L6 = 1,13
      DO 700, L7 = 1,13
      DO 800, L8 = 1,13
      IF ((.NOT.((L1.EQ.L2) .AND. (L2.EQ.L3) .AND. (L3.EQ.L4))) .AND.
     +    (.NOT.((L2.EQ.L3) .AND. (L3.EQ.L4) .AND. (L4.EQ.L5))) .AND.
     +    (.NOT.((L3.EQ.L4) .AND. (L4.EQ.L5) .AND. (L5.EQ.L6))) .AND.
     +    (.NOT.((L4.EQ.L5) .AND. (L5.EQ.L6) .AND. (L6.EQ.L7))) .AND.
     +    (.NOT.((L5.EQ.L6) .AND. (L6.EQ.L7) .AND. (L7.EQ.L8)))) THEN 
         WRITE(*,*) ALPHA(L1),ALPHA(L2),ALPHA(L3),ALPHA(L4),ALPHA(L5),
     +              ALPHA(L6),ALPHA(L7),ALPHA(L8)
         TOTAL=TOTAL+1
      END IF
  800 CONTINUE
  700 CONTINUE
  600 CONTINUE
  500 CONTINUE
  400 CONTINUE
  300 CONTINUE
  200 CONTINUE
  100 CONTINUE
      RETURN
      END
C
      SUBROUTINE GEN9
      CHARACTER*1 ALPHA(13)
      INTEGER*8 TOTAL
      COMMON /NINEB/ TOTAL,ALPHA
      INTEGER L1,L2,L3,L4,L5,L6,L7,L8,L9
      DO 100, L1 = 1,13
      DO 200, L2 = 1,13
      DO 300, L3 = 1,13
      DO 400, L4 = 1,13
      DO 500, L5 = 1,13
      DO 600, L6 = 1,13
      DO 700, L7 = 1,13
      DO 800, L8 = 1,13
      DO 900, L9 = 1,13
      IF ((.NOT.((L1.EQ.L2) .AND. (L2.EQ.L3) .AND. (L3.EQ.L4))) .AND.
     +    (.NOT.((L2.EQ.L3) .AND. (L3.EQ.L4) .AND. (L4.EQ.L5))) .AND.
     +    (.NOT.((L3.EQ.L4) .AND. (L4.EQ.L5) .AND. (L5.EQ.L6))) .AND.
     +    (.NOT.((L4.EQ.L5) .AND. (L5.EQ.L6) .AND. (L6.EQ.L7))) .AND.
     +    (.NOT.((L5.EQ.L6) .AND. (L6.EQ.L7) .AND. (L7.EQ.L8))) .AND.
     +    (.NOT.((L6.EQ.L7) .AND. (L7.EQ.L8) .AND. (L8.EQ.L9)))) THEN 
         WRITE(*,*) ALPHA(L1),ALPHA(L2),ALPHA(L3),ALPHA(L4),ALPHA(L5),
     +              ALPHA(L6),ALPHA(L7),ALPHA(L8),ALPHA(L9)
         TOTAL=TOTAL+1
      END IF
  900 CONTINUE
  800 CONTINUE
  700 CONTINUE
  600 CONTINUE
  500 CONTINUE
  400 CONTINUE
  300 CONTINUE
  200 CONTINUE
  100 CONTINUE
      RETURN
      END
