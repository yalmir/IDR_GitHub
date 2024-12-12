      PROGRAM  IDR1f
!     ::: f 77 f :::  (final)
!     In the f version of IDR1, no other than (0,1,8,9) digits are allowed in xn files (they are moved to xx).
!     The two immediately previous forms of the program:
!     (1)  IDR1i <--- now minimized execution time modifying the + / - routines. 
!     (2)  IDR1o <--- the following two improvements are the last before end, that had been included in (o):
!     - Extended with max & min values for dig-length in the out and overall (local) values in the tape file. 
!     - Storing in a file of input-numbers leading to a trail crossing particularly short numbers is added.
!     ==============================  future work (to do) =========================================== 
!     improving of the structure of the code / with no effect in the outcome (after multiple checks!)
!     -----------------------------------------------------------------------------------------------
!     NEXT STEPS: [1] VERSION OF IDR1-255/555 DIGITS [2] VERSION OF IDR2 NOT STORING 2-STEP ATTRACTORS
!     -----------------------------------------------------------------------------------------------
!     https://www.rapidtables.com/convert/number/base-converter.html (for numeral-system conversions)
!     -----------------------------------------------------------------------------------------------
      CHARACTER*1 Ch_Array(77), Ch_Array1(77),Ch_Array2(77),Ch_Array3(77),Ch_Array4(77),Ch_ArrayN(11000,77),Ch_ArrayM(11000,77)
      INTEGER   L_appear,L_appear_attr,TrMAX,TrMAXex,TrMIN,TrMINex,L_data,L_dataR,IIDMAX,I,AttrSTATE,AttrID
      INTEGER   ISTEPSexplMAX1,ISTEPSexplMAX,ISTEPSMAX1,ISTEPSMAX,DataNr1,DigLen,DigLenm,DigLenmin,DigLendat
      INTEGER   ISTDILeMAX, ISTDILeMAX1, ISTDILeMIN, ISTDILeMIN1, Lc, LL, DD
      INTEGER*1 T(78),V(78),W(78),X1(78),X2(78),T1(78),T0(78),Al1(78),Al2(78),DRAFT(78),datanr(78),TQT(78)
      INTEGER*1 VV(1000,78), AV(900,78), IQ(1000), ENDf(78)
      INTEGER*1 Al1old(11000,78), Al1new(11000,78), Al1met(11000,78)
      INTEGER*4 Attrold_ID(11000,4), Attrmet_ID(11000,4), Attrnew_ID(11000,4), LastDnr
      INTEGER*4 IEXEC, L_dataq, IstrNr,L_IEXECmax, Ic_Undecided, Ic_Expl, IstrNr1, IJJIMEAN
      INTEGER*4 Ic_predexplx1, Ic_predexplx2, Ic_predexplx3,  Ic_predexplx4, Ic_predexplxx
      OPEN (5, FILE='IDR_dat.txt')		!
      OPEN (6, FILE='IDR_out.txt')		!
      OPEN (7, FILE='IDR_tape.txt')		!
      OPEN (8, FILE='IDR_attrs_old.txt')	!
      OPEN (9, FILE='IDR_attrs_new.txt')	!
      OPEN (10,FILE='IDR_attrs_reused.txt')	!
      OPEN (11,FILE='IDR_explodingx1.txt')   	!
      OPEN (12,FILE='IDR_explodingx2.txt')   	!
      OPEN (13,FILE='IDR_explodingx3.txt')   	!
      OPEN (14,FILE='IDR_explodingx4.txt')   	!
      OPEN (19,FILE='IDR_explodingxx.txt')   	!
      OPEN (20,FILE='IDR_stored_LDL_IN.txt')   	!
!
      WRITE(*,*) 'ITERATIVE DIGITAL REVERSION IDR1 - ver.1.0       (f)           ::'
      WRITE(6,*) ' ITERATIVE DIGITAL REVERSION IDR1 - ver.1.0       (f)           ::'
      WRITE(7,*) 'ITERATIVE DIGITAL REVERSION IDR1 - ver.1.0       (f)           ::'
!     ==============================================================================
      WRITE(7,*) 'data file inscriptions'
      READ (5,*)   DD
      WRITE(7,*)   DD 
      WRITE(6,*) ' Number system:   ', DD
      READ (5,*)   L_appear, L_disappear
      WRITE(7,*)   L_appear, L_disappear
      IF(L_appear.EQ.0)        WRITE(6,*) ' IDR steps will be omitted   '
      IF(L_appear.EQ.1)        WRITE(6,*) ' IDR steps will appear       '
      IF(L_appear.EQ.2)        WRITE(6,*) ' IDR steps and initial number will be omitted  '
      IF(L_disappear.EQ.1)     WRITE(6,*) ' Report line for individual input-numbers only for Exec-Numbers: 1-11  '
      READ (5,*)   L_appear_attr
      WRITE(7,*)   L_appear_attr
      IF(L_appear_attr.EQ.0)   WRITE(6,*) ' Attractor will be omitted   '
      IF(L_appear_attr.EQ.1)   WRITE(6,*) ' Attractor will appear       '
!
      READ (5,*)   L_data, L_dataq, L_dataR   
      WRITE(7,*)   L_data, L_dataq, L_dataR   
      IF(L_data.EQ.0)  WRITE(6,*) ' Input will be a list of integers    '
      IF(L_data.EQ.1)  WRITE(6,*) ' IDR computed from 1 to', DD, '   exp',  L_dataq
      IF((L_data.LT.0).and.(L_dataR.eq.0))  WRITE(6,*) ' Input will be',L_dataq,'  rndm nrs of digital length:', -L_data,' - standard seed' 
      IF((L_data.LT.0).and.(L_dataR.ne.0))  WRITE(6,*) ' Input will be',L_dataq,'  rndm nrs of digital length:', -L_data,' - random seed' 
      IF(L_dataR.ne.0)     CALL RANDOM_SEED		! FOR L_dataR = 0 THE RANDOM NUMBERS WILL ALLWAYS BE FROM THE 'STANDARD SET'
!    
      READ (5,*)  L_IEXECmax
      WRITE(7,*)  L_IEXECmax
      IF(L_IEXECmax.EQ.0) WRITE(6,*) ' No max execution counter will be used'
      IF(L_IEXECmax.NE.0) WRITE(6,*) ' Execution counter will stop program after', L_IEXECmax, ' input numbers' 
!    
      READ (5,*) Kadd 
      WRITE(7,*) Kadd
      IF(Kadd.EQ.0)  WRITE(6,*) ' No additive factor for the IDs of the NEW attractors registry: Kadd = 0' 
      IF(Kadd.NE.0)  WRITE(6,*) ' Additive factor for the IDs in the NEW attractors registry:', Kadd
!     
      DigLenmin = 7
      DigLenm = 1.5*L_dataq
      IF(L_data.LT.0) DigLenm = -1.5*L_data
      DigLenm = MAX0(DigLenm, DigLenmin)	
      READ (5,*) DigLen, MaxLenStore
      DigLendat =  DigLen		
      IF(DigLendat.EQ.0)  WRITE(6,*) 'NO MIN.DIG.LENGTH FOR THE OUT-Nr COMPUTATION (DigLen = 0)', '     [', MaxLenStore, '  ]'
      IF(DigLendat.EQ.0)  WRITE(7,*) 'NO MIN.DIG.LENGTH FOR THE OUT-No COMPUTATION (DigLen = 0)', '     [', MaxLenStore, '  ]'	
      IF(DigLen.GT.70)    DigLen = 70			! In the following lines an effective-DigLen value is computed so that
      IF(DigLendat.NE.0)  DigLen = MAX0(DigLen, DigLenm)! it suffices for working with particularly long randomly-composed numbers
      IF(DigLen.GT.70)    DigLen = 70			! of 'L_data digits', or by adding units (+1) til reaching DDexp(L_dataq)
      IF((DigLendat.NE.0).AND.(DigLendat.NE.DigLen))  WRITE(6,*) ' INITIAL DigLen VALUE CHANGED TO effective-DigLen =',  DigLen
      IF(DigLendat.EQ.0)  WRITE(6,*) ' Outcome printed just when explosive pattern "10999.." established'     
      IF(DigLendat.NE.0)  WRITE(6,*) ' Outcome printed at digital length ~', DigLen    
      IF(DigLendat.NE.0)  WRITE(7,*) DigLenDAT, '       ---->', DigLen, '     [', MaxLenStore, '  ]'
!
      IEXEC = 0     			!counter used internally counting input-numbers for IDR computing 
      DO JUC=1,11000			! NUMBERING OLD ATTRACTORS
      READ (8,898,ERR=89,END=14) (Ch_Array2(I), I=77,1,-1), Attrold_ID(JUC,1), Attrold_ID(JUC,2)
      CALL CEDREHGIH (TQT,Ch_Array2)  	
      DO J=77,1,-1			! ^ ATTENTION: Ch_Array2(77) (THE ATTRACTOR) IS 1DIGIT CHARACTER-VECTOR (INPUT-Nr NOT READ)
      Al1old(JUC,J) = TQT(J)		! <--- THE ATTR GENERATS 1DIGIT CHARACTER ARRAY FOR FURTHER COMPARISONS 
      ENDDO	
      ENDDO			
   14 CONTINUE
      JUC=JUC-1                     	! JUC is the number of old attractors. 
      JUN=0				! JUN is the number of new attractors. 
      Jmet=0   				! Jmet nr of OLD attractors MET here.
!     ============  for use in IstrNr (INTEGER*4) computation ================ 
!     INTEGER*4 from -2,147,483,648 to 2,147,483,647; 10! = 3628801
      IF(DD.EQ.17) IIDMAX = 5		
      IF(DD.EQ.16) IIDMAX = 5		! IIDMAX is in use within subroutive  STROVILNr
      IF(DD.EQ.15) IIDMAX = 5		! in order to not exceet the rande of INTEGER*4
      IF(DD.EQ.14) IIDMAX = 5		! in the IstrNr computation. 
      IF(DD.EQ.13) IIDMAX = 5
      IF(DD.EQ.12) IIDMAX = 5
      IF(DD.EQ.11) IIDMAX = 8
      IF(DD.EQ.10) IIDMAX = 9
      IF(DD.EQ. 9) IIDMAX = 9
      IF(DD.EQ. 8) IIDMAX = 10
      IF(DD.EQ. 7) IIDMAX = 11
      IF(DD.EQ. 6) IIDMAX = 11
      IF(DD.EQ. 5) IIDMAX = 13
      IF(DD.EQ. 4) IIDMAX = 15
      IF(DD.EQ. 3) IIDMAX = 19
      IF(DD.EQ. 2) IIDMAX = 30
!
!     ======:::::=====:::::=====:::::=====:::::======:::::=====:::::======:::::=====:::::=====:::::======:::::===== 
      WRITE(*,*) 'computation started    ::'
      ISTEPSexplMAX = 0			! counting max step-nr to explosion     - tape recording (whole inp-nr collection)
      Istepsmax  = 0			! counting max step-nr to any attractor - tape recording (whole inp-nr collection)   
      ISTDILeMAX = 0			! serves the max-len on the way to attr - tape recording (whole inp-nr collection)
      ISTDILeMIN = 1000			! serves the min-len on the way to attr - tape recording (whole inp-nr collection)
   10 CONTINUE				! THE OUTEST LOOP: input reading etc etc etc...			* is here 
      IQQ1 = 0				! ---------------  Use in counting 1s in the exploding pattern	* their ini-
      IQQ8 = 0				!                  Use in counting 8s in the exploding pattern	* tialization
      IQQ  = 0				! INITIALIZATION "pattern 109 present IN 4 steps"		* necessary?
      Lc   = 0   			! Used for termination control
      TrMAX= 0   			! This par. serves the max-len determination for attractor reaching
      TrMIN= 1000   			! This par. serves the min-len determination for attractor reaching
      CALL DATASTRV(IstrNr,T,W,L_data,L_dataq,Lc,LL,DD,I,NU,T1,T0,ENDf,IEXEC)
      CALL STROVILNr(IstrNr,DD,T,IIDMAX)
!     TERMINATION-f CHECK START
      IF((ENDf(78).NE.0).AND.(L_data.EQ.1)) call ENDfcheck(T,ENDf,Lc,IEXEC)
      IF(Lc.EQ.2) GO TO 89
!     TERMINATION-f CHECK END
      do iear = 1, 78
      datanr(iear) = t(iear)		! input-nr(1-78) copies T and remains the active 'input-number' for any use
      enddo				! CHECK POSSIBLE USES (NOT IN USE) OR DELETE!
!     
      IF(Lc.EQ.2) GO TO 89 		! in case of a input-list which came to its end #CHECK ITS USE#
      IF(IstrNr.EQ.0) GO TO 10		! handling of zero input-numbers
!       
      IF(T(78).EQ.0) WRITE(7,*) 'WARNING! T(78) equals zero!' ! COMMENT: what is the meaning in this line???
      DO 11 I=1,77			! START CHECK An input-NUMBER 
         IF(T(I).GE.DD) THEN							 
         WRITE(6,*) '! This number includes a non-permitted digit   IstrNr =', IstrNr
         Lc = 1    			!@! Lc = 1 or 2? check!
         ELSE									 
         ENDIF   								 
   11 CONTINUE
      IF(Lc.NE.0) GO TO 7  		! i.e. go for the next input-number  !@! is this check-line needed?
!  
      DO 2 I=1,78			! Start writing in the work-array VV
      VV(1,I) = T(I)
    2 CONTINUE
! 
      CALL LLEN(T)  
      IF(T(78).NE.VV(1,78)) WRITE(6,*) 'Warning: line 148 is necessary!', T(78), VV(1,78)   ! <-- check temporary!
      VV(1,78) = T(78)
!     if((L_disappear.EQ.0).AND.(L_appear.NE.2))  WRITE(6,*) ' The number to compute its Dig.Rev. is:'
      DO ITA =1, 78			! check that this 78 (instead 77) added lately is OK
      DRAFT(ITA) = VV(1,ITA)		! ok, this is for a --> character --> print
      ENDDO
      call STROVILNr(IstrNr,DD,DRAFT,IIDMAX)
      CALL HIGHERDEC (DRAFT,Ch_Array)			!  <--- perhaps? Ch_Array1 directly? avoid copy-to?
      DO ITi =1, 77					!  for printing with IDR format (1996) on screen
      Ch_Array1(ITi) = Ch_Array(ITi)			!  for printing with IDR format (1996) on screen
      ENDDO						!  for printing with IDR format (1996) on screen
      IF(IstrNr.EQ.999999999) I9999 = 1
! The line below is for normal printing of all input-numbers (along with the decimal INTEGER*4 IstrNr) 
      if((L_disappear.EQ.0).AND.(L_appear.NE.2))                         WRITE(6,1995) (Ch_Array(J), J=27,1,-1), IstrNr 
!	>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
! The line below IS for printing of only the first 11 lines 
      if((L_disappear.EQ.1).and.((IEXEC.le.11)))                         WRITE(6,1995) (Ch_Array(J), J=27,1,-1), IstrNr
!
      IF(L_appear.EQ.1)  WRITE(6,*) ' Sequence of steps:'  
!
!     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      L109 = 0				! Initialization of the control of the '109-pattern detection'
      I109 = 0				! Initialization of steps no till  the '109-pattern detection'
      IJJI = 0				! Another name for I109
      DO 6 I=2,1000,1			! *** THE MAIN 999-STEP LOOP [DO-6] ***
!                                             ----------------------------- 
      IQ(I) = 0 			! The every-step-explosive-attractor-check array initialization
      TrMAXex = TrMAX			! This par. serves the max-len determination
      TrMINex = TrMIN			! This par. serves the min-len determination
      TrMAX = MAX0(TrMAX, T(78))
      TrMIN = MIN0(TrMIN, T(78))
      IF(TrMAX.NE.TrMAXex) Imax = I-1 
      IF(TrMIN.NE.TrMINex) Imin = I-1 
      CALL IINV(T,V)			! The central operations: reversion // 
      CALL CMPP(T,V,KK)			! comparison // plus or minus // nr-size check    
         IF(KK.GE.2) THEN							 
         CALL SSUM(IstrNr,Lc,T ,V ,W,LL,DD,I)
         K = 1
         IF(Lc.EQ.3) GO TO 7        	! NEW COMMENT ???????? 
         ELSE									 
         CALL DDIF(Lc,T,V,W,LL,DD)
         K = 0
         ENDIF      								 
      KK = 0
         IF(L_appear.EQ.1) THEN						 
            CALL HIGHERDEC (W,Ch_Array)
            IF(K.EQ.0) THEN						
            WRITE(6,193) (Ch_Array(J), J=77,1,-1)
            ELSE								! 
            IF(Lc.EQ.0) WRITE(6,194) (Ch_Array(J), J=77,1,-1)        ! ??? WHY Lc? WHAT SERVES THAT? ! NEW COMMENT
            ENDIF						 
!  
         ELSE			 						 
	 ENDIF									 						
!
      DO 3 J=1,78				! The next-step-nr W is written in the work-array VV 
      VV(I,J) = W(J)				! and in the work-vector T
      T(J)    = W(J)
    3 CONTINUE
!
!     @@@@@@@@@@@@@@@@@@@@ here is the treatment detecting the base-line exploding pattern @@@@@@@@@@@@@@@@@@@@@@@@@@ 
!                   Ic_predexplx(i) [Lc=i+4] counts the population of cases with the '10999...000' x1,2,...
!                   --------------------------------------------------------------------------------------- 
      IY0 = VV(I,78)					! IN THE FOLLOWING LINES EXPLOSIVE BEHAVIOR IS PREDICTED        
      IF(IY0.LT.7) GO TO 44    				! OR POSTPONED for very low <7 values of step count (I)
 							! IY0: monitoring digital length 
      IQQ = 0						! INITIALIZATION "pattern 109 presence" IN 4 steps
      IF(T(IY0).EQ.1.AND.T(IY0-1).EQ.0.AND.T(IY0-2).EQ.(DD-1)) IQ(I) = 1
      MOM=MODULO(I,5)					! IQ(I)=1 means: the pattern 109 is present
!@    write(6,961) VV(I,78), I, IY0, MOM, IQ(I), L109, IJJI
!@961 format (18H%%EXTE%% VV(I,78)=, I3, 8H   I =  , I3, 8H   IY0 =, I3,  8H   MOM =, I3, 8H  IQ(I)=, I3, 8H  L109 =, I3, 8H  IJJI =, I3)
         IF(I.GE.10.AND.MOM.EQ.0) THEN 			! IQQ=1: 109 present for 4 conseq.steps	!@ if structute 12
         IF(IQ(I).EQ.1.AND.IQ(I-1).EQ.1.AND.IQ(I-2).EQ.1.AND.IQ(I-3).EQ.1.AND.IQ(I-4).EQ.1) IQQ=1
         IY2 = VV((I-2),78)
         IY4 = VV((I-4),78)
         IY6 = VV((I-6),78)
            IF(IQQ.EQ.1.AND.IY2.EQ.(IY0-1).AND.IY4.EQ.(IY2-1).AND.IY6.EQ.(IY4-1)) THEN		!@ if structute 13
            IF(L109.EQ.0) I109  = I		! L109  = 1 means: "109 & digital increase as in explosive pattern"
            L109  = 1				! I109  = I means: after Isteps the "109 pattern' has been established
            IJJI  = I109			! IJJI instead of I109 for continue using the existent structure
          ! IMPORTANT: THE IF (-2lines) locked IJJI=I109 value to the first I where pattern 109 were established!
!@    write(6,962) VV(I,78), I, IY0, MOM, IQ(I), L109, IJJI
!@962 format (18H%%EXTE%% VV(I,78)=, I3, 8H   I =  , I3, 8H   IY0 =, I3,  8H   MOM =, I3, 8H  IQ(I)=, I3, 8H  L109 =, I3, 8H  IJJI =, I3)
!      ====== here is the treatment detecting multiple exploding patterns =======  Note: Iw stands for counting x-explosive patterns 	
      	    if(L109.EQ.1.AND.(IY0.GE.DigLen.or.DigLendat.eq.0)) then					!@ if structute 14
            DO Iw= 1, IY0
            IF(VV(I,Iw).EQ.1) IQQ1 = IQQ1+1
            IF(VV(I,Iw).EQ.(DD-2)) IQQ8 = IQQ8+1
            IF((T(Iw).NE.0).AND.(T(Iw).NE.1).AND.(T(Iw).NE.(DD-2)).AND.(T(Iw).NE.(DD-1))) Iww = 1
            ENDDO
               IF((IQQ1.NE.IQQ8).OR.(Iww.EQ.1)) THEN
               Iww = 0
               IQQ = 9				!    this (IF NOT TRANSIENT) is the complete breakdown
               Lc  = 9				!    of the type of digital composition of the train of digits 
               Ic_predexplxx = Ic_predexplxx +1	!    in the 109.......00/99, i.e. unequal nr of 1s and 8s and/or
               GO TO 44		!@#@!		!    digits other than 0, 1,(DD-2) and (DD-1).
               ELSE
               ENDIF
!
               IF((L109.EQ.1).AND.(IQQ1.EQ.1)) THEN 
               Ic_predexplx1 = Ic_predexplx1 +1		
               Lc  = 5	
               ELSE
               ENDIF
!
               IF((L109.EQ.1).AND.(IQQ1.EQ.2)) THEN 
               Ic_predexplx2 = Ic_predexplx2 +1 	
               Lc  = 6	
               ELSE
               ENDIF
!
               IF((L109.EQ.1).AND.(IQQ1.EQ.3)) THEN 
               Ic_predexplx3 = Ic_predexplx3 +1 	
               Lc  = 7
               ELSE
               ENDIF	
!
               IF((L109.EQ.1).AND.(IQQ1.GE.4)) THEN 
               Ic_predexplx4 = Ic_predexplx4 +1 	
               Lc  = 8	
               ELSE
               ENDIF
!
            else										!@ if structute 14
            endif										!@ if structute 14
!	
            ELSE										!@ if structute 13
            ENDIF										!@ if structute 13
         ELSE											!@ if structute 12
         ENDIF											!@ if structute 12
!     ============ here the treatment detecting multiple exploding patterns ends ========
!     @@@@@@@@@@@@@@@@@@@@@@@@ 44 IS THE END POINT OF THE EXPLOSION CHECK  @@@@@@@@@@@@@@@@@@@
   44 CONTINUE
      IF(Lc.NE.0) GO TO 7
!
!     ^^^^^^^^^^^^^^^^^^^^^ Attractor Detection - Characterization ^^^^^^^^^^^^^^^^^	|||||||||||||||||||||||||| previous
!  
      AttrSTATE = 0                     	! In order to have AttrSTATE as indicator of attractor_existence
      DO 5 ISI = I-1, 1, -1			! THE NEXT IMPORTANT WORK-LOOP [DO-5] 
!                                         	-----------------------------------
      DO 8 JJ=1,78				! Search for repetition defining 
      X1(JJ) = VV(I,JJ)				! the limits of the attractor
      X2(JJ) = VV(ISI,JJ)
    8 CONTINUE    
!
      CALL CMPP(X1,X2,KK)
         IF(KK.EQ.2) THEN  ! HERE AN ATTRACTOR (K=2) IS DETECTED  *******					 
         KK = 0		   !THIS IS IN ORDER FOR KK TO BE REUSED! <???>
         IF(L_appear_attr.EQ.1)  WRITE(6,91) 
         IF(L_appear_attr.EQ.1)  WRITE(6,*) ' Attractor is the loop:'  
!		       
         JI = 0
         DO 4 ISA=ISI, I-1 !
         JI = JI+1
		 DO J=1, 78		    	! Here is written down the attractor!   				 
		 AV(JI,J)=VV(ISA,J)         	! -----------------------------------
		 ENDDO			    	! 
!
      DO ITA =1, 77				
      DRAFT(ITA) = AV(JI,ITA)			! ok, this is for a --> character --> print	
      ENDDO
      CALL HIGHERDEC (DRAFT,Ch_Array) 
         IF(L_appear_attr.EQ.1)  WRITE(6,196)  (Ch_Array(J), J=77,1,-1), JI        
    4    CONTINUE ! JI is the number of members of the attractor (JI =? ISA-1) 
!                   ----------------------------------------------------------          
         ISO=ISI-1
         ITA=I-ISI
         I$steps_before=ISO
         I$Attr_length=ITA		
!         
!        ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!        MANAGMENT OF THE REGISTRIES OF THE ATTRACTORS   
!        ---------------------------------------------
         III = 0		   ! III is about in/out the Old set of Attrs [-1:out/0:unknown/+1:in]
         IYI = 0		   ! IYI is about in/out the Met set of Attrs [-1:out/0:unknown/+1:in]
         INI = 0		   ! INI is about in/out the New set of Attrs [-1:out/0:unknown/+1:in]							
! [Met]  =============================================================================================
!        FOLLOWING LINES 27-27 CHECK IF THIS ATTR IS AN OLD ONE ALREADY MET HERE  
!        -----------------------------------------------------------------------
         DO 27 JY=1, Jmet	   ! Jmet is the nr of OLD Attrs already found (MET) here
         DO 26 JR=1, JI            ! Scan all lines of the present attractor
         DO JQ=1, 78               ! Production of vectors to compare					         
         Al2(JQ) = AV(JR,JQ)       ! from the attractor
         Al1(JQ) = Al1met(JY,JQ)   ! from the attr_registry of cases found in this input-set 
         ENDDO
         CALL LLEN(Al1)
!@#@     IF((Al1(78).EQ.0).OR.(Al2(78).EQ.0)) IRA3=IRA3+1
         CALL CMPP(Al1,Al2,KK)
            IF(KK.EQ.2)       THEN  									  	    
            IYI=1                  ! FOUND IN THE REUSER REGISTRY (ATTR IS AN OLD ONE, ALREADY FOUND HERE)
            III=1		   ! THIS IS SO BECAUSE WE KNOW THAT IT BELONGS IN THE OLD REGISTRY 
            Attrmet_ID(JY,4) = Attrmet_ID(JY,4) + 1  
            AttrSTATE= 201
            AttrID = Attrmet_ID(JY,2)
            ELSE											   	 
            ENDIF											   	 
   26    CONTINUE
   27    CONTINUE 
         IF(IYI.NE.1) IYI = -1											 
! [Old]  ===========================================================================
            IF(IYI.EQ.-1) THEN								               		 
!           FOLLOWING LINES 23-23 CHECK IF THIS ATTR IS INCLUDED IN THE old_registry
!           [in this case we also register it in the 'met' registry]
!           ------------------------------------------------------------------------
            DO 23 JP=1, JUC           ! Comparison with the known  attrs      
            DO 22 JR=1, JI            ! Scan all lines of the present attr 
            DO JQ=1, 78               ! Production of vectors to compare. 					 
            Al2(JQ) = AV(JR,JQ)       ! from the attractor         
            Al1(JQ) = Al1old(JP,JQ)   ! from the attr_registry
            ENDDO
            CALL LLEN(Al1)
            CALL CMPP(Al1,Al2,KK)
               IF(KK.EQ.2) THEN 							 
               III=1                  ! FOUND in the old_registry (so is put III=1)
               Jmet = Jmet+1          ! We put the Attr in the MET registry
               Attrmet_ID(Jmet,1) = Attrold_ID(JP,1) 
               if(Attrmet_ID(Jmet,1).ne.I$Attr_length) write(6,*) 'HUSTON, we have problem type (1)'
               Attrmet_ID(Jmet,2) = Attrold_ID(JP,2) 
               Attrmet_ID(Jmet,3) = IstrNr		!@@@@@@@@@@@@@@@@
               DO I33=1,27				!@@@@@@@@@@@@@@@@
               Ch_Array3(I33) = Ch_Array1(I33)		!@@@ met once @@@
               ENDDO					!@@@@@@@@@@@@@@@@
               Attrmet_ID(Jmet,4) = 1			!@@@@@@@@@@@@@@@@ 
               AttrID1 = Attrmet_ID(Jmet,2)
  !            WRITE (6,*) 'uuuuuuuuu  AttrID1 = ', AttrID1
               DO I333=1,27
               Ch_ArrayM(AttrID1,I333) = Ch_Array1(I333)
               ENDDO
               AttrSTATE = 101
               AttrID = Attrold_ID(JP,2)
               IYI = 1		      ! Which means put in the MET registry (change of status)
               DO JQ=1, 78											 
               Al1met(Jmet,JQ) = Al1old(JP,JQ)! <-- completion of the 'met' registry
               ENDDO
               ELSE												 
               ENDIF												 
   22       CONTINUE
   23       CONTINUE
            ELSE												 
            ENDIF												 
            IF(III.NE.1) III = -1										 
!
! [New]  =========================================================================== 
            IF(III.NE.1) THEN 											 
            DO 21 JN=1, JUN
            DO 20 JR=1, JI   
            DO JQ=1, 78               ! Production of vectors to compare. 					 
            Al2(JQ) = AV(JR,JQ)       ! from the attractor
            Al1(JQ) = Al1new(JN,JQ)   ! from the new-attr_registry of NEW cases found in this input-set 
            ENDDO 
            CALL CMPP(Al1,Al2,KK)
               IF(KK.EQ.2)   THEN   										 
               INI = 1
               AttrSTATE = 202 
               AttrID = Attrnew_ID(JN,2)
               Attrnew_ID(JN,4) = Attrnew_ID(JN,4) + 1 
               ELSE												 
               ENDIF												 
   20       CONTINUE
   21       CONTINUE  
         IF(INI.NE.1) INI = -1											 
            IF(INI.EQ.-1) THEN											 					                	!11[11.1]	            
            JUN = JUN+1
            DO JQ=1, 78												 
            Al1new (JUN,JQ) = X1(JQ)
            ENDDO
            AttrSTATE = 201 
            AttrID = Attrnew_ID(JN,2)
            Attrnew_ID(JUN,1) = I$Attr_length 
            Attrnew_ID(JUN,2) = Kadd+JUN 
            AttrID = Attrnew_ID(JUN,2)
!           write(6,*) '# First Attractor apparition! Attrnew_ID(2) =', Attrnew_ID(JUN,2) !FOR EMERGENCY PURPOSES!
            Attrnew_ID(JUN,3) = IstrNr
!           WRITE (6,*) 'UUUUUUUUUU  AttrID = ', AttrID
            DO I333=1,27
            Ch_ArrayN(AttrID,I333) = Ch_Array1(I333)
            ENDDO
            Attrnew_ID(JUN,4) = 1
! ATTENTION:TEMPORARY STOPPED SCREEN PRINTING FOR TIME-READING PURPOSES (BELOW ! ---------)
            write (*, 1996)  Attrnew_ID(JUN,2), Attrnew_ID(JUN,1), IEXEC, (Ch_Array1(J), J=27,1,-1)
       1996 format(18H new attractor nr:, I5, 17H   Nr of members:, I3, 27H   input-number enumeration:, I13, 16H   input-number: , 27(A1))
            if (Attrnew_ID(JUN,2).ge.11000) GO TO 89
            CALL HIGHERDEC (datanr,Ch_Array) 
            ELSE												 
            ENDIF												 
         ELSE													 
         ENDIF													 
         GO TO 7   
!        
         ELSE													 
         ENDIF													 
    5 CONTINUE				! *** End of the Search for Attractors ***
!     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    6 CONTINUE 				! *** THE MAIN 999-STEP LOOP [DO-6] ***
!     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^    
!  
!     --- *** END OF THE MAIN 999-STEP LOOP *** ---
      Ic_Undecided = Ic_Undecided + 1
      Lc = 11		! (Ic_Und=1)  	Lc = 11 <---> UNDECIDED!
!
    7 CONTINUE
!
      do ii=1, 78
      t(ii)=0
      enddo
!
         IF(Lc.EQ.5) THEN      			
         Ic_Expl = Ic_Expl + 1
         CALL HIGHERDEC (W,Ch_Array) 
!        Important Note: The value of the counted path to the '109...' pattern, has to be reduced by 5  due to
!        --------------  the used algorithm.  This -5 is done after simple inspection 'by eye', if a collection 
!        runs resulting to unbound expantion. Example of what we define as point of emergence is the following:
! - 000000000000000003bb42eca33b : 
! + 0000000000000000100000deeeee :   we decide, somewhat arbitrarily, to put the exact point of emergence after 
! + 00000000000000010eeeede00000 :   three '10y...' and two concequtive increases of digital length by one. The
! - 00000000000000010eedeee000ee :   sequence +/-/+/-... has to have been established. 
! + 0000000000000010eeeeeede0000 : <--- this is considered as point of emergence. 
! - 0000000000000010ee00eede00ee : 
! + 000000000000010eeeeeede00000 : 
!     ........................
         IJJI = IJJI - 5
         if(L_disappear.EQ.0)  WRITE( 6,1997) (Ch_Array1(I), I=27,1,-1), IJJI, (Ch_Array(I), I=77,1,-1)  !, Lc      
         if(L_disappear.EQ.0)  WRITE(11,1997) (Ch_Array1(I), I=27,1,-1), IJJI, (Ch_Array(I), I=77,1,-1)  !, Lc       
         IJJIMEAN = IJJIMEAN + IJJI
         ELSE
         ENDIF
!
         IF(Lc.EQ.6) THEN   			
         Ic_Expl = Ic_Expl + 1
         CALL HIGHERDEC (W,Ch_Array) 
         if(L_disappear.EQ.0)  WRITE( 6,1997) (Ch_Array1(I), I=27,1,-1), IJJI, (Ch_Array(I), I=77,1,-1)  !, Lc       
                               WRITE(12,1997) (Ch_Array1(I), I=27,1,-1), IJJI, (Ch_Array(I), I=77,1,-1)  !, Lc        
         IJJIMEAN = IJJIMEAN + IJJI
         ELSE
         ENDIF
!
         IF(Lc.EQ.7) THEN        		
         Ic_Expl = Ic_Expl + 1
         CALL HIGHERDEC (W,Ch_Array) 
         if(L_disappear.EQ.0)  WRITE( 6,1997) (Ch_Array1(I), I=27,1,-1), IJJI, (Ch_Array(I), I=77,1,-1)  !, Lc       
                               WRITE(13,1997) (Ch_Array1(I), I=27,1,-1), IJJI, (Ch_Array(I), I=77,1,-1)  !, Lc        
         IJJIMEAN = IJJIMEAN + IJJI         
         ELSE
         ENDIF
!
         IF(Lc.EQ.8) THEN        		
         Ic_Expl = Ic_Expl + 1
         CALL HIGHERDEC (W,Ch_Array) 
         if(L_disappear.EQ.0)  WRITE( 6,1997) (Ch_Array1(I), I=27,1,-1), IJJI, (Ch_Array(I), I=77,1,-1)  !, Lc       
                               WRITE(14,1997) (Ch_Array1(I), I=27,1,-1), IJJI, (Ch_Array(I), I=77,1,-1)  !, Lc        
         IJJIMEAN = IJJIMEAN + IJJI
         ELSE
         ENDIF
!
         IF(Lc.EQ.9) THEN        		
         Ic_Expl = Ic_Expl + 1
         CALL HIGHERDEC (W,Ch_Array) 
         if(L_disappear.EQ.0)  WRITE( 6,1997) (Ch_Array1(I), I=27,1,-1), IJJI, (Ch_Array(I), I=77,1,-1)  !, Lc       
                               WRITE(19,1997) (Ch_Array1(I), I=27,1,-1), IJJI, (Ch_Array(I), I=77,1,-1)  !, Lc        
         IJJIMEAN = IJJIMEAN + IJJI
         ELSE
         ENDIF
!
         IF(Lc.ge.4) THEN  
         ISTEPSexplMAX1 = ISTEPSexplMAX			! This par. serves the max-pred-expl step-nr determination 
         ISTEPSexplMAX  = MAX0(IJJI, ISTEPSexplMAX)							
            IF(ISTEPSexplMAX.GT.ISTEPSexplMAX1) THEN
            DataNr1 = IstrNr    
            WRITE(7 ,1717) ISTEPSexplMAX, (Ch_Array1(I), I=27,1,-1)
 1717 FORMAT(25H MaxSteps till PREDexpl.:, I4, 26H           For input-nr:  ,  27(A1))
            ELSE
            ENDIF         
         CALL HIGHERDEC (W,Ch_Array)
         IQQ=0
         ELSE		
         ENDIF		
!         
      I$MaxLen  = TrMAX
      I$atstep1 = Imax
      I$MinLen  = TrMIN
      I$atstep2 = Imin
         CALL HIGHERDEC (T,Ch_Array) 			! moved here. Any use?
         IF((AttrSTATE.NE.0)) THEN			! removed id Lc=4
!        write(7,*) 'uuuuuuu', Lc, AttrSTATE
!        if(L_disappear.EQ.0) WRITE( 6,1998) (Ch_Array1(I), I=27,1,-1), (Ch_Array(IA), IA=77,1,-1), I    			
!        if(L_disappear.EQ.0) WRITE(11,1998) (Ch_Array1(I), I=27,1,-1), (Ch_Array(IA), IA=77,1,-1), I             
         if(L_disappear.EQ.0.AND.AttrSTATE.NE.0) WRITE( 6, 99)  (Ch_Array1(IA), IA=27,1,-1), AttrSTATE, AttrID, I$steps_before, I$Attr_length, I$MaxLen, I$MinLen, I$atstep1, I$atstep2   
         if(I$MinLen.le.MaxLenStore) WRITE(20,898) (Ch_Array1(IA), IA=77,1,-1)
!       >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> 							
         ISTDILeMAX1 = ISTDILeMAX 
         ISTDILeMAX  = MAX0(TrMAX, ISTDILeMAX)
            IF(ISTDILeMAX.GT.ISTDILeMAX1) THEN
            WRITE(7,1967) ISTDILeMAX,  (Ch_Array1(J), J=27,1,-1)
 1967 FORMAT(25H MAX DIGITAL LENGTH T.A.:, I12, 18H   For INPUT-Nr:  , 27(A1))
            ELSE
            ENDIF
!	>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> 	
         ISTDILeMIN1 = ISTDILeMIN 
         ISTDILeMIN  = MIN0(TrMIN, ISTDILeMIN)
            IF(ISTDILeMIN.LT.ISTDILeMIN1) THEN
            WRITE(7,1968) ISTDILeMIN,  (Ch_Array1(J), J=27,1,-1)
 1968 FORMAT(25H MIN DIGITAL LENGTH T.A.:, I14, 16H For INPUT-Nr:  , 27(A1))
            ELSE
            ENDIF           
!	>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> 
         ISTEPSMAX1 = ISTEPSMAX						 
         ISTEPSMAX = MAX0(I$steps_before, ISTEPSMAX)
            IF(ISTEPSMAX.GT.ISTEPSMAX1) THEN
            IstrNr1 = IstrNr 
            WRITE(7,1966) ISTEPSMAX,  (Ch_Array1(J), J=27,1,-1)
 1966 FORMAT(25H MAX OF STEPS TILL ATTR.:, I9, 21H      For INPUT-Nr:  , 27(A1))
            ELSE
            ENDIF
!	>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> 
         IF(AttrSTATE.NE.0) Isteps = Isteps + I$steps_before   !ATTENTION IT IS ADDED BECAUSE ALL inputNRS WERE COUNTED!
         IF(AttrSTATE.NE.0) Istepcount = Istepcount + 1        !ATTENTION IT IS ADDED BECAUSE ALL inputNRS WERE COUNTED! 
         ELSE
         ENDIF
      Lc=0
      IF(L_appear.NE.2)  WRITE(6,*) '-----------------------------------------------'    
!
         IF(L_IEXECmax.NE.0)   THEN
	     IF(IEXEC.GE.L_IEXECmax) GO TO 89
		 ELSE
		 ENDIF
      GO TO 10 
!     ======:::::=====:::::=====:::::=====:::::======:::::=====:::::======:::::=====
!   
   89 CONTINUE
!     ------------mean step nr till reach an attractor ------------------
      IF(Istepcount.EQ.0) THEN
      R_mean_steps = 0
      ELSE
      R_mean_steps = real(Isteps) / real(Istepcount)
      ENDIF
      IF(Ic_Expl.EQ.0) THEN
      R_mean_steps_ExplPred = 0
      ELSE
      R_mean_steps_ExplPred = real(IJJIMEAN) / real(Ic_Expl) 
      ENDIF
!     ------------------------------------------------------------------------
      WRITE( *,*) 'computation ended      ::    processed input numbers:', iexec
      WRITE( 6,*) '..............................................................................'     
      WRITE( 6, *)  'Totally processed input-numbers =', IEXEC
      WRITE( 6, *)  'Mean Step Nr till Attr. =', R_mean_steps,'Total at/cted cases =', Istepcount
      WRITE( 6, *)  'Global Max steps till Attr.=',    ISTEPSMAX, ' (for the input-number see the "tape")'
      WRITE( 6, *)  'Predicted Exploding (109...& +1dig/2-steps) Tot.nr of cases =', Ic_Expl
      WRITE( 6, *)  'Mean Step Nr till establish. the  >>  pattern = ', R_mean_steps_ExplPred
      WRITE( 6, *)  'Glob.Max steps till establ.  the  >>  pattern =', ISTEPSexplMAX, ' (see the "tape")'
      WRITE( 6, *)  'Undecided (no attractor after 1000 steps)   Tot.nr of cases =', Ic_Undecided      
      WRITE( 7,*) '...............................................................................'     
      WRITE( 7, *)  'Totally processed input-numbers =', IEXEC
      WRITE( 7, *)  'Mean Step Nr till Attr. =', R_mean_steps,'Total at/cted cases =', Istepcount
      WRITE( 7, *)  'Global Max steps till Attr.=',    ISTEPSMAX, ' (for the input-number see above)'
      WRITE( 7, *)  'Predicted Exploding (109...& +1dig/2-steps) Tot.nr of cases =', Ic_Expl
      WRITE( 7, *)  'Mean Step Nr till establish. the  >>  pattern = ', R_mean_steps_ExplPred
      WRITE( 7, *)  'Glob.Max steps till establ.  the  >>  pattern =', ISTEPSexplMAX, ' (see above)'
      WRITE( 7, *)  'Undecided (no attractor after 1000 steps)   Tot.nr of cases =', Ic_Undecided      
!
      WRITE(11, *)  'Predicted Exploding (expl. motifx1) Tot.nr. cases =', Ic_predexplx1
      WRITE(12, *)  'Predicted Exploding (expl. motifx2) Tot.nr. cases =', Ic_predexplx2
      WRITE(13, *)  'Predicted Exploding (expl. motifx3) Tot.nr. cases =', Ic_predexplx3
      WRITE(14, *)  'Predicted Exploding (expl.motive>3) Tot.nr. cases =', Ic_predexplx4
      WRITE(19, *)  'Pred.Expl.(motif non-typical:nrs of 8s=/1s)T.n.cs =', Ic_predexplxx
      WRITE( 6, *)  'Predicted Exploding (expl. motifx1) Tot.nr. cases =', Ic_predexplx1
      WRITE( 6, *)  'Predicted Exploding (expl. motifx2) Tot.nr. cases =', Ic_predexplx2
      WRITE( 6, *)  'Predicted Exploding (expl. motifx3) Tot.nr. cases =', Ic_predexplx3
      WRITE( 6, *)  'Predicted Exploding (expl.motive>3) Tot.nr. cases =', Ic_predexplx4
      WRITE( 6, *)  'Pred.Expl.(motif non-typical:nrs of 8s=/1s)T.n.cs =', Ic_predexplxx
      WRITE( 7, *)  'Predicted Exploding (expl. motifx1) Tot.nr. cases =', Ic_predexplx1
      WRITE( 7, *)  'Predicted Exploding (expl. motifx2) Tot.nr. cases =', Ic_predexplx2
      WRITE( 7, *)  'Predicted Exploding (expl. motifx3) Tot.nr. cases =', Ic_predexplx3
      WRITE( 7, *)  'Predicted Exploding (expl.motive>3) Tot.nr. cases =', Ic_predexplx4
      WRITE( 7, *)  'Pred.Expl.(motif non-typical:nrs of 8s=/1s)T.n.cs =', Ic_predexplxx
!
      WRITE( 7, *)  '**************** new (absent from the old-repository)  attractors *************'
      DO 126 IYN=1, JUN
      DO ITA =1, 77
      DRAFT(ITA) = Al1new(IYN,ITA)			! ok, this is for a --> character --> print
      ENDDO
      IQI=Attrnew_ID(IYN,2)
!@@@  write(*,*) 'oooooooo     (new attrs)   IQI =',  iqi, '    IYN = ',  IYN
      CALL HIGHERDEC (DRAFT,Ch_Array)
      WRITE( 9,198)  (Ch_Array(J), J=77,1,-1),   Attrnew_ID(IYN,1), Attrnew_ID(IYN,2), (Ch_ArrayN(IQI,I), I=27,1,-1), Attrnew_ID(IYN,4) 
      iALLATTRSMET = iALLATTRSMET + 1
      WRITE( 7,198)  (Ch_Array(J), J=77,1,-1),   Attrnew_ID(IYN,1), Attrnew_ID(IYN,2), (Ch_ArrayN(IQI,I), I=27,1,-1), Attrnew_ID(IYN,4)
  126 CONTINUE
!
      WRITE( 7, *)  '********************* re-used old (already known) attrectors ******************'
      DO 125 IYI=1, Jmet
      DO ITA =1, 77
      DRAFT(ITA) = Al1met(IYI,ITA)			! ok, this is for a --> character --> print
      ENDDO
      IQI=Attrmet_ID(IYI,2)
!@@@  write(*,*) 'oooooooo  (reused attrs)   IQI =',  iqi, '    IYI = ',  IYI
      CALL HIGHERDEC (DRAFT,Ch_Array)
      WRITE(10,198)  (Ch_Array(J), J=77,1,-1),   Attrmet_ID(IYI,1), Attrmet_ID(IYI,2), (Ch_ArrayM(IQI,I), I=27,1,-1), Attrmet_ID(IYI,4)
      iALLATTRSMET = iALLATTRSMET + 1  
      WRITE( 7,198)  (Ch_Array(J), J=77,1,-1),   Attrmet_ID(IYI,1), Attrmet_ID(IYI,2), (Ch_ArrayM(IQI,I), I=27,1,-1), Attrmet_ID(IYI,4)  
  125 CONTINUE
!
      WRITE( 6, *)  'All met (reused & new) att/tors =', iALLATTRSMET
      WRITE( 7, *)  'All met (reused & new) att/tors =', iALLATTRSMET
      IF(I9999.EQ.1) WRITE(6,*) 'Input-nr in decimal = "999999999" denotes that its value exceeds the 1 billion' 
!    
      CLOSE (5)
      CLOSE (6)
      CLOSE (7)
      CLOSE (8)
      CLOSE (9)
      CLOSE (10)      
      CLOSE (11)   
      CLOSE (12)      
      CLOSE (13)  
      CLOSE (14)  
      CLOSE (19) 
      CLOSE (20) 
  898 FORMAT(X,X,X,77(A1),2(I6)) 
  899 FORMAT(X,X,X,77(A1),2(I6),2X,27(A1),I9) 
   90 FORMAT(1H )
   91 FORMAT(1H ) 
   92 FORMAT(77(I1))
   93 FORMAT(3H - ,77(I1))
   94 FORMAT(3H + ,77(I1))
   95 FORMAT(3H * ,77(I1))
   96 FORMAT(3H * ,77(I1), I7)
   98 FORMAT(X,X,X,77(I1),4(I12)) 
   88 FORMAT(3H @ ,77(I1),4(I12))
   99 FORMAT(12H Input-Nr = ,27(A1),4H | #,I3,11H | Attr ID:, I4,15H | Steps till =, I4,13H | Attr-len.=,I4,20H | Max/Min dig.len.=,I3,2H /,I3,19H | At stepMax/Min =,I3,2H /,I3)
  193 FORMAT(3H - ,77(A1),3H : )
  194 FORMAT(3H + ,77(A1),3H : )
  195 FORMAT(3H * ,27(A1))
 1995 FORMAT(12H Input-Nr = ,27(A1), 18H   (Decimal form: , I9,3H)  )
  196 FORMAT(3H * ,77(A1), I7)
  198 FORMAT(X,X,X,77(A1),2(I6),2X,27(A1),I9) 
 1997 FORMAT(12H Input-Nr = ,27(A1),30H | Step where "109.." appears:, I4, 2H @,77(A1)) !,3H  @,I2
 1998 FORMAT(12H Input-Nr = ,27(A1),3H | ,77(A1),49H # No attrct.found, this is the DigInvStep  # No:,I3)
!1999 FORMAT(12H Input-Nr = ,27(A1),24H | No attrct.DInvStep No,I3,9H/1000: $ ,77(A1))
 2000 FORMAT(35H THE FIRST OCCURRENCE OF NEW ATTR.[,I3,21H] IS FOR inputNUMBER:,77(A1))
!
      STOP
      END
! 
!     ===============================================================================
!
      SUBROUTINE ENDfcheck(T,ENDf,Lc,IEXEC)
!     For constantly increasing input-data set, check of reaching the end data-number
      INTEGER*1  T(78), ENDf(78)
      INTEGER  Lc
      call CMPP(T,ENDf,KK)
         IF(KK.LT.2) THEN
         Lc = 2
         IEXEC = IEXEC-1
         ELSE
         ENDIF
      RETURN
      END
! 
!     ===============================================================================
!
      SUBROUTINE DATASTRV(IstrNr,T,W,L_data,L_dataq,Lc,LL,DD,ICC,NU,T1,T0,ENDf,IEXEC)
!     Part of the program including data-reading and data-computing. 
      CHARACTER*1 Ch_T(78), Ch_T0(78), Ch_T1(78), Ch_ENDf(78)
      INTEGER*1  T(78), T1(78), W(78), T0(78), ENDf(78)
      INTEGER  NU,L_data,Lc,LL,DD,I
      INTEGER*4 IEXEC, L_dataq
      real :: r
!
!        ABOUT ICC: It is I (main program). ITS ROLE IS UNCLEAR!							
         IF(NU.EQ.0) THEN					! Here the tho first input IDR-format numbers are 
         READ(5,92,ERR=88,END=88) (Ch_T0(I)  , I=77,1,-1) 	! read from data-file. T0 equals 0 or to a constant, 
         READ(5,92,ERR=88,END=88) (Ch_T1(I)  , I=77,1,-1) 	! for composing with T1=1 additively trains of 
         READ(5,92,ERR=88,END=88) (Ch_ENDf(I), I=77,1,-1) 	! input numbers (CASE II).  
         CALL CEDREHGIH (T0,Ch_T0)          			! 
         CALL CEDREHGIH (T1,Ch_T1)				!  
         CALL CEDREHGIH (ENDf,Ch_ENDf)				!  

         IstrNr0 = 0
         IstrNr1 = 0
         CALL STROVILNr(IstrNr0,DD,T0  ,IIDMAX)		! NU = 0 holds for reading T0 & T1, while
         CALL STROVILNr(IstrNr1,DD,T1  ,IIDMAX)		! NU = 1 leads to reading the list of input numbers
         CALL STROVILNr(IstrNre,DD,ENDf,IIDMAX)		! from the data-file 
         NU = 1  					
         WRITE(6,92) (Ch_T0(I)  , I=77,1,-1) 
         WRITE(6,92) (Ch_T1(I)  , I=77,1,-1)
         WRITE(6,92) (Ch_ENDf(I), I=77,1,-1)
         WRITE(7,92) (Ch_T0(I)  , I=77,1,-1) 
         WRITE(7,92) (Ch_T1(I)  , I=77,1,-1)
         WRITE(7,92) (Ch_ENDf(I), I=77,1,-1)
         WRITE(7,*) '...............................................................................' 
         IF(IstrNr0.NE.0.AND.L_data.GT.0.AND.L_dataq.NE.0) WRITE(6,*) 'Correction: 1st input-nr is as indicated in the next line'
         WRITE(6,*) '...............................................................................' 
         ELSE
         ENDIF
!
!     CASE I
!     ------
         IF(L_data.EQ.0.AND.L_dataq.EQ.0) THEN 		! Here (L_data&q = 0) input-numbers are taken from  
         READ(5,92,ERR=88,END=88) (Ch_T(I), I=77,1,-1) 	! a list in the INPUT-NR PART OF THE DATA FILE. 
         CALL CEDREHGIH (T,Ch_T)			! Note that character arrays are read, so numbers 
         CALL STROVILNr(IstrNr,DD,T,IIDMAX)		! from any numeral system may be considered. 
         IF(IstrNr.EQ.0) GO TO 99			! <--- This solves the problem of a last input-nr=0.
         IEXEC = IEXEC + 1  
   99    GO TO 9							
         ELSE
         ENDIF
!
!     CASE II
!     -------
         IF(L_data.GE.0.AND.L_dataq.NE.0) THEN 		! Here (L_data>=0,L_dataq>0) data numbers are  
         CALL SSUM(IstrNr,Lc,T0,T1,W,LL,DD,I)	! generated additively. See above and in the  
         IEXEC = IEXEC + 1				! guidelines.
         IF(Lc.EQ.3) WRITE(6,*) ' ##### ERROR! INITIAL NUMBER EXCEEDING THE 77-DIGITS LENGTH.'          
         DO I=1, 78										
         T(I)  = W(I)
         T0(I) = W(I)  !For the next application ???
         ENDDO
            IF(T(L_dataq+1).NE.0) THEN			! correction of the printed IEXEC value
            IEXEC = IEXEC - 1
            GO TO 88 
            ELSE
            ENDIF
         ELSE
         ENDIF
!   
!     CASE III
!     --------
       IF(L_data.LT.0) THEN 				! Here input-numbers are composed randomly  
       L_dataMinus = -L_data 				
       IEXEC = IEXEC + 1				
!      --@--@--@--@--@--@--@--@--@--@--@--@--@--@--@--@--@--@--@--@--@--@--@--@--@--@--@--@--@--@--@--@--@       
       DO J=1, L_dataMinus				! start input-rn composition
       call random_number(r) 
!      write(7,*) 'randomness check:', r                ! <--- for randomness check
       R10=10*R
       IR=INT(R10)
       IREX2=IREX1					! for use in the case DD > 10      
       IREX1=IR						! for use in the case DD > 10      
    10    IF(IR.GE.DD) THEN 				! manipulation of non-decimal cases for DD < 10
          IR = IR-DD
          GO TO 10
          ELSE
          ENDIF
          IF(DD.GT.10) THEN 				! manipulation of non-decimal cases for DD > 10
          IMIMI=DD-10
          IF(IREX2.LE.4)    IR = IR+IMIMI
          ELSE
          ENDIF    
       T(J) = IR
       ENDDO 
!     --@--@--@--@--@--@--@--@--@--@--@--@--@--@--@--@--@--@--@--@--@--@--@--@--@--@--@--@--@--@--@--@--@
!      write(10,*) '............................', IEXEC, IstrNr	<--- for randomness check
  17         IF(T(L_dataMinus).EQ.0) THEN
             call random_number(r) 
             R=10*R
             IR=INT(R) 
    101         IF(IR.GE.DD) THEN 
                IR = IR-DD
                GO TO 101
                ELSE
                ENDIF
             IF(IR.EQ.0) GO TO 17
             T(L_dataMinus) = IR
             ELSE
             ENDIF
!
       DO J=L_dataMinus+1, 77
       T(J) = 0
       ENDDO
!
            IF (T0(78).NE.0) THEN
            CALL SSUM(IstrNr,Lc,T0,T,W,LL,DD,I)
            DO IJ = 1, 77
            T(IJ) = W(IJ)
            ENDDO
            ELSE
            ENDIF
       CALL LLEN(T)
            IF(IEXEC.GT.L_dataq) THEN
            IEXEC = IEXEC -1
            GO TO 88
            ELSE
            ENDIF
       ELSE
       ENDIF
!
      GO TO 9
   88 Lc = 2				! this leads to an and the run of the IRD prog as the input nrs ended. 
    9 RETURN  
   92 FORMAT(3H   ,77(A1))
   95 FORMAT(3H   ,77(I1))
      END
!  
!     ===============================================================================  
!  
      SUBROUTINE STROVILNr(IstrNr,DD,T,IIDMAX)
!     A DD-based integer (< 10exp9) written in decimal 
      INTEGER*1 T(78)    
      INTEGER*4 IstrNr
      INTEGER   DD, IIDMAX
         CALL LLEN(T)
         IF(T(78).GE.10) THEN
         IstrNr = 999999999
         ELSE
         IstrNr = 0 
         DO I=1, (IIDMAX+1)
         IstrNr=IstrNr+T(I)*DD**(I-1)
         ENDDO
         ENDIF
      RETURN
      END
!  
!     =============================================================================== 
!  
      SUBROUTINE LLEN(O)
!     Writted the number of digits in the 78th element
      INTEGER*1 O(78)
      O(78)=0
      DO I=1,77
      IF(O(I).NE.0) O(78)=I
      ENDDO
      RETURN  
      END
!  
!     ===============================================================================
!  
      SUBROUTINE IINV(A,B)
!     Digital reversion of an up to 77dig. number
      INTEGER*1  A(78), B(78)
      N78 = A(78)
      DO 1 I=1,N78
      B(N78-I+1) = A(I)
    1 CONTINUE
      DO 2 I=(N78+1),77
      B(I) = 0
    2 CONTINUE
      CALL LLEN(B)
      RETURN
      END
!  
!     ===============================================================================
!
      SUBROUTINE SSUM(IstrNr,Lc,P1,P2,S,LL,DD,JJ)
!     sum of two (up to 77dig.) integers   [OPTIMIZED]
      INTEGER DD, LL, JJ, Lc, I1, I2, EX77
      INTEGER*1 P1(78), P2(78), S(78)
      Lc = 0
      LL = 0
      I1 = P1(78)
      I2 = P2(78)
      EX77 = MAX0(I1,I2) + 1         !summation
      IF(EX77.GE.78) EX77=77
      DO 1 I=1, EX77
      IS = P2(I)+P1(I)+LL
      LL = 0
      S(I)=MOD(IS,DD)
      IF(IS.GE.DD) LL=1
    1 CONTINUE
      DO 2 I=EX77+1, 77
      S(I)=0
    2 CONTINUE
         IF(LL.EQ.1) THEN
         Lc = 3
         ELSE
         CALL LLEN(S)
         ENDIF
   95 FORMAT(3H * ,77(I1))
      RETURN
      END      
!  
!     =============================================================================== 
!  
      SUBROUTINE DDIF(Lc,P1,P2,S,LL,DD)
!     subtraction of two (up to 77dig.) integers  [OPTIMIZED]
      INTEGER  Lc, DD, LL, I1, I2, EX77
      INTEGER*1 P1(78), P2(78), S(78)
      LL = 0
      I1 = P1(78)
      I2 = P2(78)
      EX77 = MAX0(I1,I2) + 1         !subtraction
      IF(EX77.GE.78) EX77=77
      DO 1 I=1, EX77
      IS = P1(I)-P2(I)-LL
      LL = 0
      IF(IS.LT.0) THEN
      IS = IS+DD
      LL = 1
      ELSE
      ENDIF
      S(I) = IS
    1 CONTINUE
      DO 2 I=EX77+1, 77
      S(I)=0
    2 CONTINUE
      IF(LL.EQ.1) THEN
      Lc = 1    
      S(78) = 99 !@@@@@@@
      WRITE(6,*) ' Impossible subtraction!'
      ELSE
      CALL LLEN(S)
      ENDIF
      RETURN
      END      
!  
!     ===============================================================================
!  
      SUBROUTINE CMPP(P1,P2,KK)
!     Comparison of two (up to 77dig.) integers  [OPTIMIZED]
      INTEGER*1  P1(78), P2(78)
      KK = 0
!         *********************
!         *P1>P2 -------> KK=1*
!         *P1=P2 -------> KK=2*
!         *P1<P2 -------> KK=3*
!         *********************
!
      IF(P1(78).EQ.P2(78)) THEN
!
      JIJ = P1(78)  
      DO 1 JJ=JIJ, 1, -1
         IF(P1(JJ).EQ.P2(JJ)) THEN
         KK=2
         GO TO 1
         ELSE
            IF(P1(JJ).GT.P2(JJ)) THEN
            KK=1
            ELSE
            KK=3
            ENDIF
         GO TO 9 
         ENDIF
    1 CONTINUE
!  
      ELSE 
         IF(P1(78).GT.P2(78)) THEN
         KK=1
         ELSE
         KK=3
         ENDIF
      ENDIF
    9 RETURN
      END 
!  
!     ===============================================================================
! 
      SUBROUTINE HIGHERDEC (Int_Array,Ch_Array)
!     From number-digits to character ( I-->A )
      CHARACTER*1 Ch_Array(77)
      INTEGER*1  Int_Array(78)
!      
      DO I = 1,77
      IF(Int_Array(I).EQ. 0) Ch_Array(i) = '0'
      IF(Int_Array(I).EQ. 1) Ch_Array(i) = '1'
      IF(Int_Array(I).EQ. 2) Ch_Array(i) = '2'
      IF(Int_Array(I).EQ. 3) Ch_Array(i) = '3'      
      IF(Int_Array(I).EQ. 4) Ch_Array(i) = '4'
      IF(Int_Array(I).EQ. 5) Ch_Array(i) = '5'
      IF(Int_Array(I).EQ. 6) Ch_Array(i) = '6'
      IF(Int_Array(I).EQ. 7) Ch_Array(i) = '7'    
      IF(Int_Array(I).EQ. 8) Ch_Array(i) = '8'
      IF(Int_Array(I).EQ. 9) Ch_Array(i) = '9'
      IF(Int_Array(I).EQ.10) Ch_Array(i) = 'a'
      IF(Int_Array(I).EQ.11) Ch_Array(i) = 'b'      
      IF(Int_Array(I).EQ.12) Ch_Array(i) = 'c'
      IF(Int_Array(I).EQ.13) Ch_Array(i) = 'd'
      IF(Int_Array(I).EQ.14) Ch_Array(i) = 'e'
      IF(Int_Array(I).EQ.15) Ch_Array(i) = 'f'              
      IF(Int_Array(I).EQ.16) Ch_Array(i) = 'g'
      IF(Int_Array(I).EQ.17) Ch_Array(i) = 'h'       
      ENDDO
!
      RETURN
      END  
!     ===============================================================================
! 
      SUBROUTINE CEDREHGIH (Int_Array,Ch_Array)
!     From character to number-digits ( A-->I )
      CHARACTER*1 Ch_Array(77)
      INTEGER*1  Int_Array(78)
!      
      DO I = 1,77
      IF(Ch_Array(i).EQ.'0') Int_Array(I) =  0
      IF(Ch_Array(i).EQ.'1') Int_Array(I) =  1 
      IF(Ch_Array(i).EQ.'2') Int_Array(I) =  2
      IF(Ch_Array(i).EQ.'3') Int_Array(I) =  3     
      IF(Ch_Array(i).EQ.'4') Int_Array(I) =  4 
      IF(Ch_Array(i).EQ.'5') Int_Array(I) =  5 
      IF(Ch_Array(i).EQ.'6') Int_Array(I) =  6 
      IF(Ch_Array(i).EQ.'7') Int_Array(I) =  7   
      IF(Ch_Array(i).EQ.'8') Int_Array(I) =  8 
      IF(Ch_Array(i).EQ.'9') Int_Array(I) =  9 
      IF(Ch_Array(i).EQ.'a') Int_Array(I) = 10 
      IF(Ch_Array(i).EQ.'b') Int_Array(I) = 11   
      IF(Ch_Array(i).EQ.'c') Int_Array(I) = 12 
      IF(Ch_Array(i).EQ.'d') Int_Array(I) = 13 
      IF(Ch_Array(i).EQ.'e') Int_Array(I) = 14 
      IF(Ch_Array(i).EQ.'f') Int_Array(I) = 15            
      IF(Ch_Array(i).EQ.'g') Int_Array(I) = 16 
      IF(Ch_Array(i).EQ.'h') Int_Array(I) = 17       
      ENDDO
!
      RETURN
      END  