*-----------------------------------------------------------------------------
* <Rating>647</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.B.EXP.ACC.INT
*-----------------------------------------------------------------------------
* Nombre: SLV.B.EXP.ACC.INT
* Descripcion: Rutina encargada de generar archivo .csv con información de los intereses diarios de las cuentas de clientes.
*----------------------------------------------------------------------------------------------------
* Version	    Autor		 Fecha			Comentario
*----------------------------------------------------------------------------------------------------
* 1.0		ITurcios		21.11.2016		Version inicial
*----------------------------------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_F.AA.INTEREST.ACCRUALS
$INSERT I_F.AA.ARRANGEMENT
*-----------------------------------------------------------------------------
GOSUB INIT
GOSUB PROCESS
RETURN
*-----------------------------------------------------------------------------
INIT:
	;*Definicion de variables

	;*App que contiene cuentas y saldos
 	FN.INT.ACCRUALS		= 'F.AA.INTEREST.ACCRUALS'
 	F.INT.ACCRUALS		= ''
 	CALL OPF(FN.INT.ACCRUALS , F.INT.ACCRUALS)
 	
 	FN.ARRANGEMENT		= 'F.AA.ARRANGEMENT'
 	F.ARRANGEMENT		= ''
 	CALL OPF(FN.ARRANGEMENT , F.ARRANGEMENT)
    
*	DIR.NAME 			= 'C:\Users\hturcios\Desktop\TEST\'
	DIR.NAME			= 'SIC'         
		 
	;*Variables para arreglo de datos
	STR.INTEREST 		= ''	
	J					= 0
	DATA.INTEREST		= ''
	ESTADO.ARR			= 'CLOSE'
	DATE.TODAY			= TODAY	;*YYYYMMDD 
	
	NAME.FILE 			= '_T24AccountsInterestDaily.' : DATE.TODAY  : '.csv'
	
	;*Separador de linea
    EndLine 			= '|'
    
    ;*Product Line
	Y.ACCOUNTS 			= 'ACCOUNTS' 
	Y.DEPOSITS			= 'DEPOSITS'
	Y.LENDING 			= 'LENDING'    	
    
RETURN
*-------------------------------------------------------------------------------
PROCESS:	
	;*Definiendo query para extracción de intereses de cuentas (solo arrangements que no han sido cerradas)
	STMT.CTAS = "SELECT ": FN.ARRANGEMENT : " ARR.STATUS NE " : ESTADO.ARR

    ;*Extrayendo Ids de Arrangement
    CALL EB.READLIST(STMT.CTAS, CTAS.LIST.IDS, '' , NO.OF.RECS.CTAS, Y.CTAS.ERR)
   	 	
	FOR LK = 1 TO NO.OF.RECS.CTAS
		;*Leyendo Data de Arrangement entrante
		CALL CACHE.READ(FN.ARRANGEMENT, CTAS.LIST.IDS<LK>, R.ARRANGEMENT_INI, F.ARRANGEMENT, Y.ARRANGEMENT.ERR2)
		
  		;*Linea del Producto (ACCOUNTS, LENDING o DEPOSITS)
  		ProductLine 		= R.ARRANGEMENT_INI<AA.ARR.PRODUCT.LINE>
  		
		;*PRINCIPALINT 	-> Creditos
		;*CRINTEREST 	-> Cuentas y Depositos
		PROPERTY			= ''
		IF ProductLine EQ Y.ACCOUNTS  THEN
			PROPERTY		= '-CRINTEREST'
		END
		IF ProductLine EQ Y.DEPOSITS THEN
			PROPERTY		= '-DEPOSITINT'
		END
		IF ProductLine EQ Y.LENDING THEN
			PROPERTY		= '-PRINCIPALINT'
		END
		;*Leyendo Data de INT.ACCRUALS del arrangemente entrante
		CALL CACHE.READ(FN.INT.ACCRUALS, CTAS.LIST.IDS<LK> : PROPERTY , R.INT.ACCRUALS, F.INT.ACCRUALS, Y.INT.ERROR)
		
		J						= J + 1		
		;*Leyendo ultimo valor de cada campo multivalor
		PosLastFromDate			= DCOUNT(R.INT.ACCRUALS<AA.INT.ACC.FROM.DATE>, VM)
		PosLastToDate			= DCOUNT(R.INT.ACCRUALS<AA.INT.ACC.TO.DATE>, VM)
		PosLastDays				= DCOUNT(R.INT.ACCRUALS<AA.INT.ACC.DAYS>, VM)
		PosLastBalance			= DCOUNT(R.INT.ACCRUALS<AA.INT.ACC.BALANCE>, VM)		
		PosLastBasis			= DCOUNT(R.INT.ACCRUALS<AA.INT.ACC.BASIS>, VM)		
		PosLastRate				= DCOUNT(R.INT.ACCRUALS<AA.INT.ACC.RATE>, VM)		
		PosLastMargin			= DCOUNT(R.INT.ACCRUALS<AA.INT.ACC.MARGIN>, VM)		
		PosLastAccrualAmt		= DCOUNT(R.INT.ACCRUALS<AA.INT.ACC.ACCRUAL.AMT>, VM)	
		PosLastActAccAmt		= DCOUNT(R.INT.ACCRUALS<AA.INT.ACC.ACT.ACC.AMT>, VM)		
		PosLastCompoundFqu		= DCOUNT(R.INT.ACCRUALS<AA.INT.ACC.COMPOUND.FQU>, VM)		
		PosLastCompoundYield	= DCOUNT(R.INT.ACCRUALS<AA.INT.ACC.COMPOUND.YIELD>, VM)
		PosLastPeriodStart		= DCOUNT(R.INT.ACCRUALS<AA.INT.ACC.PERIOD.START>, VM)
		PosLastPeriodEnd		= DCOUNT(R.INT.ACCRUALS<AA.INT.ACC.PERIOD.END>, VM)		
		PosLastAmtAccrTotal		= DCOUNT(R.INT.ACCRUALS<AA.INT.ACC.TOT.ACCR.AMT>, VM)
		PosLastAmtSusp			= DCOUNT(R.INT.ACCRUALS<AA.INT.ACC.TOT.SUSP.AMT>, VM)
		PosLastAmtDue			= DCOUNT(R.INT.ACCRUALS<AA.INT.ACC.TOT.DUE.AMT>, VM)
		PosLastAmtRpy			= DCOUNT(R.INT.ACCRUALS<AA.INT.ACC.TOT.RPY.AMT>, VM)
		PosLastAmtAdjustInt		= DCOUNT(R.INT.ACCRUALS<AA.INT.ACC.ADJUST.INT.AMT>, VM)		
		PosLastAmtRes			= DCOUNT(R.INT.ACCRUALS<AA.INT.ACC.TOT.RES.AMT>, VM)
		PosLastIntEffDate		= DCOUNT(R.INT.ACCRUALS<AA.INT.ACC.INT.EFF.DATE>, VM)		
		PosLastFloatIndex		= DCOUNT(R.INT.ACCRUALS<AA.INT.ACC.FLOATING.INDEX>, VM)
		PosLastPeriodicIndex	= DCOUNT(R.INT.ACCRUALS<AA.INT.ACC.PERIODIC.INDEX>, VM)	
		PosLastEffDate			= DCOUNT(R.INT.ACCRUALS<AA.INT.ACC.LINK.EFF.DATE>, VM)		
		PosLastLinkedArr		= DCOUNT(R.INT.ACCRUALS<AA.INT.ACC.LINKED.ARRANGEMENT>, VM)
		PosLastLinkedProperty	= DCOUNT(R.INT.ACCRUALS<AA.INT.ACC.LINKED.PROPERTY>, VM)
						 
		;*Se llena los campos para generar CSV
		Arrangement			= CTAS.LIST.IDS<LK>															;*Id de Arrangement
		FromDate 			= R.INT.ACCRUALS<AA.INT.ACC.FROM.DATE><1,PosLastFromDate>					;*From Date
		ToDate	 			= R.INT.ACCRUALS<AA.INT.ACC.TO.DATE><1,PosLastToDate>						;*To Date
		Days	 			= R.INT.ACCRUALS<AA.INT.ACC.DAYS><1,PosLastDays>							;*Dias
		Balance 			= SUM(R.INT.ACCRUALS<AA.INT.ACC.BALANCE><1,PosLastBalance>)					;*Balance
		Basis	 			= R.INT.ACCRUALS<AA.INT.ACC.BASIS><1,PosLastBasis>							;*Basis
		Rate	 			= SUM(R.INT.ACCRUALS<AA.INT.ACC.RATE><1,PosLastRate>)						;*Rate
		Margin	 			= SUM(R.INT.ACCRUALS<AA.INT.ACC.MARGIN><1,PosLastMargin>)					;*Margen
		AccrualAmt 			= SUM(R.INT.ACCRUALS<AA.INT.ACC.ACCRUAL.AMT><1,PosLastAccrualAmt>)			;*Monto Acumulado
		ActAccAmt 			= SUM(R.INT.ACCRUALS<AA.INT.ACC.ACT.ACC.AMT><1,PosLastActAccAmt>)			;*Monto Act
		CompoundFqu 		= R.INT.ACCRUALS<AA.INT.ACC.COMPOUND.FQU><1,PosLastCompoundFqu>				;*Compound Fqu
		CompoundYield		= R.INT.ACCRUALS<AA.INT.ACC.COMPOUND.YIELD><1,PosLastCompoundYield>			;*Compound Yield
		ArchiveDate			= R.INT.ACCRUALS<AA.INT.ACC.LAST.ARCHIVE.DATE>								;*Fecha de archivo
		PeriodStart			= R.INT.ACCRUALS<AA.INT.ACC.PERIOD.START><1,PosLastPeriodStart>				;*Inicio de Periodo
		PeriodEnd 			= R.INT.ACCRUALS<AA.INT.ACC.PERIOD.END><1,PosLastPeriodEnd>					;*Fin de Periodo
		TotalAccrAmt		= R.INT.ACCRUALS<AA.INT.ACC.TOT.ACCR.AMT><1,PosLastAmtAccrTotal>			;*Monto Total Acumulado
		TotalSuspAmt		= R.INT.ACCRUALS<AA.INT.ACC.TOT.SUSP.AMT><1,PosLastAmtSusp>					;*Monto Total Suspendido
		TotalDueAmt			= R.INT.ACCRUALS<AA.INT.ACC.TOT.DUE.AMT><1,PosLastAmtDue>					;*Monto Total debido
		TotalRpyAmt			= R.INT.ACCRUALS<AA.INT.ACC.TOT.RPY.AMT><1,PosLastAmtRpy>					;*Monto Total Rpy
		AdjustIntAmt		= R.INT.ACCRUALS<AA.INT.ACC.ADJUST.INT.AMT><1,PosLastAmtAdjustInt>			;*Monto Adjust
		AmtRes	 			= R.INT.ACCRUALS<AA.INT.ACC.TOT.RES.AMT><1,PosLastAmtRes>					;*Monto Restante
		IntEffDate 			= R.INT.ACCRUALS<AA.INT.ACC.INT.EFF.DATE><1,PosLastIntEffDate>				;*Fecha Efectiva
		FloatIndex 			= R.INT.ACCRUALS<AA.INT.ACC.FLOATING.INDEX><1,PosLastFloatIndex>			;*Indice flotante
		PeriodicIndex		= R.INT.ACCRUALS<AA.INT.ACC.PERIODIC.INDEX><1,PosLastPeriodicIndex>			;*Indice periodico
		AlternateInt		= R.INT.ACCRUALS<AA.INT.ACC.ALTERNATE.INTEREST>								;*Interes Alterno
		LastEffDate			= R.INT.ACCRUALS<AA.INT.ACC.LINK.EFF.DATE><1,PosLastEffDate>				;*Fecha Efectiva vinculada
		LinkedArr 			= R.INT.ACCRUALS<AA.INT.ACC.LINKED.ARRANGEMENT><1,PosLastLinkedArr>			;*Arrangmenet vinculado
		LinkedProperty		= R.INT.ACCRUALS<AA.INT.ACC.LINKED.PROPERTY><1,PosLastLinkedProperty>		;*Propiedad vinculada
		

		
		;*Contrucccion de string para generar archivo
		STR.INTEREST     		 = Arrangement			   : ";"	;*01
		STR.INTEREST			:= FromDate				   : ";"	;*02 
		STR.INTEREST			:= ToDate				   : ";"	;*03
		STR.INTEREST  			:= Days			  	       : ";"	;*04
		STR.INTEREST  			:= Balance				   : ";"	;*05
		STR.INTEREST  			:= Basis			   	   : ";"	;*06
		STR.INTEREST  			:= Rate					   : ";"	;*07
		STR.INTEREST  			:= Margin			 	   : ";"	;*08
		STR.INTEREST  			:= AccrualAmt			   : ";"	;*09
		STR.INTEREST  			:= ActAccAmt			   : ";"	;*10 
		STR.INTEREST  			:= CompoundYield		   : ";"	;*11
		STR.INTEREST  			:= ArchiveDate			   : ";"	;*12
		STR.INTEREST  			:= PeriodStart			   : ";"	;*13
		STR.INTEREST  			:= PeriodEnd			   : ";"	;*14
		STR.INTEREST  			:= TotalAccrAmt			   : ";"	;*15
		STR.INTEREST  			:= TotalSuspAmt			   : ";"	;*16
		STR.INTEREST  			:= TotalDueAmt			   : ";"	;*17
		STR.INTEREST  			:= TotalRpyAmt			   : ";"	;*18
		STR.INTEREST  			:= AdjustIntAmt			   : ";"	;*19
		STR.INTEREST  			:= AmtRes				   : ";"	;*20
		STR.INTEREST  			:= IntEffDate			   : ";"	;*21		
		STR.INTEREST  			:= FloatIndex			   : ";"	;*22
		STR.INTEREST  			:= PeriodicIndex		   : ";"	;*23
		STR.INTEREST  			:= AlternateInt			   : ";"	;*24
		STR.INTEREST			:= LastEffDate			   : ";"	;*25
		STR.INTEREST			:= LinkedArr			   : ";"	;*26
		STR.INTEREST			:= LinkedProperty		   			;*27		
	    STR.INTEREST 			:= EndLine							;*28   
	   
	    DATA.INTEREST<J>		= STR.INTEREST 		   			
		STR.INTEREST			= ''
		
		;*Limpiando variables
		GOSUB CLEAN.VARIABLES 
	NEXT LK
	
	;*Escribir Data en Archivo	
	GOSUB WRITE.FILE.CSV
	
RETURN

WRITE.FILE.CSV:
	;*Eliminando archivo si existe 
    DELETESEQ DIR.NAME, NAME.FILE THEN
    END  
    
	;*Abriendo archivo para escritura	
    OPENSEQ DIR.NAME, NAME.FILE TO SEQ.PTR THEN
        WEOFSEQ NAME.FILE
    END
    
	FOR LM = 1 TO J
	    ;*Escribiendo Data
	    IF LM < J THEN
	    	WRITESEQ DATA.INTEREST<LM> APPEND TO SEQ.PTR THEN ;*con retorno de carro
   			END
	    END
	    IF LM = J THEN
	  		WRITEBLK DATA.INTEREST<LM> ON SEQ.PTR THEN	;*sin retorno de carro
	  		END
  		END		
	NEXT LM
	
	;*Cerrando archivo
	CLOSESEQ SEQ.PTR	
	
	CRT 'Cantidad de registros escritos: ' : LM
RETURN
 
CLEAN.VARIABLES:
		ProductLine		= "" 	;*00
		Arrangement		= ""	;*01
		FromDate		= ""	;*02
		ToDate			= ""	;*03
		Days			= ""	;*04
		Balance			= ""	;*05
		Basis			= ""	;*06
		Rate			= ""	;*07
		Margin			= ""	;*08
		AccrualAmt		= ""	;*09
		ActAccAmt		= ""	;*10
		CompoundYield	= ""	;*11
		ArchiveDate		= ""	;*12
		PeriodStart		= ""	;*13
		PeriodEnd		= ""	;*14
		TotalAccrAmt	= ""	;*15
		TotalSuspAmt	= ""	;*16
		TotalDueAmt		= ""	;*17
		TotalRpyAmt		= ""	;*18
		AdjustIntAmt	= ""	;*19
		AmtRes			= ""	;*20
		IntEffDate		= ""	;*21		
		FloatIndex		= ""	;*22
		PeriodicIndex	= ""	;*23
		AlternateInt	= ""	;*24
		LastEffDate		= ""	;*25
		LinkedArr		= ""	;*26
		LinkedProperty	= ""	;*27		    
RETURN

END

