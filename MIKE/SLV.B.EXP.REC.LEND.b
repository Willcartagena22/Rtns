*-----------------------------------------------------------------------------
* <Rating>8028</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.B.EXP.REC.LEND(ARR.ID)
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_ENQUIRY.COMMON
    $INSERT I_EQUATE
    $INSERT I_F.AA.ARRANGEMENT
    $INSERT I_SLV.B.EXP.REC.LEND.COMMON
    $INSERT I_F.AA.ACTIVITY.BALANCES
    $INSERT I_F.AA.INTEREST
    $INSERT I_F.AA.TERM.AMOUNT
    $INSERT I_F.AA.ACTIVITY.HISTORY
    $INSERT I_F.AA.ARRANGEMENT.ACTIVITY
    $INSERT I_F.EB.SLV.GLOBAL.PARAM
    $INSERT I_F.STMT.ENTRY
    $INSERT I_F.EB.CONTRACT.BALANCES
    $INSERT I_F.SLV.AA.ACT.REVE.BAL.DET ;*CORR0287 Changes
    $INSERT I_F.ACCOUNT
    $INSERT I_F.AA.ACCOUNT
    $INSERT I_SLV.E.LOAN.COMMON ;*CORR0638 Changes
    $INSERT I_F.FUNDS.TRANSFER
    $INSERT I_F.AA.BALANCE.MAINTENANCE
    $INSERT I_F.TELLER.FINANCIAL.SERVICES
    $INSERT I_F.FT.BULK.MASTER
    $INSERT I_F.CUSTOMER
    $INSERT I_F.AA.CUSTOMER
    $INSERT I_F.SLV.DUI.CUSTOMER.CNT
    $INSERT I_F.EB.SLV.LEND.REC.SSF
    $INSERT I_F.EB.SLV.CLS.FECH.REC.SSF
*-----------------------------------------------------------------------------
GOSUB INIT
GOSUB PROCESS
RETURN

INIT:
	Y.ENQUIRY.CORE = 'SLV.E.LOA.SALDOS'
	DIR.NAME  = 'SIF.OUT'
	EndLine = '|'
	
    EQU L.ESTADO.ACUERDO 	TO 'CURRENT'
    EQU P_SEG_IVA_DANIO  	TO "COMP.P.PROP.IVA.S.DANIO"
    EQU P_SEG_VIDA  		TO "COMP.P.PROP.SEGUROS.VIDA"
    EQU P_INT_MORA  		TO "COMP.P.PROP.INT.MORA"
    EQU P_SEG_DANIO  		TO "COMP.P.PROP.SEGUROS.DANIO"
    EQU P_INT_CORR 			TO "COMP.P.PROP.INT.CORR"
    EQU P_CAPITAL 			TO "COMP.P.PROP.CAPITAL"
    EQU PR_REPAYMENT        TO "LENDING-APPLYPAYMENT-PR.REPAYMENT"
    EQU PR_DECREASE         TO "LENDING-APPLYPAYMENT-PR.PRINCIPAL.DECREASE"
    EQU CURRENT_BALANCES    TO "LENDING-APPLYPAYMENT-PR.CURRENT.BALANCES"
    EQU P_ACCPRINCIPALINT   TO "PRINCIPALINT.ACCPRINCIPALINT"
    EQU P_DUEPRINCIPALINT   TO "PRINCIPALINT.DUEPRINCIPALINT"
	EQU P_GRCPRINCIPALINT   TO "PRINCIPALINT.GRCPRINCIPALINT"
	EQU P_NABPENALTYINT     TO "PENALTYINT.NABPENALTYINT"
	EQU P_ACCPENALTYINT     TO "PENALTYINT.ACCPENALTYINT"
	EQU P_DELPRINCIPALINT   TO "PRINCIPALINT.DELPRINCIPALINT"

	;*******INICIO - JONAS
    PAYMENT.CAPITAL			= 0.00 ;* Pago de Capital
    PAYMENT.INT.PRN 		= 0.00 ;* Pago de Interes
    PAYMENT.INT.MOR 		= 0.00 ;* Pago de Interes Moratorios
    PAYMENT.SEG.DEU 		= 0.00 ;* Seguro de Deuda
    PAYMENT.SEG.DAN 		= 0.00 ;* Seguro de Danios
    PAYMENT.IVA.SEG.DAN 	= 0.00 ;* IVA por seguro de danios
	;*******FIN - JONAS

    PAYMENT.CANAL 			= "--" ;* Canal de pago
    V.DESEMBOLSOS       	=	"" ;* Guarda el listado de desembolsos.
    V.INSERTAR.DESEMBOLSO 	= 0  ;* 0 = Verdadero, 1=falso controla el momento que se tiene que insertar el desembolso.
    V.NO.DESMBOLSO 		  	= 0  ;* Controla el numero de desmbolso que se realizan.
    V.DESEMBOLSO.ACTUAL   	= 1  ;* Controla el oreden en que se realizan los desembolsos.
    V.SALDO 			  	= 0.0
    V.MONTO 			  	= 0.0
    V.MOVIMIENTOS		  	= 0  ;* Controla el AA.ACTIVITY.BALANCES tenga mov. para insertar los desembolsos.
    V.INTEREST.RATE			= ''
    V.INTEREST.MORA			= ''
    
    ;* FLAGS
    Y.ACT.FLAG				= ''  ;* 'D' = DESEMBOLSO; 	'T' = TRANSACCION;  'R' = REVERSA 
    
    ;*Se maneja el nombre de enquiry para diferenciar la consulta de banca en linea
    Y.ENQUIRY.NAME = ENQ.SELECTION<1,1>
    Y.ENQUIRY.CORE = 'SLV.E.LOA.SALDOS'
    
    FORMA.PAGO               = ''
    EndLine 			     = '|'
*    DIR.NAME                 = 'C:\properties\'
    REG.LOA 				 = ''
    J                        = 0
    BND                      = 1
    A.LOA.INFO				 = ''
    DATE.REG                 = ''
    NO.E.REG                 = 0
    NO.CTA                   = ''
    EQU TXN.ACPT TO 'ACPT'
    EQU TXN.ACPE TO 'ACPE'
    EQU TXN.ACRP TO 'ACRP'
    PAGO.K   = 0.0
    PAGO.I   = 0.0
    PAGO.O   = 0.0
    FRM.PAGO1 = ''
    FRM.PAGOF = ''
	TFS.1 = 0.00  
	TFS.2 = 0.00
	TFS.3 = 0.00      
	PAGO.IC = 0.00
	PAGO.IM = 0.00
	PAGO.O = 0.00
    PAGO.TTL = 0.00
    FPAGO.CANT = 0
    FPAGO.CONTA = 0
    PAYMENT.INT.PRN2=0.00
    PAYMENT.SEG.DAN2=0.00
    PAYMENT.INT.MOR2=0.00
    PAYMENT.IVA.SEG.DAN2=0.00
    PAYMENT.SEG.DEU2=0.00
    PAYMENT.COST.PROC=0.00
    PAYMENT.SEG.DEU2=0.00
    OTRO.CARGO=0.00
    PAYMENT.INT.VENC = 0.00
    PAYMENT.INT.ACCPENALT = 0.00
    FECHA.RMES = ''
    TFS.ID.PERSONA=''
    FT.ID.PERSONA = ''
    FREC.MES = ''
    R.DATA.SSF = ''
    NOMB.VENT.CARTERA = '0'
    NOMBRE.CUST=''
    ID.CUST = ''
    CTA.ESTADO = ''
    FECHA.INI.SSF= ''
    FECHA.FIN.SSF= ''
    MNT.NVO=0.0
    ARR.ESTADO=''
	PMT.CAPITAL = 0.0
	PMT.INT.PRN = 0.0
	PMT.INT.MOR = 0.0
	PMT.SEG.DEU = 0.0
	PMT.SEG.DAN = 0.0
	PMT.IVA.SEG.DAN = 0.0
	V.CTA = 0.0
RETURN

PROCESS:
		CALL F.READ(FN.FECH.SSF, 'FECHA.REC', R.FSSF, F.FECH.SSF, ERR.FECH)
	    FECHA.INI.SSF= R.FSSF<EB.SLV42.FECHA.INICIO>
	    FECHA.FIN.SSF= R.FSSF<EB.SLV42.FECHA.FIN>	    
		CALL F.READ(FN.ARRANGEMENT, ARR.ID, R.ARR.REG, F.ARRANGEMENT, Y.ARRANGEMENT.ERR2)
	    V.REFERENCIA = ARR.ID
	    
	    ARR.ESTADO = R.ARR.REG<AA.ARR.ARR.STATUS>
	    NO.CTA = R.ARR.REG<AA.ARR.LINKED.APPL.ID>
		ARRANGEMENT = V.REFERENCIA ;*para desembolsos
		ARR.ESTADO = R.ARR.REG<AA.ARR.ARR.STATUS>
		ID.CUST=R.ARR.REG<AA.ARR.CUSTOMER>
		CALL F.READ(FN.CUSTOMER, ID.CUST, R.CUSTOMER, F.CUSTOMER, ERR.CUSTOMER)

		CALL GET.LOC.REF('CUSTOMER','LF.NIT',LOCPOSNIT)
		LF.NIT = ''
		LF.NIT=R.CUSTOMER<EB.CUS.LOCAL.REF><1,LOCPOSNIT>
		
		NOMBRE.CUST=ID.CUST
		CALL SLV.UTIL.GET.NOM.CLIENTE(NOMBRE.CUST)

		CALL GET.LOC.REF('AA.PRD.DES.ACCOUNT','LF.LOAN.STATUS',Y.DAP.INDEX)
		CALL AA.GET.ARRANGEMENT.CONDITIONS(V.REFERENCIA,'ACCOUNT','ACCOUNT',TODAY,ACCOUNT.ID,R.ACCOUNT,ACCOUNT.ERROR)
		Y.R.ACCOUNT = RAISE(R.ACCOUNT)
		CTA.ESTADO  = Y.R.ACCOUNT<AA.AC.LOCAL.REF><1,Y.DAP.INDEX>

		IF CTA.ESTADO EQ 5 THEN
			S.STR.NEW = LF.NIT:";"
			S.STR.NEW := NO.CTA:";"
			S.STR.NEW := "01;"
			S.STR.NEW := "PD;"
			S.STR.NEW := "0;"
			S.STR.NEW := "0;"
			S.STR.NEW := "0;"
			S.STR.NEW := "0;"
			S.STR.NEW := "0;"
			S.STR.NEW := "0;"
			S.STR.NEW := "0;"
			S.STR.NEW := "0;"
			S.STR.NEW := "0;"    
			S.STR.NEW := "0;"
			S.STR.NEW := "0;"
			S.STR.NEW := TODAY:";"
			S.STR.NEW := "0;"
			S.STR.NEW := "0;"
			S.STR.NEW := ID.CUST:";"
			S.STR.NEW := NOMBRE.CUST:";"
			S.STR.NEW := CTA.ESTADO
			
			DATE.REG  = S.STR.NEW
			GOSUB GUARDAR.DATOS.APP
			DATE.REG  = "" 
			S.STR.NEW =	""
		END
		
		CALL AA.GET.ARRANGEMENT.CONDITIONS(ARRANGEMENT, 'CUSTOMER', '', TODAY, RETURN.IDS, RETURN.VALUES.CUST, RETURN.ER)
		
		;* Inicializa las propiedades que poseen las activides (EB.GLOBAL.PARAM)
	    GOSUB INICIALIZAR_PROPIEDADES
	    
		;* Se lista los desembolsos y se registra una linea por desembolso.
*	    GOSUB DESEMBOLSOS
	
		;* Se lista las ACCIONES realizadas sobre el prestamo
		GOSUB ACT.BAL.PROCESS
		
		;* Se lista las ADJUST BILL sobre el prestamo
		GOSUB ACT.ADJUST.BILL.PROCESS
	    ;* Se lista las acciones de REVERSAS sobre el prestamo
	    GOSUB ACT.REV.PROCESS
	 
	    ;*Calculo de saldo final
	    GOSUB RECAL.SALDO.FINAL
*	 10546 - Start
*		;*Pendiente de definir si orden para banca en linea será descendente
*		IF Y.ENQUIRY.NAME NE Y.ENQUIRY.CORE THEN     
*			GOSUB REGIS.ORDEN.RECAL.TCIB
*		END
*	** 10546-E	
		TEMP.ARRAY = A.LOA.INFO
	    IF TEMP.ARRAY AND Y.ENQUIRY.NAME EQ Y.ENQUIRY.CORE THEN 
	        CHANGE FM TO '~' IN TEMP.ARRAY
	        CALL SLV.S.CALL.LAON.STMT(TEMP.ARRAY)
	    END
	    NO.E.REG = 0
RETURN

*-----------------------------------------------------------------------------
INICIALIZAR_PROPIEDADES:
*-----------------------------------------------------------------------------
* Se inician las propiedades necesarias para la rutina 
*-----------------------------------------------------------------------------
    ;* obtenemos el ACTIVITY.BALANCES
    CALL F.READ(FN.ACT.BALC, V.REFERENCIA, R.ACT.BALC, F.ACT.BALC, Y.ERR.R2)
    	ACTIVITY.COUNT      = DCOUNT(R.ACT.BALC<AA.ACT.BAL.ACTIVITY>,VM)
    	NUM.ACTIVITY.REF    = DCOUNT(R.ACT.BALC<AA.ACT.BAL.ACTIVITY.REF>,VM)
    	
	;* obtenemos el ACTIVITY.HISTORY
    CALL F.READ(FN.ACT.HIST, V.REFERENCIA, R.ACT.HIST, F.ACT.HIST, ERR.ACT.HIST)
    	ACTIVITY.HIST.COUNT = DCOUNT(R.ACT.HIST<AA.AH.ACTIVITY.ID>,VM)
    	ACT.HIST.DATE.COUNT = DCOUNT(R.ACT.HIST<AA.AH.SYSTEM.DATE>,VM)
    	EFECTIVE.DATE.COUNT = DCOUNT(R.ACT.HIST<AA.AH.EFFECTIVE.DATE>,VM)
	;* obtenemos los datos del commitment
    CALL AA.GET.ARRANGEMENT.CONDITIONS(V.REFERENCIA, 'TERM.AMOUNT', 'COMMITMENT', TODAY, R.ID.TA, R.AA.TERM, Y.ERR.TA)
    	AMT.AMOUNT 	= R.AA.TERM<1,AA.AMT.AMOUNT>
    	AMT.TERM 	= R.AA.TERM<1,AA.AMT.TERM>

	;* obtenemos los datos del principal int-> Tasa de interes
    CALL AA.GET.ARRANGEMENT.CONDITIONS(V.REFERENCIA, 'INTEREST', 'PRINCIPALINT', TODAY, RETURN.ID.PRINCIPALINT, R.AA.PRINCIPALINT, Y.ERR.PRINCIPALINT)
    
	;* Tasa de interes moratorio
    CALL AA.GET.ARRANGEMENT.CONDITIONS(V.REFERENCIA, 'INTEREST', 'PENALTYINT', TODAY, RETURN.ID.PENALTYINT, R.AA.PENALTYINT, Y.ERR.PENALTYINT)
 
;* obtenemos las propiedades para  el iva seguros de danios.
    CALL F.READ(FN.PARAM, P_SEG_IVA_DANIO, R.PARAM.SEG.IVA.DANIO, F.PARAM,ERR.PARAM)
    FOR FOR.PARAM.SEG.IVA.DANIO = 1 TO DCOUNT (R.PARAM.SEG.IVA.DANIO<EB.SLV39.VALOR.PARAM>, @VM)
        ARR.PARAM.SEG.IVA.DANIO:=R.PARAM.SEG.IVA.DANIO<EB.SLV39.VALOR.PARAM><1,FOR.PARAM.SEG.IVA.DANIO>
    NEXT FOR.PARAM.SEG.IVA.DANIO
    
;* obtenemos las propiedades para el seguros.
    CALL F.READ(FN.PARAM, P_SEG_VIDA, R.PARAM.SEG.VIDA, F.PARAM,ERR.PARAM)
    FOR FOR.PARAM.SEG.VIDA =1 TO DCOUNT (R.PARAM.SEG.VIDA<EB.SLV39.VALOR.PARAM>, @VM)
        ARR.PARAM.SEG.VIDA:=R.PARAM.SEG.VIDA<EB.SLV39.VALOR.PARAM><1,FOR.PARAM.SEG.VIDA>
    NEXT FOR.PARAM.SEG.VIDA
    
;* obtenemos las propiedades para los intereses mora.
	CALL F.READ(FN.PARAM, P_INT_MORA, R.PARAM.INT.MORA, F.PARAM,ERR.PARAM)
    FOR FOR.PARAM.INT.MORA = 1 TO DCOUNT (R.PARAM.INT.MORA<EB.SLV39.VALOR.PARAM>, @VM)
        ARR.PARAM.INT.MORA:=R.PARAM.INT.MORA<EB.SLV39.VALOR.PARAM><1,FOR.PARAM.INT.MORA>
    NEXT FOR.PARAM.INT.MORA
    
;* obtenemos las propiedades para el seguros de danios.
	CALL F.READ(FN.PARAM, P_SEG_DANIO, R.PARAM.SEG.DANIO, F.PARAM,ERR.PARAM)
    FOR FOR.PARAM.SEG.DANIO = 1 TO DCOUNT (R.PARAM.SEG.DANIO<EB.SLV39.VALOR.PARAM>, @VM)
        ARR.PARAM.SEG.DANIO:=R.PARAM.SEG.DANIO<EB.SLV39.VALOR.PARAM><1,FOR.PARAM.SEG.DANIO>
    NEXT FOR.PARAM.SEG.DANIO
    
;* obtenemos las propiedades para los intereses corrientes.
	CALL F.READ(FN.PARAM, P_INT_CORR, R.PARAM.INT.CORR,F.PARAM, ERR.PARAM)
	
    FOR FOR.PARAM.INT.CORR = 1 TO DCOUNT (R.PARAM.INT.CORR<EB.SLV39.VALOR.PARAM>, @VM)
        ARR.PARAM.INT.CORR:=R.PARAM.INT.CORR<EB.SLV39.VALOR.PARAM><1,FOR.PARAM.INT.CORR>
        ;*CRT 'ARR.PARAM.INT.CORR>>':ARR.PARAM.INT.CORR
    NEXT FOR.PARAM.INT.CORR
    
;* obtenemos las propiedades para los movimientos de capital.
	CALL F.READ(FN.PARAM, P_CAPITAL, R.PARAM.CAPITAL,F.PARAM, ERR.PARAM)
    FOR FOR.PARAM.CAPITAL = 1 TO DCOUNT (R.PARAM.CAPITAL<EB.SLV39.VALOR.PARAM>, @VM)
        ARR.PARAM.CAPITAL:=R.PARAM.CAPITAL<EB.SLV39.VALOR.PARAM><1,FOR.PARAM.CAPITAL>
    NEXT FOR.PARAM.CAPITAL
    
    ;* Inicialización de variables para el reporte final 
    V.TERM.AMT = R.AA.TERM<1,AA.AMT.AMOUNT> 
	Y.ACCT.NO 		= R.ARR<AA.ARR.LINKED.APPL.ID>
    Y.ARR.EFF.DATE	= R.ARR<AA.ARR.START.DATE>
	V.INTEREST.RATE		= R.AA.PRINCIPALINT<1,AA.INT.FIXED.RATE>
	V.INTEREST.MORA		= R.AA.PENALTYINT<1,AA.INT.FIXED.RATE>
	IF V.INTEREST.MORA EQ 0 THEN
		V.INTEREST.MORA		= R.AA.PENALTYINT<1,AA.INT.EFFECTIVE.RATE>
	END    
RETURN

*--------------------------------------------------------------------------------------
ACT.REV.PROCESS:
*--------------------------------------------------------------------------------------
;* se obtiene el listado de actividades que corresponden a las reversas en un prestamo.
*--------------------------------------------------------------------------------------
IF FIELD(R.ACT.BALC<AA.ACT.BAL.ACTIVITY.DATE>,VM,Y.CNT) GE FECHA.INI.SSF AND FIELD(R.ACT.BALC<AA.ACT.BAL.ACTIVITY.DATE>,VM,Y.CNT) LE FECHA.FIN.SSF THEN
	Y.CNT 			= 1
    R.REVE.DET 		= ''
    REVE.DET.ERR	= ''
    CALL F.READ(FN.SLV.AA.ACT.REVE.BAL.DET, V.REFERENCIA, R.REVE.DET, F.SLV.AA.ACT.REVE.BAL.DET, REVE.DET.ERR)
    R.ACT.BALC 		= R.REVE.DET

    IF R.ACT.BALC THEN
 		Y.ACT.LIST = R.ACT.BALC<SLV.REV.ACTIV.REF>
   		LOOP 
 			REMOVE Y.CURR.REV FROM Y.ACT.LIST SETTING CUR.POS.REV
    	WHILE Y.CURR.REV : CUR.POS.REV
    		ACTIVITY.ID = Y.ACT.LIST<1, Y.CNT>
		   	Y.FT.DES			= ''   ;* El detalle del Funds Transfer
	    	PAYMENT.CANAL		= ''
			V.DATETIME			= ''   ;* Para orden de registros    			
	    	;* De la History se obtiene la Child Activity 
			CALL F.READ.HISTORY(FN.AA.ARR.HIS, ACTIVITY.ID, R.ARR.ACT.2, F.AA.ARR.HIS,ERR.ARR.ACT.2)
			
			Y.TIP.ACT = FIELD(R.ACT.BALC<AA.ACT.BAL.ACTIVITY>,VM, Y.CNT) 

			IF Y.TIP.ACT EQ PR_REPAYMENT OR Y.TIP.ACT EQ PR_DECREASE OR Y.TIP.ACT EQ CURRENT_BALANCES  THEN
	   			Y.MASTER = R.ARR.ACT.2<AA.ARR.ACT.MASTER.AAA>
	   			
	   			;* De la Master Activity se obtiene la información del FUNDS TRANSFER
	   			CALL F.READ(FN.ARR.ACT, Y.MASTER, R.ARR.ACT.2, F.ARR.ACT, ERR.ARR.ACT)
	   			IF ERR.ARR.ACT THEN
	   				CALL F.READ.HISTORY(FN.AA.ARR.HIS, Y.MASTER, R.ARR.ACT.2, F.AA.ARR.HIS,ERR.ARR.ACT.2)
	   			END
    			V.FECHA.APLICACION  = R.ARR.ACT.2<AA.ARR.ACT.EFFECTIVE.DATE>
    			V.FECHA.VALOR  		= R.ARR.ACT.2<AA.ARR.ACT.EFFECTIVE.DATE>
    			V.DATETIME			= R.ARR.ACT.2<AA.ARR.ACT.DATE.TIME>
   				Y.FT.DES 			= R.ARR.ACT.2<AA.ARR.ACT.TXN.CONTRACT.ID>
   				PAYMENT.CANAL 		= R.ARR.ACT.2<AA.ARR.ACT.CO.CODE>

				CALL F.READ(FN.FT, Y.FT.DES, R.ARR.FT, F.FT, ERR.FT)
				IF ERR.FT THEN
					CALL F.READ.HISTORY(FN.FT$HIS, Y.FT.DES, R.ARR.FT, F.FT$HIS, ERR.FT.HIS)
				END
				IF R.ARR.FT THEN
   			    	PAYMENT.CANAL 	= R.ARR.FT<FT.CO.CODE>
				END

       			;* Imprimiento Transacción Normal
   				Y.ACT.FLAG 		= 'R+'
   				
   				GOSUB OBTENER_DATA
   				GOSUB REGISTRAR_OPERACION
   				GOSUB LIMPIAR_DATOS

   				;* Imprimiendo la reversa
   				Y.ACT.FLAG 		= 'R-'
   				
   				GOSUB OBTENER_DATA
   				GOSUB REGISTRAR_OPERACION
   				GOSUB LIMPIAR_DATOS
    		END 
    		Y.CNT + = 1
    	REPEAT
    END
END 
RETURN 

ACT.BAL.PROCESS:
*-----------------------------------------------------------------------------
;* se obtiene el listado de actividades que corresponden a un prestamo.
*-----------------------------------------------------------------------------
	;* Inicializando variables para procesar las actividades 
 	Y.CNT 			= 1 									;* Contador de actividades realizadas en el prestamo
 	Y.ACT.LIST 		= R.ACT.BALC<AA.ACT.BAL.ACTIVITY.REF>
 	Y.ACT.FLAG 		= 'T' 									;* Transacción normal	
* 	Y.TOT.PAGO 		= 0
 	Y.SUB.TOT		= 0	

	LOOP 
 		REMOVE Y.CURR FROM Y.ACT.LIST SETTING CUR.POS
    WHILE Y.CURR
IF FIELD(R.ACT.BALC<AA.ACT.BAL.ACTIVITY.DATE>,VM,Y.CNT) GE FECHA.INI.SSF AND FIELD(R.ACT.BALC<AA.ACT.BAL.ACTIVITY.DATE>,VM,Y.CNT) LE FECHA.FIN.SSF THEN
    	ACTIVITY.ID = Y.ACT.LIST<1, Y.CNT>
    	Y.FT.DES			= ''   ;* El detalle del Funds Transfer
    	PAYMENT.CANAL		= ''
    	;*Si es reversa el registro esta en historico leer de ahi
    	ERR.ARR.ACT.2 = ""
    	Y.ARR.ACT.NAU.ERR = ""
    	R.ARR.ACT.MASTER = ""
    	R.ARR.ACT.CHILD = ""
    	Y.ACTIVITY.CHILD = ""
		CALL F.READ(FN.ARR.ACT, ACTIVITY.ID, R.ARR.ACT.2, F.ARR.ACT, ERR.ARR.ACT.2)
		
*TCIB-SIT-8989-S		
		IF ERR.ARR.ACT.2 THEN
			;*Obtener el total pagado
			R.ARR.ACT.NAU = ""
			Y.ARR.ACT.NAU.ERR = ""
			Y.ARR.NAU.FLAG = ""
			CALL F.READ(FN.ARR.ACT.NAU, ACTIVITY.ID, R.ARR.ACT.NAU, F.ARR.ACT.NAU, Y.ARR.ACT.NAU.ERR)
			IF R.ARR.ACT.NAU THEN
			    Y.CNT += 1
				CONTINUE
			END
		END
*TCIB-SIT-8989-E
		
    	Y.TIP.ACT 	= FIELD(R.ACT.BALC<AA.ACT.BAL.ACTIVITY>,VM, Y.CNT)
    	;* Verificar si las actividades son parte del reporte
    	IF Y.TIP.ACT EQ PR_REPAYMENT OR Y.TIP.ACT EQ PR_DECREASE OR Y.TIP.ACT EQ CURRENT_BALANCES THEN    	
    		;*Se extrae el canal del arrangement activity y la fecha efectiva de la actividad
	        V.FECHA.APLICACION  = R.ARR.ACT.2<AA.ARR.ACT.EFFECTIVE.DATE>
	        V.FECHA.VALOR       = R.ARR.ACT.2<AA.ARR.ACT.EFFECTIVE.DATE>
	        V.DATETIME 			= R.ARR.ACT.2<AA.ARR.ACT.DATE.TIME>
	       	Y.MASTER 			= R.ARR.ACT.2<AA.ARR.ACT.MASTER.AAA>
	       	Y.ADJUSTMENT		= R.ARR.ACT.2<AA.ARR.ACT.ADJUSTMENT>
	       	Y.LINKED.ACT		= R.ARR.ACT.2<AA.ARR.ACT.LINKED.ACTIVITY>
        	Y.CHILD				= R.ARR.ACT.2<AA.ARR.ACT.CHILD.ACTIVITY><1,1>
        	Y.INI.TYP			= R.ARR.ACT.2<AA.ARR.ACT.INITIATION.TYPE>
   			PAYMENT.CANAL 		= R.ARR.ACT.2<AA.ARR.ACT.CO.CODE>
   			Y.SUB.TOT			= R.ARR.ACT.2<AA.ARR.ACT.TXN.AMOUNT>
   			Y.PARCIAL			= 1
			IF Y.TOT.PAGO EQ 0 THEN
		       	IF Y.ADJUSTMENT THEN
		       		Y.MASTER 		=  ACTIVITY.ID
		       	END
		       	
		       	IF Y.LINKED.ACT THEN
		       		Y.MASTER = Y.LINKED.ACT
		       	END
				CALL F.READ(FN.ARR.ACT, Y.MASTER, R.ARR.ACT.MASTER, F.ARR.ACT, ERR.ARR.ACT.MAS)
				Y.TOT.PAGO = R.ARR.ACT.MASTER<AA.ARR.ACT.ORIG.TXN.AMT>
				CALL F.READ(FN.ARR.ACT, Y.CHILD, R.ARR.ACT.CHILD, F.ARR.ACT, ERR.ARR.ACT.CHI)
				Y.ACTIVITY.CHILD = R.ARR.ACT.CHILD<AA.ARR.ACT.ACTIVITY>
				Y.MON.CHILD = R.ARR.ACT.CHILD<AA.ARR.ACT.TXN.AMOUNT>
				Y.REVE.CHILD = R.ARR.ACT.CHILD<AA.ARR.ACT.RECORD.STATUS>
			END		
		    ;* Si es una actividad principal  
	    	IF Y.MASTER EQ ACTIVITY.ID AND Y.CHILD NE '' AND Y.INI.TYP NE 'SECONDARY' AND Y.TOT.PAGO NE Y.SUB.TOT AND Y.MON.CHILD GT 0 AND Y.REVE.CHILD NE 'REVE' THEN 
	    		Y.FT.DES 		= R.ARR.ACT.2<AA.ARR.ACT.TXN.CONTRACT.ID>
		    	GOSUB OBTENER_DATA
			END ELSE
				;* Se obtiene el detalle del Funds Transfer, cuando solo la actividad "SECONDARY" esta registrada en el BALANCES
	    		Y.PARCIAL = 0
				IF Y.FT.DES EQ '' THEN
					CALL F.READ(FN.ARR.ACT, Y.MASTER, R.ARR.ACT.2, F.ARR.ACT, ERR.ARR.ACT.2)
					Y.FT.DES 		= R.ARR.ACT.2<AA.ARR.ACT.TXN.CONTRACT.ID>
					CALL F.READ(FN.FT, Y.FT.DES, R.ARR.FT, F.FT, ERR.FT)
					IF ERR.FT THEN
						CALL F.READ.HISTORY(FN.FT$HIS, Y.FT.DES, R.ARR.FT, F.FT$HIS, ERR.FT.HIS)
					END
					 
					IF Y.FT.DES THEN  
						PAYMENT.CANAL 	= R.ARR.FT<FT.CO.CODE>
					END
				END
				;*Obtener informacion de fecha
				IF R.ARR.FT THEN
					V.FECHA.VALOR = R.ARR.FT<FT.PROCESSING.DATE>
				END
			
				GOSUB OBTENER_DATA				
				;*Todo Registro UNC debe Ocultarse en el Estado de Cuenta - Pago Anticipado : OCORNEJO 05.01.2018
				;*-----------------------------------------------------------------------------------------------
				IF Y.FLG.UNC EQ 'N' THEN
					GOSUB REGISTRAR_OPERACION
				END
				;*-----------------------------------------------------------------------------------------------
				GOSUB LIMPIAR_DATOS
*IF FIELD(R.ACT.BALC<AA.ACT.BAL.ACTIVITY.DATE>,VM,Y.CNT) GE FECHA.INI.SSF AND FIELD(R.ACT.BALC<AA.ACT.BAL.ACTIVITY.DATE>,VM,Y.CNT) LE FECHA.FIN.SSF THEN		

*IF FIELD(R.ACT.BALC<AA.ACT.BAL.ACTIVITY.DATE>,VM,Y.CNT) GE '20161001' AND FIELD(R.ACT.BALC<AA.ACT.BAL.ACTIVITY.DATE>,VM,Y.CNT) LE '20161030' THEN
TFS.CRT=''	
				CALL GET.LOC.REF('AA.ARR.CUSTOMER','LF.FORMA.PAGO', LOCPOSForPag)		

				IF TXN.ACRP EQ R.ARR.FT<FT.TRANSACTION.TYPE> THEN
					ORIGEN.RECU = '1'
					;*FT.ORDERING.CUST >> TFS17234Z5638
					IF R.ARR.FT<FT.ORDERING.CUST> EQ '' THEN
						;*FT.INWARD.PAY.TYPE >> FTBULK-BKM0015700...
						IF R.ARR.FT<FT.INWARD.PAY.TYPE> EQ '' THEN
							FRM.PAGOF = FIELD(RETURN.VALUES.CUST<1,AA.CUS.LOCAL.REF>,SM,LOCPOSForPag)
							;*FORMA.PAGO >> OPI // GENERA FT FT17242H5FDG;1
							FORMA.PAGO<-1>='Transferencia'
							CRT 'MetodoDePagoOPI>>':FRM.PAGOF:'-':'Transferencia':'-':V.REFERENCIA:'-':Y.FT.DES:'-':R.ARR.FT<FT.TRANSACTION.TYPE>
						END ELSE
							;*EL PAGO SE REALIZO POR FT a travez de BulkMaster
							CALL F.READ(FN.FT.BM, FIELD(FIELD(R.ARR.FT<FT.INWARD.PAY.TYPE>, "-", 2),".",1), R.FT.BM, F.FT.BM, Y.ERR.R2)
							CALL F.READ(FN.TFS, R.FT.BM<FT.BLK.MAS.DESCRIPTION>, R.TFS, F.TFS, Y.ERR)
							FPAGO.CANT = DCOUNT(R.TFS<TFS.TRANSACTION.INT>,VM)

							GOSUB CARGAR.FRM.PAGO
							
							IF R.TFS<TFS.TRANSACTION.INT> EQ '' THEN
*								CRT 'MetodoDePago>>':'RRHH-':'Transferencia_BP':'-':V.REFERENCIA
*								CRT 'MAS.DESCRIPTION>>':R.FT.BM<FT.BLK.MAS.DESCRIPTION>
								FORMA.PAGO<-1>='Transferencia'
							END ELSE
								;* BUILD PAYMENT
								FORMA.PAGO<-1>=R.TFS<TFS.TRANSACTION.INT>
								FRM.PAGOF = FIELD(RETURN.VALUES.CUST<1,AA.CUS.LOCAL.REF>,SM,LOCPOSForPag)

							END	 
						END					
					END ELSE
						;*EL PAGO SE REALIZO A TRAVEZ DE UNA TFS
						CALL F.READ(FN.TFS,R.ARR.FT<FT.ORDERING.CUST>, R.TFS, F.TFS, Y.ERR)
						FPAGO.CANT = DCOUNT(R.TFS<TFS.TRANSACTION.INT>,VM)
						GOSUB CARGAR.FRM.PAGO
						FORMA.PAGO<-1>=R.TFS<TFS.TRANSACTION.INT>
						FRM.PAGOF = FIELD(RETURN.VALUES.CUST<1,AA.CUS.LOCAL.REF>,SM,LOCPOSForPag)
*						CRT 'MetodoDePago>>':FRM.PAGOF:'- Transferencia_':R.TFS<TFS.TRANSACTION.INT>:'-':V.REFERENCIA	
*						CRT 'ORDERING.CUST>>':R.ARR.FT<FT.ORDERING.CUST>
						TFS.ID.PERSONA =R.ARR.FT<FT.ORDERING.CUST>
						GOSUB OBT.ID.PERSONA.TFS
					END					
				END ELSE
					ORIGEN.RECU = '1'
					IF R.ARR.FT<FT.ORDERING.CUST> EQ '' THEN
						IF R.ARR.FT<FT.INWARD.PAY.TYPE> EQ '' THEN
							;*NOTHING
						END ELSE
							CALL F.READ(FN.FT.BM, FIELD(FIELD(R.ARR.FT<FT.INWARD.PAY.TYPE>, "-", 2),".",1), R.FT.BM, F.FT.BM, Y.ERR.R2)
							CALL GET.LOC.REF('FT.BULK.MASTER','LF.TXN.BP', BKML.POS)
							IF FIELD(R.FT.BM<1,FT.BLK.MAS.LOCAL.REF>,SM,BKML.POS) EQ '' THEN
								;*PAGO DE PRÉSTAMO A TERCERO POR BANCA EN LÍNEA PERSONAS Y EMPRESAS
								ORIGEN.RECU = '2'
								FT.ID.PERSONA =R.ARR.FT<FT.DEBIT.CUSTOMER>
								GOSUB OBT.ID.PERSONA.TFS
							END ELSE
								ORIGEN.RECU = '1'
								;*PAGO DE PRÉSTAMO MASIVO POR BANCA EN LÍNEA EMPRESAS
							END 
						END
					END
					
					FRM.PAGOF = FIELD(RETURN.VALUES.CUST<1,AA.CUS.LOCAL.REF>,SM,LOCPOSForPag)
*					CRT 'MetodoDePago>>':FRM.PAGOF:'-':'Transferencia_BE':'-':V.REFERENCIA:'-':R.ARR.FT<FT.TRANSACTION.TYPE>
					FORMA.PAGO<-1>='Transferencia'
				END	
				
END	
			END	    	
    	END
    
        Y.CNT + = 1
    REPEAT
RETURN

*---------------------------------------------------------------------------------------
ACT.ADJUST.BILL.PROCESS:
*--------------------------------------------------------------------------------------
;* se obtiene el listado de adjust bill que corresponden a un prestamo.
*--------------------------------------------------------------------------------------
	;* Inicializando variables para procesar las actividades 
 	Y.CNT 			= 1 									;* Contador de actividades realizadas en el prestamo
 	Y.ACT.LIST 		= R.ACT.BALC<AA.ACT.BAL.ACTIVITY.REF>
 	Y.ACT.FLAG 		= 'AB' 									;* Transacción normal					
	CALL AA.GET.ARRANGEMENT.CONDITIONS(V.REFERENCIA, 'BALANCE.MAINTENANCE', '', TODAY, R.ID.BILL, R.ARR.BILL, Y.ERR.BILL)
	R.COND		= RAISE(R.ARR.BILL)
	Y.BILL.REF = R.COND<AA.BM.BILL.REF> 
RETURN

*-----------------------------------------------------------------------------
RECAL.SALDO.FINAL:
*-----------------------------------------------------------------------------
;*Ordenar registros
    A.LOA.INFO.ORDEN = ''
    A.LOA.INFO.SALDO = ''
    Y.COUNT = DCOUNT(A.LOA.INFO, FM)
    Y.SALDO.FINAL = 0
    FOR I=1 TO Y.COUNT
    	Y.ORDEN = FIELD(A.LOA.INFO<I>,"#",2,1)
        ;*Key para ordenamiento de registros
        Y.CUOTA = FIELD(Y.ORDEN, "*", 11)
        Y.ACT.FLAG  = FIELD(FIELD(A.LOA.INFO<I>,"#",1,1),"_",2,1)
		Y.SALDO.FINAL -= FIELDS(Y.ORDEN,"*",7)
		Y.SALDO.FINAL += FIELDS(Y.ORDEN,"*",13)
		Y.FECHA = FIELD(Y.ORDEN, "*", 2)
		Y.DATETME = FIELD(FIELD(A.LOA.INFO<I>,"#",1,1),"_",1,1)

		BEGIN CASE
			CASE Y.ACT.FLAG EQ 'D'	;* Desembolso
				Y.KEY = Y.FECHA:Y.DATETME:"W"
			CASE Y.ACT.FLAG EQ 'AB'	;* Ajuste
				Y.KEY = Y.FECHA:Y.DATETME:"X"
			CASE Y.ACT.FLAG EQ 'R+' OR Y.ACT.FLAG EQ 'R-'	;* Reversa
                IF Y.CUOTA GE 0 THEN
                    Y.KEY = Y.FECHA:Y.DATETME:"Y"
                END ELSE
                    Y.KEY = Y.FECHA:Y.DATETME:"Z"
                END
	 			V.MONTO		= ''
	 		CASE 1
	 			Y.KEY = Y.FECHA:Y.DATETME:"Z1"
		END CASE       

        ;*Agregar campo de Key al arreglo para ordenamiento
        S.STR.NEW =  Y.KEY:"*":Y.ORDEN

        A.LOA.INFO.ORDEN<-1>  = S.STR.NEW
    NEXT I
    IF Y.SALDO.FINAL LT 0 THEN
    	Y.SALDO.FINAL = 0
    END
;*Ordenar de acuerdo a Key
    A.LOA.INFO.ORDEN = SORT(A.LOA.INFO.ORDEN)
    
    GOSUB REGIS.ORDEN.RECAL
RETURN

*-----------------------------------------------------------------------------
REGIS.ORDEN.RECAL:
*-----------------------------------------------------------------------------
;*Ordenamiento para consultas T24
	Y.COUNT = 0
    Y.COUNT = DCOUNT(A.LOA.INFO.ORDEN , FM)
	Y.SALDO = 0
	S.STR.NEW = ''

    FOR I = 1 TO Y.COUNT
    	REC.MES.CAP.MORA = 0.0
    	TTL.REC.CMORA = 0.0
		Y.R = A.LOA.INFO.ORDEN<I>

		PAGO.TTL   += FIELD(Y.R,"*",12)
		PAGO.K     += FIELD(Y.R,"*",8)
		PAGO.I     += FIELD(Y.R,"*", 6) ;*+ FIELD(Y.R,"*", 7)
		PAGO.IC    += FIELD(Y.R,"*", 7)
		PAGO.O     += FIELD(Y.R,"*", 9) + FIELD(Y.R,"*",10) + FIELD(Y.R,"*",11)
		
		PAGO.SDE += FIELD(Y.R,"*", 9) ;*SEGURO DEUDA
		PAGO.SDA += FIELD(Y.R,"*", 10) ;*SEGURO DAÑOS
		PAGO.ISD += FIELD(Y.R,"*", 11) ;*IVA SEGURO DAÑOS
		
		REC.MES.CAP.MORA = FIELD(Y.R,"*", 21) - FIELD(Y.R,"*", 22)
		TTL.REC.CMORA =  REC.MES.CAP.MORA + PAYMENT.INT.VENC
		PAY11 = (((TTL.REC.CMORA*(-1)) + PAYMENT.INT.VENC) - (REC.MES.CAP.MORA*(-1)))
		
		FECHA.RMES = FIELD(Y.R,"*", 23)

		IF PAGO.K LT 0 THEN
			PAGO.K = PAGO.K * (-1)
		END

		IF PAGO.TTL LT 0 THEN						
			PAGO.TTL = V.CTA
			PAGO.K   = PMT.CAPITAL
		END
				
*		GOSUB PRELACION.PAGO
*		IF FPAGO.CANT LT 2 THEN
			BND = BND + 1
			S.STR.NEW = LF.NIT:";"                              ;*numeroIdentificadorPersona
			S.STR.NEW := NO.CTA:";"                             ;*numReferencia
			S.STR.NEW := "01;"                                  ;*codCartera
			S.STR.NEW := "PD;"                                   ;*codActivo
			;*CRT '****************************************'
	        S.STR.NEW := PAGO.TTL:";"                            ;*montoRecMes
	        ;*CRT 'montoRecMes>>':PAGO.TTL
	        
	        S.STR.NEW := PAGO.K:";"                              ;*montoRecMesCapital
	        ;*CRT 'montoRecMesCapital>>':PAGO.K
	        
	        S.STR.NEW := PAGO.I:";"                              ;*montoRecMesIntMora
	        ;*CRT 'montoRecMesIntMora>>':PAGO.I
	        
	        S.STR.NEW := "0.00;"                                ;*montoRecMesIntVen
	        ;*CRT 'montoRecMesIntVen>>0.00'
			
	        S.STR.NEW := PAGO.IC:";"                             ;*montoRecMesIntCorr
	        ;*CRT 'montoRecMesIntCorr>>':PAGO.IC
	        
	        S.STR.NEW := PAGO.SDA:";"                            ;*montoRecMesCargoSegDano
	        ;*CRT 'montoRecMesCargoSegDano>>':PAGO.SDA
	        
	        S.STR.NEW := PAY11:";"                               ;*montoRecMesCapMora  
	        ;*CRT 'montoRecMesCapMora>>':PAY11
	        
	        S.STR.NEW := PAGO.ISD:";"                            ;*montoRecMesCargoSegDanoIVA
	        ;*CRT 'montoRecMesCargoSegDanoIVA>>':PAGO.ISD
	        
	        S.STR.NEW := PAGO.SDE:";"                            ;*montoRecMesCargoSegVida     
	        ;*CRT 'montoRecMesCargoSegVida>>':PAGO.SDE
	        
	        ;*CRT '****************************************'  
	        S.STR.NEW := PAYMENT.COST.PROC:";"

			IF FORMA.PAGO<I> EQ 'DepEfectivo' THEN
				FREC.MES = 1
			END ELSE
				FREC.MES = 3
			END

	        S.STR.NEW := FREC.MES    :";"
	        S.STR.NEW := FECHA.RMES  :";"
	        S.STR.NEW := ORIGEN.RECU :";"
	        S.STR.NEW := NOMB.VENT.CARTERA :";"
	        S.STR.NEW := ID.CUST     :";"
	        S.STR.NEW := NOMBRE.CUST :";"
	        S.STR.NEW := CTA.ESTADO
*	        S.STR.NEW := EndLine	
*       ;*Quitar leyenda REVE al pago, solo queda en la reversa
*        IF FIELD(Y.R,"*",19) EQ "REVE" AND FIELD(Y.R,"*",12) GT 0 THEN
*        	S.STR.NEW := "*":FIELD(Y.R,"*",20)
*		END ELSE
*			S.STR.NEW := FIELD(Y.R,"*",19):"*":FIELD(Y.R,"*",20)
*		END
		
        ;*A.LOA.INFO<-1>  = S.STR.NEW:"*":Y.SALDO.FINAL:"*":Y
	        A.LOA.INFO<-1>= S.STR.NEW
	        DATE.REG  = S.STR.NEW
*       END
        Y.R = ''
    NEXT I
    
    GOSUB GUARDAR.DATOS.APP
*   CRT '***************************************************'
*	CRT 'S.STR.NEW>>':S.STR.NEW
*	CRT '***************************************************'
*CRT 'OTRO.CARGO>>':OTRO.CARGO
	S.STR.NEW =''
    FORMA.PAGO = ''
    A.LOA.INFO.ORDEN =''
    A.LOA.INFO = ''
    
RETURN

*-----------------------------------------------------------------------------
OBTENER_DATA:
*-----------------------------------------------------------------------------
;* se obtiene los detalles de las actividades realizadas en el prestamo
*-----------------------------------------------------------------------------
IF FIELD(R.ACT.BALC<AA.ACT.BAL.ACTIVITY.DATE>,VM,Y.CNT) GE FECHA.INI.SSF AND FIELD(R.ACT.BALC<AA.ACT.BAL.ACTIVITY.DATE>,VM,Y.CNT) LE FECHA.FIN.SSF THEN

    FECHA.RMES = FIELD(R.ACT.BALC<AA.ACT.BAL.ACTIVITY.DATE>,VM,Y.CNT)
*IF FIELD(R.ACT.BALC<AA.ACT.BAL.ACTIVITY.DATE>,VM,Y.CNT) GE '20161001' AND FIELD(R.ACT.BALC<AA.ACT.BAL.ACTIVITY.DATE>,VM,Y.CNT) LE '2016030' THEN
	NO.E.REG = 1
	;*Si el Registro es por Pago de UNC por Pago Anticipado debe ser Excluido : OCORNEJO 05.01.2018
	;*---------------------------------------------------------------------------------------------
	Y.FLG.UNC = 'N'
	GOSUB VALIDA.PAGO.UNC
	IF Y.FLG.UNC EQ 'S' THEN
		RETURN
	END
	;*---------------------------------------------------------------------------------------------
	
	;*Para Extraccion de Seguro en Pago Anticipado : OCORNEJO 04.01.2018
	;*------------------------------------------------------------------
	Y.LOAN = R.ARR<AA.ARR.LINKED.APPL.ID>
	Y.FT   = FIELD(Y.FT.DES,';',1)
	IF Y.FT.ANT NE Y.FT THEN	;*Evitar Duplicar Monto si se Repite la FT
		CALL SLV.GET.UNC.PAY.AMT(Y.LOAN, Y.FT, Y.SEG.VIDA, Y.SEG.DANIO, Y.IVA.SD)
	    IF Y.FT NE '' THEN
	    	PAYMENT.SEG.DEU     += Y.SEG.VIDA ;*Asignando Seguros pagados por Anticipado
	    	PAYMENT.SEG.DAN     += Y.SEG.DANIO
	    	PAYMENT.IVA.SEG.DAN += Y.IVA.SD
	    	Y.FT.ANT = Y.FT
	    END
    END
	;*------------------------------------------------------------------
	
	;*Para Extraccion de Seguro en Reversa de Pago Anticipado : OCORNEJO 13.02.2018
	;*------------------------------------------------------------------
	IF Y.FT.ANT EQ Y.FT AND Y.ACT.FLAG EQ 'R-' THEN
		CALL SLV.GET.UNC.PAY.AMT(Y.LOAN, Y.FT, Y.SEG.VIDA, Y.SEG.DANIO, Y.IVA.SD)
	    IF Y.FT NE '' THEN
	    	PAYMENT.SEG.DEU     += Y.SEG.VIDA ;*Asignando Seguros pagados por Anticipado
	    	PAYMENT.SEG.DAN     += Y.SEG.DANIO
	    	PAYMENT.IVA.SEG.DAN += Y.IVA.SD
	    	Y.FT.ANT = Y.FT
	    END
    END
	;*------------------------------------------------------------------
 	PROPERTY.COUNT = DCOUNT(FIELD(R.ACT.BALC<AA.ACT.BAL.PROPERTY>,VM,Y.CNT),SM)
 	Y.SOBRE.PAGO = 0
	FOR PROPERTY.FOR=1 TO PROPERTY.COUNT
        ;* extraemos las propiedades de capital y sus montos
        FOR CAPITAL.FOR=0 TO DCOUNT(ARR.PARAM.CAPITAL,'|')
            IF FIELD(R.ACT.BALC<AA.ACT.BAL.PROPERTY><1,Y.CNT>,SM,PROPERTY.FOR) EQ FIELD(ARR.PARAM.CAPITAL, '|', CAPITAL.FOR) THEN
                PAYMENT.CAPITAL += FIELD(R.ACT.BALC<AA.ACT.BAL.PROPERTY.AMT><1,Y.CNT>,SM,PROPERTY.FOR)
                IF Y.ACT.FLAG NE 'R-' THEN
                    V.SALDO -=  FIELD(R.ACT.BALC<AA.ACT.BAL.PROPERTY.AMT><1,Y.CNT>,SM,PROPERTY.FOR)
                    IF V.SALDO LT 0 THEN
                    	V.SALDO = 0
                    END
                END
                EXIT
            END
        NEXT CAPITAL.FORFA

        FOR INT.CORR.FOR=0 TO DCOUNT(ARR.PARAM.INT.CORR,'|')
            IF FIELD(R.ACT.BALC<AA.ACT.BAL.PROPERTY><1,Y.CNT>,SM,PROPERTY.FOR) EQ FIELD(ARR.PARAM.INT.CORR, '|', INT.CORR.FOR) THEN
                PAYMENT.INT.PRN += FIELD(R.ACT.BALC<AA.ACT.BAL.PROPERTY.AMT><1,Y.CNT>,SM,PROPERTY.FOR)
                ;*montoRecMesIntCorr
                EXIT
            END
        NEXT INT.CORR.FOR

        FOR INT.CORR.FOR=0 TO DCOUNT(ARR.PARAM.INT.CORR,'|')
            IF FIELD(ARR.PARAM.INT.CORR, '|', INT.CORR.FOR) EQ P_DELPRINCIPALINT THEN
                EXIT
            END
        NEXT INT.CORR.FOR
                
        FOR INT.MORA.FOR=0 TO DCOUNT(ARR.PARAM.INT.MORA,'|')      
            IF FIELD(R.ACT.BALC<AA.ACT.BAL.PROPERTY><1,Y.CNT>,SM,PROPERTY.FOR) EQ FIELD(ARR.PARAM.INT.MORA, '|', INT.MORA.FOR) THEN
                PAYMENT.INT.MOR += FIELD(R.ACT.BALC<AA.ACT.BAL.PROPERTY.AMT><1,Y.CNT>,SM,PROPERTY.FOR)                
                EXIT
            END
        NEXT INT.MORA.FOR


        FOR INT.MORA.FOR=0 TO DCOUNT(ARR.PARAM.INT.MORA,'|')    
            IF FIELD(ARR.PARAM.INT.MORA, '|', INT.MORA.FOR) EQ P_NABPENALTYINT THEN
                PAYMENT.INT.VENC += FIELD(R.ACT.BALC<AA.ACT.BAL.PROPERTY.AMT><1,Y.CNT>,SM,PROPERTY.FOR)
                EXIT
            END
        NEXT INT.MORA.FOR
        
        FOR INT.MORA.FOR=0 TO DCOUNT(ARR.PARAM.INT.MORA,'|')    
            IF FIELD(ARR.PARAM.INT.MORA, '|', INT.MORA.FOR) EQ P_ACCPENALTYINT THEN
                PAYMENT.INT.ACCPENALT += FIELD(R.ACT.BALC<AA.ACT.BAL.PROPERTY.AMT><1,Y.CNT>,SM,PROPERTY.FOR)
                EXIT
            END
        NEXT INT.MORA.FOR
                
        FOR SEG.VIDA.FOR=0 TO DCOUNT(ARR.PARAM.SEG.VIDA,'|')
            IF FIELD(R.ACT.BALC<AA.ACT.BAL.PROPERTY><1,Y.CNT>,SM,PROPERTY.FOR) EQ FIELD(ARR.PARAM.SEG.VIDA, '|', SEG.VIDA.FOR) THEN
                PAYMENT.SEG.DEU += FIELD(R.ACT.BALC<AA.ACT.BAL.PROPERTY.AMT><1,Y.CNT>,SM,PROPERTY.FOR)
				;*montoRecMesCargoSegVida
                EXIT
            END
        NEXT SEG.VIDA.FOR

        FOR SEG.DANIO.FOR=0 TO DCOUNT(ARR.PARAM.SEG.DANIO,'|')
            IF FIELD(R.ACT.BALC<AA.ACT.BAL.PROPERTY><1,Y.CNT>,SM,PROPERTY.FOR) EQ FIELD(ARR.PARAM.SEG.DANIO, '|', SEG.DANIO.FOR) THEN
                PAYMENT.SEG.DAN += FIELD(R.ACT.BALC<AA.ACT.BAL.PROPERTY.AMT><1,Y.CNT>,SM,PROPERTY.FOR)
                ;*montoRecMesCargoSegDano
                EXIT
            END
        NEXT SEG.DANIO.FOR

        FOR SEG.IVA.DANIO.FOR=0 TO DCOUNT(ARR.PARAM.SEG.IVA.DANIO,'|')
            IF FIELD(R.ACT.BALC<AA.ACT.BAL.PROPERTY><1,Y.CNT>,SM,PROPERTY.FOR) EQ FIELD(ARR.PARAM.SEG.IVA.DANIO, '|', SEG.IVA.DANIO.FOR) THEN
                PAYMENT.IVA.SEG.DAN += FIELD(R.ACT.BALC<AA.ACT.BAL.PROPERTY.AMT><1,Y.CNT>,SM,PROPERTY.FOR)
                ;*montoRecMesCargoSegDanoIVA
                EXIT
            END
        NEXT SEG.IVA.DANIO.FOR

		
        V.CUOTA = PAYMENT.CAPITAL+PAYMENT.INT.PRN+PAYMENT.INT.MOR+PAYMENT.SEG.DEU+PAYMENT.SEG.DAN+PAYMENT.IVA.SEG.DAN
		
    NEXT PROPERTY.COUNT

	PMT.CAPITAL     = PAYMENT.CAPITAL   
	PMT.INT.PRN     = PAYMENT.INT.PRN
	PMT.INT.MOR     = PAYMENT.INT.MOR
	PMT.SEG.DEU     = PAYMENT.SEG.DEU
	PMT.SEG.DAN     = PAYMENT.SEG.DAN
	PMT.IVA.SEG.DAN = PAYMENT.IVA.SEG.DAN
	V.CTA           = V.CUOTA
			
    Y.SALDO.TNX = V.SALDO - PAYMENT.CAPITAL
    IF Y.SALDO.TNX LT 0 THEN
    	Y.SALDO.TNX = 0
    END
*    IF Y.PARCIAL EQ 0 AND PROPERTY.COUNT GT 0 AND Y.ACT.FLAG NE 'R+' AND Y.ACT.FLAG NE 'R-' AND Y.REVE.CHILD  NE 'REVE' AND Y.SALDO.TNX LE 0 THEN
*	    Y.SOBRE.PAGO = Y.TOT.PAGO - V.CUOTA
*	    V.CUOTA += Y.SOBRE.PAGO
*	    PAYMENT.CAPITAL += Y.SOBRE.PAGO
*	END
END
RETURN

REGISTRAR_OPERACION:
IF FIELD(R.ACT.BALC<AA.ACT.BAL.ACTIVITY.DATE>,VM,Y.CNT) GE FECHA.INI.SSF AND FIELD(R.ACT.BALC<AA.ACT.BAL.ACTIVITY.DATE>,VM,Y.CNT) LE FECHA.FIN.SSF THEN
*IF FIELD(R.ACT.BALC<AA.ACT.BAL.ACTIVITY.DATE>,VM,Y.CNT) GE '20161001' AND FIELD(R.ACT.BALC<AA.ACT.BAL.ACTIVITY.DATE>,VM,Y.CNT) LE '20161030' THEN
*------------------------------------------------------------------------
* Ingresa una nueva transacion en el estado de cuentas.
*------------------------------------------------------------------------
	;* Consideraciones de impresion según la operación efectuada
	AMT.AMOUNT 	= R.AA.TERM<1,AA.AMT.AMOUNT>
	AMT.TERM 	= R.AA.TERM<1,AA.AMT.TERM>	
	
	IF Y.ACT.FLAG EQ 'T' THEN ;* Transacciones 
		V.MONTO		= ''
	END
    
	STR.ARR = ''

* 10546 - S	
	STR.ARR = V.DATETIME :"_":Y.ACT.FLAG:"#"
	STR.ARR := V.REFERENCIA:"*"

* 10546 - E
    STR.ARR:=V.FECHA.APLICACION:"*" ;* se replica por le momento
    STR.ARR:=V.FECHA.VALOR:"*"
    STR.ARR:=PAYMENT.CANAL:"*"
    
    IF Y.ACT.FLAG EQ 'R-' THEN
    	STR.ARR:='-':PAYMENT.INT.MOR:"*"
	    STR.ARR:='-':PAYMENT.INT.PRN:"*"
	    STR.ARR:='-':PAYMENT.CAPITAL:"*"
	    STR.ARR:='-':PAYMENT.SEG.DEU:"*"
	    STR.ARR:='-':PAYMENT.SEG.DAN:"*"
	    STR.ARR:='-':PAYMENT.IVA.SEG.DAN:"*"
	    STR.ARR:='-':V.CUOTA:"*"
	    V.SALDO -= '-':PAYMENT.CAPITAL
    END ELSE
        STR.ARR:=PAYMENT.INT.MOR:"*"
	    STR.ARR:=PAYMENT.INT.PRN:"*"
	    STR.ARR:=PAYMENT.CAPITAL:"*"
	    STR.ARR:=PAYMENT.SEG.DEU:"*"
	    STR.ARR:=PAYMENT.SEG.DAN:"*"
	    STR.ARR:=PAYMENT.IVA.SEG.DAN:"*"
	    STR.ARR:=V.CUOTA:"*"
    END

    STR.ARR:=V.SALDO:"*"
    STR.ARR:=V.MONTO:"*"
    STR.ARR:=AMT.AMOUNT:"*"
    STR.ARR:=AMT.TERM:"*"
    STR.ARR:=V.INTEREST.RATE:"*"
    STR.ARR:=V.INTEREST.MORA:"*"
    
	;***CORR0287 Starts
    IF Y.ACT.FLAG EQ 'R-' OR Y.ACT.FLAG EQ 'R+' THEN
        STR.ARR:="REVE*"
    END ELSE
        STR.ARR:="*"
    END
	;***CORR0287 Ends;
	
	STR.ARR:=Y.FT.DES:"*"
    STR.ARR:=PAYMENT.INT.VENC:"*"
    STR.ARR:=PAYMENT.INT.ACCPENALT:"*"
    STR.ARR:=FECHA.RMES
    A.LOA.INFO<-1> = STR.ARR
*    CRT STR.ARR
    
*    DATE.REG=STR.ARR
    STR.ARR = ''

END
RETURN

*-----------------------------------------------------------------------------
LIMPIAR_DATOS:
*-----------------------------------------------------------------------------
* Resetea los valores previo a setearlos con los datos de las operaciones.
*-----------------------------------------------------------------------------
    PAYMENT.INT.MOR 	= 0.00 ;* Pago de interes moratorios
    PAYMENT.INT.PRN 	= 0.00 ;* Pago de interes
    PAYMENT.CAPITAL		= 0.00 ;* Pago de capital
    PAYMENT.SEG.DEU 	= 0.00 ;* Seguro de deuda
    PAYMENT.SEG.DAN 	= 0.00 ;* Seguro de danios
    PAYMENT.IVA.SEG.DAN = 0.00 ;* IVA por seguro de danios
    V.CUOTA				= 0.00 ;* PAGO A CAPITAL
    Y.TOT.PAGO			= 0.00 ;* Pago total de cuota
    R.ARR.ACT.MASTER	= ''   ;* Informacion actividad master

RETURN 

*-----------------------------------------------------------------------------
VALIDA.PAGO.UNC:
*-----------------------------------------------------------------------------
	;*Buscar Pago en LIVE sino en Historico
	Y.FT = FIELD(Y.FT.DES,';',1)
	CALL F.READ(FN.FT, Y.FT, R.FT.UNC, F.FT, ERR.FT)
	IF ERR.FT THEN
		CALL F.READ.HISTORY(FN.FT$HIS, Y.FT, R.FT.UNC, F.FT$HIS, ERR.FT.HIS)
	END
	
	;*Si el Pago es UNC Ignorar el Registro
	IF R.FT.UNC<FT.ORDERING.BANK> EQ 'UNC' THEN
		Y.FLG.UNC = 'S'
	END
RETURN

CARGAR.FRM.PAGO:
FPAGO.CONTA = 0
IF FPAGO.CANT GT 1 THEN
	NTPI = R.TFS<TFS.TRANSACTION.INT><1,1>
	FOR K=1 TO FPAGO.CANT
		NTP = R.TFS<TFS.TRANSACTION.INT><1,K>
		IF NTPI NE NTP THEN
			FPAGO.CONTA = FPAGO.CONTA + 1
			BND = BND + 1
			TFS.1 = R.TFS<TFS.AMOUNT><1,1>
			TFS.2 = R.TFS<TFS.AMOUNT><1,K> 
			CRT 'TFS.1:':TFS.1
			CRT 'TFS.2:':TFS.2
			
		END
	NEXT K
	
	IF FPAGO.CONTA EQ 0 THEN
		FPAGO.CANT = 1
		BND = BND - FPAGO.CONTA
	END
END
RETURN

OBT.ID.PERSONA.TFS:
CALL AA.GET.ARRANGEMENT.CONDITIONS(V.REFERENCIA, 'CUSTOMER', '', TODAY, RETURN.IDS, RETURN.VALUES.CUST, RETURN.ER)

IF FT.ID.PERSONA NE '' THEN
	IF RETURN.VALUES.CUST<1,AA.CUS.OWNER> NE '' THEN
		ORIGEN.RECU = '2'
		
		VAR3=RAISE(RETURN.VALUES.CUST<1,AA.CUS.OTHER.PARTY>)
		FINDSTR FT.ID.PERSONA IN VAR3 SETTING Ap, Vp THEN
			CRT 'ROL>>':RETURN.VALUES.CUST<1,AA.CUS.ROLE,Vp>
		END ELSE
			ORIGEN.RECU = '5'
		END			
	END	
END ELSE
	CALL F.READ(FN.TFS,TFS.ID.PERSONA, R.TFS, F.TFS, Y.ERR)
	FORMA.PAGO=R.TFS<TFS.TRANSACTION.INT>

	CALL GET.LOC.REF('TELLER.FINANCIAL.SERVICES','LF.DOC.CLIEN.EX', LocPosDui)

	VAR2=R.TFS<TFS.LOCAL.REF,LocPosDui>
	
	IF VAR2 EQ '' THEN
		ORIGEN.RECU = '5'
	END ELSE
		CALL F.READ(FN.D.CUST, VAR2, R.D.CUST, F.D.CUST, Y.ERR)
		IF R.D.CUST EQ '' THEN
			CRT 'ORIGEN.RECU>> 5'
		END ELSE
		
			IF RETURN.VALUES.CUST<1,AA.CUS.OWNER> NE '' THEN
				ORIGEN.RECU = '2'
				VAR3=RAISE(RETURN.VALUES.CUST<1,AA.CUS.OTHER.PARTY>)
				
				FINDSTR VAR2 IN VAR3 SETTING Ap, Vp THEN
					CRT 'ROL>>':RETURN.VALUES.CUST<1,AA.CUS.ROLE,Vp>
				END ELSE
					ORIGEN.RECU = '5'
				END			
			END
		END
	END	
END

RETURN

*----------------------------------------------------------------------------
*PRELACION.PAGO:
*----------------------------------------------------------------------------

GUARDAR.DATOS.APP:
	IF FIELD(DATE.REG,';',2) NE '' THEN
		MNT.NVO=0.0
		FECHAYHORA = TIMEDATE()
		
	    CRT '***************************************************'
		CRT 'DATE.REG>>':DATE.REG
		CRT '***************************************************'
		ID.REF = FIELD(DATE.REG,';',2):'-':TODAY:'-':FECHAYHORA[1,8]:RND(99)
		
		R.DATA.SSF<EB.SLV76.NO.NIT>          = FIELD(DATE.REG,';',1)
		R.DATA.SSF<EB.SLV76.NO.REFERENCIA>   = FIELD(DATE.REG,';',2)
		R.DATA.SSF<EB.SLV76.COD.CARTERA>     = FIELD(DATE.REG,';',3)
		R.DATA.SSF<EB.SLV76.COD.ACTIVO>      = FIELD(DATE.REG,';',4)
		R.DATA.SSF<EB.SLV76.MNT.REC.MES>     = FIELD(DATE.REG,';',5)	
		R.DATA.SSF<EB.SLV76.MNT.REC.MES.CAP> = FIELD(DATE.REG,';',6)
		R.DATA.SSF<EB.SLV76.MNT.RMES.INTM>   = FIELD(DATE.REG,';',7) 
		R.DATA.SSF<EB.SLV76.MNT.RMES.INTV>   = FIELD(DATE.REG,';',8)
		R.DATA.SSF<EB.SLV76.MNT.RMES.INTC>   = FIELD(DATE.REG,';',9) 
		R.DATA.SSF<EB.SLV76.MNT.RMES.SEGD>   = FIELD(DATE.REG,';',10)
		R.DATA.SSF<EB.SLV76.MNT.RMES.CAPM>   = FIELD(DATE.REG,';',11)
		R.DATA.SSF<EB.SLV76.MNT.RMES.SEGDI>  = FIELD(DATE.REG,';',12)
		R.DATA.SSF<EB.SLV76.MNT.RMES.SEGV>   = FIELD(DATE.REG,';',13)
		R.DATA.SSF<EB.SLV76.MNT.RMES.CPROC>  = FIELD(DATE.REG,';',14)
		R.DATA.SSF<EB.SLV76.FRM.REC.MES>     = FIELD(DATE.REG,';',15)
		R.DATA.SSF<EB.SLV76.FCH.REC.MES>     = FIELD(DATE.REG,';',16)
		R.DATA.SSF<EB.SLV76.ORI.REC.MES>     = FIELD(DATE.REG,';',17)
		R.DATA.SSF<EB.SLV76.NOM.VENT.CART>   = FIELD(DATE.REG,';',18)
		R.DATA.SSF<EB.SLV76.ID.CLIENTE>      = FIELD(DATE.REG,';',19)
		R.DATA.SSF<EB.SLV76.NOMBRE.CLIENTE>  = FIELD(DATE.REG,';',20)
		R.DATA.SSF<EB.SLV76.CTA.ESTADO>      = FIELD(DATE.REG,';',21)
				
	    CALL F.WRITE(FN.LEND.SSF,ID.REF,R.DATA.SSF)
	    DATE.REG=''
	    
		PAGO.TTL = 0.0
		PAGO.K  = 0.0
		PAGO.I  = 0.0
		PAGO.IC  = 0.0
		PAGO.SDA = 0.0
		PAYMENT.INT.VENC = 0.0
		PAGO.ISD = 0.0
		PAGO.SDE = 0.0
		REC.MES.CAP.MORA = 0.0
	END
RETURN
END
