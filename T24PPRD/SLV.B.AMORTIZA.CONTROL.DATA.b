*-----------------------------------------------------------------------------
* <Rating>471</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE SLV.B.AMORTIZA.CONTROL.DATA
*-----------------------------------------------------------------------------
* 
*-----------------------------------------------------------------------------
* Modification History :
*------------------------------------------------- ----------------------------
    $INSERT I_COMMON 
    $INSERT I_EQUATE
    $INSERT I_F.EB.SLV.AMORTIZA.COM.CONTROL
    $INSERT I_F.AA.ARRANGEMENT.ACTIVITY
    $INSERT I_F.AA.TERM.AMOUNT
*----------------------------------------------------------------------------- 
    GOSUB INIT
    GOSUB OPENFILE
    GOSUB PROCESS
    RETURN 

INIT:
    FN.AMORTZ	= 'F.EB.SLV.AMORTIZA.COM.CONTROL'
    F.AMORTZ 	= ''
    
    FN.ARR.ACT 		= 'F.AA.ARRANGEMENT.ACTIVITY'
    F.ARR.ACT  		= ''
    
    EndLine = '|'

    EQU localZone TO 'America/El_Salvador'

;*Definicion de variables
;*-----------------------
	DIR.NAME		= 'SIC'
*	DIR.NAME		= 'C:\Users\mamenjivar\Desktop\TEST\LoanComissions2\'
    NAME.FILE		= ''
    Y.DATETIME 		= ''
    ARR.DATA		= ''
    PROCESS.DATE = TODAY
    DAY.COUNT = "-1C"
    CALL CDT('', PROCESS.DATE, DAY.COUNT)
 	DATE.TODAY = PROCESS.DATE

    RETURN

OPENFILE:
;* Apertura de Archivos
;*----------------------
    CALL OPF(FN.AMORTZ,F.AMORTZ)
    CALL OPF(FN.ARR.ACT, F.ARR.ACT)
    RETURN

PROCESS:

	GOSUB GET.LOAN.COMISSIONS
	GOSUB GET.LOAN.DISBURSEMENTS 
    RETURN

GET.LOAN.COMISSIONS:

	ARR.DATA = ''
	
	;*Query para traer información
    STMT.AMORT.CONTROL	= "SELECT " : FN.AMORTZ : " WITH DISBURST.DATE EQ " : DATE.TODAY ;*Trayendo todos los creditos desembolsados del dia
*	STMT.AMORT.CONTROL	= "SELECT " : FN.AMORTZ : " WITH DISBURST.DATE EQ 20160407"
*	STMT.AMORT.CONTROL	= "SELECT " : FN.AMORTZ
	;*Leyendo ids
    CALL EB.READLIST(STMT.AMORT.CONTROL, LIST.AMORT.IDS, '', NO.OF.RECS, Y.ERROR)

    IF NO.OF.RECS NE 0 THEN
        FOR LK = 1 TO NO.OF.RECS
            ;*Leyendo información de comisiones
            CALL F.READ(FN.AMORTZ, LIST.AMORT.IDS<LK>, REC.AMORT, F.AMORTZ, Y.ERROR2)

            IdArrangement 		= LIST.AMORT.IDS<LK>								;*Id de Arrangement
            Producto 	   		= REC.AMORT<EB.SLV63.PRODUCT>						;*Id de Producto
            Term				= REC.AMORT<EB.SLV63.TERM>							;*Plazo en Meses
            Customer			= REC.AMORT<EB.SLV63.CUSTOMER>						;*Id del Cliente
            GrantComPercent 	= REC.AMORT<EB.SLV63.GRANT.COM.PERCENT>				;*Porcentaje de Comisión Otrogado
            GrantComAmount		= REC.AMORT<EB.SLV63.GRANT.COM.AMOUNT>				;*Monto de Comisión Otrogada
            DefComBalance		= REC.AMORT<EB.SLV63.DEF.COM.BALANCE>				;*Saldo de Comisión Diferida
            AmrComAmount		= REC.AMORT<EB.SLV63.AMR.COM.AMOUNT>				;*Monto de Comisión Amortizada
            FixedCostCashed		= REC.AMORT<EB.SLV63.FIXED.COST.CASHED>				;*Costo Fijo Cobrado en Deseembolso
            CurrBalance			= REC.AMORT<EB.SLV63.CURR.BALANCE>					;*Saldo Actual
            LastStatusLoan		= REC.AMORT<EB.SLV63.LAST.STATUS.LOAN>				;*Ultimo Estado del Prestamo
            CurrStatusLoan		= REC.AMORT<EB.SLV63.CURR.STATUS.LOAN>				;*Estado Actual del Prestamo
            ;*--------------------------------------------------------------
            ;*Comisiones y limites sugeridos por sistema.
            ;*Se llama a Rutina SLV.NOF.GET.COMMISSION.PARAM(P.PRODUCT , P.AMOUNT) para obtener Producto, Charge Type, % Ch0arged, Cargo Min, Cargo Max, Costo Fijo, Tasa Maxima
            ARR.RES		= ''
            CALL SLV.NOF.GET.COMMISSION.PARAM(Producto , DisburstAmount, ARR.RES)
            ;*STRING RETORNADA EJM: HIP.NUEVA.EMPLEADO*CPEFE*.05*225.00*900.00*207.46*15.40*
            ChargeType		= 	FIELD(ARR.RES, "*", 2)								;*Tipo de Comisión aplicada
            PercenCharge	=	FIELD(ARR.RES, "*", 3)								;*Porcentaje de Comisión sugerida por sistema
            ChargeMin		=	FIELD(ARR.RES, "*", 4)								;*Cargo Minimo
            ChargeMax		=	FIELD(ARR.RES, "*", 5)								;*Cargo Maximo
            CostoFijo		=	FIELD(ARR.RES, "*", 6)								;*Costo Fijo
            TasaMaxima		=	FIELD(ARR.RES, "*", 7)								;*Tasa Maxima
            ;*--------------------------------------------------------------

            ;*Campos Auditoria
            ;*--------------------------------------------------------------
            Inputter 		= FIELD(REC.AMORT<EB.SLV63.INPUTTER>, "_", 2) 			;*Digitador

            ;* Personalizar DateTime Field
            ;*-----------------------------------------------------------------
            Y.DATETIME		= REC.AMORT<EB.SLV63.DATE.TIME>
            GOSUB PARSE.DATETIME
            ;*-----------------------------------------------------------------
            DateTime 		= Y.DATETIME											;*Fecha y Hora del Registro
            Authorizer 		= FIELD(REC.AMORT<EB.SLV63.AUTHORISER>,"_",2)			;*Autorizador
            Company 		= REC.AMORT<EB.SLV63.CO.CODE> 							;*Agencia
            CurrNum 		= REC.AMORT<EB.SLV63.CURR.NO> 							;*Numero Actual del Registro

            ;* Obtener ultimo valor de campo OVERRIDE
            ;*----------------------------------------
            OVER = DCOUNT(REC.AMORT<EB.SLV63.OVERRIDE>, SM)
            Override = REC.AMORT<EB.SLV63.OVERRIDE><1,OVER>			
            ;*Generando arreglo con información
            STR.ARR := IdArrangement	: ";"	;*1
            STR.ARR := Producto			: ";"	;*2
            STR.ARR := Term				: ";"	;*3
            STR.ARR := Customer			: ";"	;*4
            STR.ARR := GrantComPercent	: ";"	;*5
            STR.ARR := GrantComAmount	: ";"	;*6
            STR.ARR := DefComBalance	: ";"	;*7
            STR.ARR := AmrComAmount		: ";"	;*8
            STR.ARR := FixedCostCashed	: ";"	;*9
            STR.ARR := CurrBalance		: ";"	;*10
            STR.ARR := LastStatusLoan	: ";"	;*11
            STR.ARR := CurrStatusLoan	: ";"	;*12
            STR.ARR := ChargeType		: ";"	;*13
            STR.ARR := PercenCharge		: ";"	;*14
            STR.ARR := ChargeMin		: ";"	;*15
            STR.ARR := ChargeMax		: ";"	;*16
            STR.ARR := CostoFijo		: ";"	;*17
            STR.ARR := TasaMaxima		: ";"	;*18
            STR.ARR := Inputter			: ";"	;*19
            STR.ARR := DateTime			: ";"	;*20
            STR.ARR := Authorizer		: ";"	;*21
            STR.ARR := Company			: ";"	;*22
            STR.ARR := CurrNum			: ";"	;*23
            STR.ARR := Override					;*24
            STR.ARR := EndLine					;*25

            ;************************************************'
            ARR.DATA<LK> = STR.ARR
            STR.ARR 	 = ''
            GOSUB SET.VARIABLES
        NEXT LK
        NAME.FILE 		 = '_T24LoanCommissions.' : DATE.TODAY : '.csv'
        GOSUB WRITE.FILE.CSV
    END

    RETURN
GET.LOAN.DISBURSEMENTS:
	
	ARR.DATA = ''
	;* Ejecuta consulta sobre AA.ARRANGEMENT.ACTIVITY buscando desembolsos para el arrangement
    Y.STMT.DES = 'SELECT FBNK.AA.ARRANGEMENT.ACTIVITY WITH ACTIVITY EQ LENDING-DISBURSE-COMMITMENT';* :' AND EFFECTIVE.DATE EQ ': DATE.TODAY
    CALL EB.READLIST(Y.STMT.DES, DES.LIST, '', DES.REC, DES.ERR)

    IF DES.REC GT 0 THEN
        FOR LK = 1 TO DES.REC
            CALL F.READ(FN.ARR.ACT, DES.LIST<LK>, R.AR.ACT, F.ARR.ACT, ERR.AR.ACT)
			
			DisbursementId 		= DES.LIST<LK>
            IdArrangement 		= R.AR.ACT<AA.ARR.ACT.ARRANGEMENT>
            DisburstDate		= R.AR.ACT<AA.ARR.ACT.EFFECTIVE.DATE>				;* Fecha otorgamiento de Deseembolso YYYYMMDD
            DisburstAmount	    = R.AR.ACT<AA.ARR.ACT.TXN.AMOUNT>					;* Monto Otrogado en Deseembolso
            Customer			= R.AR.ACT<AA.ARR.ACT.CUSTOMER>						;* Id del Cliente
            Inputter 			= FIELD(R.AR.ACT<AA.ARR.ACT.INPUTTER>, "_", 2) 		;* Digitador

            Authorizer 			= FIELD(R.AR.ACT<AA.ARR.ACT.AUTHORISER>,"_",2)		;* Autorizador
            Company 			= R.AR.ACT<AA.ARR.ACT.CO.CODE> 						;* Agencia
            CALL AA.GET.ARRANGEMENT.CONDITIONS(IdArrangement, 'TERM.AMOUNT', 'COMMITMENT', TODAY, RETURN.IDS, RETURN.VALUES.TERM, RETURN.ER)
            
            FullDisbursement	= RETURN.VALUES.TERM<1,AA.AMT.PR.VALUE>				;* Desembolso Parcial o Completo
            OVER 				= DCOUNT(R.AR.ACT<AA.ARR.ACT.OVERRIDE>, SM)
            Override 			= R.AR.ACT<AA.ARR.ACT.OVERRIDE><1,OVER>				;* Override
						

			;*Generando arreglo con informacióny
            
            STR.ARR := DisbursementId	: ";"	;*1
            STR.ARR := IdArrangement	: ";"	;*2
            STR.ARR := DisburstDate		: ";"	;*3
            STR.ARR := DisburstAmount	: ";"	;*4
            STR.ARR := Customer			: ";"	;*5
            STR.ARR := Inputter			: ";"	;*6
            STR.ARR := Authorizer		: ";"	;*7
            STR.ARR := Company			: ";"	;*8
            STR.ARR := FullDisbursement	: ";"	;*9
            STR.ARR := Override					;*10
 			STR.ARR := EndLine					;*11
 			
            ;************************************************' 
            CREATED.FILES ++
            ARR.DATA<CREATED.FILES> = STR.ARR
            STR.ARR 	 	= ''
	 
    		GOSUB SET.VARIABLES
    	NEXT LK    	
    	NAME.FILE 		 = '_T24LoanDisbursement.' : DATE.TODAY : '.csv'
    	GOSUB WRITE.FILE.CSV
    END
	
RETURN

WRITE.FILE.CSV:
	;*Eliminando archivo si existe
    DELETESEQ DIR.NAME, NAME.FILE THEN
    END

	;*Abriendo archivo para escritura
    OPENSEQ DIR.NAME, NAME.FILE TO SEQ.PTR THEN
        WEOFSEQ NAME.FILE
    END

    FOR LM = 1 TO LK
        ;*Escribiendo Data
        IF LM < LK THEN
            WRITESEQ ARR.DATA<LM> APPEND TO SEQ.PTR THEN ;*con retorno de carro
        	END
    	END
    	IF LM = LK THEN
       		WRITEBLK ARR.DATA<LM> ON SEQ.PTR THEN	;*sin retorno de carro
    		END
    	END
    NEXT LM

;*Cerrando archivo
    CLOSESEQ SEQ.PTR

    CRT 'Cantidad de registros escritos: ' : LM
RETURN


PARSE.DATETIME:
    utcDateTime =  Y.DATETIME
    UTC.FLAG = ''
;*Evaluar UTC Time or Standard Time

    FINDSTR "." IN utcDateTime SETTING Ap, Vp THEN
        UTC.FLAG = '1'
    END

    IF UTC.FLAG EQ '1' THEN
        localZoneDate1 = OCONV(LOCALDATE(utcDateTime,localZone),'D4/E')
        localZoneTime1= OCONV(LOCALTIME(utcDateTime,localZone),'MTS')
        ;*16/07/2016 12:17:01 --dia/mes/año devuelve se modifica para enviar (AÑOMESDIA 12:17:00)
        Y.DATETIME = localZoneDate1[7,4]:localZoneDate1[4,2]:localZoneDate1[1,2]:' ':localZoneTime1
    END
    ELSE
    Y.DAY.BC = utcDateTime[3,2]
    Y.MONTH.BC = utcDateTime[5,2]
    Y.YEAR.BC = utcDateTime[1,2]
    Y.DATE.BC = OCONV(ICONV(utcDateTime[1,6],'D'),'D4/E')
;*07/16/2016 12:17:01 --mes/dia/año devuelve se modifica para enviar (AÑOMESDIA 12:17:00)
    Y.DATE.BC2 = Y.DATE.BC[7,4]:Y.DATE.BC[1,2]:Y.DATE.BC[4,2] ;*AÑOMESDIA
    Y.TIME.BC = utcDateTime[7,2]:':':utcDateTime[9,2]:':':'00'
    Y.DATETIME = Y.DATE.BC2: ' ': Y.TIME.BC
    END
RETURN

SET.VARIABLES:
;*Se inicializan/limpian las variables que contienen la informacion para evitar data erronea dentro del ciclo
;*Generando arreglo con información
	DisbursementId  = ''
    IdArrangement	= ''
    Producto		= ''
    Term			= ''
    Customer		= ''
    GrantComPercent	= ''
    GrantComAmount	= ''
    DefComBalance	= ''
    AmrComAmount	= ''
    FixedCostCashed	= ''
    DisburstDate	= ''
    DisburstAmount	= ''
    CurrBalance		= ''
    LastStatusLoan	= ''
    CurrStatusLoan	= ''
    ChargeType		= ''
    PercenCharge	= ''
    ChargeMin		= ''
    ChargeMax		= ''
    CostoFijo		= ''
    TasaMaxima		= ''
    Inputter		= ''
    DateTime		= ''
    Authorizer		= ''
    Company			= ''
    CurrNum			= ''
    Override		= ''
    FullDisbursement= ''
RETURN

END

    
