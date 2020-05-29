*-----------------------------------------------------------------------------
* <Rating>-43</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.B.EXP.REC.LEND.LOAD
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_SLV.B.EXP.REC.LEND.COMMON
*-----------------------------------------------------------------------------
    GOSUB INIT
    GOSUB OPENFILE
*-----------------------------------------------------------------------------
INIT:
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
*    LOCATE "REFERENCIA" IN D.FIELDS<1> SETTING PAR.POS.1 THEN
*    	V.REFERENCIA = D.RANGE.AND.VALUE<PAR.POS.1>
*    END
	;* ************************************************************************************ DEBUG  ************
*V.REFERENCIA = 'AA15308S16P7';*'AA15281BPHGT';*'AA16270KV698' ;*'AA15308S16P7' ;*'AA15273CV909'
******************************************** /DEBUG ************

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
RETURN

OPENFILE:;* Definicion de las aplicaciones
    FN.ACT.BALC 	= 'F.AA.ACTIVITY.BALANCES'
    F.ACT.BALC  	= ''
	CALL OPF(FN.ACT.BALC, F.ACT.BALC)
	
    FN.ACT.HIST 	= 'F.AA.ACTIVITY.HISTORY'
    F.ACT.HIST  	= ''
	CALL OPF(FN.ACT.HIST, F.ACT.HIST)
	
    FN.ARR.ACT 		= 'F.AA.ARRANGEMENT.ACTIVITY'
    F.ARR.ACT  		= ''
    CALL OPF(FN.ARR.ACT, F.ARR.ACT)
*TCIB-SIT-8989-S    
    FN.ARR.ACT.NAU 		= 'F.AA.ARRANGEMENT.ACTIVITY$NAU'
    F.ARR.ACT.NAU  		= ''
    CALL OPF(FN.ARR.ACT.NAU, F.ARR.ACT.NAU)
*TCIB-SIT-8989-E  
    FN.AA.ARR.HIS 	= 'F.AA.ARRANGEMENT.ACTIVITY$HIS'
    F.AA.ARR.HIS  	= ''
    CALL OPF(FN.AA.ARR.HIS, F.AA.ARR.HIS)

    FN.PARAM 		= 'F.EB.SLV.GLOBAL.PARAM'
    F.PARAM  		= ''
    CALL OPF(FN.PARAM, F.PARAM)

    FN.STMT.ENTRY 	= 'F.STMT.ENTRY'
    F.STMT.ENTRY  	= ''
    CALL OPF(FN.STMT.ENTRY, F.STMT.ENTRY)

    FN.EB.BALANCES 	= 'F.EB.CONTRACT.BALANCES'
    F.EB.BALANCES  	= ''
    CALL OPF(FN.EB.BALANCES, F.EB.BALANCES)

    FN.ACC			= 'F.ACCOUNT'
    F.ACC			= ''
    CALL OPF(FN.ACC, F.ACC)
    
    FN.FT			= 'F.FUNDS.TRANSFER'
    F.FT			= ''
    CALL OPF(FN.FT, F.FT)
    
    FN.FT$HIS		= 'F.FUNDS.TRANSFER$HIS'
    F.FT$HIS 		= ''
    CALL OPF(FN.FT$HIS, F.FT$HIS)

	;*** CORR0287 Starts
    FN.SLV.AA.ACT.REVE.BAL.DET 	= 'F.SLV.AA.ACT.REVE.BAL.DET'
    F.SLV.AA.ACT.REVE.BAL.DET 	= ''
    CALL OPF(FN.SLV.AA.ACT.REVE.BAL.DET, F.SLV.AA.ACT.REVE.BAL.DET)
    
    FN.TFS = 'FBNK.TELLER.FINANCIAL.SERVICES'
    F.TFS = ''
    CALL OPF(FN.TFS, F.TFS)

	FN.ARRANGEMENT		= 'FBNK.AA.ARRANGEMENT'
    F.ARRANGEMENT 		= ''
    CALL OPF(FN.ARRANGEMENT, F.ARRANGEMENT)

	FN.FT.BM  	= 'FBNK.FT.BULK.MASTER'
	F.FT.BM	    = ''
	CALL OPF(FN.FT.BM, F.FT.BM)

	FN.CUSTOMER			= 'FBNK.CUSTOMER'
	F.CUSTOMER			= ''
	CALL OPF(FN.CUSTOMER, F.CUSTOMER)
	
	FN.D.CUST  	= 'F.SLV.DUI.CUSTOMER.CNT'
	F.D.CUST    = ''
	CALL OPF(FN.D.CUST, F.D.CUST)
	
	FN.LEND.SSF  = 'F.EB.SLV.LEND.REC.SSF'
	F.LEND.SSF   = ''
	CALL OPF(FN.LEND.SSF, F.LEND.SSF)
	
	FN.FECH.SSF = 'F.EB.SLV.CLS.FECH.REC.SSF'
	F.FECH.SSF = ''
	CALL OPF(FN.FECH.SSF, F.FECH.SSF)
RETURN

END

