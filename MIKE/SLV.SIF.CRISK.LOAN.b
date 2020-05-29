*-----------------------------------------------------------------------------
* <Rating>2175</Rating>
*-----------------------------------------------------------------------------
*Name : SLV.SIF.CRISK.LOAN
*-----------------------------------------------------------------------------
*Description: Extrae datos financieros para creditos
*-----------------------------------------------------------------------------
* Modification History :
* Autor    	Fecha     	 SIF			Version
*-----------------------------------------------------------------------------
* GAlfaro  	29.07.2014 					Initial Code
* Galfaro	07.04.2015	 v9				Agregar valores de seguros provenientes de EB.CONTRACT.BALANCE
*						 v9				Incluir el calculo de la fecha de ultimo pago de intereS
* Galfaro	16.04.2015   v9				Se modifica para utilizar la ultima fecha de cierre de la tabla DATES
* Galfaro	20.04.2015   v9     		implementacion de AA.BILL.DETAIL Para extraer las cuotas en gracia
* Galfaro	22.04.2015   v9     		agregar campos de capital vencido e interes vencido
* Galfaro	12.05.2015	 v9      		CODIGOCARTERA			=	R.REG.INFO<SLV.AA.REG.COD.CARTERA>
*		            			 		FREQUENCIA DE PAGO= FIELD(ARR_SCHED_FIELD_LOA<AA.PS.PAYMENT.FREQ>,@VM,1)
*										Se agrega el tipo de producto ARR_LOCAL_FIELD_ACC<AA.AC.ACCOUNT.TITLE.1>
*						 v9.6   		Se agrega la tasa efectiva     
*Galfaro	27.05.2015   v9.6b 9.3ap 	Definir vencidos en mas de 90 dias de atraso
*Galfaro	28.05.2015	 v9.7b 9.3ap 	Agregar tratamiento estado DEL en 6 dias
*Galfaro	28.06.2015   v9.9b 9.4ap 	Agregar tratamiento para nuevo campo de orden de descuento	
*Galfaro	06.06.2015	 v9.10b 9.4ap	 SALDOMORAK				=   R.REG.INFO<SLV.AA.REG.SALDO.MORA.K>
*            							 SALDOMORAI				=	R.REG.INFO<SLV.AA.REG.SALDO.MORA.I>
*Galfaro	08.06.2015					ASIGNAR ABONO DEPOSITO PRINCIPALPREPAYMENT		=	R.REG.INFO<SLV.AA.REG.ABONO.DEPOSITO>
*Galfaro	09.06.2015					extraer el dia de pago de capita e interes de R.AA.ACC.DET.VAL<AA.AD.PAYMENT.START.DATE>[7,2]	
*										Agregar para descontar seguro de la cuota para n417 N_PAYMENTAMOUNT
*Galfaro	10.06.2015	v9.12b	9.4ap	Modificar para no aplicar gracia de capital 
*Galfaro	11.06.2015  v9.12b	9.4ap	Agregar creditos cancelados en el mes de proceso
*										Modificar CLOSEDATE			=	D_DATELOANCLOSE
*										modificar para tomar el estdo del credito segun sistema para tratarlo en SIF LOANSTATUSID=R.ARRANGEMENT<AA.ARR.ARR.STATUS>
*										Evaluar dias de gracias a partir del segundo dia
*Galfaro	18.06.2015	v9.13b	9.4ap	Cambiar logica para tomar seguro de AA.BILL.DETAILS
*Galfaro	22.06.2015  v9.13b  9.4ap   Se modifica para tomas inicamente las cuotas de seguro con un dia o mas de mora
*RGaray		19.06.2015	v9.14b	9.4ap	Se agrega la extraccón del campo para actualizacón de Buró.
*Galfaro	22.06.2015	v9.14b	9.4ap	Se agrega la extraccón de campos FECHACASTIGO			=	R.REG.INFO<SLV.AA.REG.FECHA.CASTIGO>
*            							ULTIMAFECHAVENC			=	R.REG.INFO<SLV.AA.REG.UTIMA.FECHA.VENC>
*            							FECHADEMANDA 			=	R.REG.INFO<SLV.AA.REG.FECHA.DEMANDA>   
*Galfaro	10.07.2015	v9.19b  9.4ap	Modificar para tratamiento de campo para capital												
*-----------------------------------------------------------------------------
SUBROUTINE SLV.SIF.CRISK.LOAN
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.AA.ARRANGEMENT
    $INSERT I_F.AA.INTEREST
    
    $INSERT I_F.AA.CUSTOMER
    $INSERT I_F.CUSTOMER
    $INSERT I_F.AA.TERM.AMOUNT
*    $INSERT I_F.INDUSTRY
    $INSERT I_F.EB.CONTRACT.BALANCES
    $INSERT I_F.AA.PRODUCT.DESIGNER
    $INSERT I_F.SLV.REGULATORY.INFO
    $INSERT I_F.ACCOUNT
    $INSERT I_F.AA.ACCOUNT
    $INSERT I_F.AA.ACCOUNT.DETAILS
    $INSERT I_F.AA.PAYMENT.SCHEDULE
*    $INSERT I_F.SLV.RISK.ASSET.TYPE
    $INSERT I_F.DATES
    $INSERT I_F.AA.BILL.DETAILS
    $INSERT I_F.AA.CHARGE
    $INSERT I_F.AA.ACCOUNT.DETAILS
    $INSERT I_F.AA.ACTIVITY.HISTORY
    
    $INSERT I_RULES

*-----------------------------------------------------------------------------
*
    GOSUB INIT
    GOSUB OPENFILES
    GOSUB PROCESS

*
    RETURN
INIT:
*** <region>
* 	Definicion de tablas
    FN.ARRANGEMENT		= 'F.AA.ARRANGEMENT'
    F.ARRANGEMENT 		= ''
    FN.ARR.INTEREST 	= 'F.AA.ARR.INTEREST'
    F.ARR.INTEREST 		= ''
    FN.ARR.TERM_AMO 	= 'F.AA.TERM.AMOUNT'
    F.ARR.TERM_AMO  	= ''
    FN.CTRCT.BLNC		= 'F.EB.CONTRACT.BALANCES'
    F.CTRCT.BLNC		= ''
    FN.PROD.DES			= 'F.AA.PRODUCT.DESIGNER'
    F.PROD.DES			= ''
    FN.REGUL.INFO		= 'F.SLV.REGULATORY.INFO'
    F.REGUL.INFO		= ''
    FN.ACCOUNT			= 'F.ACCOUNT'
    F.ACCOUNT			= ''
    FN.AA.ACCOUNT		= 'F.AA.ACCOUNT'
    F.AA.ACCOUNT		= ''
    FN.CUSTOMER			= 'F.CUSTOMER'
    F.CUSTOMER			= ''
    FN.AA.ACC.DET		= 'F.AA.ACCOUNT.DETAILS'
 	F.AA.ACC.DET		= ''
 	FN.PAYMENT.SCHEDULE = 'F.AA.PAYMENT.SCHEDULE'
 	F.PAYMENT.SCHEDULE  = ''
 	FN.DATES			= 'F.DATES'
 	F.DATES				= ''
 	FN.AA.BILL.DET		= 'F.AA.BILL.DETAILS'
 	F.AA.BILL.DET		= ''
 	FN.AA.ACT.HIS		= 'F.AA.ACTIVITY.HISTORY'
 	F.AA.ACT.HIS		= ''
	
**   Variables de trabajo
    ERR		=''
    chNd	=' '
    inNd	= 0
    edLine	='|'    
    DIR.NAME='SIF.OUT'
*    DIR.NAME='C:\EXTRACT-CRISK\'
    NAME.FILE='_Loan.csv'
    NAME.FILE.RF='_LoanRefinan.csv'
    AR.REG=''
    J=0

;*Variables de salida
    LOANID				=	'a99'
    BRANCHID			=	'a99'
    LOANSTATUSID		=	'a99'
    APPROVEDTERM		=	'a99'
    COUNTRY				=	'a99'
    LOANDESTINATIONID	=	'a99'
    FINANCEDACTIVITYID	=	'a99'
    APPROVEDAMOUNT		=	99.99
    GRANTDATE			=	'19000101'
    MATURITYDATE		=	'19000101'
    CLOSEDATE			=	'19000101'
    FUNDID				=	0
    PAIDPRINCIPAL		=	99.99
    PAIDINTEREST		=	99.99
    PAIDADDITIONALCHARGES	=	99.99
    ZONE				=	'a99'
    BALANCEPRINCIPAL	=	99.99
    ACTIVEBALANCEPRINCIPAL	=	99.99
    EXPIREDBALANCEPRINCIPAL	=	99.99
    ARREARBALANCEPRINCIPAL	=	99.99
    BALANCEINTEREST			=	99.99
    ACTIVEBALANCEINTEREST	=	99.99
    EXPIREDBALANCEINTEREST	=	99.99
    ARREARBALANCEINTEREST	=	99.99
    DAYSINARREARPRINCIPAL	=	0
    DAYSINARREARINTEREST	=	0
    ISINARREAR				=	0
    APPROVEDPAYMENT			=	0
    DAYSTERM				=	0
    GRACEPERIOD				=	0
    GRACETYPE				=	0
    INITARREARDATEPRINCIPAL	=	'19000101'
    INITARREARDATEINTEREST	=	'19000101'
    PENALTYDATE				=	'19000101'
    REFINANCINGTYPE			=	0
    ISINJUDICIALCOLLECTION	=	0
    APPROVEDDISBURSMENTCOMMISIONRATE	=	99.99
    INTERESTRATE			=	99.99
    ACCOUNTINGINTERESTRATE	=	99.99
    REFERENCEINTERESTRATE	=	99.99
    ISREGULARIZED			=	0
    EFFECTIVEINTERESTRATE	=	99.99
    LASTPAYDATEPRINCIPAL	=	'19000101'
    LASTPAYDATEINTEREST		=	'19000101'
    ARREARPAYMENTSPRINCIPAL	=	0
    ARREARPAYMENTSINTEREST	=	0
    PRINCIPALACCOUNT		=	'a99'
    INTERESTACCOUNT			=	'a99'
    PRINCIPALPREPAYMENT		=	99.99
    FIRSTPAYMENTDATE		=	'19000101'
    PAYMENTAMOUNT			=	99.99
    NUMBERPAIDPAYMENTS		=	0
    NEXTPAYMENTDATE			=	'19000101'
    BALANCESURE				=	99.99
    CLIENTID				=	0
    PRINCIPALPAYMENTDAY		=	0
    INTERESTPAYMENTDAY		=	0
    LOANTYPE				=	'a99'
    OUTSTANDINGDEPOSITS		=	99.99
    BALANCECOURTCOSTS		=	99.99
    PAYMENTPLANADVANCE		=	99.99
    PRINCIPALADVANCEPERCENT	=	99.99
    ISREFINANCEREFINANCED	=	'a99'
    D_DATENOW				=	OCONV(DATE(),'D')
    D_SYSCLOSEDATE			=	''
    PRODUCTCATERGORYT24		=	'a99'
    DISBURSMENTCOMMISIONVALUE = 99.99
    LOANCONDITION			=	'a99'
    ABONODEPOSITO			= 0
    D_MOTHCLOSEDATE			=''
      
;*Fecha de maquina
    D_DATENOW = 	OCONV(ICONV(D_DATENOW,"DMDY"),"D-E[2,A3,2]")
    D_DATENOW	=	D_DATENOW[7,4]:D_DATENOW[4,2]:D_DATENOW[1,2]

*SLV.AA.ACTIVITY.HISTORY --Para adelanto de capital
    RETURN
*** </region>

OPENFILES:
*** <region>
;*abriendo archivos necesarios
    CALL OPF(FN.ARRANGEMENT,F.ARRANGEMENT)
    CALL OPF(FN.ARR.INTEREST,F.ARR.INTEREST)
    CALL OPF(FN.CTRCT.BLNC,F.CTRCT.BLNC)
    CALL OPF(FN.ACCOUNT, F.ACCOUNT)
	CALL OPF(FN.AA.ACCOUNT, F.AA.ACCOUNT)
	CALL OPF(FN.CUSTOMER, F.CUSTOMER)
	CALL OPF(FN.AA.ACC.DET,F.AA.ACC.DET)
	CALL OPF(FN.REGUL.INFO, F.REGUL.INFO)
	CALL OPF(FN.PROD.DES, F.PROD.DES)
	CALL OPF (FN.DATES, F.DATES)
	CALL OPF (FN.AA.BILL.DET, F.AA.BILL.DET)
*	CALL OPF(FN.PAYMENT.SCHEDULE, F.PAYMENT.SCHEDULE)
	CALL OPF (FN.AA.ACT.HIS, F.AA.ACT.HIS)

;*eliminando archivo existente
    DELETESEQ DIR.NAME,NAME.FILE THEN
    END
    DELETESEQ DIR.NAME,NAME.FILE.RF THEN
    END


;* Abriendo archivo para escritura
    OPENSEQ DIR.NAME, NAME.FILE TO SEQ.PTR THEN
        WEOFSEQ NAME.FILE
    END

    OPENSEQ DIR.NAME, NAME.FILE.RF TO SEQ.PTR.RF THEN
        WEOFSEQ NAME.FILE.RF
    END

    RETURN
*** </region>

PROCESS:
*** <region>
;*Fecha del ultimo cierre
 CALL F.READ(FN.DATES,'SV0010001', R.DATE,F.DATES,Y.ARRANGEMENT.ERR2) 
		D_SYSCLOSEDATE			=	R.DATE<EB.DAT.LAST.WORKING.DAY>		
*		D_SYSCLOSEDATE='20160219'
		D_MOTHCLOSEDATE			=   D_SYSCLOSEDATE[5,2]
	
;*variables locales
ARR_LOCAL_FIELD_ACC=''
ARR_LOCAL_FIELD_AMT=''
ARR_LOCAL_FIELD_CUS=''
ARR_LOCAL_FIELD_PRD=''
ARR_LOCAL_FIELD_INT=''
ARR_SCHED_FIELD_LOA=''
ARR_LOCAL_FIELD_A_CUS=''

;*definiendo query para extracción
   STMT.ARRANGEMENT = "SELECT ":FN.ARRANGEMENT 
*    STMT.ARRANGEMENT = "SELECT ":FN.ARRANGEMENT:" WITH @ID EQ AA15175MW169 "
    STMT.PRODUCT	 = "SELECT ":FN.PROD.DES
    


;*extrayendo ids
    CALL EB.READLIST(STMT.ARRANGEMENT, ARRANGEMENT.LIST_INI,'',NO.OF.RECS,Y.ARRANGEMENT.ERR1)
    CALL EB.READLIST(STMT.PRODUCT, PRODUCT.LIST,'',NO.OF.RECS.PRO,Y.ERR)
    CALL EB.READLIST(STMT.ASSET.TYPE,ASSET.TYPE.LIST,'',NO.OF.RECS.ASSET,ASSET.ERR)
  CRT  STMT.ARRANGEMENT
*solo clientes con las condiciones
*-----------------------------------------------------------------------------

 FOR LK=1 TO NO.OF.RECS
    CRT LK    
        CALL F.READ(FN.ARRANGEMENT, ARRANGEMENT.LIST_INI<LK>, R.ARRANGEMENT_INI,F.ARRANGEMENT,Y.ARRANGEMENT.ERR2) 
        CALL F.READ(FN.AA.ACT.HIS, ARRANGEMENT.LIST_INI<LK>, R.ACTIVITY_INI,F.AA.ACT.HIS,Y.ACTIVITY.ERR) 
  		  			
  		;*Identificando creditos en estado close que se dieron solo en el mes de proceso	 	
  		NOACT =	DCOUNT(R.ACTIVITY_INI<AA.AH.ACTIVITY.ID>,VM)
  		FOR INOACT=1 TO NOACT				
			IF 	R.ACTIVITY_INI<AA.AH.ACTIVITY.ID><1,INOACT> EQ 	'LENDING-CLOSE-ARRANGEMENT' THEN
				D_DATELOANCLOSE = R.ACTIVITY_INI<AA.AH.ACT.DATE><1,INOACT>
				D_MONTHDATELONACLOSE=D_DATELOANCLOSE[5,2]				
				;*CRT D_MONTHDATELONACLOSE:" ":D_MOTHCLOSEDATE:";":D_DATELOANCLOSE:" ":D_SYSCLOSEDATE		
			END 			
		NEXT INOACT  		
  		
       IF R.ARRANGEMENT_INI<AA.ARR.PRODUCT.LINE> EQ 'LENDING' AND (R.ARRANGEMENT_INI<AA.ARR.ARR.STATUS> EQ 'PENDING.CLOSURE' OR R.ARRANGEMENT_INI<AA.ARR.ARR.STATUS> EQ 'CURRENT' OR R.ARRANGEMENT_INI<AA.ARR.ARR.STATUS> EQ 'EXPIRED' OR (R.ARRANGEMENT_INI<AA.ARR.ARR.STATUS> EQ 'CLOSE' AND D_MONTHDATELONACLOSE EQ D_MOTHCLOSEDATE ))  THEN
        	ARRANGEMENT.LIST<-1>=  ARRANGEMENT.LIST_INI<LK>
       END        
 NEXT LK
 
N_NOARRANALIZER=DCOUNT(ARRANGEMENT.LIST,FM)
*N_NOARRANALIZER=20     
*-----------------------------------------------------------------------------

    FOR I=1 TO N_NOARRANALIZER 
        CALL F.READ(FN.ARRANGEMENT, ARRANGEMENT.LIST<I>, R.ARRANGEMENT,F.ARRANGEMENT,Y.ARRANGEMENT.ERR2) 

CRT I:R.ARRANGEMENT<AA.ARR.ARR.STATUS>:ARRANGEMENT.LIST<I>
            J=J+1
                     
            ;*para identificar bills en gracia
        	;*-----------------------------------------------------------------------------
        	STMT.AA.BILL.DET = "SELECT ":FN.AA.BILL.DET:" WITH ARRANGEMENT.ID EQ ":ARRANGEMENT.LIST<I>
        	CALL EB.READLIST(STMT.AA.BILL.DET ,AA.BILL.DET.LIST,'',NO.OF.RECS.BILL.DET, BILL.DET.ERR)
        	          
            ;*Consulta de datos
            ;**ACCOUNT
            ;*-----------------------------------------------------------------------------
            ;**Campos locales
            CALL AA.GET.ARRANGEMENT.CONDITIONS(ARRANGEMENT.LIST<I>,'ACCOUNT','ACCOUNT',TODAY, RETURN.ID1, REC.ACC, ERROR1)        
			;**Campos locales
			ARR_LOCAL_FIELD_ACC = RAISE(REC.ACC)
            CALL GET.LOC.REF('AA.ARR.ACCOUNT','LF.LOAN.STATUS',LOCPOSLoanStatus)
		    CALL GET.LOC.REF('AA.ARR.ACCOUNT','LF.COND.ASSET',LOCPOSCondAsset)
	    
			;**Term.amount
			;*--------------------------------------------------------------------------
			CALL AA.GET.ARRANGEMENT.CONDITIONS(ARRANGEMENT.LIST<I>,'TERM.AMOUNT','COMMITMENT',TODAY,RETURN.ID2, REC.AMT, ERROR2)      	 
			;**Campos locales
			ARR_LOCAL_FIELD_AMT = RAISE(REC.AMT)
			;*Ids
            CALL GET.LOC.REF('AA.ARR.TERM.AMOUNT','LF.DESTINO.SSF',LOCPOSDest)
            CALL GET.LOC.REF('AA.ARR.TERM.AMOUNT','LF.SPEND.CODE' ,LOCPOSGasto)
            CALL GET.LOC.REF('AA.ARR.TERM.AMOUNT','LF.SPEND.TYPE',LOCPOSTipoGasto)
            CALL GET.LOC.REF('AA.ARR.TERM.AMOUNT','LF.SPEND.AMT',LOCPOSMontoGasto)
              
            ;**Schedule
			;*--------------------------------------------------------------------------
			CALL AA.GET.ARRANGEMENT.CONDITIONS(ARRANGEMENT.LIST<I>,'PAYMENT.SCHEDULE','SCHEDULE',TODAY,RETURN.ID3, REC.SCH, ERROR.SCH)      	                  
                      ARR_SCHED_FIELD_LOA= RAISE(REC.SCH)
                    
           	;***Customer
           	;*--------------------------------------------------------------------------
	        CALL AA.GET.ARRANGEMENT.CONDITIONS(ARRANGEMENT.LIST<I>,'CUSTOMER','CUSTOMER',TODAY,RETURN.ID3,REC.CUS,ERROR3)
			;*App Customer	 
			ARR_LOCAL_FIELD_A_CUS =RAISE(REC.CUS)   
			
			CALL F.READ(FN.CUSTOMER, REC.CUS<1,AA.CUS.PRIMARY.OWNER>, R.APP.CUS,F.CUSTOMER,ERROR4)
			;**Campos locales
			ARR_LOCAL_FIELD_CUS = (R.APP.CUS)
			;*Ids
			CALL GET.LOC.REF('CUSTOMER','LF.ZONA',LOCPOSZona)
			CALL GET.LOC.REF('AA.ARR.CUSTOMER','LF.FORMA.PAGO',LOCPOSPaymentDesc) 
			CALL GET.LOC.REF('AA.ARR.CUSTOMER','LF.AUT.BURO',LOCPOSAutBuro)
			
			;*Account.Detail
			;*--------------------------------------------------------------------------				
			;*ACCOUNT						
			CALL F.READ(FN.AA.ACC.DET,ARRANGEMENT.LIST<I>,R.AA.ACC.DET.VAL,F.AA.ACC.DET,ERROR5)
					
			;***Tasa de interes	   
			;*------------------------------------------------------------------------ 
	        CALL AA.GET.ARRANGEMENT.CONDITIONS(ARRANGEMENT.LIST<I>,'INTEREST','PRINCIPALINT',TODAY,RETURN.ID, RETURN.CONDITIONS,RETURN.ERROR)      	 		 	
	 		;**Campos locales
	 		ARR_LOCAL_FIELD_INT=(RETURN.CONDITIONS)
	 		;*Ids
	 		CALL GET.LOC.REF('AA.INTEREST','LF.REF.INT.RATE',LOCPOSRefRate) 	
			CALL GET.LOC.REF('AA.ARR.INTEREST','LF.EFEC.INTRATE',LOCPOSTasaInteresEfec)

		   ;***Producto	
		   ;*--------------------------------------------------------------------------
		   CALL GET.LOC.REF('AA.PRODUCT.DESIGNER','LF.PROD.CLASSIF',LOCPOSProd)
		   FOR K=1 TO NO.OF.RECS.PRO
		   		CALL F.READ(FN.PROD.DES,  PRODUCT.LIST<K>, R.PROD.DES,F.PROD.DES,ERR.PROD.DES)
				    IF PRODUCT.LIST<K>[1,LEN(PRODUCT.LIST<K>)-9] EQ R.ARRANGEMENT<AA.ARR.PRODUCT> THEN 
						 ;*Campos locales
				   		ARR_LOCAL_FIELD_PRD = (R.PROD.DES)		   		
				 		BREAK
				  	END
		  	NEXT K		

			;*Informacion regulatoria
			;*--------------------------------------------------------------------------
			 CALL F.READ(FN.REGUL.INFO, ARRANGEMENT.LIST<I>, R.REG.INFO, F.REGUL.INFO, ERROR6)		
			 ;**Campos locales
	 		ARR_LOCAL_FIELD_RIN=(R.REG.INFO)
			
			;*evaluacion de creditos
			GOSUB OTHERCALC
  			
            LOANID				=	 R.ARRANGEMENT<AA.ARR.LINKED.APPL.ID><1,1>
            BRANCHID			=	 R.ARRANGEMENT<AA.ARR.CO.CODE>
            LOANSTATUSID		=	 R.ARRANGEMENT<AA.ARR.ARR.STATUS>;*ARR_LOCAL_FIELD_ACC<AA.AC.LOCAL.REF><1,LOCPOSLoanStatus>
            APPROVEDTERM		=	 REC.AMT<1,AA.AMT.TERM>
            COUNTRY				=	R.REG.INFO<SLV.AA.REG.PAIS.DESTINO.CRED>
            LOANDESTINATIONID	=	ARR_LOCAL_FIELD_AMT<AA.AMT.LOCAL.REF><1,LOCPOSDest>
            FINANCEDACTIVITYID	=	R.APP.CUS<EB.CUS.INDUSTRY>
            APPROVEDAMOUNT		=	REC.AMT<1,AA.AMT.AMOUNT>
            GRANTDATE			=	R.ARRANGEMENT<AA.ARR.START.DATE>
            MATURITYDATE		=	REC.AMT<1,AA.AMT.MATURITY.DATE>
            CLOSEDATE			=	D_DATELOANCLOSE
            FUNDID				=	R.REG.INFO<SLV.AA.REG.CODIGO.RECURSO>
            PAIDPRINCIPAL		=	APPROVEDAMOUNT-(R.REG.INFO<SLV.AA.REG.SALDO.VIGENTE.K>+N_ARRARAMOUNTCAPITAL)
            PAIDINTEREST		=	0.00
            PAIDADDITIONALCHARGES	=	0.00
            ZONE				=	ARR_LOCAL_FIELD_CUS<EB.CUS.LOCAL.REF><1,LOCPOSZona>
            BALANCEPRINCIPAL	=	N_ARRARAMOUNTCAPITAL
            ACTIVEBALANCEPRINCIPAL	=	0.00
            EXPIREDBALANCEPRINCIPAL	=	0.00
            ARREARBALANCEPRINCIPAL	=	N_VENCAPITAL
            BALANCEINTEREST			=	N_ARRARAMOUNTINTEREST
            ACTIVEBALANCEINTEREST	=	0.00
            EXPIREDBALANCEINTEREST	=	0.00
            ARREARBALANCEINTEREST	=	N_VENCINTEREST;*R.REG.INFO<SLV.AA.REG.SALDO.VENCIDO.I>
            DAYSINARREARPRINCIPAL	=	R.REG.INFO<SLV.AA.REG.DIAS.MORA.K>
            DAYSINARREARINTEREST	=	R.REG.INFO<SLV.AA.REG.DIAS.MORA.I>
            ISINARREAR				=	N_ARRARLOAN
            APPROVEDPAYMENT			=	N_NOPAYMENT
            DAYSTERM				=	N_DAYSTERM
            GRACEPERIOD				=	R.REG.INFO<SLV.AA.REG.PERIODO.GRACIA.K>
            GRACETYPE				=	0
            INITARREARDATEPRINCIPAL	=	R.REG.INFO<SLV.AA.REG.FEC.INICIO.MORA.K> 
            INITARREARDATEINTEREST	=	R.REG.INFO<SLV.AA.REG.FEC.INIC.MORA.I>
            PENALTYDATE				=	'19000101'
            REFINANCINGTYPE			=	0
            ISINJUDICIALCOLLECTION	=	0
            APPROVEDDISBURSMENTCOMMISIONRATE	=	0.00
            INTERESTRATE			=	RETURN.CONDITIONS<1,AA.INT.FIXED.RATE>
            ACCOUNTINGINTERESTRATE	=	0.00
            REFERENCEINTERESTRATE	=	R.REG.INFO<SLV.AA.REG.TASA.REFERENCIA>
            ISREGULARIZED			=	0
            EFFECTIVEINTERESTRATE	=	FIELD(ARR_LOCAL_FIELD_INT<1,AA.INT.LOCAL.REF>,SM,LOCPOSTasaInteresEfec)
            LASTPAYDATEPRINCIPAL	=	R.REG.INFO<SLV.AA.REG.FEC.ULTIMO.PAGO.K>
            LASTPAYDATEINTEREST		=	D_FECULTIMOPAGOI 
            ARREARPAYMENTSPRINCIPAL	=	R.REG.INFO<SLV.AA.REG.CUOTA.MORA.K>
            ARREARPAYMENTSINTEREST	=	R.REG.INFO<SLV.AA.REG.CUOTA.MORA.I>
            PRINCIPALACCOUNT		=	'114'
            INTERESTACCOUNT			=	'114'
            PRINCIPALPREPAYMENT		=	R.REG.INFO<SLV.AA.REG.ADELANTO.CAPITAL>
            FIRSTPAYMENTDATE		=	'19000101'
            PAYMENTAMOUNT			=	N_PAYMENTAMOUNT ;* Para darle tratamiento al multivalor
            NUMBERPAIDPAYMENTS		=	N_NOPAYMENT
            NEXTPAYMENTDATE			=	R.AA.ACC.DET.VAL<AA.AD.MATURITY.DATE>
            BALANCESURE				=	N_TOTALSURE
            CLIENTID				=	REC.CUS<1,AA.CUS.PRIMARY.OWNER>
            PRINCIPALPAYMENTDAY		=	R.AA.ACC.DET.VAL<AA.AD.PAYMENT.START.DATE>[7,2]								;*R.REG.INFO<SLV.AA.REG.DIA.PAGO.K>
            INTERESTPAYMENTDAY		=	R.AA.ACC.DET.VAL<AA.AD.PAYMENT.START.DATE>[7,2]								;*R.REG.INFO<SLV.AA.REG.DIA.PAGO.I>
            LOANTYPE				=	ARR_LOCAL_FIELD_PRD<AA.PRD.LOCAL.REF><1,LOCPOSProd>
            OUTSTANDINGDEPOSITS		=	0.00
            BALANCECOURTCOSTS		=	0.00
            PAYMENTPLANADVANCE		=	0.00
            PRINCIPALADVANCEPERCENT	=	0.00
            ISREFINANCEREFINANCED	=	0
            D_DATENOW				=	D_DATENOW
            PRODUCTCATERGORYT24		=	ARR_LOCAL_FIELD_ACC<AA.AC.CATEGORY>
            DISBURSMENTCOMMISIONVALUE = 0.00
            LOANCONDITION			=	ARR_LOCAL_FIELD_ACC<AA.AC.LOCAL.REF><1,LOCPOSCondAsset>
            CODIGOGASTO				=  ARR_LOCAL_FIELD_AMT<AA.AMT.LOCAL.REF><1,LOCPOSGasto>
            TIPOGASTO				=	ARR_LOCAL_FIELD_AMT<AA.AMT.LOCAL.REF><1,LOCPOSTipoGasto>
            MONTOGASTO				=	ARR_LOCAL_FIELD_AMT<AA.AMT.LOCAL.REF><1,LOCPOSMontoGasto>
            CONUNIDADMEDIDA			=  R.REG.INFO<SLV.AA.REG.CON.UNIDAD.MEDIDA>
            CANTIDADUNIDAD			=	R.REG.INFO<SLV.AA.REG.CANTIDAD.UNIDAD>
            ORDENDESCUENTO			=	S_ORDERPAYMENT;*R.REG.INFO<SLV.AA.REG.ORDEN.DESCUENTO>
            CODIGOCARTERA			=	R.REG.INFO<SLV.AA.REG.COD.CARTERA>
            FREQUENCIADEPAGO		=	FIELD(ARR_SCHED_FIELD_LOA<AA.PS.PAYMENT.FREQ>,@VM,1)
            TIPOPRODUCTO			=	ARR_LOCAL_FIELD_ACC<AA.AC.ACCOUNT.TITLE.1>
            SALDOMORAK				=   R.REG.INFO<SLV.AA.REG.SALDO.MORA.K>
            SALDOMORAI				=	R.REG.INFO<SLV.AA.REG.SALDO.MORA.I>
            ABONODEPOSITO			=	R.REG.INFO<SLV.AA.REG.ABONO.DEPOSITO>
            AUTBURO                 =   ARR_LOCAL_FIELD_A_CUS<AA.CUS.LOCAL.REF><1,LOCPOSAutBuro>                                 
            FECHACASTIGO			=	R.REG.INFO<SLV.AA.REG.FECHA.CASTIGO>
            ULTIMAFECHAVENC			=	R.REG.INFO<SLV.AA.REG.UTIMA.FECHA.VENC>
            FECHADEMANDA 			=	R.REG.INFO<SLV.AA.REG.FECHA.DEMANDA>     		
*-----------------------------------------------------------------------------
	;*tratamientos especiales
			IF DAYSINARREARPRINCIPAL EQ '' THEN 
				DAYSINARREARPRINCIPAL=0
			END
			IF DAYSINARREARINTEREST EQ '' THEN 
				DAYSINARREARINTEREST=0
			END
			IF ABONODEPOSITO EQ '' THEN
				ABONODEPOSITO	 = 0
			END	
			
			IF LOANCONDITION EQ 'RF' THEN			
				GOSUB REFINANCIADOS
			END 
 	;*EVALUACION TEMPORAL----RETIRAR DESPUES DE LA PRUEBA
            IF LOANSTATUSID EQ '' THEN 
            	LOANSTATUSID=1
            	LOANTYPE='EMPRESAS'
            	LOANCONDITION='OR'
            END   		
*--------------------------------------------------------------------------------
            ;***Valores para cadena
            R.LINE=	LOANID:";":BRANCHID:";":LOANSTATUSID:";":APPROVEDTERM:";":COUNTRY:";":LOANDESTINATIONID:";":FINANCEDACTIVITYID:";":APPROVEDAMOUNT:";":GRANTDATE:";":MATURITYDATE:";":CLOSEDATE:";":FUNDID:";":PAIDPRINCIPAL:";":PAIDINTEREST:";":PAIDADDITIONALCHARGES:";":ZONE:";":BALANCEPRINCIPAL:";":ACTIVEBALANCEPRINCIPAL:";":EXPIREDBALANCEPRINCIPAL:";":ARREARBALANCEPRINCIPAL:";":BALANCEINTEREST:";":ACTIVEBALANCEINTEREST:";":EXPIREDBALANCEINTEREST:";":ARREARBALANCEINTEREST:";":DAYSINARREARPRINCIPAL:";":DAYSINARREARINTEREST:";":ISINARREAR:";":APPROVEDPAYMENT:";":DAYSTERM:";":GRACEPERIOD:";":GRACETYPE:";":INITARREARDATEPRINCIPAL:";":INITARREARDATEINTEREST:";":PENALTYDATE:";":REFINANCINGTYPE:";":ISINJUDICIALCOLLECTION:";":APPROVEDDISBURSMENTCOMMISIONRATE:";":INTERESTRATE:";":ACCOUNTINGINTERESTRATE:";":REFERENCEINTERESTRATE:";":ISREGULARIZED:";":EFFECTIVEINTERESTRATE:";":LASTPAYDATEPRINCIPAL:";":LASTPAYDATEINTEREST:";":ARREARPAYMENTSPRINCIPAL:";":ARREARPAYMENTSINTEREST:";":PRINCIPALACCOUNT:";":INTERESTACCOUNT:";":PRINCIPALPREPAYMENT:";":FIRSTPAYMENTDATE:";":PAYMENTAMOUNT:";":NUMBERPAIDPAYMENTS:";":NEXTPAYMENTDATE:";":BALANCESURE:";":CLIENTID:";":PRINCIPALPAYMENTDAY:";":INTERESTPAYMENTDAY:";":LOANTYPE:";":OUTSTANDINGDEPOSITS:";":BALANCECOURTCOSTS:";":PAYMENTPLANADVANCE:";":PRINCIPALADVANCEPERCENT:";":ISREFINANCEREFINANCED:";":D_DATENOW:";":PRODUCTCATERGORYT24:";"DISBURSMENTCOMMISIONVALUE:";":LOANCONDITION:";":CODIGOGASTO:";":TIPOGASTO:";":MONTOGASTO:";":CONUNIDADMEDIDA:";":CANTIDADUNIDAD:";":ORDENDESCUENTO:";":CODIGOCARTERA:";":FREQUENCIADEPAGO:";":TIPOPRODUCTO:";":SALDOMORAK:";":SALDOMORAI:";":ABONODEPOSITO:";":AUTBURO:";":FECHACASTIGO:";":ULTIMAFECHAVENC:";":FECHADEMANDA:edLine
            	
            AR.REG<J>=R.LINE

*        END
    NEXT I

;* Generación de csv array para escritura de en csv
*-----------------------------------------------------------------------------
    FOR L=1 TO J	
		     IF L< J THEN		
		            WRITESEQ AR.REG<L> APPEND TO SEQ.PTR THEN ;*con retorno de carro
		            
		        	END
		       
		    	END
		    IF L=J THEN
		        WRITEBLK AR.REG<L> ON SEQ.PTR THEN ;*sin retorno de carro
		        
		    	END
		 	END
	 	  
    NEXT L

*REFINANCIADOS         
*----------------------------------------------------------        
 N_NORF=DCOUNT(R.LINECANC,FM)
 FOR RFI=1 TO N_NORF
 			IF RFI< N_NORF THEN		
		            WRITESEQ R.LINECANC<RFI> APPEND TO SEQ.PTR.RF THEN ;*con retorno de carro
		            
		        	END
		       
		    END
		    IF RFI=N_NORF THEN
		        WRITEBLK R.LINECANC<RFI> ON SEQ.PTR.RF THEN ;*sin retorno de carro
		        
		    	END
		 	END
 NEXT RFI
         
*-----------------------------------------------------------------------------
    CLOSESEQ SEQ.PTR
    CLOSESEQ SEQ.PTR.RF
 RETURN
*** </region>

OTHERCALC:
*** <region>
*-----------------------------------------------------------------------------
   N_MAXARRARDAY			=0
   N_ARRARLOAN				=0
   N_ARRARAMOUNTINTEREST 	=0.00
   N_ARRARAMOUNTCAPITAL		=0.00
   N_DAYSTERM				=0
   N_NOPAYMENT				=0
   N_VENCINTEREST			=0
   N_VENCAPITAL				=0
   S_ORDERPAYMENT			=''
   N_PAYMENTAMOUNT			=0.00

   ;*Saldos vencidos

		N_ARRARAMOUNTCAPITAL 	= 	R.REG.INFO<SLV.AA.REG.SALDO.VENCIDO.K>
   		IF R.REG.INFO<SLV.AA.REG.DIAS.MORA.K> GT 90 OR  R.REG.INFO<SLV.AA.REG.DIAS.MORA.I> GT 90 THEN
	   		N_ARRARLOAN				=	1
	   		N_VENCINTEREST			=	R.REG.INFO<SLV.AA.REG.SALDO.VENCIDO.I>
	   		N_VENCAPITAL			=	R.REG.INFO<SLV.AA.REG.SALDO.VENCIDO.K>
	   	END ELSE
	   		N_ARRARAMOUNTINTEREST	=	R.REG.INFO<SLV.AA.REG.SALDO.VIGENTE.I>+R.REG.INFO<SLV.AA.REG.SALDO.MORA.I>
	   	END
		
 ;*calculo de plazo en dias
 	TYPE.RETURN='C' 
	CALL CDD(Y.REG,R.ARRANGEMENT<AA.ARR.START.DATE>,REC.AMT<1,AA.AMT.MATURITY.DATE>,TYPE.RETURN)           
	N_DAYSTERM=TYPE.RETURN

 ;*Calculo de numero cuotas
	N_NOPAYMENT				= 	INT((N_DAYSTERM/365)*12)
				
 ;*Definicion de orden de descuento segun norma 417
  S_ORDERPAYMENT='SO' 	 
  IF ARR_LOCAL_FIELD_A_CUS<AA.CUS.LOCAL.REF><1,LOCPOSPaymentDesc> EQ 'OPI' THEN
  	S_ORDERPAYMENT='CO'
  END

 ;*totalizando valor de seguros y valor en gracia
 ;*---------------------------------------------------------------------				   
		N_TOTALSUREDANO		=''
    	N_TOTALSUREVIDA		=''
   		N_TOTALIVASEGURO	=''
   		N_TOTALSURE			=''
   		N_TOTALGRCKAP		=''
   		N_TOTALGRCINT		=''
		   		    		
   		;*periodo de gracia	
   	   GOSUB AMOUNT_GRACE

   		N_ARRARAMOUNTCAPITAL	+=(N_TOTALGRCKAP)
   		N_ARRARAMOUNTINTEREST	+=(N_TOTALGRCINT)  	
   		N_TOTALSURE 			=(N_TOTALSUREDANO + N_TOTALSUREVIDA + N_TOTALIVASEGURO)
*debug   		   	
*   	CRT 'PRB':N_ARRARAMOUNTCAPITAL
*   	CRT 'sv ':N_TOTALSUREVIDA
*   	CRT 'sd ':N_TOTALSUREDANO
*   	CRT 'sviva ':N_TOTALIVASEGURO
*   	CRT 'grck ':N_TOTALGRCKAP
*   	CRT 'grci ':N_TOTALGRCINT
*	CRT 'ttk ':N_ARRARAMOUNTCAPITAL
*   	CRT 'tti ':N_ARRARAMOUNTINTEREST
*   	CRT 'ttse ':N_TOTALSURE	
*    CRT 'Fgrc	': D_PAYMENTDATE
*    CRT 'Fclose ':D_SYSCLOSEDATE
*    CRT 'Day_grc ':N_DAYS_GRC
*    CRT 'Day arrear k': R.REG.INFO<SLV.AA.REG.DIAS.MORA.K>:' i ':  R.REG.INFO<SLV.AA.REG.DIAS.MORA.I>
*    CRT  SRTGRC
   

 ;*---------------------------------------------------------------------
;*Fecha de ultimo pago de interes
	D_FECULTIMOPAGOI = D_SYSCLOSEDATE ;* se envia la fecha de cierre para darle tratamiento en sql
 ;*---------------------------------------------------------------------
;*Monto de la cuota
N_PAYMENTAMOUNT=SWAP(ARR_SCHED_FIELD_LOA<AA.PS.CALC.AMOUNT>, @VM, '')
N_PAYMENTAMOUNT-=R.REG.INFO<SLV.AA.REG.SEGUROS.DANOS>
N_PAYMENTAMOUNT-=R.REG.INFO<SLV.AA.REG.SEGUROS.VIDA>
N_PAYMENTAMOUNT-=R.REG.INFO<SLV.AA.REG.IVA.SEGUROS.DANOS>


  RETURN
*** </region>
;*Cuotas en gracia/delincuent  
;*---------------------------------------------------------------------
;*---------------------------------------------------------------------
 AMOUNT_GRACE:
*** <region>
     FOR GR=1 TO NO.OF.RECS.BILL.DET
	    CRT GR
	      	N_DAYS_GRC=0
    		D_PAYMENTDATE=''
	    
	     CALL F.READ(FN.AA.BILL.DET, AA.BILL.DET.LIST<GR>, R.AA.ACC.DET.BILL.DET, F.AA.BILL.DET, E.AA.BILL.DET)
	   	     	   		
	     ;*Verificando el estado del bill
		  SRTGRC = R.AA.ACC.DET.BILL.DET<AA.BD.AGING.STATUS><1,1>
		
		 
		  D_PAYMENTDATE=   R.AA.ACC.DET.BILL.DET<AA.BD.PAYMENT.DATE>
	      ;*calculo de plazo en dias en gracia
		 	NO_DATE_GRC='C' 		 	
			CALL CDD(Y.REG,D_PAYMENTDATE,D_SYSCLOSEDATE,NO_DATE_GRC)           
			N_DAYS_GRC=NO_DATE_GRC+1
		
		;*logica para superar el problema de al0016 en los cierres (no toma creditos en gracia o del bajo las siguientes condiciones)
	     IF R.AA.ACC.DET.BILL.DET<AA.BD.SETTLE.STATUS> EQ 'UNPAID' AND N_DAYS_GRC GT 0 THEN
			NO_PROP = DCOUNT(R.AA.ACC.DET.BILL.DET<AA.BD.PAY.PROPERTY>,@SM)		  
			FOR NP=1 TO NO_PROP													
				;*Capital e interes: para WORK AROUND de periodos de gracia y de DEL	
				IF 	((SRTGRC EQ 'GRC' AND N_DAYS_GRC GT 1) OR  (SRTGRC EQ 'DEL' AND N_DAYS_GRC EQ 6)) THEN
							;*Capital												  						  
					  	IF FIELD(R.AA.ACC.DET.BILL.DET<AA.BD.PAY.PROPERTY>,@SM,NP)<1,1> EQ 'ACCOUNT' THEN 					  								    		
				   			N_TOTALGRCKAP=N_TOTALGRCKAP + FIELD(R.AA.ACC.DET.BILL.DET<AA.BD.OS.PR.AMT>,@SM,NP)			   				    
				   		END	
				   			;*interes
				   		IF FIELD(R.AA.ACC.DET.BILL.DET<AA.BD.PAY.PROPERTY>,@SM,NP)<1,1> EQ 'PRINCIPALINT' THEN  			   		
				   			N_TOTALGRCINT=N_TOTALGRCINT + FIELD(R.AA.ACC.DET.BILL.DET<AA.BD.OS.PR.AMT>,@SM,NP)			   				    
				   		END				  
				END
			
			
				;*Seguros DAÑO Y VIDA: para todos las cuotas pendientes vigentes y vencidas			    			   
			   IF FIELD(R.AA.ACC.DET.BILL.DET<AA.BD.PAY.PROPERTY>,@SM,NP)<1,1> EQ 'ALSEGUROVIDA' THEN
*			   CRT 'SEGv ':FIELD(R.AA.ACC.DET.BILL.DET<AA.BD.OS.PR.AMT>,@SM,NP)<1,1>:' date ':D_PAYMENTDATE
			  		N_TOTALSUREVIDA +=  FIELD(R.AA.ACC.DET.BILL.DET<AA.BD.OS.PR.AMT>,@SM,NP)<1,1>
			   END
			   
			   IF FIELD(R.AA.ACC.DET.BILL.DET<AA.BD.PAY.PROPERTY>,@SM,NP)<1,1> EQ 'ALSEGURODANO' THEN
			   		N_TOTALSUREDANO +=  FIELD(R.AA.ACC.DET.BILL.DET<AA.BD.OS.PR.AMT>,@SM,NP)<1,1>
*			   	CRT 'SEGd ':FIELD(R.AA.ACC.DET.BILL.DET<AA.BD.OS.PR.AMT>,@SM,NP)<1,1>:' date ':D_PAYMENTDATE			   		
			   END			    			
			   
			NEXT NP 	
	     END   
     NEXT GR
    		
	RETURN
*** </region>

REFINANCIADOS:	
*** <region name= REFINANCIADOS-REESTRUCTURADOS>
*** <desc> creditos que se cancelaron por medio de refinanciamientos </desc>


NO_LOAN	=	DCOUNT(R.REG.INFO<SLV.AA.REG.NUM.REFERENC.CANC>,VM)

FOR LRF=1 TO NO_LOAN
	LOANNEW= R.ARRANGEMENT<AA.ARR.LINKED.APPL.ID><1,1>                 ;*num_referencia
	LOANCANC=R.REG.INFO<SLV.AA.REG.NUM.REFERENC.CANC><1,LRF>           ;*num_referencia_canc
	LAONDATECANC=R.ARRANGEMENT<AA.ARR.START.DATE>                      ;*
	LOANCANCK=R.REG.INFO<SLV.AA.REG.PAGO.CAPITAL.CANC><1,LRF>          ;*pago_capital
	
	LOANCANCI=R.REG.INFO<SLV.AA.REG.PAGO.INTERES.CANC><1,LRF>          ;*pago_interes
	LOANCANCITOTAL=R.REG.INFO<SLV.AA.REG.SALDO.TOT.INT.CANC><1,LRF>    ;*saldo_total_interes
	
	LOANCANCCONDICION=LOANCONDITION
	LOANCLIENT =  CLIENTID
		
	;*Valores para cadena
    R.LINECANC<-1>=	LOANNEW:";":LOANCANC:";":LAONDATECANC:";":LOANCANCK:";":LOANCANCI:";":LOANCANCITOTAL:";":LOANCANCCONDICION:";":LOANCLIENT:edLine
NEXT NO_LOAN

RETURN
*** </region>

 END
  