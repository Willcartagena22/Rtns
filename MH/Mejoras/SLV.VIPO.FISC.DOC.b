*-----------------------------------------------------------------------------
* <Rating>4242</Rating>
*-----------------------------------------------------------------------------
* Genera data para impresion de facturas y creditos fiscales por trasacciones(RUTINA CENTRAL)
* jar: SLV.VIPO.FISC.DOC
*-----------------------------------------------------------------------------
* Modification History :
* Autor    	Fecha     		Version.
* GAlfaro  	20.12.2014 		Initial Code
* GAlfaro   10.02.2015		Agregar funcionalidad para trabajo con FT
* GAlfaro	11.02.2015		Agregar funcionalidad para trabajo con TT puro
* GAlfaro	19.02.2015		Se modifica FIELD_BY_APP para tratar captura de numero de cliente
*			19.02.2015		Se agrega busque de id de cliente por numero de cuenta
*			03.03.2015		Se agregar compo para tratamiento de documento en para teller plano, partiendo del NRC
*							Se agrega funcionalidad de documento exento
*			07.03.2015		Se agrega tratamiento a multivalor en razon social
*			10.03.2015		Se agrega funcionalidad para emsion de comision por venta de chequera
*			16.03.2015		Se agrega funcionalidad de nota de credito
*			19.03.2015		Se agrega codigo de agencia a variable empresa para el spool	
*DFlores	20.03.2015		Se agrega evaluacion de condicion de campo TT.TE.WAIVE.CHARGES para emision de documento
*Galfaro	28.03.2015		se agrega evaluacion de transaccion en LIVE y HIST
*OCornejo	08.01.2016		Agregar Funcionalidad para Trabajo con CHG	
*Galfaro	05.03.2016		Agregar funcinalidad para combro de comsiones por desembolso de creditos		
*Calvarado   23.01.2017		Agregar logica para implemter descripcion para ministerio de hacienda.
*OCornejo    11.04.2017		Logica para Exluir Liof en Certificacion de Cheques	 
*Calvarado   30.04.2017		Se agrega tratamiento para multivalores en FT
*------------------------------------------------------------------------------
    SUBROUTINE SLV.VIPO.FISC.DOC
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.USER
    $INSERT I_F.VERSION
    $INSERT I_F.INDUSTRY
    $INSERT I_F.CUSTOMER
    $INSERT I_F.FUNDS.TRANSFER
    $INSERT I_F.TELLER.FINANCIAL.SERVICES
    $INSERT I_F.AA.PAYMENT.SCHEDULE
    $INSERT I_F.AA.CHARGE
    $INSERT I_F.AA.PROPERTY
    $INSERT I_F.AA.ARRANGEMENT
    $INSERT I_F.AA.TERM.AMOUNT
    $INSERT I_F.AA.ACTIVITY.BALANCES
    $INSERT I_F.TELLER
    $INSERT I_F.EB.SLV.GLOBAL.PARAM
    $INSERT I_F.COMPANY
    $INSERT I_F.FT.COMMISSION.TYPE	
    $INSERT I_F.ACCOUNT
    $INSERT I_F.CHEQUE.ISSUE
    $INSERT I_F.CHEQUE.CHARGE.BAL
    $INSERT I_F.EB.SLV.NOTE.CREDIT
    $INSERT I_F.AC.CHARGE.REQUEST
    $INSERT I_F.EB.SLV.ACCOUNT.PARAM
*------------------------------------------------------------------------------
    GOSUB INIT
    GOSUB OPENFILES
    GOSUB PROCESS

    RETURN
*------------------------------------------------------------------------------
INIT:
;* Aplicaciones a utilizar
    FN_CUS 			= 'F.CUSTOMER'
    F_CUS 			= ''
    FN_INDUST		= 'F.INDUSTRY'
    F_INDUST 		= ''
    FN_SLVPA 		= 'F.EB.SLV.GLOBAL.PARAM'
    F_SLVPA 		= ''
    FN_FUNDS_TRANS 	= 'F.FUNDS.TRANSFER'
    F_FUNDS_TRANS 	= ''
    FN_FUNDS_TR_HIS	= 'F.FUNDS.TRANSFER$HIS'
    F_FUNDS_TR_HIS 	= ''
    FN_TELLER 		= 'F.TELLER'
    F_TELLER 		= ''
    FN_HIS_TELLER	= 'F.TELLER$HIS'
    F_HIS_TELLER	= ''
    FN_ARRNG 		= 'F.AA.ARRANGEMENT'
    F_ARRNG 		= ''
    FN_ARR_TERM_AMO = 'F.AA.TERM.AMOUNT'
    F_ARR_TERM_AMO  = ''
    FN_COMPANY		= 'F.COMPANY'
    F_COMPANY		= ''
    FN_COMM_TYPE	= 'F.FT.COMMISSION.TYPE'
    F_COMM_TYPE		= ''
    FN_ACCOUNT		= 'F.ACCOUNT'
    F_ACCOUNT		= ''
    FN_CHQ_ISSUE	= 'F.CHEQUE.ISSUE'    
    F_CHQ_ISSUE		= ''
    FN_CHQ_ISSUE_HIS= 'F.CHEQUE.ISSUE$HIS'    
    F_CHQ_ISSUE_HIS	= ''
    FN_CH_CHRG_BAL	= 'F.CHEQUE.CHARGE.BAL'
    F_CH_CHRG_BAL	= ''
    FN_NOTE_CREDIT	= 'F.EB.SLV.NOTE.CREDIT'
    F_NOTE_CREDIT	= ''
    FN_CHG_REQ		= 'F.AC.CHARGE.REQUEST'
    F_CHG_REQ		= ''
    FN_HIS_CHG_REQ	= 'F.AC.CHARGE.REQUEST$HIS'
    F_HIS_CHG_REQ	= ''
    FN_ACC_PARAM	= 'F.EB.SLV.ACCOUNT.PARAM'
    F_ACC_PARAM		= ''

;*variables de trabajo
    BANDERA			= ''
    DATOS 			= ''
    CADENA 			= ''
    TIPO_FISCAL 	= ''
    RUTA.ARCHIVO 	= ''
    NOMBRE.PLANTILLA = ''
    FECHA.FORMATEADA = TODAY[1,4] : TODAY[5,2] : TODAY[7,2]
    FECHA_EMISION 	= TODAY[7,2] : '/' : TODAY[5,2] : '/' : TODAY[1,4]
    DESCRIPCION_STATIC='Ingresos varios'
    GIRO_COMERCIAL	='Varios'

;*Variables de cadena de datos para docuemento
    EMPRESAS =R.USER<EB.USE.COMPANY.CODE><1,1>
    AGENCIAS =R.USER<EB.USE.COMPANY.CODE><1,1>
    CORP='SV0010001'
    IF EMPRESAS EQ 'ALL' THEN
    	EMPRESAS=CORP
    END
    IF AGENCIAS EQ 'ALL' THEN
    	AGENCIAS=CORP
    END
    
    
*	EMPRESAS ='BANCOAZUL'
    TIPOFISC =''
    CODTRANS =''
    CORRELAT =''
    FECHAEMI =''
    
    CODCUSTO =''
    NOMBRERA =''
    DIRECCIO =''
    GIROCOME =''
    NITCLIEN =''
    NUMERNRC =''
    DETALLE  =''
    VENTAGRA =''
    IMPUESTO =''
    SUBTOTAL =''
    VENTANOS =''
    VENTAEXE =''
    IVARETEN =''
    IVAPERCI =''
    VENTANET =''
    VENTAIMP =''
    VENTATOT =''
    CANTILET =''

    TRANSACCION = ID.NEW
    CODIGO_CLIENTE	= ''
   	N_CUS_ACCOUNT	= ''
    S_CUS_NAME		= ''
    CUENTA.CREDITO 	= ''
    TRANSACCION.TFS = ''
    TIPO_CLIENTE	= ''
    NUMERO_NIT		= ''
    NUMERO_NRC		= ''
    N_CUS_TYPE_DOC  = ''
    A_TNX_TYPE		= ''
    S_DESC_COMM		= ''
    N_ACCOUNT_NO	= ''
    N_VAL_IN_DETA	= ''
    S_ISEXE			= ''
    N_VENTA_EXENTA	= ''
    S_TYPE_ENTRY	= ''
    S_DOCUMENT_RNEW= ''
    B_PAY_COMMI		=''
    S_DESC_COM_PREST=''
    FL_EXEP_DOC_COM=''

;*tipos de documentos 
    N_ISCCF			= 1
    N_ISFCF			= 2
    N_ISNDC			= 3
    N_ISNDD			= 4
    N_TYPE_DOC		= ''

;*tipos descripcion de documentos para vipo
    A_TYPE_DOC<1,1>='CREDITOFISCAL'
    A_TYPE_DOC<1,2>='FACTURA' 
    A_TYPE_DOC<1,3>='NOTACREDITO'
    A_TYPE_DOC<1,4>='NOTADEBITO'
	A_TYPE_DOC<1,5>='EXENTO'
;*documentos para seleccion    
    A_TYPE_DOC_RNEW<1>='FACTURA'
	A_TYPE_DOC_RNEW<2>='CREDITO FISCAL'
	A_TYPE_DOC_RNEW<3>='NOTACREDITO'
	A_TYPE_DOC_RNEW<4>='NOTA DE DEBITO'
        
 ;*tipos entradas
 	A_TYPE_ENTRY<1> ='TELL' 	
 	A_TYPE_ENTRY<2> ='FUND' 
 	A_TYPE_ENTRY<3> ='SERV' 
 	A_TYPE_ENTRY<4> ='COMM' 
 	 
;*Tipos de aplicaciones
    A_TYPE_APP<1>='TFS'
    A_TYPE_APP<2>='FT'
    A_TYPE_APP<3>='TT'
    A_TYPE_APP<4>='CH'

;*campo local de cliente en CUSTOMER
    CALL GET.LOC.REF('CUSTOMER', 'LF.TYPE.CUST', POSTypeCus)
;*campo local de cliente en TELLER O NRC  en teller plano   
    CALL GET.LOC.REF('TELLER', 'ATM.GEN.MAP.ID', POSCusNextVer) ;*Para trabajar con el id de cliente O NUMERO DE REGISTRO PARA TELLER PLANO
    CALL GET.LOC.REF('TELLER','AT.UNIQUE.ID', POSTypeDocTT) ;*almacena el tipo de documento 
;*campo local de cliente en FT O NRC  en teller plano   
    CALL GET.LOC.REF('FUNDS.TRANSFER', 'ATM.GEN.MAP.ID', POSNrcFt) ;*Para trabajar con el NUMERO DE REGISTRO PARA FT PLANO
    CALL GET.LOC.REF('FUNDS.TRANSFER','AT.UNIQUE.ID', POSTypeDocFT) ;*almacena el tipo de documento 
;*Campos locales comision por desembolso
    CALL GET.LOC.REF('FUNDS.TRANSFER','LF.DIF.AMT.COM',LOCPOSDferirAmt)
    CALL GET.LOC.REF('FUNDS.TRANSFER','LF.FIX.AMT.COM',LOCPOSPrductAmt)
    CALL GET.LOC.REF('FUNDS.TRANSFER','LF.CODE.COMM',LOCPOSCodeCom)    
    
        
;*compo local para docuemento de chequera
	CALL GET.LOC.REF('CHEQUE.ISSUE','LF.CHG.CODE', POSCodComm)
	CALL GET.LOC.REF('CHEQUE.ISSUE','LF.CHG.AMOUNT', POSAmtComm)
	CALL GET.LOC.REF('CHEQUE.ISSUE','LF.CHG.TAX.AMT', POSTaxComm)
	CALL GET.LOC.REF('CHEQUE.ISSUE','LF.CHG.DOC.TYPE', POSDocTypeComm)
	CALL GET.LOC.REF('CHEQUE.ISSUE','LF.CHG.NRC', POSNrcComm)

;*control de gravado y exento
	 CALL GET.LOC.REF('CUSTOMER', 'LF.EXE.TAX', POSExe)
;*monto de iva precalculado para creditos provenientes de originador
  CALL GET.LOC.REF('AA.ARR.TERM.AMOUNT','LF.CHG.TAX.AMT',LOCPOSIvaArr)	

    RETURN

*-------------------------------------------------------------------------------------
*Apertura de areas de trabajo
*-------------------------------------------------------------------------------------
OPENFILES:
;* Apertura de archivos a utilizar
    CALL OPF(FN_CUS, F_CUS)
    CALL OPF(FN_ARRNG, F_ARRNG)
    CALL OPF(FN_TELLER, F_TELLER)
    CALL OPF(FN_HIS_TELLER, F_HIS_TELLER)
    CALL OPF(FN_SLVPA, F_SLVPA)
    CALL OPF(FN_INDUST, F_INDUST)
    CALL OPF(FN_COMPANY,F_COMPANY)
    CALL OPF(FN_ACCOUNT,F_ACCOUNT)
	CALL OPF(FN_CHQ_ISSUE,F_CHQ_ISSUE)
	CALL OPF(FN_CH_CHRG_BAL,F_CH_CHRG_BAL)
	CALL OPF(FN_FUNDS_TR_HIS,F_FUNDS_TR_HIS)
	CALL OPF(FN_FUNDS_TRANS, F_FUNDS_TRANS)
	CALL OPF(FN_CHQ_ISSUE_HIS, F_CHQ_ISSUE_HIS)
	CALL OPF(FN_CHG_REQ, F_CHG_REQ)

	
    RETURN
*-------------------------------------------------------------------------------------
*Ejecucion de procesos 
*-------------------------------------------------------------------------------------    
PROCESS:
*-------------------------------------------------------------------------------------
;*Asignando valores a variables de acuerdo a APP utilizada
    GOSUB FIELD_BY_APP
;*Asignando valores para el documento
    GOSUB VAR_VALUE_DOC
;*componiendo data
    GOSUB COMPOSE_DATA
;*Manda ejecutar el enquiry
	GOSUB EXE_ENQ

    RETURN
*-------------------------------------------------------------------------------------
*Compone cadena para envio de datos a archivo
*-------------------------------------------------------------------------------------
COMPOSE_DATA:

    DATOS := 'EMPRESAS!' : EMPRESAS : CHAR(10)
    DATOS := 'TIPOFISC!' : TIPOFISC : CHAR(10)
    DATOS := 'CODTRANS!' : CODTRANS : CHAR(10)
    DATOS := 'CORRELAT!' : CHAR(10)
    DATOS := 'FECHAEMI!' : FECHAEMI : CHAR(10)
    DATOS := 'AGENCIAS!' : AGENCIA	: CHAR(10)
    DATOS := 'CODCUSTO!' : CODCUSTO : CHAR(10)
    DATOS := 'NOMBRERA!' : NOMBRERA	 : CHAR(10)
    DATOS := 'DIRECCIO!' : DIRECCIO : CHAR(10)
    DATOS := 'GIROCOME!' : GIROCOME : CHAR(10)
    DATOS := 'NITCLIEN!' : NITCLIEN : CHAR(10)
    DATOS := 'NUMERNRC!' : NUMERNRC : CHAR(10)
    DATOS :=  DETALLE : CHAR(10)
    DATOS := 'VENTAGRA!' : FMT(VENTAGRA, 'R2') : CHAR(10)
    DATOS := 'IMPUESTO!' : FMT(IMPUESTO, 'R2') : CHAR(10)
    DATOS := 'SUBTOTAL!' : FMT(SUBTOTAL, 'R2') : CHAR(10)
    DATOS := 'VENTANOS!' : FMT(VENTANOS, 'R2') : CHAR(10)
    DATOS := 'VENTAEXE!' : FMT(VENTAEXE, 'R2') : CHAR(10)
    DATOS := 'IVARETEN!' : '(':FMT(IVARETEN, 'R2'):')' : CHAR(10)
    DATOS := 'IVAPERCI!' : FMT(IVAPERCI, 'R2') : CHAR(10)
    DATOS := 'VENTANET!' : FMT(VENTANET, 'R2') : CHAR(10)
    DATOS := 'VENTAIMP!' : FMT(VENTAIMP, 'R2') : CHAR(10)
    DATOS := 'VENTATOT!' : FMT(VENTATOT, 'R2') : CHAR(10)
    DATOS := 'CANTILET!' : CANTILET : CHAR(10)

    A_DATA<-1> = DATOS

*------------------------------------------------------------------------------------------
IF B_PAY_COMMI EQ 'NO' OR B_PAY_COMMI EQ ''  THEN 
*generar archivo para documento
    CALL F.READ(FN_SLVPA, RUTA.ARCHIVO, R.TABLE.PA, F_SLVPA, F.ERR.PA)
    DIR.NAME = R.TABLE.PA<EB.SLV39.VALOR.PARAM>
*		DIR.NAME = 'C:\xgfc\mislib\'
    
    R.ID = TRANSACCION : '.txt'

;* Plantilla a utilizar
    CALL F.READ(FN_SLVPAPA, NOMBRE.PLANTILLA, R.TABLE.PA, F_SLVPA, F.ERR.PA)
    PLANTILLA = '(' : R.TABLE.PA<EB.SLV39.VALOR.PARAM> : ') STARTLM'

;* Armando la cadena para el archivo
    CADENA := '%!' : CHAR(10)
    CADENA := PLANTILLA : CHAR(10)
    CADENA := UPCASE(DATOS)
    CADENA := '%%EOF': CHAR(10)
;* Creando el archivo
    OPENSEQ DIR.NAME, R.ID TO SEQ.PTR ELSE
    END
    

;* Escribiendo la cadena en el archivo

    WRITESEQ CADENA ON SEQ.PTR ELSE
    E = 'EB-SLV.DAT.WRT.FAIL'
    CALL ERR
    END

;* Cerrando el archivo
    CLOSESEQ SEQ.PTR
END
    RETURN

*-------------------------------------------------------------------------------------
*Recupera valores de la aplicacion para componer datos iniciales
*-------------------------------------------------------------------------------------
FIELD_BY_APP:
*--------------------------------------------------------------------------------------	

*ID.NEW='FT16204C63JJ' 
*ID.NEW='CURR.10000000042968.0000017'
 ;*ID.NEW = 'CHG1521608938'
	S_DOCUMENT_RNEW	=	R.NEW(EB.EB.DOC.TYPE)

	N_ACCOUNT_NO 	= 	ID.NEW[6,14]
	
    T_APP			=	ID.NEW[1,2]
    
    
    ;*IF T_APP EQ 'CH' THEN
    ;*N_ACCOUNT_NO = R.NEW(CHG.DEBIT.ACCOUNT)
    ;*END
    
   ;*Id para parametro de nombre de comision siempre existe para administracion
   ID_CODE_CHARGE	=R.NEW(FT.LOCAL.REF)<1,LOCPOSCodeCom>
*   ID_CODE_CHARGE='CPEFP1'
*   AGENCIAS='SV0010001'
   ID_ACC_PARAM = AGENCIAS:'.':ID_CODE_CHARGE[1,5]
     

IF A_TYPE_DOC_RNEW<3> EQ S_DOCUMENT_RNEW THEN 
;*emision de notas de credito 
	TRANSACCION:='NC'
	GOSUB NOTE_CREDIT		
END ELSE   

	;*Definiendo valores en caso de FT
	*=========================================================================================
	    IF T_APP EQ A_TYPE_APP<2> THEN  
	    
	    	;*Extraer Monto de Liof
	        GOSUB GET.TAX.LIOF
	         	    		     	
	    	CODIGO_CLIENTE		= R.NEW(FT.DEBIT.CUSTOMER)								;*codigo de cliente    	
	   		DESCRIPCION			= R.NEW(FT.ORDERING.CUST)	     						;*Concepto de la transacci�n 
	        N_AMOUNT_TOT		= CHANGE(R.NEW(FT.TOTAL.CHARGE.AMOUNT),'USD','')		;*Monto neto sujeto
	        N_AMOUNT_TOT        = N_AMOUNT_TOT - Y.LIOF.AMT
			N_AMOUNT_NET		= CHANGE(R.NEW(FT.COMMISSION.AMT)<1,1>,'USD','') 			;*Monto sin iva		        
		
			N_AMOUNT_RATE 		= CHANGE(R.NEW(FT.TOTAL.TAX.AMOUNT),'USD','')			;*Monto de impuesto	
			N_AMOUNT_RATE       = N_AMOUNT_RATE - Y.LIOF.AMT
			S_GIRO				= ''													;*Giro economico        
	        S_CODE_COMM			= R.NEW(FT.COMMISSION.TYPE)	 							;*Codigo de comisi�n
	        S_NAME_CUS_OTH		= ''													;*Nombre del cliente
	        S_NRC_OTH			= R.NEW(FT.LOCAL.REF)<1,POSNrcFt>						;*NRC        
	        S_NIT_OTH			= ''					  								;*NIT
	        S_TYPE_ENTRY		= A_TYPE_ENTRY<2>       								;*TIPO DE ENTRADA
	        S_DOCUMENT_RNEW		= R.NEW(FT.LOCAL.REF)<1,POSTypeDocFT>					;*TIPO DOCUENTO 
	        B_PAY_COMMI			= ''
	        
	      ;*para comisiones por desembolso	        	        
	        	;*Monto total cargo
				AMOUNT_CHARGE	=	R.NEW(FT.LOCAL.REF)<1,LOCPOSDferirAmt>
				AMOUNT_COST		=	R.NEW(FT.LOCAL.REF)<1,LOCPOSPrductAmt>
				TOTAL_CHARGE    =	(AMOUNT_CHARGE + AMOUNT_COST )
				;*Codigo de comision
				;*Definiendo gravado o exento
				EXE_GRAV =ID_CODE_CHARGE[6,1]
				
	    	IF TOTAL_CHARGE NE 0 THEN
	    		;*Extrayendo datos concepto para la comsion en factura
					 CALL F.READ(FN_ACC_PARAM, ID_ACC_PARAM, R_ACC_PARAM, F_ACC_PARAM, Y_ACC_PAR)	
					S_DESC_COM_PREST	=R_ACC_PARAM<EB.SLV47.DESCRIPCION>	
  			
	    			N_AMOUNT_TOT 	=0
	    			N_AMOUNT_NET	=0
	    			N_AMOUNT_RATE	=0
	    			N_IVA_ARR		=0
	    			IF EXE_GRAV NE 0 THEN
	    						N_AMOUNT_RATE=0
	    						;*obtener iva en caso de que sea credito de personas
	    						;*obteniendo el numero id arr
	    						ACC_PRESTAMO	=R.NEW(FT.DEBIT.ACCT.NO)
								ARR_ID			=ACC_PRESTAMO
								CALL SLV.UTIL.GET.ARR.X.ACC(ARR_ID)
								CALL F.READ (FN_ARRANGEM, ARR_ID, R_ARRANGEM, F_ARRANGEM, ERR_ARRANGEM)		
	    					
	    						;**Term.amount
								;*--------------------------------------------------------------------------
								CALL AA.GET.ARRANGEMENT.CONDITIONS(ARR_ID,'TERM.AMOUNT','COMMITMENT',TODAY,RETURN.ID2, REC.AMT, ERROR2)      	 
								;**Campos locales
								ARR_LOCAL_FIELD_AMT = RAISE(REC.AMT)
	    						N_IVA_ARR	=	ARR_LOCAL_FIELD_AMT<AA.AMT.LOCAL.REF><1,LOCPOSIvaArr>

	    						N_AMOUNT_RATE= DROUND( (AMOUNT_CHARGE + AMOUNT_COST )  * 0.13, 2)
	    						;*si existe un valor proveniente de arr se asignara ese valor, de lo contrario quedara el calculado
	    						IF N_IVA_ARR GT 0  THEN	
	    							N_AMOUNT_RATE =0	
	    							N_AMOUNT_RATE = N_IVA_ARR	    							
	    						END	    			       
	    			END
	    				    			
	    			N_AMOUNT_NET = TOTAL_CHARGE
	    			N_AMOUNT_TOT = TOTAL_CHARGE + N_AMOUNT_RATE
	    	END  ELSE
		    	FL_EXEP_DOC_COM='SI'
	    	END
				        
	    END
	    
	;*Definiendo valores en caso de TT 
	*=========================================================================================
	
		IF 	T_APP EQ A_TYPE_APP<3> THEN
	    	CODIGO_CLIENTE		= R.NEW(TT.TE.CUSTOMER.2)								;*codigo de cliente    	
	   		DESCRIPCION			= R.NEW(TT.TE.PAYEE.NAME)     							;*Concepto de la transacci�n 
	        N_AMOUNT_TOT		= R.NEW(TT.TE.NET.AMOUNT)-R.NEW(TT.TE.AMOUNT.LOCAL.1)	;*Monto neto sujeto
			N_AMOUNT_NET		= R.NEW(TT.TE.CHRG.AMT.LOCAL)<1,1> 						;*Monto sin iva		        							
			N_AMOUNT_RATE		= R.NEW(TT.TE.CHRG.AMT.LOCAL)<1,2>						;*Monto de impuesto
			S_GIRO				= '' 													;*Giro economico        
	        S_CODE_COMM			= R.NEW(TT.TE.CHARGE.CODE) 								;*Codigo de comisi�n
	        S_NAME_CUS_OTH		= ''													;*Nombre del cliente
	        S_NRC_OTH			= R.NEW(TT.TE.LOCAL.REF)<1,POSCusNextVer>				;*NRC        
	        S_NIT_OTH			= '' 													;*NIT      
	        S_TYPE_ENTRY		= A_TYPE_ENTRY<1>       								;*TIPO DE ENTRADA
	        S_DOCUMENT_RNEW	= R.NEW(TT.TE.LOCAL.REF)<1,POSTypeDocTT>					;*TIPO DOCUENTO
	        B_PAY_COMMI     =   R.NEW(TT.TE.WAIVE.CHARGES)
	    END
	
	;*Definiendo valores en caso de TT para version de facturas por pago de comisiones varias
	*=========================================================================================
	    IF 	T_APP EQ A_TYPE_APP<3> AND FIELD(R.NEW(TT.TE.NARRATIVE.1),@SM,1) NE ''  THEN
	        CODIGO_CLIENTE		= R.NEW(TT.TE.LOCAL.REF)<1,POSCusNextVer>;*codigo de cliente    	
	   		DESCRIPCION			= FIELD(R.NEW(TT.TE.NARRATIVE.2),@SM,1) ;*Concepto de la transacci�n 
	        N_AMOUNT_TOT		= R.NEW(TT.TE.AMOUNT.LOCAL.1)			;*Monto neto sujeto
			N_AMOUNT_NET		= FIELD(R.NEW(TT.TE.NARRATIVE.1),@SM,1) ;*Monto sin iva		        
			N_AMOUNT_RATE 		= FIELD(R.NEW(TT.TE.NARRATIVE.1),@SM,2)	;*Monto de impuesto				
			S_GIRO				= FIELD(R.NEW(TT.TE.NARRATIVE.1),@SM,3) ;*Giro economico        
	        S_CODE_COMM			= FIELD(R.NEW(TT.TE.NARRATIVE.1),@SM,4) ;*Codigo de comisi�n
	        S_NAME_CUS_OTH		= FIELD(R.NEW(TT.TE.NARRATIVE.1),@SM,5) ;*Nombre del cliente
	        S_NRC_OTH			= FIELD(R.NEW(TT.TE.NARRATIVE.1),@SM,6) ;*NRC        
	        S_NIT_OTH			= R.NEW(TT.TE.PAYEE.NAME)    
	   		S_TYPE_ENTRY		= A_TYPE_ENTRY<4>				
	   		B_PAY_COMMI     	=   ''
	    END
	    
	;*Definiendo valores en caso de CHG : OCORNEJO 08.01.2016
	*=========================================================================================
	    IF 	T_APP EQ A_TYPE_APP<4> THEN
	    	CALL F.READ(FN_COMM_TYPE, R.NEW(CHG.CHARGE.CODE), R_COMM_TYPE, F_COMM_TYPE, ERR_COMM_TYPE)
	        CODIGO_CLIENTE		= R.NEW(CHG.CUSTOMER.NO)		;*codigo de cliente 
	   		DESCRIPCION			= R_COMM_TYPE<FT4.DESCRIPTION>	;*Concepto de la transacci�n 
	        N_AMOUNT_TOT		= R.NEW(CHG.TOTAL.CHG.AMT)		;*Monto neto sujeto
			N_AMOUNT_NET		= R.NEW(CHG.CHARGE.AMOUNT) 		;*Monto sin iva		        
			N_AMOUNT_RATE 		= R.NEW(CHG.TAX.AMT)			;*Monto de impuesto				
			S_GIRO				= R.NEW(CHG.EXTRA.DETAILS) 		;*Giro economico        
	        S_CODE_COMM			= R.NEW(CHG.CHARGE.CODE) 		;*Codigo de comisi�n
	        S_NAME_CUS_OTH		= R.NEW(CHG.SENDER.INFO) 		;*Nombre del cliente
	        S_NRC_OTH			= R.NEW(CHG.REMARKS) 			;*NRC        
	        S_NIT_OTH			= ''
	   		S_TYPE_ENTRY		= A_TYPE_ENTRY<4>				
	   		B_PAY_COMMI     	=   ''
	   		
	   		CALL GET.LOC.REF ('AC.CHARGE.REQUEST', 'LF.COD.CL', POS)
    		COLECTOR = R.NEW(CHG.LOCAL.REF)<1,POS>
   		 	
   		 	IF COLECTOR EQ '01-MH' THEN
   		 	
   		 	PARAMETRO='RETENCION.IVA.COL'
	   		CALL F.READ(FN_SLVPA, PARAMETRO, R.TABLE.PA, F_SLVPA, F.ERR.PA)
	   		
	   		IVAPORCENTAJE = R.TABLE.PA<EB.SLV39.VALOR.PARAM>
	   		IVARETEN = DROUND(N_AMOUNT_NET * IVAPORCENTAJE,4) 				;* Retenci�n 1%	   		
	   		N_AMOUNT_TOT = N_AMOUNT_TOT - IVARETEN
	   		
   		 	END
;*	   		ELSE 
;*		   		ETEXT ="ERROR:":COLECTOR
;*		   		CALL STORE.END.ERROR
;*	   		END
	   		
	   		
	    END
	    
	;*Definiendo valores en caso de documento prestacion de servicios(eje.emision de chequera) cuya semilla es la cuenta
	*=========================================================================================
	   ;* recuperando cliente a traves de la cuenta

	
	    CALL F.READ(FN_ACCOUNT, N_ACCOUNT_NO, R_ACCOUNT, F_ACCOUNT, F_ACCOUNT_ERR)
	    CALL F.READ(FN_CH_CHRG_BAL, ID.NEW, R_CH_CHRG_BA, F_CH_CHRG_BAL, E_CH_CHRG_BAL)
	
	    IF 	N_ACCOUNT_NO NE '' AND R_ACCOUNT<AC.CUSTOMER> NE ''  THEN    	    
	    	CODIGO_CLIENTE		= R_ACCOUNT<AC.CUSTOMER>								;*codigo de cliente    	
	   		DESCRIPCION			= 'Emision de chq. ': ID.NEW    						;*Concepto de la transacci�n 
	        N_AMOUNT_TOT		= R.NEW(CHEQUE.IS.LOCAL.REF)<1,POSAmtComm>+R.NEW(CHEQUE.IS.LOCAL.REF)<1,POSTaxComm>;*Monto neto sujeto
			N_AMOUNT_NET		= R.NEW(CHEQUE.IS.LOCAL.REF)<1,POSAmtComm> 				;*Monto sin iva		        							
			N_AMOUNT_RATE		= R.NEW(CHEQUE.IS.LOCAL.REF)<1,POSTaxComm>				;*Monto de impuesto
			S_GIRO				= '' 													;*Giro economico        
	        S_CODE_COMM			= R.NEW(CHEQUE.IS.LOCAL.REF)<1,POSCodComm> 				;*Codigo de comisi�n
	        S_NAME_CUS_OTH		= ''													;*Nombre del cliente
	        S_NRC_OTH			= R.NEW(CHEQUE.IS.LOCAL.REF)<1,POSNrcComm>				;*NRC        
	        S_NIT_OTH			= '' 													;*NIT	
	   		S_TYPE_ENTRY		= A_TYPE_ENTRY<3>										;*TIPO DE ENTRADA
	   		S_DOCUMENT_RNEW		= R.NEW(CHEQUE.IS.LOCAL.REF)<1,POSDocTypeComm>			;*TIPO DOCUENTO		
	   		B_PAY_COMMI			= ''			
	    END
END

IF  S_NRC_OTH EQ '-' THEN 
	S_NRC_OTH=''
END

;*si no es definida una descripcion como parte de los datos de entrada de asigna una gen�rica   
IF DESCRIPCION EQ '' THEN 
		DESCRIPCION =DESCRIPCION_STATIC
END

*  Datos iniciales de cliente
    CALL F.READ(FN_CUS, CODIGO_CLIENTE, R_CUS, F_CUS, F_CUS_ERR)
*  Datos iniciales del tipo de comsi�n 
	CALL F.READ(FN_COMM_TYPE, S_CODE_COMM, R_COMM_TYPE, F_COMM_TYPE, F_COMM_TYPE_ERR)
	
S_DESC_COMM	=	R_COMM_TYPE<FT4.DESCRIPTION>	
S_ISEXE		=   R_CUS<EB.CUS.LOCAL.REF><1,POSExe>

IF TOTAL_CHARGE NE 0 THEN
	S_DESC_COMM = S_DESC_COM_PREST
END

    RETURN

*-------------------------------------------------------------------------------------
*se defininen e identifica la distribuci�n de datos por tipo de documento
*-------------------------------------------------------------------------------------
VAR_VALUE_DOC:
;*buscando numero de NIT
    NO_MVNIT	=	DCOUNT(R_CUS<EB.CUS.LEGAL.ID>,VM)
    FOR Y=1 TO NO_MVNIT
        BEGIN CASE
            CASE  R_CUS<EB.CUS.LEGAL.DOC.NAME><1,Y> EQ 'NUM.IDEN.TRIBUT'
                NUMERO_NIT = R_CUS<EB.CUS.LEGAL.ID><1,Y>
            CASE 	R_CUS<EB.CUS.LEGAL.DOC.NAME><1,Y> EQ 'REGISTRO.FISCAL.IVA'
                NUMERO_NRC = R_CUS<EB.CUS.LEGAL.ID><1,Y>
        END CASE
    NEXT Y
    
;*numero proveniente de campo para nrc, tratamiento general	
IF  NUMERO_NRC EQ '' AND S_NRC_OTH NE '' THEN
	NUMERO_NRC = S_NRC_OTH
END

;*Tratamiento cuando aplicaciones especificas
IF S_TYPE_ENTRY EQ 'TELL' OR S_TYPE_ENTRY EQ 'SERV' THEN
	BEGIN CASE
		CASE S_DOCUMENT_RNEW EQ A_TYPE_DOC_RNEW<1> ;*factura
			NUMERO_NRC = ''
		CASE S_DOCUMENT_RNEW EQ A_TYPE_DOC_RNEW<2> ;*credito fiscal
			IF  NUMERO_NRC EQ '' AND S_NRC_OTH NE '' THEN
				NUMERO_NRC = S_NRC_OTH
			END
  	END CASE
END

;*definicion de tipo de documento por condiciones de cliente(para los casos en los que no se pueda definir documento siempre sera factura)
    N_TYPE_DOC			=	N_ISFCF
   
        IF NUMERO_NRC NE ''  THEN
            N_TYPE_DOC			=	N_ISCCF
        END ELSE
            N_TYPE_DOC			=	N_ISFCF
        END
        
        ;*tratamiento para notas de credito
        IF A_TYPE_DOC_RNEW<3> EQ S_DOCUMENT_RNEW THEN 
        	N_TYPE_DOC			=	N_ISNDC
        END
        
        ;* Validar si es un AC.Charge para MH y obligarlo a emitir factura aunque tenga NRC
        IF COLECTOR EQ '01-MH' AND T_APP EQ A_TYPE_APP<4> THEN
        N_TYPE_DOC			=	N_ISFCF
        END
       
;*asignacion de plantillas y parametro de impresion	
    BEGIN CASE
            ;*Comprobante de credito fiscal
        CASE N_TYPE_DOC EQ 1
            TIPO_FISCAL			=  A_TYPE_DOC<1,1>
            RUTA.ARCHIVO 		= 'RUTA.CRED.FISC'
            NOMBRE.PLANTILLA 	= 'PLANTILLA.CCF'

			GOSUB COMPOSE_QTY_VALUE
            GOSUB COMPOSE_CUA_VALUE
          
            ;*factura de consumidor final
        CASE N_TYPE_DOC EQ 2
            TIPO_FISCAL			=	A_TYPE_DOC<1,2>
            RUTA.ARCHIVO 		=  'RUTA.FACT.COM'
            NOMBRE.PLANTILLA 	=  'PLANTILLA.FCF'
            
            GOSUB COMPOSE_QTY_VALUE
            GOSUB COMPOSE_CUA_VALUE
            
              ;*Nota de credito
        CASE N_TYPE_DOC EQ 3
            TIPO_FISCAL			=	A_TYPE_DOC<1,3>
            RUTA.ARCHIVO 		=  'RUTA.CRED.FISC'
            NOMBRE.PLANTILLA 	=  'PLANTILLA.NDC'
            
            GOSUB COMPOSE_QTY_VALUE
            GOSUB COMPOSE_CUA_VALUE
            
              ;*Nota de debito
        CASE N_TYPE_DOC EQ 4
            TIPO_FISCAL			=	A_TYPE_DOC<1,4>
            RUTA.ARCHIVO 		=  'RUTA.CRED.FISC'
            NOMBRE.PLANTILLA 	=  'PLANTILLA.NDD' 
            
            GOSUB COMPOSE_QTY_VALUE
            GOSUB COMPOSE_CUA_VALUE   
    END CASE


    RETURN
*-------------------------------------------------------------------------------------
*Se definien datos cualitativos del documentos de acuerdo al tipo
*-------------------------------------------------------------------------------------
COMPOSE_CUA_VALUE:

*Direcci�n de compa�ia
COMPANY_ID=AGENCIAS
CALL F.READ(FN_COMPANY,COMPANY_ID, R_COMPANY, F_COMPANY, F_ERR_COMPANY)
 NO_COMP		=	DCOUNT(R_COMPANY<EB.COM.NAME.ADDRESS>,VM) 
 COMPANY_DPT 	=	R_COMPANY<EB.COM.NAME.ADDRESS><1,NO_COMP>
 COMPANY_MNP 	=	R_COMPANY<EB.COM.NAME.ADDRESS><1,NO_COMP-1>
 
 
    TIPO_CLIENTE ='NAT'
    TIPO_CLIENTE = R_CUS<EB.CUS.LOCAL.REF, POSTypeCus>[1,3]

    IF TIPO_CLIENTE EQ 'NAT' THEN
        ;* Encontrando el nombre completo
        PRIMER.NOMBRE = R_CUS<EB.CUS.NAME.1>
        SEGUNDO.NOMBRE = R_CUS<EB.CUS.NAME.2>
        TERCER.NOMBRE = R_CUS<EB.CUS.GIVEN.NAMES>
        PRIMER.APELLIDO = R_CUS<EB.CUS.TEXT>
        SEGUNDO.APELLIDO = R_CUS<EB.CUS.FAMILY.NAME>
        APELLIDO.CASADA = R_CUS<EB.CUS.PREVIOUS.NAME>
        NOMBRES = PRIMER.NOMBRE : ' ' : SEGUNDO.NOMBRE : ' ' : TERCER.NOMBRE
        APELLIDOS = PRIMER.APELLIDO : ' ' : SEGUNDO.APELLIDO : ' ' : APELLIDO.CASADA
        S_CUS_NAME = TRIM(NOMBRES : ' ' : APELLIDOS)
    END ELSE IF TIPO_CLIENTE EQ 'JUR' THEN
    
        ;* Encontrando la razon social
        CALL GET.LOC.REF('CUSTOMER', 'LF.RAZON.SOCIAL', POS.RAZON.SOCIAL)
        S_CUS_NAME = R_CUS<EB.CUS.LOCAL.REF, POS.RAZON.SOCIAL>
      	;*Se le da tratamiento a multivalor(definir una rubroutine general para tratamiento de campos y caracteres)
      	S_CUS_NAME = SWAP(R_CUS<EB.CUS.LOCAL.REF, POS.RAZON.SOCIAL>, @SM, ' ') 
    END

;* Componiendo la direccion del ciente
    CALL GET.LOC.REF('CUSTOMER', 'LF.CANTON', POS.CANTON)
    CANTON = R_CUS<EB.CUS.LOCAL.REF, POS.CANTON>
    CALL GET.LOC.REF('CUSTOMER', 'LF.COLONIA', POS.COLONIA)
    COLONIA = R_CUS<EB.CUS.LOCAL.REF, POS.COLONIA>
    CALL GET.LOC.REF('CUSTOMER', 'LF.CALLE', POS.CALLE)
    CALLE = R_CUS<EB.CUS.LOCAL.REF, POS.CALLE>
    CALL GET.LOC.REF('CUSTOMER', 'LF.AVENIDA', POS.AVENIDA)
    AVENIDA = R_CUS<EB.CUS.LOCAL.REF, POS.AVENIDA>
    CALL GET.LOC.REF('CUSTOMER', 'LF.NUM.DEPTO', POS.NUM.DEPTO)
    NUM.DEPTO = R_CUS<EB.CUS.LOCAL.REF, POS.NUM.DEPTO>
    DIREC_CLIENTE = TRIM(CANTON : ' ' : COLONIA : ' ' : CALLE : ' ' : AVENIDA : ' ' : NUM.DEPTO)

;* Encontrando el giro comercial (actividad económica)
    CODIGO_GIRO =R_CUS<EB.CUS.INDUSTRY>
    CALL F.READ(FN_INDUST, CODIGO_GIRO, R_INDUST, F_INDUST, F.ERR.IND)
    GIRO_COMERCIAL = R_INDUST<EB.IND.DESCRIPTION>

;* Llamando la clase para convertir números a letras
    THIS.PACKAGE.CLASS = 'com.slv.ba.convertidor.NumeroLetrasFiscal'
    THIS.METHOD = 'convertNumberToLetter'
    CALLJ.ARGUMENTS = N_AMOUNT_TOT
    CALLJ.ERROR = ''
    CALLJ.RESPONSE = ''
    
;* Llamando a clase con método estático
    CALL EB.CALLJ(THIS.PACKAGE.CLASS, '$' : THIS.METHOD, CALLJ.ARGUMENTS, CALLJ.RESPONSE, CALLJ.ERROR)
    CANTILET 	=	CALLJ.RESPONSE


DESCRIPCION =TRANSACCION:"-":DESCRIPCION

AGENCIA=R.USER<EB.USE.COMPANY.CODE><1,1> 
CORP='SV0010001'
    
    IF AGENCIA EQ 'ALL' THEN
    AGENCIA=CORP
    END


LINE_CONCEPT='DETALLES!' : S_DESC_COMM : '!' : FMT(NOSUJETA, 'R2') : '!' : FMT(N_VENTA_EXENTA, 'R2') : '!' : FMT(N_VAL_IN_DETA, 'R2') : '!':CHAR(10)
LINE_CONCEPT:='DETALLES!' : DESCRIPCION 


;* Validar si es un AC.Charge para MH y cambiar la el detalle de la factura

        
       IF COLECTOR EQ '01-MH' AND T_APP EQ A_TYPE_APP<4> THEN
        SUBTOTAL		=N_AMOUNT_TOT +  IVARETEN
        LINE_CONCEPT='DETALLES!' : S_DESC_COMM : '!' : FMT(NOSUJETA, 'R2') : '!' : FMT(N_VENTA_EXENTA, 'R2') : '!' : FMT(SUBTOTAL, 'R2') : '!':CHAR(10)
	    LINE_CONCEPT:='DETALLES!' : DESCRIPCION 
       
        PARAMETRO2='ACC.MH.NO.CONTRATO'
        
        CALL F.READ(FN_SLVPA, PARAMETRO2, R.TABLE.PA, F_SLVPA, F.ERR.PA)
        NO.CONTRATO = R.TABLE.PA<EB.SLV39.VALOR.PARAM>
        LINE_CONCEPT:='!':CHAR(10)
	   	LINE_CONCEPT:= 'DETALLES!' : 'No. de Contrato: ':NO.CONTRATO:'!':CHAR(10)
	   
	   	PARAMETRO3='ACC.MH.FECHA.CONTRATO'
	   	CALL F.READ(FN_SLVPA, PARAMETRO3, R.TABLE.PA, F_SLVPA, F.ERR.PA)
        FECHA.CONTRATO = R.TABLE.PA<EB.SLV39.VALOR.PARAM>
	   	LINE_CONCEPT:='DETALLES!':'Fecha de Contrato: ':FECHA.CONTRATO:'!':CHAR(10)
	   	
	   	FECHA.TODAY=TODAY
	    ANIO.TODAY= FECHA.TODAY[1,4] 
	    MES.TODAY= FECHA.TODAY[5,2] 
	    
	   
        BEGIN CASE
            CASE  MES.TODAY EQ '01'
           	MES.LETRAS='ENERO'
           	CASE  MES.TODAY EQ '02'
           	MES.LETRAS='FEBRERO'
           	CASE  MES.TODAY EQ '03'
           	MES.LETRAS='MARZO'
           	CASE  MES.TODAY EQ '04'
           	MES.LETRAS='ABRIL'
           	CASE  MES.TODAY EQ '05'
           	MES.LETRAS='MAYO'
           	CASE  MES.TODAY EQ '06'
           	MES.LETRAS='JUNIO'
           	CASE  MES.TODAY EQ '07'
           	MES.LETRAS='JULIO'
           	CASE  MES.TODAY EQ '08'
           	MES.LETRAS='AGOSTO'
           	CASE  MES.TODAY EQ '09'
           	MES.LETRAS='SEPTIEMBRE'
           	CASE  MES.TODAY EQ '10'
           	MES.LETRAS='OCTUBRE'
           	CASE  MES.TODAY EQ '11'
           	MES.LETRAS='NOVIEMBRE'
           	CASE  MES.TODAY EQ '12'
           	MES.LETRAS='DICIEMBRE'
           
        END CASE
       LINE_CONCEPT:='DETALLES!':'Periodo: ':MES.LETRAS:' ':ANIO.TODAY:'!':CHAR(10)
       LINE_CONCEPT:='DETALLES!':'Canal: Banca en Linea'
    
END
    
;*sustituye con el ingresado por el usuario
IF GIRO_COMERCIAL EQ '' THEN 
GIRO_COMERCIAL=S_GIRO
END

  
IF CODIGO_CLIENTE EQ '' THEN
 	 GOSUB CUS_BLUE_BANK
END ELSE
;*Asignando valores
    TIPOFISC	=	TIPO_FISCAL
    CODTRANS	=	TRANSACCION
    FECHAEMI	=	FECHA_EMISION
    CODCUSTO	=	CODIGO_CLIENTE
    NOMBRERA	=	S_CUS_NAME
    DIRECCIO	=	DIREC_CLIENTE
    GIROCOME	=	GIRO_COMERCIAL
    NITCLIEN	=	NUMERO_NIT
    NUMERNRC	=	NUMERO_NRC
    DETALLE 	=	LINE_CONCEPT
  
 END 
    RETURN
*-------------------------------------------------------------------------------------
*Se definien datos cuantitativos del documento de acuerdo al tipo
*-------------------------------------------------------------------------------------
COMPOSE_QTY_VALUE:

*;Valor a mostrar en leyenda concepta detalle de documento fiscal
N_VAL_IN_DETA = N_AMOUNT_TOT
IF N_TYPE_DOC EQ  N_ISCCF OR N_TYPE_DOC EQ  N_ISNDC  THEN
    N_VAL_IN_DETA	=	N_AMOUNT_NET
END


;* cantidades para documento
    VENTA_GRABADA 	=N_AMOUNT_NET
    IMPUESTO		=N_AMOUNT_RATE
    SUBTOTAL		=N_AMOUNT_TOT
    VENTA_NO_SUJETA	=0
    VENTA_EXENTA	=N_VENTA_EXENTA
    IVA_RETENIDO	=0
    IVA_PERCIBIDO	=0
    VENTA_NETA		=N_AMOUNT_NET
    VENTA_IMPUESTO	=N_AMOUNT_RATE
    VENTA_TOTAL		=N_AMOUNT_TOT

	IF COLECTOR EQ '01-MH' AND T_APP EQ A_TYPE_APP<4> THEN
	IVA_RETENIDO	=IVARETEN
	
    SUBTOTAL		=N_AMOUNT_TOT   + IVARETEN
    VENTA_GRABADA 	=N_AMOUNT_TOT   + IVARETEN
    END



*; en caso de documentos exentos
IF S_ISEXE EQ 'SI' THEN
		N_VENTA_EXENTA=N_VAL_IN_DETA
		N_VAL_IN_DETA=0
		IMPUESTO=0
		SUBTOTAL=N_VENTA_EXENTA
		VENTA_TOTAL=N_VENTA_EXENTA
		VENTA_EXENTA=N_VENTA_EXENTA
END
	
    VENTAGRA	=	VENTA_GRABADA
    IMPUESTO	=	IMPUESTO
    SUBTOTAL	=	SUBTOTAL
    VENTANOS	=	VENTA_NO_SUJETA
    VENTAEXE	=	VENTA_EXENTA
    IVARETEN	=	IVA_RETENIDO
    IVAPERCI	=	IVA_PERCIBIDO
    VENTANET	=	VENTA_NETA
    VENTAIMP	=	VENTA_IMPUESTO
    VENTATOT	=	VENTA_TOTAL

    RETURN
*-------------------------------------------------------------------------------------
*Se definien datos cualitativos fijos con datos del banco, esto si resulta que no existe cliente
*sustituir por parametros globales
*-------------------------------------------------------------------------------------    
CUS_BLUE_BANK:
	TIPOFISC	=	TIPO_FISCAL
    CODTRANS	=	TRANSACCION
    FECHAEMI	=	FECHA_EMISION
    CODCUSTO	=	1
    NOMBRERA	=	S_NAME_CUS_OTH
    DIRECCIO	=	COMPANY_MNP:",":COMPANY_MNP
    GIROCOME	=	GIRO_COMERCIAL
    NITCLIEN	=	S_NIT_OTH
    NUMERNRC	=	NUMERO_NRC
    AGENCIA		=  	COMPANY_ID
    DETALLE 	=  	LINE_CONCEPT
  RETURN
  
*-----------------------------------------------------------------------------------
*compone data basica para notas de credito
*-----------------------------------------------------------------------------------
NOTE_CREDIT:


 ;* Se agrega esta ';1' al id tomando en cuenta que es producto de una reversa y que esta permanece en historico
	ID_TNX 			= ID.NEW:';1'	
 	 CALL F.READ(FN_ACCOUNT, N_ACCOUNT_NO, R_ACCOUNT, F_ACCOUNT, F_ACCOUNT_ERR)
   
  ;*FT  se da tratamiento a los casos en los que las transacciones no dependendan de una reversa (chq certificado)
 IF T_APP EQ A_TYPE_APP<2> THEN
  	CALL F.READ(FN_FUNDS_TR_HIS, ID_TNX, R_FUNDS_TRANFER, F_FUNDS_TR_HIS, E_FUNDS_TRA)
	   IF R_FUNDS_TRANFER EQ '' THEN
	    	ID_TNX 			= ID.NEW
	   		CALL F.READ(FN_FUNDS_TRANS, ID_TNX, R_FUNDS_TRANFER, F_FUNDS_TRANS, E_FUNDS_TRA)
	   END
 END 
 
 ;*TT
 IF T_APP EQ A_TYPE_APP<3> THEN     
  CALL F.READ(FN_HIS_TELLER, ID_TNX, R_TELLER, F_HIS_TELLER, E_TELLER)
   		IF R_TELLER EQ '' THEN
	    	ID_TNX 			= ID.NEW
	   		CALL F.READ(FN_TELLER, ID_TNX, R_TELLER, F_TELLER, E_TELLER)
	   END
  END
 
  ;*CH : OCORNEJO - 08.01.2016
  ;*--------------------------
 IF T_APP EQ A_TYPE_APP<4> THEN     
  CALL F.READ(FN_HIS_CHG_REQ, ID_TNX, R_CHG_REQ, F_HIS_CHG_REQ, E_CHG_REQ)
   		IF R_CHG_REQ EQ '' THEN
	    	ID_TNX 			= ID.NEW
	   		CALL F.READ(FN_CHG_REQ, ID_TNX, R_CHG_REQ, F_CHG_REQ, E_CHG_REQ)
	   END
  END
  
    ;*CHEQUE.ISSUE
  IF 	N_ACCOUNT_NO NE '' AND R_ACCOUNT<AC.CUSTOMER> NE '' THEN
  CALL F.READ(FN_FUNDS_TR_HIS, ID_TNX, R_CHQ_ISS, F_CHQ_ISSUE_HIS, E_CH_ISS)
  		IF R_TELLER EQ '' THEN
	    	ID_TNX 			= ID.NEW
	   		CALL F.READ(FN_CHQ_ISSUE, ID_TNX, R_CHQ_ISS, F_CHQ_ISSUE, E_CH_ISS)
	   END
  END

;*FT plano     
*   ======================================================================================== 	
    IF T_APP EQ A_TYPE_APP<2> THEN  
    	
    	;*Extraer Monto de Liof : OCORNEJO 11.04.2017
	    GOSUB GET.TAX.LIOF
    	
    	CODIGO_CLIENTE		= R_FUNDS_TRANFER<FT.DEBIT.CUSTOMER>							;*codigo de cliente    	
   		DESCRIPCION			= R_FUNDS_TRANFER<FT.ORDERING.CUST>:' CCF REF.':R.NEW(EB.EB.REF.CCF)     						;*Concepto de la transacci�n 
        N_AMOUNT_TOT		= CHANGE(R_FUNDS_TRANFER<FT.TOTAL.CHARGE.AMOUNT>,'USD','')		;*Monto neto sujeto
        N_AMOUNT_TOT        = N_AMOUNT_TOT - Y.LIOF.AMT
		N_AMOUNT_NET		= CHANGE(R_FUNDS_TRANFER<FT.COMMISSION.AMT>,'USD','') 			;*Monto sin iva		        
		N_AMOUNT_RATE 		= CHANGE(R_FUNDS_TRANFER<FT.TOTAL.TAX.AMOUNT>,'USD','')			;*Monto de impuesto	
		N_AMOUNT_RATE       = N_AMOUNT_RATE - Y.LIOF.AMT			
		S_GIRO				= ''															;*Giro economico        
        S_CODE_COMM			= R_FUNDS_TRANFER<FT.COMMISSION.TYPE>	 						;*Codigo de comisi�n
        S_NAME_CUS_OTH		= ''															;*Nombre del cliente
        S_NRC_OTH			= R_FUNDS_TRANFER<FT.LOCAL.REF><1,POSNrcFt>						;*NRC        
        S_NIT_OTH			= ''					  										;*NIT
        S_TYPE_ENTRY		= A_TYPE_ENTRY<2>       										;*TIPO DE ENTRADA
*       S_DOCUMENT_RNEW		= ''														;*TIPO DOCUENTO 
END
CRT S_DOCUMENT_RNEW
;*emision de cheque de caja    
 IF T_APP EQ A_TYPE_APP<3> THEN 
*=========================================================================================
    	CODIGO_CLIENTE		= R_TELLER<TT.TE.CUSTOMER.2>								;*codigo de cliente    	
   		DESCRIPCION			= R_TELLER<TT.TE.PAYEE.NAME>:' CCF REF.':R.NEW(EB.EB.REF.CCF)     							;*Concepto de la transacci�n 
        N_AMOUNT_TOT		= R_TELLER<TT.TE.NET.AMOUNT>-R_TELLER<TT.TE.AMOUNT.LOCAL.1>;*Monto neto sujeto
		N_AMOUNT_NET		= R_TELLER<TT.TE.CHRG.AMT.LOCAL><1,1> 						;*Monto sin iva		        							
		N_AMOUNT_RATE		= R_TELLER<TT.TE.CHRG.AMT.LOCAL><1,2>						;*Monto de impuesto
		S_GIRO				= '' 														;*Giro economico        
        S_CODE_COMM			= R_TELLER<TT.TE.CHARGE.CODE> 								;*Codigo de comisi�n
        S_NAME_CUS_OTH		= ''														;*Nombre del cliente
        S_NRC_OTH			= R_TELLER<TT.TE.LOCAL.REF><1,POSCusNextVer>				;*NRC        
        S_NIT_OTH			= '' 														;*NIT      
        S_TYPE_ENTRY		= A_TYPE_ENTRY<1>       									;*TIPO DE ENTRADA
*       S_DOCUMENT_RNEW		= ''														;*TIPO DOCUENTO
      
 END
;*comisiones varias  
  IF 	T_APP EQ A_TYPE_APP<3> AND FIELD(R_TELLER<TT.TE.NARRATIVE.1>,@SM,1) NE ''  THEN
   
   		CODIGO_CLIENTE		= R_TELLER<TT.TE.LOCAL.REF><1,POSCusNextVer>;*codigo de cliente    	
   		DESCRIPCION			= FIELD(R_TELLER<TT.TE.NARRATIVE.2>,@SM,1):' CCF REF.':R.NEW(EB.EB.REF.CCF) ;*Concepto de la transacci�n 
        N_AMOUNT_TOT		= R_TELLER<TT.TE.AMOUNT.LOCAL.1>			;*Monto neto sujeto
		N_AMOUNT_NET		= FIELD(R_TELLER<TT.TE.NARRATIVE.1>,@SM,1) ;*Monto sin iva		        
		N_AMOUNT_RATE 		= FIELD(R_TELLER<TT.TE.NARRATIVE.1>,@SM,2)	;*Monto de impuesto				
		S_GIRO				= FIELD(R_TELLER<TT.TE.NARRATIVE.1>,@SM,3) ;*Giro economico        
        S_CODE_COMM			= FIELD(R_TELLER<TT.TE.NARRATIVE.1>,@SM,4) ;*Codigo de comisi�n
        S_NAME_CUS_OTH		= FIELD(R_TELLER<TT.TE.NARRATIVE.1>,@SM,5) ;*Nombre del cliente
        S_NRC_OTH			= FIELD(R_TELLER<TT.TE.NARRATIVE.1>,@SM,6) ;*NRC        
        S_NIT_OTH			= R_TELLER<TT.TE.PAYEE.NAME>    
   		S_TYPE_ENTRY		= A_TYPE_ENTRY<4>		

   END
 
 ;*Charges : OCORNEJO - 08.01.2016
 ;*-------------------------------
 IF T_APP EQ A_TYPE_APP<4> THEN 
*=========================================================================================
		CALL F.READ(FN_COMM_TYPE, R_CHG_REQ<CHG.CHARGE.CODE>, R_COMM_TYPE, F_COMM_TYPE, ERR_COMM_TYPE)
    	CODIGO_CLIENTE		= R_CHG_REQ<CHG.CUSTOMER.NO>								;*codigo de cliente    	
   		DESCRIPCION			= R_COMM_TYPE<FT4.DESCRIPTION>								;*Concepto de la transacci�n 
        N_AMOUNT_TOT		= R_CHG_REQ<CHG.TOTAL.CHG.AMT>								;*Monto neto sujeto
		N_AMOUNT_NET		= R_CHG_REQ<CHG.CHARGE.AMOUNT>		 						;*Monto sin iva		        							
		N_AMOUNT_RATE		= R_CHG_REQ<CHG.TAX.AMT>									;*Monto de impuesto
		S_GIRO				= R_CHG_REQ<CHG.EXTRA.DETAILS>								;*Giro economico        
        S_CODE_COMM			= R_CHG_REQ<CHG.CHARGE.CODE> 								;*Codigo de comisi�n
        S_NAME_CUS_OTH		= R_CHG_REQ<CHG.SENDER.INFO>								;*Nombre del cliente
        S_NRC_OTH			= R_CHG_REQ<CHG.REMARKS>									;*NRC        
        S_NIT_OTH			= '' 														;*NIT      
        S_TYPE_ENTRY		= A_TYPE_ENTRY<4>       									;*TIPO DE ENTRADA
*       S_DOCUMENT_RNEW		= ''														;*TIPO DOCUENTO
      
 END
;*venta de chequeras 
*=========================================================================================	 
		CALL F.READ(FN_ACCOUNT, N_ACCOUNT_NO, R_ACCOUNT, F_ACCOUNT, F_ACCOUNT_ERR)
	    CALL F.READ(FN_CH_CHRG_BAL, ID.NEW, R_CH_CHRG_BA, F_CH_CHRG_BAL, E_CH_CHRG_BAL)
   IF 	N_ACCOUNT_NO NE '' AND R_ACCOUNT<AC.CUSTOMER> NE ''  THEN     
    	CODIGO_CLIENTE		= R_ACCOUNT<AC.CUSTOMER>								;*codigo de cliente    	
   		DESCRIPCION			= 'Emision de chq. ': ID_TNX :' CCF REF.':R.NEW(EB.EB.REF.CCF)   						;*Concepto de la transacci�n 
        N_AMOUNT_TOT		= R_CHQ_ISS<CHEQUE.IS.LOCAL.REF><1,POSAmtComm>+R_CHQ_ISS<CHEQUE.IS.LOCAL.REF><1,POSTaxComm>;*Monto neto sujeto
		N_AMOUNT_NET		= R_CHQ_ISS<CHEQUE.IS.LOCAL.REF><1,POSAmtComm> 			;*Monto sin iva		        							
		N_AMOUNT_RATE		= R_CHQ_ISS<CHEQUE.IS.LOCAL.REF><1,POSTaxComm>			;*Monto de impuesto
		S_GIRO				= '' 													;*Giro economico        
        S_CODE_COMM			= R_CHQ_ISS<CHEQUE.IS.LOCAL.REF><1,POSCodComm> 			;*Codigo de comisi�n
        S_NAME_CUS_OTH		= ''													;*Nombre del cliente
        S_NRC_OTH			= R_CHQ_ISS<CHEQUE.IS.LOCAL.REF><1,POSNrcComm>			;*NRC        
        S_NIT_OTH			= '' 													;*NIT	
   		S_TYPE_ENTRY		= A_TYPE_ENTRY<3>										;*TIPO DE ENTRADA
*       S_DOCUMENT_RNEW		= ''													;*TIPO DOCUENTO					

	END  
  RETURN
 
;*Metodo para Obtener LIOF de la Txn : OCORNEJO 21.11.2016
;*-------------------------------------------------------
GET.TAX.LIOF:
	Y.LIOF = '15' ;*15 = LiofCheque
	Y.LIOF.AMT = 0
	
	BEGIN CASE
		;*Para FUNDS.TRANSFER
		CASE T_APP EQ A_TYPE_APP<2>
			FIND Y.LIOF IN R_FUNDS_TRANFER<FT.TAX.TYPE> SETTING Ap, Vp THEN
				Y.LIOF.AMT = R_FUNDS_TRANFER<FT.TAX.AMT><Ap, Vp>
			END
			ELSE
				FIND Y.LIOF IN R.NEW(FT.TAX.TYPE) SETTING Ap, Vp THEN
					Y.LIOF.AMT = R.NEW(FT.TAX.AMT)<Ap, Vp>
				END
			END
	END CASE
	
	Y.LIOF.AMT = CHANGE(Y.LIOF.AMT,'USD','')
RETURN
  
*-------------------------------------------------------------------------------------
* Ejecuta el enquiry con el parametro fijo de la transacci�n
*-------------------------------------------------------------------------------------       
EXE_ENQ:
	;*tratamiento parametro de exoneracion en comisiones
	IF B_PAY_COMMI EQ 'NO' OR B_PAY_COMMI EQ '' OR FL_EXEP_DOC_COM EQ '' THEN  
		Y.TASK = 'ENQ SLV.E.NOF.FISC.DOC TRANSACTION.ID EQ ': TRANSACCION
	    CALL EB.SET.NEW.TASK(Y.TASK)
    END         
RETURN   
 END
