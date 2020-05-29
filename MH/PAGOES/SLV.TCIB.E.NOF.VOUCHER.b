*-----------------------------------------------------------------------------
* <Rating>848</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE SLV.TCIB.E.NOF.VOUCHER(OUT.ARRAY)
*-----------------------------------------------------------------------------
* Program Description: Rutina de comprobantes para TCIB
*-----------------------------------------------------------------------------
* Version 1.0 21/09/2014  FBatres 		Initial
* Version 1.1 			  RCortez 		Ajuste y correcciones
* Version 1.2 10/11/2015  RGaray 		Se modifica la recuperaci�n del monto de transaccion cuando se trata de activity, se modifica el case
* Version 1.3 15/12/2015  RCortes 		Optimizaci�n + correcciones + adici�n de funcinalidad
* Version 1.4 26/01/2016  FBatres		Adici�n y formateo de fecha y hora de la transaccion con TimeZone  
* Version 1.5			  RGaray		Adicionar campos de LIOF
* Version 1.6 26/04/2016  FBatres		Modificar campos de LIOF para lectura correcta
* Version 1.7 26/04/2016  Sunder        To consider INAO case of FT - SIT issue no : 7851
* Version 1.8 18/05/2016  RCortes		Modificacion de leyendas
* Version 1.9 20/05/2016  FBatres		Fix "Comprobante de - Null" INAO's
* Version 2.0 30/05/2016  RCortes		Ticket 6745 - Se agregado la siguiente informaci�n para las transferencias ACAT, ACIB, ACPE
*											[Titular de la cuenta del beneficiario, Fecha de sistema, y Tipo de producto]
* Version 2.1 13/06/2014  FBatres		Se cambi� localzone a America/El_Salvador
* Version 2.2 08/07/2016  RGaray		Se devolvi� calculo de fecha, cuando es antigua no invierte posiciones. Esto funciona para certificacion PAR.
* Version 2.3 14/07/2016  RCortes		Modificaciones para la informacion a transferencias a terceros (titular, prestamo, etc) 
* Version 2.4 07/09/2016  RCortes		Modificacion del Tipo de Transaccion de Pagos de prestamo de terceros
* Version 2.4 27/09/2016  RGaray		Adicion de nombre de colector para txn AC64 
* Version 2.5 23.11.2016  RGaray		Integracion de comprobante MH para consultas en Banca
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.FUNDS.TRANSFER
    $INSERT I_F.STMT.ENTRY
    $INSERT I_F.FT.TXN.TYPE.CONDITION
    $INSERT I_F.TRANSACTION
    $INSERT I_ENQUIRY.COMMON
    $INSERT I_F.TELLER
    $INSERT I_F.TELLER.TRANSACTION
    $INSERT I_F.AA.ARRANGEMENT.ACTIVITY
    $INSERT I_F.AA.ACTIVITY
    $INSERT I_F.TELLER.FINANCIAL.SERVICES
    $INSERT I_F.TFS.TRANSACTION
    $INSERT I_F.AC.CHARGE.REQUEST
    $INSERT I_F.FT.COMMISSION.TYPE
    $INSERT I_F.CHEQUE.ISSUE
    $INSERT I_F.EB.SLV.GLOBAL.PARAM
    $INSERT I_F.ACCOUNT
    $INSERT I_F.CUSTOMER
    $INSERT I_F.EB.SLV.COLECTOR 
    $INSERT I_F.EB.SLV.COL.FRONT.END 
     
*----------------------------------------------------------------------------- 
    GOSUB INIT
    GOSUB OPENFILES
    GOSUB PROCESS
    CRT OUT.ARRAY
    RETURN
*-----------------------------------------------------------------------------

INIT:
    OUT.ARRAY 		= ''

    FN.TABLE   		= 'F.FUNDS.TRANSFER'
    F.TABLE   		= ''

*7851 - Start	
	FN.FUNDS.TRANSFER.NAU = 'F.FUNDS.TRANSFER$NAU'
	F.FUNDS.TRANSFER.NAU = ''
*7851 - End

    FN.TXN.TYPE  	= 'F.FT.TXN.TYPE.CONDITION'
    F.TXN.TYPE  	= ''

    FN.STMT.ENTRY 	= 'F.STMT.ENTRY'
    F.STMT.ENTRY 	= ''

    FN.TRANS   		= 'F.TRANSACTION'
    F.TRANS   		= ''

    FN.FT$HIS		= 'F.FUNDS.TRANSFER$HIS'
    F.FT$HIS 		= ''

    FN.TT 			= 'F.TELLER'
    F.TT			= ''

    FN.TT$HIS		= 'F.TELLER$HIS'
    F.TT$HIS 		= ''

    FN.TT.TYPE 		= 'F.TELLER.TRANSACTION'
    F.TT.TYPE		= ''

    FN.AA.ACT		= 'F.AA.ARRANGEMENT.ACTIVITY'
    F.AA.ACT		= ''

    FN.AA.TYPE		= 'F.AA.ACTIVITY'
    F.AA.TYPE		= ''

    FN.TFS.ACT 		= 'F.TELLER.FINANCIAL.SERVICES'
    F.TFS.ACT		= ''

    FN.TFS.ACT$HIS	= 'F.TELLER.FINANCIAL.SERVICES$HIS'
    F.TFS.ACT$HIS	= ''

    FN.TFS.TYPE  	= 'F.TFS.TRANSACTION'
    F.TFS.TYPE  	= ''

    FN.CHG			= 'F.AC.CHARGE.REQUEST'
    F.CHG 			= ''

    FN.CHG$HIS		= 'F.AC.CHARGE.REQUEST$HIS'
    F.CHG$HIS		= ''

    FN.CHG.TYPE 	= 'F.FT.COMMISSION.TYPE'
    F.CHG.TYPE 		= ''

    FN.CURR			= 'F.CHEQUE.ISSUE'
    F.CURR			= ''

    FN.CURR$HIS		= 'F.CHEQUE.ISSUE$HIS'
    F.CURR$HIS		= ''

	FN.EB.SLV.GLOBAL.PARAM = 'F.EB.SLV.GLOBAL.PARAM'
    F.EB.SLV.GLOBAL.PARAM = ''
    
    FN.COLECTOR 	= 'F.EB.SLV.COLECTOR' 
    F.COLECTOR		= '' 

*6745 - Start	
    FN.ACC			= 'F.ACCOUNT'
    F.ACC			= ''
    
    FN.CUS			= 'F.CUSTOMER'
    F.CUS			=''
*6745 - Start	

;* RGARAY 20161123 - adicion de archivo de App Local MH 
	FN.EB.SLV.COL.FRONT.END = 'F.EB.SLV.COL.FRONT.END' 
	F.EB.SLV.COL.FRONT.END = '' 

    EQU FT 	  TO 'FT'
    EQU AA 	  TO 'AAAA'
    EQU TT 	  TO 'TT'
    EQU TFS   TO 'TFS'
    EQU CHG   TO 'CHG'
    EQU CURR  TO 'CURR'
    
    EQU localZone TO 'America/El_Salvador'

    RETURN

OPENFILES:
;* Variable donde se recuperar� la informaci�n de la tabla
    CALL OPF(FN.TABLE, F.TABLE)
    CALL OPF(FN.TXN.TYPE, F.TXN.TYPE)
    CALL OPF(FN.STMT.ENTRY, F.STMT.ENTRY)
    CALL OPF(FN.TRANS, F.TRANS)
    CALL OPF(FN.FT$HIS,F.FT$HIS)
    CALL OPF(FN.TT,F.TT)
    CALL OPF(FN.TT$HIS,F.TT$HIS)
    CALL OPF(FN.TT.TYPE,F.TT.TYPE)
    CALL OPF(FN.AA.ACT,F.AA.ACT)
    CALL OPF(FN.TFS.ACT, F.TFS.ACT)
    CALL OPF(FN.TFS.TYPE,F.TFS.TYPE)
    CALL OPF(FN.TFS.ACT$HIS, F.TFS.ACT$HIS)
    CALL OPF(FN.CHG, F.CHG)
    CALL OPF(FN.CHG$HIS, F.CHG$HIS)
    CALL OPF(FN.CHG.TYPE, F.CHG.TYPE)
    CALL OPF(FN.CURR, F.CURR)
    CALL OPF(FN.CURR$HIS, F.CURR$HIS)
    CALL OPF(FN.EB.SLV.GLOBAL.PARAM,F.EB.SLV.GLOBAL.PARAM)
     
	
*7851 - Start	
    CALL OPF(FN.FUNDS.TRANSFER.NAU,F.FUNDS.TRANSFER.NAU) 
*7851 - End

*6745 - Start
	CALL OPF(FN.ACC,F.ACC)
	CALL OPF(FN.CUS,F.CUS)
*6745 - End
	
	CALL OPF(FN.COLECTOR,F.COLECTOR) 
	
	CALL OPF(FN.EB.SLV.COL.FRONT.END,F.EB.SLV.COL.FRONT.END)
	
    RETURN

PROCESS:
*-----------------------------------------------------------------------------
*------DEBUG------------------------------------------------------------------

*    S.APP.NAME = "STMT.ENTRY"
*
*    S.APP.NAME = 'FUNDS.TRANSFER'
*
*    S.APP.NAME = 'TELLER'
*	 S.APP.NAME = "AC.CHARGE.REQUEST"
*    S.APP.NAME = 'TELLER.FINANCIAL.SERVICES' 
*    S.APP.NAME = "CHEQUE.ISSUE"
*
*	 S.APP.NAME = "AA.ARRANGEMENT.ACTIVITY"
*    S.APP.ID = "CURR.10000000000203.0000001"
*	 S.APP.ID= '173669832104657.010001'	;* S.APP.ID= '170736275867828.010001'
*

*	 S.APP.ID = 'FT152120WPPP' 
*	 S.APP.ID = 'FT15221PF6S1' 
*	 S.APP.ID = 'FT15221F047L' ;* MH
*	 S.APP.ID = 'FT15221VCLPN' ;* MH

*    S.APP.ID = 'FT15207ZZ88S' ;* 'FT15212Q8S4L'  ;*  'FT152120QYH4 ' ;* 'FT162503NVPK' ;* 'FT162823FWHK ' ;* "FT162503NVPK" ;*'FT15212S41P0' ;*'FT15207WH63P' ;* 'FT15207ZZ88S' ;* "FT15206035TQ" ;* 
*	 S.APP.ID =  'AAACT16183HWJXBJ54' ;* 'AAACT153054F71FNLR' ;* 'AAACT15213P9HZRB4L' 'AAACT161834VVKGRTL' ;*
*    S.APP.ID =  'TT152123F345' ;* 'TT16250LKNFL' ;*'TT15212DPN3M' ;* 'TT15212N2NHN' ;*'TT152075HWWH' ;*'TT15207VMV2C'
*	 S.APP.ID = 'CHG1609086570'
*
*	 S.APP.ID =  'AAACT161522G7XVBYV' ;*'AAACT16092ZR7L0Y04' ;* 'AAACT16122TVRR6B6T' ;*'AAACT16152H1ZDQ0PP' ;*
*    S.APP.ID = 'TFS15207JWL92' ;* 'TFS1625091RZL' ;* "TFS15206RM777" ;*"TFS15205MG7GG" ;* 'TFS16154BFQBN' 'TFS15207Y0Q1H' ;* "TFS152050CTRR" ;*
*	 S.APP.ID = 'FT15213X30MV'

*-----------------------------------------------------------------------------

    LOCATE 'APPLICATION' IN D.FIELDS<1> SETTING FLD.POS THEN  ;* es para obtener los parametros de invocacion
    	S.APP.NAME = D.RANGE.AND.VALUE<FLD.POS>
    END

    LOCATE '@ID' IN D.FIELDS<1> SETTING FT.POS THEN
    	S.APP.ID = D.RANGE.AND.VALUE<FT.POS>
    END

;* QUITANDO CARACTERES DEL ID
    S.APP.ID = EREPLACE(S.APP.ID, '/','')
    S.APP.ID = FIELD(S.APP.ID, ' ', 1)

;* REFERENCIA
    Y.VOU.REF = SWAP(S.APP.ID,';','') ;* Cambio de Caracteres

    BEGIN CASE
        CASE S.APP.NAME = 'FUNDS.TRANSFER'
            GOSUB PROCESS_FT
			BREAK
			
        CASE S.APP.NAME = 'STMT.ENTRY'
            GOSUB PROCESS_ST
            BREAK
            
        CASE S.APP.NAME = 'TELLER.FINANCIAL.SERVICES'
            GOSUB PROCESS_TFS
			BREAK

        CASE S.APP.NAME = 'TELLER'
            GOSUB PROCESS_TT
			BREAK

        CASE S.APP.NAME = 'AC.CHARGE.REQUEST'
            GOSUB PROCESS_CHG
			BREAK

        CASE S.APP.NAME = 'CHEQUE.ISSUE'
            GOSUB PROCESS_CURR
			BREAK

        CASE S.APP.NAME = 'AA.ARRANGEMENT.ACTIVITY' ;* 'F.AA.ARRANGEMENT.ACTIVITY'  ;* RGaray - 20151119 
            GOSUB PROCESS_AA
			BREAK

        CASE 1
            CRT 'ERROR, PROCESO NO DEFINIDO PARA: ' : S.APP.NAME
    END CASE


    RETURN

PROCESS_FT:
	;* S.APP.ID = FIELD(S.APP.ID,';',1)
    CALL F.READ(FN.TABLE, S.APP.ID, A.RECORD, F.TABLE, ERR5)
    IF ERR5 NE '' THEN
*7851 - Start	
		Y.FT.NAU.ERR = ""
		CALL F.READ(FN.FUNDS.TRANSFER.NAU,S.APP.ID,A.RECORD,F.FUNDS.TRANSFER.NAU,Y.FT.NAU.ERR)
		IF Y.FT.NAU.ERR THEN	
*7851 - End		
			CALL F.READ.HISTORY(FN.FT$HIS, S.APP.ID, A.RECORD, F.FT$HIS, ERR.HIS)
			CALL F.READ(FN.TXN.TYPE, A.RECORD<FT.TRANSACTION.TYPE>, A.TXN.TYPE, F.TXN.TYPE, ERR6)
		END
		ELSE
			CALL F.READ(FN.TXN.TYPE, A.RECORD<FT.TRANSACTION.TYPE>, A.TXN.TYPE, F.TXN.TYPE, ERR6)
		END
;**7851 - Start/End	
    END
    ELSE
    	CALL F.READ(FN.TXN.TYPE, A.RECORD<FT.TRANSACTION.TYPE>, A.TXN.TYPE, F.TXN.TYPE, ERR6)
    END
    
    ;* RGARAY 20161123 - obtener el campo local de MH para 
    CALL GET.LOC.REF ('FUNDS.TRANSFER', 'LF.ID.COL.MOD', ID.COL.MOD.POS)
    LF.ID.COL.MOD = A.RECORD<FT.LOCAL.REF, ID.COL.MOD.POS> 
    
* ***************************
* Componer ENQUIRY resultante
* ***************************

	Y.TRAN.TYP = A.RECORD<FT.TRANSACTION.TYPE>

;* TITULO 
		;* Cuando el tipo de transacci�n es >> Cheque Certificado ACCC
	IF Y.TRAN.TYP EQ 'ACCC' THEN
		CALL F.READ(FN.EB.SLV.GLOBAL.PARAM, 'SLV.TCIB.E.NOF.VOUCHER.FT.ACCC', A.PARAM, F.EB.SLV.GLOBAL.PARAM, ERR7)
    	Y.VOU.TIT = "COMPROBANTE DE " : UPCASE(A.PARAM<EB.SLV39.VALOR.PARAM>)
    END ELSE	
    	;* Si tiene descripcion ES
    	IF DCOUNT(A.TXN.TYPE<FT6.DESCRIPTION>, VM) GT 1 THEN
    		Y.VOU.TIT = "COMPROBANTE DE " : UPCASE(A.TXN.TYPE<FT6.DESCRIPTION><1,2>) : " - " : UPCASE(A.RECORD<FT.PAYMENT.DETAILS><1,1>)
    	END ELSE
    		Y.VOU.TIT = "COMPROBANTE DE " : UPCASE(A.TXN.TYPE<FT6.DESCRIPTION>) : " - " : UPCASE(A.RECORD<FT.PAYMENT.DETAILS><1,1>)
    	END
    	;* cambio de titulo para AC64 de colectores
    	IF Y.TRAN.TYP EQ 'AC64' THEN 
    		;* RGARAY 20161123 - verificar si se trata de PEX o MH
    		IF LF.ID.COL.MOD EQ '' THEN 
    			;* RGARAY 20161123 - pago PEX 
    			Y.VOU.TIT = "COMPROBANTE DE " : SWAP(UPCASE(A.TXN.TYPE<FT6.DESCRIPTION>), @VM, ' ') 
    		END
    		IF LF.ID.COL.MOD NE '' THEN 
    			;* RGARAY 20161123 - pago MH 
    			Y.VOU.TIT = "COMPROBANTE DE PAGO DE IMPUESTOS POR INTERNET "  
    		END
    	END 
    END
    
;* FECHA
    Y.VOU.FEC = A.RECORD<FT.DATE.TIME>
	GOSUB PARSE.DATETIME

   
   
;* CTA DEBITADA (CARGADA)

	;* Cuando el tipo de transacci�n es: Abono de Intereses la Cta. >> ACIM
    IF Y.TRAN.TYP EQ 'ACIM' OR A.RECORD<FT.DEBIT.ACCT.NO>[1,3] EQ 'USD' THEN
        Y.VOU.DEB = ''
    END
    ELSE
		;* 6745 - Start
	    CALL F.READ(FN.ACC, A.RECORD<FT.DEBIT.ACCT.NO>, A.ACC, F.ACC, E.ACC)
	    Y.VOU.DEB = A.RECORD<FT.DEBIT.ACCT.NO> : ' - ' : UPCASE(A.ACC<AC.ACCOUNT.TITLE.1>) 
	    ;* 6745 - Stop    	
    END

;* MONTO 
    Y.VOU.MONTO = A.RECORD<FT.DEBIT.AMOUNT>
    IF Y.VOU.MONTO EQ '' THEN
        Y.VOU.MONTO = A.RECORD<FT.CREDIT.AMOUNT>
    END
			;* Cuando el tipo de transacci�n es >> Cheque Certificado ACCC
    IF Y.TRAN.TYP EQ 'ACCC' THEN
        Y.VOU.MONTO = Y.VOU.MONTO + A.RECORD<FT.TOT.SND.CHG.CRCCY>
    END

;* CTA ACREDITADA (ABONADA)

	;* Cuando el tipo de transacci�n es >> Cheque Certificado ACCC
    IF Y.TRAN.TYP EQ 'ACCC' OR A.RECORD<FT.CREDIT.ACCT.NO>[1,3] EQ 'USD' THEN
        Y.VOU.ACR = ''
    END 
    ELSE
    	;* 6745 - Start
    	CALL F.READ(FN.ACC, A.RECORD<FT.CREDIT.ACCT.NO>, A.ACC.CRE, F.ACC, E.ACC.CRE)
    	Y.VOU.ACR = A.RECORD<FT.CREDIT.ACCT.NO> : ' - ' : UPCASE(A.ACC.CRE<AC.ACCOUNT.TITLE.1>)
    	;* 6745 - Stop 
    END
    
    ;* RGARAY 20161123 - obtener datos de pago desde App local 
    IF Y.TRAN.TYP EQ 'AC64' AND LF.ID.COL.MOD NE '' THEN 
    	GOSUB RECIBO.MH
    END
    ELSE
		    	;* construccion de registro 
		    GOSUB SETRECORD
		    
		    
		;* RGARAY - OBTENER CAMPOS PARA MOSTRAR LIOF - TRANSFERENCIA TERCEROS ACAT TCIB
		;*//Setear el titular para ACTP
			IF Y.TRAN.TYP EQ 'ACPT' THEN
					;* TITULAR DE CUENTA DEL BENEFICIARIO
					;* 6745 - Start [Titular de la Cta a Abonar]
					CALL F.READ(FN.CUS, A.RECORD<FT.CHARGED.CUSTOMER>, A.CUS, F.CUS, E.CUS)
					Y.VOU.BEN = UPCASE(A.CUS<EB.CUS.SHORT.NAME>)
					Y.VOU.BEN = SWAP(Y.VOU.BEN, @VM, '')
					
					;* 6745 - Stop 
					GOSUB SETRECORD.TITULAR
			END
			IF Y.TRAN.TYP EQ 'ACAT' THEN
			
				;* TITULAR DE CUENTA DEL BENEFICIARIO
					;* 6745 - Start [Titular de la Cta a Abonar]
					CALL F.READ(FN.CUS, A.RECORD<FT.CHARGED.CUSTOMER>, A.CUS, F.CUS, E.CUS)
					Y.VOU.BEN = UPCASE(A.CUS<EB.CUS.SHORT.NAME>)
					Y.VOU.BEN = SWAP(Y.VOU.BEN, @VM, '')
					;* 6745 - Stop 
			
				;* IMPUESTO A PAGAR
					Y.VOU.IMP = A.RECORD<FT.LOC.TOT.TAX.AMT> 
					IF Y.VOU.IMP EQ '' THEN
						Y.VOU.IMP = '0.00'
					END
				;* FBATRES	
				;* MONTO TOTAL DE CARGO
					Y.VOU.CARGO.TOT = A.RECORD<FT.LOC.AMT.DEBITED> + A.RECORD<FT.LOC.TOT.TAX.AMT>
					Y.VOU.CARGO.TOT = TRIM(FMT(Y.VOU.CARGO.TOT, 'R2#19'), '', 'D')
					
					GOSUB SETRECORD.TITULAR
					GOSUB SETRECORD.ACAT
					
			END
			
			;* mostrar datos adicionales para colectores 
			IF Y.TRAN.TYP EQ 'AC64' THEN 
				CALL GET.LOC.REF ('FUNDS.TRANSFER', 'LF.RS.PX', PX.POS)
				LF.MULTI.DESC = A.RECORD<FT.LOCAL.REF, PX.POS> 
				LF.ID.COL = FIELD(LF.MULTI.DESC, 'collectorId:', 2) 
				LF.ID.COL = FIELD(LF.ID.COL, @SM, 1) 
				
				REF.COL = FIELD(LF.MULTI.DESC, 'collectorRef:', 2) 
				REF.COL = FIELD(REF.COL, @SM, 1) 
				
				CALL F.READ(FN.COLECTOR, LF.ID.COL, R.COLECTOR, F.COLECTOR, F.ERR) 
				NOMBRE.COLECTOR = R.COLECTOR<EB.CL.NOMBRE.COLECTOR> 
						
				CALL GET.LOC.REF ('FUNDS.TRANSFER', 'LF.NUM.NPE', NPE.POS)
				NPE = A.RECORD<FT.LOCAL.REF, NPE.POS> 
				NPE = FIELD(NPE, '-', 1)
				
				GOSUB SETRECORD.COLECTORES 
				
			END 
			
			IF Y.TRAN.TYP EQ 'ACAT' OR Y.TRAN.TYP EQ 'ACIB' OR Y.TRAN.TYP EQ 'ACPE' OR Y.TRAN.TYP EQ 'ACRP' OR Y.TRAN.TYP EQ 'ACPT' OR Y.TRAN.TYP EQ 'AC64' THEN 
				Y.VOU.DATE.SYS = OCONV(ICONV(A.RECORD<FT.PROCESSING.DATE>,'D'),'D4/E')
				GOSUB SETRECORD.DATE.SYS	
			END
			
			GOSUB SETRECORD.END
    END
    
    

RETURN

PROCESS_ST:
    Y.VOU.FEC 	= ''
    Y.VOU.DEB 	= ''
    Y.VOU.MONTO = ''
    Y.VOU.ACR 	= ''

    CALL F.READ(FN.STMT.ENTRY, S.APP.ID, A.RECORD, F.STMT.ENTRY, ERR0)
    SYSTEM.ID = A.RECORD<AC.STE.SYSTEM.ID>
    TRANS.REF = A.RECORD<AC.STE.TRANS.REFERENCE>

    BEGIN CASE
            ;* En el caso que sea un TT
        CASE SYSTEM.ID EQ TT
            CALL F.READ(FN.TT,TRANS.REF,R.TT,F.TT,ERR.TT)
            IF ERR.TT NE '' THEN
                CALL F.READ.HISTORY(FN.TT$HIS,TRANS.REF,R.TT,F.TT$HIS,ERR.TT)
            END
            Y.VOU.FEC 	= R.TT<TT.TE.DATE.TIME>
            GOSUB PARSE.DATETIME
            Y.VOU.DEB 	= R.TT<TT.TE.ACCOUNT.1>
            Y.VOU.MONTO = R.TT<TT.TE.AMOUNT.LOCAL.1>
            IF Y.VOU.MONTO EQ '' THEN
                Y.VOU.MONTO = R.TT<TT.TE.AMOUNT.LOCAL.2>
            END
            Y.VOU.ACR 	= R.TT<TT.TE.ACCOUNT.2>

            ;* En el caso que sea un AA Activity
        CASE SYSTEM.ID EQ AA
            CALL F.READ(FN.AA.ACT,TRANS.REF,R.AA.ACT,F.AA.ACT,ERR.AA.ACT)
            Y.VOU.FEC 	= R.AA.ACT<AA.ARR.ACT.DATE.TIME>
            GOSUB PARSE.DATETIME
            Y.VOU.DEB 	= A.RECORD<AC.STE.ACCOUNT.NUMBER>
            Y.VOU.MONTO = R.AA.ACT<AA.ARR.ACT.TXN.AMOUNT>
            Y.VOU.ACR 	= A.RECORD<AC.STE.ACCOUNT.NUMBER>

            ;* En el caso que sea un FT
        CASE SYSTEM.ID EQ FT
            CALL F.READ(FN.TABLE,TRANS.REF,R.TABLE,F.TABLE,ERR.TABLE)
            IF ERR.TABLE THEN
                CALL F.READ.HISTORY(FN.FT$HIS, TRANS.REF, R.TABLE, F.FT$HIS, ERR.HIS)
            END
            Y.VOU.FEC 	= R.TABLE<FT.DATE.TIME>
            GOSUB PARSE.DATETIME
            Y.VOU.DEB 	= R.TABLE<FT.DEBIT.ACCT.NO>
            Y.VOU.MONTO = R.TABLE<FT.DEBIT.AMOUNT>
            
            IF Y.VOU.MONTO EQ '' THEN
                Y.VOU.MONTO = R.TABLE<FT.CREDIT.AMOUNT>
            END
            Y.VOU.ACR 	= R.TABLE<FT.CREDIT.ACCT.NO>

    END CASE

* ***************************
* Componer ENQUIRY resultante
* ***************************
    
    CALL F.READ(FN.TRANS, A.RECORD<AC.STE.TRANSACTION.CODE>, A.TRANS, F.TRANS, ERR1)
    Y.VOU.TIT = "COMPROBANTE DE ":UPCASE(A.TRANS<AC.TRA.NARRATIVE>)
    Y.VOU.REF = TRANS.REF
	GOSUB SETRECORD
	GOSUB SETRECORD.END

RETURN


PROCESS_TFS:
    CALL F.READ(FN.TFS.ACT, S.APP.ID, A.RECORD, F.TFS.ACT, ERR7)
    IF ERR7 NE '' THEN
        CALL F.READ.HISTORY(FN.TFS.ACT$HIS, S.APP.ID, A.RECORD, F.TFS.ACT$HIS, ERR.HIS)
    END
    
* ***************************
* Componer ENQUIRY resultante
* ***************************

;* TITULO
	Y.VOU.TIT = A.RECORD<TFS.TRANSACTION>
	GOSUB TFS.TR.TIPO
	IF Y.BOOL.DEB EQ 'T' THEN  
		CALL F.READ(FN.EB.SLV.GLOBAL.PARAM, 'SLV.TCIB.E.NOF.VOUCHER.TFS.C', A.PARAM, F.EB.SLV.GLOBAL.PARAM, ERR7)
		Y.VOU.TIT = "COMPROBANTE DE " : UPCASE(A.PARAM<EB.SLV39.VALOR.PARAM>)
	END ELSE
		CALL F.READ(FN.TFS.TYPE, Y.VOU.TIT, A.TFS.TYPE, F.TFS.TYPE, ERR7)
		Y.VOU.TIT = "COMPROBANTE DE " : UPCASE(A.TFS.TYPE<TFS.TXN.DESCRIPTION>)	
	END
    
;* FECHA
    Y.VOU.FEC = A.RECORD<TFS.DATE.TIME>
    GOSUB PARSE.DATETIME
	
;* CTA DEBITADA 	
	Y.VOU.DEB = A.RECORD<TFS.ACCOUNT.DR><1,1>
	IF Y.VOU.DEB[1,3] EQ 'USD' THEN
    	Y.VOU.DEB = ''
   	END

;* MONTO 			
	Y.VOU.MONTO = A.RECORD<TFS.AMOUNT.DR>
    IF Y.VOU.MONTO  EQ '' THEN
	    Y.VOU.MONTO  = A.RECORD<TFS.AMOUNT.CR>
    END
    GOSUB TFS.MULTIVALOR.MONTO

;* CTA ACREDITADA 	
	Y.VOU.ACR = A.RECORD<TFS.ACCOUNT.CR><1,1>
	IF Y.VOU.ACR[1,3] EQ 'USD' THEN
        Y.VOU.ACR = ''
    END
  
	GOSUB SETRECORD
	GOSUB SETRECORD.END

RETURN

PROCESS_TT:
    CALL F.READ(FN.TT, S.APP.ID, A.RECORD, F.TT, ERR8)
    IF ERR8 NE '' THEN
        CALL F.READ.HISTORY(FN.TT$HIS, S.APP.ID, A.RECORD, F.TT$HIS, ERR.HIS)
        CALL F.READ(FN.TT.TYPE, A.RECORD<TT.TE.TRANSACTION.CODE>, A.TT.TYPE, F.TT.TYPE, ERR9)
    END
    ELSE
    	CALL F.READ(FN.TT.TYPE, A.RECORD<TT.TE.TRANSACTION.CODE>, A.TT.TYPE, F.TT.TYPE, ERR9)
    END
    DR.CR = A.RECORD<TT.TE.DR.CR.MARKER>

* ***************************
* Componer ENQUIRY resultante
* ***************************

;* TITULO 			
	Y.VOU.TIT = "COMPROBANTE DE " : UPCASE(A.TT.TYPE<TT.TR.DESC>)

;* FECHA
	Y.VOU.FEC = A.RECORD<TT.TE.DATE.TIME>
    GOSUB PARSE.DATETIME

;* CTA DEBITADA 	
	Y.VOU.DEB = A.RECORD<TT.TE.ACCOUNT.2>
	;* Si es cta interna o esta siendo acreditada la cuenta del cliente
    IF Y.VOU.DEB[1,3] EQ 'USD' OR DR.CR EQ 'DEBIT' THEN
    	Y.VOU.DEB = ''
    END

;* MONTO 			
    Y.VOU.MONTO = A.RECORD<TT.TE.AMOUNT.LOCAL.1>
    IF Y.VOU.MONTO EQ '' THEN
	    Y.VOU.MONTO = A.RECORD<TT.TE.AMOUNT.LOCAL.2>
    END
    	;* Si la TT es una Emisi�n de Cheque de Caja se despliega el valor + la comisi�n
    IF A.RECORD<TT.TE.TRANSACTION.CODE> EQ 77 THEN
		Y.VOU.MONTO = A.RECORD<TT.TE.NET.AMOUNT> 
    END
       
;* CTA ACREDITADA 	
  	Y.VOU.ACR = A.RECORD<TT.TE.ACCOUNT.2>
	;* Si es cta interna o esta siendo debitada la cuenta del cliente
    IF Y.VOU.ACR[1,3] EQ 'USD' OR DR.CR EQ 'CREDIT' THEN
    	Y.VOU.ACR = ''
    END 

	GOSUB SETRECORD
	GOSUB SETRECORD.END

RETURN

PROCESS_CHG:
    CALL F.READ(FN.CHG, S.APP.ID, A.RECORD, F.CHG, ERR10)
    IF ERR10 NE '' THEN
        CALL F.READ.HISTORY(FN.CHG$HIS, S.APP.ID, A.RECORD, F.CHG$HIS, ERR.HIS)
        CALL F.READ(FN.CHG.TYPE, A.RECORD<CHG.CHARGE.CODE>, A.CHG.TYPE, F.CHG.TYPE, ERR11)
    END
    ELSE
    	CALL F.READ(FN.CHG.TYPE, A.RECORD<CHG.CHARGE.CODE>, A.CHG.TYPE, F.CHG.TYPE, ERR12)
    END

* ***************************
* Componer ENQUIRY resultante
* ***************************

;* TITULO 			
	Y.VOU.TIT = "COMPROBANTE DE " : UPCASE(A.CHG.TYPE<FT4.DESCRIPTION>)

;* FECHA			
	Y.VOU.FEC = A.RECORD<CHG.DATE.TIME>
    GOSUB PARSE.DATETIME

;* CTA DEBITADA 	
	Y.VOU.DEB = A.RECORD<CHG.DEBIT.ACCOUNT>
    IF Y.VOU.DEB[1,3] EQ 'USD' THEN
    	Y.VOU.DEB = ''
    END

;* MONTO 			
	Y.VOU.MONTO = A.RECORD<CHG.TOTAL.CHG.AMT>

;* CTA ACREDITADA 
	Y.VOU.ACR = '' ;*A.RECORD<TT.TE.ACCOUNT.2>

	GOSUB SETRECORD
	GOSUB SETRECORD.END
	
RETURN

PROCESS_CURR:
    POS_COD = ''; POS_TAX = ''; POS_AMO= ''

    CALL GET.LOC.REF(S.APP.NAME, 'LF.CHG.CODE', POS_COD)
    CALL GET.LOC.REF(S.APP.NAME, 'LF.CHG.TAX.AMT', POS_TAX)
    CALL GET.LOC.REF(S.APP.NAME, 'LF.CHG.AMOUNT', POS_AMO)

    CALL F.READ(FN.CURR, S.APP.ID, A.RECORD, F.CURR, ERR13)
    IF ERR12 NE '' THEN
        CALL F.READ.HISTORY(FN.CURR$HIS, S.APP.ID, A.RECORD, F.CURR$HIS, ERR.HIS)
        CALL F.READ(FN.CHG.TYPE, A.RECORD<CHEQUE.IS.LOCAL.REF, POS_COD>, A.CURR.TYPE, F.CHG.TYPE, ERR14) ;* FT.COMMISSION.TYPE
    END
    ELSE
    	CALL F.READ(FN.CHG.TYPE, A.RECORD<CHEQUE.IS.LOCAL.REF, POS_COD>, A.CURR.TYPE, F.CHG.TYPE, ERR15) ;* FT.COMMISSION.TYPE
    END

* ***************************
* Componer ENQUIRY resultante
* ***************************

;* TITULO 			
	Y.VOU.TIT = "COMPROBANTE DE " : UPCASE(A.CURR.TYPE<FT4.DESCRIPTION>)

	;*Buscando el registros en el STMT.ENTRY para obtener la fecha & Modificando
    Y.ID.STMT = A.RECORD<CHEQUE.IS.STMT.NO><1,1> : '0001'
    CALL F.READ(FN.STMT.ENTRY, Y.ID.STMT, A.STMT.RECORD, F.STMT.ENTRY, ERR0)

;* FECHA			
	Y.VOU.FEC = A.STMT.RECORD<AC.STE.DATE.TIME>
    GOSUB PARSE.DATETIME

;* CTA DEBITADA 	
	Y.VOU.DEB = FIELD(S.APP.ID,'.',2)
	
;* MONTO 			
	Y.VOU.MONTO = A.RECORD<CHEQUE.IS.LOCAL.REF, POS_AMO> + A.RECORD<CHEQUE.IS.LOCAL.REF, POS_TAX>
    IF Y.VOU.MONTO EQ '' THEN
    	Y.VOU.MONTO =  A.RECORD<CHEQUE.IS.CHG.AMOUNT> + A.RECORD<CHEQUE.IS.TAX.CODE>
    END

;* CTA ACREDITADA 	
	Y.VOU.ACR = '' ;*A.RECORD<TT.TE.ACCOUNT.2>

	GOSUB SETRECORD
	GOSUB SETRECORD.END
	
RETURN

PROCESS_AA:

    CALL F.READ(FN.AA.ACT, S.APP.ID, A.RECORD, F.AA.ACT, ERR13)
    CALL F.READ(FN.AA.TYPE, A.RECORD<AA.ARR.ACT.ACTIVITY>, A.AA.TYPE, F.AA.TYPE, ERR11)
	    Y.ID.STMT = A.RECORD<AA.ARR.ACT.STMT.NOS><1,1> : '0001'
	    CALL F.READ(FN.STMT.ENTRY, Y.ID.STMT, A.STMT.RECORD, F.STMT.ENTRY, ERR23)
	
	;* Obteniendo el valor del campo local "DEBIT.CREDIT"
	POS.SDC= ''
    CALL GET.LOC.REF('STMT.ENTRY', 'DEBIT.CREDIT', POS.SDC)
   	DEB.CRE = A.STMT.RECORD<AC.STE.LOCAL.REF,POS.SDC>
	
	
* ***************************
* Componer ENQUIRY resultante
* ***************************

;* TITULO 
	;* obteniendo la descpripici�n ES
	IF DCOUNT(A.AA.TYPE<AA.ACT.DESCRIPTION>,VM)	GT 1 THEN
		Y.VOU.TIT = "COMPROBANTE DE " : UPCASE(A.AA.TYPE<AA.ACT.DESCRIPTION><1,2>)
	END ELSE 
		Y.VOU.TIT = "COMPROBANTE DE " : UPCASE(A.AA.TYPE<AA.ACT.DESCRIPTION>)
	END		

;* FECHA			
	Y.VOU.FEC = A.RECORD<AA.ARR.ACT.DATE.TIME>
    GOSUB PARSE.DATETIME

;* CTA DEBITADA 	
	IF DEB.CRE EQ 'D' THEN
        Y.VOU.DEB = A.STMT.RECORD<AC.STE.ACCOUNT.NUMBER>
        IF Y.VOU.DEB[1,3] EQ 'USD' THEN
    		Y.VOU.DEB = ''
    	END
    END
    ELSE 
    	Y.VOU.DEB = ''
    END

;* MONTO 			
	Y.VOU.MONTO = A.RECORD<AA.ARR.ACT.TXN.AMOUNT>
    IF Y.VOU.MONTO EQ '' THEN
	    Y.VOU.MONTO = A.STMT.RECORD<AC.STE.AMOUNT.LCY>
    END

;* CTA ACREDITADA 	>>  Y.VOU.ACR
    IF DEB.CRE EQ 'C' THEN
        Y.VOU.ACR = A.STMT.RECORD<AC.STE.ACCOUNT.NUMBER>
        IF Y.VOU.ACR[1,3] EQ 'USD' THEN
    		Y.VOU.ACR = ''
    	END
    END
    ELSE
    	 Y.VOU.ACR = ''
    END	

	GOSUB SETRECORD
	GOSUB SETRECORD.END

RETURN


SETRECORD:
;* la condicion IF es nueva para mostrar voucher de colectores sin cuenta a abonar 
IF Y.TRAN.TYP EQ 'AC64' THEN  
	;* Version colectores
	Y.VOU.IDEN 	= 'TITLE' 	: @VM : 'FT.ID' 	 : @VM : 'DEBIT.ACCT.NO' 	: @VM : 'AMOUNT' 	
	Y.VOU.LABEL = 'TITLE' 	: @VM : 'Referencia' : @VM : 'Cuenta a Cargar'  : @VM : 'Monto' 	
	Y.VOY.VALUE = Y.VOU.TIT : @VM : Y.VOU.REF 	 : @VM : Y.VOU.DEB 			: @VM : Y.VOU.MONTO 		 
END 
ELSE
	;* version original 
	Y.VOU.IDEN 	= 'TITLE' 	: @VM : 'FT.ID' 	 : @VM : 'DEBIT.ACCT.NO' 	: @VM : 'AMOUNT' 	: @VM : 'CREDIT.ACCT.NO'
	Y.VOU.LABEL = 'TITLE' 	: @VM : 'Referencia' : @VM : 'Cuenta a Cargar'  : @VM : 'Monto' 	: @VM : 'Cuenta a Abonar' 
	Y.VOY.VALUE = Y.VOU.TIT : @VM : Y.VOU.REF 	 : @VM : Y.VOU.DEB 			: @VM : Y.VOU.MONTO : @VM : Y.VOU.ACR		 
END
RETURN

;* adicionado los campos LIOF en Y.VOU.IDEN (nombre campo), Y.VOU.LABEL (titulo), Y.VOY.VALUE (valor)  
;* 6745 Adicion del titular de la cuenta del beneficiario
;* 6745 Se separo el titular para poder cumplir con observaciones
SETRECORD.ACAT:
	IF Y.VOU.IMP NE '' THEN
		Y.VOU.IDEN 	= Y.VOU.IDEN	: @VM : 'LOC.TOT.TAX.AMT' 		: @VM : 'LOC.AMT.DEBITED'
		Y.VOU.LABEL = Y.VOU.LABEL	: @VM : 'Impuesto LIOF a pagar'	: @VM : 'Monto Total de Cargo' 
		Y.VOY.VALUE = Y.VOY.VALUE	: @VM : Y.VOU.IMP		 		: @VM : Y.VOU.CARGO.TOT 	
	END
RETURN

;* subfuncion para preparar valores a mostrar en colectores para txn AC64 
SETRECORD.COLECTORES:
	Y.VOU.IDEN 	= Y.VOU.IDEN	: @VM :	'NOMBRE.COLECTOR'	: @VM :	'NPE'	: @VM :	'REF.COL'
	Y.VOU.LABEL = Y.VOU.LABEL	: @VM : 'Servicio Pagado' 	: @VM : 'NPE'	: @VM : 'Referencia Servicio'
	Y.VOY.VALUE = Y.VOY.VALUE	: @VM : NOMBRE.COLECTOR 	: @VM : NPE		: @VM :	REF.COL
RETURN 
;*

SETRECORD.TITULAR:
		Y.VOU.IDEN 	= Y.VOU.IDEN	: @VM :	'TIT.CTA.CRE'	
		Y.VOU.LABEL = Y.VOU.LABEL	: @VM : 'Titular cuenta de abono' 
		Y.VOY.VALUE = Y.VOY.VALUE	: @VM : Y.VOU.BEN
RETURN 

;* 6745 - Start [Adici�n de la fecha del sistema]
SETRECORD.DATE.SYS:
		Y.VOU.IDEN 	= Y.VOU.IDEN	: @VM :	'DATE.SYS' 				
		Y.VOU.LABEL = Y.VOU.LABEL	: @VM : 'Fecha de sistema' 
		Y.VOY.VALUE = Y.VOY.VALUE	: @VM : Y.VOU.DATE.SYS	
RETURN
;* 6745 - End

SETRECORD.END:
	Y.VOU.IDEN 	= Y.VOU.IDEN 	: @VM : 'AUTH.DATE' 				
	Y.VOU.LABEL = Y.VOU.LABEL 	: @VM : 'Fecha y Hora de Creacion'
	Y.VOY.VALUE = Y.VOY.VALUE 	: @VM : Y.VOU.FEC 	
	
	MULTI.ACT.CONT = DCOUNT(Y.VOU.IDEN, @VM)
    FOR I = 1 TO MULTI.ACT.CONT 
           LS.RL.ACTIVIDAD.ECONOMICA = LS.RL.ACTIVIDAD.ECONOMICA : "" : FIELD(MULTI.ACT.ECON, @SM, I)
    	OUT.ARRAY<-1> = I : "*" : FIELD(Y.VOU.IDEN , @VM, I) : "*" : FIELD(Y.VOU.LABEL , @VM, I) : "*" : FIELD(Y.VOY.VALUE , @VM, I)
    NEXT I 
RETURN 


TFS.TR.TIPO:
	LOOP 
		REMOVE VAL.ID FROM Y.VOU.TIT SETTING POS.VAL
		WHILE VAL.ID
			IF VAL.ID EQ 'AcredChAjeno' OR VAL.ID EQ 'AcredChCert' OR VAL.ID EQ 'DepChPropio' OR VAL.ID EQ 'DepEfectivo' OR VAL.ID EQ 'xConsol-Charge' OR VAL.ID EQ 'xConsol-Leg' THEN
				 Y.BOOL.DEB = 'T'
			END
		REPEAT
RETURN


TFS.MULTIVALOR.MONTO:
    Y.AMT = ''
    LOOP
        REMOVE VAL.ID FROM Y.VOU.MONTO SETTING POS.VAL
    WHILE VAL.ID
        Y.AMT = Y.AMT + VAL.ID
    REPEAT
    Y.VOU.MONTO = Y.AMT

RETURN

PARSE.DATETIME:
		utcDateTime =  Y.VOU.FEC
		UTC.FLAG = ''
		;*Evaluar UTC Time or Standard Time
		
		FINDSTR "." IN utcDateTime SETTING Ap, Vp THEN
			UTC.FLAG = '1'
		END
		
		IF UTC.FLAG EQ '1' THEN
			localZoneDate1 = OCONV(LOCALDATE(utcDateTime,localZone),'D4/E')
			localZoneTime1= OCONV(LOCALTIME(utcDateTime,localZone),'MTS')
			Y.VOU.FEC = localZoneDate1:' ':localZoneTime1
		END
		ELSE
			Y.DAY.BC = utcDateTime[3,2]
			Y.MONTH.BC = utcDateTime[5,2]
			Y.YEAR.BC = utcDateTime[1,2]
			Y.DATE.BC = OCONV(ICONV(utcDateTime[1,6],'D'),'D4/E')
			;*Y.DATE.BCs = OCONV(ICONV(utcDateTime,'D'),'D4/E')
*			Y.DATE.BC = Y.DAY.BC:'/':Y.MONTH.BC:'/20':Y.YEAR.BC
			Y.TIME.BC = utcDateTime[7,2]:':':utcDateTime[9,2]:':':'00'
			Y.VOU.FEC = Y.DATE.BC: ' ': Y.TIME.BC
		END
RETURN

RECIBO.MH:
	
	Y.VOU.IDEN 	= 'TITLE' 		
	Y.VOU.LABEL = 'TITLE' 		
	Y.VOY.VALUE = Y.VOU.TIT 		 
	
	;* colocar otro titulo intermedio
		Y.VOU.IDEN 	= Y.VOU.IDEN	: @VM : 'SUBTITULO.1' 			: @VM : 'BLK.SUB.1.2'
		Y.VOU.LABEL = Y.VOU.LABEL	: @VM : ' '						: @VM : ' '
		Y.VOY.VALUE = Y.VOY.VALUE	: @VM : 'DETALLE DE DOCUMENTO'	: @VM : ' '
	
	CALL F.READ(FN.EB.SLV.COL.FRONT.END, LF.ID.COL.MOD, VOU.MH, F.EB.SLV.COL.FRONT.END, ERRMH)
	;* RGARAY 20161124 - recuperando valores de MH y dando formato de voucher
	Y.MH.NUM.TES = VOU.MH<EB.SLV8.RESERVADO.18>
	
	Y.MH.TXN.GUB = VOU.MH<EB.SLV8.RESERVADO.6>
	
	Y.MH.COD.INST = VOU.MH<EB.SLV8.RESERVADO.2>
	Y.MH.NOM.INST = VOU.MH<EB.SLV8.RESERVADO.7>
	Y.MH.NIT.DOC = VOU.MH<EB.SLV8.RESERVADO.40>
	Y.MH.NUM.DOC = VOU.MH<EB.SLV8.RESERVADO.8>
	Y.MH.NUM.REF = VOU.MH<EB.SLV8.RESERVADO.12>
	
	Y.MH.TOT.PAGO = VOU.MH<EB.SLV8.RESERVADO.14>
	Y.MH.FEC.VENCI = VOU.MH<EB.SLV8.RESERVADO.11>
	
	Y.MH.FVENCI.FORMAT = SUBSTRINGS(Y.MH.FEC.VENCI,7,2) :'/': SUBSTRINGS(Y.MH.FEC.VENCI,5,2) :'/': SUBSTRINGS(Y.MH.FEC.VENCI,1,4)
	
	Y.VOU.IDEN 	= Y.VOU.IDEN	: @VM :	'MH.NUM.TES' 		: @VM : 'MH.TXN.GUB' 						: @VM :	'MH.CODIGO.INST'		: @VM :	'MH.NOM.INST'			: @VM :	'MH.NIT.DOC'	: @VM : 'MH.NUM.REF'		 
	Y.VOU.LABEL = Y.VOU.LABEL	: @VM : 'No. de Tesoreria' 	: @VM : 'No. de Transaccion Gubernamental' 	: @VM : 'Codigo Institucion' 	: @VM : 'Nombre Institucion'	: @VM : 'NIT'			: @VM : 'No de Referencia'	 
	Y.VOY.VALUE = Y.VOY.VALUE	: @VM : Y.MH.NUM.TES		: @VM : Y.MH.TXN.GUB 						: @VM : Y.MH.COD.INST 			: @VM : Y.MH.NOM.INST			: @VM :	Y.MH.NIT.DOC	: @VM : Y.MH.NUM.REF		 
	
	;* colocar otro titulo intermedio
		Y.VOU.IDEN 	= Y.VOU.IDEN	: @VM : 'BLK.SUB.2.1' 	: @VM : 'BLK.SUB.2.1.1' : @VM : 'BLK.SUB.2.1.2'	: @VM : 'SUBTITULO.2'		: @VM : 'BLK.SUB.2.2'
		Y.VOU.LABEL = Y.VOU.LABEL	: @VM : ' ' 			: @VM : ' '				: @VM : ' '				: @VM : ' '					: @VM : ' '
		Y.VOY.VALUE = Y.VOY.VALUE	: @VM : ' '				: @VM : ' '				: @VM : ' '				: @VM : 'DETALLE DE COBRO'	: @VM : ' '
	
	;*leer especificos y colocarlos 
	LF.MULTI.ESPEC.COD = VOU.MH<EB.SLV8.RESERVADO.35>
	LF.MULTI.ESPEC.NOM = VOU.MH<EB.SLV8.RESERVADO.36>
	LF.MULTI.ESPEC.VAL = VOU.MH<EB.SLV8.RESERVADO.37>
	
	MULTI.ESPEC.CONT = DCOUNT(LF.MULTI.ESPEC.COD, @VM) 
	;* nombre de columnas 
	Y.VOU.IDEN 	= Y.VOU.IDEN	: @VM : 'NOMB.COLUMNA'
	Y.VOU.LABEL = Y.VOU.LABEL	: @VM : 'Codigo Especifico'
	Y.VOY.VALUE = Y.VOY.VALUE	: @VM : 'Monto'
	
	FOR I = 1 TO MULTI.ESPEC.CONT 
		Y.LABEL.IDEN = 'MH.ESPEC.COD'
		MONTO.UNFORMAT = FIELD(LF.MULTI.ESPEC.VAL, @VM, I)
		Y.MONTO.FORMAT = '$':MONTO.UNFORMAT
		
		Y.VOU.IDEN 	= Y.VOU.IDEN	: @VM : Y.LABEL.IDEN
		Y.VOU.LABEL = Y.VOU.LABEL	: @VM : FIELD(LF.MULTI.ESPEC.COD, @VM, I)
		Y.VOY.VALUE = Y.VOY.VALUE	: @VM : Y.MONTO.FORMAT
		 
	NEXT I 
	
	Y.VOU.IDEN 	= Y.VOU.IDEN	: @VM : 'BLK.ESP.3.1'		: @VM : 'BLK.ESP.3.2'	: @VM : 'BLK.ESP.3.3'
	Y.VOU.LABEL = Y.VOU.LABEL	: @VM : ' '					: @VM : ' '				: @VM : ' '	
	Y.VOY.VALUE = Y.VOY.VALUE	: @VM : ' '					: @VM : ' '				: @VM : ' '
	
	;* adicionar la referencia, cuenta debito, monto y fecha 	
	Y.VOU.IDEN 	= Y.VOU.IDEN	: @VM :  'FT.ID' 	 : @VM : 'DEBIT.ACCT.NO' 	: @VM : 'AMOUNT' 	: @VM : 'FEC.VENCI'
	Y.VOU.LABEL = Y.VOU.LABEL	: @VM : 'Referencia' : @VM : 'Cuenta a Cargar'  : @VM : 'TOTAL PAGO' 	: @VM : 'Fecha Vencimiento' 
	Y.VOY.VALUE = Y.VOY.VALUE	: @VM : Y.VOU.REF 	 : @VM : Y.VOU.DEB 			: @VM : '$':Y.VOU.MONTO : @VM : Y.MH.FVENCI.FORMAT	
	GOSUB SETRECORD.END 
	
RETURN

 END