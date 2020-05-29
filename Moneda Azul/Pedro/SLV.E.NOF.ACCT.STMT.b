*------------------------------------------------------------------------------
* <Rating>12006</Rating>
*------------------------------------------------------------------------------ 
    SUBROUTINE SLV.E.NOF.ACCT.STMT(OUT.ARRAY)    
*------------------------------------------------------------------------------
* Modification History : 
* Modification Id          Date        Modified By              Description 
* 20150717             16 Jul 2015     Nishant Yadav         Replace the selection of the STMT.ENTRY with the core routine EB.ACCT.ENTRY.LIST
* 20150717             17 Jul 2015     Nishant Yadav         Display the Initial and end balance even if the there are no transaction
*                                                            happened into the given date range in selection Cretrion.
* 20150728P            28 Jul 2015     Prabha                Exclude the transaction with the BALANCE.TYPE EQ DUEISRTAXFEE
* 20150728N            28 Jul 2015     Nishant Yadav         Y.TAX.FLAG was initalised to display some AAA interest amount.
*                                                            Include the CHEQUE.NUMBER and Current Time in the the Final Array
* 20150805N            05 Aug 2015     Nishant               Modified the TFS array formation toSLV.E.NOF.ACCT.STMTMA accomodate the cheques and other balances.
* 20161108E			   08 Nov 2016 	   Ernesto 				 Modificaciones por LIOF nuevas trasnsacciones, definicion de tipo producto ref0001
* 20170104I		       04 Jan 2017	   iTurcios				 Se agregó logica para concatenar nuevo narrative para transacciones desde TCE
* 20172704M		       04 Jan 2017	   MAGARCIA				 Se unifican desarrollo entre eurias e iturcios
* 20170517R            17 May 2017     rortiz                Se agregan descripciones de todas las transacciones de colectores.
* 20172104             21 Abr 2017     JHENIQUEZ            Se agrego GoSub para modificar el Narrative de las aplicaciones AC.CHARGE.REQUEST por el momento solo
*                                                            se modifica los que tengan CHARGE.CODE = COMMER
* 20170904			   04 Sep 2017	   iTurcios				 Se añade logica para colocar narrative de txn Banca Empresas Bulk Payment.
* 20170904             11 Sep 2017     Jhenriquez            Se agrega logica para separar comision e iva para las aplicaciones AC.CHARGE.REQUEST GOSUB COMISION.IVA.CHQ.BREAK.OFF
* 20170911             11 Sep 2017     rortiz                Modificación para descripción de la transacción Abono por rentabilidad dejada de percibir AFP Crecer.
* 20170920             20 Sep 2017     Jhenriquez            Se agrega logica para ocultar efecto de credito/debito cuando se liberacion un cheque ajeno antes de tiempo.
* 20171220             20 Dic 2017     rortiz                Modificación para visualizar de forma correcta la descripción de colectores para Banca Personas.
* 20181001             10 Ene 2018     Jhenriquez            Se optimiza consulta hacia cheque collection y se reparan los "falsos" comentarios.
* 20180129             29 Ene 2018     rmoran				 Se realiza cambio para segmentar la consulta desde banca en linea se coloca bandera Y.FLAG.TCIB  para detectar que solicitud es de banca en linea y consultar por fango de limites
* 20180308             08 Mar 2018     rcortes				 Actualizacion para los FT con AC67 - Colectores CINTEX
*--------------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_ENQUIRY.COMMON  
    $INSERT I_F.ENQUIRY
    $INSERT I_F.STMT.ENTRY
    $INSERT I_F.TRANSACTION  
    $INSERT I_F.EB.SYSTEM.ID
    $INSERT I_F.SLV.ACC.STMT.TXN.CODE
    $INSERT I_F.TELLER.FINANCIAL.SERVICES
    $INSERT I_F.FUNDS.TRANSFER
    $INSERT I_F.TELLER
    $INSERT I_F.FT.TXN.TYPE.CONDITION
    $INSERT I_F.ACCOUNT
    $INSERT I_F.AA.ARRANGEMENT
    $INSERT I_SLV.E.ACCT.COMMON
    $INSERT I_F.EB.SLV.GLOBAL.PARAM
    $INSERT I_F.EB.SLV.KEYS.PARAMS
    $INSERT I_F.TELLER.TRANSACTION
    $INSERT I_F.AC.CHARGE.REQUEST
    $INSERT I_F.FT.COMMISSION.TYPE 
    $INSERT I_F.SLV.GEN.PARAM
    $INSERT I_F.EB.SLV.COL.FRONT.END
    $INSERT I_F.EB.SLV.COLECTOR
    $INSERT I_F.FT.BULK.MASTER
    $INSERT I_F.CHEQUE.COLLECTION
*-------------------------------------------------------------------------------
    GOSUB INIT    
    RETURN
*** <region name= INIT>
*--------------------------------------------------------------------------------
INIT:
*** <desc> </desc>
    FN.STMT.ENTRY = 'F.STMT.ENTRY'
    F.STMT.ENTRY = ''
    CALL OPF(FN.STMT.ENTRY,F.STMT.ENTRY)

    FN.TRANSACTION = 'F.TRANSACTION'
    F.TRANSACTION = ''
    CALL OPF(FN.TRANSACTION,F.TRANSACTION)

    FN.ACCOUNT = 'F.ACCOUNT'
    F.ACCOUNT = ''
    CALL OPF(FN.ACCOUNT,F.ACCOUNT)

    FN.AC.SUB.ACCOUNT = 'F.AC.SUB.ACCOUNT'
    F.AC.SUB.ACCOUNT = ''
    CALL OPF(FN.AC.SUB.ACCOUNT,F.AC.SUB.ACCOUNT)

    FN.TFS = 'F.TELLER.FINANCIAL.SERVICES'
    F.TFS = ''
    CALL OPF(FN.TFS,F.TFS)

    FN.TFS.HIS = 'F.TELLER.FINANCIAL.SERVICES$HIS'
    F.TFS.HIS = ''
    CALL OPF(FN.TFS.HIS,F.TFS.HIS)

    FN.FUNDS.TRANSFER = 'F.FUNDS.TRANSFER'
    F.FUNDS.TRANSFER = ''
    CALL OPF(FN.FUNDS.TRANSFER,F.FUNDS.TRANSFER)

    FN.FT.TXN.TYPE.CONDITION = 'F.FT.TXN.TYPE.CONDITION'
    F.FT.TXN.TYPE.CONDITION =''
    CALL OPF(FN.FT.TXN.TYPE.CONDITION,F.FT.TXN.TYPE.CONDITION)

    FN.FUNDS.TRANSFER$HIS='F.FUNDS.TRANSFER$HIS'
    F.FUNDS.TRANSFER$HIS=''
    CALL OPF(FN.FUNDS.TRANSFER$HIS,F.FUNDS.TRANSFER$HIS)

    FN.EB.SLV.GLOBAL.PARAM = 'F.EB.SLV.GLOBAL.PARAM'
    F.EB.SLV.GLOBAL.PARAM = ''
    CALL OPF(FN.EB.SLV.GLOBAL.PARAM,F.EB.SLV.GLOBAL.PARAM)
    
    Y.EB.SLV.GLOBAL.PARAM.ID = 'HIDDEN.TXN.STMT.BALANCE.TYPE'

    FN.EB.KEYS.PARAM = 'F.EB.SLV.KEYS.PARAMS'
    F.EB.KEYS.PARAM	 = ''
    CALL OPF(FN.EB.KEYS.PARAM,F.EB.KEYS.PARAM)

    FN.TELLER = 'FBNK.TELLER' ;*--- magarcia
    F.TELLER = ''
    CALL OPF(FN.TELLER, F.TELLER)

    FN.TELLER.TRANS       = 'FBNK.TELLER.TRANSACTION'
    F.TELLER.TRANS       = ''
    CALL OPF(FN.TELLER.TRANS, F.TELLER.TRANS)

    FN.CHARGE.REQUEST = 'F.AC.CHARGE.REQUEST'
    F.CHARGE.REQUEST = ''
    CALL OPF(FN.CHARGE.REQUEST,F.CHARGE.REQUEST)

    FN.COMMISSION.TYPE = 'F.FT.COMMISSION.TYPE'
    F.COMMISSION.TYPE = ''
    CALL OPF(FN.COMMISSION.TYPE,F.COMMISSION.TYPE)

    FN.SLV.GEN.PARAM = 'F.SLV.GEN.PARAM'
    F.SLV.GEN.PARAM  = ''
    CALL OPF(FN.SLV.GEN.PARAM, F.SLV.GEN.PARAM)

    FN.SLV.COL.FRONT.END = 'F.EB.SLV.COL.FRONT.END'
    F.SLV.COL.FRONT.END  = ''
    CALL OPF(FN.SLV.COL.FRONT.END,F.SLV.COL.FRONT.END)

    FN.EB.SLV.COLECTOR = 'F.EB.SLV.COLECTOR'
    F.EB.SLV.COLECTOR  = ''
    CALL OPF(FN.EB.SLV.COLECTOR,F.EB.SLV.COLECTOR)

    FN.BULK.MASTER	   = 'F.FT.BULK.MASTER'
    F.BULK.MASTER	   = ''
    CALL OPF(FN.BULK.MASTER, F.BULK.MASTER)

    FN.CHQ.COL = 'F.CHEQUE.COLLECTION'
    F.CHQ.COL = ''
    CALL OPF(FN.CHQ.COL, F.CHQ.COL)
    
    EQU PAGO.CH.PROPIO TO 'PagoChPropio'
    EQU ACRE.CH.AJENO  TO 'AcredChAjeno'
    TFS.TIPO.TRANS   = ''
    TFS.TT.TRANS     = ''
    FT.NOTA.DESC     = ''
    ES.CHQ.PRP       = 0

;*Se Reemplaza por Catalogo en SLV.GEN.PARAM>SLV.TXN.STMT.ACC : OCORNEJO 02.06.2017
;*---------------------------------------------------------------------------------
;*EQU NOTA.ABONO TO 'ACNA'
;*EQU NOTA.CARGO TO 'ACNC' ;*--- magarcia
;*EQU REINT.COM.LF TO 'ACRL'
;*TXN.ARR = NOTA.ABONO : VM : NOTA.CARGO : VM : REINT.COM.LF
    CALL F.READ(FN.SLV.GEN.PARAM, 'SLV.TXN.STMT.ACC', R.SLV.GEN.PARAM, F.SLV.GEN.PARAM, ERR.SLV.GEN.PARAM)
    TXN.ARR = R.SLV.GEN.PARAM<SLV.GEN.TX.TYPE.PARAM>
;*---------------------------------------------------------------------------------
 
;*@Author: Ronald Ortiz
;*@Date: 20170517 
    EQU ABONO.COLECTOR TO 'AC66'
    EQU PAGO.COLECTOR  TO 'AC64'
    EQU CARGO.COLECTOR TO 'AC67'
    EQU CARGO.COMISION TO 'AC68'

    Y.ARR.TFS.CHQ.AJENO = ''
    R.SUB.ACC.LIST = ''
    R.ACCT = ''
    TOTAL.BAL = 0
    VALUE.FLAG = ''
    Y.DATE = ''
    BALANCE.TYPE = ''
    Y.RECORD.STATUS.TEMP = ''
    Y.OUT.DATA = ''
    READ.ASP = ''   ;* Flag to represent read to ACCT.STMT.PRINT
    ASP.ID = ''
    ASP.BAL = ''
    Y.REV.FLAG = ''
    FN.ACCT.STMT.PRINT = "F.ACCT.STMT.PRINT"
    FV.ACCT.STMT.PRINT = ''
    CALL OPF(FN.ACCT.STMT.PRINT , FV.ACCT.STMT.PRINT)

    Y.TFS.ID.TEMPS = ''
    BALANCE.TYPE = 'BOOKING'
    SEL.CMD1 = '' ; SEL.CMD = ''
    FN.SLV.ACC.STMT.TXN.CODE = 'F.SLV.ACC.STMT.TXN.CODE'
    R.SLV.ACC.STMT.TXN.CODE = '' ; SLV.ACC.STMT.TXN.CODE.ERR = ''
    Y.COMMISSION.CODES = '' ; Y.TAX.CODES = ''
    CALL CACHE.READ(FN.SLV.ACC.STMT.TXN.CODE,'SYSTEM',R.SLV.ACC.STMT.TXN.CODE,SLV.ACC.STMT.TXN.CODE.ERR)
    IF R.SLV.ACC.STMT.TXN.CODE THEN
        Y.COMMISSION.CODES = R.SLV.ACC.STMT.TXN.CODE<SLV.TXN.COMMISSION.CODE>
        Y.TAX.CODES = R.SLV.ACC.STMT.TXN.CODE<SLV.TXN.TAX.CODE>
    END
    ACCT.POS = '' ; FROM.POS = '' ; TO.POS = '' ; ACCT.NO = '' ; FROM.VAL = '' ; TO.VAL = ''
    
    LOCATE "ACCT.ID" IN D.FIELDS<1> SETTING ACCT.POS THEN
    	ACCT.NO = D.RANGE.AND.VALUE<ACCT.POS>
    END

    LOCATE "DATE.FROM" IN D.FIELDS<1> SETTING FROM.POS THEN
    	DATE.FROM = D.RANGE.AND.VALUE<FROM.POS>
    END
    
    LOCATE "DATE.TO" IN D.FIELDS<1> SETTING TO.POS THEN
    	DATE.TO = D.RANGE.AND.VALUE<TO.POS>
    END
 
   ;*Debug  
*  ACCT.NO   = '10000000023491' ;*Cuando se llama desde t24 
*   ACCT.NO   =  '10000000041899'   ;*'10000000000227|0|';* formado de llamada desde banca en linea #CUENTA*VALORACTUALRANGO=150,300,450|ultimo closingbalance
*   DATE.FROM = TODAY ;*'20150710';*'20150701';
*   DATE.TO   = TODAY;*'20150809' ;*'20150810' 

   ;*Se separa el valor de la cuenta y el current para la peticion que se hace desde banca
    Y.FLAG.TCIB = 0
    
    FINDSTR "|" IN ACCT.NO SETTING Ap,Vp THEN 
        ;*Peticion por banca en linea
    	Y.FLAG.TCIB = 1
    	Y.ACCT.NO	= ACCT.NO
    	ACCT.NO 	= FIELD(ACCT.NO,'|',1)		
		Y.LMT		= FIELD(Y.ACCT.NO,'|',2)
		Y.CLOSING.BALANCE = FIELD(Y.ACCT.NO,'|',3)
		Y.LIMIT.I	= 0
		Y.LIMIT.F	= 0 
	END  
		  
;*20161108 - E
    APPL.TFS = 'TELLER.FINANCIAL.SERVICES'
    FIELDNAME.TFS = 'LF.COD.VER'
    POS.TFS = ''
    CALL MULTI.GET.LOC.REF(APPL.TFS,FIELDNAME.TFS,POS.COD.VER)

    ID.CAT.VER = 'SLV.CATA.VER.CJ'
    FN.EB.SLV.KEYS.PARAMS = 'F.EB.SLV.KEYS.PARAMS'
    F.EB.SLV.KEYS.PARAMS = ''
    CALL OPF(FN.EB.SLV.KEYS.PARAMS,F.EB.SLV.KEYS.PARAMS)
    CALL CACHE.READ (FN.EB.SLV.KEYS.PARAMS,ID.CAT.VER,R.CAT.VER,ER)

;*20161108 - E
 
;*20150728P - S
    GOSUB GLOBAL.PARAM.BAL.FET
    GOSUB HORA.EMISION.DATE
;*20150728P - E
    IF ACCT.NO THEN
        GOSUB CHECK.ACCT.NO ;*
    END
    IF Y.NO.ACCT.FLG THEN
        RETURN
    END
    ACCT.NO = FIELD(ACCT.NO,';',1)

;*20150716 - E 
    IF Y.ACCOUNT.NUMBER EQ '' THEN
        Y.ACCOUNT.NUMBER = ACCT.NO
    END
    GOSUB GET.ACCT.OPEN.BALANCE ;*
    
;*20150716 - S    
    Y.AC.DUP.VAR = ACCT.NO
    CALL EB.ACCT.ENTRY.LIST(Y.AC.DUP.VAR,DATE.FROM,DATE.TO,YID.LIST,OPENING.BAL,ER)
    SEL.LIST = YID.LIST
    
;*  SEL.CMD ='SELECT ' : FN.STMT.ENTRY: ' WITH ACCOUNT.NUMBER EQ ':ACCT.NO:SEL.CMD1
*20150716 - E
    GOSUB SELECT.STMT 
   
*20150717 -S
    IF YID.LIST EQ "" THEN
        CALL F.READ(FN.ACCOUNT,Y.ACCOUNT.NUMBER, R.ACCT.REC, F.ACCOUNT, AC.ERR)
        IF R.ACCT.REC THEN
            Y.STMT.CURRENCY = R.ACCT<AC.CURRENCY>
        END
        Y.TEMP.ARR = "":'*':Y.ACCOUNT.NUMBER:'*':Y.ACCOUNT.NUMBER:'*':Y.STMT.CURRENCY:'*':""
        Y.TEMP.ARR:='*':"":'*':"":'*':"":'*':""
        Y.TEMP.ARR:='*':"":'*':"":'*':"":'*':""
        Y.TEMP.ARR:='*':"":'*':"":'*':Y.OPEN.BAL:'*':"":'*':""
    END

    Y.TEMP.ARR.BKP = Y.TEMP.ARR
    Y.NEW.FMT.ARR  = Y.TEMP.ARR
    GOSUB NEW.FMT.CR.DR
    
*20150717 - E
;*    OUT.ARRAY = Y.TEMP.ARR
;*eurias
    GOSUB AGRUPAR.NOTA ;*Agrupar las notas de cargo

    OUT.ARRAY = Y.VAL.ARR
    Y.TEMP.ARR = Y.VAL.ARR 
	 
*	CRT 'OUT.ARRAY FINAL : ' : OUT.ARRAY	
 
    GOSUB UPDATE.SPOOL.FILE ;*  

    RETURN  
*-----------------------------------------------------------------------------
NEW.FMT.CR.DR:  
*************
    CONVERT VM TO "###" IN Y.NEW.FMT.ARR                                ;*If the records have VM markers. It wasnt responding properly by REMOVE command
    Y.LCCY = LCCY
    Y.FST.REC =1
    Y.COUNTER = 0
    Y.NXT.FMT.VAL = ''
    Y.AMT = ''
 
    LOOP
        REMOVE Y.NXT.FMT.VAL FROM Y.NEW.FMT.ARR SETTING Y.POS.NXT.SEL
    WHILE Y.NXT.FMT.VAL:Y.POS.NXT.SEL
        CONVERT "###" TO VM IN Y.NXT.FMT.VAL
        Y.COUNTER++
        GOSUB GET.DR.CR.TYPE.VAL
        GOSUB INIT.PROCESS.ARR
    REPEAT
    RETURN
*-----------------------------------------------------------------------------
INIT.PROCESS.ARR:
*****************

    IF Y.FST.REC EQ '1' THEN                                             ;* For the record if first record is TFS
        GOSUB INIT.1.DR.CR
        Y.LAST.REC.FLAG = 1
        Y.FST.REC = 0
    END ELSE
        GOSUB INIT.2.DR.CR
        GOSUB FMT.VAL.ARR
        LOCATE Y.2.TREF IN Y.1.TREF<1,1> SETTING Y.FND.POS THEN      ;*process if txn id matches
        GOSUB CALC.CR.DR.REV.AMT
        FLAG.MATCH = 1
        Y.LAST.REC.FLAG = 1
        GOSUB EXCHANGE.VALUES
    END ELSE
        IF FLAG.MATCH EQ '1' THEN                                    ;*When Id changes and we have added amount earlier
            GOSUB FMT.VAL.ARR
            IF Y.1.AMT LT '0' AND Y.1.REC.STAT EQ "REVE" THEN             ;*add and display debited amount which are reversed
                Y.NEG.AMT = Y.NEG.AMT + Y.1.AMT
                Y.VAL.ARR<-1> = Y.FIRST.TEMP:"*":Y.NEG.AMT:"*":Y.LAST.TEMP
            END

            IF Y.1.AMT LT '0' AND Y.1.REC.STAT NE "REVE" THEN           ;* dispaly the debit transaction which are not reversed.
                Y.VAL.ARR<-1> = Y.1.ARR
            END

            IF Y.1.AMT GT '0' THEN                                        ;* if its credit txn, add and display
                Y.AMT = Y.1.AMT+Y.AMT
                Y.VAL.ARR<-1> = Y.FIRST.TEMP:"*":Y.AMT:"*":Y.LAST.TEMP
            END
            FLAG.MATCH = 0
            Y.LAST.REC.FLAG = 1
            Y.AMT = ''
            Y.NEG.AMT = ''
            GOSUB EXCHANGE.VALUES
        END ELSE
            Y.VAL.ARR<-1> = Y.1.ARR                                   ;*When Id Changes and no addition happen with last arrays
            FLAG.MATCH = 0
            Y.LAST.REC.FLAG = 1
            GOSUB EXCHANGE.VALUES
        END                           ;*If end.
    END                               ;*Locate end
    END

    IF DCOUNT(Y.TEMP.ARR,FM) EQ Y.COUNTER AND Y.LAST.REC.FLAG EQ '1' THEN   ;*for the last value in the array. Not to be missed.
        IF FLAG.MATCH EQ '1' THEN
            GOSUB FMT.VAL.ARR
            IF Y.1.AMT LT '0' AND Y.1.REC.STAT EQ "REVE" THEN             ;*add and display debited amount which are reversed
                Y.NEG.AMT = Y.NEG.AMT + Y.1.AMT
                Y.VAL.ARR<-1> = Y.FIRST.TEMP:"*":Y.NEG.AMT:"*":Y.LAST.TEMP
            END

            IF Y.1.AMT LT '0' AND Y.1.REC.STAT NE "REVE" THEN           ;* dispaly the debit transaction which are not reversed.
                Y.VAL.ARR<-1> = Y.1.ARR
            END

            IF Y.1.AMT GT '0' THEN                                        ;* if its credit txn, add and display
                Y.AMT = Y.1.AMT+Y.AMT
                Y.VAL.ARR<-1> = Y.FIRST.TEMP:"*":Y.AMT:"*":Y.LAST.TEMP
            END

            FLAG.MATCH = 0
            Y.AMT = ''
            Y.NEG.AMT = ''
        END ELSE
            Y.VAL.ARR<-1> = Y.1.ARR
            FLAG.MATCH = 0
        END
    END
 
    RETURN 
*-----------------------------------------------------------------------------
CALC.CR.DR.REV.AMT:
*******************
*For the transactions with SAME Ids, process the details for credit and debit.
*All the credit of same transaction should be added and displayed as one line with total sum irrespective of whether it is reversed or not.
*All the debit of same transaction id(Which are reversed) should be only added and dispalyed.
    IF Y.1.AMT LT '0' AND Y.1.REC.STAT EQ "REVE" THEN
        Y.NEG.AMT = Y.NEG.AMT + Y.1.AMT
        IF FLG.1.DR EQ FLG.2.CR THEN                                    ;*Changes in amount txn from  debit to credit, display the total of credit.
            Y.VAL.ARR<-1> = Y.FIRST.TEMP:"*":Y.NEG.AMT:"*":Y.LAST.TEMP
            Y.NEG.AMT= ''
        END
    END

    IF Y.1.AMT LT '0' AND Y.1.REC.STAT NE "REVE" THEN
        Y.VAL.ARR<-1> = Y.1.ARR
    END

    IF Y.1.AMT GT '0' THEN
        Y.AMT = Y.1.AMT+Y.AMT
        IF FLG.1.CR EQ FLG.2.DR THEN
            Y.VAL.ARR<-1> = Y.FIRST.TEMP:"*":Y.AMT:"*":Y.LAST.TEMP
        END
    END
    RETURN
*-----------------------------------------------------------------------------
FMT.VAL.ARR:
***********
    Y.NULL = ''
    Y.NXT.TEMP = Y.1.ARR
    Y.CNT.FULL.ARR= DCOUNT(Y.1.ARR,"*")
    Y.FIRST.TEMP = FIELD(Y.NXT.TEMP,"*",1,5)
    Y.DCNT.CHQ = DCOUNT(FIELD(Y.NXT.TEMP,"*",7,Y.CNT.FULL.ARR),"*")
    Y.LAST.TEMP = FIELD(Y.NXT.TEMP,"*",7,Y.DCNT.CHQ-1):"*":Y.NULL ; *Remove the cheque number
    RETURN
*-----------------------------------------------------------------------------
EXCHANGE.VALUES:
****************
    Y.1.ARR = Y.2.ARR
    Y.1.CCY = Y.2.CCY
    Y.1.TREF = Y.2.TREF
    Y.1.AMT = Y.2.AMT
    Y.1.REC.STAT = Y.2.REC.STAT
    FLG.1.DR = FLG.2.DR
    FLG.1.CR = FLG.2.CR

    RETURN
*-----------------------------------------------------------------------------
INIT.1.DR.CR:
************
    Y.1.REC.STAT = FIELD(Y.NXT.FMT.VAL,"*",18,1)
    Y.1.ARR = Y.NXT.FMT.VAL
    Y.1.CCY = FIELD(Y.NXT.FMT.VAL,"*",4,1)
    Y.1.TREF = FIELD(Y.NXT.FMT.VAL,"*",10,1)
    IF Y.1.TREF[1,3] NE "TFS" THEN
        Y.1.TREF = FIELD(Y.NXT.FMT.VAL,"*",12,1)
    END
    Y.1.AMT = ''
    IF Y.1.CCY EQ LCCY THEN
        Y.1.AMT = FIELD(Y.NXT.FMT.VAL,"*",6,1)
    END ELSE
        Y.1.AMT = FIELD(Y.NXT.FMT.VAL,"*",7,1)
    END

    IF Y.1.AMT LT '0' THEN
        FLG.1.DR  = '1'
        FLG.1.CR = '0'
    END ELSE
        FLG.1.CR = '1'
        FLG.1.DR  = '0'
    END
    RETURN
*-----------------------------------------------------------------------------
INIT.2.DR.CR:
************
    Y.2.REC.STAT = FIELD(Y.NXT.FMT.VAL,"*",18,1)
    Y.2.ARR = Y.NXT.FMT.VAL
    Y.2.CURR = FIELD(Y.NXT.FMT.VAL,"*",4,1)
    Y.2.TREF = FIELD(Y.NXT.FMT.VAL,"*",10,1)
    IF Y.2.TREF[1,3] NE "TFS" THEN
        Y.2.TREF = FIELD(Y.NXT.FMT.VAL,"*",12,1)
    END
    IF Y.2.CURR EQ LCCY THEN
        Y.2.AMT = FIELD(Y.NXT.FMT.VAL,"*",6,1)
    END ELSE
        Y.2.AMT = FIELD(Y.NXT.FMT.VAL,"*",7,1)
    END

    IF Y.2.AMT LT '0' THEN
        FLG.2.DR  = '1'
        FLG.2.CR = '0'
    END ELSE
        FLG.2.CR = '1'
        FLG.2.DR  = '0'
    END

    RETURN
*-----------------------------------------------------------------
GET.DR.CR.TYPE.VAL:
*******************
    Y.REC.STAT = ""
    Y.REC.STAT = FIELD(Y.NXT.FMT.VAL,"*",18,1)
    Y.CK.ARR = Y.NXT.FMT.VAL
    Y.CK.CCY = FIELD(Y.NXT.FMT.VAL,"*",4,1)
    Y.CK.TREF = FIELD(Y.NXT.FMT.VAL,"*",10,1)
    Y.CK.AMT = ''
    IF Y.CK.CCY EQ LCCY THEN
        Y.CK.AMT = FIELD(Y.NXT.FMT.VAL,"*",6,1)
    END ELSE
        Y.CK.AMT = FIELD(Y.NXT.FMT.VAL,"*",7,1)
    END

    RETURN
*-----------------------------------------------------------------------------
*20150728P - S
GLOBAL.PARAM.BAL.FET:
*********************
    R.EB.SLV.GLOBAL.PARAM = '' ; Y.ERR.GLB.PARAM = ''
    CALL F.READ(FN.EB.SLV.GLOBAL.PARAM,Y.EB.SLV.GLOBAL.PARAM.ID,R.EB.SLV.GLOBAL.PARAM,F.EB.SLV.GLOBAL.PARAM,Y.ERR.GLB.PARAM)
    IF R.EB.SLV.GLOBAL.PARAM THEN
        Y.BAL.TYPE.PARAMATERIZE = R.EB.SLV.GLOBAL.PARAM<EB.SLV39.VALOR.PARAM>
    END
    RETURN
;*20150728P - E

*-----------------------------------------------------------------------------

*** <region name= SELECT.STMT>
SELECT.STMT:
*** <desc> </desc>
    YID.STMT.TEMP = ''
;*20150716 - S
;*   CALL EB.READLIST(SEL.CMD,SEL.LIST,'',NO.OF.RECS,RET.ERR)
;*20150716 - E
	;*TOTAL DE TX
   Y.LAST.CNT 	= DCOUNT(SEL.LIST,FM)
   Y.LAST.COUNT = DCOUNT(SEL.LIST,FM)
   Y.LAST.STMT = FIELD(SEL.LIST,FM,Y.LAST.CNT,1)
    
*;Variables limites consulta banca en linea
;*@Autor:Roberto Moran
;*@Date:23/01/2018
;*@Coment: Funcionalidad para la consulta desde banca en linea, se limita consulta por pagineo de registros
	Y.COUNT.BANCA = 1	
	IF Y.FLAG.TCIB EQ 1 THEN	
		Y.LIMIT.I = Y.LMT + 1
		Y.LIMIT.F = Y.LIMIT.I + 149 
    END 
     
    LOOP
       REMOVE YID.STMT FROM SEL.LIST SETTING STMT.POS
    WHILE YID.STMT : STMT.POS 
;***************************************************************************    
    ;*@En caso se manda desde banca en linea se limita la busqueda por limites
   	IF Y.FLAG.TCIB EQ 1 THEN  
       IF Y.COUNT.BANCA GE Y.LIMIT.I AND Y.COUNT.BANCA LE Y.LIMIT.F THEN
			;*En caso sea de banca en linea 
			Y.TXN.CODE = ''
	        GOSUB NULLIFY.VALUES ;*
	        GOSUB READ.STMT.ENTRY ;*
	        ;*20150728P - S
	        GOSUB STMT.BALANCE.TYPE.CHK
	        IF R.ENTRY.REC AND (Y.BAL.TYPE.FLAG EQ '') THEN
	        	;*20150728P - E
	        	GOSUB GET.RECORD.VALUES ;*
	        END	                
			Y.COUNT.BANCA = Y.COUNT.BANCA + 1			
		END ELSE			
			Y.COUNT.BANCA = Y.COUNT.BANCA + 1			
		END
	END	
	;*En caso la peticion sea por t24 debe ejecutarse para todas las peticiones
;***************************************************************************
	ELSE IF Y.FLAG.TCIB EQ 0 THEN
		;*En caso sea de agencia T24
		Y.TXN.CODE = ''
	    GOSUB NULLIFY.VALUES ;*
	    GOSUB READ.STMT.ENTRY ;*
	    ;*20150728P - S
	    GOSUB STMT.BALANCE.TYPE.CHK
	    IF R.ENTRY.REC AND (Y.BAL.TYPE.FLAG EQ '') THEN
	    ;*20150728P - E
	    	GOSUB GET.RECORD.VALUES ;*
	    END
		Y.COUNT.BANCA = Y.COUNT.BANCA + 1
	END 
;***************************************************************************	
REPEAT

    IF Y.TEMP.ARR.TFS AND Y.TEMP.ARR EQ '' THEN
        Y.TEMP.ARR = Y.TEMP.ARR.TFS
        Y.TEMP.ARR.TFS = ''
    END
    IF Y.TEMP.ARR.TFS AND Y.TEMP.ARR NE '' THEN
        Y.TEMP.ARR<-1>= Y.TEMP.ARR.TFS
        Y.TEMP.ARR.TFS = ''
    END

    RETURN
*-----------------------------------------------------------------------------
;*20150728P - S
STMT.BALANCE.TYPE.CHK:
**********************

    Y.BAL.TYPE.FLAG = '' ; Y.BAL.TYPE = ''
    Y.BAL.TYPE = R.ENTRY.REC<AC.STE.BALANCE.TYPE>
    IF Y.BAL.TYPE EQ Y.BAL.TYPE.PARAMATERIZE THEN
        Y.BAL.TYPE.FLAG = '1'
    END
    RETURN
;*20150728P - E

*** </region>
*-----------------------------------------------------------------------------

*** <region name= GET.RECORD.VALUES>
GET.RECORD.VALUES:
*** <desc> </desc>
    Y.STMT.ID = YID.STMT
    Y.AMT.LCY = R.ENTRY.REC<AC.STE.AMOUNT.LCY>
    Y.ACCT.ID = ACCT.NO
    Y.ACCOUNT.NUMBER = R.ENTRY.REC<AC.STE.ACCOUNT.NUMBER>
    Y.STMT.CURRENCY = R.ENTRY.REC<AC.STE.CURRENCY>
    Y.STMT.COMPANY = R.ENTRY.REC<AC.STE.COMPANY.CODE>
    Y.STMT.AMOUNT.LCY = R.ENTRY.REC<AC.STE.AMOUNT.LCY>
    Y.STMT.AMOUNT.FCY = R.ENTRY.REC<AC.STE.AMOUNT.FCY>
    Y.TXN.CODE = R.ENTRY.REC<AC.STE.TRANSACTION.CODE>
    Y.STMT.VALUE.DATE = R.ENTRY.REC<AC.STE.VALUE.DATE>
    Y.STMT.THEIR.REF = R.ENTRY.REC<AC.STE.THEIR.REFERENCE>
    Y.STMT.BOOKING.DATE = R.ENTRY.REC<AC.STE.BOOKING.DATE>
    Y.STMT.TRANS.REFERENCE = R.ENTRY.REC<AC.STE.TRANS.REFERENCE>
    Y.STMT.NARRATIVE = R.ENTRY.REC<AC.STE.NARRATIVE>
    Y.STMT.SYSTEM.ID = R.ENTRY.REC<AC.STE.SYSTEM.ID>
    Y.STMT.RECORD.STATUS = R.ENTRY.REC<AC.STE.RECORD.STATUS>
    Y.STMT.REVERSAL.MARKER = R.ENTRY.REC<AC.STE.REVERSAL.MARKER>
    Y.STMT.CHQ.COLL.ID = R.ENTRY.REC<AC.STE.CHQ.COLL.ID>
    Y.STMT.MASK.PRINT = R.ENTRY.REC<AC.STE.MASK.PRINT>
;* 20150728N - S
    Y.STMT.CHQ.NUM.ARR = R.ENTRY.REC<AC.STE.CHEQUE.NUMBER>
    Y.CHQ.NUM = FIELD(Y.STMT.CHQ.NUM.ARR,VM,1,1)
    
;* 20150728N - E
;*    IF Y.STMT.CHQ.COLL.ID[1,2] EQ 'CC' THEN
;*        GOSUB CHECK.CHQ.COLL ; *check the cheque collection details to hide the mask entries
;*    END
;*    IF NOT(CHQ.COLL.FLG) THEN

    GOSUB MAIN.PROCESS
;*    END
RETURN
*-----------------------------------------------------------------------------

MAIN.PROCESS:
    GOSUB READ.TRANSACTION ;*Read the transaction to get the narrative,. this change is included for TFS display.
    IF FIELD(Y.STMT.THEIR.REF,'.',1) EQ 'PS' THEN
        Y.STMT.TRANS.REFERENCE = THEIR.REF.VALUE
    END

    GOSUB GET.NARRATIVE ;*Get the narrative by calling a core rtn, and it needs to be replaced by the conversion rtn in enquiry for the field DESC

    Y.TFS.ID = ''
    IF Y.STMT.THEIR.REF[1,3] EQ 'TFS' THEN
        Y.TFS.ID = Y.STMT.THEIR.REF
        Y.TFS.FLAG = '1'
        GOSUB CHECK.TFS ;*Check the TFS for primary account
    END ELSE
        Y.TFS.FLAG = ''
        IF Y.TEMP.ARR.TFS THEN
            YTFS.NEW.FLAG = ''
            Y.TFS.ID.TEMPS = ''
            IF Y.TEMP.ARR EQ '' THEN
                Y.TEMP.ARR = Y.TEMP.ARR.TFS
                Y.TEMP.ARR.TFS = ''
                Y.STMT.AMOUNT.LCY.TEMP = ''
                Y.STMT.AMOUNT.FCY.TEMP = ''
                Y.STMT.TRANS.REFERENCE.TEMP = ''
            END ELSE
                Y.TEMP.ARR<-1>= Y.TEMP.ARR.TFS
                Y.TEMP.ARR.TFS = ''
                Y.STMT.AMOUNT.LCY.TEMP = ''
                Y.STMT.AMOUNT.FCY.TEMP = ''
                Y.STMT.TRANS.REFERENCE.TEMP = ''
            END
        END
    END
 
** 20150805N - S When the last TFS is non chq and next is Non TFS transaction.
*    IF Y.NON.CHQ.FLAG AND Y.STMT.THEIR.REF[1,3] NE 'TFS' THEN
*        GOSUB APPEND.NON.CHQ.ARR
*        Y.NON.CHQ.FLAG = ""
*        Y.NON.CHQ.AMT.LCY = ""
*        Y.NON.CHQ.AMT.FCY = ""
*        Y.CHQ.FLAG = ""
*    END
** 20150805N - E

    IF FIELD(Y.STMT.THEIR.REF,'.',1) = 'PS' THEN
        Y.STMT.TRANS.REFERENCE = THEIR.REF.VALUE
    END
*
    GOSUB GET.BASE.APP ;*
    GOSUB CHECK.CURRENT.ID 

    IF YID.STMT.TEMP EQ '' THEN
        ;*       Y.TAX.FLAG = ''
        YID.STMT.TEMP = YID.STMT:'*':Y.TXN.CODE
    END ELSE
        GOSUB CHECK.EXISTING.ID 
        IF NOT(Y.TXN.FLAG) THEN
            YID.STMT.TEMP<-1> = YID.STMT:'*':Y.TXN.CODE
        END
    END
    IF Y.STMT.TRANS.REFERENCE[1,2] EQ 'FT' THEN
        Y.FT.ID = Y.STMT.TRANS.REFERENCE
        GOSUB PROCESS.FT ;*Read the FT,get the transaction type and pass the description oly for ACRC
    END
    IF NOT(Y.RECORD.STATUS) AND Y.STMT.RECORD.STATUS THEN
        Y.RECORD.STATUS = Y.STMT.RECORD.STATUS
    END
    IF Y.RECORD.STATUS NE '' AND Y.RECORD.STATUS NE 'REVE' THEN
        Y.RECORD.STATUS = ''
    END
*** Change 24/04/15
    Y.STMT.TRANS.REFERENCE = FIELD(Y.STMT.TRANS.REFERENCE,'\',1)
    IF Y.STMT.BOOKING.DATE THEN
        Y.STMT.BOOKING.DATE = OCONV(ICONV(Y.STMT.BOOKING.DATE,"D4"),"D4/E")
    END
    IF Y.STMT.VALUE.DATE THEN
        Y.STMT.VALUE.DATE = OCONV(ICONV(Y.STMT.VALUE.DATE,"D4"),"D4/E")
    END
*** Change 24/04/15
    IF Y.TEMP.ARR.TFS AND NOT(Y.CURR.TEMP.ARR.TFS) THEN
        GOSUB UPDATE.MAIN.ARR ;*Check the TFS array and pass it to main array,. this process for single leg account in TFS
    END
*Chnages for the CR on 25/08/2015
    IF NARR THEN
        Y.TRANS.NARR.DESC = Y.TRANS.NARR.DESC:VM:NARR
        NARR = ''
    END

;*CAMBIO DESCRIPCION DE AC.CHARGE.REQUEST
;*---------------------------------------
    GOSUB UPDATE.DESCR.CHG

    IF NOT(Y.TAX.FLAG) AND NOT(Y.TFS.FLAG) THEN
        ;* Separar Comision e IVA
        GOSUB COMISION.IVA.CHQ.BREAK.OFF
        ;*evaluar si es comision cheque cert,
        ;*evaluar si tiene liof
        ;*generar linea detalle liof, quitar liof de comision
        GOSUB EVAL.LIOF.COMM ;**evaluar si es comision cheque cert, *evaluar si tiene liof *generar linea detalle liof, quitar liof de comision
        IF Y.TEMP.ARR EQ '' THEN
            Y.TEMP.ARR = Y.STMT.ID:'*':Y.ACCT.ID:'*':Y.ACCOUNT.NUMBER:'*':Y.STMT.CURRENCY:'*':Y.STMT.COMPANY
            GOSUB FILL.TEMP.ARR ;*eurias cambio a gosub
        END ELSE
            Y.TEMP.ARR<-1> = Y.STMT.ID:'*':Y.ACCT.ID:'*':Y.ACCOUNT.NUMBER:'*':Y.STMT.CURRENCY:'*':Y.STMT.COMPANY
            GOSUB FILL.TEMP.ARR ;*eurias cambio a gosub
        END

    END

    IF Y.TAX.AMT THEN
        Y.TAX.FLAG = 1
    END
RETURN
*** </region>

*-----------------------------------------------------------------------------
*** <region name= CHECK.EXISTING.ID>
CHECK.EXISTING.ID:
*** <desc> </desc>
    Y.STMT.EXIST.FIRST = '' ; Y.STMT.CURR.FIRST = ''
    LOOP
        REMOVE Y.EXIST.ID FROM YID.STMT.TEMP SETTING EXIST.POS
    WHILE Y.EXIST.ID : EXIST.POS
        Y.STMT.EXIST.FIRST = FIELD(Y.EXIST.ID,'.',1)
        Y.STMT.CURR.FIRST = FIELD(YID.STMT,'.',1)
        GOSUB MOD.TAX.AMT
    REPEAT

RETURN
*** </region>

*-----------------------------------------------------------------------------
*** <region name= CHECK.CURRENT.ID>
CHECK.CURRENT.ID:
*** <desc> </desc>
    LOCATE Y.TXN.CODE IN Y.TAX.CODES<1,1> SETTING TXN.POS THEN
    Y.TAX.FLAG = 1
    IF Y.STMT.AMOUNT.LCY THEN
        Y.TAX.AMT = Y.STMT.AMOUNT.LCY
    END ELSE
        Y.TAX.AMT = Y.STMT.AMOUNT.FCY
    END
    END ELSE
* 20150728N - S
    Y.TAX.FLAG = ""
* 20150728N - E
    END
RETURN
*** </region>
*-----------------------------------------------------------------------------

*** <region name= READ.STMT.ENTRY>
READ.STMT.ENTRY:
*** <desc> </desc>

    R.ENTRY.REC = '' ; Y.ERR = ''
    CALL F.READ(FN.STMT.ENTRY,YID.STMT,R.ENTRY.REC,F.STMT.ENTRY,Y.ERR)

RETURN
*** </region>
*-----------------------------------------------------------------------------

*** <region name= MOD.TAX.AMT>
MOD.TAX.AMT:
*** <desc> </desc>
    IF Y.STMT.EXIST.FIRST EQ Y.STMT.CURR.FIRST THEN
        GOSUB MOD.TAX.AMT.EXT
    END ELSE
        YID.STMT.TEMP = ''
    END
RETURN
*** </region>

MOD.TAX.AMT.EXT:
***************
*    IF Y.TAX.FLAG THEN
    COMM.POS = ''
    Y.TAX.AMT.CHG = 0

;*Obteniendo el catalogo de las transacciones que requieres separacion de comision - IVA
;*---------------------------------------------------------------------------------------
    CALL F.READ(FN.EB.KEYS.PARAM, 'SLV.CAT.COMI.IVA.DIVIDE', RECORD.CAT.DIVIDE, F.EB.KEYS.PARAM, ERROR.CAT.DIVIDE)
    Y.CAT.DIV.CHG = RECORD.CAT.DIVIDE<EB.SLV18.VALOR>
;*Obteniendo el Id de AC.CHARGE.REQUEST
;*-------------------------------------
    CALL F.READ(FN.CHARGE.REQUEST,Y.STMT.THEIR.REF,RECORD.CHG,F.CHARGE.REQUEST,ERR.CHG)
    Y.TYPE.COMMISSION =  RECORD.CHG<CHG.CHARGE.CODE>

;*Verificar si el FT.COMMISSION.TYPE asociado al AC.CHG.REQUEST se encuentra en el catalogo
;*-----------------------------------------------------------------------------------------
    LOCATE Y.TXN.CODE IN Y.COMMISSION.CODES<1,1> SETTING COMM.POS THEN
    Y.TAX.FLAG = ''
    FIND Y.TYPE.COMMISSION IN Y.CAT.DIV.CHG SETTING Ap,Vp THEN
;*Si aplica se realiza separacion de comision e IVA caso contrario seguira el flujo normal
;*-----------------------------------------------------------------------------------------
    IF Y.STMT.AMOUNT.LCY THEN
        Y.STMT.AMOUNT.LCY = Y.STMT.AMOUNT.LCY ;*+ Y.TAX.AMT
        Y.TAX.AMT.CHG = Y.TAX.AMT
    END
    ELSE
    Y.STMT.AMOUNT.FCY = Y.STMT.AMOUNT.FCY ;*+ Y.TAX.AMT
    Y.TAX.AMT.CHG = Y.TAX.AMT
    END 
    END
    ELSE
    IF Y.STMT.AMOUNT.LCY THEN
        Y.STMT.AMOUNT.LCY = Y.STMT.AMOUNT.LCY + Y.TAX.AMT
    END ELSE
        Y.STMT.AMOUNT.FCY = Y.STMT.AMOUNT.FCY + Y.TAX.AMT
    END
    END
    Y.TAX.AMT = ''
    END ELSE
    GOSUB CHECK.CURRENT.ID ;* Just to check the txn code for tax
    END

    RETURN
*-----------------------------------------------------------------------------

*** <region name= NULLIFY.VALUES>
NULLIFY.VALUES:
*** <desc> </desc>
    CHQ.COLL.FLG = ''
    Y.FT.ID = ''
    Y.AMT.LCY = ''
    Y.ACCOUNT.NUMBER = ''
    Y.STMT.CURRENCY = ''
    Y.STMT.COMPANY = ''
    Y.STMT.AMOUNT.LCY = ''
    Y.STMT.AMOUNT.FCY = ''
    Y.TXN.CODE = ''
    Y.STMT.VALUE.DATE = ''
    Y.STMT.THEIR.REF = ''
    Y.STMT.BOOKING.DATE = ''
    Y.STMT.TRANS.REFERENCE = ''
    Y.STMT.NARRATIVE = ''
    Y.STMT.SYSTEM.ID = ''
    Y.RECORD.STATUS = ''
    Y.STMT.RECORD.STATUS = ''
    Y.STMT.REVERSAL.MARKER = ''
    Y.STMT.MASK.PRINT = ''
* 20150728N - S
    Y.STMT.CHQ.NUM.ARR = ""
    Y.CHQ.NUM = ""
* 20150728N - E
    Y.STMT.CHQ.COLL.ID = ''
    RETURN
*** </region>

*-----------------------------------------------------------------------------
*** <region name= GET.BASE.APP>
GET.BASE.APP:
*** <desc> </desc>
    Y.BASE.APP = ''
    IF Y.STMT.SYSTEM.ID EQ 'AC' AND FIELD(Y.STMT.TRANS.REFERENCE,'-',1) EQ 'AZ' THEN
        Y.STMT.SYSTEM.ID = 'AZ'
    END

    CALL CACHE.READ('F.EB.SYSTEM.ID',Y.STMT.SYSTEM.ID,R.EB.SYS.ID,ER)
    Y.BASE.APP = R.EB.SYS.ID<SID.APPLICATION>

    IF (Y.STMT.SYSTEM.ID EQ 'IC1' OR Y.STMT.SYSTEM.ID EQ 'IC2' OR Y.STMT.SYSTEM.ID EQ 'IC3' OR Y.STMT.SYSTEM.ID EQ 'IC4' OR Y.STMT.SYSTEM.ID EQ 'IC5') THEN
        IF NOT(FIELD(Y.STMT.TRANS.REFERENCE,'-',2)) THEN
            Y.BASE.APP= ""
        END
    END

    BEGIN CASE

        CASE Y.STMT.SYSTEM.ID EQ 'AC' AND Y.STMT.TRANS.REFERENCE[1,3] EQ 'CHG'
            Y.BASE.APP = 'AC.CHARGE.REQUEST'

        CASE Y.STMT.SYSTEM.ID EQ "PD" AND Y.STMT.TRANS.REFERENCE[1,4] EQ "PDCA"
            Y.BASE.APP = "PD.CAPTURE"

        CASE Y.STMT.SYSTEM.ID EQ 'SC' AND Y.STMT.TRANS.REFERENCE[1,4] EQ 'OPOD'
            Y.BASE.APP = "SEC.OPEN.ORDER"

        CASE Y.STMT.SYSTEM.ID EQ 'CQ' AND INDEX(Y.STMT.TRANS.REFERENCE,'.',1)
            Y.BASE.APP = 'CHEQUE.ISSUE'

        CASE Y.STMT.SYSTEM.ID EQ 'AA'
            Y.BASE.APP = 'AA.ARRANGEMENT.ACTIVITY'

    END CASE
    RETURN
*** </region>
*-----------------------------------------------------------------------------

*** <region name= GET.ACCT.OPEN.BALANCE>
GET.ACCT.OPEN.BALANCE:
*** <desc> </desc>

    ACCT.ID.TEMP = ''
    Y.INP.DATA = Y.ACCOUNT.NUMBER
    Y.DATE = DATE.FROM
    CALL CDT("",Y.DATE,"-01C")
    GOSUB CHECK.SUB.ACCOUNTS  ;* Check if sub accounts exist for the requested account

    BEGIN CASE
        CASE R.SUB.ACC.LIST       ;* Process sub accounts
            GOSUB GET.SUB.ACCOUNT.BALANCE
        CASE READ.ASP
            ASP.ID = Y.INP.DATA
            GOSUB GET.ACCT.STMT.PRINT.BALANCE         ;* Get balance from ACCT.STMT.PRINT for the operands LT nd LE
            Y.OPEN.BAL =  ASP.BAL
        CASE R.ACCT<AC.ARRANGEMENT.ID>      ;* Get Arrangement account balance
            GOSUB GET.ARRANGEMENT.ACCOUNT.BALANCE
        CASE OTHERWISE  ;* Get balance for normal Accounts
            GOSUB GET.ACCOUNT.BALANCE
    END CASE

    RETURN

*** </region>
*-----------------------------------------------------------------------
CHECK.SUB.ACCOUNTS:
*------------------
* Check for sub accounts and if exist then append the sub account ids with master account
    ACCT.ID.LIST = Y.INP.DATA
    CALL F.READ(FN.ACCOUNT, Y.INP.DATA, R.ACCT, F.ACCOUNT, AC.ERR)
    IF R.ACCT<AC.MAX.SUB.ACCOUNT> THEN
        CALL F.READ("F.AC.SUB.ACCOUNT",Y.INP.DATA,R.SUB.ACC.LIST,F.AC.SUB.ACCOUNT,ER)
        IF R.SUB.ACC.LIST THEN
            ACCT.ID.LIST := FM:R.SUB.ACC.LIST
        END
    END

    RETURN
*-----------------------------------------------------------------------
GET.SUB.ACCOUNT.BALANCE:
************************
* In case of sub account setup add up amounts of all accounts to arrive at the opening balance
    IDX = 1
    LOOP
        ACCT.ID.TEMP = ACCT.ID.LIST<IDX>
    UNTIL ACCT.ID.TEMP = '' DO
        IDX += 1
        YVAL.BALANCE = ''
        IF READ.ASP THEN
            ASP.ID = ACCT.ID.TEMP
            GOSUB GET.ACCT.STMT.PRINT.BALANCE
            TOTAL.BAL += ASP.BAL
        END ELSE
            CALL EB.GET.ACCT.BALANCE(ACCT.ID.TEMP, "", BALANCE.TYPE, Y.DATE, "", YVAL.BALANCE, "", "", "")
            TOTAL.BAL += YVAL.BALANCE
        END
    REPEAT
	;* Se agrega para fincionalidad estado de cuenta Banca en linea
 	IF Y.FLAG.TCIB EQ 1 AND Y.CLOSING.BALANCE NE '' THEN
		 Y.OPEN.BAL = Y.CLOSING.BALANCE   
    END ELSE
    	Y.OPEN.BAL = TOTAL.BAL
    END   
 
    RETURN 
*-----------------------------------------------------------------------
GET.ACCT.STMT.PRINT.BALANCE:
*****************************
* Get balance from ACCT.STMT.PRINT for the operands LT nd LE
* Since start date cannot be determined when previos movement is archived,
* get opening balance from first position of ACCT.STMT.PRINT live file which will contain purge
* date closing balance
*
    ASP.BAL = 0
    CALL F.READ(FN.ACCT.STMT.PRINT , ASP.ID , R.ASP , FV.ACCT.STMT.PRINT , ASP.ERR)
    ASP.BAL = FIELD(R.ASP<1>,"/",2)

    RETURN
*-----------------------------------------------------------------------
GET.ARRANGEMENT.ACCOUNT.BALANCE:
*******************************
* Get balance for arrangement Account
    ACCT.ID.TEMP = Y.INP.DATA

    CALL AA.GET.ACCT.BALANCE(ACCT.ID.TEMP, "", BALANCE.TYPE, Y.DATE, "", TOTAL.BAL, "", "", "")
    ;* Se agrega para fincionalidad estado de cuenta Banca en linea
 	IF Y.FLAG.TCIB EQ 1 AND Y.CLOSING.BALANCE NE '' THEN
		 Y.OPEN.BAL = Y.CLOSING.BALANCE   
    END ELSE
    	Y.OPEN.BAL = TOTAL.BAL
    END   

    RETURN
*-----------------------------------------------------------------------
GET.ACCOUNT.BALANCE:
********************
* Get balance from core routine.
*
    ACCT.ID.TEMP = Y.INP.DATA
    CALL EB.GET.ACCT.BALANCE(ACCT.ID.TEMP, "", BALANCE.TYPE, Y.DATE, "", TOTAL.BAL, "", "", "")
	;* Se agrega para fincionalidad estado de cuenta Banca en linea
 	IF Y.FLAG.TCIB EQ 1 AND Y.CLOSING.BALANCE NE '' THEN
		 Y.OPEN.BAL = Y.CLOSING.BALANCE   
    END ELSE
    	Y.OPEN.BAL = TOTAL.BAL
    END   

    RETURN
*------------------------------------------------------------------------

*** <region name= GET.NARRATIVE>
GET.NARRATIVE:
*** <desc>Get the narrative by calling a core rtn, and it needs to be replaced by the conversion rtn in enquiry for the field DESC </desc>
    Y.NARR.DESC = ''
    NARR = ''
    ENTRY.REC = R.ENTRY.REC
    CALL GET.NARRATIVE(YID.STMT,ENTRY.REC,NARR)
    IF NARR THEN
*** Nullfied the description, Bank doesnt want to show the internal account number in report.
*** Removed the column number in enquiry>SLV.E.STMT.ENTRY
**** Colum details - field>EXTRA.NAAR - Column>34,+2
**** Colum details - field>NARRATIVE - Column>34,+1
        ;*Y.NARR.DESC = NARR
        Y.NARR.DESC = ''
        ;*NARR = ''
    END
    RETURN 
*** </region>

*-----------------------------------------------------------------------------

*** <region name= CHECK.TFS>
CHECK.TFS:
*
*** <desc>Check the TFS for primary account </desc>
    IF Y.TFS.ID.TEMPS EQ '' THEN
        Y.TFS.ID.TEMPS = Y.TFS.ID
        YTFS.NEW.FLAG = 1
        Y.TEMP.ARR.TFS = ''
        Y.REV.FLAG = ''
        Y.CHQ.FLAG = ""
        Y.NON.CHQ.FLAG = ""
    END ELSE
        YTFS.NEW.FLAG = ''
        IF Y.TFS.ID.TEMPS NE Y.TFS.ID THEN
            YTFS.NEW.FLAG = 1
            ;*            * 20150805N - S *When the last TFS is non chq and next is TFS transaction.
            ;*            IF Y.NON.CHQ.FLAG THEN
            ;*                GOSUB APPEND.NON.CHQ.ARR
            ;*            END
            ;*            Y.CHQ.FLAG = ""
            ;*            Y.NON.CHQ.FLAG = ''
            ;*            Y.NON.CHQ.AMT.LCY = ""
            ;*            Y.NON.CHQ.AMT.FCY = ""
            ;*            * 20150805N - E
            Y.TFS.ID.TEMPS = Y.TFS.ID
            Y.RECORD.STATUS.TEMP = ''
            Y.REV.FLAG = ''
        END ELSE
            YTFS.NEW.FLAG = ''
            Y.CHQ.FLAG = ""
        END
    END

    Y.TFS.FLAG = ''
    R.TFS = ''
    TFS.ERR = ''
    CALL F.READ(FN.TFS,Y.TFS.ID,R.TFS,F.TFS,TFS.ERR)
    IF NOT(R.TFS) THEN
        R.TFS = ''
        TFS.ERR = ''
        Y.TFS.ID.HIS.TEMP = ''
        Y.TFS.ID.HIS.TEMP = Y.TFS.ID
        CALL EB.READ.HISTORY.REC(F.TFS.HIS,Y.TFS.ID,R.TFS,TFS.ERR)
        Y.TFS.ID = Y.TFS.ID.HIS.TEMP
    END

    IF R.TFS THEN
        Y.PRIMARY.ACCOUNT = '' ; Y.UNDERLYINGS = ''; TXN.REF.POS = ''
        Y.PRIMARY.ACCOUNT = R.TFS<TFS.PRIMARY.ACCOUNT>
        Y.RECORD.STATUS = R.TFS<TFS.RECORD.STATUS>
        TFS.TIPO.TRANS = R.TFS<TFS.TRANSACTION> ;*magarcia
        TFS.TT.TRANS   = R.TFS<TFS.UNDERLYING>  ;*magarcia
        GOSUB CHECK.REVERSAL.FLG ;*Check the reversal marker for reversal records

        Y.PRM.FLG = ''
        IF Y.PRIMARY.ACCOUNT EQ ACCT.NO THEN
            Y.PRM.FLG = 1
        END

        IF Y.PRIMARY.ACCOUNT EQ '' THEN
            Y.UNDERLYINGS = R.TFS<TFS.UNDERLYING>
            Y.UNDER.CNT = ''
            Y.UNDER.CNT = DCOUNT(Y.UNDERLYINGS,VM)
            IF Y.UNDER.CNT EQ '1' THEN
                Y.PRM.FLG = 1
            END
        END

        IF Y.PRM.FLG THEN
            Y.UNDERLYINGS = R.TFS<TFS.UNDERLYING>
*-----------------------------------------------------------------------------
            Y.STMT.TRANS.REFERENCE = FIELD(Y.STMT.TRANS.REFERENCE,'\',1)
*-----------------------------------------------------------------------------
            LOCATE Y.STMT.TRANS.REFERENCE IN Y.UNDERLYINGS<1,1> SETTING TXN.REF.POS THEN
            Y.TFS.FLAG = '1'
            GOSUB PROCESS.TFS ;*
        END
    END ELSE
        IF Y.TEMP.ARR.TFS THEN
            GOSUB UPDATE.MAIN.ARR
        END
    END
    END

    RETURN
*** </region>
*-----------------------------------------------------------------------------

*** <region name= PROCESS.TFS>
PROCESS.TFS:
*** <desc> </desc>
    IF Y.TFS.FLAG THEN
        GOSUB CALC.TFS.AMOUNT ;*Calculate TFS amount
        Y.STMT.TRANS.REFERENCE.TEMP = Y.STMT.THEIR.REF
        Y.BASE.APP = 'TELLER.FINANCIAL.SERVICES'
        IF NOT(Y.RECORD.STATUS) AND Y.STMT.RECORD.STATUS THEN
            Y.RECORD.STATUS = Y.STMT.RECORD.STATUS
        END
*** Change 24/04/15
        Y.STMT.TRANS.REFERENCE = FIELD(Y.STMT.TRANS.REFERENCE,'\',1)
        IF Y.STMT.BOOKING.DATE THEN
            Y.STMT.BOOKING.DATE = OCONV(ICONV(Y.STMT.BOOKING.DATE,"D4"),"D4/E")
        END
        IF Y.STMT.VALUE.DATE THEN
            Y.STMT.VALUE.DATE = OCONV(ICONV(Y.STMT.VALUE.DATE,"D4"),"D4/E")
        END
*** Change 24/04/15
 
        ;*@Author: Jorge Henriquez 10.01.18
        ;*Se realiza validacion para identificar cuando la txn es liberacion de cheque ajeno y ocultar movimiento Credito/Debito en estado de cuenta
        ;*------------------------------------------------------------------------------------------------------------------------------------- 
        NO.RECORD.STMT.CHQ.COL = 0
        IF Y.TXN.CODE EQ 92 THEN	          
	        STMT.CHQ.COL = "SELECT ":FN.CHQ.COL:" WITH TXN.ID EQ " : Y.STMT.TRANS.REFERENCE
	        CALL EB.READLIST (STMT.CHQ.COL, ID.STMT.CHQ.COL, '', NO.RECORD.STMT.CHQ.COL, SYSTEM.ERR.STMT.CHQ.COL)                
        END 
         
        IF NO.RECORD.STMT.CHQ.COL EQ 0 THEN
            IF NARR THEN
                Y.TRANS.NARR.DESC = Y.TRANS.NARR.DESC:VM:NARR 
                NARR = ''
            END 

            GOSUB PROCS.TFS.ARR
        END
	    ELSE IF Y.STMT.AMOUNT.LCY.TEMP GT 0 THEN
	        ;*Se ejecuta nuevamente el GOSUB GET.NARRATIVE para obtener el valor de la variable NARR
	        ;*--------------------------------------------------------------------------------------
	       	 	GOSUB GET.NARRATIVE
	       	 	
	        ;*Se verifique que la TFS ya existe en el ARRAY para que no se realize suma del monto duplicando el monto real de la txn
	        ;*Se crea una array de las TT que corresponden a cheques ajenos y se valida que la TT no exista en el array para que no repita la txn
	        ;*-------------------------------------------------------------------------------------------------------------------
	        FINDSTR Y.STMT.ID IN Y.CURR.TEMP.ARR.TFS SETTING Ap,Vp ELSE
	          
	         CALL F.READ(FN.STMT.ENTRY, Y.STMT.ID, RECORD.STMT.ENTRY, F.STMT.ENTRY, ERROR.STMT.ENTRY)
	         Y.ID.TT = RECORD.STMT.ENTRY<AC.STE.TRANS.REFERENCE>
	         
		         FINDSTR Y.ID.TT IN Y.TEMP.ARR.TT SETTING Ap,Vp ELSE
		         ;*Si no existe la TT, se ingresa la TT al array
		             Y.TEMP.ARR.TT<-1> = Y.ID.TT
		            GOSUB PROCS.TFS.ARR
		         END
	        END
	    END
    END
    RETURN
*** </region>
*-----------------------------------------------------------------------------
**
*** <region name=separacion Comision - IVA >
COMISION.IVA.CHQ.BREAK.OFF:

    DESCRIPCION.IVA.COMI = 'IVA por Liberacion de Fondos'
    Y.APP = SUBSTRINGS(Y.STMT.THEIR.REF,1,3)
    IF Y.TAX.AMT.CHG LT 0 AND Y.APP EQ 'CHG'  THEN
        Y.TEMP.ARR<-1> = Y.STMT.ID:'*':Y.ACCT.ID:'*':Y.ACCOUNT.NUMBER:'*':Y.STMT.CURRENCY:'*':Y.STMT.COMPANY
        Y.TEMP.ARR:='*':Y.TAX.AMT.CHG:'*':Y.STMT.AMOUNT.FCY:'*':DESCRIPCION.IVA.COMI:'*':Y.STMT.VALUE.DATE
        Y.TEMP.ARR:='*':Y.STMT.THEIR.REF:'*':Y.STMT.BOOKING.DATE:'*':Y.STMT.TRANS.REFERENCE:'*':Y.STMT.NARRATIVE
        Y.TEMP.ARR:='*':Y.STMT.SYSTEM.ID:'*':Y.BASE.APP:'*':Y.OPEN.BAL:'*':Y.NARR.DESC:'*':Y.RECORD.STATUS:"*":Y.HORA.TIME:"*":Y.CHQ.NUM
    END
    RETURN
*** </region>

PROCS.TFS.ARR:
*** </region>
**Chnages for the CR 25/08/2015.

    Y.CURR.TEMP.ARR.TFS = Y.STMT.ID:'*':Y.ACCT.ID:'*':Y.ACCOUNT.NUMBER:'*':Y.STMT.CURRENCY:'*':Y.STMT.COMPANY
    Y.CURR.TEMP.ARR.TFS:='*':Y.NON.CHQ.AMT.LCY:'*':Y.NON.CHQ.AMT.FCY:'*':Y.TRANS.NARR.DESC:'*':Y.STMT.VALUE.DATE
    Y.CURR.TEMP.ARR.TFS:='*':Y.STMT.THEIR.REF:'*':Y.STMT.BOOKING.DATE:'*':Y.STMT.TRANS.REFERENCE.TEMP:'*':Y.STMT.NARRATIVE
    Y.CURR.TEMP.ARR.TFS:='*':Y.STMT.SYSTEM.ID:'*':Y.BASE.APP:'*':Y.OPEN.BAL:'*':Y.NARR.DESC:'*' :Y.RECORD.STATUS:"*":Y.HORA.TIME:"*":""
;* 20150805N - S
    IF Y.STMT.AMOUNT.LCY.TEMP OR Y.STMT.AMOUNT.FCY.TEMP THEN
        ;*IF YTFS.NEW.FLAG OR Y.CHQ.NUM THEN
        ;*20150805N - E
        IF Y.TEMP.ARR.TFS EQ '' THEN
            Y.TEMP.ARR.TFS = Y.STMT.ID:'*':Y.ACCT.ID:'*':Y.ACCOUNT.NUMBER:'*':Y.STMT.CURRENCY:'*':Y.STMT.COMPANY
            Y.TEMP.ARR.TFS:='*':Y.STMT.AMOUNT.LCY.TEMP:'*':Y.STMT.AMOUNT.FCY.TEMP:'*':Y.TRANS.NARR.DESC:'*':Y.STMT.VALUE.DATE
            Y.TEMP.ARR.TFS:='*':Y.STMT.THEIR.REF:'*':Y.STMT.BOOKING.DATE:'*':Y.STMT.TRANS.REFERENCE.TEMP:'*':Y.STMT.NARRATIVE
            Y.TEMP.ARR.TFS:='*':Y.STMT.SYSTEM.ID:'*':Y.BASE.APP:'*':Y.OPEN.BAL:'*':Y.NARR.DESC:'*':Y.RECORD.STATUS:"*":Y.HORA.TIME:"*":Y.CHQ.NUM
        END ELSE
            ;*IF Y.CHQ.FLAG THEN
            Y.TEMP.ARR.TFS:=FM:Y.STMT.ID:'*':Y.ACCT.ID:'*':Y.ACCOUNT.NUMBER:'*':Y.STMT.CURRENCY:'*':Y.STMT.COMPANY
            Y.TEMP.ARR.TFS:='*':Y.STMT.AMOUNT.LCY.TEMP:'*':Y.STMT.AMOUNT.FCY.TEMP:'*':Y.TRANS.NARR.DESC:'*':Y.STMT.VALUE.DATE
            Y.TEMP.ARR.TFS:='*':Y.STMT.THEIR.REF:'*':Y.STMT.BOOKING.DATE:'*':Y.STMT.TRANS.REFERENCE.TEMP:'*':Y.STMT.NARRATIVE
            Y.TEMP.ARR.TFS:='*':Y.STMT.SYSTEM.ID:'*':Y.BASE.APP:'*':Y.OPEN.BAL:'*':Y.NARR.DESC:'*':Y.RECORD.STATUS:"*":Y.HORA.TIME:"*":Y.CHQ.NUM
            ;*END
        END
        ;*Y.CHQ.FLAG = ""
        ;*END
    END
;*    IF Y.LAST.STMT EQ YID.STMT THEN
;*        IF Y.NON.CHQ.FLAG THEN
;*            GOSUB APPEND.NON.CHQ.ARR
;*            Y.NON.CHQ.FLAG = ''
;*        END
;*    END
*CRT Y.TEMP.ARR.TFS
    RETURN
*-----------------------------------------------------------------------------
* 20150805N - S
APPEND.NON.CHQ.ARR:

    Y.TOT.ARR.CNT = ''
*   IF Y.SUM.FLAG THEN
    Y.TOT.ARR.CNT = DCOUNT(Y.TEMP.ARR.TFS,FM) + 1
    Y.TEMP.ARR.TFS<Y.TOT.ARR.CNT> =Y.CURR.TEMP.ARR.TFS
    Y.CURR.TEMP.ARR.TFS = '' 

    RETURN
*-----------------------------------------------------------------------------
*** <region name= CALC.TFS.AMOUNT>
CALC.TFS.AMOUNT:
*** <desc>Calculate TFS amount </desc>

*    IF Y.CHQ.NUM THEN
    Y.STMT.AMOUNT.LCY.TEMP= Y.STMT.AMOUNT.LCY
    Y.STMT.AMOUNT.FCY.TEMP= Y.STMT.AMOUNT.FCY
*        Y.CHQ.FLAG = "1"
*    END ELSE
*        Y.NON.CHQ.AMT.LCY +=  Y.STMT.AMOUNT.LCY
*        Y.NON.CHQ.AMT.FCY += Y.STMT.AMOUNT.FCY
*        Y.NON.CHQ.FLAG = "1"
*    END

    IF Y.STMT.AMOUNT.LCY[1,1] EQ '-' THEN
        ;*    	Y.TRANS.NARR.DESC = 'Retiro en cuenta' ;*--- magarcia
        IF TFS.TIPO.TRANS EQ PAGO.CH.PROPIO THEN ;* Se agrega nueva funcionalidad para Pago de Cheque Propio
            CALL F.READ(FN.TELLER, TFS.TT.TRANS, R.TELLER, F.TELLER, F.ERR.TELLER)
            TT.CODE.TRANS = R.TELLER<TT.TE.TRANSACTION.CODE>
            CALL F.READ(FN.TELLER.TRANS, TT.CODE.TRANS, R.TELLER.TRANS, F.TELLER.TRANS, F.ERR.TELLER)
            Y.TXN.CODE = R.TELLER.TRANS<TT.TR.TRANSACTION.CODE.2>
            ES.CHQ.PRP = 1
            GOSUB READ.TRANSACTION
        END										;*--- magarcia
    END ELSE
        Y.TRANS.NARR.DESC = 'Deposito en cuenta'
    END
    GOSUB GET.ACC.PRODUCT ; *
    RETURN
*** </region>
* 20150805N - E
*-----------------------------------------------------------------------------

*** <region name= READ.TRANSACTION>
READ.TRANSACTION:
*** <desc>Read the transaction to get the narrative,. this change is included for TFS display. </desc>
*** Removed conversion for the field TRANS.NARRATIVE
*** Conversion field >L TRANSACTION,NARRATIVE

    R.TRANSACTION = ''
    TRANSACTION.ERR = ''
    CALL F.READ(FN.TRANSACTION,Y.TXN.CODE,R.TRANSACTION,F.TRANSACTION,TRANSACTION.ERR)
    IF R.TRANSACTION THEN
        Y.TRANS.NARR.DESC=R.TRANSACTION<AC.TRA.NARRATIVE>
    END
    RETURN
*** </region>


*----------------------------------------------------------------------------
*** <region name= PROCESS.FT>
PROCESS.FT:
*** <desc>Read the FT,get the transaction type and pass the description oly for ACRC </desc>
    R.FUNDS.TRANSFER = ''
    FUNDS.TRANSFER.ERR = ''
    LF.TCE.NARR			= ''
    Y.FT.TXN.TYPE = ''
;*ft de bnk
    Y.FT.ID = FIELD(Y.FT.ID,'\',1);
    CALL F.READ(FN.FUNDS.TRANSFER,Y.FT.ID,R.FUNDS.TRANSFER,F.FUNDS.TRANSFER,FUNDS.TRANSFER.ERR)
    IF NOT(R.FUNDS.TRANSFER) THEN
        Y.ERR = ''
        CALL EB.READ.HISTORY.REC (F.FUNDS.TRANSFER$HIS,Y.FT.ID,R.FUNDS.TRANSFER,Y.ERR)
    END
    IF R.FUNDS.TRANSFER THEN
        Y.FT.TXN.TYPE = R.FUNDS.TRANSFER<FT.TRANSACTION.TYPE>
        Y.RECORD.STATUS = R.FUNDS.TRANSFER<FT.RECORD.STATUS>
        Y.DEBIT.AMOUNT = R.FUNDS.TRANSFER<FT.DEBIT.AMOUNT>
        Y.DEBIT.AMOUNT = Y.DEBIT.AMOUNT * -1
        Y.INWARD.PAY.TYPE = R.FUNDS.TRANSFER<FT.INWARD.PAY.TYPE>

        IF Y.FT.TXN.TYPE EQ 'ACRC' THEN
            GOSUB GET.FT.TXN.TYP.DESC ; *
        END

        ;*@Author:Ronald Ortiz
        ;*@Date: 20170514
        IF (Y.FT.TXN.TYPE EQ ABONO.COLECTOR) THEN

            IF R.FUNDS.TRANSFER<FT.ORDERING.CUST> EQ 'RENT' AND R.FUNDS.TRANSFER<FT.CREDIT.ACCT.NO> EQ ACCT.NO THEN
                ;*Consultamos el narrative configurado en la aplicación EB.SLV.KEYS.PARAM
                ID.RECORD.NARR = ABONO.COLECTOR:'.':R.FUNDS.TRANSFER<FT.ORDERING.CUST>:'.':'CREDIT'
                CALL F.READ(FN.EB.SLV.KEYS.PARAMS,ID.RECORD.NARR,RECORD.APP,F.EB.SLV.KEYS.PARAMS,ERR.KEYS.PARAM)
                ;*Y.TRANS.NARR.DESC = 'Abono Rentabilidad dejada de percibir'
                Y.TRANS.NARR.DESC = RECORD.APP<EB.SLV18.DESCRIPCION>
                ;*NARR = 'AFP Crecer'
            END
            ELSE IF R.FUNDS.TRANSFER<FT.CREDIT.ACCT.NO> EQ ACCT.NO AND R.FUNDS.TRANSFER<FT.ORDERING.CUST> NE 'RENT' AND  R.FUNDS.TRANSFER<FT.ORDERING.BANK> NE 'ABONOMANUALVS' THEN
            RECORD.ABONO.APP = ABONO.COLECTOR:'.ABN.CREDIT'
            GOSUB GET.NARRATIVE.ABONO.COLECTOR
            DESC.ABN = CHANGE(NARRATIVE.ABN,"[CUENTA.DEBITO]",R.FUNDS.TRANSFER<FT.DEBIT.ACCT.NO>)

            FINDSTR "~" IN DESC.ABN SETTING Ap,Vp THEN
                CANTIDAD.LINEAS.ABN = DCOUNT(DESC.ABN,'~')

                IF CANTIDAD.LINEAS.ABN EQ 2 THEN
                    Y.TRANS.NARR.DESC = FIELD(DESC.ABN,'~',1)
                    NARR = FIELD(DESC.ABN,'~',2)
                END
                ELSE
                Y.TRANS.NARR.DESC = DESC.ABN
            END

        END ;*END FINDSTR

        ;*Y.TRANS.NARR.DESC = 'Transferencia de fondos'
        ;*NARR = 'Desde cuenta No.':R.FUNDS.TRANSFER<FT.DEBIT.ACCT.NO>
    END
    ELSE IF R.FUNDS.TRANSFER<FT.DEBIT.ACCT.NO> EQ ACCT.NO AND R.FUNDS.TRANSFER<FT.ORDERING.CUST> NE 'RENT' AND  R.FUNDS.TRANSFER<FT.ORDERING.BANK> NE 'ABONOMANUALVS' THEN
    RECORD.ABONO.APP = ABONO.COLECTOR:'.ABN.DEBIT'
    GOSUB GET.NARRATIVE.ABONO.COLECTOR
    DESC.ABN = CHANGE(NARRATIVE.ABN,"[CUENTA.CREDITO]",R.FUNDS.TRANSFER<FT.CREDIT.ACCT.NO>)

    FINDSTR "~" IN DESC.ABN SETTING Ap,Vp THEN
        CANTIDAD.LINEAS.ABN = DCOUNT(DESC.ABN,'~')

        IF CANTIDAD.LINEAS.ABN EQ 2 THEN
            Y.TRANS.NARR.DESC = FIELD(DESC.ABN,'~',1)
            NARR = FIELD(DESC.ABN,'~',2)
        END
        ELSE
        Y.TRANS.NARR.DESC = DESC.ABN
    END

    END ;*END FINDSTR

;*Y.TRANS.NARR.DESC = 'Transferencia de fondos'
;*NARR = 'A cuenta No.':R.FUNDS.TRANSFER<FT.CREDIT.ACCT.NO>
    END
    ELSE IF R.FUNDS.TRANSFER<FT.DEBIT.ACCT.NO> EQ ACCT.NO AND R.FUNDS.TRANSFER<FT.ORDERING.CUST> EQ 'ABONOMANUALVS' THEN
    ID.RECORD.DEBIT.PARAM = 'AC66.ABONO.MANUAL.DEBIT'
    CALL F.READ(FN.EB.SLV.KEYS.PARAMS,ID.RECORD.DEBIT.PARAM,RESPONSE.KEYS.PARAM.DEBIT,F.EB.SLV.KEYS.PARAMS,ERR.KEYS.PARAMS.DEBIT)
    DESCRIPTION.PARAM.DB = RESPONSE.KEYS.PARAM.DEBIT<EB.SLV18.DESCRIPCION>
    CALL GET.LOC.REF('FUNDS.TRANSFER','LF.ID.COL',PsColector)
    COLECTOR = R.FUNDS.TRANSFER<FT.LOCAL.REF><1,PsColector>
    Y.TRANS.NARR.DESC = CHANGE(DESCRIPTION.PARAM.DB,"[NOMBRECOLECTOR]",FIELD(COLECTOR,'-',2))
    NARR = ''
    END
    ELSE IF R.FUNDS.TRANSFER<FT.CREDIT.ACCT.NO> EQ ACCT.NO AND R.FUNDS.TRANSFER<FT.ORDERING.CUST> EQ 'ABONOMANUALVS' THEN
    ID.RECORD.CREDIT.PARAM = 'AC66.ABONO.MANUAL.CREDIT'
    CALL F.READ(FN.EB.SLV.KEYS.PARAMS,ID.RECORD.CREDIT.PARAM,RESPONSE.KEYS.PARAM.CREDIT,F.EB.SLV.KEYS.PARAMS,ERR.KEYS.PARAMS.DEBIT)
    DESCRIPTION.PARAM.CR = RESPONSE.KEYS.PARAM.CREDIT<EB.SLV18.DESCRIPCION>
    CALL GET.LOC.REF('FUNDS.TRANSFER','LF.ID.COL',PsColectorCr)
    COLECTOR.CR = R.FUNDS.TRANSFER<FT.LOCAL.REF><1,PsColectorCr>
    Y.TRANS.NARR.DESC = CHANGE(DESCRIPTION.PARAM.CR,"[NOMBRECOLECTOR]",FIELD(COLECTOR.CR,'-',2))
    NARR = ''
    END
;*NARR = ''
    END

;*@Author:Ronald Ortiz
    CALL GET.LOC.REF('FUNDS.TRANSFER','LF.ID.COL.MOD',POS.COD.MOD)
    CODIGO.MOD.COL = R.FUNDS.TRANSFER<FT.LOCAL.REF, POS.COD.MOD>

    CALL GET.LOC.REF('FUNDS.TRANSFER','LF.COD.CL', POS.CL.PEX)
    CODIGO.COLECTOR.PX = R.FUNDS.TRANSFER<FT.LOCAL.REF, POS.CL.PEX>

*** <region name= DESC PAGO COLECTOR>
    IF Y.FT.TXN.TYPE EQ PAGO.COLECTOR AND CODIGO.COLECTOR.PX EQ '' AND R.FUNDS.TRANSFER<FT.CREDIT.ACCT.NO> EQ ACCT.NO THEN
        TIPO.MOVIMIENTO = 'CREDIT'
        RESPUESTA.RUTINA.DESC = ''
        CALL SLV.GENERATE.DESCRIPTION.TRX(PAGO.COLECTOR,Y.FT.ID,TIPO.MOVIMIENTO,RESPUESTA.RUTINA.DESC)

        FINDSTR "~" IN RESPUESTA.RUTINA.DESC SETTING Ap,Vp THEN
            CANTIDAD.LINEAS = DCOUNT(RESPUESTA.RUTINA.DESC,'~')

            IF CANTIDAD.LINEAS EQ 2 THEN
                Y.TRANS.NARR.DESC = FIELD(RESPUESTA.RUTINA.DESC,'~',1)
                NARR = FIELD(RESPUESTA.RUTINA.DESC,'~',2)
                ;*NARR = 'Corporativo ':FIELD(RESPUESTA.RUTINA.DESC,'~',2)
            END
        END
        ELSE
        Y.TRANS.NARR.DESC = RESPUESTA.RUTINA.DESC
        NARR = ''
    END

    END ;*FIN DE IF
    ELSE IF Y.FT.TXN.TYPE EQ PAGO.COLECTOR AND CODIGO.COLECTOR.PX EQ '' AND R.FUNDS.TRANSFER<FT.DEBIT.ACCT.NO> EQ ACCT.NO THEN

    TIPO.MOVIMIENTO = 'DEBIT'
    RESPUESTA.RUTINA.DESC = ''
    CALL SLV.GENERATE.DESCRIPTION.TRX(PAGO.COLECTOR,Y.FT.ID,TIPO.MOVIMIENTO,RESPUESTA.RUTINA.DESC)

    FIND "~" IN RESPUESTA.RUTINA.DESC SETTING Ap,Vp THEN
    CANTIDAD.LINEAS = DCOUNT(RESPUESTA.RUTINA.DESC,'~')

    IF CANTIDAD.LINEAS EQ 2 THEN
        Y.TRANS.NARR.DESC = FIELD(RESPUESTA.RUTINA.DESC,'~',1)
        NARR = FIELD(RESPUESTA.RUTINA.DESC,'~',2)
    END ;* FIN DE IF
    END
    ELSE
;*@Author:Ronald Ortiz
    Y.TRANS.NARR.DESC = RESPUESTA.RUTINA.DESC
    NARR = ''
    END  ;*END ELSE
    END ;*FIN DE ELSE IF
    ELSE IF Y.FT.TXN.TYPE EQ PAGO.COLECTOR AND CODIGO.COLECTOR.PX NE '' AND CODIGO.MOD.COL EQ '' AND R.FUNDS.TRANSFER<FT.DEBIT.ACCT.NO> EQ ACCT.NO THEN
    ID.NARR.RECORD.PEX = PAGO.COLECTOR:'.PEX.DEBIT'
    CALL F.READ(FN.EB.SLV.KEYS.PARAMS,ID.NARR.RECORD.PEX,RS.KEYS.PEX,F.EB.SLV.KEYS.PARAMS,ERR.NARR.PEX)
    Y.TRANS.NARR.DESC = RS.KEYS.PEX<EB.SLV18.DESCRIPCION>
;*Y.TRANS.NARR.DESC = 'Pago por Banca en Linea'
    CALL F.READ(FN.EB.SLV.COLECTOR,CODIGO.COLECTOR.PX,RESPONSE.APP.PEX,F.EB.SLV.COLECTOR,ERR.COLECTOR.PEX)
    NARR = RESPONSE.APP.PEX<EB.CL.NOMBRE.COLECTOR>
*		      NARR = '[NOMBRECOLECTOR]'
    END
*** <desc> </desc>  

*** <region name= DESC CARGO.COLECTOR>
    ORDERING.CUST = R.FUNDS.TRANSFER<FT.ORDERING.CUST>

    IF Y.FT.TXN.TYPE EQ CARGO.COLECTOR AND CODIGO.COLECTOR.PX NE '' AND R.FUNDS.TRANSFER<FT.CREDIT.ACCT.NO> EQ ACCT.NO THEN
        TIPO.MOVIMIENTO 		= 'CREDIT'
        RESPUESTA.RUTINA.DESC 	= ''
        ID.TRX.TYPE 			= ''
        IF ORDERING.CUST EQ 'CLTRREINTEGRO' THEN
            ID.TRX.TYPE = CARGO.COLECTOR:'REINTEGRO'
        END
        ELSE IF ORDERING.CUST EQ 'CLTRABONO' THEN
	        ID.TRX.TYPE = CARGO.COLECTOR
	    END
	    
	    CALL SLV.GENERATE.DESCRIPTION.TRX(ID.TRX.TYPE,Y.FT.ID,TIPO.MOVIMIENTO,RESPUESTA.RUTINA.DESC)
	    CANTIDAD.LINEAS = DCOUNT(RESPUESTA.RUTINA.DESC,'~')
	    IF CANTIDAD.LINEAS EQ 2 THEN
	        Y.TRANS.NARR.DESC	= FIELD(RESPUESTA.RUTINA.DESC,'~',1)
	        NARR 				= FIELD(RESPUESTA.RUTINA.DESC,'~',2)
	    END
	    ELSE
		    Y.TRANS.NARR.DESC 	= RESPUESTA.RUTINA.DESC
		    NARR 				= ''
	    END

    END ;*FIN DE IF
    ELSE IF Y.FT.TXN.TYPE EQ CARGO.COLECTOR AND CODIGO.COLECTOR.PX NE '' AND R.FUNDS.TRANSFER<FT.DEBIT.ACCT.NO> EQ ACCT.NO THEN
	    TIPO.MOVIMIENTO 		= 'DEBIT'
	    RESPUESTA.RUTINA.DESC 	= ''
	    ID.TRX.TYPE 			= ''
	    IF ORDERING.CUST EQ 'CLTRREINTEGRO' THEN
	        ID.TRX.TYPE = CARGO.COLECTOR:'REINTEGRO'
	    END
	    ELSE IF ORDERING.CUST EQ 'CLTRABONO' THEN
	    	ID.TRX.TYPE = CARGO.COLECTOR
	    END
	
	    CALL SLV.GENERATE.DESCRIPTION.TRX(ID.TRX.TYPE,Y.FT.ID,TIPO.MOVIMIENTO,RESPUESTA.RUTINA.DESC)
	    CANTIDAD.LINEAS = DCOUNT(RESPUESTA.RUTINA.DESC,'~')
	    IF CANTIDAD.LINEAS EQ 2 THEN
	        Y.TRANS.NARR.DESC 	= FIELD(RESPUESTA.RUTINA.DESC,'~',1)
	        NARR 				= FIELD(RESPUESTA.RUTINA.DESC,'~',2)
	    END
	    ELSE
		    Y.TRANS.NARR.DESC 	= RESPUESTA.RUTINA.DESC
		    NARR 				= ''
	    END 
    END 
*** <desc> </desc>

*** <region name= DESC CARGO.COMISION>
    IF Y.FT.TXN.TYPE EQ CARGO.COMISION AND R.FUNDS.TRANSFER<FT.DEBIT.ACCT.NO> EQ ACCT.NO THEN
        ID.RECORD.PARAM = CARGO.COMISION:'.DEBIT'
        CALL F.READ(FN.EB.SLV.KEYS.PARAMS,"AC68.DEBIT",RS.KEYS.COMM,F.EB.SLV.KEYS.PARAMS,ERR.APP.KEYS.COMM)
        Y.TRANS.NARR.DESC = RS.KEYS.COMM<EB.SLV18.DESCRIPCION>
        NARR = ''
    END
*** <desc> </desc>

;*Se reemplaza IF por FIND porque es mas sostenible : OCORNEJO 25.05.2017
    FIND Y.FT.TXN.TYPE IN TXN.ARR SETTING Ap, Vp THEN
;*IF (Y.FT.TXN.TYPE EQ NOTA.ABONO) OR (Y.FT.TXN.TYPE EQ NOTA.CARGO) THEN ;*magarcia
    FT.NOTA.DESC = R.FUNDS.TRANSFER<FT.ORDERING.BANK>
    GOSUB GET.FT.TXN.TYP.DESC
    NARR = FT.NOTA.DESC
    END

;*iTurcios: Txn desde TCE
    CALL GET.LOC.REF('FUNDS.TRANSFER','LF.TCE.NARR', POS.LF.TEC.NARR)
    LF.TCE.NARR	= R.FUNDS.TRANSFER<FT.LOCAL.REF, POS.LF.TEC.NARR>
    LF.TXN.BP 	= '' ;*Tipo de Txn de Pago Masivo desde Banca Empresas

    IF Y.INWARD.PAY.TYPE THEN ;*Proviene de Bulk Payments
        GOSUB GET.FT.BULK.MASTER ;*LF.TXN.BP con salida
    END

    IF (LF.TCE.NARR OR LF.TXN.BP) AND Y.FT.TXN.TYPE NE 'AC64' THEN
        IF Y.DEBIT.AMOUNT EQ Y.STMT.AMOUNT.LCY THEN
            GOSUB GET.FT.KEYS.PARMS.DESC
        END
    END
;*@Author:Ronald Ortiz
;*@Date:20170619
    ELSE IF LF.TCE.NARR AND Y.FT.TXN.TYPE EQ 'AC64' THEN
    ID.RECORD.FOOTER = 'FOOTER.COLLECTOR.TCE'
    CALL F.READ(FN.EB.SLV.KEYS.PARAMS,ID.RECORD.FOOTER,RS.NARR.FOOTER,F.EB.SLV.KEYS.PARAMS,ERR.NARR.FOOTER)
    NARR.TCE = RS.NARR.FOOTER<EB.SLV18.DESCRIPCION>

    IF NARR EQ '' THEN
        ;*Y.TRANS.NARR.DESC = Y.TRANS.NARR.DESC
        CALL F.READ(FN.EB.SLV.COLECTOR,CODIGO.COLECTOR.PX,RESPONSE.APP.PEX,F.EB.SLV.COLECTOR,ERR.COLECTOR.PEX)
		COLECTOR.NAME = RESPONSE.APP.PEX<EB.CL.NOMBRE.COLECTOR>
        DESCRIPTION = COLECTOR.NAME :NARR.TCE
        NARR = NARR.TCE
        ;*NARR = '-Empresas'
    END
    ELSE
	    DESCRIPTION = NARR :NARR.TCE
        ;*Y.TRANS.NARR.DESC = Y.TRANS.NARR.DESC:VM:NARR:VM:'Empresas'
        NARR = DESCRIPTION
        ;*NARR = NARR : '-Empresas'
    END
    END
    END
    
    RETURN
*** </region>
*-----------------------------------------------------------------------------

*** <region name= GET.FT.TXN.TYP.DESC>
GET.FT.TXN.TYP.DESC:
*** <desc> </desc>

    R.FT.TXN.TYPE.CONDITION = ''
    FT.TXN.TYPE.CONDITION.ERR = ''
    Y.FT.TRANS.NARR.DESC = ''
    CALL F.READ(FN.FT.TXN.TYPE.CONDITION,Y.FT.TXN.TYPE,R.FT.TXN.TYPE.CONDITION,F.FT.TXN.TYPE.CONDITION,FT.TXN.TYPE.CONDITION.ERR)
    IF R.FT.TXN.TYPE.CONDITION THEN
        Y.TRANS.NARR.DESC = R.FT.TXN.TYPE.CONDITION<FT6.DESCRIPTION>
    END
    RETURN
*** </region>
*-----------------------------------------------------------------------------

*** <region name= Check Account No>
CHECK.ACCT.NO:
*** <desc> </desc>
    Y.NO.ACCT.FLG = ''
    R.ACCOUNT.REC = '' ; ACCT.ERR = '' ; Y.CUSTOMER = ''
    CALL F.READ(FN.ACCOUNT, ACCT.NO, R.ACCOUNT.REC, F.ACCOUNT, ACCT.ERR)
    IF NOT(R.ACCOUNT.REC) THEN
        FN.ACCOUNT.HIS = 'F.ACCOUNT$HIS'
        F.ACCOUNT.HIS = ''
        CALL OPF(FN.ACCOUNT.HIS,F.ACCOUNT.HIS)
        ACCT.ERR = ''
        CALL EB.READ.HISTORY.REC(F.ACCOUNT.HIS,ACCT.NO,R.ACCOUNT.REC,ACCT.ERR)
    END
    IF R.ACCOUNT.REC THEN
        Y.ARR.ID = R.ACCOUNT.REC<AC.ARRANGEMENT.ID>
        Y.CUSTOMER = R.ACCOUNT.REC<AC.CUSTOMER>
        Y.CATEGORY = R.ACCOUNT.REC<AC.CATEGORY>
        Y.CURRENCY = R.ACCOUNT.REC<AC.CURRENCY>
        IF NOT(R.ACCOUNT.REC<AC.CUSTOMER>) THEN
            Y.NO.ACCT.FLG = '1'
        END
        R.AA.ARRANGEMENT = '' ; RET.ERROR = ''
        IF R.ACCOUNT.REC<AC.ARRANGEMENT.ID> THEN
            GOSUB CHECK.ARANGEMENT 
        END
    END
    RETURN
*** </region>
*-----------------------------------------------------------------------------

*** <region name= CHECK.ARANGEMENT>
CHECK.ARANGEMENT:
*** <desc> </desc>

    CALL AA.GET.ARRANGEMENT(Y.ARR.ID, R.AA.ARRANGEMENT, RET.ERROR)
    IF R.AA.ARRANGEMENT THEN
        Y.PRD.LINE = R.AA.ARRANGEMENT<AA.ARR.PRODUCT.LINE>
        IF Y.PRD.LINE NE 'ACCOUNTS' THEN
            Y.NO.ACCT.FLG = '1'
        END
    END
    RETURN
*** </region>
*-----------------------------------------------------------------------------


*** <region name= UPDATE.MAIN.ARR>
UPDATE.MAIN.ARR:
**** Changed on 29Apr2015
*** <desc>Check the TFS array and pass it to main array,. this process for single leg account in TFS </desc>
    IF Y.TEMP.ARR EQ '' THEN
        Y.TEMP.ARR = Y.TEMP.ARR.TFS
        Y.TEMP.ARR.TFS = ''
        Y.STMT.AMOUNT.LCY.TEMP = ''
        Y.STMT.AMOUNT.FCY.TEMP = ''
        Y.STMT.TRANS.REFERENCE.TEMP = ''
    END ELSE
        Y.TEMP.ARR<-1>= Y.TEMP.ARR.TFS
        Y.TEMP.ARR.TFS = ''
        Y.STMT.AMOUNT.LCY.TEMP = ''
        Y.STMT.AMOUNT.FCY.TEMP = ''
        Y.STMT.TRANS.REFERENCE.TEMP = ''
    END
    RETURN
*** </region>

*-----------------------------------------------------------------------------

*** <region name= CHECK.REVERSAL.FLG>
CHECK.REVERSAL.FLG:
*** <desc>Check the reversal marker for reversal records </desc>
    IF Y.STMT.REVERSAL.MARKER THEN
        IF Y.TFS.ID.TEMPS EQ Y.TFS.ID AND NOT(Y.REV.FLAG) THEN
            YTFS.NEW.FLAG = 1
            Y.TFS.ID.TEMPS = Y.TFS.ID
            Y.REV.FLAG = 1
        END
    END ELSE
        IF Y.TFS.ID.TEMPS EQ Y.TFS.ID AND Y.REV.FLAG THEN
            YTFS.NEW.FLAG = 1
            Y.TFS.ID.TEMPS = Y.TFS.ID
            Y.REV.FLAG = ''
        END
    END

    RETURN
*** </region>

*-----------------------------------------------------------------------------
*** <region name= CHECK.CHQ.COLL>
CHECK.CHQ.COLL:
*** <desc>check the cheque collection details to hide the mask entries </desc>

    Y.STMT.CHQ.COLL.ID.TEMP = ''
    Y.STMT.CHQ.COLL.ID.TEMP = FIELD(Y.STMT.CHQ.COLL.ID,'-',1)
    COLL.POS = ''
    IF Y.CHQ.COLL.ARR EQ '' THEN
        IF Y.STMT.MASK.PRINT THEN
            CHQ.COLL.FLG = 1
        END ELSE
            CHQ.COLL.FLG = ''
        END
        Y.CHQ.COLL.ARR = Y.STMT.CHQ.COLL.ID.TEMP
    END ELSE
        GOSUB CRS.CHK.CHQ.COLL.ARR 
    END

    RETURN
*** </region>

*-----------------------------------------------------------------------------
*** <region name= CRS.CHK.CHQ.COLL.ARR>
CRS.CHK.CHQ.COLL.ARR:
*** <desc> </desc>
    LOCATE Y.STMT.CHQ.COLL.ID.TEMP IN Y.CHQ.COLL.ARR SETTING COLL.POS THEN
    IF Y.STMT.MASK.PRINT THEN
        CHQ.COLL.FLG = 1
    END ELSE
        CHQ.COLL.FLG = ''
    END
    END ELSE
    CHQ.COLL.FLG = ''
    Y.CHQ.COLL.ARR:=FM:Y.STMT.CHQ.COLL.ID.TEMP
    END

    RETURN
*** </region>

*-----------------------------------------------------------------------------
*** <region name= UPDATE.SPOOL.FILE>
UPDATE.SPOOL.FILE:
*** <desc> </desc>
    Y.TEMP.ARR.PASS = ''
    Y.TEMP.ARR.PASS = Y.TEMP.ARR
    IF Y.TEMP.ARR.PASS THEN
        CHANGE FM TO '~' IN Y.TEMP.ARR.PASS
        CALL SLV.S.ACCT.STMT.PDF.GEN(Y.TEMP.ARR.PASS)
    END

    RETURN
*** </region>
* 20150728P - S
*-----------------------------------------------------------------------------
HORA.EMISION.DATE:

    Y.TIME = TIMEDATE()
    Y.HR = Y.TIME[1,2]
    Y.MIN = Y.TIME[4,2]
    Y.SEC = Y.TIME[7,2]
    Y.MIN.FIN = Y.MIN

    BEGIN CASE
        CASE Y.HR LT '12'
            Y.DAY.TIME = "AM"
            Y.HR.FIN = Y.HR

        CASE Y.HR EQ '12'
            Y.HR.FIN = Y.HR
            Y.DAY.TIME = "PM"

        CASE (Y.HR GT '12') AND (Y.HR NE '24')
            Y.DAY.TIME = "PM"
            Y.HR.FIN = Y.HR - 12

        CASE Y.HR EQ '24'
            Y.HR.FIN = "12"
            Y.DAY.TIME = "AM"

    END CASE

    Y.HR.FIN = FMT(Y.HR.FIN,"R%2")
    Y.MIN.FIN = FMT(Y.MIN.FIN,"R%2")
    Y.HORA.TIME =  Y.HR.FIN :":": Y.MIN.FIN : " " : Y.DAY.TIME

    RETURN
*-----------------------------------------------------------------------------------

*-----------------------------------------------------------------------------

*** <region name= GET.ACC.PRODUCT>
GET.ACC.PRODUCT:
*** <desc>0001 eurias tipo de cuenta</desc>
    TIPO.CUENTA = Y.ACCOUNT.NUMBER
    CALL SLV.UTIL.GET.TIPO.CTA(TIPO.CUENTA)

    IF TIPO.CUENTA EQ 0 THEN 
        TIPO.CUENTA = ' de Ahorro'
    END ELSE 
        IF ES.CHQ.PRP NE 1 THEN ;* --- magarcia
            TIPO.CUENTA = ' Corriente'
        END ELSE
            TIPO.CUENTA = ''
        END                     ;* --- magarcia
    END
    Y.TRANS.NARR.DESC := TIPO.CUENTA
    IF Y.STMT.AMOUNT.LCY[1,1] EQ '-' THEN ;* si es debito verificar si la trx se realizo en version nota de cargo parametrizada en param keys
        VER.VAL = R.TFS<TFS.LOCAL.REF,POS.COD.VER>
        IF VER.VAL THEN
            Y.IDS = R.CAT.VER<EB.SLV18.PARAM.ID>
            FIND VER.VAL IN Y.IDS SETTING POS.VER,POS.VER.1 THEN
            Y.DESCRIPCION = R.CAT.VER<EB.SLV18.DESCRIPCION><POS.VER,POS.VER.1>
            IF Y.DESCRIPCION THEN
                Y.TRANS.NARR.DESC = Y.DESCRIPCION ;* --- magarcia
                ;*Y.TRANS.NARR.DESC =	'DESC.NOTA&':VER.VAL ;* setteo solo el id de la version
            END
        END
    END
    END ELSE
    NARR = ''
    END
    RETURN
*** </region>


*-----------------------------------------------------------------------------

*** <region name= AGRUPAR.NOTA>
AGRUPAR.NOTA:
*** <desc>Agrupar las notas de cargo </desc>
    Y.DETALLE = Y.VAL.ARR
    Y.VAL.ARR = '' ;*reconstruyo el detalle
    PRE.VAL = ''
    Y.IDS.TFS = ''
    Y.IDS.TFS.ANT = '' ;*TFS ANTERIOR
    Y.LINE.ANTERIOR = ''
    Y.MONTO.ANTERIOR = ''
    CONTADOR = 1
    CONVERT VM TO "&&&" IN Y.DETALLE
    LOOP
        REMOVE Y.LINE FROM Y.DETALLE SETTING SEPARADOR.LINE
    WHILE Y.LINE NE ''

        DESCRIPCION = FIELD(Y.LINE,'*',8)
        PRE.VAL = FIELD(DESCRIPCION,'&',1);*DESC.NOTA&2
        Y.IDS.TFS = FIELD(Y.LINE,'*',10)
        Y.OUT = ''

        IF Y.IDS.TFS[1,3] EQ 'TFS' THEN;* es tfs
            IF Y.IDS.TFS EQ Y.IDS.TFS.ANT THEN ;*es el mismo tfs
                IF  PRE.VAL EQ 'DESC.NOTA' THEN;*es nota de cargo
                    *CONVERT '&' TO '###' IN DESCRIPCION;*por si trae complementario con # evaluar solo el val de la parametrizacion
                    POST.VAL = FIELD(DESCRIPCION,'&',2)
                    FIND POST.VAL IN Y.IDS SETTING POS.VER,POS.VER.1 THEN

                    CALL SLV.STMT.ACC.MERGE(Y.LINE.ANTERIOR,Y.LINE,Y.OUT);*crear una sola linea con monto acumulado
                    Y.DESCRIPCION = R.CAT.VER<EB.SLV18.DESCRIPCION><POS.VER,POS.VER.1>
                    Y.LINE = Y.OUT
                    Y.LINE = CHANGE(Y.LINE,DESCRIPCION,Y.DESCRIPCION);*cambio la descripcion por la parametrizada

                    Y.VAL.ARR<CONTADOR-1> = Y.LINE;*es una segunda,tercera etc linea del tfs por tanto sobreescribir la primera
                    CONTADOR--;* si acumulo resto 1 al contador
                END

            END ELSE
                Y.VAL.ARR<-1> = Y.LINE
            END

        END ELSE
            Y.VAL.ARR<-1> = Y.LINE
        END
    END ELSE;*si no es el mismo tfs solo acumular la linea
    Y.VAL.ARR<-1> = Y.LINE
    END
    CONVERT '&' TO VM IN Y.VAL.ARR
    Y.IDS.TFS.ANT = Y.IDS.TFS;*settear los valores de para la siguiente iteracion
    Y.LINE.ANTERIOR = Y.LINE
    CONTADOR++
    REPEAT

    RETURN
*** </region>
*-----------------------------------------------------------------------------

*** <region name= FILL.TEMP.ARR>
FILL.TEMP.ARR:
*** <desc>eurias cambio a gosub </desc>
    Y.TEMP.ARR:='*':Y.STMT.AMOUNT.LCY:'*':Y.STMT.AMOUNT.FCY:'*':Y.TRANS.NARR.DESC:'*':Y.STMT.VALUE.DATE
    Y.TEMP.ARR:='*':Y.STMT.THEIR.REF:'*':Y.STMT.BOOKING.DATE:'*':Y.STMT.TRANS.REFERENCE:'*':Y.STMT.NARRATIVE
    Y.TEMP.ARR:='*':Y.STMT.SYSTEM.ID:'*':Y.BASE.APP:'*':Y.OPEN.BAL:'*':Y.NARR.DESC:'*':Y.RECORD.STATUS:"*":Y.HORA.TIME:"*":Y.CHQ.NUM
    RETURN
*** </region>
*-----------------------------------------------------------------------------


*** <region name=UPDATE.DESCR.CHG> ;* JORGE HENRIQUEZ 21.04.2017
UPDATE.DESCR.CHG:
    Y.APP = SUBSTRINGS(Y.STMT.THEIR.REF,1,3)
    IF Y.APP EQ 'CHG' THEN
        CALL F.READ(FN.CHARGE.REQUEST,Y.STMT.THEIR.REF,RECORD.CHG,F.CHARGE.REQUEST,ERR.CHG)
        Y.TYPE.COMMISSION =  RECORD.CHG<CHG.CHARGE.CODE>

        ;*@Author:Ronald Ortiz
        CALL GET.LOC.REF('AC.CHARGE.REQUEST','LF.COD.CL', POS.CL.CHG)
        CODIGO.COLECTOR.CHG = RECORD.CHG<CHG.LOCAL.REF, POS.CL.CHG>

        IF CODIGO.COLECTOR.CHG NE '' THEN
            CALL SLV.GENERATE.DESCRIPTION.TRX(Y.APP,Y.STMT.THEIR.REF,'DEBIT',PRM.RESPONSE)
            Y.TRANS.NARR.DESC = PRM.RESPONSE
        END ;*END IF CODIGO.COLECTOR.CHG NE ''
        ELSE
        IF Y.TYPE.COMMISSION EQ 'COMMER' THEN
            CALL F.READ(FN.COMMISSION.TYPE,Y.TYPE.COMMISSION,RECORD.COMM.TYPE,F.COMMISSION.TYPE,ERROR.COMM.TYPE)
            Y.TRANS.NARR.DESC = RECORD.COMM.TYPE<FT4.DESCRIPTION>
        END
    END ;*END ELSE CODIGO.COLECTOR.CHG NE ''
    END
    RETURN
*** </region>

*** <region name= EVAL.LIOF.COMM>
EVAL.LIOF.COMM:
*** <desc>*evaluar si es comision cheque cert, *evaluar si tiene liof *generar linea detalle liof, quitar liof de comision </desc>
    COMISION.LIST = ''
    COMISION.ID = ''

    IF (Y.TXN.CODE EQ '142') OR (Y.TXN.CODE EQ '244')  THEN        
        COMISION.LIST = R.FUNDS.TRANSFER<FT.TAX.TYPE>
        CONTADOR.COMM = 1
        MONTO.COMM.LIOF = 0
        FLAG.COMM.LIOF = 0
        MONTO.LIOF= 0
        DESCRIPCION.LIOF = 'Cargo por LIOF'
        LOOP
            REMOVE COMISION.ID FROM COMISION.LIST SETTING COMISION.MARK
        WHILE COMISION.ID : COMISION.MARK
            IF COMISION.ID EQ '15' THEN
                MONTO.COMM.LIOF =FIELD(R.FUNDS.TRANSFER<FT.TAX.AMT,CONTADOR.COMM>,'USD',2)
                IF MONTO.COMM.LIOF > 0 THEN
                    FLAG.COMM.LIOF = 1
                    MONTO.LIOF = (MONTO.COMM.LIOF * -1)

                    Y.STMT.AMOUNT.LCY = Y.STMT.AMOUNT.LCY - MONTO.LIOF;*si hay cargo liof en la comision settear el monto para la comision por emision de cheque
                END
            END
            CONTADOR.COMM++
        REPEAT

        ;* si es commision liof entonces a la linea de comision agregar
        IF FLAG.COMM.LIOF EQ 1 THEN
            IF Y.TEMP.ARR THEN
                Y.TEMP.ARR<-1> = Y.STMT.ID:'.1':'*':Y.ACCT.ID:'*':Y.ACCOUNT.NUMBER:'*':Y.STMT.CURRENCY:'*':Y.STMT.COMPANY
                Y.TEMP.ARR:='*':MONTO.LIOF:'*':Y.STMT.AMOUNT.FCY:'*':DESCRIPCION.LIOF:'*':Y.STMT.VALUE.DATE
                Y.TEMP.ARR:='*':Y.STMT.THEIR.REF:'*':Y.STMT.BOOKING.DATE:'*':Y.STMT.TRANS.REFERENCE:'*':Y.STMT.NARRATIVE
                Y.TEMP.ARR:='*':Y.STMT.SYSTEM.ID:'*':Y.BASE.APP:'*':Y.OPEN.BAL:'*':Y.NARR.DESC:'*':Y.RECORD.STATUS:"*":Y.HORA.TIME:"*":Y.CHQ.NUM
            END ELSE
                Y.TEMP.ARR = Y.STMT.ID:'*':Y.ACCT.ID:'*':Y.ACCOUNT.NUMBER:'*':Y.STMT.CURRENCY:'*':Y.STMT.COMPANY
                Y.TEMP.ARR:='*':MONTO.LIOF:'*':Y.STMT.AMOUNT.FCY:'*':DESCRIPCION.LIOF:'*':Y.STMT.VALUE.DATE
                Y.TEMP.ARR:='*':Y.STMT.THEIR.REF:'*':Y.STMT.BOOKING.DATE:'*':Y.STMT.TRANS.REFERENCE:'*':Y.STMT.NARRATIVE
                Y.TEMP.ARR:='*':Y.STMT.SYSTEM.ID:'*':Y.BASE.APP:'*':Y.OPEN.BAL:'*':Y.NARR.DESC:'*':Y.RECORD.STATUS:"*":Y.HORA.TIME:"*":Y.CHQ.NUM
            END
            FLAG.COMM.LIOF = 0
        END
    END
    RETURN
*** </region>

GET.FT.KEYS.PARMS.DESC:
    R.FT.TXN.TYPE.CONDITION = ''
    FT.TXN.TYPE.CONDITION.ERR = ''
    Y.FT.TRANS.NARR.DESC = ''
    CALL F.READ(FN.FT.TXN.TYPE.CONDITION,Y.FT.TXN.TYPE,R.FT.TXN.TYPE.CONDITION,F.FT.TXN.TYPE.CONDITION,FT.TXN.TYPE.CONDITION.ERR)
    IF R.FT.TXN.TYPE.CONDITION THEN
        ID.KEYS.PARAMS 	= 'SUB.TXN.TYPE' ;*id con Narratives parametrizados en EB.KEYS.PARAMS

        CALL CACHE.READ(FN.EB.KEYS.PARAM, ID.KEYS.PARAMS, REC.SUBTYPE.TXN, KEYS.PARAM.ER)

        IF REC.SUBTYPE.TXN THEN
            ;*Se forma el Id para traer Narrative en app KEYS.PARAM dentro de registro SUB.TXN.TYPE
            IF LF.TXN.BP THEN ;*Elegir Narratives para Pagos Masivos desde TCE
                ID.SUB.TYPE 	= Y.FT.TXN.TYPE : '.' : LF.TXN.BP
            END ELSE ;* Transacciones no masivas desde TCE
                ID.SUB.TYPE 	= Y.FT.TXN.TYPE : '.' : LF.TCE.NARR
            END

            ;*Se lee ids de app Local
            ID.KEYS.PARAM 	= REC.SUBTYPE.TXN<EB.SLV18.PARAM.ID>
            FIND ID.SUB.TYPE IN ID.KEYS.PARAM SETTING POS.SUB.TYPE, POS.SUB.TYPE2 THEN
	            ;* Si tiene descripcion ES
	            IF DCOUNT(R.FT.TXN.TYPE.CONDITION<FT6.DESCRIPTION>, VM) GT 1 THEN
	                Y.TRANS.NARR.DESC = R.FT.TXN.TYPE.CONDITION<FT6.DESCRIPTION><1,1> : ' ' :  REC.SUBTYPE.TXN<EB.SLV18.VALOR><1,POS.SUB.TYPE2>
	            END ELSE
	                Y.TRANS.NARR.DESC = R.FT.TXN.TYPE.CONDITION<FT6.DESCRIPTION> : ' ' :  REC.SUBTYPE.TXN<EB.SLV18.VALOR><1,POS.SUB.TYPE2>
	            END
       		END
   		END
    END
RETURN

GET.FT.BULK.MASTER:
	Y.ID.MASTER	= FIELD(Y.INWARD.PAY.TYPE, '-', 2)
	CALL CACHE.READ(FN.BULK.MASTER, Y.ID.MASTER, REC.MASTER, ERR.MASTER)
	
	IF REC.MASTER THEN
	    ;*Tipo de Transaccion del Master Bulk Payments desde Banca Empresas
	    CALL GET.LOC.REF('FT.BULK.MASTER','LF.TXN.BP', POS.TXN.BP)
	    LF.TXN.BP = REC.MASTER<FT.BLK.MAS.LOCAL.REF, POS.TXN.BP>
	
	    IF LF.TXN.BP THEN
	        ;*Se limpia variable NARR que contiene 'A Cuenta No.- USD1761000030001' para no mostrar cuenta interna
	        NARR 	= ''
	    END
    END
RETURN

*@Author:Ronald Ortiz
*@Date: 20170908
GET.NARRATIVE.ABONO.COLECTOR:
;*Obtenemos la descripcion configurada en la aplicación EV.SLV.KEYS.PARAM
    CALL F.READ(FN.EB.SLV.KEYS.PARAMS,RECORD.ABONO.APP,RESPONSE.NRR.ABN,F.EB.SLV.KEYS.PARAMS,ERR.APP.KEYS)
    NARRATIVE.ABN = RESPONSE.NRR.ABN<EB.SLV18.DESCRIPCION>
RETURN

END	 

