*-----------------------------------------------------------------------------
* <Rating>-76</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.B.CARD.PROCESS.OFS 
*-----------------------------------------------------------------------------
* 
*-----------------------------------------------------------------------------
* Modification History :
* Autor		Fecha		Comentario
* KDAMAS	10.03.2017	Initial Code
*-----------------------------------------------------------------------------
$INSERT I_COMMON 
$INSERT I_EQUATE
$INSERT I_F.FUNDS.TRANSFER 
$INSERT I_F.EB.SLV.COMISIONES.TARJETAS  
*-----------------------------------------------------------------------------

GOSUB INIT
GOSUB OPENF
GOSUB PREPROCESS
GOSUB PROCESS
RETURN

*** <region name= INIT>
INIT:
*** <desc>Inicializar Valores de componentes </desc>
FN.FT='F.FUNDS.TRANSFER'
F.FT=''
FN.SCT='F.EB.SLV.COMISIONES.TARJETAS'
F.SCT=''
THIS.PACKAGE.CLASS ="com.bancoazul.collector.Collector";
THIS.METHOD.CLT= "getDataExtreme"
CALLJ.ARGUMENTS.CLT = "SpGetPruebasInserts"
CALLJ.ERROR.SMS = " "
CALLJ.RESPONSE.CLT = " "
STR.ERR = "/-1/"
ID.PARAM.OFS ='OFS.COM.VISA.DEB'
*ID.PARAM.OFS ='OFS.INPUT.FT.GEN' 
TRANS.ID = ''
R.FT = ''
CANT.REQ =''
ID.FT=''
;*--------------------------------------------------
*Valores de prueba
DEBIT.ACC ='USD1760700030001'
CREDIT.ACC = '10000000042526'
*CREDIT.ACC = '10'
CURRENCY = 'USD'
Y.AMOUNT = '52.00'
ORDER.BANK = 'CONTABILIDAD VISA TC' 
TRANSACTION.TYPE = 'ACC1'
ID.OFS = '82'
;*--------------------------------------------------
RETURN
*** </region>


*** <region name= OPENF>
OPENF:
*** <desc>Apertura de Aplicaciones </desc>
	CALL OPF(FN.FT,F.FT)
	CALL OPF(FN.SCT,F.SCT)
RETURN
*** </region>


*** <region name= PREPROCESS>
PREPROCESS:
*** <desc>Barrido de FT pendientes </desc>
	STMT.SCT = "SELECT ":FN.SCT:" STATUS EQ '0' AND CARD.TYPE EQ 'C'"
;*Aplicando el Statement
	CALL EB.READLIST(STMT.SCT, SCT.LIST,'',NO.OF.SCT,SCT.ERR)
    FOR I=1 TO NO.OF.SCT
;*Obtener registro no aplicado de app local 
	R.FT = ''
    CALL F.READ(FN.SCT,SCT.LIST<I>,AR.SCT,F.SCT,ERR.SCT)
    R.FT<FT.DEBIT.ACCT.NO>    =  AR.SCT<EB.SLV91.ACC.DEBITO>
	R.FT<FT.CREDIT.ACCT.NO>   =  AR.SCT<EB.SLV91.ACC.CREDITO>
	R.FT<FT.DEBIT.CURRENCY>   =  'USD'
	R.FT<FT.DEBIT.AMOUNT>     =  AR.SCT<EB.SLV91.AMOUNT>
	R.FT<FT.ORDERING.BANK>    =  AR.SCT<EB.SLV91.ORDERING.BANK>
	R.FT<FT.TRANSACTION.TYPE> =  AR.SCT<EB.SLV91.TYPE.TRANSACTION>
	
	CALL GET.LOC.REF('FUNDS.TRANSFER','LF.IDOFS.CARD', LocPosIdOfsCard)	
    R.FT<FT.LOCAL.REF,LocPosIdOfsCard> = SCT.LIST<I>
    
    CALL SLV.OFS.UTIL.OL.TRX(TRANS.ID,R.FT,ID.PARAM.OFS,Y.OUT)
	FINDSTR STR.ERR IN FIELD(Y.OUT, FM, 2) SETTING Ap, Vp THEN
		 AR.SCT<EB.SLV91.STATUS>='0'
*	    CALL F.WRITE (FN.SCT,SCT.LIST<I>,AR.SCT)	
	END
	ELSE	
	 	 AR.SCT<EB.SLV91.STATUS>='1'
	 	 AR.SCT<EB.SLV91.ID.OFS> =	FIELD(FIELD(Y.OUT,'<request>', 2),'//1',1)
	 	  
	 	 ;*Campos Auditoria
		AR.SCT<EB.SLV91.INPUTTER> = OPERATOR 
		AR.SCT<EB.SLV91.AUTHORISER>= OPERATOR	
		AR.SCT<EB.SLV91.CURR.NO> += 1
		X = OCONV(DATE(),"D-")
    	V$TIMEDATE = TIMEDATE()
    	V$TIMEDATE = V$TIMEDATE[1,5]
    	CONVERT ":" TO "" IN V$TIMEDATE
    	X = X[9,2] : X[1,2] : X[4,2] : V$TIMEDATE
		AR.SCT<EB.SLV91.DATE.TIME>		   = X
		AR.SCT<EB.SLV91.RECORD.STATUS>	   = 'AUTH'
		CALL F.WRITE (FN.SCT,SCT.LIST<I>,AR.SCT)
*		CALL JOURNAL.UPDATE(FN.SCT)
	END 
	
    NEXT I
RETURN
*** </region>


*** <region name= PROCESS>
PROCESS: 
*** <desc>Procesamiento del programa </desc>
;*	LLAMADA AL METODO CALLJ
;*	CALL EB.CALLJ(THIS.PACKAGE.CLASS,THIS.METHOD.CLT,CALLJ.ARGUMENTS.CLT,CALLJ.RESPONSE.CLT,CALLJ.ERROR.CLT)
;*	DATA.EXTRME = CALLJ.RESPONSE.CLT
;*	NUM.OFS = DCOUNT(DATA.EXTRME,'|')-1
;*	CRT 'NUM REGISTROS:': DCOUNT(DATA.EXTRME,'|')-1 
	
*FOR I=1 TO 100

*	ID.OFS += 1
	CALL F.READ(FN.FT,'', R.FT,F.FT,E.FT)  
    R.FT<FT.DEBIT.ACCT.NO>    =  DEBIT.ACC
 	R.FT<FT.CREDIT.ACCT.NO>   =  CREDIT.ACC
	R.FT<FT.DEBIT.CURRENCY>   =  CURRENCY
	R.FT<FT.DEBIT.AMOUNT>     =  Y.AMOUNT
	R.FT<FT.ORDERING.BANK>    =  ORDER.BANK
	R.FT<FT.TRANSACTION.TYPE> =  TRANSACTION.TYPE
*Enviando campo local que se utilizara como identificador
	CALL GET.LOC.REF('FUNDS.TRANSFER','LF.IDOFS.CARD', LocPosIdOfsCard)	
	R.FT<FT.LOCAL.REF,LocPosIdOfsCard> = ID.OFS
*Llamada de rutina para aplicar OFS
	CALL SLV.OFS.UTIL.OL.TRX(TRANS.ID,R.FT,ID.PARAM.OFS,Y.OUT)
*	CALL JOURNAL.UPDATE(FN.FT)
	FINDSTR STR.ERR IN FIELD(Y.OUT, FM, 2) SETTING Ap, Vp THEN
*		ID.FT=FIELD(FIELD(Y.OUT,'<request>', 1),'//-1',1)
*		CRT 'ID.FTKD:':ID.FT
*		ID.FT2 = FIELD(ID.FT,'FT',2)
*		ID.FT2 = 'FT':ID.FT2
*		ID.FT = ID.FT2
		R.SCT.STATUS='0'
		GOSUB WRIT
	END
	ELSE
		STMT.FT = "SELECT ":FN.FT:" LF.IDOFS.CARD EQ '":ID.OFS:"'"
		CALL EB.READLIST(STMT.FT,FT.LIST,'',NO.OF.FT,FT.ERR)	
*		ID.FT=FIELD(FIELD(Y.OUT,'<request>', 2),'//1',1)
*		CRT 'ID.FT:':ID.FT
		R.SCT<EB.SLV16.ID.OFS> =FT.LIST 	
		R.SCT.STATUS='1'
		GOSUB WRIT
	END

*NEXT I

 
RETURN
*** </region>


*** <region name= WRIT>
WRIT:
*** <desc>Persistir en la App Local </desc>
	R.SCT<EB.SLV91.ACC.DEBITO> = DEBIT.ACC
	R.SCT<EB.SLV91.ACC.CREDITO> = CREDIT.ACC
	R.SCT<EB.SLV91.AMOUNT>=Y.AMOUNT
	R.SCT<EB.SLV91.TYPE.TRANSACTION> = TRANSACTION.TYPE
	R.SCT<EB.SLV91.ORDERING.BANK> =  ORDER.BANK
	R.SCT<EB.SLV91.CREDIT.DATE> = TODAY
	R.SCT<EB.SLV91.DEBIT.DATE> = TODAY
	R.SCT<EB.SLV91.COB.DATE> =TODAY
	R.SCT<EB.SLV91.CARD.TYPE> ='C'
	R.SCT <EB.SLV91.IVA> = '1'
	R.SCT<EB.SLV91.STATUS>=R.SCT.STATUS
	
	;*Campos Auditoria
	R.SCT<EB.SLV91.INPUTTER> = OPERATOR 
	R.SCT<EB.SLV91.AUTHORISER>= OPERATOR	
	R.SCT<EB.SLV91.CURR.NO> += 1
	X = OCONV(DATE(),"D-")
    V$TIMEDATE = TIMEDATE()
    V$TIMEDATE = V$TIMEDATE[1,5]
    CONVERT ":" TO "" IN V$TIMEDATE
    X = X[9,2] : X[1,2] : X[4,2] : V$TIMEDATE
	R.SCT<EB.SLV91.DATE.TIME>		   = X
	R.SCT<EB.SLV91.RECORD.STATUS>	   = 'AUTH'
	
	;*Escribir Nuevo Registro
		;*-----------------------
		CALL F.WRITE (FN.SCT,ID.OFS, R.SCT)	 
*		CALL JOURNAL.UPDATE(FN.SCT)
	
RETURN
*** </region>

END
