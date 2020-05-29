*-----------------------------------------------------------------------------
* <Rating>-43</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE Prueba.Util.TXN
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.EB.SLV.COLECTOR
    $INSERT I_F.EB.SLV.COLECTOR.TRX.PAGO
    $INSERT I_F.FUNDS.TRANSFER
;*$INSERT I_F.EB.SLV.ACCOUNT.PARAM
    $INSERT I_F.COMPANY
*-----------------------------------------------------------------------------
    GOSUB INIT
    GOSUB OPENFILE
    GOSUB CALL.J
	
    RETURN

INIT:
   
	ID.PARAM.OFS = 'OFS.INPUT.CARD'

    RETURN

OPENFILE:

    RETURN

CALL.J:



;*ASIGNACION DE PARAMETROS PARA EL CALLJ
    THIS.PACKAGE.CLASS ="com.bancoazul.collector.Collector";
    THIS.METHOD.CLT= "getDataExtreme"
    CALLJ.ARGUMENTS.CLT = "CobroComisiones"
    CALLJ.ERROR.SMS = " "
    CALLJ.RESPONSE.CLT = " "

;*LLAMADA AL METODO CALLJ
    CALL EB.CALLJ(THIS.PACKAGE.CLASS,THIS.METHOD.CLT,CALLJ.ARGUMENTS.CLT,CALLJ.RESPONSE.CLT,CALLJ.ERROR.CLT)
   ;*DATA.EXTRME = CALLJ.RESPONSE.CLT
    
DATA.EXTRME ="1~10000000005507~USD1093100020001~41849.75000~AC~REV LIMITE CLASICA~2017-03-09 15:26:51~2017-03-09 15:26:51~A~2017-03-09 15:26:51~C~0|"

    Y.COUNT.FILE = DCOUNT(DATA.EXTRME,'|')-1
  
    FOR I=1 TO Y.COUNT.FILE
        YBLOQUE.1 = FIELD(DATA.EXTRME,'|',I)
        Y.CAMPOS.B1 = CHANGE(YBLOQUE.1,'~',VM)

        IDOFS = FIELD( Y.CAMPOS.B1,VM,1)
        CRT 'ID:  ..':IDOFS
        CUENTACREDITO = FIELD( Y.CAMPOS.B1,VM,2)
        CUENTADEBITO = FIELD( Y.CAMPOS.B1,VM,3)
        MONTO = FIELD( Y.CAMPOS.B1,VM,4)
        TIPOTRANSACTION = FIELD( Y.CAMPOS.B1,VM,5)
        ORDENBANCO = FIELD( Y.CAMPOS.B1,VM,6)
        FECHACREDITO = FIELD( Y.CAMPOS.B1,VM,7)
        FECHADEBITO = FIELD( Y.CAMPOS.B1,VM,8)
        ESTADO = FIELD( Y.CAMPOS.B1,VM,9)
        FECHACOB = FIELD( Y.CAMPOS.B1,VM,10)
        TIPOTARJETA = FIELD( Y.CAMPOS.B1,VM,11)
        IVA = FIELD( Y.CAMPOS.B1,VM,12)
        
	GOSUB PROCESS
    NEXT I
    
PROCESS:
    
        TRANS.ID = ''
        R.FT = ''             
        Y.OUT = ''
	    R.FT<FT.DEBIT.ACCT.NO>    =  CUENTADEBITO
	 	R.FT<FT.CREDIT.ACCT.NO>   =  CUENTACREDITO
		R.FT<FT.DEBIT.CURRENCY>   =  LCCY
		R.FT<FT.DEBIT.AMOUNT>     =  MONTO
		R.FT<FT.ORDERING.BANK>    =  ORDENBANCO
		R.FT<FT.TRANSACTION.TYPE> =  TIPOTRANSACTION
		CALL GET.LOC.REF('FUNDS.TRANSFER','LF.IDOFS.CARD', LocPosIdOfsCard)	
	    R.FT<FT.LOCAL.REF,LocPosIdOfsCard> = IDOFS

        CALL SLV.UTIL.OFS.TRX(TRANS.ID,R.FT,ID.PARAM.OFS,Y.OUT)
		
		CRT 'PROCESO'
		    
    
    

    RETURN


    END

