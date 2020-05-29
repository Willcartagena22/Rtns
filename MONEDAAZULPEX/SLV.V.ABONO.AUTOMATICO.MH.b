*-----------------------------------------------------------------------------
* <Rating>-53</Rating>
*-----------------------------------------------------------------------------
*-----------------------------------------------------------------------------
    SUBROUTINE SLV.V.ABONO.AUTOMATICO.MH
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
    F.FUNDSTRANFER.APP  = 'F.FUNDS.TRANSFER'
    FN.FUNDSTRANFER.APP = ''
    RETURN

OPENFILE:

    RETURN


CALL.J:
;*ASIGNACION DE PARAMETROS PARA EL CALLJ
    THIS.PACKAGE.CLASS ="com.bancoazul.collector.Collector";
    THIS.METHOD.CLT= "getBalance"
    CALLJ.ARGUMENTS.CLT = ""
    CALLJ.ERROR.SMS = " "
    CALLJ.RESPONSE.CLT = " "

;*LLAMADA AL METODO CALLJ
    CALL EB.CALLJ(THIS.PACKAGE.CLASS,THIS.METHOD.CLT,CALLJ.ARGUMENTS.CLT,CALLJ.RESPONSE.CLT,CALLJ.ERROR.CLT)
    PAGOS.AGENCIAS = CALLJ.RESPONSE.CLT

    NUM.OFS = DCOUNT(PAGOS.AGENCIAS,'|')-1
    
    TEXTO.ARCHIVO =PAGOS.AGENCIAS
	GOSUB ESCRIBIR.ARCHIVO 
    
    
    GOSUB PROCESS


    RETURN

PROCESS:
    FOR J = 1 TO NUM.OFS

        SEGMENTO.X2 = FIELD(PAGOS.AGENCIAS,'|',J)
        AGENCIA.COL = FIELD(SEGMENTO.X2,'*',1)
        AGENCIA.COL = CHANGE((AGENCIA.COL),'"','')
        COLECTOR.COL = FIELD(SEGMENTO.X2,'*',2)
        CUENTA.DEB = FIELD(SEGMENTO.X2,'*',3)
        CUENTA.DEB=CHANGE((CUENTA.DEB),'"','')
        CUENTA.CRE = FIELD(SEGMENTO.X2,'*',4)
        MONTO= FIELD(SEGMENTO.X2,'*',5)
        DATE.COL= FIELD(SEGMENTO.X2,'*',6)
        ORDER.CUST = FIELD(SEGMENTO.X2,'*',7) ;*'ABONO DIARIO A CLIENTE'
        ORDER.BANK = 'SYSTEM'
        ID.PARAM.OFS = 'OFS.ABONO.DIARIO.COL'

       
        TRANS.ID = ''
        R.FT = ''
        R.FT<FT.DEBIT.ACCT.NO> = CUENTA.DEB
        R.FT<FT.CREDIT.ACCT.NO> = CUENTA.CRE
        R.FT<FT.DEBIT.CURRENCY> = 'USD'
        R.FT<FT.DEBIT.AMOUNT> = MONTO
        R.FT<FT.ORDERING.BANK> = ORDER.BANK
        R.FT<FT.ORDERING.CUST> = ORDER.CUST
        R.FT<FT.TRANSACTION.TYPE> ='AC66'
        
        IF CUENTA.DEB EQ 'PL52924' THEN
        	R.FT<FT.PROFIT.CENTRE.DEPT> ='1'
        END          
        
        Y.OUT = ''


        CALL SLV.UTIL.OFS.TRX(TRANS.ID,R.FT,ID.PARAM.OFS,Y.OUT)
		
    NEXT J
    
    
    

    RETURN

ESCRIBIR.ARCHIVO:
    DIR.NAME= 'MHLogs'
    R.ID   = 'SLV.V.ABONO.AUTOMATICO.MH':TODAY:'.txt'
;* hacer que escriba un archivo

    OPENSEQ DIR.NAME,R.ID TO SEQ.PTR
    WRITESEQ TEXTO.ARCHIVO APPEND TO SEQ.PTR THEN
    END
    CLOSESEQ SEQ.PTR
    RETURN 
    END

