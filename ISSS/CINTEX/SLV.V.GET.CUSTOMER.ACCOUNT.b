*-----------------------------------------------------------------------------
* <Rating>-47</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.V.GET.CUSTOMER.ACCOUNT
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_F.AC.CHARGE.REQUEST
$INSERT I_F.FUNDS.TRANSFER
$INSERT I_F.ACCOUNT
*-----------------------------------------------------------------------------

GOSUB INIT
GOSUB PROCESS
RETURN

INIT:
FN.ACCOUNT = 'F.ACCOUNT'
F.ACCOUNT  = ''
RETURN

OPEN.FILE:
CALL OPF(FN.ACCOUNT,F.ACCOUNT)
RETURN

PROCESS:

ACCOUNT = COMI
ACCOUNT.FIELD = FIELD(ACCOUNT,'-',1)
NOMBRE.VERSION = PGM.VERSION

IF NOMBRE.VERSION EQ ',SLV.COBRO.COMISION.COLECTORES' THEN

;*CONSULTAMOS EL REGISTRO DE LA CUENTA ENVIADA.
;*CALL F.READ(FN.ACCOUNT,ACCOUNT.FIELD,RESPONSE.ACCOUNT,F.ACCOUNT,ERR.ACCOUNT)
;*ID.CUSTOMER = RESPONSE.ACCOUNT<AC.CUSTOMER>
;*ID.CUSTOMER = '100119'

;*TEXTO.ARCHIVO = 'COMI > ':COMI
;*GOSUB ESCRIBIR.ARCHIVO

;*TEXTO.ARCHIVO = 'FLD > ': FIELD(ACCOUNT,'-',1)
;*GOSUB ESCRIBIR.ARCHIVO
;*R.NEW(CHG.DEBIT.ACCOUNT) = ACCOUNT.FIELD
;*R.NEW(CHG.CUSTOMER.NO)   = ID.CUSTOMER
R.NEW(CHG.CHARGE.CCY)    = 'USD'
END
ELSE
R.NEW(FT.DEBIT.ACCT.NO)  = ACCOUNT.FIELD 
END

RETURN


ESCRIBIR.ARCHIVO: 
DIR.NAME= 'COLECTORES'
R.ID   = 'SLV.V.GET.CUSTOMER.ACCOUNT':TODAY:'.txt'
;* hacer que escriba un archivo 
OPENSEQ DIR.NAME,R.ID TO SEQ.PTR 
        WRITESEQ TEXTO.ARCHIVO APPEND TO SEQ.PTR THEN
END 
CLOSESEQ SEQ.PTR 
RETURN


END
