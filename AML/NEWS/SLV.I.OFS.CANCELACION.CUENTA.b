*-----------------------------------------------------------------------------
* <Rating>-33</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.I.OFS.CANCELACION.CUENTA
*-----------------------------------------------------------------------------
*-----------------------------------------------------------------------------
*-----------------------------------------------------------------------------
* Modification History :
* FECHA: 27/11/2019
* AUTOR: CALVARADO
* DESCRIPCION: Envío de OFS para Desvinculacion de cuenta Simplificada
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_ENQUIRY.COMMON
    $INSERT I_F.USER
    $INSERT I_F.ACCOUNT
    $INSERT I_F.AA.ARRANGEMENT
    $INSERT I_F.CUSTOMER
    $INSERT I_F.ACCT.INACTIVE.RESET
    $INSERT I_F.AA.ARRANGEMENT.ACTIVITY
    $INSERT I_F.EB.SLV.OFS.PARAM.BUILD
    $INSERT I_F.EB.SLV.AML.CUSTOMER.SIMP
     $INSERT I_AA.ACTION.CONTEXT
    $INSERT I_F.AA.ARRANGEMENT.ACTIVITY
    $INSERT I_F.AA.SETTLEMENT
    $INSERT I_AA.LOCAL.COMMON
*-----------------------------------------------------------------------------
    GOSUB INI
    GOSUB OPENFILE
    GOSUB PROCESS
    RETURN

INI:
	;* Inicilizar variables
    ID.PARAM.OFS = 'OFS.AAA.EXPIRED'
    FN.AA  = 'F.ACCOUNT'
	F.AA   = ''
	Y.OUT = ''
	ID.CTA=R.NEW(EB.EB.84.ACCOUNT)
	
    RETURN

OPENFILE:
	;* Abrir las aplicaciones
    CALL OPF(FN.AA, F.AA)
 RETURN


PROCESS:
	;* Leer la aplicación ACCOUNT
    CALL F.READ(FN.AA, ID.CTA, RECORD.ACC, F.AA, ERROR.READ)
    ID.ARR = RECORD.ACC<AC.ARRANGEMENT.ID>
    R.NEW(EB.EB.84.RESERVADO.4)='DESVINCULADA'
  
    
      
    ;*ACCOUNTS-UPDATE-SETTLEMEN
    OFS='AA.ARRANGEMENT.ACTIVITY,SLV.CANCELACION.CTA/I/PROCESS//0/,//SV0010001/////,,ARRANGEMENT:1:1=':ID.ARR:',ACTIVITY:1:1=ACCOUNTS-UPDATE-SETTLEMENT,EFFECTIVE.DATE:1:1=':TODAY:',':'PROPERTY:1:1=SETTLEMENT,FIELD.NAME:1:1=PAYIN.SETTLEMENT:1:1,FIELD.VALUE:1:1=NO,FIELD.NAME:1:2=PAYIN.ACCOUNT:1:1,FIELD.VALUE:1:2=,' 
    ;*OFS='AA.ARRANGEMENT.ACTIVITY,/I/PROCESS//0/,//SV0010001/////,,ARRANGEMENT:1:1=':ID.ARR:',ACTIVITY:1:1=ACCOUNTS-REQUEST.CLOSURE-ARRANGEMENT,EFFECTIVE.DATE:1:1=':TODAY:',' 
    Y.OPTIONS='SLVOFSPS'
    CRT OFS
    CALL OFS.POST.MESSAGE(OFS,Y.RESPONSE,Y.OPTIONS,TXN.RESULT)
     
RETURN

END
