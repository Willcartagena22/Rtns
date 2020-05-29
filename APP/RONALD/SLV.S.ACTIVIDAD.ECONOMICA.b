*-----------------------------------------------------------------------------
* <Rating>44</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.S.ACTIVIDAD.ECONOMICA
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_AA.ACTION.CONTEXT
$INSERT I_F.AA.ARRANGEMENT.ACTIVITY
$INSERT I_F.CUSTOMER
$INSERT I_F.EB.SLV.CLS.CIIU
$INSERT I_F.INDUSTRY
$INSERT I_F.AA.ACCOUNT
$INSERT I_AA.LOCAL.COMMON
$INSERT I_AA.APP.COMMON
*-----------------------------------------------------------------------------

GOSUB INIT
GOSUB OPEN.FILE
GOSUB PROCESS
RETURN

INIT:
 CUST.FN  = 'F.CUSTOMER'
 CUST.F   = ''
 CIIU.FN  = 'F.EB.SLV.CLS.CIIU'
 CIIU.F   = ''
 INDUS.FN = 'F.INDUSTRY'
 INDUS.F  = ''
RETURN

OPEN.FILE:
 CALL OPF(CUST.FN,CUST.F)
 CALL OPF(CIIU.FN,CIIU.F)
 CALL OPF(INDUS.FN,INDUS.F)
RETURN

PROCESS:
 Y.CUST  = c_arrActivityRec<AA.ARR.ACT.CUSTOMER>
 TEXTO.ARCHIVO = 'Y.CUST > ':Y.CUST
 GOSUB ESCRIBIR.ARCHIVO
 CALL F.READ(CUST.FN,Y.CUST,CUST.R,CUST.F,ERR)
 ACTIVIDAD.ECONOMICA = CUST.R<EB.CUS.INDUSTRY>
 CALL F.READ(INDUS.FN,ACTIVIDAD.ECONOMICA,INDUST.R,INDUS.F,ERR.IND)
 DESCRIPCION           = INDUST.R<EB.IND.DESCRIPTION>
 
    Y.APP = ""
    Y.FIELDS = ""
    Y.FLD.POS = ""
    LF.AML.PROC.FON = ""
    Y.APP = "AA.PRD.DES.ACCOUNT"
    Y.FIELDS = "LF.AML.PROC.FON"
    CALL MULTI.GET.LOC.REF(Y.APP,Y.FIELDS,Y.FLD.POS)
    LF.AML.PROC.FON = Y.FLD.POS<1,1>
    
    R.NEW(AA.AC.LOCAL.REF)<1,LF.AML.PROC.FON> = DESCRIPCION
    
RETURN

ESCRIBIR.ARCHIVO: 
	DIR.NAME= 'FATCA'
	R.ID   = 'SLV.S.ACTIVIDAD.ECONOMICA_':TODAY:'.txt'
	;* hacer que escriba un archivo 
	
	OPENSEQ DIR.NAME,R.ID TO SEQ.PTR 
		WRITESEQ TEXTO.ARCHIVO APPEND TO SEQ.PTR THEN
		END 
	CLOSESEQ SEQ.PTR 
	
RETURN 

END
