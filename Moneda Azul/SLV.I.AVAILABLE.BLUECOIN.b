*-----------------------------------------------------------------------------
* <Rating>-20</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.I.AVAILABLE.BLUECOIN
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :
* Date              who          Reference          description
*16-JUL-18       PSANCHEZ                        Record routine for Defaulting the Customer to Local field
*												 in AA.ARR.PRODUCT.ACCESS,
*-----------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_AA.LOCAL.COMMON
$INSERT I_F.AA.PRODUCT.ACCESS
$INSERT I_F.AA.ARRANGEMENT.ACTIVITY
$INSERT I_AA.APP.COMMON
$INSERT I_F.AA.ARRANGEMENT
*-----------------------------------------------------------------------------

	GOSUB INIT
	GOSUB PROCESS
	
	RETURN
	
	INIT:
		APPL.ARR ='AA.PRD.DES.PRODUCT.ACCESS'
		FIELDNAME.ARR	='LF.MONEDA.AZUL':VM:'LF.MINIMUM.AZUL':VM:'LF.MAXIMUM.AZUL':VM:'LF.PERCENT.AZUL'
		CALL MULTI.GET.LOC.REF(APPL.ARR,FIELDNAME.ARR,POS.ARR)
	RETURN
	
	PROCESS:
*		R.NEW(AA.PRODA.LOCAL.REF)<1,AAA.POSITION>='SI'
		R.NEW(AA.PRODA.LOCAL.REF)<1,POS.ARR<1,1>>='SI'
		R.NEW(AA.PRODA.LOCAL.REF)<1,POS.ARR<1,2>>=0.20
		R.NEW(AA.PRODA.LOCAL.REF)<1,POS.ARR<1,3>>=3.0
		R.NEW(AA.PRODA.LOCAL.REF)<1,POS.ARR<1,4>>=2.5
		
		
	RETURN
END
