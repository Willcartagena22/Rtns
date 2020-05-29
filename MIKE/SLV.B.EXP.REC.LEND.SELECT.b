*-----------------------------------------------------------------------------
* <Rating>-20</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.B.EXP.REC.LEND.SELECT
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_EQUATE
$INSERT I_F.AA.ARRANGEMENT 
$INSERT I_SLV.B.EXP.REC.LEND.COMMON
*----------------------------------------------------------------------------------------------------

*----------------------------------------------------------------------------------------------------
GOSUB INIT
GOSUB PROCESS
RETURN
*----
INIT:
*----
STMT.LEND = '' ; ARRANGEMENT.LIST.IDS = '' ; NO.OF.RECS.LEND = '' ; Y.LEND.ERR = ''

RETURN
PROCESS:
*-------
	EXECUTE 'CLEAR-FILE FBNK.EB.SLV.LEND.REC.SSF'
	
	STMT.LEND = "SELECT ":FN.ARRANGEMENT:" WITH PRODUCT.LINE EQ 'LENDING' AND (ARR.STATUS EQ 'CURRENT' OR ARR.STATUS EQ 'PENDING.CLOSURE' OR ARR.STATUS EQ 'CLOSE' OR ARR.STATUS EQ 'EXPIRED')" 
    CALL EB.READLIST(STMT.LEND, ARRANGEMENT.LIST.IDS, '', NO.OF.RECS.LEND, Y.LEND.ERR)
    CALL BATCH.BUILD.LIST('',ARRANGEMENT.LIST.IDS)
RETURN
END
