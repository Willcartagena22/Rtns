*-----------------------------------------------------------------------------
* <Rating>-21</Rating>
*----------------------------------------------------------------------------------------------------
* Nombre: SLV.B.EXP.DEPOSITS.T24.SELECT.b
* Descripcion: Rutina encargada de generar archivo .csv con informacion de Arrangemente de Depositos, Creditos y Cuentas de
*				 T24 , para volcado de data a SIC
*----------------------------------------------------------------------------------------------------
* Version	    Autor		 Fecha			Comentario
*----------------------------------------------------------------------------------------------------
* 1.0		MMenjivar		29.06.2017		Version inicial
*-----------------------------------------------------------------------------------------------------

SUBROUTINE SLV.B.EXP.DEPOSITS.T24.SELECT
  
$INSERT I_COMMON 
$INSERT I_EQUATE     
$INSERT I_F.AA.ARRANGEMENT.ACTIVITY  
$INSERT I_F.AA.ACTIVITY.HISTORY 
$INSERT I_AA.LOCAL.COMMON     
$INSERT I_AA.APP.COMMON	
$INSERT I_F.AA.ARRANGEMENT   
$INSERT I_SLV.B.EXP.DEPOSITS.T24.COMMON
$INSERT I_F.EB.SLV.DAILY.ARR.DATA
*----------------------------------------------------------------------------- 
GOSUB INIT 
GOSUB PROCESS
RETURN
*-----------------------------------------------------------------------------
INIT:
 	MAXDIAS = '-4C'
	STMT.ARRANGEMENT = '' ; ARRANGEMENT.LIST.IDS = '' ; NO.OF.RECS.ARRG = '' ; Y.ARRANGEMENT.ERR = ''
	STMT.DAILY.ARR = '' ; DAILY.ARR.LIST.IDS = '' ; NO.OF.RECS.DAILY.ARR = '' ; Y.DAILY.ARR.ERR = ''
	
	FECHA = TODAY
	;*FECHA = '20180730'
	CALL CDT ('', FECHA, MAXDIAS)

RETURN



PROCESS:

	STMT.DAILY.ARR = 'SELECT ': FN.DAILY.ARR : ' WITH DATE.TIME LT ' : FECHA
	CRT STMT.DAILY.ARR
	CALL EB.READLIST(STMT.DAILY.ARR, DAILY.ARR.LIST.IDS, '', NO.OF.RECS.DAILY.ARR, Y.DAILY.ARR.ERR)

	CRT NO.OF.RECS.DAILY.ARR

	FOR I = 1 TO NO.OF.RECS.DAILY.ARR
		CRT 'ID: ':DAILY.ARR.LIST.IDS<I>
		CALL SLV.OFS.UTIL.OL.TRX( DAILY.ARR.LIST.IDS<I>,'', 'OFS.DEL.ARR.DATA' ,Y.OUT)
	NEXT I

	EXECUTE 'CLEAR-FILE FBNK.EB.SLV.DAILY.ARR.DATA$HIS'

	STMT.ARRANGEMENT = "SELECT ": FN.ARRANGEMENT  : " WITH PRODUCT.LINE EQ ACCOUNTS OR PRODUCT.LINE EQ DEPOSITS"

    ;*Extrayendo Ids de Arrangement
    CALL EB.READLIST(STMT.ARRANGEMENT, ARRANGEMENT.LIST.IDS, '' , NO.OF.RECS.ARRG, Y.ARRANGEMENT.ERR)
    CALL BATCH.BUILD.LIST('',ARRANGEMENT.LIST.IDS)

RETURN

END
