*-----------------------------------------------------------------------------
* <Rating>-11</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.V.ACCT.ARR
**************************************************************
*-----------------------------------------------------------------------------
* Nombre:		SLV.V.ACCT.ARR
* Descripcion: 	Rutina para setear cuenta payin
*-------------------------------------------------------------------------------------------*
* Version	Autor		Fecha		Comentario
*-------------------------------------------------------------------------------------------*
* 1.0		VBURGOS		12.01.2018	Version inicial
**************************************************************
* 
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
*-----------------------------------------------------------------------------
    $INSERT I_AA.ACTION.CONTEXT
    $INSERT I_F.AA.ARRANGEMENT.ACTIVITY
    $INSERT I_F.AA.SETTLEMENT
    $INSERT I_AA.LOCAL.COMMON
    $INSERT I_AA.APP.COMMON	 
    

    GOSUB PROCESS

    RETURN
 
PROCESS:

	Y.ARRGMNT =  c_aalocArrId ;*Toma el arrgmnt que esta creando en el momento
	R.NEW(AA.SET.PAYIN.SETTLEMENT)<1,1>='YES'
	R.NEW(AA.SET.PAYIN.ACCOUNT)<1,1> = Y.ARRGMNT
	
RETURN