*-----------------------------------------------------------------------------
* <Rating>-20</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE SLV.V.CREATE.ACCT.ARRGMNT
**************************************************************
*-----------------------------------------------------------------------------
* Nombre:		SLV.V.CREATE.ACCT.ARRGMNT
* Descripcion: 	Rutina para setear cuenta payin
*-------------------------------------------------------------------------------------------*
* Version	Autor		Fecha		Comentario
*-------------------------------------------------------------------------------------------*
* 1.0		VBURGOS		07.01.2018	Version inicial
**************************************************************
*
    $INSERT I_COMMON
    $INSERT I_EQUATE
*-----------------------------------------------------------------------------
    $INSERT I_AA.ACTION.CONTEXT
    $INSERT I_F.AA.ARRANGEMENT.ACTIVITY
    $INSERT I_F.AA.SETTLEMENT
    $INSERT I_AA.LOCAL.COMMON
    $INSERT I_AA.APP.COMMON
    $INSERT I_F.AA.ARRANGEMENT
    $INSERT I_TSS.COMMON
    $INSERT I_GTS.COMMON


    GOSUB PROCESS

    RETURN


PROCESS:

	Y.ARRGMNT =  c_aalocArrId

   	Y.ACCT = c_aalocLinkedAccount
    R.NEW(AA.SET.PAYIN.ACCOUNT) = Y.ARRGMNT
    TEXTO.ARCHIVO='Y.ARRGMNT :':Y.ARRGMNT
    GOSUB ESCRIBIR.ARCHIVO
    TEXTO.ARCHIVO='Y.ACCT :':Y.ACCT
    GOSUB ESCRIBIR.ARCHIVO

RETURN

ESCRIBIR.ARCHIVO:
    DIR.NAME= 'MASIVOS'
    R.ID   = 'CuentaSIMP.txt'
    OPENSEQ DIR.NAME,R.ID TO SEQ.PTR
    WRITESEQ TEXTO.ARCHIVO APPEND TO SEQ.PTR THEN
    END
    CLOSESEQ SEQ.PTR
    RETURN
    
    
    
    END




