*-----------------------------------------------------------------------------
* <Rating>-10</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.S.ID.BULK.MASTER
*-----------------------------------------------------------------------------
* Nombre: SLV.ID.BULK.MASTER
* Descripcion: Rutina Encargada de Generar ID para tabla EB.SLV.BULK.MASTER  
*----------------------------------------------------------------------------------------------------
* Version	Autor		Fecha		Comentario
*----------------------------------------------------------------------------------------------------
* 1.0		iTurcios	28.06.17	Version inicial    
*----------------------------------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
*-----------------------------------------------------------------------------

GOSUB GENERATE_ID
 
GENERATE_ID:
	COMI = 'BKML' : CHANGE(TIMESTAMP(),'.','')
*	CRT ID.NEW
RETURN
 
END  
 