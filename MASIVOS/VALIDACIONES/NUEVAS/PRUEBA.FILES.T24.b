*-----------------------------------------------------------------------------
* <Rating>-24</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE PRUEBA.FILES.T24
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :-L
*-----------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
*-----------------------------------------------------------------------------
GOSUB MOVER
GOSUB BORRAR
RETURN
 
MOVER:
;*ASIGNACION DE PARAMETROS PARA EL CALLJ
    THIS.PACKAGE.CLASS ="filest24.FilesT24";
   	THIS.METHOD.CLT= "MoverDirectorio"
    CALLJ.ARGUMENTS.CLT = "C:\\Users\\calvarado\\Desktop\\Proyectos\\Creacion masiva de clientes\\DIGITALIZACION\\CARPETA1\\campos.xlsx~C:\\Users\\calvarado\\Desktop\\Proyectos\\Creacion masiva de clientes\\DIGITALIZACION\\CARPETA2\\"
    CALLJ.ERROR.SMS = " "
    CALLJ.RESPONSE.CLT = " "

;*LLAMADA AL METODO CALLJ
    CALL EB.CALLJ(THIS.PACKAGE.CLASS,THIS.METHOD.CLT,CALLJ.ARGUMENTS.CLT,CALLJ.RESPONSE.CLT,CALLJ.ERROR.CLT)
    RESPUESTA= CALLJ.RESPONSE.CLT
	CRT 'RESPUESTA: ':RESPUESTA


    RETURN
    
 BORRAR:
;*ASIGNACION DE PARAMETROS PARA EL CALLJ
    THIS.PACKAGE.CLASS ="filest24.FilesT24";
   	THIS.METHOD.CLT= "LimpiarDirectorio"
    CALLJ.ARGUMENTS.CLT = "C:\\Users\\calvarado\\Desktop\\Proyectos\\Creacion masiva de clientes\\DIGITALIZACION\\CARPETA1\\"
    CALLJ.ERROR.SMS = " "
    CALLJ.RESPONSE.CLT = " "

;*LLAMADA AL METODO CALLJ
    CALL EB.CALLJ(THIS.PACKAGE.CLASS,THIS.METHOD.CLT,CALLJ.ARGUMENTS.CLT,CALLJ.RESPONSE.CLT,CALLJ.ERROR.CLT)
    RESPUESTA= CALLJ.RESPONSE.CLT
	CRT 'RESPUESTA: ':RESPUESTA


    RETURN   
    
    
END
