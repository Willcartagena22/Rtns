*-----------------------------------------------------------------------------
* <Rating>-6</Rating>
*-----------------------------------------------------------------------------
*@autor eurias
*@util obtener nombre completo de cliente
*@in vars customer cod cliente
*@out vars nombre de cliente
SUBROUTINE SLV.UTIL.GET.NOM.CLIENTE(customer)
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
	$INSERT I_COMMON
	$INSERT I_EQUATE
	$INSERT I_F.CUSTOMER
*-----------------------------------------------------------------------------
	nombreApliacion ="F.CUSTOMER"
	aplicacionFull = ''
    aplicacionDataCliente=''
    Error=''
    CALL OPF(nombreApliacion, aplicacionFull)
    CALL F.READ(nombreApliacion,customer,aplicacionDataCliente,aplicacionFull,Error)   
*    IF(aplicacionDataCliente<EB.CUS.NAME.1>)THEN
*       	nomCliente := TRIM(aplicacionDataCliente<EB.CUS.NAME.1>):' '
*    END       	
*    IF((aplicacionDataCliente<EB.CUS.NAME.2>))THEN
*       	nomCliente := TRIM(aplicacionDataCliente<EB.CUS.NAME.2>):' '
*    END       	
*    IF((aplicacionDataCliente<EB.CUS.TEXT>))THEN
*       	nomCliente := TRIM(aplicacionDataCliente<EB.CUS.TEXT>):' '
*    END       	
*    IF((aplicacionDataCliente<EB.CUS.SHORT.NAME>))THEN
*       	;*nomCliente := TRIM(aplicacionDataCliente<EB.CUS.SHORT.NAME>):' '
*    END       	
*    IF((aplicacionDataCliente<EB.CUS.FAMILY.NAME>))THEN
*       	nomCliente := TRIM(aplicacionDataCliente<EB.CUS.FAMILY.NAME>):' '
*    END       	
*    IF((aplicacionDataCliente<EB.CUS.PREVIOUS.NAME>))THEN
*       	;*nomCliente := TRIM(aplicacionDataCliente<EB.CUS.PREVIOUS.NAME>):' '
*    END 
	nomCliente = aplicacionDataCliente<EB.CUS.SHORT.NAME>
    customer = nomCliente
END
