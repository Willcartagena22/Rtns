*-----------------------------------------------------------------------------
* <Rating>0</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE TestPrueba
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_ENQUIRY.COMMON
    $INSERT I_EQUATE
    $INSERT I_F.ACCOUNT
    $INSERT I_F.CUSTOMER
    $INSERT I_F.USER
    $INSERT I_F.COMPANY
    $INSERT I_F.AA.ARRANGEMENT
    $INSERT I_F.AA.CUSTOMER
    $INSERT I_F.AA.ACCOUNT
    $INSERT I_F.EB.SLV.GLOBAL.PARAM
    $INSERT I_F.SLV.CUS.MUNICIPIO
    $INSERT I_F.SLV.CUS.DEPARTAMENTO
    $INSERT I_F.CUS.OCUPACION.SSF
    $INSERT I_F.EB.SLV.PRIN.CON.ACC
    $INSERT I_F.AA.PRODUCT
    $INSERT I_F.AA.PRODUCT.MANAGER
    $INSERT I_F.AA.PRODUCT.DESIGNER
    $INSERT I_F.AA.PRODUCT.ACCESS
    $INSERT I_F.EB.EXTERNAL.USER
	$INSERT I_F.CUSTOMER
*-----------------------------------------------------------------------------

 	PACKAGE.NAME 		= "t24broadcasterutilnotifications.T24BroadCasterUtilNotifications"
		THIS.METHOD.NAME 	= ""
		METHOD.NAME			= "notificationHalCash"
        ARR.SMS = ''
		ARR.SMS	:= Y.TOKEN						: "%%"
		ARR.SMS	:= 'USD'						: "%%"
		ARR.SMS	:= '$'							: "%%"
		ARR.SMS	:= MONTOSINCOMISION	: "%%"
		ARR.SMS	:= R.FAV<EB.SLV83.PHONE>		: "%%"
		ARR.SMS	:= R.CUS<EB.CUS.EMAIL.1>		: "%%"
		ARR.SMS	:= R.NEW(AC.LCK.TO.DATE)[7,2] : '/' : R.NEW(AC.LCK.TO.DATE)[5,2] : '/' : R.NEW(AC.LCK.TO.DATE)[1,4]	    



END
