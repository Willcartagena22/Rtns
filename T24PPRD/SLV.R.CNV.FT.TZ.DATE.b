*-----------------------------------------------------------------------------
* <Rating>176</Rating>
*-----------------------------------------------------------------------------
*-----------------------------------------------------------------------------
* Nombre: SLV.R.CNV.FT.TZ.DATE
* Referencia: ID 
* Descripcion: Rutina para conversion de Date.Time en FT y mostrar resultado en enquiry.
*----------------------------------------------------------------------------------------------------
* Version	Autor		Fecha		Comentario
*----------------------------------------------------------------------------------------------------
* 1.0		g.alfaro		22.08.2016	Version inicial
*----------------------------------------------------------------------------------------------------

SUBROUTINE SLV.R.CNV.FT.TZ.DATE(DATE_CONV)
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_ENQUIRY.COMMON
$INSERT I_F.FUNDS.TRANSFER
*-----------------------------------------------------------------------------

GOSUB INIT 
GOSUB OPENFILE 
GOSUB PROCESS 

RETURN 

*-----------------------------------------------------------------------------
INIT: 
*-----------------------------------------------------------------------------


*Y.VOU.FEC = R.RECORD<FT.DATE.TIME> 
Y.VOU.FEC = O.DATA

;* DEBUG 
;*Y.VOU.FEC = '1467909361.386'

RETURN 

*-----------------------------------------------------------------------------
OPENFILE: 
*-----------------------------------------------------------------------------


RETURN 

*-----------------------------------------------------------------------------
PROCESS: 
*-----------------------------------------------------------------------------
GOSUB PARSE.DATETIME 

DATE_CONV = Y.VOU.FEC 

RETURN 


PARSE.DATETIME:

		utcDateTime =  DATE_CONV
		UTC.FLAG = ''
		;*Evaluar UTC Time or Standard Time
		
		FINDSTR "." IN utcDateTime SETTING Ap, Vp THEN
			UTC.FLAG = '1'
		END
		
		IF UTC.FLAG EQ '1' THEN
			localZoneDate1 = OCONV(LOCALDATE(utcDateTime,localZone),'D4/E')
			localZoneTime1= OCONV(LOCALTIME(utcDateTime,localZone),'MTS')
			Y.VOU.FEC = localZoneDate1:' ':localZoneTime1
			Y.VOU.FEC = Y.VOU.FEC[7,4]:Y.VOU.FEC[4,2]:Y.VOU.FEC[1,2]: ' ' : localZoneTime1
			;*CRT Y.VOU.FEC 
		END
		ELSE
			Y.DAY.BC = utcDateTime[3,2]
			Y.MONTH.BC = utcDateTime[5,2]
			Y.YEAR.BC = utcDateTime[1,2]
			Y.DATE.BC = OCONV(ICONV(utcDateTime[1,6],'D'),'D4/E')
			;*Y.DATE.BCs = OCONV(ICONV(utcDateTime,'D'),'D4/E')
*			Y.DATE.BC = Y.DAY.BC:'/':Y.MONTH.BC:'/20':Y.YEAR.BC
			Y.TIME.BC = utcDateTime[7,2]:':':utcDateTime[9,2]:':':'00'
			Y.VOU.FEC = Y.DATE.BC: ' ': Y.TIME.BC
			Y.VOU.FEC = Y.VOU.FEC[7,4]:Y.VOU.FEC[4,2]:Y.VOU.FEC[1,2]: ' ' : Y.TIME.BC
			;*CRT Y.VOU.FEC 
		END
RETURN

END
