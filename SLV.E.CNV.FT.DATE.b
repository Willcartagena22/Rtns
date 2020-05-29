*-----------------------------------------------------------------------------
* <Rating>-44</Rating>
*-----------------------------------------------------------------------------
*-----------------------------------------------------------------------------
* Nombre: SLV.E.CNV.FT.DATE
* Referencia: ID 
* Descripcion: Rutina para conversion de Date.Time en FT y mostrar resultado en enquiry.
*----------------------------------------------------------------------------------------------------
* Version	Autor		Fecha		Comentario
*----------------------------------------------------------------------------------------------------
* 1.0		R.Garay		20.07.16	Version inicial
*----------------------------------------------------------------------------------------------------

SUBROUTINE SLV.E.CNV.FT.DATE
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
FN.FT = 'F.FUNDS.TRANSFER'
F.FT  = ''

*Y.VOU.FEC = R.RECORD<FT.DATE.TIME> 
Y.VOU.FEC = O.DATA

;* DEBUG 
*Y.VOU.FEC = '1467909361.386'

RETURN 

*-----------------------------------------------------------------------------
OPENFILE: 
*-----------------------------------------------------------------------------
CALL OPF(FN.FT, F.FT) 

RETURN 

*-----------------------------------------------------------------------------
PROCESS: 
*-----------------------------------------------------------------------------
GOSUB PARSE.DATETIME 

O.DATA = Y.VOU.FEC 

RETURN 


PARSE.DATETIME:

		utcDateTime =  Y.VOU.FEC
		UTC.FLAG = ''
		;*Evaluar UTC Time or Standard Time
		
		FINDSTR "." IN utcDateTime SETTING Ap, Vp THEN
			UTC.FLAG = '1'
		END
		
		IF UTC.FLAG EQ '1' THEN
			localZoneDate1 = OCONV(LOCALDATE(utcDateTime,localZone),'D4/E')
			localZoneTime1= OCONV(LOCALTIME(utcDateTime,localZone),'MTS')
			Y.VOU.FEC = localZoneDate1:' ':localZoneTime1
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
		END
RETURN

END
