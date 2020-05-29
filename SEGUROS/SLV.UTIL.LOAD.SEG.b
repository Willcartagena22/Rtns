*------------------------------------------------------------------------------------------
* <Rating>336</Rating>
*------------------------------------------------------------------------------------------
    SUBROUTINE SLV.UTIL.LOAD.SEG
*------------------------------------------------------------------------------------------
* RUTINA QUE REALIZA DESCARGA DE DATOS PARA LA FUNCIONALIDAD DE SEGUROS
*------------------------------------------------------------------------------------------
* Modification History :
* Autor    	Fecha     		Version.
* SFUSILLO 	11.09.2017 		Initial Code
* SFUSILLO 					Esta rutina se utiliza para la funcionalidad de SEGUROS 
*							y realiza bajada de datos cuando en la aplicacion se da commit
*------------------------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_ENQUIRY.COMMON
    $INSERT I_EQUATE
	$INSERT I_F.CUSTOMER
	$INSERT I_F.EB.SLV.ALTA.SEGURO
	$INSERT I_F.EB.SLV.PLAN.SEGURO
	$INSERT I_F.USER
	$INSERT I_TSS.COMMON
    $INSERT I_GTS.COMMON
    $INSERT I_F.ACCOUNT
    $INSERT I_F.BATCH
    $INSERT I_F.EB.SLV.KEYS.PARAMS
    $INSERT I_F.EB.SLV.GLOBAL.PARAM
    $INSERT I_F.EB.LOOKUP
	
    GOSUB INIT
    GOSUB OPENFILE
    GOSUB PROCESS


RETURN

INIT:
;*ARCHIVOS
;*-----------------------------------
;*-----------------------------------
    FN.CUS = 'F.CUSTOMER'
    F.CUS  = ''

	FN.LOOKUP = 'F.EB.LOOKUP'
    F.LOOKUP  = ''
    FN.PLAN.SEGURO = 'F.EB.SLV.PLAN.SEGURO'
    F.PLAN.SEGURO  = ''
    FN.USER = 'F.USER'
    F.USER  = ''
    FN.ACCOUNT = 'F.ACCOUNT'
    F.ACCOUNT  = ''
    FN.KEYS.PARAMS = 'F.EB.SLV.KEYS.PARAMS'
    F.KEYS.PARAMS  = ''
    
    FN.GLOBAL.PARAM = 'F.EB.SLV.GLOBAL.PARAM'
    F.GLOBAL.PARAM  = ''
;*-----------------------------------


;*VARIABLES
;*-----------------------------------
VAR.ID.TRANS.SEG=ID.NEW
VAR.SEGURO.ALTA.EJECUTIVO=R.NEW(EB.SEG.NOMBRE.EJECUTIVO)
VAR.PLAN.SEGURO.ID=R.NEW(EB.SEG.ID.TIPO.SEGURO)
BEGIN CASE
CASE R.NEW(EB.SEG.REFERENCIA) NE '' AND R.NEW(EB.SEG.REFERENCIA) NE '0'
VAR.SEG.CTADEBITO=R.NEW(EB.SEG.REFERENCIA)
END CASE

;*Variables de Certificado
;*-----------------------------------
VAR.SEG.PLAN.CODTIPOSEG=''
VAR.SEG.PLAN.CODPLAN=''
VAR.SEG.POLIZA.MADRE=''
VAR.SEG.NUM.CERTI=''
VAR.SEG.FINICIO.COBRO=''
VAR.SEG.FVENTA=''
VAR.SEG.FINICIO.VIG=''
VAR.SEG.FFIN.VIG=''
VAR.USR.CODEMP=''
VAR.USR.CTAEMP=''
VAR.USR.NOM.EMP=''
VAR.USR.COD.AGE=''
VAR.SEG.ECORREO=''
;*-----------------------------------

;*Variables de Cliente
;*-----------------------------------
VAR.CLIENTE.ID=''
VAR.CLIENTE.COD.IDENT=''
VAR.CLIENTE.NUM.IDENT=''
VAR.CLIENTE.PNOMBRE=''
VAR.CLIENTE.SNOMBRE=''
VAR.CLIENTE.PAPELLIDO=''
VAR.CLIENTE.SAPELLIDO=''
VAR.CLIENTE.FNACIMIENTO=''
VAR.CLIENTE.GENERO=''
VAR.CLIENTE.STDOCIVIL=''
VAR.CLIENTE.OCUPACION=''
VAR.CLIENTE.CORREO=''
VAR.CLIENTE.CODNAC=''
VAR.CLIENTE.TELEFONO1=''
VAR.CLIENTE.TELEFONO2=''
VAR.CLIENTE.DIRECDOM=''
VAR.CLIENTE.CODPAISDOM=''
VAR.CLIENTE.CODDEPTODOM=''
VAR.CLIENTE.CODMUNDOM=''
VAR.CLIENTE.DIRECTRAB=''
VAR.CLIENTE.CODPAISTRAB=''
VAR.CLIENTE.CODDEPTOTRAB=''
VAR.CLIENTE.CODMUNTRAB=''
VAR.CLIENTE.NIT=''

;*Variables de Forma de Pago
;*-----------------------------------
VAR.SEG.FREC.PAGO=''
VAR.SEG.TIPO.PRODBAN=''
VAR.SEG.CODPRODBAN=''
VAR.SEG.MON.PRODBAN=''
VAR.SEG.LIMITE.CREDTC=''
VAR.SEG.FVENCI.TC=''
VAR.SEG.CODTIPO.TC=''
VAR.SEG.CODBANCO.EMI=''
VAR.SEG.NOM.TC=''
		
;*Variables de Beneficiario
;*-----------------------------------		
VAR.SEG.BENEF.TIPOIDENT=''
VAR.SEG.BENEF.NOIDENT=''
VAR.SEG.BENEF.PNOMBRE=''
VAR.SEG.BENEF.SNOMBRE=''
VAR.SEG.BENEF.PAPELLIDO=''
VAR.SEG.BENEF.SAPELLIDO=''
VAR.SEG.BENEF.PARENTESCO=''
VAR.SEG.BENEF.TAPELLIDO=''
VAR.SEG.BENEF.FNAC=''
VAR.SEG.BENEF.PORCENT=''
VAR.LEN.APE=''
VAR.LEN.NOM=''
VAR.SEG.BENEF.TNOMBRE=''
;*CONSTANTES
;*-----------------------------------
CONS.SEPAR.CERTI=">"
CONS.CONV.TIPO.DOC='SEG.ALTA.TIPO.DOC'
CONS.CONV.GENERO='SEG.ALTA.GENERO'
CONS.GLOBAL.PARAM='RUTA.SEG.REIMP.DOC.PDF'

;*PARAMETROS
;*-----------------------------------


RETURN

OBTENERID:
;* idColector~idCanal
	Y.PARAMETRO.ARGUMENT = '03':'~':'VENTANILLA'
	THIS.PACKAGE.CLASS 	= "com.bancoazul.collector.Collector";* "com.bancoazul.t24colecturia.ColectorPEXMWS"
    THIS.METHOD.CLT		= "getID"
    CALLJ.ARGUMENTS.CLT = Y.PARAMETRO.ARGUMENT
    CALLJ.ERROR.SMS 	= " "
    CALLJ.RESPONSE.CLT 	= " "
	;*LLAMADA AL METODO CALLJ
    CALL EB.CALLJ(THIS.PACKAGE.CLASS,THIS.METHOD.CLT,CALLJ.ARGUMENTS.CLT,CALLJ.RESPONSE.CLT,CALLJ.ERROR.CLT)
    
    ;* Asignar el Id recuperado
    ID.TRANSACTION = FIELD(CALLJ.RESPONSE.CLT,'|',3)


RETURN


*APERTURA DE ARCHIVOS A USAR
OPENFILE:

 CALL OPF(FN.CUS, F.CUS)
 CALL OPF(FN.LOOKUP, F.LOOKUP)
 CALL OPF(FN.PLAN.SEGURO, F.PLAN.SEGURO)
 CALL OPF(FN.USER, F.USER)
 CALL OPF(FN.ACCOUNT, F.ACCOUNT)
 CALL OPF(FN.KEYS.PARAMS, F.KEYS.PARAMS)
 CALL OPF(FN.GLOBAL.PARAM, F.GLOBAL.PARAM)
 
RETURN

*PROCESO MAIN
PROCESS:
		GOSUB GOPOSFIELD
		GOSUB READFILE
	 	GOSUB GENERAR.TRAMA
	 	
RETURN

READFILE:
	CALL F.READ(FN.PLAN.SEGURO,VAR.PLAN.SEGURO.ID,R.PLAN.SEGURO,F.PLAN.SEGURO,Y.ERR)
	CALL F.READ(FN.ACCOUNT,VAR.SEG.CTADEBITO,R.ACCOUNT,F.ACCOUNT,Y.ERR)
	K.STMT.ARRANGEMENT 	= "SELECT " : FN.USER : " WITH USER.NAME EQ '" : VAR.SEGURO.ALTA.EJECUTIVO :"' "
	CALL EB.READLIST(K.STMT.ARRANGEMENT, ARRANGEMENT.LIST, R.NOCUS, NO.OF.RECS, Y.ARRANGEMENT.ERR1)
	VAR.SEGURO.ALTA.USER=ARRANGEMENT.LIST<1>
	CALL F.READ(FN.USER,VAR.SEGURO.ALTA.USER,R.USR,F.USER,Y.ERR)
	CALL F.READ(FN.GLOBAL.PARAM,CONS.GLOBAL.PARAM,R.GPARAM,F.GLOBAL.PARAM,Y.ERR)
	VAR.RUTA.ARCHIVO=R.GPARAM<EB.SLV39.VALOR.PARAM>
	VAR.GLOBAL.PARAM.ID='CORREO.SEG.NOTIF.POLIZA'
	CALL F.READ(FN.GLOBAL.PARAM,VAR.GLOBAL.PARAM.ID,R.GLOBAL,F.GLOBAL.PARAM,Y.ERR)

RETURN

GOPOSFIELD:
CALL GET.LOC.REF("CUSTOMER","LF.OCUPACION",OCU.POS)
CALL GET.LOC.REF("CUSTOMER","LF.DEPTO.3",DPT3.POS)
CALL GET.LOC.REF('CUSTOMER','LF.MUNICIPIO.3',MUN3.POS)
CALL GET.LOC.REF("CUSTOMER","LF.DEPTO.2",DPT2.POS)
CALL GET.LOC.REF("CUSTOMER","LF.MUNICIPIO.2",MUN2.POS)
CALL GET.LOC.REF("CUSTOMER","LF.NIT",NIT.POS)

CALL GET.LOC.REF("CUSTOMER","LF.CANTON.3",CAN.POS)
CALL GET.LOC.REF("CUSTOMER","LF.COLONIA.3",COL.POS)
CALL GET.LOC.REF("CUSTOMER","LF.CALLE.3",CALL.POS)
CALL GET.LOC.REF("CUSTOMER","LF.AVENIDA.3",AVE.POS)
CALL GET.LOC.REF("CUSTOMER","LF.NUM.DEPTO.3",NDEP.POS)
RETURN

GENERAR.TRAMA:

	;*Contruccion de variables
	CUSTOMER.ID= R.NEW(EB.SEG.CUSTOMER.ID);*ID cliente
	SEGURADO.ID=	R.NEW(EB.SEG.ASEGURADO)
*	CUSTOMER.ID=100151;*ID cliente
*	SEGURADO.ID=100151
	GOSUB OBTENERID

	;* Abrir Archivo a Escribir
	;*--------------------------
	NAME.FILE		= 'AltaDeSeguro' : '.txt'
	DIR.NAME	= 'SEGUROS'
	;*GOSUB DELETE_AND_OPEN
	
	;*Leer datos del certificado
	GOSUB CERTIFICADO
	VAR.TRAMA.DATA.CERTI=VAR.SEG.PLAN.CODTIPOSEG:FM:VAR.SEG.PLAN.CODPLAN:FM:VAR.SEG.POLIZA.MADRE:FM:VAR.SEG.NUM.CERTI:FM:VAR.SEG.FINICIO.COBRO:FM:VAR.SEG.FVENTA:FM
	VAR.TRAMA.DATA.CERTI:=VAR.SEG.FINICIO.VIG:FM:VAR.SEG.FFIN.VIG:FM:VAR.USR.CODEMP:FM:VAR.USR.CTAEMP:FM:VAR.USR.NOM.EMP:FM:VAR.USR.COD.AGE:FM:VAR.SEG.ECORREO:FM
	VAR.CLIENTE.ID=SEGURADO.ID
	GOSUB LEER.CLIENTE
	VAR.TRAMA.DATA.ASEGURADO=VAR.CLIENTE.ID:VM:VAR.CLIENTE.COD.IDENT:VM:VAR.CLIENTE.NUM.IDENT:VM:VAR.CLIENTE.PNOMBRE:VM:VAR.CLIENTE.SNOMBRE:VM:VAR.CLIENTE.PAPELLIDO:VM
	VAR.TRAMA.DATA.ASEGURADO:=VAR.CLIENTE.SAPELLIDO:VM:VAR.CLIENTE.FNACIMIENTO:VM:VAR.CLIENTE.GENERO:VM:VAR.CLIENTE.STDOCIVIL:VM:VAR.CLIENTE.OCUPACION:VM:VAR.CLIENTE.CORREO:VM
	VAR.TRAMA.DATA.ASEGURADO:=VAR.CLIENTE.CODNAC:VM:VAR.CLIENTE.TELEFONO1:VM:VAR.CLIENTE.TELEFONO2:VM:VAR.CLIENTE.DIRECDOM:VM:VAR.CLIENTE.CODPAISDOM:VM:VAR.CLIENTE.CODDEPTODOM:VM
	VAR.TRAMA.DATA.ASEGURADO:=VAR.CLIENTE.CODMUNDOM:VM:VAR.CLIENTE.DIRECTRAB:VM:VAR.CLIENTE.CODPAISTRAB:VM:VAR.CLIENTE.CODDEPTOTRAB:VM:VAR.CLIENTE.CODMUNTRAB:VM:VAR.CLIENTE.NIT
	VAR.CLIENTE.ID=CUSTOMER.ID
	GOSUB LEER.CLIENTE
	VAR.TRAMA.DATA.RESPON=FM:VAR.CLIENTE.ID:VM:VAR.CLIENTE.COD.IDENT:VM:VAR.CLIENTE.NUM.IDENT:VM:VAR.CLIENTE.PNOMBRE:VM:VAR.CLIENTE.SNOMBRE:VM:VAR.CLIENTE.PAPELLIDO:VM
	VAR.TRAMA.DATA.RESPON:=VAR.CLIENTE.SAPELLIDO:VM:VAR.CLIENTE.FNACIMIENTO:VM:VAR.CLIENTE.GENERO:VM:VAR.CLIENTE.STDOCIVIL:VM:VAR.CLIENTE.OCUPACION:VM:VAR.CLIENTE.CORREO:VM
	VAR.TRAMA.DATA.RESPON:=VAR.CLIENTE.CODNAC:VM:VAR.CLIENTE.TELEFONO1:VM:VAR.CLIENTE.TELEFONO2:VM:VAR.CLIENTE.DIRECDOM:VM:VAR.CLIENTE.CODPAISDOM:VM:VAR.CLIENTE.CODDEPTODOM:VM
	VAR.TRAMA.DATA.RESPON:=VAR.CLIENTE.CODMUNDOM:VM:VAR.CLIENTE.DIRECTRAB:VM:VAR.CLIENTE.CODPAISTRAB:VM:VAR.CLIENTE.CODDEPTOTRAB:VM:VAR.CLIENTE.CODMUNTRAB:VM:VAR.CLIENTE.NIT
	VAR.CORREO.TITULAR=VAR.CLIENTE.CORREO
	GOSUB LEER.FORMA.PAGO
	VAR.TRAMA.DATA.FORMA.PAGO=VM:VAR.SEG.FREC.PAGO:SM:VAR.SEG.TIPO.PRODBAN:SM:VAR.SEG.CODPRODBAN:SM:VAR.SEG.CTADEBITO:SM:VAR.SEG.MON.PRODBAN:SM:VAR.SEG.LIMITE.CREDTC:SM
	VAR.TRAMA.DATA.FORMA.PAGO:=VAR.SEG.FVENCI.TC:SM:VAR.SEG.CODTIPO.TC:SM:VAR.SEG.CODBANCO.EMI:SM:VAR.SEG.NOM.TC:FM
	
	V.CONTAR=COUNT(R.NEW(EB.SEG.NOMBRES),VM)+1
	i=1
    LOOP WHILE i LE V.CONTAR  DO
		GOSUB LEER.BENEFICIARIO
		;*VAR.TRAMA.DATA.BENEF:=VAR.SEG.BENEF.TIPOIDENT:SM:VAR.SEG.BENEF.NOIDENT:SM:VAR.SEG.BENEF.PNOMBRE:SM:VAR.SEG.BENEF.SNOMBRE:SM:VAR.SEG.BENEF.PAPELLIDO
		VAR.TRAMA.DATA.BENEF:=VAR.SEG.BENEF.PNOMBRE:SM:VAR.SEG.BENEF.SNOMBRE:SM:VAR.SEG.BENEF.PAPELLIDO
		VAR.TRAMA.DATA.BENEF:=SM:VAR.SEG.BENEF.SAPELLIDO:SM:VAR.SEG.BENEF.PARENTESCO:SM:VAR.SEG.BENEF.FNAC:SM:VAR.SEG.BENEF.PORCENT
		IF i LT V.CONTAR THEN
			VAR.TRAMA.DATA.BENEF:=VM
		END
		i=i+1	
	REPEAT
	VAR.CONV.STR.ARR2=EREPLACE(EREPLACE(EREPLACE(VAR.TRAMA.DATA.CERTI:VAR.TRAMA.DATA.ASEGURADO:VAR.TRAMA.DATA.RESPON:VAR.TRAMA.DATA.FORMA.PAGO:VAR.TRAMA.DATA.BENEF,FM,'~'),VM,'|'),SM,'^')
	STR.ARR2 = VAR.CONV.STR.ARR2
	;* Generacion de csv array para escritura de en csv
	;*-----------------------------------------------------------------------------
	GOSUB ENVIO.TRAMA
	
	STR.ARR2 = "TRAMA: " :STR.ARR2: CHAR(13): CHAR(13): "RESPUESTA: " : RESPUESTA.CALLJ
*    WRITEBLK STR.ARR2 ON SEQ.PTR THEN ;*sin retorno de carro
*    END	
*	
*   	CLOSESEQ SEQ.PTR

RETURN


LEER.CLIENTE:

CALL F.READ(FN.CUS,VAR.CLIENTE.ID,R.CUS,F.CUS,Y.ERR)
*-----------------------------------------------------------------
VAR.KEY.PARAM.ID=CONS.CONV.TIPO.DOC
*VAR.CONV.KEY.PARAM=R.CUS<EB.CUS.LEGAL.DOC.NAME><1,1>
*VAR.CLIENTE.NUM.IDENT=R.CUS<EB.CUS.LEGAL.ID><1,1>

	B=1
	LOOP WHILE B < 3 DO     
	
    VAR.CONV.KEY.PARAM=R.CUS<EB.CUS.LEGAL.DOC.NAME><1,B>
   	CRT VAR.CONV.KEY.PARAM
	    IF VAR.CONV.KEY.PARAM='DOCTO.UNICO.IDENT' THEN
		    VAR.CLIENTE.NUM.IDENT=R.CUS<EB.CUS.LEGAL.ID><1,B> 	
		    VAR.CONV.KEY.PARAM=R.CUS<EB.CUS.LEGAL.DOC.NAME><1,B>	
	    END
	    ELSE 
	            IF VAR.CONV.KEY.PARAM='PASSPORT' THEN
		        VAR.CLIENTE.NUM.IDENT=R.CUS<EB.CUS.LEGAL.ID><1,B> 		 
		        VAR.CONV.KEY.PARAM=R.CUS<EB.CUS.LEGAL.DOC.NAME><1,B>    
	            END
	            ELSE
		            IF VAR.CONV.KEY.PARAM='CARNET.RESIDENTE' THEN
			        VAR.CLIENTE.NUM.IDENT=R.CUS<EB.CUS.LEGAL.ID><1,B> 		     
			        VAR.CONV.KEY.PARAM=R.CUS<EB.CUS.LEGAL.DOC.NAME><1,B>
		            END 
		            ELSE	
		              VAR.CONV.KEY.PARAM=R.CUS<EB.CUS.LEGAL.DOC.NAME><1,1>
                      VAR.CLIENTE.NUM.IDENT=R.CUS<EB.CUS.LEGAL.ID><1,1>
		            
		            END
	            END
	    END
	    B=B+1
    
    REPEAT


GOSUB CONVERT.KEY.PARAM
VAR.CLIENTE.COD.IDENT=VAR.CONV.KEY.PARAM
*-----------------------------------------------------------------
VAR.FECHA = R.CUS<EB.CUS.DATE.OF.BIRTH>
GOSUB CONVERT.FECHA
VAR.CLIENTE.FNACIMIENTO=VAR.CONV.FECHA
*-----------------------------------------------------------------
VAR.KEY.PARAM.ID=CONS.CONV.GENERO
VAR.CONV.KEY.PARAM=R.CUS<EB.CUS.GENDER> 
GOSUB CONVERT.KEY.PARAM
VAR.CLIENTE.GENERO= VAR.CONV.KEY.PARAM 
*-----------------------------------------------------------------

VAR.CLIENTE.PNOMBRE=R.CUS<EB.CUS.NAME.1>
VAR.CLIENTE.SNOMBRE=R.CUS<EB.CUS.NAME.2>
VAR.CLIENTE.PAPELLIDO=R.CUS<EB.CUS.TEXT>
VAR.CLIENTE.SAPELLIDO=R.CUS<EB.CUS.FAMILY.NAME>
VAR.CLIENTE.STDOCIVIL= R.CUS<EB.CUS.MARITAL.STATUS>
VAR.CLIENTE.OCUPACION=R.CUS<EB.CUS.LOCAL.REF><1, OCU.POS>
VAR.CLIENTE.CORREO= R.CUS<EB.CUS.EMAIL.1><1,1>
VAR.CLIENTE.CODNAC= R.CUS<EB.CUS.NATIONALITY>

CALL GET.LOC.REF('CUSTOMER','SEGMENT',POS.SEGMENT) 
SEGMENTO= R.CUS<EB.CUS.LOCAL.REF><1,POS.SEGMENT> 



BEGIN CASE
	CASE SEGMENTO EQ 2
	 
		CALL GET.LOC.REF('CUSTOMER','LF.RAZON.SOCIAL',POS.RAZON) 
		VAR.CLIENTE.PNOMBRE= R.CUS<EB.CUS.LOCAL.REF><1,POS.RAZON>		
		VAR.CLIENTE.SNOMBRE=R.CUS<EB.CUS.SHORT.NAME>
		VAR.FECHA = R.CUS<EB.CUS.BIRTH.INCORP.DATE>
		GOSUB CONVERT.FECHA
		VAR.CLIENTE.FNACIMIENTO=VAR.CONV.FECHA
		VAR.CLIENTE.STDOCIVIL=''
		VAR.CLIENTE.OCUPACION=''
		
		CALL GET.LOC.REF('CUSTOMER','LF.NOB.NIT',POS.NOBNIT) 
        VAR.CLIENTE.PAPELLIDO= R.CUS<EB.CUS.LOCAL.REF><1,POS.NOBNIT> 
        VAR.CLIENTE.GENERO='J'
        
        CALL GET.LOC.REF("CUSTOMER","LF.CANTON",CAN.POSJ)
        VAR.DIR.CANT=R.CUS<EB.CUS.LOCAL.REF><1,CAN.POSJ>
        
        CALL GET.LOC.REF("CUSTOMER","LF.COLONIA",COL.POSJ)
        VAR.DIR.COLONIA=R.CUS<EB.CUS.LOCAL.REF><1,COL.POSJ>
        
        CALL GET.LOC.REF("CUSTOMER","LF.CALLE",CALL.POSJ)
        VAR.DIR.CALLE=R.CUS<EB.CUS.LOCAL.REF><1,CALL.POSJ>
        
        CALL GET.LOC.REF("CUSTOMER","LF.AVENIDA",AVE.POSJ)
        VAR.DIR.AVENIDA=R.CUS<EB.CUS.LOCAL.REF><1,AVE.POSJ>
        
        VAR.CLIENTE.DIRECDOM = VAR.DIR.CANT:' ':VAR.DIR.COLONIA:' ':VAR.DIR.CALLE:' ':VAR.DIR.AVENIDA
        

END CASE


BEGIN CASE
	CASE R.CUS<EB.CUS.PHONE.1><1,1> NE '' AND R.CUS<EB.CUS.PHONE.1><1,1> NE '0'	
	VAR.CLIENTE.TELEFONO1=R.CUS<EB.CUS.PHONE.1><1,1>
	
END CASE

VAR.CLIENTE.TELEFONO2=R.CUS<EB.CUS.SMS.1><1,1>	
VAR.CLIENTE.DIRECDOM= R.CUS<EB.CUS.LOCAL.REF><1, CAN.POS> :' ':R.CUS<EB.CUS.LOCAL.REF><1, COL.POS> :' ': R.CUS<EB.CUS.LOCAL.REF><1, CALL.POS> :' ': R.CUS<EB.CUS.LOCAL.REF><1, AVE.POS> :' ': R.CUS<EB.CUS.LOCAL.REF><1, NDEP.POS>
VAR.CLIENTE.CODPAISDOM= R.CUS<EB.CUS.RESIDENCE>
VAR.CLIENTE.NIT=R.CUS<EB.CUS.LOCAL.REF><1, NIT.POS>

BEGIN CASE
	CASE SEGMENTO EQ 2
	         
        CALL GET.LOC.REF("CUSTOMER","LF.CANTON",CAN.POSJ)
        VAR.DIR.CANT=R.CUS<EB.CUS.LOCAL.REF><1,CAN.POSJ>
        
        CALL GET.LOC.REF("CUSTOMER","LF.COLONIA",COL.POSJ)
        VAR.DIR.COLONIA=R.CUS<EB.CUS.LOCAL.REF><1,COL.POSJ>
        
        CALL GET.LOC.REF("CUSTOMER","LF.CALLE",CALL.POSJ)
        VAR.DIR.CALLE=R.CUS<EB.CUS.LOCAL.REF><1,CALL.POSJ>
        
        CALL GET.LOC.REF("CUSTOMER","LF.AVENIDA",AVE.POSJ)
        VAR.DIR.AVENIDA=R.CUS<EB.CUS.LOCAL.REF><1,AVE.POSJ>
        
        VAR.CLIENTE.DIRECDOM = VAR.DIR.CANT:' ':VAR.DIR.COLONIA:' ':VAR.DIR.CALLE:' ':VAR.DIR.AVENIDA
        

END CASE

BEGIN CASE
	CASE R.CUS<EB.CUS.LOCAL.REF><1, DPT3.POS> NE '' AND R.CUS<EB.CUS.LOCAL.REF><1, DPT3.POS> NE '0'
	VAR.CLIENTE.CODDEPTODOM=INT(R.CUS<EB.CUS.LOCAL.REF><1, DPT3.POS>)
END CASE
BEGIN CASE
	CASE R.CUS<EB.CUS.LOCAL.REF><1, MUN3.POS> NE '' AND R.CUS<EB.CUS.LOCAL.REF><1, MUN3.POS> NE '0'
	VAR.CLIENTE.CODMUNDOM=INT(R.CUS<EB.CUS.LOCAL.REF><1, MUN3.POS>)
END CASE
VAR.CLIENTE.DIRECTRAB= R.CUS<EB.CUS.EMPLOYERS.ADD><1,1>
BEGIN CASE
	CASE R.CUS<EB.CUS.RESIDENCE> NE '' AND R.CUS<EB.CUS.RESIDENCE> NE '0'
VAR.CLIENTE.CODPAISTRAB= R.CUS<EB.CUS.RESIDENCE>
END CASE
BEGIN CASE
	CASE R.CUS<EB.CUS.LOCAL.REF><1, DPT2.POS> NE '' AND R.CUS<EB.CUS.LOCAL.REF><1, DPT2.POS> NE '0'
VAR.CLIENTE.CODDEPTOTRAB=INT(R.CUS<EB.CUS.LOCAL.REF><1, DPT2.POS>)
END CASE
BEGIN CASE
	CASE R.CUS<EB.CUS.LOCAL.REF><1, MUN2.POS> NE '' AND R.CUS<EB.CUS.LOCAL.REF><1, MUN2.POS> NE '0'
VAR.CLIENTE.CODMUNTRAB=INT(R.CUS<EB.CUS.LOCAL.REF><1, MUN2.POS>)
END CASE

RETURN

CERTIFICADO:

;*GOSUB CONVERT.FECHA
GOSUB LEER.PLAN.SEGURO
GOSUB LEER.ALTA.SEGURO
GOSUB LEER.USER

RETURN

LEER.ALTA.SEGURO:
BEGIN CASE
CASE R.NEW(EB.SEG.NO.POLIZA) NE '' AND R.NEW(EB.SEG.NO.POLIZA) NE '0'
VAR.SEG.POLIZA.MADRE=R.NEW(EB.SEG.NO.POLIZA)
END CASE
BEGIN CASE
CASE R.NEW(EB.SEG.NO.CERTIFICADO) NE '' AND R.NEW(EB.SEG.NO.CERTIFICADO) NE '0'
VAR.SEG.NUM.CERTI=R.NEW(EB.SEG.NO.CERTIFICADO)
END CASE
BEGIN CASE
CASE R.NEW(EB.SEG.DOC.CORREO) NE '' AND R.NEW(EB.SEG.DOC.CORREO) NE '0'
VAR.SEG.ECORREO=SUBSTRINGS(R.NEW(EB.SEG.DOC.CORREO),1,1)
END CASE
*-------------------------------------------------------
VAR.FECHA = R.NEW(EB.SEG.EFECTIVE.DATE)
GOSUB CONVERT.FECHA

VAR.SEG.FVENTA=VAR.CONV.FECHA
VAR.SEG.FINICIO.VIG=VAR.CONV.FECHA
VAR.SEG.FFIN.VIG=VAR.CONV.FECHA


*-------------------------------FECHA.COBRO
VAR.FECHA=R.NEW(EB.SEG.FECHA.PAGO)
GOSUB CONVERT.FECHA
VAR.SEG.FINICIO.COBRO=VAR.CONV.FECHA

*-------------------------------------------------------
RETURN

LEER.USER:
VAR.USR.CODEMP=VAR.SEGURO.ALTA.USER
VAR.USR.CTAEMP=VAR.SEGURO.ALTA.USER
VAR.USR.NOM.EMP=R.USR<EB.USE.USER.NAME>
VAR.USR.COD.AGE=R.USR<EB.USE.COMPANY.CODE><1,1>

IF VAR.USR.COD.AGE EQ 'ALL' THEN
	VAR.USR.COD.AGE='SV0010001'
END

RETURN

LEER.PLAN.SEGURO:
VAR.SEG.PLAN.CODTIPOSEG=R.PLAN.SEGURO<EB.SLV.PLAN.COD.TIPO.SEGURO>
VAR.SEG.PLAN.CODPLAN=R.PLAN.SEGURO<EB.SLV.PLAN.COD.PLAN.SEGURO>
RETURN

LEER.FORMA.PAGO:

BEGIN CASE
CASE R.NEW(EB.SEG.FECUENCIA.PAGO) NE '' AND R.NEW(EB.SEG.FECUENCIA.PAGO) NE '0'
*VAR.LOOKUP.ID='FRECUENCIA.PAGO*':R.NEW(EB.SEG.FECUENCIA.PAGO);*:R.ALTA<EB.SEG.FECUENCIA.PAGO>
*CALL F.READ(FN.LOOKUP,VAR.LOOKUP.ID,R.LOOKUP,F.LOOKUP,Y.ERR)
*VAR.SEG.FREC.PAGO=R.LOOKUP<EB.LU.DESCRIPTION><1,2>
VAR.SEG.FREC.PAGO=R.NEW(EB.SEG.FECUENCIA.PAGO)
END CASE
BEGIN CASE
CASE R.NEW(EB.SEG.MEDIO.PAGO) NE '' AND R.NEW(EB.SEG.MEDIO.PAGO) NE '0'
VAR.LOOKUP.ID='MEDIO.PAGO*':R.NEW(EB.SEG.MEDIO.PAGO);*:R.ALTA<EB.SEG.FECUENCIA.PAGO>
CALL F.READ(FN.LOOKUP,VAR.LOOKUP.ID,R.LOOKUP,F.LOOKUP,Y.ERR)
VAR.SEG.TIPO.PRODBAN=R.LOOKUP<EB.LU.OTHER.INFO>
END CASE
VAR.SEG.CODPRODBAN=R.LOOKUP<EB.LU.DATA.NAME>
VAR.SEG.MON.PRODBAN=R.ACCOUNT<AC.CURRENCY>
VAR.SEG.LIMITE.CREDTC=0
*---------------------------------------------------------------
VAR.FECHA = ''
GOSUB CONVERT.FECHA
VAR.SEG.FVENCI.TC=VAR.CONV.FECHA
*---------------------------------------------------------------
VAR.SEG.CODTIPO.TC=""
VAR.SEG.CODBANCO.EMI=""
VAR.SEG.NOM.TC=""

IF R.NEW(EB.SEG.MEDIO.PAGO) EQ 'TCO' THEN

MARCA.EMISORA=R.NEW(EB.SEG.MARCA.EMISORA)
VAR.SEG.FVENCI.TC=R.NEW(EB.SEG.FECHA.VENCIMIENTO)
VAR.SEG.CTADEBITO=R.NEW(EB.SEG.NO.TARJETA)
VAR.SEG.CODBANCO.EMI=R.NEW(EB.SEG.BANCO.EMISOR)
VAR.SEG.MON.PRODBAN='USD'

END


RETURN

LEER.BENEFICIARIO:
BEGIN CASE
CASE FIELD (R.NEW(EB.SEG.NOMBRES)<1,i>," ",1,1) NE '' AND FIELD (R.NEW(EB.SEG.NOMBRES)<1,i>," ",1,1) NE '0'
VAR.SEG.BENEF.PNOMBRE=FIELD(R.NEW(EB.SEG.NOMBRES)<1,i>," ",1,1)
END CASE
BEGIN CASE
CASE R.NEW(EB.SEG.PARENTESCO)<1,i> NE ''
VAR.SEG.BENEF.PARENTESCO=R.NEW(EB.SEG.PARENTESCO)<1,i>
END CASE
BEGIN CASE
CASE R.NEW(EB.SEG.PORCENTAJE)<1,i> NE '' AND R.NEW(EB.SEG.PORCENTAJE)<1,i> NE '0'
VAR.SEG.BENEF.PORCENT=R.NEW(EB.SEG.PORCENTAJE)<1,i>
END CASE

*-----------------------------------------------------------------
VAR.KEY.PARAM.ID=CONS.CONV.TIPO.DOC
VAR.CONV.KEY.PARAM=""
GOSUB CONVERT.KEY.PARAM
VAR.SEG.BENEF.TIPOIDENT=VAR.CONV.KEY.PARAM
*-----------------------------------------------------------------
VAR.FECHA = ''
GOSUB CONVERT.FECHA
VAR.SEG.BENEF.FNAC=VAR.CONV.FECHA
*---------------------------------------------------------------
VAR.SEG.BENEF.SNOMBRE=FIELD (R.NEW(EB.SEG.NOMBRES)<1,i>," ",2,1)
VAR.SEG.BENEF.TNOMBRE=FIELD (R.NEW(EB.SEG.NOMBRES)<1,i>," ",3,1)
VAR.LEN.NOM=LEN(R.NEW(EB.SEG.NOMBRES)<1,i>)
NOMCOUNT=''
NOMCOUNT =LEN(VAR.SEG.BENEF.TNOMBRE)

VAR.COMPLEMENTO.NOM=''
VAR.COMPLEMENTO.NOM=LEN(VAR.SEG.BENEF.PNOMBRE)
VAR.COMPLEMENTO.NOM=VAR.COMPLEMENTO.NOM+1
IF NOMCOUNT GE 1 THEN
NOM.COMP=''
NOM.COMP=R.NEW(EB.SEG.NOMBRES)<1,i>
VAR.SEG.BENEF.SNOMBRE=NOM.COMP[VAR.COMPLEMENTO.NOM,VAR.LEN.NOM]
END

VAR.SEG.BENEF.PAPELLIDO=FIELD (R.NEW(EB.SEG.APELLIDOS)<1,i>," ",1,1)
VAR.SEG.BENEF.SAPELLIDO=FIELD (R.NEW(EB.SEG.APELLIDOS)<1,i>," ",2,1)
VAR.SEG.BENEF.TAPELLIDO=FIELD (R.NEW(EB.SEG.APELLIDOS)<1,i>," ",3,1)
APECOUNT=LEN(VAR.SEG.BENEF.TAPELLIDO)
VAR.LEN.APE=LEN(R.NEW(EB.SEG.APELLIDOS)<1,i>)

*VALIDAR EL LEN DE TODO EL APELLIDO Y NOMBRE PARA AGREGARSELO AL SEGUNDO NOMBRE O APELLIDO SI TIENE TERCERO
VAR.COMPLEMENTO.APE=''
VAR.COMPLEMENTO.APE=LEN(VAR.SEG.BENEF.PAPELLIDO)
VAR.COMPLEMENTO.APE=VAR.COMPLEMENTO.APE+1
IF APECOUNT GE 1 THEN
APE.COM=''
APE.COM=R.NEW(EB.SEG.APELLIDOS)<1,i>
VAR.SEG.BENEF.SAPELLIDO=APE.COM[VAR.COMPLEMENTO.APE,VAR.LEN.APE]
END

VAR.SEG.BENEF.NOIDENT=""
VAR.PREGUNTA.A=R.NEW(EB.SEG.PREGUNTA.A)
VAR.PREGUNTA.B=R.NEW(EB.SEG.PREGUNTA.B)
VAR.PREGUNTA.C=R.NEW(EB.SEG.PREGUNTA.C)
VAR.PREGUNTA.D=R.NEW(EB.SEG.PREGUNTA.D)

RETURN

DELETE_AND_OPEN:
		;* Eliminando archivo existente
		;*------------------------------
	    DELETESEQ DIR.NAME,NAME.FILE THEN
	    END
	
		;* Abriendo archivo para escritura
		;*---------------------------------
	    OPENSEQ DIR.NAME,NAME.FILE TO SEQ.PTR THEN
	        WEOFSEQ NAME.FILE
	    END
RETURN


ENVIO.TRAMA:

R.NEW(EB.SEG.RESERVADO.2)=ID.TRANSACTION

BEGIN CASE 
	CASE VAR.PREGUNTA.A EQ 'NO' AND VAR.PREGUNTA.B EQ 'NO' AND VAR.PREGUNTA.C EQ 'NO' AND VAR.PREGUNTA.D EQ 'NO'

	THIS.PACKAGE.CLASS 	="com.bancoazul.collector.Collector";* "com.bancoazul.t24colecturia.ColectorPEXMWS"
    THIS.METHOD.CLT		= "setConnect"
    ;*STR.ARR2= "1~2~~~2015-08-09~2015-08-09~2015-08-09~2015-08-09~~~~~N~100116|NUM.IDE|10931010781703|SAMUEL||Fusillo||1978-01-09|M|SINGLE|10|fusillos@gmail.com|SV|99999999|| XXXX   |SV|203|20308||SV|||10931010781703~100151|DOC.UNI|003497214|OSCAR|JAVIER|ARRIAGA|QUINTEROS|1948-02-06|M|SINGLE|1|oarriaga@gmail.com|SV|99999999|| RPTO SANTA LEONOR 76 AV. NORTE #20 G  |SV|106|10614||SV|106|10608|06140602480038|ME^CC^Cuenta Ahorro^10000000036798^USD^0^^^^~SAMUEL^ISAIAS^FUSILLO^CANJURA^19^1981-01-01^100"
    CALLJ.ARGUMENTS.CLT = "~SEGUROS AZUL~03~VENTANILLA~SEGUROS~~XML~1~":STR.ARR2
    ;*CALLJ.ARGUMENTS.CLT = '3~10~~~2015-08-09~2015-08-09~2015-08-09~2015-08-09~~~~SV0010401~psanchez@bancoazul.com~100168|NUM.IDE|06141902801108|EDUARDO|MERCADO|CASTRO|MERCADO|1980-02-19|M|SINGLE|1|psanchez@bancoazul.com|SV|25558138|77479566|COL PROVIDENCIA CALLE MADRID #714|SV|106|10614|COL PROVIDENCIA CALLE MADRID #714|SV|106|10614|06141902801108~100168|NUM.IDE|06141902801108|EDUARDO|MERCADO|CASTRO|MERCADO|1980-02-19|M|SINGLE|1|psanchez@bancoazul.com|SV|25558138|77479566|COL PROVIDENCIA CALLE MADRID #714|SV|106|10614|COL PROVIDENCIA CALLE MADRID #714|SV|106|10614|06141902801108|AN^CC^Cuenta Ahorro^1234567891123456^USD^0^^^^~NUM.IDE^06141902801108^SAMUEL^ISAIAS^FUSILLO^CANJURA^Hijo^1981-01-01^100'
    CALLJ.ERROR.SMS 	= " "
    CALLJ.RESPONSE.CLT	= " "
							    
    CALL EB.CALLJ(THIS.PACKAGE.CLASS,THIS.METHOD.CLT,CALLJ.ARGUMENTS.CLT,CALLJ.RESPONSE.CLT,CALLJ.ERROR.CLT)
    RESPUESTA.CALLJ = CALLJ.RESPONSE.CLT
    RESPUESTA=FIELD(RESPUESTA.CALLJ,'|',2)
    R.NEW(EB.SEG.COD.EJECUTIVO)=VAR.SEGURO.ALTA.USER
    BEGIN CASE
    CASE RESPUESTA EQ 'OK' 
    	R.NEW(EB.SEG.ESTADO)='ACT'
    	R.NEW(EB.SEG.RESPONSE)=RESPUESTA.CALLJ
    	R.NEW(EB.SEG.NO.CERTIFICADO)= FIELD(RESPUESTA.CALLJ,'|',3)
    	R.NEW(EB.SEG.NO.POLIZA)= FIELD(RESPUESTA.CALLJ,'|',4)     
    	R.NEW(EB.SEG.ARCHIVO.CONSENT)=FIELD(RESPUESTA.CALLJ,'|',7) 
    	
    	
    	;*Trayendo mensaje mensaje parametrizado y modificando campos para el correo
		;*-------------------------------------------------------------------------------------------------------------------------------
		VAR.NOMBRE.SEGURO=R.NEW(EB.SEG.TIPO.SEGURO)
		VAR.SUBJECT=R.GLOBAL<EB.SLV39.DESC.PARAM>
		VAR.CUERPO.CORREO=EREPLACE(EREPLACE(EREPLACE(R.GLOBAL<EB.SLV39.VALOR.PARAM>,VM:VM,CHAR(13):CHAR(13)),',':VM,',':CHAR(13)),VM,' ')
		;*Preparando reemplazo de campos
		VAR.NOMBRE.CAMPO.REPLACE='[NOMBRE.CLIENTE]':FM:'[FECHA.CARGO]':FM:'[NOMBRE.SEGURO]'
		VAR.VALOR.REPLACE=R.CUS<EB.CUS.SHORT.NAME>:FM:TODAY:FM:VAR.NOMBRE.SEGURO
    	;*Reemplazando campos
		V.CONTAR=COUNT(VAR.NOMBRE.CAMPO.REPLACE,FM)+1
		i=1
	    LOOP WHILE i LE V.CONTAR  DO
			VAR.CUERPO.CORREO=EREPLACE(VAR.CUERPO.CORREO,VAR.NOMBRE.CAMPO.REPLACE<i>,VAR.VALOR.REPLACE<i>)
			i=i+1	
		REPEAT
		;*-------------------------------------------------------------------------------------------------------------------------------
		
		;*Enviando Correo
		;*--------------------------------------------------------------------------------------------------------------------
		
    	THIS.METHOD.CLT	= "setConnect"
    	
    	CALLJ.ARGUMENTS.CLT="~SEGUROS AZUL~03~VENTANILLA~MENSAJERIA~~XML~1~":VAR.CORREO.TITULAR:"~":VAR.SUBJECT:"~":VAR.CUERPO.CORREO:"~":R.NEW(EB.SEG.ARCHIVO.CONSENT):"~":VAR.CORREO.TITULAR
    	IF VAR.SEG.ECORREO EQ 'SI' THEN
    		CALL EB.CALLJ(THIS.PACKAGE.CLASS,THIS.METHOD.CLT,CALLJ.ARGUMENTS.CLT,CALLJ.RESPONSE.CLT,CALLJ.ERROR.CLT)
    	END
    CASE 1
    	R.NEW(EB.SEG.ESTADO)=FIELD(RESPUESTA.CALLJ,'|',2) 
    	;*R.NEW(EB.SEG.RESPONSE)=FIELD(RESPUESTA.CALLJ,'|',3) 
    	R.NEW(EB.SEG.RESPONSE)=RESPUESTA.CALLJ
    END CASE
    
   CASE 1
   		R.NEW(EB.SEG.ESTADO)='ANU'
END CASE  

	V.ENQUIRY="ENQ SLV.E.SEG.IMP.DOC @ID EQ '" :ID.NEW: "'"
	CALL EB.SET.NEXT.TASK(V.ENQUIRY)  

RETURN

CONVERT.FECHA:

IF VAR.FECHA NE '' AND VAR.FECHA NE '0'  THEN
VAR.CONV.FECHA=OCONV(VAR.FECHA,'DI')
VAR.CONV.FECHA=OCONV(VAR.CONV.FECHA, 'DY'):'-':RIGHT('00':OCONV(VAR.CONV.FECHA, 'DM'),2):'-':RIGHT('00':OCONV(VAR.CONV.FECHA, 'DD'),2)
END
ELSE
VAR.CONV.FECHA=''
END

RETURN

CONVERT.KEY.PARAM:

BEGIN CASE
CASE VAR.CONV.KEY.PARAM NE '' AND VAR.CONV.KEY.PARAM NE '0' 
	CALL F.READ(FN.KEYS.PARAMS,VAR.KEY.PARAM.ID,R.KEY.PARAM,F.KEYS.PARAMS,Y.ERR)
	V.CONTAR.KEY=COUNT(R.KEY.PARAM<EB.SLV18.PARAM.ID>,VM)+1
	j=1
    LOOP WHILE j LE V.CONTAR.KEY  DO
    	PARAM.KEY=R.KEY.PARAM<EB.SLV18.PARAM.ID><1,j>
    	IF VAR.CONV.KEY.PARAM EQ PARAM.KEY THEN
			VAR.CONV.KEY.PARAM=R.KEY.PARAM<EB.SLV18.VALOR><1,j>
		END
		j=j+1	
	REPEAT
CASE 1
VAR.CONV.KEY.PARAM=''
END CASE

RETURN


*-----------------------------------------------------------------------------
    END