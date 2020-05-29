*-----------------------------------------------------------------------------
* <Rating>1372</Rating>
*-----------------------------------------------------------------------------
*-----------------------------------------------------------------------------
* Nombre: SLV.E.NOF.TC.CONTRATO        -- LA QUE ME PASO >>>>>  GERSON segun TSHOW  <<<<<<<<<
* Descripción: Rutina NOFILE para enquiry de emisión de contratos para Banca Por Internet
*-----------------------------------------------------------------------------
* Version	Paquete								Autor		Fecha		Comentario
*-----------------------------------------------------------------------------
* 1.0		AZUL001-042015.CONTRATO.SV			FBatres		04.05.15	Versión inicial.
* 1.1											RGaray		04.09.15	Se adiciona tomar en cuenta DAP y prestamos que se colocan en
*																		los campos ARRGT.TRANS y ARRGT.SEE ademas de AA.PRODA.ACCT.TRANS
* 1.2		Ticket1642_ContratoTConnect-ISOV2	RGaray		10.09.15	se adiciona capacidad para leer datos de cliente aun si acuerdo no esta autorizado
*																		se modifica algoritmo para calculo de la edad del cliente
* 1.3       Ticket								PSanchez	27.06.16	se adiciona capacidad para mostrar cuentas a visualizar
* 1.4       Ticket26342_ApoderadosLegales       R.Rodriguez 28.03.19    Se agrega lógica para Obtener datos de apoderado legal de la aplicación EB.SLV.COMPANY.APO.DATA
*-----------------------------------------------------------------------------
    SUBROUTINE SLV.E.NOF.TC.CONTRATO(ENQ.DATA)
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
*-- rprodríguez 29032019
    $INSERT I_F.EB.SLV.COMPANY.APO.DATA
    

    GOSUB INIT
    GOSUB OPENFILE
    GOSUB LOAD
    GOSUB PROCESS

    IF BANDERA EQ '1' THEN
        GOSUB GENERA.CONTRATO
    END
    RETURN

*-----------------------------------------------------------------------------
INIT:
*-----------------------------------------------------------------------------
    FN.TABLE.ACC = 'F.ACCOUNT'
    F.TABLE.ACC = ''
    FN.TABLE.CUS = 'F.CUSTOMER'
    F.TABLE.CUS = ''
    FN.TABLE.COM = 'F.COMPANY'
    F.TABLE.COM = ''
    FN.TABLE.AA = 'F.AA.ARRANGEMENT'
    F.TABLE.AA = ''
    
    FN.TABLE.AA.VIN = 'F.AA.ARRANGEMENT'
    F.TABLE.AA.VIN = ''
    FN.TABLE.AA.SEE = 'F.AA.ARRANGEMENT'
    F.TABLE.AA.SEE = ''
    FN.TABLE.AA.VER = 'F.AA.ARRANGEMENT'
    F.TABLE.AA.VER = ''
    
    FN.TABLE.PRO = 'F.AA.PRODUCT.GROUP'
    F.TABLE.PRO = ''
    FN.TABLE.PA = 'F.EB.SLV.GLOBAL.PARAM'
    F.TABLE.PA = ''
    FN.TABLE.ACU = 'F.AA.ARR.CUSTOMER'
    F.TABLE.ACU = ''
    FN.TABLE.BAL = 'F.AA.ARR.ACCOUNT'
    F.TABLE.BAL = ''
    FN.TABLE.DEP = 'F.EB.SLV.CUS.DEPARTAMENTO'
    F.TABLE.DEP = ''
    FN.TABLE.MUN = 'F.EB.SLV.CUS.MUNICIPIO'
    F.TABLE.MUN = ''
    FN.TABLE.OCU = 'F.EB.CUS.OCUPACION.SSF'
    F.TABLE.OCU = ''
    FN.TABLE.PAN = 'F.EB.SLV.PRIN.CON.ACC'
    F.TABLE.PAN = ''
    FN.AA.PRODUCT = 'F.AA.PRODUCT'
    F.AA.PRODUCT = ''
    FN.AA.PRODUCT.MANAGER = 'F.AA.PRODUCT.MANAGER'
    F.AA.PRODUCT.MANAGER = ''
    FN.AA.PRODUCT.DESIGNER = 'F.AA.PRODUCT.DESIGNER'
    F.AA.PRODUCT.DESIGNER = ''
    FN.AA.PRODUCT.ACCESS = 'F.AA.PRODUCT.ACCESS'
    F.AA.PRODUCT.ACCESS = ''
*-- rprodriguez 29032019
    FN.APO.DATA = 'F.EB.SLV.COMPANY.APO.DATA'
    F.APO.DATA = ''
    
    EQU YEAR.ON.DAYS TO '365.25'
    EQU TIME.FORMAT TO 'MTS'	
    EQU YEAR.FORMAT TO 'DY4'
    EQU MONTH.FORMAT TO 'DM'
    EQU DAY.FORMAT TO 'DD'
    EQU INTERNAL.FORMAT TO 'D'
    EQU YEAR.ON.DAYS TO '365.25'
    
    EQU TEXT.NUMBER.SEPARATOR TO ' CON '

    EQU LENGTH.DUI TO '9'
    EQU LENGTH.NIT TO '14'
*-- 
    BANDERA = '0'


;****************************************************************************
;* DEBUG
;****************************************************************************
*   DIR.NAME.SPOOL = 'C:\TEMP'
*    SS.ARR.ID = 'AA15205B419J'
*    SS.ARR.ID = 'AA152052NFTT'
*    SS.ARR.ID = 'AA15205WMHY1'
*	SS.ARR.ID = 'AA152122Q16T' 
*-- rprodriguez para realizar pruebas
*	SS.ARR.ID = 'AA172232KFT3'
*	APO.DATA.ID = 'SV0010410.APO.01'	
*--

;****************************************************************************
*----------------------------------------------------
*-- Bloque comentado por rprodriguez 28032019
*----------------------------------------------------
;* Variable apoderado legal para leer en Global Param
*    APODERADO.LEGAL = 'SV0010401' : '.APO.NOMBRE'
;* Variable apoderado legal para leer Profesión en Global Param
*    APODERADO.LEGAL.PROFESION = 'SV0010401' : '.APO.PROFE'
;* Variable apoderado legal para leer Día en Global Param
*    APODERADO.LEGAL.DIA = 'SV0010401' : '.APO.DIA.PODER'
;* Variable apoderado legal para leer Mes en Global Param
*    APODERADO.LEGAL.MES = 'SV0010401' : '.APO.MES.PODER'
;* Variable apoderado legal para leer Domicilio en Global Param
*    APODERADO.LEGAL.DOMICILIO = 'SV0010401' : '.APO.DOMICILIO'
;* Variable apoderado legal para leer fecha nacimiento en Global Param
*    APODERADO.LEGAL.FECHA = 'SV0010401' : '.APO.FECHA.NAC'
*----------------------------------------------------
*-- Fin de Bloque comentado por rprodriguez 28032019
*----------------------------------------------------    


;* Nombres de los meses del año
    SA.NOMBRE.MES = "ENERO":@FM:"FEBRERO":@FM:"MARZO":@FM:"ABRIL":@FM:"MAYO":@FM:"JUNIO":@FM:"JULIO":@FM:"AGOSTO":@FM:"SEPTIEMBRE":@FM:"OCTUBRE":@FM:"NOVIEMBRE":@FM:"DICIEMBRE"

;* Fecha actual del sistema T24.
    SD.DATE = TODAY

*----------------------------------------------------
*-- Bloque comentado por rprodriguez 28032019
*----------------------------------------------------
;* Variable apoderado legal para leer en Global Param
*    APODERADO.LEGAL = R.USER<EB.USE.COMPANY.CODE><1,1> : '.APO.NOMBRE'
;* Variable apoderado legal para leer Profesión en Global Param
*    APODERADO.LEGAL.PROFESION = R.USER<EB.USE.COMPANY.CODE><1,1> : '.APO.PROFE'
;* Variable apoderado legal para leer Día en Global Param
*    APODERADO.LEGAL.DIA = R.USER<EB.USE.COMPANY.CODE><1,1> : '.APO.DIA.PODER'
;* Variable apoderado legal para leer Mes en Global Param
*    APODERADO.LEGAL.MES = R.USER<EB.USE.COMPANY.CODE><1,1> : '.APO.MES.PODER'
;* Variable apoderado legal para leer Domicilio en Global Param
*    APODERADO.LEGAL.DOMICILIO = R.USER<EB.USE.COMPANY.CODE><1,1> : '.APO.DOMICILIO'
;* Variable apoderado legal para leer fecha nacimiento en Global Param
*    APODERADO.LEGAL.FECHA = R.USER<EB.USE.COMPANY.CODE><1,1> : '.APO.FECHA.NAC'
*----------------------------------------------------
*-- Fin de Bloque comentado por rprodriguez 28032019
*----------------------------------------------------    
    

;* DEBUG
;* Parámetro de Entrada Número de Acuerdo
    LOCATE "ARR.ID" IN D.FIELDS<1> SETTING LN.POS THEN
    SS.ARR.ID = D.RANGE.AND.VALUE<LN.POS> ;* comentar para debug 
    END

*-- rprodriguez 27032019  
    	;* Recuperando ID de Apoderado
    LOCATE 'APO.DATA.ID' IN D.FIELDS<1> SETTING LN.POS THEN
    	APO.DATA.ID = D.RANGE.AND.VALUE<LN.POS>
    END

;* Constante Nombre Contrato
    EQU BUSCAR.CONTRATO TO 'azul_banca_net_natural.jdt'
;* Constante Property Class para PRODUCT ACCESS
    EQU PROP.CLASS.PA TO 'PRODUCT.ACCESS'
    RETURN

*-----------------------------------------------------------------------------
OPENFILE:
*-----------------------------------------------------------------------------
    CALL OPF(FN.TABLE.ACC, F.TABLE.ACC)
    CALL OPF(FN.TABLE.CUS, F.TABLE.CUS)
    CALL OPF(FN.TABLE.COM, F.TABLE.COM)
    CALL OPF(FN.TABLE.AA, F.TABLE.AA)
    CALL OPF(FN.TABLE.OCU, F.TABLE.OCU)
    CALL OPF(FN.TABLE.PA, F.TABLE.PA)
    CALL OPF(FN.TABLE.ACU, F.TABLE.ACU)
    CALL OPF(FN.TABLE.BAL, F.TABLE.BAL)
    CALL OPF(FN.TABLE.DEP, F.TABLE.DEP)
    CALL OPF(FN.TABLE.MUN, F.TABLE.MUN)
    CALL OPF(FN.TABLE.PAN, F.TABLE.PAN)
    CALL OPF(FN.AA.PRODUCT.MANAGER, F.AA.PRODUCT.MANAGER)
    CALL OPF(FN.AA.PRODUCT.DESIGNER, F.AA.PRODUCT.DESIGNER)
*--rprodriguez 27032019
    CALL OPF(FN.APO.DATA, F.APO.DATA) 
    RETURN

*-----------------------------------------------------------------------------
LOAD:
*-----------------------------------------------------------------------------
;*VARIABLE DE REPORTE
    FECHA.EMISION = SD.DATE[7,2] : ' de ': SA.NOMBRE.MES<SD.DATE[5,2]> : ' de ' : SD.DATE[1,4]

;* Ruta donde se guarda el archivo
    RUTA.SPOOL.ID = 'RUTA.CONTRATO.SPOOL'

;*TODO: primera parte LINK
    RUTA.PDF.ID = 'RUTA.CONTRATO.PDF'
    RUTA.VALID.ID = 'RUTA.VALIDAR.PDF'

;*DEBUG

    CALL F.READ(FN.TABLE.PA, RUTA.SPOOL.ID, R.TABLE.PA, F.TABLE.PA, F.ERR.PA)
    DIR.NAME.SPOOL = R.TABLE.PA<EB.SLV39.VALOR.PARAM> ;* comentar para debug
;*TODO: primera parte LINK
    CALL F.READ(FN.TABLE.PA, RUTA.PDF.ID, R.TABLE.PA, F.TABLE.PA, F.ERR.PA)
    DIR.NAME.PDF = R.TABLE.PA<EB.SLV39.VALOR.PARAM>

   CALL F.READ(FN.TABLE.PA, RUTA.VALID.ID, R.TABLE.PA, F.TABLE.PA, F.ERR.PA)
    DIR.NAME.VALID = R.TABLE.PA<EB.SLV39.VALOR.PARAM>

    RETURN

*-----------------------------------------------------------------------------
PROCESS:
*-----------------------------------------------------------------------------
    DATOS = ''

;*Llave para extraer todos los datos
    NUMERO.ARREGLO = SS.ARR.ID

;* Leyendo el archivo de arreglos
    CALL F.READ(FN.TABLE.AA, NUMERO.ARREGLO, R.TABLE.AA, F.TABLE.AA, F.ERR.AA)

;*Validación de registro encontrado
    IF F.ERR.AA EQ '' THEN
        ;* Leyendo el archivo de agencias
        CALL F.READ(FN.TABLE.COM, R.TABLE.AA<AA.ARR.CO.CODE>, R.TABLE.COM, F.TABLE.COM, F.ERR.COM)
        ;* Leyendo el archivo de clientes
        CALL F.READ(FN.TABLE.CUS, R.TABLE.AA<AA.ARR.CUSTOMER>, R.TABLE.CUS, F.TABLE.CUS, F.ERR.CUS)		

        ;* Datos del Acuerdo
        PROPIETARIO.CUENTA = R.TABLE.AA<AA.ARR.CUSTOMER>
        ESTADO.ARREGLO = R.TABLE.AA<AA.ARR.ARR.STATUS>

        ;* Datos de la Agencia
        Y.POS = DCOUNT(R.TABLE.COM<EB.COM.NAME.ADDRESS>, @VM)
        AGENCIA.NOMBRE = R.TABLE.COM<EB.COM.COMPANY.NAME>
        ;*VARIABLE DE REPORTE
        LUGAR.EMISION = R.TABLE.COM<EB.COM.NAME.ADDRESS><1, Y.POS>				
		
			Y.TEMPORAL.COCODE = R.TABLE.AA<AA.ARR.CO.CODE>
			Y.TEMPORAL.ADRESSES = R.TABLE.COM<EB.COM.NAME.ADDRESS>
			Y.TEMPORAL.ADRESS.POS = R.TABLE.COM<EB.COM.NAME.ADDRESS><1, Y.POS>

        ;* Datos de Global Param
        ;* Fecha de la SSF
        CALL F.READ(FN.TABLE.PA, 'FECHA.SSF.CONT.TC', R.TABLE.PA, F.TABLE.PA, F.ERR.PA)
        ;*VARIABLE DE REPORTE
        FECHA.SSF = R.TABLE.PA<EB.SLV39.VALOR.PARAM>

        ;* Número de libro del registro comercial
        CALL F.READ(FN.TABLE.PA, 'NO.LIBR.REG.COM', R.TABLE.PA, F.TABLE.PA, F.ERR.PA)
        ;*VARIABLE DE REPORTE
        NO.LIB.REG = R.TABLE.PA<EB.SLV39.VALOR.PARAM>

        ;* Nombre del notario
        CALL F.READ(FN.TABLE.PA, 'NOM.NOTARIO', R.TABLE.PA, F.TABLE.PA, F.ERR.PA)
        ;*VARIABLE DE REPORTE
        NOMBRE.NOTARIO = R.TABLE.PA<EB.SLV39.VALOR.PARAM>

*----------------------------------------------------
*-- Bloque comentado por rprodriguez 28032019
*----------------------------------------------------
        ;* Datos del apoderado legal
*        CALL F.READ(FN.TABLE.PA, APODERADO.LEGAL, R.TABLE.PA, F.TABLE.PA, F.ERR.PA)
        ;* manejo de posible multivalor si excede una linea
*        MULTI.APO = R.TABLE.PA<EB.SLV39.VALOR.PARAM>
*        MULTI.APO.CONT = DCOUNT(MULTI.APO, @VM)
*        FOR I = 1 TO MULTI.APO.CONT
            ;*VARIABLE DE REPORTE
*            APODERADO.NOMBRE = APODERADO.NOMBRE:" ":MULTI.APO<1,I>
*        NEXT I

        ;* Fecha Apoderado Legal (Edad)
*        CALL F.READ(FN.TABLE.PA, APODERADO.LEGAL.FECHA, R.TABLE.PA, F.TABLE.PA, F.ERR.PA)
        
*       APO.FECHA.BDAY = R.TABLE.PA<EB.SLV39.VALOR.PARAM>
*        APODERADO.BDAY = OCONV(DATE(), 'DY') - APO.FECHA.BDAY[7,4]
*        IF (OCONV(DATE(), 'DM') - APO.FECHA.BDAY[4,2]) > 0 THEN
            ;*VARIABLE DE REPORTE
*            APODERADO.BDAY = APODERADO.BDAY + 1
*        END ELSE IF (OCONV(DATE(), 'DM') - APO.FECHA.BDAY[4,2]) = 0 THEN
*            IF (OCONV(DATE(), 'DD') - APO.FECHA.BDAY[1,2]) > 0 THEN
                ;*VARIABLE DE REPORTE
*                APODERADO.BDAY = APODERADO.BDAY + 1
*            END
*        END
		
		;* RGaray 20150910 inicio - cambio en calculo de edad 	
*		EDAD.CALCULO = R.TABLE.PA<EB.SLV39.VALOR.PARAM>
*		GOSUB CALCULAR.EDAD		
*		APODERADO.BDAY = EDAD.CLIENTE 
		;* RGaray 20150910 final - cambio en calculo de edad 
		
        ;* Profesión Apoderado Legal
*        CALL F.READ(FN.TABLE.PA, APODERADO.LEGAL.PROFESION, R.TABLE.PA, F.TABLE.PA, F.ERR.PA)
        ;*VARIABLE DE REPORTE
*        APODERADO.PROFESION = R.TABLE.PA<EB.SLV39.VALOR.PARAM>

        ;* Día Apoderado Legal
*        CALL F.READ(FN.TABLE.PA, APODERADO.LEGAL.DIA, R.TABLE.PA, F.TABLE.PA, F.ERR.PA)
        ;*VARIABLE DE REPORTE
*        APODERADO.DIA = R.TABLE.PA<EB.SLV39.VALOR.PARAM>

        ;* Mes Apoderado Legal
*        CALL F.READ(FN.TABLE.PA, APODERADO.LEGAL.MES, R.TABLE.PA, F.TABLE.PA, F.ERR.PA)
        ;*VARIABLE DE REPORTE
*        APODERADO.MES = R.TABLE.PA<EB.SLV39.VALOR.PARAM>

        ;* Domicilio Apoderado Legal
*        CALL F.READ(FN.TABLE.PA, APODERADO.LEGAL.DOMICILIO, R.TABLE.PA, F.TABLE.PA, F.ERR.PA)
        ;*VARIABLE DE REPORTE
*        APO.DOMICILIO = R.TABLE.PA<EB.SLV39.VALOR.PARAM>

*----------------------------------------------------
*-- Bloque comentado por rprodriguez 28032019
*----------------------------------------------------


    ;* determinando la personeria del cliente
    CALL GET.LOC.REF ('CUSTOMER', 'SEGMENT', POS)
    LS.PERSONERIA = R.TABLE.CUS<EB.CUS.LOCAL.REF, POS>

    IF ESTADO.ARREGLO EQ 'AUTH' THEN
            ;*CALL F.READ(FN.TABLE.ACU, BUSCAR.CLIENTE, R.TABLE.ACU, F.TABLE.ACU, F.ERR.ACU)
            ;* solucion192
            CALL AA.GET.ARRANGEMENT.CONDITIONS(NUMERO.ARREGLO, 'CUSTOMER', 'CUSTOMER', TODAY, R.PRE.ID, R.PRE.CUSTOMER, F.ERR.ACU)
            R.TABLE.ACU = RAISE(R.PRE.CUSTOMER)

    END
    ELSE
        BUSCAR.CLIENTE = R.ACC<AC.ARRANGEMENT.ID> : '-CUSTOMER-' : R.TABLE.AA<AA.ARR.START.DATE> : '.1'
        CALL F.READ('F.AA.ARR.CUSTOMER$NAU', BUSCAR.CLIENTE, R.TABLE.ACU, F.TABLE.ACU, F.ERR.ACU)
    END
    
*-- Obtiene datos de apoderado legal rprodriguez 28032019
	GOSUB GET_APO_DATA
    GOSUB OBTENER.REP.LEGAL
    GOSUB OBTENER.CLIENTE
    GOSUB OBTENER.CUENTAS
    GOSUB OBTENER.TARIFAS   

;*BANDERA PARA DETERMINAR SI EJECUTA GENERA.CONTRATO
    BANDERA = '1'

    END

    RETURN
*-- rprodriguez 28032019    
*----------------------------------------------------
* Obtiene Info de Apoderado legal
*   rprodriguez 28032019
*----------------------------------------------------
GET_APO_DATA:
	TEXT.MONTH.NAMES = 'ENERO' : @FM : 'FEBRERO' : @FM : 'MARZO' : @FM : 'ABRIL' : @FM : 'MAYO' : @FM : 'JUNIO' : @FM : 'JULIO' : @FM : 'AGOSTO' : @FM : 'SEPTIEMBRE' : @FM : 'OCTUBRE' : @FM : 'NOVIEMBRE' : @FM : 'DICIEMBRE'
	;* Recuperando el registro del Apoderado
	CALL F.READ(FN.APO.DATA, APO.DATA.ID, R.APO.DATA, F.APO.DATA, ERR.APO.DATA)

	;* Nombre de Apoderado
	APODERADO.NOMBRE = TRIM(R.APO.DATA<EB.SLV45.APO.NAME, 1>)
	CRT 'APODERADO NOMBRE --> ' : APODERADO.NOMBRE
*	GOSUB WRITE_LOG_FILE

	;* Fecha de Nacimiento para Apoderado
	FECHA.BDAY = TRIM(R.APO.DATA<EB.SLV45.APO.BIRTHDATE, 1>)
	CRT 'APO FECHA.BDAY --> ' : FECHA.BDAY
*	GOSUB WRITE_LOG_FILE

	;* Edad de Apoderado
	BIRTH.DATE = FECHA.BDAY
	GOSUB GET_AGE
	APODERADO.BDAY = AGE.YEARS
	CRT 'APODERADO EDAD --> ' : APODERADO.BDAY
*	GOSUB WRITE_LOG_FILE

	;* Ocupación del Apoderado
	APODERADO.PROFESION = TRIM(R.APO.DATA<EB.SLV45.APO.CAREER, 1>)
*	GOSUB WRITE_LOG_FILE
	CRT 'APODERADO PROFESION --> ' : APODERADO.PROFESION
	;* Departamento del Apoderado
	APO.DEPT.ID = TRIM(R.APO.DATA<EB.SLV45.APO.DEPT, 1>)
*	GOSUB WRITE_LOG_FILE

	;* Código del Departamento

	CALL F.READ(FN.TABLE.DEP, APO.DEPT.ID, R.TABLE.DEP, F.TABLE.DEP, F.ERR.DEP)
*	GOSUB WRITE_LOG_FILE

	;* Descripción del Departamento
	APO.DEPT = TRIM(R.TABLE.DEP<EB.CUS17.DESCRIPTION, 1>)
*	GOSUB WRITE_LOG_FILE

	;* Municipio (Dirección) del Apoderado
	APO.ADDRESS.ID = TRIM(R.APO.DATA<EB.SLV45.APO.ADDRESS, 1>)
*	GOSUB WRITE_LOG_FILE

	;* Código del Municipio
	CALL F.READ(FN.TABLE.MUN, APO.ADDRESS.ID, R.TABLE.MUN, F.TABLE.MUN, F.ERR.MUN)
*	GOSUB WRITE_LOG_FILE

	;* Descripción del Municipio
	APO.DOMICILIO = TRIM(R.TABLE.MUN<EB.CUS17.DESCRIPTION, 1>)
	APO.DOMICILIO = APO.DOMICILIO :',':APO.DEPT
	CRT 'APODERADO DOMICILIO --> ' : APO.DOMICILIO
*	GOSUB WRITE_LOG_FILE

	;* DUI Almacenado para el Apoderado
	APO.DUI.SAVED = TRIM(R.APO.DATA<EB.SLV45.APO.DUI, 1>)
*	GOSUB WRITE_LOG_FILE

	;* DUI del Apoderado
	STR.IN = APO.DUI.SAVED
	GOSUB GET_DUI_WITH_FORMAT
	APO.DUI = DUI.FORMATTED
    CRT 'APODERADO DUI --> ' : APO.DUI

	;* NIT Almacenado para el Apoderado
	APO.NIT.SAVED = TRIM(R.APO.DATA<EB.SLV45.APO.NIT, 1>)
*	GOSUB WRITE_LOG_FILE

	;* NIT del Apoderado
	STR.IN = APO.NIT.SAVED
*	GOSUB GET_NIT_WITH_FORMAT
	APO.NIT = NIT.FORMATTED

	;* Fecha Almacenada de Autorización para Apoderado
	APO.AUTH.DATE.SAVED = TRIM(R.APO.DATA<EB.SLV45.APO.AUTHDATE, 1>)
    CRT 'APO.AUTH.DATE.SAVED --> ' : APO.AUTH.DATE.SAVED	
*	GOSUB WRITE_LOG_FILE
 
	;* Año de Autorización

	APO.AUTH.YEAR = TRIM(OCONV(ICONV(APO.AUTH.DATE.SAVED, INTERNAL.FORMAT), YEAR.FORMAT))
    CRT 'APO.AUTH.YEAR --> ' : APO.AUTH.YEAR	
*	GOSUB WRITE_LOG_FILE

	STR.IN = APO.AUTH.YEAR
	CALL SLV.UTIL.IMP.GET.NUMLETRAS(STR.IN)
	APO.AUTH.YEAR.TXT = FIELD(STR.IN, TEXT.NUMBER.SEPARATOR, 1)
	CRT 'APO.AUTH.YEAR.TXT --> ' : APO.AUTH.YEAR.TXT
*	GOSUB WRITE_LOG_FILE

	;* Mes de Autorización
	APO.AUTH.MONTH = TRIM(OCONV(ICONV(APO.AUTH.DATE.SAVED, INTERNAL.FORMAT), MONTH.FORMAT))
	CRT 'APO.AUTH.MONTH --> ' : APO.AUTH.MONTH
*	GOSUB WRITE_LOG_FILE

	;* Mes de Autorización en Letras
	APODERADO.MES = TEXT.MONTH.NAMES<APO.AUTH.MONTH>
*	GOSUB WRITE_LOG_FILE
	CRT 'APODERADO MES --> ' : APODERADO.MES

	;* Día de Autorización
	APO.AUTH.DAY = TRIM(OCONV(ICONV(APO.AUTH.DATE.SAVED, INTERNAL.FORMAT), DAY.FORMAT))
	CRT 'APO.AUTH.DAY --> ' : APO.AUTH.DAY
		
*	GOSUB WRITE_LOG_FILE

	STR.IN = APO.AUTH.DAY
	CALL SLV.UTIL.IMP.GET.NUMLETRAS(STR.IN)
	APODERADO.DIA = FIELD(STR.IN, TEXT.NUMBER.SEPARATOR, 1)
	CRT 'APODERADO DIA --> ' : APODERADO.DIA
*	GOSUB WRITE_LOG_FILE

	;* Número de Registro de Comercio
	APO.AUTH.NRC = TRIM(R.APO.DATA<EB.SLV45.APO.NRC, 1>)
	Y.TXT = 'APO.AUTH.NRC = ' : APO.AUTH.NRC
*	GOSUB WRITE_LOG_FILE

	;* Número de Libro para Registro de Otros Contratos Mercantiles
	APO.AUTH.LRCM = TRIM(R.APO.DATA<EB.SLV45.APO.LRCM, 1>)
	Y.TXT = 'APO.AUTH.LRCM = ' : APO.AUTH.LRCM
*	GOSUB WRITE_LOG_FILE

RETURN
*----------------------------------------------------
* Fin de Obtiene Info de Apoderado legal
*   rprodriguez 28032019
*----------------------------------------------------
*------------------------------------------------------------------------------
;* Obtener Edad Actual a partir de una Fecha de Nacimiento
GET_AGE:
*	TODAY.DATE = '20181120'
	TODAY.DATE = TODAY
*	Y.TXT = 'TODAY.DATE = ' : TODAY.DATE
*	GOSUB WRITE_LOG_FILE

	;* Definir el cálculo para Días Calendario
	DAYS.DIFF.CAL = "C"
    CRT 'APO TODAY.DATE --> ' : TODAY.DATE
    CRT 'APO BIRTH.DATE --> ' : BIRTH.DATE
    IF BIRTH.DATE NE '' AND BIRTH.DATE < TODAY.DATE THEN
        CALL CDD('', BIRTH.DATE, TODAY.DATE, DAYS.DIFF.CAL)
    END
*	Y.TXT = 'DAYS.DIFF.CAL = ' : DAYS.DIFF.CAL
*	GOSUB WRITE_LOG_FILEQ

    IF BIRTH.DATE EQ '' THEN
    	AGE.YEARS = ''
    END ELSE
        CRT 'APO DAYS.DIFF.CAL --> ' : DAYS.DIFF.CAL   
    	AGE.YEARS = INT(DIV(DAYS.DIFF.CAL, YEAR.ON.DAYS))
    	CRT 'APO AGE.YEARS --> ' : AGE.YEARS  
    END
*	Y.TXT = 'AGE.YEARS = ' : AGE.YEARS
*	GOSUB WRITE_LOG_FILE	
RETURN
*------------------------------------------------------------------------------
;* Convertir Código de DUI a Texto
GET_DUI_WITH_FORMAT:
*	Y.TXT = 'STR.IN = ' : STR.IN
*	GOSUB WRITE_LOG_FILE

	;* Formateando DUI
	STR.LENGTH.OUT = LENGTH.DUI
	STR.FILL.CHAR = CREDENTIALS.FILL.CHAR
	STR.FILL.SIDE = CREDENTIALS.FILL.LEFT
	GOSUB FILL_TEXT

	;* DUI con Formato
	DUI.FILLED = STR.OUT
*	Y.TXT = 'DUI.FILLED = ' : DUI.FILLED
*	GOSUB WRITE_LOG_FILE
	IF DUI.FILLED NE '' THEN
		DUI.FORMATTED = DUI.FILLED[1, 8] : CREDENTIALS.SEPARATOR : DUI.FILLED[9, 1]
	END ELSE
		DUI.FORMATTED = DUI.FILLED
	END
*	Y.TXT = 'DUI.FORMATTED = ' : DUI.FORMATTED
*	GOSUB WRITE_LOG_FILE
RETURN
*------------------------------------------------------------------------------
;* Agregar Caracteres a un Texto
FILL_TEXT:
*	Y.TXT = 'STR.IN = ' : STR.IN
*	GOSUB WRITE_LOG_FILE

	STR.OUT = ''
	STR.FILL.TMP = ''
	STR.LENGTH.IN = LEN(STR.IN)
	STR.LENGTH.TMP = ABS(STR.LENGTH.OUT - STR.LENGTH.IN)

	Y.AUX = 0
    LOOP WHILE Y.AUX LT STR.LENGTH.TMP DO
		STR.FILL.TMP = STR.FILL.TMP : STR.FILL.CHAR
		Y.AUX = Y.AUX + 1
    REPEAT

    IF STR.FILL.SIDE EQ 'L' THEN
		STR.OUT = STR.FILL.TMP : STR.IN
    END ELSE
    	STR.OUT = STR.IN : STR.FILL.TMP
    END
*	Y.TXT = 'STR.OUT = ' : STR.OUT
*	GOSUB WRITE_LOG_FILE
RETURN
*------------------------------------------------------------------------------
*-----------------------------------------------------------------------------
OBTENER.CLIENTE:
*---- Obtener la informacion general del cliente dueño del Contrato de Banca en Línea
*-----------------------------------------------------------------------------
    COD.CLIENTE = R.TABLE.ACU<AA.CUS.PRIMARY.OWNER>
*    CALL F.READ(FN.TABLE.CUS, COD.CLIENTE, R.TABLE.CUS, F.TABLE.CUS, F.ERR.CUS) ;* RGaray se comento porque parece no necesario, hay una llamada previa

    PRIMER.NOMBRE = TRIM(R.TABLE.CUS<EB.CUS.NAME.1>)
    SEGUNDO.NOMBRE = TRIM(R.TABLE.CUS<EB.CUS.NAME.2>)
    TERCER.NOMBRE = TRIM(R.TABLE.CUS<EB.CUS.GIVEN.NAMES>)
    PRIMER.APELLIDO = TRIM(R.TABLE.CUS<EB.CUS.TEXT>)
    SEGUNDO.APELLIDO = TRIM(R.TABLE.CUS<EB.CUS.FAMILY.NAME>)
    APELLIDO.CASADA = TRIM(R.TABLE.CUS<EB.CUS.PREVIOUS.NAME>)
    NOMBRES = PRIMER.NOMBRE : ' ' : SEGUNDO.NOMBRE : ' ' : TERCER.NOMBRE
    APELLIDOS = PRIMER.APELLIDO : ' ' : SEGUNDO.APELLIDO : ' ' : APELLIDO.CASADA
;*VARIABLE DE REPORTE
    CLIENTE = TRIM(NOMBRES : ' ' : APELLIDOS)
    DATOS:= 'CLIENTE1!' : CLIENTE : CHAR(10)
    
;* Calculando la edad del cliente principal
*    EDAD.CLIENTE = OCONV(DATE(), 'DY') - SUBSTRINGS(R.TABLE.CUS<EB.CUS.DATE.OF.BIRTH>, 1, 4)
*    IF OCONV(DATE(), 'DM') - SUBSTRINGS(R.TABLE.CUS<EB.CUS.DATE.OF.BIRTH>, 6, 2) > 0 THEN
*        ;*VARIABLE DE REPORTE
*        EDAD.CLIENTE = EDAD.CLIENTE + 1
*    END ELSE IF OCONV(DATE(), 'DM') - SUBSTRINGS(R.TABLE.CUS<EB.CUS.DATE.OF.BIRTH>, 6, 2) = 0 THEN
*        IF OCONV(DATE(), 'DD') - SUBSTRINGS(R.TABLE.CUS<EB.CUS.DATE.OF.BIRTH>, 4, 2) > 0 THEN
*            ;*VARIABLE DE REPORTE
*            EDAD.CLIENTE = EDAD.CLIENTE + 1
*        END
*    END
*    
	
	;* RGaray 20150910 inicio - cambio en calculo de edad 	
		EDAD.CALCULO = R.TABLE.CUS<EB.CUS.DATE.OF.BIRTH>
		GOSUB CALCULAR.EDAD		
		DATOS := 'EDADCLI1!' : EDAD.CLIENTE : CHAR(10)
	;* RGaray 20150910 final - cambio en calculo de edad 
	
;* Encontrando el municipio del propietario
    CALL GET.LOC.REF('CUSTOMER', 'LF.MUNICIPIO', POS)
    COD.MUNICIPIO = R.TABLE.CUS<EB.CUS.LOCAL.REF, POS>
    CALL F.READ(FN.TABLE.MUN, COD.MUNICIPIO, R.TABLE.MUN, F.TABLE.MUN, F.ERR.MUN)
;*VARIABLE DE REPORTE
    MUNI.CLIENTE = R.TABLE.MUN<EB.SLV33.DESCRIPTION>
    DATOS := 'MUNICICL!' : MUNI.CLIENTE : CHAR(10)

;* Encontrando el departamento del propietario
    CALL GET.LOC.REF ('CUSTOMER', 'LF.DEPARTAMENTO', POS)
    COD.DEPARTAMENTO = R.TABLE.CUS<EB.CUS.LOCAL.REF, POS>
    CALL F.READ(FN.TABLE.DEP, COD.DEPARTAMENTO, R.TABLE.DEP, F.TABLE.DEP, F.ERR.DEP)
;*VARIABLE DE REPORTE
    DEPARTAMENTO.CLIENTE = R.TABLE.DEP<EB.SLV43.DESCRIPTION>
    DATOS := 'DEPTOCLI!' : DEPARTAMENTO.CLIENTE : CHAR(10)

;* Encontrando el número y tipo de documento principal
    FINDSTR 'DOCTO.UNICO.IDENT' IN R.TABLE.CUS<EB.CUS.LEGAL.DOC.NAME> SETTING AP, VP THEN
        ;*VARIABLE DE REPORTE
        DOC.CLIENTE = R.TABLE.CUS<EB.CUS.LEGAL.ID, VP>
        ;*VARIABLE DE REPORTE
        TIPO.DOC.CLIENTE = 'DUI'
    END
    ELSE FINDSTR 'PASSPORT' IN R.TABLE.CUS<EB.CUS.LEGAL.DOC.NAME> SETTING AP, VP THEN
;*VARIABLE DE REPORTE
    DOC.CLIENTE = R.TABLE.CUS<EB.CUS.LEGAL.ID, VP>
;*VARIABLE DE REPORTE
    TIPO.DOC.CLIENTE = 'pasaporte'
    END
    ELSE FINDSTR 'CARNET.RESIDENTE' IN R.TABLE.CUS<EB.CUS.LEGAL.DOC.NAME> SETTING AP, VP THEN
;*VARIABLE DE REPORTE
    DOC.CLIENTE = R.TABLE.CUS<EB.CUS.LEGAL.ID, VP>
;*VARIABLE DE REPORTE
    TIPO.DOC.CLIENTE = 'Carnet de Residente'
    END
    ELSE FINDSTR 'CARNET.MENOR.EDAD' IN R.TABLE.CUS<EB.CUS.LEGAL.DOC.NAME> SETTING AP, VP THEN
;*VARIABLE DE REPORTE
    DOC.CLIENTE = R.TABLE.CUS<EB.CUS.LEGAL.ID, VP>
;*VARIABLE DE REPORTE
    TIPO.DOC.CLIENTE = 'Carnet de Menor de Edad'
    END
    ELSE
    DOC.CLIENTE = '_______________'
    TIPO.DOC.CLIENTE = '_______________'
    END

;* Encontrando el número de NIT
    FINDSTR 'NUM.IDEN.TRIBUT' IN R.TABLE.CUS<EB.CUS.LEGAL.DOC.NAME> SETTING AP, VP THEN
        ;*VARIABLE DE REPORTE
        NIT.TIT = R.TABLE.CUS<EB.CUS.LEGAL.ID, VP>
    END ELSE
        NIT.TIT = '______________'
    END

    DATOS := 'TIPODOC1!' : TIPO.DOC.CLIENTE : CHAR(10)
    DATOS := 'NUMERDO1!' : DOC.CLIENTE : CHAR(10)
    DATOS := 'NITTITU1!' : NIT.TIT : CHAR(10)

;* Encontrando la profesion
;*VARIABLE DE REPORTE
    OCUPACION.REP = R.TABLE.CUS<EB.CUS.OCCUPATION>
    DATOS := 'PROFESI1!' : OCUPACION.REP : CHAR(10)

    RETURN

*-----------------------------------------------------------------------------
OBTENER.TARIFAS:
*---- Obtener las comisiones para Banca por Internet
*-----------------------------------------------------------------------------
;* Buscando las tarifas
    CALL F.READ(FN.TABLE.PA, 'TC.PN1', R.TABLE.PA, F.TABLE.PA, F.ERR.PA)
    DATOS := 'TARIFA01!' : R.TABLE.PA<EB.SLV39.VALOR.PARAM> : CHAR(10)
    CALL F.READ(FN.TABLE.PA, 'TC.PN2', R.TABLE.PA, F.TABLE.PA, F.ERR.PA)
    DATOS := 'TARIFA02!' : R.TABLE.PA<EB.SLV39.VALOR.PARAM> : CHAR(10)

    RETURN


*-----------------------------------------------------------------------------
GENERA.CONTRATO:
*-----------------------------------------------------------------------------

    NAME.ID = 'CON' : '-' : SS.ARR.ID : '-' : R.USER<EB.USE.DEPARTMENT.CODE> : '-' : (TIMESTAMP() * 1000)

    R.ID.TXT := NAME.ID : '.txt'
;*TODO: parte 2 LINK
    R.ID.PDF := NAME.ID : '.pdf'

;* Plantilla a utilizar
;* CALL F.READ(FN.TABLE.PA, BUSCAR.CONTRATO, R.TABLE.PA, F.TABLE.PA, F.ERR.PA)
;* PLANTILLA = '(' : R.TABLE.PA<EB.SLV39.VALOR.PARAM> : ') STARTLM'
    PLANTILLA = '(' : BUSCAR.CONTRATO : ') STARTLM'

;* Armando la cadena para el archivo
    CADENA = '%!' : CHAR(10)
    CADENA := PLANTILLA : CHAR(10)
    CADENA := 'APOBANCO!' : UPCASE(APODERADO.NOMBRE) : CHAR(10)
    CADENA := 'EDADAPO0!' : APODERADO.BDAY : CHAR(10)
    CADENA := 'PROFAPO0!' : UPCASE(APODERADO.PROFESION) : CHAR(10)
    CADENA := 'DOMIAPO0!' : UPCASE(APO.DOMICILIO) : CHAR(10)
    CADENA := 'DIAPODER!' : APODERADO.DIA : CHAR(10)
    CADENA := 'MESPODER!' : APODERADO.MES : CHAR(10)
    CADENA := 'NOTARIOS!' : UPCASE(NOMBRE.NOTARIO) : CHAR(10)
    CADENA := 'LIBROREG!' : NO.LIB.REG : CHAR(10)

    CADENA := 'CUENSERV!' : CUENTA.SERVICIOS : CHAR(10)
    CADENA := 'CUENCARG!' : CUENTA.CARGOS : CHAR(10)
    CADENA := UPCASE(DATOS)
    CADENA := 'LUGAREMI!' : UPCASE(LUGAR.EMISION) : CHAR(10)
    CADENA := 'FECHAEMI!' : UPCASE(FECHA.EMISION) : CHAR(10)
    CADENA := 'FECHASSF!' : FECHA.SSF : CHAR(10)

    GOSUB LIMPIA.CADENA
    CADENA := 'TIPOCUEN!' : CUENTAS.TIPO : CHAR(10)
    CADENA := '%%EOF'
    CADENA = TRIM(CADENA)
    
;* Creando el archivo
    OPENSEQ DIR.NAME.SPOOL, R.ID.TXT TO SEQ.PTR ELSE
        CREATE SEQ.PTR ELSE
        E = 'EB-SLV.DIR.WRT.FAIL'
        CALL ERR
    END
    END

;* Escribiendo la cadena en el archivo
    WRITESEQ CADENA ON SEQ.PTR ELSE
    E = 'EB-SLV.DAT.WRT.FAIL'
    CALL ERR
    END

;* Cerrando el archivo
    CLOSESEQ SEQ.PTR

***********************************************************************
;* bloque adicionado para pruebas de rutina que valida si existe el pdf
    PERFIL.ARCHIVO = DIR.NAME.VALID : R.ID.PDF
    CALL SLV.E.NOF.GEN.FILEEXIST(PERFIL.ARCHIVO)
    IF PERFIL.ARCHIVO EQ 1 THEN
        STR.ARR := DIR.NAME.PDF : R.ID.PDF
        CRT 'ARCHIVO ENCONTRADO.'
    END ELSE
        STR.ARR := "No fue posible generar el archivo. Contacte al administrador del sistema."
        CRT 'ARCHIVO PERDIDO.'
    END
***********************************************************************

;* Insert de línea en ENQUIRY resultante
*    STR.ARR = SS.CUENTA.ID : "*"
*    STR.ARR := TITULAR.CUENTA : "*"
*    STR.ARR := SS.DOCUMENT.NAME : "*"
*    STR.ARR := DIR.NAME.PDF : R.ID.PDF

    ENQ.DATA<-1>=STR.ARR
    RETURN


*-----------------------------------------------------------------------------
LIMPIA.CADENA:
*-----------------------------------------------------------------------------
;* Codificar caracteres especiales
    CADENA = CHANGE(CADENA, 'Ñ', CHAR(465))
    CADENA = CHANGE(CADENA, 'ñ', CHAR(497))
;* tratamiento de letras tildadas o semejantes
    CADENA = CHANGE(CADENA, CHAR(193), 'A')
    CADENA = CHANGE(CADENA, CHAR(201), 'E')
    CADENA = CHANGE(CADENA, CHAR(205), 'I')
    CADENA = CHANGE(CADENA, CHAR(211), 'O')
    CADENA = CHANGE(CADENA, CHAR(218), 'U')
    CADENA = CHANGE(CADENA, CHAR(192), 'A')
    CADENA = CHANGE(CADENA, CHAR(200), 'E')
    CADENA = CHANGE(CADENA, CHAR(204), 'I')
    CADENA = CHANGE(CADENA, CHAR(210), 'O')
    CADENA = CHANGE(CADENA, CHAR(217), 'U')
    CADENA = CHANGE(CADENA, CHAR(220), 'U')
    CADENA = CHANGE(CADENA, CHAR(219), 'U')
    CADENA = CHANGE(CADENA, CHAR(214), 'O')
    CADENA = CHANGE(CADENA, CHAR(212), 'O')
    CADENA = CHANGE(CADENA, CHAR(207), 'I')
    CADENA = CHANGE(CADENA, CHAR(206), 'I')
    CADENA = CHANGE(CADENA, CHAR(203), 'E')

    CADENA = CHANGE(CADENA, CHAR(225), 'A')
    CADENA = CHANGE(CADENA, CHAR(233), 'E')
    CADENA = CHANGE(CADENA, CHAR(237), 'I')
    CADENA = CHANGE(CADENA, CHAR(243), 'O')
    CADENA = CHANGE(CADENA, CHAR(250), 'U')
    RETURN

*-----------------------------------------------------------------------------
OBTENER.REP.LEGAL:
*-----------------------------------------------------------------------------

;* obtener representante legal si existe
    CALL GET.LOC.REF ('AA.ARR.CUSTOMER', 'LF.REP.LEGAL', POS)
    COD.REP.LEGAL = R.TABLE.ACU<AA.CUS.LOCAL.REF, POS>
    IF COD.REP.LEGAL EQ '' THEN
        ;*VARIABLE DE REPORTE
        DATOS := 'SENALREP!' : '' : CHAR(10)
    END
    ELSE
;*VARIABLE DE REPORTE
    DATOS := 'SENALREP!' : '1' : CHAR(10)

    CALL F.READ(FN.TABLE.CUS, COD.REP.LEGAL, R.TABLE.CUS, F.TABLE.CUS, F.ERR.CUS)
    PRIMER.NOMBRE = TRIM(R.TABLE.CUS<EB.CUS.NAME.1>)
    SEGUNDO.NOMBRE = TRIM(R.TABLE.CUS<EB.CUS.NAME.2>)
    TERCER.NOMBRE = TRIM(R.TABLE.CUS<EB.CUS.GIVEN.NAMES>)
    PRIMER.APELLIDO = TRIM(R.TABLE.CUS<EB.CUS.TEXT>)
    SEGUNDO.APELLIDO = TRIM(R.TABLE.CUS<EB.CUS.FAMILY.NAME>)
    APELLIDO.CASADA = TRIM(R.TABLE.CUS<EB.CUS.PREVIOUS.NAME>)
    NOMBRES = PRIMER.NOMBRE : ' ' : SEGUNDO.NOMBRE : ' ' : TERCER.NOMBRE
    APELLIDOS = PRIMER.APELLIDO : ' ' : SEGUNDO.APELLIDO : ' ' : APELLIDO.CASADA

;*VARIABLE DE REPORTE
    REP.LEGAL = TRIM(NOMBRES : ' ' : APELLIDOS)
    DATOS := 'REPRESEN!' : REP.LEGAL : CHAR(10)

;* Calculando la edad del representante legal
*    EDAD.REP.LEGAL = OCONV(DATE(), 'DY') - SUBSTRINGS(R.TABLE.CUS<EB.CUS.DATE.OF.BIRTH>, 1, 4)
*    IF OCONV(DATE(), 'DM') - SUBSTRINGS(R.TABLE.CUS<EB.CUS.DATE.OF.BIRTH>, 6, 2) > 0 THEN
*        ;*VARIABLE DE REPORTE
*        EDAD.REP.LEGAL = EDAD.REP.LEGAL + 1
*    END ELSE IF OCONV(DATE(), 'DM') - SUBSTRINGS(R.TABLE.CUS<EB.CUS.DATE.OF.BIRTH>, 6, 2) = 0 THEN
*        IF OCONV(DATE(), 'DD') - SUBSTRINGS(R.TABLE.CUS<EB.CUS.DATE.OF.BIRTH>, 4, 2) > 0 THEN
*            ;*VARIABLE DE REPORTE
*            EDAD.REP.LEGAL = EDAD.REP.LEGAL + 1
*        END
*    END
    
	
	;* RGaray 20150910 inicio - cambio en calculo de edad 	
		EDAD.CALCULO = R.TABLE.CUS<EB.CUS.DATE.OF.BIRTH>
		GOSUB CALCULAR.EDAD		
		EDAD.REP.LEGAL = EDAD.CLIENTE 
		DATOS := 'EDADREPR!' : EDAD.REP.LEGAL : CHAR(10)
	;* RGaray 20150910 final - cambio en calculo de edad 
	
;* Encontrando la profesion
;*VARIABLE DE REPORTE
    OCUPACION.REP = R.TABLE.CUS<EB.CUS.OCCUPATION>
    DATOS := 'PROFREPR!' : OCUPACION.REP : CHAR(10)

;* Encontrando el municipio del representante legal
    CALL GET.LOC.REF ('CUSTOMER', 'LF.MUNICIPIO', POS)
    COD.MUNICIPIO = R.TABLE.CUS<EB.CUS.LOCAL.REF, POS>
    CALL F.READ(FN.TABLE.MUN, COD.MUNICIPIO, R.TABLE.MUN, F.TABLE.MUN, F.ERR.MUN)
;*VARIABLE DE REPORTE
    MUNICIPIO.REP = R.TABLE.MUN<EB.SLV33.DESCRIPTION>
    DATOS := 'MUNICREP!' : MUNICIPIO.REP : CHAR(10)

;* Encontrando el departamento del representante legal
    CALL GET.LOC.REF ('CUSTOMER', 'LF.DEPARTAMENTO', POS)
    COD.DEPARTAMENTO = R.TABLE.CUS<EB.CUS.LOCAL.REF, POS>
    CALL F.READ(FN.TABLE.DEP, COD.DEPARTAMENTO, R.TABLE.DEP, F.TABLE.DEP, F.ERR.DEP)
;*VARIABLE DE REPORTE
    DEPARTAMENTO.REP = R.TABLE.DEP<EB.SLV43.DESCRIPTION>
    DATOS := 'DEPTOREP!' : DEPARTAMENTO.REP : CHAR(10)


;* Encontrando el número de documento
    FINDSTR 'DOCTO.UNICO.IDENT' IN R.TABLE.CUS<EB.CUS.LEGAL.DOC.NAME> SETTING AP, VP THEN
        ;*VARIABLE DE REPORTE
        DOC.REP = R.TABLE.CUS<EB.CUS.LEGAL.ID, VP>
        TIPO.DOC.REP = 'DUI'
    END
    ELSE FINDSTR 'PASSPORT' IN R.TABLE.CUS<EB.CUS.LEGAL.DOC.NAME> SETTING AP, VP THEN
;*VARIABLE DE REPORTE
    DOC.REP = R.TABLE.CUS<EB.CUS.LEGAL.ID, VP>
    TIPO.DOC.REP = 'pasaporte'
    END
    ELSE
    DOC.REP = '_______________'
    TIPO.DOC.REP = '_______________'
    END
    DATOS := 'TIPDOCRE!' : TIPO.DOC.REP : CHAR(10)
    DATOS := 'NUMDOCRE!' : DOC.REP : CHAR(10)

;* Encontrando el número de NIT
    FINDSTR 'NUM.IDEN.TRIBUT' IN R.TABLE.CUS<EB.CUS.LEGAL.DOC.NAME> SETTING AP, VP THEN
        ;*VARIABLE DE REPORTE
        NIT.REP = R.TABLE.CUS<EB.CUS.LEGAL.ID, VP>
    END ELSE

        NIT.REP = '______________'
    END
    DATOS := 'NITREPLE!' : NIT.REP : CHAR(10)
    END
;* solucion192


    RETURN

*-----------------------------------------------------------------------------
OBTENER.CUENTAS:
*-----------------------------------------------------------------------------
    CALL AA.GET.ARRANGEMENT.CONDITIONS(NUMERO.ARREGLO, PROP.CLASS.PA, '', TODAY, RETURN.IDS, RETURN.CONDITIONS, RETURN.ERROR)
    R.CONDITION = RAISE(RETURN.CONDITIONS)
    Y.CUENTAS.VINCULADAS = R.CONDITION<AA.PRODA.ACCT.TRANS>
        
    ;* adicionado RGaray 20150904 - inicio 
    Y.ARR.VINC.TRANS = R.CONDITION<AA.PRODA.ARRGT.TRANS>    
    Y.ARR.VINC.SEE = R.CONDITION<AA.PRODA.ARRGT.SEE> 
    Y.CUENTA.SEE = R.CONDITION<AA.PRODA.ACCT.SEE>
   
    CUENTA.SERVICIOS = SWAP(Y.CUENTAS.VINCULADAS,@VM,', ')
    ;*--Cambio a mostrar las visualizar
    IF Y.CUENTA.SEE NE '' THEN
    CUENTA.SERVICIOS.SEE = SWAP(Y.CUENTA.SEE,@VM,', ')
    CUENTA.SERVICIOS = CUENTA.SERVICIOS :', ': CUENTA.SERVICIOS.SEE
    END
    ;*--hasta aca
    
    GOSUB CAMBIAR.ARR.ACCT
    
    ;* adicionado RGaray 20150904 - fin
    
    
    CUENTA.CARGOS = Y.CUENTAS.VINCULADAS<1,1>
    
    GOSUB IDENTIFICAR.CUENTAS
    RETURN

*-----------------------------------------------------------------------------
CAMBIAR.ARR.ACCT:
*-----------------------------------------------------------------------------
	;* adiciona acuerdos transaccionales 
	LOOP
        REMOVE ARR.ID.TMP FROM Y.ARR.VINC.TRANS SETTING POS.AV
    WHILE ARR.ID.TMP
    	CALL F.READ(FN.TABLE.AA.VIN, ARR.ID.TMP, R.ARR.VIN, F.TABLE.AA.VIN, F.ERR.AA.VIN)
    	Y.ACCT.NUMBER = R.ARR.VIN<AA.ARR.LINKED.APPL.ID>
    	
    	IF LEN(TRIM(CUENTA.SERVICIOS)) EQ 0 THEN 
    		CUENTA.SERVICIOS = CUENTA.SERVICIOS : Y.ACCT.NUMBER 
    		Y.CUENTAS.VINCULADAS = Y.CUENTAS.VINCULADAS : Y.ACCT.NUMBER 
    	END 
    	ELSE  
    		CUENTA.SERVICIOS = CUENTA.SERVICIOS : ', ' : Y.ACCT.NUMBER 
    		Y.CUENTAS.VINCULADAS = Y.CUENTAS.VINCULADAS : @VM : Y.ACCT.NUMBER 
    	END 
     
    REPEAT 
    ;* adiciona acuerdos de consulta  
    LOOP
        REMOVE ARR.ID.SEE FROM Y.ARR.VINC.SEE SETTING POS.SEE
    WHILE ARR.ID.SEE
    	CALL F.READ(FN.TABLE.AA.SEE, ARR.ID.SEE, R.ARR.SEE, F.TABLE.AA.SEE, F.ERR.AA.SEE)
    	Y.ACCT.NUMBER = R.ARR.SEE<AA.ARR.LINKED.APPL.ID>
    	
    	IF LEN(TRIM(CUENTA.SERVICIOS)) EQ 0 THEN 
    		CUENTA.SERVICIOS = CUENTA.SERVICIOS : Y.ACCT.NUMBER 
    		Y.CUENTAS.VINCULADAS = Y.CUENTAS.VINCULADAS : Y.ACCT.NUMBER 
    	END 
    	ELSE  
    		CUENTA.SERVICIOS = CUENTA.SERVICIOS : ', ' : Y.ACCT.NUMBER 
    		Y.CUENTAS.VINCULADAS = Y.CUENTAS.VINCULADAS : @VM : Y.ACCT.NUMBER
    	END 
     
    REPEAT
      ;* adiciona cuentas de consulta visualizar
      
      
    
  
*-----------------------------------------------------------------------------
IDENTIFICAR.CUENTAS:
*-----------------------------------------------------------------------------
    B.AH = '0'
    B.CC = '0'
    B.PP = '0'
    B.DP = '0'

    LOOP
        REMOVE ACC.ID FROM Y.CUENTAS.VINCULADAS SETTING POS.CV
    WHILE ACC.ID
        CALL F.READ(FN.TABLE.ACC,ACC.ID,R.ACC,F.TABLE.ACC,ACC.ERR)
        CATEGORY = R.ACC<AC.CATEGORY>
        IF CATEGORY GE 1000 AND CATEGORY LE 1999 THEN
            ;* Corriente
            B.CC = '1'
        END
        ELSE
        IF CATEGORY GE 6000 AND CATEGORY LE 6499 THEN
            ;* Ahorro
            B.AH = '1'
        END
        ELSE
        IF CATEGORY GE 3000 AND CATEGORY LE 3999 THEN
            ;* Préstamo
            B.PP = '1'
        END
        ELSE
        IF CATEGORY GE 6500 AND CATEGORY LE 6999 THEN
            ;* Depósito a plazo
            B.DP = '1'
        END
    END
    END
    END
    REPEAT

    IF B.CC EQ '1' THEN
        TIPO.CUENTAS = 'Corriente'
    END
    IF B.AH EQ '1' THEN
        IF TIPO.CUENTAS NE '' THEN
            TIPO.CUENTAS := ', Ahorro'
        END
        ELSE
        TIPO.CUENTAS := 'Ahorro'
    END
    END
    IF B.PP EQ '1' THEN
        IF TIPO.CUENTAS NE '' THEN
            TIPO.CUENTAS := ', ': 'Pr':CHAR(489):'stamo'
        END
        ELSE
        TIPO.CUENTAS := 'Pr':CHAR(489):'stamo'
    END
    END
    IF B.DP EQ '1' THEN
        IF TIPO.CUENTAS NE '' THEN
            TIPO.CUENTAS := ', ': 'Dep':CHAR(499):'sito a Plazo'
        END
        ELSE
        TIPO.CUENTAS := 'Dep':CHAR(499):'sito a Plazo'
    END
    END
    
*TIPO.CUENTAS := ', PrÃ©stamo'
*TIPO.CUENTAS := 'Pr':CHAR(489):'stamo';*
*TIPO.CUENTAS := ', Préstamo'
*TIPO.CUENTAS := 'Dep':CHAR(499):'sito a Plazo';*
*TIPO.CUENTAS := 'Depósito a Plazo'
*TIPO.CUENTAS := 'DepÃ³sito a Plazo'

    CUENTAS.TIPO = TIPO.CUENTAS
    RETURN


*-----------------------------------------------------------------------------
CALCULAR.EDAD:
*-----------------------------------------------------------------------------
	
	;* ADICIONAL PARA PRUEBA EDAD RGaray 20150724 - INICIO 
			
		;*EDAD.ANIO = SUBSTRINGS(EDAD.CALCULO, 1, 4)
		;*EDAD.DIA = SUBSTRINGS(EDAD.CALCULO, 6, 2)
		;*EDAD.MES = SUBSTRINGS(EDAD.CALCULO, 4, 2)
		
		EDAD.ANIO.2 = SUBSTRINGS(EDAD.CALCULO, 1, 4)
		EDAD.DIA.2 = SUBSTRINGS(EDAD.CALCULO, 7, 2)
		EDAD.MES.2 = SUBSTRINGS(EDAD.CALCULO, 5, 2)
		FECHA.REF.DY = OCONV(DATE(), 'DY')


		FECHA.REF.DM = OCONV(DATE(), 'DM')
		FECHA.REF.DD = OCONV(DATE(), 'DD')
				
		;* Calculando la edad del cliente principal
	    EDAD.CLIENTE = FECHA.REF.DY - EDAD.ANIO.2 - 1
	    IF FECHA.REF.DM - EDAD.MES.2 > 0 THEN
	        EDAD.CLIENTE = EDAD.CLIENTE + 1
	    END ELSE IF FECHA.REF.DM - EDAD.MES.2 = 0 THEN
	        IF FECHA.REF.DD - EDAD.DIA.2 >= 0 THEN

	            EDAD.CLIENTE = EDAD.CLIENTE + 1
	        END
	    END
		;* ADICIONAL PARA PRUEBA EDAD RGaray 20150724 - FINAL 
		
		RETURN 


    RETURN
