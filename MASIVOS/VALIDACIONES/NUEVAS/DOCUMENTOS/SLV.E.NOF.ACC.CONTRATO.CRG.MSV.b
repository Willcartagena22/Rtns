*-----------------------------------------------------------------------------
* <Rating>8316</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.E.NOF.ACC.CONTRATO.CRG.MSV(SS.CUENTA.ID,SS.DOCUMENT.SELECTION,ITEMS.MASIVO.ID,ID.MASTER.MASIVOS,SS.CUSTOMER.ID)
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
*--------------------------------------------------------------
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
    $INSERT I_F.AA.INTEREST
    $INSERT I_F.EB.SLV.ITEMS.MASIVOS

    GOSUB INIT
    GOSUB OPENFILE
    GOSUB LOAD
    GOSUB PROCESS

    CRT 'TITULAR.CUENTA -> ' : TITULAR.CUENTA

    IF SS.DOCUMENT.SELECTION = 'DOCUMENTO-CONTRATO-APERTURA' THEN
        SS.DOCUMENT.NAME = 'Contrato de Apertura y Registro de Firmas'
        GOSUB GENERA.CONTRATO

        SS.DOCUMENT.NAME = 'Comprobante de Deposito Inicial'
        GOSUB GENERA.DEPOSITO

    END
    ELSE IF SS.DOCUMENT.SELECTION = 'DOCUMENTO-REGISTRO-FIRMAS' THEN
    SS.DOCUMENT.NAME = 'Registro de Firmas'
    GOSUB GENERA.CONTRATO
    END
    ;*GOSUB UPDATE.ITEM.RECORD
    CRT 'FIN -> ':SS.CUENTA.ID
    RETURN

*-----------------------------------------------------------------------------
INIT:
*-----------------------------------------------------------------------------
    FN.TABLE.ACC           = 'F.ACCOUNT'
    F.TABLE.ACC            = ''
    FN.TABLE.CUS           = 'F.CUSTOMER'
    F.TABLE.CUS            = ''
    FN.TABLE.COM           = 'F.COMPANY'
    F.TABLE.COM            = ''
    FN.TABLE.AA            = 'F.AA.ARRANGEMENT'
    F.TABLE.AA             = ''
    FN.TABLE.PRO           = 'F.AA.PRODUCT.GROUP'
    F.TABLE.PRO            = ''
    FN.TABLE.PA            = 'F.EB.SLV.GLOBAL.PARAM'
    F.TABLE.PA             = ''
    FN.TABLE.ACU           = 'F.AA.ARR.CUSTOMER'
    F.TABLE.ACU            = ''
    FN.TABLE.BAL           = 'F.AA.ARR.ACCOUNT'
    F.TABLE.BAL            = ''
    FN.TABLE.DEP           = 'F.EB.SLV.CUS.DEPARTAMENTO'
    F.TABLE.DEP            = ''
    FN.TABLE.MUN           = 'F.EB.SLV.CUS.MUNICIPIO'
    F.TABLE.MUN            = ''
    FN.TABLE.OCU           = 'F.EB.CUS.OCUPACION.SSF'
    F.TABLE.OCU            = ''
    FN.TABLE.PAN           = 'F.EB.SLV.PRIN.CON.ACC'
    F.TABLE.PAN            = ''
    FN.AA.PRODUCT          = 'F.AA.PRODUCT'
    F.AA.PRODUCT           = ''
    FN.AA.PRODUCT.MANAGER  = 'F.AA.PRODUCT.MANAGER'
    F.AA.PRODUCT.MANAGER   = ''
    FN.AA.PRODUCT.DESIGNER = 'F.AA.PRODUCT.DESIGNER'
    F.AA.PRODUCT.DESIGNER  = ''
    FN.ITEMS.MASIVOS       = 'F.EB.SLV.ITEMS.MASIVOS'
    F.ITEMS.MASIVOS        = ''
;* Nombres de los meses del año
    SA.NOMBRE.MES = "ENERO":@FM:"FEBRERO":@FM:"MARZO":@FM:"ABRIL":@FM:"MAYO":@FM:"JUNIO":@FM:"JULIO":@FM:"AGOSTO":@FM:"SEPTIEMBRE":@FM:"OCTUBRE":@FM:"NOVIEMBRE":@FM:"DICIEMBRE"

;* Fecha actual del sistema T24.
    SD.DATE = TODAY

*    LOCATE "ACCOUNT.ID" IN D.FIELDS<1> SETTING LN.POS THEN
*    SS.CUENTA.ID = D.RANGE.AND.VALUE<LN.POS>
*    END
*
*    LOCATE "DOCUMENT.SELECTION" IN D.FIELDS<1> SETTING LN.POS THEN
*    SS.DOCUMENT.SELECTION = D.RANGE.AND.VALUE<LN.POS>
*    END

*-----------------------------------------------------------------------------
* Datos de prueba
*-----------------------------------------------------------------------------
;* DEBUG
* SS.CUENTA.ID = '10000000008662'
;* SS.CUENTA.ID = '10000000073251'
;* SS.CUENTA.ID = '10000000001293'
*SS.DOCUMENT.SELECTION = 'DOCUMENTO-CONTRATO-APERTURA'
*SS.CUENTA.ID = '10000000039754'
*SS.CUENTA.ID = '10000000117178'
*SS.CUENTA.ID = '10000000117248'
*SS.CUENTA.ID = '10000000038375'
*SS.CUENTA.ID = '10000000061849'
*	SS.CUENTA.ID = '10000000003067' ;* PJ
*	SS.CUENTA.ID = '10000000073437' ;* AH
*	SS.CUENTA.ID = '10000000073267' ;* AHORRO ELECTRONICA
*   SS.CUENTA.ID = '10000000003067'		;* cta. cte. PJ
* SS.CUENTA.ID = '10000000036291'		;* cta. cte. clasica PN
* SS.DOCUMENT.SELECTION = 'DOCUMENTO-CONTRATO-APERTURA'
* SS.DOCUMENT.SELECTION = 'DOCUMENTO-REGISTRO-FIRMAS'
*SS.CUENTA.ID = '10000000108152';* mancomunada
*SS.CUENTA.ID = '10000000108478';* mancomunada
*SS.CUENTA.ID = '10000000077052'
*SS.CUENTA.ID = '10000000042925' ;* LIOF Corriente empresa
*SS.CUENTA.ID = '10000000045142' ;* Cuenta de Ahorro Electrónica - PJ
*SS.CUENTA.ID = '10000000045681'

*SS.DOCUMENT.SELECTION = 'DOCUMENTO-CONTRATO-APERTURA'
*SS.CUENTA.ID = '10000000780012'
*SS.CUENTA.ID = '10000000549922'
*-----------------------------------------------------------------------------

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
    CALL OPF(FN.ITEMS.MASIVOS,F.ITEMS.MASIVOS)
    RETURN

*-----------------------------------------------------------------------------
LOAD:
*-----------------------------------------------------------------------------
;* Obtienendo la fecha actual con el formato adecuado
;* Obtienendo la fecha actual con el formato adecuado
    FECHA.EMISION = SD.DATE[7,2] : ' de ': SA.NOMBRE.MES<SD.DATE[5,2]> : ' de ' : SD.DATE[1,4]

    RETURN

*-----------------------------------------------------------------------------
PROCESS:
*-----------------------------------------------------------------------------
    DATOS = ''

;* Leyendo el archivo de cuentas
    CALL F.READ(FN.TABLE.ACC, SS.CUENTA.ID, R.TABLE.ACC, F.TABLE.ACC, F.ERR.ACC)
    IF R.TABLE.ACC EQ '' THEN
        RETURN
    END

    NUMERO.ARREGLO = R.TABLE.ACC<AC.ARRANGEMENT.ID>

;* Leyendo el archivo de arreglos
    CALL F.READ(FN.TABLE.AA, NUMERO.ARREGLO, R.TABLE.AA, F.TABLE.AA, F.ERR.AA)
    PROPIETARIO.CUENTA = R.TABLE.AA<AA.ARR.CUSTOMER>
    ESTADO.ARREGLO = R.TABLE.AA<AA.ARR.ARR.STATUS>

;* Leyendo el archivo de agencias
    CALL F.READ(FN.TABLE.COM, R.TABLE.AA<AA.ARR.CO.CODE>, R.TABLE.COM, F.TABLE.COM, F.ERR.COM)

;* Leyendo el archivo de clientes
    CALL F.READ(FN.TABLE.CUS, R.TABLE.AA<AA.ARR.CUSTOMER>, R.TABLE.CUS, F.TABLE.CUS, F.ERR.CUS)

;* Recuperando el número de cuenta
    NUMERO.CUENTA = R.TABLE.AA<AA.ARR.LINKED.APPL.ID>

;* Encontrando datos de la agencia
    Y.POS = DCOUNT(R.TABLE.COM<EB.COM.NAME.ADDRESS>, @VM)
    AGENCIA.NOMBRE = R.TABLE.COM<EB.COM.COMPANY.NAME>
    LUGAR.EMISION = R.TABLE.COM<EB.COM.NAME.ADDRESS><1, Y.POS>

;* Fecha de la SSF
;*CALL F.READ(FN.TABLE.PA, 'FECHA.SSF.CONT', R.TABLE.PA, F.TABLE.PA, F.ERR.PA)
;*FECHA.SSF = R.TABLE.PA<EB.SLV39.VALOR.PARAM>

;* Número de libro del registro comercial
    CALL F.READ(FN.TABLE.PA, 'NO.LIBR.REG.COM', R.TABLE.PA, F.TABLE.PA, F.ERR.PA)
    NO.LIB.REG = R.TABLE.PA<EB.SLV39.VALOR.PARAM>

;* Nombre del notario
    CALL F.READ(FN.TABLE.PA, 'NOM.NOTARIO', R.TABLE.PA, F.TABLE.PA, F.ERR.PA)
    NOMBRE.NOTARIO = R.TABLE.PA<EB.SLV39.VALOR.PARAM>

;* Datos del apoderado legal SV0010001.APO.NOMBRE
    CALL F.READ(FN.TABLE.PA, R.USER<EB.USE.COMPANY.CODE><1,1> : '.APO.NOMBRE', R.TABLE.PA, F.TABLE.PA, F.ERR.PA)
    ;*CALL F.READ(FN.TABLE.PA, 'SV0010001.APO.NOMBRE', R.TABLE.PA, F.TABLE.PA, F.ERR.PA)
    
;* obtener personeria 20150218
;* leer datos de titular primario en CUSTOMER
    CALL F.READ(FN.TABLE.CUS, PROPIETARIO.CUENTA, R.TABLE.PRS, F.TABLE.CUS, F.ERR.CUS)

;* determinando la personeria del cliente
    CALL GET.LOC.REF ('CUSTOMER', 'SEGMENT', POS)
    LS.PERSONERIA = R.TABLE.PRS<EB.CUS.LOCAL.REF, POS>

;* Obtener razon social de la empresa
    CALL GET.LOC.REF ('CUSTOMER', 'LF.RAZON.SOCIAL', POS)
    LS.RAZON.SOCIAL = R.TABLE.PRS<EB.CUS.LOCAL.REF, POS>

    CRT 'LS.PERSONERIA -> ' : LS.PERSONERIA
    CRT 'LS.RAZON.SOCIAL -> ' : LS.RAZON.SOCIAL

;* manejo de posible multivalor si excede una linea
    MULTI.APO = R.TABLE.PA<EB.SLV39.VALOR.PARAM>
    MULTI.APO.CONT = DCOUNT(MULTI.APO, @VM)
    FOR I = 1 TO MULTI.APO.CONT
        APODERADO.NOMBRE = APODERADO.NOMBRE:" ":MULTI.APO<1,I>
    NEXT I

;*APODERADO.NOMBRE = R.TABLE.PA<EB.SLV39.VALOR.PARAM>

    CALL F.READ(FN.TABLE.PA, R.USER<EB.USE.COMPANY.CODE><1,1> : '.APO.FECHA.NAC', R.TABLE.PA, F.TABLE.PA, F.ERR.PA)
    ;*CALL F.READ(FN.TABLE.PA,'SV0010001.APO.FECHA.NAC', R.TABLE.PA, F.TABLE.PA, F.ERR.PA)
  

    APO.FECHA.BDAY = R.TABLE.PA<EB.SLV39.VALOR.PARAM>
    APODERADO.BDAY = OCONV(DATE(), 'DY') - APO.FECHA.BDAY[7,4]
    IF (OCONV(DATE(), 'DM') - APO.FECHA.BDAY[4,2]) > 0 THEN
        APODERADO.BDAY = APODERADO.BDAY + 1
    END ELSE IF (OCONV(DATE(), 'DM') - APO.FECHA.BDAY[4,2]) = 0 THEN
        IF (OCONV(DATE(), 'DD') - APO.FECHA.BDAY[1,2]) > 0 THEN
            APODERADO.BDAY = APODERADO.BDAY + 1
        END
    END


    CALL F.READ(FN.TABLE.PA, R.USER<EB.USE.COMPANY.CODE><1,1> : '.APO.PROFE', R.TABLE.PA, F.TABLE.PA, F.ERR.PA)
    APODERADO.PROFESION = R.TABLE.PA<EB.SLV39.VALOR.PARAM>

    CALL F.READ(FN.TABLE.PA, R.USER<EB.USE.COMPANY.CODE><1,1> : '.APO.DIA.PODER', R.TABLE.PA, F.TABLE.PA, F.ERR.PA)
    APODERADO.DIA = R.TABLE.PA<EB.SLV39.VALOR.PARAM>

    CALL F.READ(FN.TABLE.PA, R.USER<EB.USE.COMPANY.CODE><1,1> : '.APO.MES.PODER', R.TABLE.PA, F.TABLE.PA, F.ERR.PA)
    APODERADO.MES = R.TABLE.PA<EB.SLV39.VALOR.PARAM>

    CALL F.READ(FN.TABLE.PA, R.USER<EB.USE.COMPANY.CODE><1,1> : '.APO.DOMICILIO', R.TABLE.PA, F.TABLE.PA, F.ERR.PA)
    APO.DOMICILIO = R.TABLE.PA<EB.SLV39.VALOR.PARAM>

;* Obteniendo el tipo de producto
    PRODUCTO = R.TABLE.AA<AA.ARR.PRODUCT>
    LINEA.PRODUCTO = R.TABLE.AA<AA.ARR.PRODUCT.LINE>
    GRUPO.PRODUCTO = R.TABLE.AA<AA.ARR.PRODUCT.GROUP>

    CRT 'PRODUCTO -> ' : PRODUCTO
    CRT 'LINEA.PRODUCTO -> ' : LINEA.PRODUCTO
    CRT 'GRUPO.PRODUCTO -> ' : GRUPO.PRODUCTO

    CALL F.READ(FN.AA.PRODUCT, PRODUCTO, R.AA.PRODUCT, F.AA.PRODUCT, F.ERR.APR)
    LS.NOMBRE.PRODUCTO = R.AA.PRODUCT<AA.PDT.DESCRIPTION>

    CRT 'LS.NOMBRE.PRODUCTO -> ' : LS.NOMBRE.PRODUCTO

;* Monto mínimo de apertura del producto
    STMT.PRODUCT = "SELECT " : FN.AA.PRODUCT.DESIGNER

    CALL EB.READLIST(STMT.PRODUCT, PRODUCT.LIST, '', NO.OF.RECS.PRO, Y.ERR)
    CALL GET.LOC.REF('AA.PRODUCT.DESIGNER', 'LF.MONTO.APER', POS)

    FOR K=1 TO NO.OF.RECS.PRO
        CALL F.READ(FN.AA.PRODUCT.DESIGNER,  PRODUCT.LIST<K>, R.AA.PRODUCT.DESIGNER, F.AA.PRODUCT.DESIGNER, ERR.PROD.DES)
        IF PRODUCT.LIST<K>[1,LEN(PRODUCT.LIST<K>)-9] EQ R.TABLE.AA<AA.ARR.PRODUCT> THEN
            ;*Campos locales
            ARR_LOCAL_FIELD_PRD = (R.PROD.DES)

            CRT 	ARR_LOCAL_FIELD_PRD
            CALL GET.LOC.REF('AA.PRODUCT.DESIGNER', 'LF.MONTO.APER', POST)
            CRT ARR_LOCAL_FIELD_PRD<AA.PRD.LOCAL.REF><1,POST>
            CRT POST

            LN.MONTO.MINIMO.APERTURA = FMT(R.AA.PRODUCT.DESIGNER<AA.PRD.LOCAL.REF, POST>, 'R2')

            BREAK
        END
    NEXT K



;*CALL F.READ(FN.AA.PRODUCT.MANAGER, PRODUCTO, R.AA.PRODUCT.MANAGER, F.AA.PRODUCT.MANAGER, ERR.PMNG)
;*ID.PRFECHA = R.AA.PRODUCT.MANAGER<AA.PM.AVAILABLE.DATE>
;*ID.DESIGNER = PRODUCTO : '-' : ID.PRFECHA

;*CALL F.READ( FN.AA.PRODUCT.DESIGNER, ID.DESIGNER, R.AA.PRODUCT.DESIGNER,  F.AA.PRODUCT.DESIGNER, ERR.PDES)
;*CALL GET.LOC.REF('AA.PRODUCT.DESIGNER', 'LF.MONTO.APER', POS)
;*LN.MONTO.MINIMO.APERTURA = FMT(R.AA.PRODUCT.DESIGNER<AA.PRD.LOCAL.REF, POS>, 'R2')


;* Monto de apertura, beneficiarios
    CALL AA.GET.ARRANGEMENT.CONDITIONS(NUMERO.ARREGLO, 'ACCOUNT', 'BALANCE', SD.DATE, ID.BAL, RETORNA.BAL, ERR.BAL)
    CALL GET.LOC.REF('AA.ARR.ACCOUNT', 'LF.MONTO.APER', POS.ACO)
    CALL GET.LOC.REF('AA.ARR.ACCOUNT', 'LF.BENEFICIARIO', POS.BENEFICIARIOS)
    CALL GET.LOC.REF('AA.ARR.ACCOUNT', 'LF.PARENTESCO', POS.PARENTESCO)
    CALL GET.LOC.REF('AA.ARR.ACCOUNT', 'LF.PORCENTAJE', POS.PORCENTAJE)

    MONTO.APERTURA = FMT(RETORNA.BAL<1, AA.AC.LOCAL.REF, POS.ACO>, 'R2')

;* Llamando la clase para convertir números a letras
    PAQUETE.CLASE = 'com.slv.ba.convertidor.NumeroLetrasFiscal'
    METODO = 'convertNumberToLetter'
    ARGUMENTOS = MONTO.APERTURA
    CALLJ.ERROR = ''
    CALLJ.RESPONSE = ''

;* Llamando a clase con método estático
    CALL EB.CALLJ(PAQUETE.CLASE, '$' : METODO, ARGUMENTOS, CALLJ.RESPONSE, CALLJ.ERROR)
    IF CALLJ.ERROR THEN
        ETEXT = CALLJ.ERROR
        CALL STORE.END.ERROR
    END
    MONTO = '$' : MONTO.APERTURA : ' (' : TRIM(CALLJ.RESPONSE) : ')'

;*** Tasa de Interest
    REC.INTEREST2 = ''
    INTEREST.ERROR2 = ''
    CALL AA.GET.ARRANGEMENT.CONDITIONS(NUMERO.ARREGLO,'INTEREST','CRINTEREST',TODAY,INTEREST.ID2, REC.INTEREST2,INTEREST.ERROR2)

    Y.INTEREST2 = RAISE(REC.INTEREST2)

    COUNT.INTEREST = DCOUNT(Y.INTEREST2<AA.INT.EFFECTIVE.RATE>,@VM)

    FOR I.INTEREST = 1 TO COUNT.INTEREST

        IF I.INTEREST = COUNT.INTEREST THEN
            IF Y.INTEREST2<AA.INT.TIER.AMOUNT,I.INTEREST> = '' THEN
                N_INTEREST=Y.INTEREST2<AA.INT.EFFECTIVE.RATE,I.INTEREST>
                N_INTEREST_OR = N_INTEREST
            END
        END

        IF Y.INTEREST2<AA.INT.TIER.AMOUNT,I.INTEREST> GT MONTO.APERTURA THEN
            N_INTEREST=Y.INTEREST2<AA.INT.EFFECTIVE.RATE,I.INTEREST>
            IF N_INTEREST = '' THEN
                N_INTEREST = 0
            END
            N_INTEREST_OR = N_INTEREST
            BREAK
        END
    NEXT I.INTEREST


    CRT 'TASA ':MONTO.APERTURA:" _": N_INTEREST

;* Buscando los beneficiarios e instrucciones especiales
    CALL AA.GET.ARRANGEMENT.CONDITIONS(NUMERO.ARREGLO, 'CUSTOMER', 'CUSTOMER', SD.DATE, ID.XUS, RETORNA.XUS, ERR.XUS)
    CALL GET.LOC.REF('AA.ARR.CUSTOMER', 'LF.CTAS.INSTRUC', POS.CTAS.INSTRUC)
    INSTRUCCION.ESPECIAL = RETORNA.XUS<1, AA.CUS.LOCAL.REF, POS.CTAS.INSTRUC>

    NOMBRES.BENEFICIARIOS = RETORNA.BAL<1, AA.AC.LOCAL.REF, POS.BENEFICIARIOS>
    PARENTESCOS.BENEFICIARIOS = RETORNA.BAL<1, AA.AC.LOCAL.REF, POS.PARENTESCO>
    PORCENTAJES.BENEFICIARIOS = RETORNA.BAL<1, AA.AC.LOCAL.REF, POS.PORCENTAJE>

    BENEFICIARIOS = ''

    LOOP
        REMOVE ID.NAM FROM NOMBRES.BENEFICIARIOS SETTING POS.NAM
        REMOVE ID.PAR FROM PARENTESCOS.BENEFICIARIOS SETTING POS.PAR
        REMOVE ID.POR FROM PORCENTAJES.BENEFICIARIOS SETTING POS.POR
    WHILE ID.NAM NE ''
        BENEFICIARIOS := 'BENEFICI!' : ID.NAM : '!' : ID.PAR : '!' : FMT(ID.POR, 'R2') : '%' : '!' : CHAR(10)
    REPEAT

;* 20151014 - se recupera el valor de category para la cuenta
    R.TABLE.ACCOUNT = RAISE(RETORNA.BAL)
    AA.ARR.ACC.CATEGORY = R.TABLE.ACCOUNT<AA.AC.CATEGORY>

;* Buscando a los propietarios de las cuentas
    BUSCAR.CLIENTE = R.TABLE.ACC<AC.ARRANGEMENT.ID> : '-CUSTOMER-' : R.TABLE.AA<AA.ARR.START.DATE> : '.1'

    IF ESTADO.ARREGLO EQ 'AUTH' THEN
        ;*CALL F.READ(FN.TABLE.ACU,            BUSCAR.CLIENTE, R.TABLE.ACU, F.TABLE.ACU, F.ERR.ACU)
        ;* solucion192
        CALL AA.GET.ARRANGEMENT.CONDITIONS(NUMERO.ARREGLO, 'CUSTOMER', 'CUSTOMER', TODAY, R.PRE.ID, R.PRE.CUSTOMER, F.ERR.ACU)
        R.TABLE.ACU = RAISE(R.PRE.CUSTOMER)

    END
    ELSE
    CALL F.READ('F.AA.ARR.CUSTOMER$NAU', BUSCAR.CLIENTE, R.TABLE.ACU, F.TABLE.ACU, F.ERR.ACU)
    END

    GOSUB OBTENER.REP.LEGAL

;* Buscando los propietarios de las cuenta
;*********************************************************************************
    LS.TIPO.RELACION = 'TITULAR'

    CALL AA.GET.ARRANGEMENT.CONDITIONS(NUMERO.ARREGLO, 'CUSTOMER', 'CUSTOMER', SD.DATE, ID.ACU, RETORNA.ACU, ERR.ACU)
    CODIGOS.CLIENTES =  PROPIETARIO.CUENTA : @VM : RETORNA.ACU<1, AA.CUS.OTHER.PARTY>
    TITULARES = ''
    I = 1
    LN.CORRELATIVO = 1
    LOOP
        REMOVE ID.COD FROM CODIGOS.CLIENTES SETTING POS.COD
    WHILE ID.COD NE ''
        CALL F.READ(FN.TABLE.CUS, ID.COD, R.TABLE.CODS, F.TABLE.CUS, F.ERR.CODS)

        PRI.NOMBRE = R.TABLE.CODS<EB.CUS.NAME.1>
        SEG.NOMBRE = R.TABLE.CODS<EB.CUS.NAME.2>
        TER.NOMBRE = R.TABLE.CODS<EB.CUS.GIVEN.NAMES>
        PRI.APELLIDO = R.TABLE.CODS<EB.CUS.TEXT>
        SEG.APELLIDO = R.TABLE.CODS<EB.CUS.FAMILY.NAME>
        APELL.CASADA = R.TABLE.CODS<EB.CUS.PREVIOUS.NAME>
        NOMB = PRI.NOMBRE : ' ' : SEG.NOMBRE : ' ' : TER.NOMBRE
        APELL = PRI.APELLIDO : ' ' : SEG.APELLIDO : ' ' : APELL.CASADA
        NOMB.CLIENT = TRIM(NOMB : ' ' : APELL)
        ;* adicionado para obtener firma titular
        *        CALL GET.LOC.REF('AA.ARR.CUSTOMER', 'LF.AUT.COND', POS.FIR.TIT)
        *    	FIR.TITULAR = RETORNA.ACU<1, AA.CUS.LOCAL.REF, POS.FIR.TIT>

        FINDSTR 'DOCTO.UNICO.IDENT' IN R.TABLE.CODS<EB.CUS.LEGAL.DOC.NAME> SETTING AP, VP THEN
            NUM.DUI = R.TABLE.CODS<EB.CUS.LEGAL.ID><1, VP>
        END ELSE
            NUM.DUI = ''
        END

        FINDSTR 'NUM.IDEN.TRIBUT' IN R.TABLE.CODS<EB.CUS.LEGAL.DOC.NAME> SETTING AP.NIT, VP.NIT THEN
            NUM.NIT = R.TABLE.CODS<EB.CUS.LEGAL.ID, VP.NIT>
        END ELSE
            NUM.NIT = ''
        END

        TITULARES := 'NUMCLIE' : I : '!' : ID.COD : CHAR(10)
        TITULARES := 'TIPOREL' : I : '!' : LS.TIPO.RELACION : ' ' : LN.CORRELATIVO : ' ' : CHAR(10)

        ;* INFORMACION DEL TITULAR PRIMARIO
        ;* En caso de persona juridica el nombre es razon social
        IF LS.PERSONERIA EQ '2' THEN
            TITULARES := 'TITULAR' : I : '!' : LS.RAZON.SOCIAL : CHAR(10)
            TITULARES := 'FIRTITU' : I : '!' : FIRMA.REP : CHAR(10)
            CRT 'TITULAR' : I : ' -> ' : LS.RAZON.SOCIAL
        END
        ELSE
        TITULARES := 'TITULAR' : I : '!' : NOMB.CLIENT : CHAR(10)
        TITULARES := 'FIRTITU' : I : '!' : FIR.TITULAR : CHAR(10)
        CRT 'TITULAR' : I : ' -> ' : NOMB.CLIENT
    END

    TITULARES := 'DUITITU' : I : '!' : NUM.DUI : CHAR(10)
    TITULARES := 'NITTITU' : I : '!' : NUM.NIT : CHAR(10)

    I += 1
    LN.CORRELATIVO += 1
    REPEAT

;* Buscando los autorizados de las cuenta
;*********************************************************************************
    LS.TIPO.RELACION = 'AUTORIZADO'
    LN.CORRELATIVO = 1

    CALL GET.LOC.REF('AA.ARR.CUSTOMER', 'LF.AC.AUTORIZAD', POS.AUTORIZADO)
    CALL GET.LOC.REF('AA.ARR.CUSTOMER', 'LF.AUT.COND.AUT', POS.FIRMAS)
    CODIGOS.CLIENTES = RETORNA.ACU<1, AA.CUS.LOCAL.REF, POS.AUTORIZADO>
    FIRMAS.CLIENTES = RETORNA.ACU<1, AA.CUS.LOCAL.REF, POS.FIRMAS>
    LOOP
        REMOVE ID.COD FROM CODIGOS.CLIENTES SETTING POS.COD
        REMOVE ID.FIR FROM FIRMAS.CLIENTES SETTING POS.FIR
    WHILE ID.COD NE ''
        CALL F.READ(FN.TABLE.CUS, ID.COD, R.TABLE.CODS, F.TABLE.CUS, F.ERR.CODS)
        PRI.NOMBRE = R.TABLE.CODS<EB.CUS.NAME.1>
        SEG.NOMBRE = R.TABLE.CODS<EB.CUS.NAME.2>
        TER.NOMBRE = R.TABLE.CODS<EB.CUS.GIVEN.NAMES>
        PRI.APELLIDO = R.TABLE.CODS<EB.CUS.TEXT>
        SEG.APELLIDO = R.TABLE.CODS<EB.CUS.FAMILY.NAME>
        APELL.CASADA = R.TABLE.CODS<EB.CUS.PREVIOUS.NAME>
        NOMB = PRI.NOMBRE : ' ' : SEG.NOMBRE : ' ' : TER.NOMBRE
        APELL = PRI.APELLIDO : ' ' : SEG.APELLIDO : ' ' : APELL.CASADA
        NOMB.CLIENT = TRIM(NOMB : ' ' : APELL) 

        FINDSTR 'DOCTO.UNICO.IDENT' IN R.TABLE.CODS<EB.CUS.LEGAL.DOC.NAME> SETTING AP, VP THEN
            NUM.DUI = R.TABLE.CODS<EB.CUS.LEGAL.ID><1, VP>
        END ELSE
            NUM.DUI = ''
        END

        FINDSTR 'NUM.IDEN.TRIBUT' IN R.TABLE.CODS<EB.CUS.LEGAL.DOC.NAME> SETTING AP.NIT, VP.NIT THEN
            NUM.NIT = R.TABLE.CODS<EB.CUS.LEGAL.ID, VP.NIT>
        END ELSE
            NUM.NIT = ''
        END

        TITULARES := 'NUMCLIE' : I : '!' : ID.COD : CHAR(10)
        TITULARES := 'TIPOREL' : I : '!' : LS.TIPO.RELACION : ' ' : LN.CORRELATIVO : ' ' : CHAR(10)
        TITULARES := 'TITULAR' : I : '!' : NOMB.CLIENT : CHAR(10)
        TITULARES := 'DUITITU' : I : '!' : NUM.DUI : CHAR(10)
        TITULARES := 'NITTITU' : I : '!' : NUM.NIT : CHAR(10)
        TITULARES := 'FIRTITU' : I : '!' : ID.FIR : CHAR(10)

        I += 1
        LN.CORRELATIVO += 1
    REPEAT

;* Estableciendo parámetros generales


;* determinar la condición de la cuenta
    CALL GET.LOC.REF('AA.ARR.CUSTOMER', 'LF.CON.INDIS', POS)
    LS.CON.INDIS = R.TABLE.ACU<AA.CUS.LOCAL.REF, POS>

    IF LS.CON.INDIS EQ 'Y' THEN
        LS.TIPO.CONDICION = 'COPROPIEDAD (Y)'
    END
    ELSE IF LS.CON.INDIS EQ 'O' THEN
    LS.TIPO.CONDICION = 'PROPIEDAD ALTERNATIVA (O)'
    END
    ELSE
    LS.TIPO.CONDICION = 'INDIVIDUAL'
    END


    DATOS := 'TIPOCOND!' : LS.TIPO.CONDICION : CHAR(10)
    DATOS := 'MONTOMIN!' : LN.MONTO.MINIMO.APERTURA : CHAR(10)

;*-----------------------------------------------------------------------------
;* Bloque para contratos de cuentas de ahorro persona natural
    TITULAR.CUENTA = ''

;* 20151014 - nuevo bloque para identificar el tipo de producto del contrato
;* 20170926 - Se agrega bloque para identificar el tipo de persona en el producto Cuenta de Ahorro Electrónica @Author: Ronald Ortiz
    IF AA.ARR.ACC.CATEGORY GT '6000' AND AA.ARR.ACC.CATEGORY LT '6600' THEN
        ;* cuenta de ahorro PN
        
        IF LS.PERSONERIA EQ '2' THEN
            GOSUB CONSTRUIR.AHO.PJ
        END
        ELSE
            GOSUB CONSTRUIR.AHO.PN
        END
        
        
    END
    ELSE IF AA.ARR.ACC.CATEGORY GT '1000' AND AA.ARR.ACC.CATEGORY LT '2000' THEN
    IF LS.PERSONERIA EQ '2' THEN ;* CTE PJ
           GOSUB CONSTRUIR.CTE.PJ
    END
    ELSE	;* CTE PN
           GOSUB CONSTRUIR.CTE.PN
    END
    END
    ELSE
    ETEXT = 'Producto no encontrado.'
    CALL STORE.END.ERROR
    END



    DATOS := 'TIPODOC' : I + 1 : '!' : TIPO.DOC.CLIENTE : CHAR(10)
    DATOS := 'NUMERDO' : I + 1 : '!' : DOC.CLIENTE : ' ' : CONDICION : CHAR(10)

;*-----------------------------------------------------------------------------
;* Bloque de creación de archivos

;* Ruta donde se guarda el archivo
    RUTA.SPOOL.ID = 'RUTA.CONTRATO.SPOOL'
    RUTA.PDF.ID = 'RUTA.CONTRATO.PDF'
    RUTA.VALID.ID = 'RUTA.VALIDAR.PDF'

    CALL F.READ(FN.TABLE.PA, RUTA.SPOOL.ID, R.TABLE.PA, F.TABLE.PA, F.ERR.PA)
    DIR.NAME.SPOOL = R.TABLE.PA<EB.SLV39.VALOR.PARAM>

    CALL F.READ(FN.TABLE.PA, RUTA.PDF.ID, R.TABLE.PA, F.TABLE.PA, F.ERR.PA)
    DIR.NAME.PDF = R.TABLE.PA<EB.SLV39.VALOR.PARAM>

    CALL F.READ(FN.TABLE.PA, RUTA.VALID.ID, R.TABLE.PA, F.TABLE.PA, F.ERR.PA)
    DIR.NAME.VALID = R.TABLE.PA<EB.SLV39.VALOR.PARAM>

    RETURN

UPDATE.ITEM.RECORD:
    CALL F.READ(FN.ITEMS.MASIVOS,ITEMS.MASIVO.ID,ITEM.R,F.ITEMS.MASIVOS,ITEM.ERR)
    ITEM.R<EB.SLV3.RESERVADO.5> = 'Contrato y comprobante deposito generado.'
    ITEM.R<EB.SLV3.RESERVADO.8> = R.ID.PDF.CONTRATO
    ITEM.R<EB.SLV3.RESERVADO.9> = R.ID.PDF.DEPOSITOS
    CALL F.WRITE (FN.ITEMS.MASIVOS,ITEMS.MASIVO.ID,ITEM.R)
RETURN


*-----------------------------------------------------------------------------
GENERA.CONTRATO:
*-----------------------------------------------------------------------------

;****************************************************************************
;* DEBUG
;****************************************************************************
;*DIR.NAME.SPOOL = 'C:\Temp\'
;****************************************************************************
    ;*NAME.ID = 'CON' : '-' : SS.CUENTA.ID : '-' : R.USER<EB.USE.DEPARTMENT.CODE> : '-' : (TIMESTAMP() * 1000)
      NAME.ID = 'CTMSV' : '-':ID.MASTER.MASIVOS:'-':SS.CUSTOMER.ID
    R.ID.TXT            := NAME.ID : '.txt'
    R.ID.PDF            := NAME.ID : '.pdf'
    R.ID.PDF.CONTRATO    = R.ID.PDF 
    CRT 'DIR > ':R.ID.TXT
;* Plantilla a utilizar
;* CALL F.READ(FN.TABLE.PA, BUSCAR.CONTRATO, R.TABLE.PA, F.TABLE.PA, F.ERR.PA)
;* PLANTILLA = '(' : R.TABLE.PA<EB.SLV39.VALOR.PARAM> : ') STARTLM'
    PLANTILLA = '(' : BUSCAR.CONTRATO : ') STARTLM'

;* Armando la cadena para el archivo
    CADENA = '%!' : CHAR(10)
    CADENA := PLANTILLA : CHAR(10)
    CADENA := 'DOCUSELE!' : UPCASE(SS.DOCUMENT.SELECTION) : CHAR(10)
    CADENA := 'APOBANCO!' : APODERADO.NOMBRE : CHAR(10)
    CADENA := 'EDADAPBA!' : APODERADO.BDAY : CHAR(10)
    CADENA := 'PROAPOBA!' : APODERADO.PROFESION : CHAR(10)
    CADENA := 'DOMIAPOB!' : APO.DOMICILIO : CHAR(10)
    CADENA := 'DIAPODER!' : APODERADO.DIA : CHAR(10)
    CADENA := 'MESPODER!' : APODERADO.MES : CHAR(10)
    CADENA := 'NOTARIOS!' : NOMBRE.NOTARIO : CHAR(10)
    CADENA := 'LIBROREG!' : NO.LIB.REG : CHAR(10)
    CADENA := 'NUMEROCU!' : UPCASE(NUMERO.CUENTA) : CHAR(10)
    CADENA := 'TIPOCUEN!' : UPCASE(LS.NOMBRE.PRODUCTO) : CHAR(10) ;* se adiciona el tipo de producto al registro firmas
;*CADENA := UPCASE(DATOS)
    CADENA := 'HEADMENO!' : CLIENTE : CHAR(10)
    CADENA := DATOS
    CADENA := 'LUGAREMI!' : UPCASE(LUGAR.EMISION) : CHAR(10)
    CADENA := 'FECHAEMI!' : UPCASE(FECHA.EMISION) : CHAR(10)
    CADENA := 'FECHASSF!' : FECHA.SSF : CHAR(10)
    CADENA := 'INSTRUCC!' : UPCASE(INSTRUCCION.ESPECIAL) : CHAR(10)
    CADENA := 'REPLEGAL!' : REP.LEGAL : CHAR(10)
    CADENA := 'TASAINTE!' : FMT(N_INTEREST, "R2#10") : CHAR(10)
    CADENA := BENEFICIARIOS
    CADENA := TITULARES
    CADENA := '%%EOF'
    CADENA = TRIM(CADENA)

    GOSUB LIMPIA.CADENA

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
;*    PERFIL.ARCHIVO = DIR.NAME.VALID : R.ID.PDF
;*    CALL SLV.E.NOF.GEN.FILEEXIST(PERFIL.ARCHIVO)
;*    IF PERFIL.ARCHIVO EQ 1 THEN
;*        STR.ARR = SS.CUENTA.ID : "*"
;*        STR.ARR := TITULAR.CUENTA : "*"
;*        STR.ARR := SS.DOCUMENT.NAME : "*"
;*        STR.ARR := DIR.NAME.PDF : R.ID.PDF
;*        CRT 'ARCHIVO ENCONTRADO.'
;*    END ELSE
;*        STR.ARR = SS.CUENTA.ID : "*"
;*        STR.ARR := TITULAR.CUENTA : "*"
;*        STR.ARR := SS.DOCUMENT.NAME : "*"
;*        STR.ARR := "No fue posible generar el archivo. Contacte al administrador del sistema."
;*        CRT 'ARCHIVO PERDIDO.'
;*    END
***********************************************************************

;* Insert de línea en ENQUIRY resultante
*    STR.ARR = SS.CUENTA.ID : "*"
*    STR.ARR := TITULAR.CUENTA : "*"
*    STR.ARR := SS.DOCUMENT.NAME : "*"
*    STR.ARR := DIR.NAME.PDF : R.ID.PDF

*    ENQ.DATA<-1>=STR.ARR

    RETURN


*-----------------------------------------------------------------------------
GENERA.DEPOSITO:
*-----------------------------------------------------------------------------
    ;*NAME.ID = 'DIN' : '-' : SS.CUENTA.ID : '-' : R.USER<EB.USE.DEPARTMENT.CODE> : '-' : (TIMESTAMP() * 1000)
      NAME.ID = 'DPMSV' : '-' :ID.MASTER.MASIVOS:'-' : SS.CUSTOMER.ID
    CRT 'R.ID.TXT > ':R.ID.TXT
    CRT 'NAME.ID  > ':NAME.ID
    
    
    R.ID.TXT            = NAME.ID : '.txt'
    R.ID.PDF            = NAME.ID : '.pdf'
    R.ID.PDF.DEPOSITOS  = R.ID.PDF

;*BUSCAR.CONTRATO = 'PLAN.CTA.COM.INI'
    BUSCAR.CONTRATO = 'azul_comprobante_cuenta.jdt'
;* Plantilla a utilizar
;*CALL F.READ(FN.TABLE.PA, BUSCAR.CONTRATO, R.TABLE.PA, F.TABLE.PA, F.ERR.PA)
;*PLANTILLA = '(' : R.TABLE.PA<EB.SLV39.VALOR.PARAM> : ') STARTLM'
    PLANTILLA = '(' : BUSCAR.CONTRATO : ') STARTLM'


;* Armando la cadena para el archivo
    CADENA = '%!' : CHAR(10)
    CADENA := PLANTILLA : CHAR(10)
    CADENA := 'DOCUSELE!' : UPCASE(SS.DOCUMENT.SELECTION) : CHAR(10)
    CADENA := 'AGENCIAS!' : UPCASE(AGENCIA.NOMBRE) : CHAR(10)
    CADENA := 'NUMERCTA!' : UPCASE(NUMERO.CUENTA) : CHAR(10)
    CADENA := 'TIPOCUEN!' : UPCASE(LS.NOMBRE.PRODUCTO) : CHAR(10)
    CADENA := 'LUGAFECH!' : UPCASE(LUGAR.EMISION : ", " : FECHA.EMISION) : CHAR(10)
    CADENA := 'MONTODEP!' : TRIM(FMT(MONTO, 'R2,#100')) : CHAR(10)
    CADENA := 'CUSTOMER!' : TITULAR.CUENTA : CHAR(10)
;*CADENA := 'DOCUMENT!' : TIPO.DOC.CLIENTE : ' ' : DOC.CLIENTE : CHAR(10);* CAMBIO PARA QUE MUESTRE LOS DOCS DE CTAS MANCOMUNADAS
    CADENA := 'DOCUMENT!' : UPCASE(DOCS.CTA.MANCOMUNADAS) : CHAR(10)
    CADENA := 'REPLEGAL!' : REP.LEGAL : CHAR(10)
    CADENA := '%%EOF'
    CADENA = TRIM(CADENA)

    GOSUB LIMPIA.CADENA

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
*    PERFIL.ARCHIVO = DIR.NAME.VALID : R.ID.PDF
*    CALL SLV.E.NOF.GEN.FILEEXIST(PERFIL.ARCHIVO)
*    IF PERFIL.ARCHIVO EQ 1 THEN
*        STR.ARR = SS.CUENTA.ID : "*"
*        STR.ARR := TITULAR.CUENTA : "*"
*        STR.ARR := SS.DOCUMENT.NAME : "*"
*        STR.ARR := DIR.NAME.PDF : R.ID.PDF
*        CRT 'ARCHIVO ENCONTRADO.'
*    END ELSE
*        STR.ARR = SS.CUENTA.ID : "*"
*        STR.ARR := TITULAR.CUENTA : "*"
*        STR.ARR := SS.DOCUMENT.NAME : "*"
*        STR.ARR := "No fue posible generar el archivo. Contacte al administrador del sistema."
*        CRT 'ARCHIVO PERDIDO.'
*    END
***********************************************************************

;* Insert de línea en ENQUIRY resultante
*    STR.ARR = SS.CUENTA.ID : "*"
*    STR.ARR := TITULAR.CUENTA : "*"
*    STR.ARR := SS.DOCUMENT.NAME : "*"
*    STR.ARR := DIR.NAME.PDF : R.ID.PDF

*    ENQ.DATA<-1>=STR.ARR

    RETURN


*-----------------------------------------------------------------------------
LIMPIA.CADENA:
*-----------------------------------------------------------------------------
;* Codificar caracteres especiales
    CADENA = CHANGE(CADENA, 'Ñ', CHAR(465))
    CADENA = CHANGE(CADENA, 'ñ', CHAR(497))
;* quitar caracteres de separador multivalor
    CADENA = SWAP(CADENA, @SM, ' ')
    CADENA = SWAP(CADENA, @VM, ' ')
    CADENA = SWAP(CADENA, @FM, ' ')
    CADENA = SWAP(CADENA, @TM, ' ')



    RETURN


*-----------------------------------------------------------------------------
OBTENER.REP.LEGAL:
*-----------------------------------------------------------------------------

;* obtener representante legal si existe
    CALL GET.LOC.REF ('AA.ARR.CUSTOMER', 'LF.REP.LEGAL', POS)

    COD.REP.LEGAL = R.TABLE.ACU<AA.CUS.LOCAL.REF, POS>
;* solucion192

    CALL F.READ(FN.TABLE.CUS, COD.REP.LEGAL, R.TABLE.CUS, F.TABLE.CUS, F.ERR.CUS)

    PRIMER.NOMBRE = TRIM(R.TABLE.CUS<EB.CUS.NAME.1>)
    SEGUNDO.NOMBRE = TRIM(R.TABLE.CUS<EB.CUS.NAME.2>)
    TERCER.NOMBRE = TRIM(R.TABLE.CUS<EB.CUS.GIVEN.NAMES>)
    PRIMER.APELLIDO = TRIM(R.TABLE.CUS<EB.CUS.TEXT>)
    SEGUNDO.APELLIDO = TRIM(R.TABLE.CUS<EB.CUS.FAMILY.NAME>)
    APELLIDO.CASADA = TRIM(R.TABLE.CUS<EB.CUS.PREVIOUS.NAME>)
    NOMBRES = PRIMER.NOMBRE : ' ' : SEGUNDO.NOMBRE : ' ' : TERCER.NOMBRE
    APELLIDOS = PRIMER.APELLIDO : ' ' : SEGUNDO.APELLIDO : ' ' : APELLIDO.CASADA
    REP.LEGAL = TRIM(NOMBRES : ' ' : APELLIDOS)
    DATOS := 'REPRESEN!' : REP.LEGAL : CHAR(10)
;*DATOS := 'REPRESEN!' : REP.LEGAL : CHAR(10)

;* adicionado para garantizar que en persona juridica
;* se coloque tipo y numero de documento de representante legal
;*20150327 EURIAS tratamiendo para mostrar el DUI de las personas que son representante legal
    FINDSTR 'DOCTO.UNICO.IDENT' IN R.TABLE.CUS<EB.CUS.LEGAL.DOC.NAME> SETTING AP, VP THEN
        DOC.CLIENTE = R.TABLE.CUS<EB.CUS.LEGAL.ID, VP>
        TIPO.DOC.CLIENTE = 'DUI'
    END
    ELSE FINDSTR 'PASSPORT' IN R.TABLE.CUS<EB.CUS.LEGAL.DOC.NAME> SETTING AP, VP THEN
    DOC.CLIENTE = R.TABLE.CUS<EB.CUS.LEGAL.ID, VP>
    TIPO.DOC.CLIENTE = 'pasaporte'
    END
    ELSE FINDSTR 'CARNET.RESIDENTE' IN R.TABLE.CUS<EB.CUS.LEGAL.DOC.NAME> SETTING AP, VP THEN
    DOC.CLIENTE = R.TABLE.CUS<EB.CUS.LEGAL.ID, VP>
    TIPO.DOC.CLIENTE = 'Carnet de Residente'
    END
    ELSE FINDSTR 'CARNET.MENOR.EDAD' IN R.TABLE.CUS<EB.CUS.LEGAL.DOC.NAME> SETTING AP, VP THEN
    DOC.CLIENTE = R.TABLE.CUS<EB.CUS.LEGAL.ID, VP>
    TIPO.DOC.CLIENTE = 'Carnet de Menor de Edad'
    END
    ELSE
    DOC.CLIENTE = '_______________'
    TIPO.DOC.CLIENTE = '_______________'
    END
    DOCS.CTA.MANCOMUNADAS := TIPO.DOC.CLIENTE:'-':DOC.CLIENTE
;*fin cambio 20150327
;*CRT "TEST:":DOCS.CTA.MANCOMUNADAS
;*Encontrando el tipo de firma del representante legal
    CALL GET.LOC.REF ('AA.ARR.CUSTOMER', 'LF.AUT.COND', POS)
    FIRMA.REP = R.TABLE.ACU<AA.CUS.LOCAL.REF, POS>
    DATOS := 'FIRMAREP!' : FIRMA.REP : CHAR(10)

;*cambio en calculo de edad de cliente RGaray 20150724
    EDAD.CALCULO = R.TABLE.CUS<EB.CUS.DATE.OF.BIRTH>
;* DEBUG EDad
;*EDAD.CALCULO = '19850724'
    GOSUB CALCULAR.EDAD

;* Calculando la edad del representante legal
*	    EDAD.REP.LEGAL = OCONV(DATE(), 'DY') - SUBSTRINGS(R.TABLE.CUS<EB.CUS.DATE.OF.BIRTH>, 1, 4)
*	    IF OCONV(DATE(), 'DM') - SUBSTRINGS(R.TABLE.CUS<EB.CUS.DATE.OF.BIRTH>, 6, 2) > 0 THEN
*	        EDAD.REP.LEGAL = EDAD.REP.LEGAL + 1
*	    END ELSE IF OCONV(DATE(), 'DM') - SUBSTRINGS(R.TABLE.CUS<EB.CUS.DATE.OF.BIRTH>, 6, 2) = 0 THEN
*	        IF OCONV(DATE(), 'DD') - SUBSTRINGS(R.TABLE.CUS<EB.CUS.DATE.OF.BIRTH>, 4, 2) > 0 THEN
*	            EDAD.REP.LEGAL = EDAD.REP.LEGAL + 1
*	        END
*	    END


*	    DATOS := 'EDADREPR!' : EDAD.REP.LEGAL : CHAR(10)
*	    DATOS := 'HEADRPAG!' : EDAD.REP.LEGAL : CHAR(10)

    DATOS := 'EDADREPR!' : EDAD.CLIENTE : CHAR(10)
    DATOS := 'HEADRPAG!' : EDAD.CLIENTE : CHAR(10)

;* Encontrando la profesion
    OCUPACION.REP = R.TABLE.CUS<EB.CUS.OCCUPATION>
    DATOS := 'OCUPAREP!' : OCUPACION.REP : CHAR(10)

;* Encontrando el municipio del representante legal
    CALL GET.LOC.REF ('CUSTOMER', 'LF.MUNICIPIO', POS)
    COD.MUNICIPIO = R.TABLE.CUS<EB.CUS.LOCAL.REF, POS>
    CALL F.READ(FN.TABLE.MUN, COD.MUNICIPIO, R.TABLE.MUN, F.TABLE.MUN, F.ERR.MUN)
    MUNICIPIO.REP = R.TABLE.MUN<EB.SLV33.DESCRIPTION>
    DATOS := 'MUNICREP!' : MUNICIPIO.REP : CHAR(10)


;* Encontrando el departamento del representante legal
    CALL GET.LOC.REF ('CUSTOMER', 'LF.DEPARTAMENTO', POS)
    COD.DEPARTAMENTO = R.TABLE.CUS<EB.CUS.LOCAL.REF, POS>
    CALL F.READ(FN.TABLE.DEP, COD.DEPARTAMENTO, R.TABLE.DEP, F.TABLE.DEP, F.ERR.DEP)
    DEPARTAMENTO.REP = R.TABLE.DEP<EB.SLV43.DESCRIPTION>
    DATOS := 'DEPTOREP!' : DEPARTAMENTO.REP : CHAR(10)

    DATOS := 'HEADRPLC!' : MUNICIPIO.REP : ', ' : DEPARTAMENTO.REP : CHAR(10)

;* Encontrando el número de documento
    FINDSTR 'DOCTO.UNICO.IDENT' IN R.TABLE.CUS<EB.CUS.LEGAL.DOC.NAME> SETTING AP, VP THEN
        DOC.REP = R.TABLE.CUS<EB.CUS.LEGAL.ID, VP>
        DOC.NUM.REP.LG = DOC.REP
        DOC.NUM.FIRMAS = DOC.REP
        TIPO.DOC.REP = 'DUI'
    END
    ELSE FINDSTR 'PASSPORT' IN R.TABLE.CUS<EB.CUS.LEGAL.DOC.NAME> SETTING AP, VP THEN
    DOC.REP = R.TABLE.CUS<EB.CUS.LEGAL.ID, VP>
    DOC.NUM.FIRMAS = ''
    DOC.NUM.REP.LG = DOC.REP
    DOC.CLIENTE = R.TABLE.CUS<EB.CUS.LEGAL.ID, VP>
    TIPO.DOC.REP = 'pasaporte'
    END
    ELSE FINDSTR 'CARNET.RESIDENTE' IN R.TABLE.CUS<EB.CUS.LEGAL.DOC.NAME> SETTING AP, VP THEN
    DOC.REP = R.TABLE.CUS<EB.CUS.LEGAL.ID, VP>
    DOC.NUM.FIRMAS = ''
    ;*DOC.REP = R.TABLE.CUS<EB.CUS.LEGAL.ID, VP>
    DOC.NUM.REP.LG = R.TABLE.CUS<EB.CUS.LEGAL.ID, VP>
    TIPO.DOC.REP = 'Carnet de Residente'
    DOC.CLIENTE = R.TABLE.CUS<EB.CUS.LEGAL.ID, VP>
    TIPO.DOC.CLIENTE = 'Carnet de Residente'
    END
    ELSE FINDSTR 'CARNET.MENOR.EDAD' IN R.TABLE.CUS<EB.CUS.LEGAL.DOC.NAME> SETTING AP, VP THEN
    DOC.REP = R.TABLE.CUS<EB.CUS.LEGAL.ID, VP>
    DOC.NUM.REP.LG = R.TABLE.CUS<EB.CUS.LEGAL.ID, VP>
    DOC.NUM.FIRMAS = ''
    ;*DOC.REP = R.TABLE.CUS<EB.CUS.LEGAL.ID, VP>
    TIPO.DOC.REP = 'Carnet de Menor de Edad'
    DOC.CLIENTE = R.TABLE.CUS<EB.CUS.LEGAL.ID, VP>
    TIPO.DOC.CLIENTE = 'Carnet de Menor de Edad'
    END
    
    ELSE
    DOC.REP = '_______________'
    TIPO.DOC.REP = '_______________'
    END
    DATOS := 'TIPODOCU!' : TIPO.DOC.REP : CHAR(10)
    DATOS := 'NUMERODO!' : DOC.REP : CHAR(10)
    DATOS := 'HEADRPDC!' : TIPO.DOC.REP : ' ' : DOC.REP : CHAR(10)
    
    ;*Ronald Ortiz
    ;*Variables que se utilizan únicamente para el contrato de cuenta de ahorro empresas
    DATOS := 'NMDOCFIR!' :DOC.NUM.FIRMAS:CHAR(10)
    ;*DATOS := 'NMDOCFIR!' :'044396150':CHAR(10)
    DATOS := 'NMDOCLG|'  :DOC.NUM.REP.LG:CHAR(10) 
    
;* Encontrando el número de NIT
    FINDSTR 'NUM.IDEN.TRIBUT' IN R.TABLE.CUS<EB.CUS.LEGAL.DOC.NAME> SETTING AP, VP THEN
        NIT.REP = R.TABLE.CUS<EB.CUS.LEGAL.ID, VP>
    END ELSE

        NIT.REP = '______________'
    END

    DATOS := 'NITREPRE!' : NIT.REP : CHAR(10)

    IF REP.LEGAL NE '' THEN
        DATOS := 'REPRESEN!actuando en representacion de ' : REP.LEGAL : CHAR(10)
    END ELSE
        DATOS := 'REPRESEN!' : REP.LEGAL : CHAR(10)
    END

    DATOS := 'HEADRPNM!':REP.LEGAL : CHAR(10)

    CRT 'REP.LEGAL -> ' : REP.LEGAL
    CRT 'TIPO.DOC.CLIENTE -> ' : TIPO.DOC.CLIENTE
    CRT 'DOC.CLIENTE -> ' : DOC.CLIENTE
    CRT 'NIT.REP -> ' : NIT.REP
    CRT 'EDAD.REP -> ' : EDAD.REP.LEGAL
    CRT 'DATOS -> ' : DATOS

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


*-----------------------------------------------------------------------------
CONSTRUIR.CTE.PN:
*-----------------------------------------------------------------------------


    IF REP.LEGAL NE '' THEN
        BUSCAR.CONTRATO = 'azul_cta_corriente_pn_menor.jdt'
    END ELSE
        BUSCAR.CONTRATO = 'azul_cuenta_corriente_persona_natural.jdt'
    END

;* Fecha de la SSF
    CALL F.READ(FN.TABLE.PA, 'FECHA.SSF.CONT.CC.PN', R.TABLE.PA, F.TABLE.PA, F.ERR.PA)
    FECHA.SSF = R.TABLE.PA<EB.SLV39.VALOR.PARAM>

    COD.CLIENTE = R.TABLE.ACU<AA.CUS.PRIMARY.OWNER>
    CALL F.READ(FN.TABLE.CUS, COD.CLIENTE, R.TABLE.CUS, F.TABLE.CUS, F.ERR.CUS)

    PRIMER.NOMBRE = TRIM(R.TABLE.CUS<EB.CUS.NAME.1>)
    SEGUNDO.NOMBRE = TRIM(R.TABLE.CUS<EB.CUS.NAME.2>)
    TERCER.NOMBRE = TRIM(R.TABLE.CUS<EB.CUS.GIVEN.NAMES>)
    PRIMER.APELLIDO = TRIM(R.TABLE.CUS<EB.CUS.TEXT>)
    SEGUNDO.APELLIDO = TRIM(R.TABLE.CUS<EB.CUS.FAMILY.NAME>)
    APELLIDO.CASADA = TRIM(R.TABLE.CUS<EB.CUS.PREVIOUS.NAME>)
    NOMBRES = PRIMER.NOMBRE : ' ' : SEGUNDO.NOMBRE : ' ' : TERCER.NOMBRE
    APELLIDOS = PRIMER.APELLIDO : ' ' : SEGUNDO.APELLIDO : ' ' : APELLIDO.CASADA
    CLIENTE = TRIM(NOMBRES : ' ' : APELLIDOS)

    CALL GET.LOC.REF ('AA.ARR.CUSTOMER', 'LF.CON.INDIS', POS)
    CONDICION = R.TABLE.ACU<AA.CUS.LOCAL.REF, POS>
    IF CONDICION EQ 'Y' OR CONDICION EQ 'O' THEN
        DATOS := 'NOMBREC1!' : CLIENTE : CHAR(10)
        TITULAR.CUENTA = CLIENTE : ' ' : LOWCASE(CONDICION)
    END ELSE
        DATOS := 'NOMBREC1!' : CLIENTE : CHAR(10)
        CONDICION = ''
        TITULAR.CUENTA = CLIENTE
    END

;*cambio en calculo de edad de cliente RGaray 20150724
    EDAD.CALCULO = R.TABLE.CUS<EB.CUS.DATE.OF.BIRTH>
;* DEBUG EDad
;*EDAD.CALCULO = '19850724'
    GOSUB CALCULAR.EDAD

    DATOS := 'EDADCLI1!' : EDAD.CLIENTE : ' AÑOS' :CHAR(10)

;* Encontrando el departamento del propietario
    CALL GET.LOC.REF('CUSTOMER', 'LF.MUNICIPIO', POS)
    COD.MUNICIPIO = R.TABLE.CUS<EB.CUS.LOCAL.REF, POS>
    CALL F.READ(FN.TABLE.MUN, COD.MUNICIPIO, R.TABLE.MUN, F.TABLE.MUN, F.ERR.MUN)
    MUNI.CLIENTE = R.TABLE.MUN<EB.SLV33.DESCRIPTION>

    CALL GET.LOC.REF ('CUSTOMER', 'LF.DEPARTAMENTO', POS)
    COD.DEPARTAMENTO = R.TABLE.CUS<EB.CUS.LOCAL.REF, POS>
    CALL F.READ(FN.TABLE.DEP, COD.DEPARTAMENTO, R.TABLE.DEP, F.TABLE.DEP, F.ERR.DEP)
    DEPARTAMENTO.CLIENTE = R.TABLE.DEP<EB.SLV43.DESCRIPTION>

    DATOS := 'DOMICIL1!' : MUNI.CLIENTE : ', ' : DEPARTAMENTO.CLIENTE : CHAR(10)

;* Encontrando el número y tipo de documento principal
    FINDSTR 'DOCTO.UNICO.IDENT' IN R.TABLE.CUS<EB.CUS.LEGAL.DOC.NAME> SETTING AP, VP THEN
        DOC.CLIENTE = R.TABLE.CUS<EB.CUS.LEGAL.ID, VP>
        TIPO.DOC.CLIENTE = 'DUI'
    END
    ELSE FINDSTR 'PASSPORT' IN R.TABLE.CUS<EB.CUS.LEGAL.DOC.NAME> SETTING AP, VP THEN
    DOC.CLIENTE = R.TABLE.CUS<EB.CUS.LEGAL.ID, VP>
    TIPO.DOC.CLIENTE = 'pasaporte'
    END
    ELSE FINDSTR 'CARNET.RESIDENTE' IN R.TABLE.CUS<EB.CUS.LEGAL.DOC.NAME> SETTING AP, VP THEN
    DOC.CLIENTE = R.TABLE.CUS<EB.CUS.LEGAL.ID, VP>
    TIPO.DOC.CLIENTE = 'Carnet de Residente'
    END
    ELSE FINDSTR 'CARNET.MENOR.EDAD' IN R.TABLE.CUS<EB.CUS.LEGAL.DOC.NAME> SETTING AP, VP THEN
    DOC.CLIENTE = R.TABLE.CUS<EB.CUS.LEGAL.ID, VP>
    TIPO.DOC.CLIENTE = 'Carnet de Menor de Edad'
    END
    ELSE
    DOC.CLIENTE = '_______________'
    TIPO.DOC.CLIENTE = '_______________'
    END
    DATOS := 'TIPODOC1!' : TIPO.DOC.CLIENTE : CHAR(10)
    DATOS := 'NUMERDO1!' : DOC.CLIENTE : ' ' : CONDICION : CHAR(10)
    DOCS.CTA.MANCOMUNADAS = TIPO.DOC.CLIENTE:DOC.CLIENTE;*SETTEO EL DUI DEL REPRESENTANTE LEGAL
    IF CONDICION EQ 'Y' OR CONDICION EQ 'O' THEN
        CONTADOR = DCOUNT(R.TABLE.ACU<AA.CUS.OTHER.PARTY>, @VM)
        FOR I = 1 TO CONTADOR
            COD.CLIENTE = R.TABLE.ACU<AA.CUS.OTHER.PARTY><1,I>
            CALL F.READ(FN.TABLE.CUS, COD.CLIENTE, R.TABLE.CUS, F.TABLE.CUS, F.ERR.CUS)


            PRIMER.NOMBRE = TRIM(R.TABLE.CUS<EB.CUS.NAME.1>)
            SEGUNDO.NOMBRE = TRIM(R.TABLE.CUS<EB.CUS.NAME.2>)
            TERCER.NOMBRE = TRIM(R.TABLE.CUS<EB.CUS.GIVEN.NAMES>)
            PRIMER.APELLIDO = TRIM(R.TABLE.CUS<EB.CUS.TEXT>)
            SEGUNDO.APELLIDO = TRIM(R.TABLE.CUS<EB.CUS.FAMILY.NAME>)
            APELLIDO.CASADA = TRIM(R.TABLE.CUS<EB.CUS.PREVIOUS.NAME>)
            NOMBRES = PRIMER.NOMBRE : ' ' : SEGUNDO.NOMBRE : ' ' : TERCER.NOMBRE
            APELLIDOS = PRIMER.APELLIDO : ' ' : SEGUNDO.APELLIDO : ' ' : APELLIDO.CASADA
            CLIENTE = TRIM(NOMBRES : ' ' : APELLIDOS)
            IF I <> CONTADOR THEN
                DATOS := 'NOMBREC': I + 1 : '!' : CLIENTE : CHAR(10)
                TITULAR.CUENTA = TITULAR.CUENTA : ' ' : CLIENTE : ' ' : LOWCASE(CONDICION)
            END ELSE
                DATOS := 'NOMBREC': I + 1 : '!' : CLIENTE : '!' : LOWCASE(CONDICION) : CHAR(10)
                CONDICION = ''
                TITULAR.CUENTA = TITULAR.CUENTA : ' ' : CLIENTE
            END

            ;*cambio en calculo de edad de cliente RGaray 20150724
            EDAD.CALCULO = R.TABLE.CUS<EB.CUS.DATE.OF.BIRTH>
            ;* DEBUG EDad
            ;*EDAD.CALCULO = '19850724'
            GOSUB CALCULAR.EDAD

            ;* proceso antiguo
            *	            EDAD.CLIENTE = OCONV(DATE(), 'DY') - SUBSTRINGS(R.TABLE.CUS<EB.CUS.DATE.OF.BIRTH>, 1, 4)
            *	            IF OCONV(DATE(), 'DM') - SUBSTRINGS(R.TABLE.CUS<EB.CUS.DATE.OF.BIRTH>, 6, 2) > 0 THEN
            *	                EDAD.CLIENTE = EDAD.CLIENTE + 1
            *	            END ELSE IF OCONV(DATE(), 'DM') - SUBSTRINGS(R.TABLE.CUS<EB.CUS.DATE.OF.BIRTH>, 6, 2) = 0 THEN
            *	                IF OCONV(DATE(), 'DD') - SUBSTRINGS(R.TABLE.CUS<EB.CUS.DATE.OF.BIRTH>, 4, 2) > 0 THEN
            *	                    EDAD.CLIENTE = EDAD.CLIENTE + 1
            *	                END
            *	            END

            DATOS := 'EDADCLI': I + 1 : '!' : EDAD.CLIENTE : ' AÑOS':CHAR(10)

            CALL GET.LOC.REF('CUSTOMER', 'LF.MUNICIPIO', POS)
            COD.MUNICIPIO = R.TABLE.CUS<EB.CUS.LOCAL.REF, POS>
            CALL F.READ(FN.TABLE.MUN, COD.MUNICIPIO, R.TABLE.MUN, F.TABLE.MUN, F.ERR.MUN)
            MUNI.CLIENTE = R.TABLE.MUN<EB.SLV33.DESCRIPTION>

            CALL GET.LOC.REF ('CUSTOMER', 'LF.DEPARTAMENTO', POS)
            COD.DEPARTAMENTO = R.TABLE.CUS<EB.CUS.LOCAL.REF, POS>
            CALL F.READ(FN.TABLE.DEP, COD.DEPARTAMENTO, R.TABLE.DEP, F.TABLE.DEP, F.ERR.DEP)
            DEPARTAMENTO.CLIENTE = R.TABLE.DEP<EB.SLV43.DESCRIPTION>

            DATOS := 'DOMICIL' : I + 1 : '!' : MUNI.CLIENTE : ', ' : DEPARTAMENTO.CLIENTE : CHAR(10)

            FINDSTR 'DOCTO.UNICO.IDENT' IN R.TABLE.CUS<EB.CUS.LEGAL.DOC.NAME> SETTING AP, VP THEN
                DOC.CLIENTE = R.TABLE.CUS<EB.CUS.LEGAL.ID, VP>
                TIPO.DOC.CLIENTE = 'DUI'
            END
            ELSE FINDSTR 'PASSPORT' IN R.TABLE.CUS<EB.CUS.LEGAL.DOC.NAME> SETTING AP, VP THEN
            DOC.CLIENTE = R.TABLE.CUS<EB.CUS.LEGAL.ID, VP>
            TIPO.DOC.CLIENTE = 'pasaporte'
        END
        ELSE FINDSTR 'CARNET.RESIDENTE' IN R.TABLE.CUS<EB.CUS.LEGAL.DOC.NAME> SETTING AP, VP THEN
        DOC.CLIENTE = R.TABLE.CUS<EB.CUS.LEGAL.ID, VP>
        TIPO.DOC.CLIENTE = 'Carnet de Residente'
    END
    ELSE FINDSTR 'CARNET.MENOR.EDAD' IN R.TABLE.CUS<EB.CUS.LEGAL.DOC.NAME> SETTING AP, VP THEN
    DOC.CLIENTE = R.TABLE.CUS<EB.CUS.LEGAL.ID, VP>
    TIPO.DOC.CLIENTE = 'Carnet de Menor de Edad'
    END
    ELSE
    DOC.CLIENTE = '_______________'
    IPO.DOC.CLIENTE = '_______________'
    END
    DATOS := 'TIPODOC' : I + 1 : '!' : TIPO.DOC.CLIENTE : CHAR(10)
    DATOS := 'NUMERDO' : I + 1 : '!' : DOC.CLIENTE : ' ' : CONDICION : CHAR(10)
*-----------------------------------------------------------------------------
;*OBTENER LOS DOCUMENTOS TIPO DUI PARA LAS CUENTAS MANCOMUNADAS
    DOCS.CTA.MANCOMUNADAS = DOCS.CTA.MANCOMUNADAS:', ':TIPO.DOC.CLIENTE:"-":DOC.CLIENTE
;*CRT "SALIDA--->":DOCS.CTA.MANCOMUNADAS
*-----------------------------------------------------------------------------
    NEXT I
    END



;* Buscando los titulares de las cuentas
*	    IF COD.REP.LEGAL NE '' THEN
*	        TITULAR.CUENTA = REP.LEGAL
*	        DATOS := 'TITULARC!' : TITULAR.CUENTA : CHAR(10)
*	    END ELSE
*	        DATOS := 'TITULARC!' : TITULAR.CUENTA : CHAR(10)
*	    END
    DATOS := 'TITULARC!' : TITULAR.CUENTA : CHAR(10)

;* Buscando las tarifas
    CALL F.READ(FN.TABLE.PA, 'TAR.CTA.CTE.PN1', R.TABLE.PA, F.TABLE.PA, F.ERR.PA)
    DATOS := 'TARIFA01!' : R.TABLE.PA<EB.SLV39.VALOR.PARAM> : CHAR(10)
    CALL F.READ(FN.TABLE.PA, 'TAR.CTA.CTE.PN2', R.TABLE.PA, F.TABLE.PA, F.ERR.PA)
    DATOS := 'TARIFA02!' : R.TABLE.PA<EB.SLV39.VALOR.PARAM> : CHAR(10)
    CALL F.READ(FN.TABLE.PA, 'TAR.CTA.CTE.PN3', R.TABLE.PA, F.TABLE.PA, F.ERR.PA)
    DATOS := 'TARIFA03!' : R.TABLE.PA<EB.SLV39.VALOR.PARAM> : CHAR(10)
    CALL F.READ(FN.TABLE.PA, 'TAR.CTA.CTE.PN4', R.TABLE.PA, F.TABLE.PA, F.ERR.PA)
    DATOS := 'TARIFA04!' : R.TABLE.PA<EB.SLV39.VALOR.PARAM> : CHAR(10)
    CALL F.READ(FN.TABLE.PA, 'TAR.CTA.CTE.PN5', R.TABLE.PA, F.TABLE.PA, F.ERR.PA)
    DATOS := 'TARIFA05!' : R.TABLE.PA<EB.SLV39.VALOR.PARAM> : CHAR(10)
    CALL F.READ(FN.TABLE.PA, 'TAR.CTA.CTE.PN6', R.TABLE.PA, F.TABLE.PA, F.ERR.PA)
    DATOS := 'TARIFA06!' : R.TABLE.PA<EB.SLV39.VALOR.PARAM> : CHAR(10)
    CALL F.READ(FN.TABLE.PA, 'TAR.CTA.CTE.PN7', R.TABLE.PA, F.TABLE.PA, F.ERR.PA)
    DATOS := 'TARIFA07!' : R.TABLE.PA<EB.SLV39.VALOR.PARAM> : CHAR(10)
    CALL F.READ(FN.TABLE.PA, 'TAR.CTA.CTE.PN8', R.TABLE.PA, F.TABLE.PA, F.ERR.PA)
    DATOS := 'TARIFA08!' : R.TABLE.PA<EB.SLV39.VALOR.PARAM> : CHAR(10)
    CALL F.READ(FN.TABLE.PA, 'TAR.CTA.CTE.PN9', R.TABLE.PA, F.TABLE.PA, F.ERR.PA)
    DATOS := 'TARIFA09!' : R.TABLE.PA<EB.SLV39.VALOR.PARAM> : CHAR(10)
    CALL F.READ(FN.TABLE.PA, 'TAR.CTA.CTE.PN10', R.TABLE.PA, F.TABLE.PA, F.ERR.PA)
    DATOS := 'TARIFA10!' : R.TABLE.PA<EB.SLV39.VALOR.PARAM> : CHAR(10)
    CALL F.READ(FN.TABLE.PA, 'TAR.CTA.CTE.PN11', R.TABLE.PA, F.TABLE.PA, F.ERR.PA)
    DATOS := 'TARIFA11!' : R.TABLE.PA<EB.SLV39.VALOR.PARAM> : CHAR(10)
    CALL F.READ(FN.TABLE.PA, 'TAR.CTA.CTE.PN12', R.TABLE.PA, F.TABLE.PA, F.ERR.PA)
    DATOS := 'TARIFA12!' : R.TABLE.PA<EB.SLV39.VALOR.PARAM> : CHAR(10)
    CALL F.READ(FN.TABLE.PA, 'TAR.CTA.CTE.PN13', R.TABLE.PA, F.TABLE.PA, F.ERR.PA)
    DATOS := 'TARIFA13!' : R.TABLE.PA<EB.SLV39.VALOR.PARAM> : CHAR(10)


    RETURN

*-----------------------------------------------------------------------------
CONSTRUIR.CTE.PJ:
*-----------------------------------------------------------------------------

    BUSCAR.CONTRATO = 'azul_cuenta_corriente_empresa.jdt'

;* Fecha de la SSF
    CALL F.READ(FN.TABLE.PA, 'FECHA.SSF.CONT.CC.PJ', R.TABLE.PA, F.TABLE.PA, F.ERR.PA)
    FECHA.SSF = R.TABLE.PA<EB.SLV39.VALOR.PARAM>

;* Encontrando el nombre de la sociedad
;*--------------------------------------------------------------------------------------------------------
;* Se agrega IF en caso que RazonSocial No Tenga Dato Use Los Nombres de la Sociedad : OCORNEJO 30.09.2015
;*--------------------------------------------------------------------------------------------------------
    NOMBRE.SOCTEMP = LS.RAZON.SOCIAL
;* las dos lineas a continuacion se mantienen siempre para que recupera quien es el owner y obtenga los datos de PJ en empresa
    COD.SOCIEDAD = R.TABLE.ACU<AA.CUS.OWNER>
    CALL F.READ(FN.TABLE.CUS, COD.SOCIEDAD, R.TABLE.CUS, F.TABLE.CUS, F.ERR.CUS)

    IF NOMBRE.SOCTEMP EQ '' THEN
        *        COD.SOCIEDAD = R.TABLE.ACU<AA.CUS.OWNER>
        *        CALL F.READ(FN.TABLE.CUS, COD.SOCIEDAD, R.TABLE.CUS, F.TABLE.CUS, F.ERR.CUS)
        NOMBRE.SOCTEMP = SWAP(R.TABLE.CUS<EB.CUS.NAME.1> :' ' : R.TABLE.CUS<EB.CUS.NAME.2>, @SM, ' ')
        NOMBRE.SOCTEMP = SWAP(NOMBRE.SOCTEMP, @VM, ' ')
        NOMBRE.SOCTEMP = SWAP(NOMBRE.SOCTEMP, @FM, ' ')
        NOMBRE.SOCTEMP = SWAP(NOMBRE.SOCTEMP, @TM, ' ')
    END

    NOMBRE.SOCIEDAD = NOMBRE.SOCTEMP
;* NOMBRE.SOCIEDAD = R.TABLE.CUS<EB.CUS.NAME.1> :' ' : R.TABLE.CUS<EB.CUS.NAME.2>
    DATOS := 'NOMSOCIE!' : NOMBRE.SOCIEDAD : CHAR(10)

;* Encontrando el municipio de la sociedad
    CALL GET.LOC.REF ('CUSTOMER', 'LF.MUNICIPIO', POS)
    COD.MUNICIPIO = R.TABLE.CUS<EB.CUS.LOCAL.REF, POS>
    CALL F.READ(FN.TABLE.MUN, COD.MUNICIPIO, R.TABLE.MUN, F.TABLE.MUN, F.ERR.MUN)
    MUNICIPIO.SOCIEDAD = R.TABLE.MUN<EB.SLV33.DESCRIPTION>
    DATOS := 'MUNICSOC!' : MUNICIPIO.SOCIEDAD : CHAR(10)

;* Encontrando el departamento de la sociedad
    CALL GET.LOC.REF ('CUSTOMER', 'LF.DEPARTAMENTO', POS)
    COD.DEPARTAMENTO = R.TABLE.CUS<EB.CUS.LOCAL.REF, POS>
    CALL F.READ(FN.TABLE.DEP, COD.DEPARTAMENTO, R.TABLE.DEP, F.TABLE.DEP, F.ERR.DEP)
    DEPARTAMENTO.SOCIEDAD = R.TABLE.DEP<EB.SLV43.DESCRIPTION>
    DATOS := 'DEPTOSOC!' : DEPARTAMENTO.SOCIEDAD : CHAR(10)

;* Encontrando el NIT de la sociedad
    FINDSTR 'NUM.IDEN.TRIBUT' IN R.TABLE.CUS<EB.CUS.LEGAL.DOC.NAME> SETTING AP, VP THEN
        NIT.SOCIEDAD = R.TABLE.CUS<EB.CUS.LEGAL.ID, VP>
    END ELSE
        NIT.SOCIEDAD = '______________'
    END
    DATOS := 'NITSOCIE!' : NIT.SOCIEDAD : CHAR(10)

;* Encontrando el número de registro de la sociedad
    FINDSTR 'REG.COMER' IN R.TABLE.CUS<EB.CUS.LEGAL.DOC.NAME> SETTING AP, VP THEN
        NUMERO.REGISTRO = R.TABLE.CUS<EB.CUS.LEGAL.ID, VP>
    END ELSE
        NUMERO.REGISTRO = '______________'
    END
    DATOS := 'NUMEREGI!' : NUMERO.REGISTRO : CHAR(10)


;* Indicando el titular de la cuenta
    TITULAR.CUENTA = NOMBRE.SOCIEDAD
    DATOS := 'TITULARC!' : TITULAR.CUENTA : CHAR(10)

;* Buscando las tarifas
    CALL F.READ(FN.TABLE.PA, 'TAR.CTA.CTE.EM1', R.TABLE.PA, F.TABLE.PA, F.ERR.PA)
    DATOS := 'TARIFA01!' : R.TABLE.PA<EB.SLV39.VALOR.PARAM> : CHAR(10)
    CALL F.READ(FN.TABLE.PA, 'TAR.CTA.CTE.EM2', R.TABLE.PA, F.TABLE.PA, F.ERR.PA)
    DATOS := 'TARIFA02!' : R.TABLE.PA<EB.SLV39.VALOR.PARAM> : CHAR(10)
    CALL F.READ(FN.TABLE.PA, 'TAR.CTA.CTE.EM3', R.TABLE.PA, F.TABLE.PA, F.ERR.PA)
    DATOS := 'TARIFA03!' : R.TABLE.PA<EB.SLV39.VALOR.PARAM> : CHAR(10)
    CALL F.READ(FN.TABLE.PA, 'TAR.CTA.CTE.EM4', R.TABLE.PA, F.TABLE.PA, F.ERR.PA)
    DATOS := 'TARIFA04!' : R.TABLE.PA<EB.SLV39.VALOR.PARAM> : CHAR(10)
    CALL F.READ(FN.TABLE.PA, 'TAR.CTA.CTE.EM5', R.TABLE.PA, F.TABLE.PA, F.ERR.PA)
    DATOS := 'TARIFA05!' : R.TABLE.PA<EB.SLV39.VALOR.PARAM> : CHAR(10)
    CALL F.READ(FN.TABLE.PA, 'TAR.CTA.CTE.EM6', R.TABLE.PA, F.TABLE.PA, F.ERR.PA)
    DATOS := 'TARIFA06!' : R.TABLE.PA<EB.SLV39.VALOR.PARAM> : CHAR(10)
    CALL F.READ(FN.TABLE.PA, 'TAR.CTA.CTE.EM7', R.TABLE.PA, F.TABLE.PA, F.ERR.PA)
    DATOS := 'TARIFA07!' : R.TABLE.PA<EB.SLV39.VALOR.PARAM> : CHAR(10)
    CALL F.READ(FN.TABLE.PA, 'TAR.CTA.CTE.EM8', R.TABLE.PA, F.TABLE.PA, F.ERR.PA)
    DATOS := 'TARIFA08!' : R.TABLE.PA<EB.SLV39.VALOR.PARAM> : CHAR(10)
    CALL F.READ(FN.TABLE.PA, 'TAR.CTA.CTE.EM9', R.TABLE.PA, F.TABLE.PA, F.ERR.PA)
    DATOS := 'TARIFA09!' : R.TABLE.PA<EB.SLV39.VALOR.PARAM> : CHAR(10)
    CALL F.READ(FN.TABLE.PA, 'TAR.CTA.CTE.EM10', R.TABLE.PA, F.TABLE.PA, F.ERR.PA)
    DATOS := 'TARIFA10!' : R.TABLE.PA<EB.SLV39.VALOR.PARAM> : CHAR(10)
    CALL F.READ(FN.TABLE.PA, 'TAR.CTA.CTE.EM11', R.TABLE.PA, F.TABLE.PA, F.ERR.PA)
    DATOS := 'TARIFA11!' : R.TABLE.PA<EB.SLV39.VALOR.PARAM> : CHAR(10)
    CALL F.READ(FN.TABLE.PA, 'TAR.CTA.CTE.EM12', R.TABLE.PA, F.TABLE.PA, F.ERR.PA)
    DATOS := 'TARIFA12!' : R.TABLE.PA<EB.SLV39.VALOR.PARAM> : CHAR(10)
    CALL F.READ(FN.TABLE.PA, 'TAR.CTA.CTE.EM13', R.TABLE.PA, F.TABLE.PA, F.ERR.PA)
    DATOS := 'TARIFA13!' : R.TABLE.PA<EB.SLV39.VALOR.PARAM> : CHAR(10)
    CALL F.READ(FN.TABLE.PA, 'TAR.CTA.CTE.EM14', R.TABLE.PA, F.TABLE.PA, F.ERR.PA)
    DATOS := 'TARIFA14!' : R.TABLE.PA<EB.SLV39.VALOR.PARAM> : CHAR(10)
    CALL F.READ(FN.TABLE.PA, 'TAR.CTA.CTE.EM15', R.TABLE.PA, F.TABLE.PA, F.ERR.PA)
    DATOS := 'TARIFA15!' : R.TABLE.PA<EB.SLV39.VALOR.PARAM> : CHAR(10)

    RETURN


;*20170926 Creacion de subproceso para obtener la generación del contrato del cliente tipo juridico.
;*@Author:Ronald Ortiz
*-----------------------------------------------------------------------------
CONSTRUIR.AHO.PJ:
*-----------------------------------------------------------------------------
    BUSCAR.CONTRATO = 'azul_cuenta_ahorro_elect_empresas.jdt'

;*Fecha de la SSF
    CALL F.READ(FN.TABLE.PA, 'FECHA.SSF.CONT.AH.PJ', R.TABLE.PA, F.TABLE.PA, F.ERR.PA)
    FECHA.SSF = R.TABLE.PA<EB.SLV39.VALOR.PARAM>

;* Encontrando el nombre de la sociedad
;*--------------------------------------------------------------------------------------------------------
;* Se agrega IF en caso que RazonSocial No Tenga Dato Use Los Nombres de la Sociedad : OCORNEJO 30.09.2015
;*--------------------------------------------------------------------------------------------------------
    NOMBRE.SOCTEMP = LS.RAZON.SOCIAL
;* las dos lineas a continuacion se mantienen siempre para que recupera quien es el owner y obtenga los datos de PJ en empresa
    COD.SOCIEDAD = R.TABLE.ACU<AA.CUS.OWNER>
    CALL F.READ(FN.TABLE.CUS, COD.SOCIEDAD, R.TABLE.CUS, F.TABLE.CUS, F.ERR.CUS)

    IF NOMBRE.SOCTEMP EQ '' THEN
*     COD.SOCIEDAD = R.TABLE.ACU<AA.CUS.OWNER>
*      CALL F.READ(FN.TABLE.CUS, COD.SOCIEDAD, R.TABLE.CUS, F.TABLE.CUS, F.ERR.CUS)
       NOMBRE.SOCTEMP = SWAP(R.TABLE.CUS<EB.CUS.NAME.1> :' ' : R.TABLE.CUS<EB.CUS.NAME.2>, @SM, ' ')
       NOMBRE.SOCTEMP = SWAP(NOMBRE.SOCTEMP, @VM, ' ')
       NOMBRE.SOCTEMP = SWAP(NOMBRE.SOCTEMP, @FM, ' ')
       NOMBRE.SOCTEMP = SWAP(NOMBRE.SOCTEMP, @TM, ' ')
    END

    NOMBRE.SOCIEDAD = NOMBRE.SOCTEMP
;* NOMBRE.SOCIEDAD = R.TABLE.CUS<EB.CUS.NAME.1> :' ' : R.TABLE.CUS<EB.CUS.NAME.2>
    DATOS := 'NOMSOCIE!' : NOMBRE.SOCIEDAD : CHAR(10)

   ;* Encontrando el municipio de la sociedad
    CALL GET.LOC.REF ('CUSTOMER', 'LF.MUNICIPIO', POS)
    COD.MUNICIPIO = R.TABLE.CUS<EB.CUS.LOCAL.REF, POS>
    CALL F.READ(FN.TABLE.MUN, COD.MUNICIPIO, R.TABLE.MUN, F.TABLE.MUN, F.ERR.MUN)
    MUNICIPIO.SOCIEDAD = R.TABLE.MUN<EB.SLV33.DESCRIPTION>
    DATOS := 'MUNICSOC!' : MUNICIPIO.SOCIEDAD : CHAR(10)

;* Encontrando el departamento de la sociedad
    CALL GET.LOC.REF ('CUSTOMER', 'LF.DEPARTAMENTO', POS)
    COD.DEPARTAMENTO = R.TABLE.CUS<EB.CUS.LOCAL.REF, POS>
    CALL F.READ(FN.TABLE.DEP, COD.DEPARTAMENTO, R.TABLE.DEP, F.TABLE.DEP, F.ERR.DEP)
    DEPARTAMENTO.SOCIEDAD = R.TABLE.DEP<EB.SLV43.DESCRIPTION>
    DATOS := 'DEPTOSOC!' : DEPARTAMENTO.SOCIEDAD : CHAR(10)
    
    ;* Encontrando el NIT de la sociedad
    FINDSTR 'NUM.IDEN.TRIBUT' IN R.TABLE.CUS<EB.CUS.LEGAL.DOC.NAME> SETTING AP, VP THEN
        NIT.SOCIEDAD = R.TABLE.CUS<EB.CUS.LEGAL.ID, VP>
    END ELSE
        NIT.SOCIEDAD = '______________'
    END
    DATOS := 'NITSOCIE!' : NIT.SOCIEDAD : CHAR(10)
    
    ;* Encontrando el número de registro de la sociedad
    FINDSTR 'REG.COMER' IN R.TABLE.CUS<EB.CUS.LEGAL.DOC.NAME> SETTING AP, VP THEN
        NUMERO.REGISTRO = R.TABLE.CUS<EB.CUS.LEGAL.ID, VP>
    END ELSE
        NUMERO.REGISTRO = '______________'
    END
    DATOS := 'NUMEREGI!' : NUMERO.REGISTRO : CHAR(10)


;* Indicando el titular de la cuenta
    TITULAR.CUENTA = NOMBRE.SOCIEDAD
    DATOS := 'TITULARC!' : TITULAR.CUENTA : CHAR(10)
    
;*Buscando las tarifas
    CALL F.READ(FN.TABLE.PA, 'TAR.CTA.AHO.PJ.EM1', R.TABLE.PA, F.TABLE.PA, F.ERR.PA)
    DATOS := 'TARIFA01!' : R.TABLE.PA<EB.SLV39.VALOR.PARAM> : CHAR(10)
    CALL F.READ(FN.TABLE.PA, 'TAR.CTA.AHO.PJ.EM2', R.TABLE.PA, F.TABLE.PA, F.ERR.PA)
    DATOS := 'TARIFA02!' : R.TABLE.PA<EB.SLV39.VALOR.PARAM> : CHAR(10)
    CALL F.READ(FN.TABLE.PA, 'TAR.CTA.AHO.PJ.EM3', R.TABLE.PA, F.TABLE.PA, F.ERR.PA)
    DATOS := 'TARIFA03!' : R.TABLE.PA<EB.SLV39.VALOR.PARAM> : CHAR(10)
    CALL F.READ(FN.TABLE.PA, 'TAR.CTA.AHO.PJ.EM4', R.TABLE.PA, F.TABLE.PA, F.ERR.PA)
    DATOS := 'TARIFA04!' : R.TABLE.PA<EB.SLV39.VALOR.PARAM> : CHAR(10)
    
    RETURN   ;*END CONSTRUIR.AHO.PJ


*-----------------------------------------------------------------------------
CONSTRUIR.AHO.PN:
*-----------------------------------------------------------------------------


    IF REP.LEGAL NE '' THEN
        BUSCAR.CONTRATO = 'azul_cta_ahorro_elect_pn_menor.jdt'
    END ELSE
        BUSCAR.CONTRATO = 'azul_cuenta_ahorro_electronica_persona_natural.jdt'
    END

;* Fecha de la SSF
    CALL F.READ(FN.TABLE.PA, 'FECHA.SSF.CONT.AH.PN', R.TABLE.PA, F.TABLE.PA, F.ERR.PA)
    FECHA.SSF = R.TABLE.PA<EB.SLV39.VALOR.PARAM>

    COD.CLIENTE = R.TABLE.ACU<AA.CUS.PRIMARY.OWNER>
    CALL F.READ(FN.TABLE.CUS, COD.CLIENTE, R.TABLE.CUS, F.TABLE.CUS, F.ERR.CUS)

    PRIMER.NOMBRE = TRIM(R.TABLE.CUS<EB.CUS.NAME.1>)
    SEGUNDO.NOMBRE = TRIM(R.TABLE.CUS<EB.CUS.NAME.2>)
    TERCER.NOMBRE = TRIM(R.TABLE.CUS<EB.CUS.GIVEN.NAMES>)
    PRIMER.APELLIDO = TRIM(R.TABLE.CUS<EB.CUS.TEXT>)
    SEGUNDO.APELLIDO = TRIM(R.TABLE.CUS<EB.CUS.FAMILY.NAME>)
    APELLIDO.CASADA = TRIM(R.TABLE.CUS<EB.CUS.PREVIOUS.NAME>)
    NOMBRES = PRIMER.NOMBRE : ' ' : SEGUNDO.NOMBRE : ' ' : TERCER.NOMBRE
    APELLIDOS = PRIMER.APELLIDO : ' ' : SEGUNDO.APELLIDO : ' ' : APELLIDO.CASADA
    CLIENTE = TRIM(NOMBRES : ' ' : APELLIDOS)

    CALL GET.LOC.REF('AA.ARR.CUSTOMER', 'LF.CON.INDIS', POS)
    CONDICION = R.TABLE.ACU<AA.CUS.LOCAL.REF, POS>
    IF CONDICION EQ 'Y' OR CONDICION EQ 'O' THEN
        DATOS := 'NOMBREC1!' : CLIENTE : CHAR(10)
        TITULAR.CUENTA = CLIENTE : ' ' : LOWCASE(CONDICION)
    END ELSE
        DATOS := 'NOMBREC1!' : CLIENTE : CHAR(10)
        CONDICION = ''
        TITULAR.CUENTA = CLIENTE
    END

;*cambio en calculo de edad de cliente RGaray 20150724
    EDAD.CALCULO = R.TABLE.CUS<EB.CUS.DATE.OF.BIRTH>
;* DEBUG EDad
;*EDAD.CALCULO = '19850724'
    GOSUB CALCULAR.EDAD


    DATOS := 'EDADCLI1!' : EDAD.CLIENTE : ' AÑOS ' : CHAR(10)

;* Encontrando el departamento del propietario
    CALL GET.LOC.REF('CUSTOMER', 'LF.MUNICIPIO', POS)
    COD.MUNICIPIO = R.TABLE.CUS<EB.CUS.LOCAL.REF, POS>
    CALL F.READ(FN.TABLE.MUN, COD.MUNICIPIO, R.TABLE.MUN, F.TABLE.MUN, F.ERR.MUN)
    MUNI.CLIENTE = R.TABLE.MUN<EB.SLV33.DESCRIPTION>

    CALL GET.LOC.REF ('CUSTOMER', 'LF.DEPARTAMENTO', POS)
    COD.DEPARTAMENTO = R.TABLE.CUS<EB.CUS.LOCAL.REF, POS>
    CALL F.READ(FN.TABLE.DEP, COD.DEPARTAMENTO, R.TABLE.DEP, F.TABLE.DEP, F.ERR.DEP)
    DEPARTAMENTO.CLIENTE = R.TABLE.DEP<EB.SLV43.DESCRIPTION>

    DATOS := 'DOMICIL1!' : MUNI.CLIENTE : ', ' : DEPARTAMENTO.CLIENTE : CHAR(10)

;* Encontrando el número y tipo de documento principal
    FINDSTR 'DOCTO.UNICO.IDENT' IN R.TABLE.CUS<EB.CUS.LEGAL.DOC.NAME> SETTING AP, VP THEN
        DOC.CLIENTE = R.TABLE.CUS<EB.CUS.LEGAL.ID, VP>
        TIPO.DOC.CLIENTE = 'DUI'
    END
    ELSE FINDSTR 'PASSPORT' IN R.TABLE.CUS<EB.CUS.LEGAL.DOC.NAME> SETTING AP, VP THEN
    DOC.CLIENTE = R.TABLE.CUS<EB.CUS.LEGAL.ID, VP>
    TIPO.DOC.CLIENTE = 'pasaporte'
    END
    ELSE FINDSTR 'CARNET.RESIDENTE' IN R.TABLE.CUS<EB.CUS.LEGAL.DOC.NAME> SETTING AP, VP THEN
    DOC.CLIENTE = R.TABLE.CUS<EB.CUS.LEGAL.ID, VP>
    TIPO.DOC.CLIENTE = 'Carnet de Residente'
    END
    ELSE FINDSTR 'CARNET.MENOR.EDAD' IN R.TABLE.CUS<EB.CUS.LEGAL.DOC.NAME> SETTING AP, VP THEN
    DOC.CLIENTE = R.TABLE.CUS<EB.CUS.LEGAL.ID, VP>
    TIPO.DOC.CLIENTE = 'Carnet de Menor de Edad'
    END
    ELSE
    DOC.CLIENTE = '_______________'
    TIPO.DOC.CLIENTE = '_______________'
    END
    DATOS := 'TIPODOC1!' : TIPO.DOC.CLIENTE : CHAR(10)
    DATOS := 'NUMERDO1!' : DOC.CLIENTE : ' ' : CONDICION : CHAR(10)

    DOCS.CTA.MANCOMUNADAS = TIPO.DOC.CLIENTE:"-":DOC.CLIENTE;*SETTEO EL DUI DEL REPRESENTANTE LEGAL
    IF CONDICION EQ 'Y' OR CONDICION EQ 'O' THEN
        CONTADOR = DCOUNT(R.TABLE.ACU<AA.CUS.OTHER.PARTY>, @VM)
        FOR I = 1 TO CONTADOR
            COD.CLIENTE = R.TABLE.ACU<AA.CUS.OTHER.PARTY><1,I>
            CALL F.READ(FN.TABLE.CUS, COD.CLIENTE, R.TABLE.CUS, F.TABLE.CUS, F.ERR.CUS)

            PRIMER.NOMBRE = TRIM(R.TABLE.CUS<EB.CUS.NAME.1>)
            SEGUNDO.NOMBRE = TRIM(R.TABLE.CUS<EB.CUS.NAME.2>)
            TERCER.NOMBRE = TRIM(R.TABLE.CUS<EB.CUS.GIVEN.NAMES>)
            PRIMER.APELLIDO = TRIM(R.TABLE.CUS<EB.CUS.TEXT>)
            SEGUNDO.APELLIDO = TRIM(R.TABLE.CUS<EB.CUS.FAMILY.NAME>)
            APELLIDO.CASADA = TRIM(R.TABLE.CUS<EB.CUS.PREVIOUS.NAME>)
            NOMBRES = PRIMER.NOMBRE : ' ' : SEGUNDO.NOMBRE : ' ' : TERCER.NOMBRE
            APELLIDOS = PRIMER.APELLIDO : ' ' : SEGUNDO.APELLIDO : ' ' : APELLIDO.CASADA
            CLIENTE = TRIM(NOMBRES : ' ' : APELLIDOS)

            IF I <> CONTADOR THEN
                DATOS := 'NOMBREC': I + 1 : '!' : CLIENTE : CHAR(10)
                TITULAR.CUENTA = TITULAR.CUENTA : ' ' : CLIENTE : ' ' : LOWCASE(CONDICION)
            END ELSE
                CONDICION = ''
                DATOS := 'NOMBREC': I + 1 : '!' : CLIENTE : CHAR(10)
                TITULAR.CUENTA = TITULAR.CUENTA : ' ' : CLIENTE
            END

            ;*cambio en calculo de edad de cliente RGaray 20150724
            EDAD.CALCULO = R.TABLE.CUS<EB.CUS.DATE.OF.BIRTH>
            ;* DEBUG EDad
            ;*EDAD.CALCULO = '19850724'
            GOSUB CALCULAR.EDAD

            ;* calculo antiguo
            *	            EDAD.CLIENTE = OCONV(DATE(), 'DY') - SUBSTRINGS(R.TABLE.CUS<EB.CUS.DATE.OF.BIRTH>, 1, 4)
            *	            IF OCONV(DATE(), 'DM') - SUBSTRINGS(R.TABLE.CUS<EB.CUS.DATE.OF.BIRTH>, 6, 2) > 0 THEN
            *	                EDAD.CLIENTE = EDAD.CLIENTE + 1
            *	            END ELSE IF OCONV(DATE(), 'DM') - SUBSTRINGS(R.TABLE.CUS<EB.CUS.DATE.OF.BIRTH>, 6, 2) = 0 THEN
            *	                IF OCONV(DATE(), 'DD') - SUBSTRINGS(R.TABLE.CUS<EB.CUS.DATE.OF.BIRTH>, 4, 2) > 0 THEN
            *	                    EDAD.CLIENTE = EDAD.CLIENTE + 1
            *	                END
            *	            END
            DATOS := 'EDADCLI': I + 1 : '!' : EDAD.CLIENTE : ' AÑOS ' : CHAR(10)


            CALL GET.LOC.REF('CUSTOMER', 'LF.MUNICIPIO', POS)
            COD.MUNICIPIO = R.TABLE.CUS<EB.CUS.LOCAL.REF, POS>
            CALL F.READ(FN.TABLE.MUN, COD.MUNICIPIO, R.TABLE.MUN, F.TABLE.MUN, F.ERR.MUN)
            MUNI.CLIENTE = R.TABLE.MUN<EB.SLV33.DESCRIPTION>

            CALL GET.LOC.REF ('CUSTOMER', 'LF.DEPARTAMENTO', POS)
            COD.DEPARTAMENTO = R.TABLE.CUS<EB.CUS.LOCAL.REF, POS>
            CALL F.READ(FN.TABLE.DEP, COD.DEPARTAMENTO, R.TABLE.DEP, F.TABLE.DEP, F.ERR.DEP)
            DEPARTAMENTO.CLIENTE = R.TABLE.DEP<EB.SLV43.DESCRIPTION>

            DATOS := 'DOMICIL' : I + 1 : '!' : MUNI.CLIENTE : ', ' : DEPARTAMENTO.CLIENTE : CHAR(10)

            FINDSTR 'DOCTO.UNICO.IDENT' IN R.TABLE.CUS<EB.CUS.LEGAL.DOC.NAME> SETTING AP, VP THEN
                DOC.CLIENTE = R.TABLE.CUS<EB.CUS.LEGAL.ID, VP>
                TIPO.DOC.CLIENTE = 'DUI'
            END
            ELSE FINDSTR 'PASSPORT' IN R.TABLE.CUS<EB.CUS.LEGAL.DOC.NAME> SETTING AP, VP THEN
            DOC.CLIENTE = R.TABLE.CUS<EB.CUS.LEGAL.ID, VP>
            TIPO.DOC.CLIENTE = 'pasaporte'
        END
        ELSE FINDSTR 'CARNET.RESIDENTE' IN R.TABLE.CUS<EB.CUS.LEGAL.DOC.NAME> SETTING AP, VP THEN
        DOC.CLIENTE = R.TABLE.CUS<EB.CUS.LEGAL.ID, VP>
        TIPO.DOC.CLIENTE = 'Carnet de Residente'
    END
    ELSE FINDSTR 'CARNET.MENOR.EDAD' IN R.TABLE.CUS<EB.CUS.LEGAL.DOC.NAME> SETTING AP, VP THEN
    DOC.CLIENTE = R.TABLE.CUS<EB.CUS.LEGAL.ID, VP>
    TIPO.DOC.CLIENTE = 'Carnet de Menor de Edad'
    END
    ELSE
    DOC.CLIENTE = '_______________'
    TIPO.DOC.CLIENTE = '_______________'
    END
    DATOS := 'TIPODOC' : I + 1 : '!' : TIPO.DOC.CLIENTE : CHAR(10)
    DATOS := 'NUMERDO' : I + 1 : '!' : DOC.CLIENTE : ' ' : CONDICION : CHAR(10)

*-----------------------------------------------------------------------------
;*OBTENER LOS DOCUMENTOS TIPO DUI PARA LAS CUENTAS MANCOMUNADAS
    DOCS.CTA.MANCOMUNADAS = DOCS.CTA.MANCOMUNADAS:', ':TIPO.DOC.CLIENTE:"-":DOC.CLIENTE

*-----------------------------------------------------------------------------

    NEXT I
    END



;* Buscando los titulares de las cuentas
*	    IF COD.REP.LEGAL NE '' THEN
*	        TITULAR.CUENTA = REP.LEGAL
*	        DATOS := 'TITULARC!' : TITULAR.CUENTA : CHAR(10)
*	    END ELSE
*	        DATOS := 'TITULARC!' : TITULAR.CUENTA : CHAR(10)
*	    END
    DATOS := 'TITULARC!' : TITULAR.CUENTA : CHAR(10)

;* Buscando las tarifas
    CALL F.READ(FN.TABLE.PA, 'TAR.CTA.AHO.PN1', R.TABLE.PA, F.TABLE.PA, F.ERR.PA)
    DATOS := 'TARIFA01!' : R.TABLE.PA<EB.SLV39.VALOR.PARAM> : CHAR(10)
    CALL F.READ(FN.TABLE.PA, 'TAR.CTA.AHO.PN2', R.TABLE.PA, F.TABLE.PA, F.ERR.PA)
    DATOS := 'TARIFA02!' : R.TABLE.PA<EB.SLV39.VALOR.PARAM> : CHAR(10)
    CALL F.READ(FN.TABLE.PA, 'TAR.CTA.AHO.PN3', R.TABLE.PA, F.TABLE.PA, F.ERR.PA)
    DATOS := 'TARIFA03!' : R.TABLE.PA<EB.SLV39.VALOR.PARAM> : CHAR(10)
    CALL F.READ(FN.TABLE.PA, 'TAR.CTA.AHO.PN4', R.TABLE.PA, F.TABLE.PA, F.ERR.PA)
    DATOS := 'TARIFA04!' : R.TABLE.PA<EB.SLV39.VALOR.PARAM> : CHAR(10)
    CALL F.READ(FN.TABLE.PA, 'TAR.CTA.AHO.PN5', R.TABLE.PA, F.TABLE.PA, F.ERR.PA)
    DATOS := 'TARIFA05!' : R.TABLE.PA<EB.SLV39.VALOR.PARAM> : CHAR(10)
    CALL F.READ(FN.TABLE.PA, 'TAR.CTA.AHO.PN6', R.TABLE.PA, F.TABLE.PA, F.ERR.PA)
    DATOS := 'TARIFA06!' : R.TABLE.PA<EB.SLV39.VALOR.PARAM> : CHAR(10)
    CALL F.READ(FN.TABLE.PA, 'TAR.CTA.AHO.PN7', R.TABLE.PA, F.TABLE.PA, F.ERR.PA)
    DATOS := 'TARIFA07!' : R.TABLE.PA<EB.SLV39.VALOR.PARAM> : CHAR(10)
    CALL F.READ(FN.TABLE.PA, 'TAR.CTA.AHO.PN8', R.TABLE.PA, F.TABLE.PA, F.ERR.PA)
    DATOS := 'TARIFA08!' : R.TABLE.PA<EB.SLV39.VALOR.PARAM> : CHAR(10)
    CALL F.READ(FN.TABLE.PA, 'TAR.CTA.AHO.PN9', R.TABLE.PA, F.TABLE.PA, F.ERR.PA)
    DATOS := 'TARIFA09!' : R.TABLE.PA<EB.SLV39.VALOR.PARAM> : CHAR(10)

    RETURN

END
