*------------------------------------------------------------------------------------------------------
* <Rating>1300</Rating>
*------------------------------------------------------------------------------------------------------
    SUBROUTINE SLV.NOF.UIF.OTROS.MEDIOS(ENQ.DATA)
* Nombre: SLV.NOF.UIF.OTROS.MEDIOS.b 
* Enquiry : SLV.NOF.UIF.OTROS.MEDIOS
* Descripcion: Rutina NOFILE para enquiry de desembolso de prostamos
*------------------------------------------------------------------------------------------------------
* Version	Autor		Fecha		Comentario
*------------------------------------------------------------------------------------------------------
* 1.0		Javier		30.03.16	Version inicial
* 1.1		Jonas		31.03.16	Formato de columnas de reporte
* 2.0		Jonás 		11.11.16	Agregar registros incluyendo monto ingresado en parámetro.
*									mostrar campos completos TK 12801.
*			Jonas		07.12.16	Optimizar rendimiento de generacion de datos.
* 2.1		ITurcios	27.01.17	Se agrega lectura a FT y se modifica descripciones para txn desde TCE.
* 2.2		ITurcios	26.10.17	Se agregan txn desde Bulk Payment Empresas y se añade logica para sus respectivos Narratives
* 2.3		ITurcios	20.11.17  	Se agrega modifica para que los abonos recibidos de bulk payment tengan la misma descripcion que los cargos a la empresa.
* 2.4		VBurgos		21.11.17	Se agrega descripcion MH Banca Personas 
*------------------------------------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_ENQUIRY.COMMON
    $INSERT I_F.EB.SLV.UIF.CHNL.TRX
    $INSERT I_F.ACCOUNT
    $INSERT I_F.ATM.TRANSACTION
    $INSERT I_F.COMPANY
    $INSERT I_F.FT.TXN.TYPE.CONDITION
    $INSERT I_F.SLV.CUS.MUNICIPIO
    $INSERT I_F.FUNDS.TRANSFER
    $INSERT I_F.EB.SLV.KEYS.PARAMS
    $INSERT I_F.FT.BULK.MASTER
    $INSERT I_F.EB.SLV.GLOBAL.PARAM
*-----------------------------------------------------------------------------

    GOSUB INIT
    GOSUB OPENFILE
    GOSUB PROCESS
*	CRT ENQ.DATA
    RETURN

INIT:
    FN.TXN = 'F.EB.SLV.UIF.CHNL.TRX'
    F.TXN  = ''
    FN.ACC = 'F.ACCOUNT'
    F.ACC  = ''
    FN.ATM = 'F.ATM.TRANSACTION'
    F.ATM  = ''
    FN.COM = 'F.COMPANY'
    F.COM  = ''
    FN.FT.TXN = 'F.FT.TXN.TYPE.CONDITION'
    F.FT.TXN = ''
    FN.MUN = 'F.EB.SLV.CUS.MUNICIPIO'
    F.MUN = ''
    FN.FT = 'F.FUNDS.TRANSFER'
    F.FT  = ''
    FN.FUNDS.TRANSFER.NAU = 'F.FUNDS.TRANSFER$NAU'
    F.FUNDS.TRANSFER.NAU = ''
    FN.FT$HIS = 'F.FUNDS.TRANSFER$HIS'
    F.FT$HIS  = ''
    FN.EB.KEYS.PARAM = 'F.EB.SLV.KEYS.PARAMS'
    F.EB.KEYS.PARAM	 = ''
    FN.BULK.MASTER	 = 'F.FT.BULK.MASTER'
    F.BULK.MASTER	 = ''
    FN.PRM 	= 'F.EB.SLV.GLOBAL.PARAM'
    F.PRM 	= ''

;*Parametro
;*---------
    LOCATE "FECHA.INICIO" IN D.FIELDS<1> SETTING POS THEN
    Y.FECHA.INICIO = D.RANGE.AND.VALUE<POS>
    END

    LOCATE "FECHA.FIN" IN D.FIELDS<1> SETTING POS THEN
    Y.FECHA.FIN = D.RANGE.AND.VALUE<POS>
    END

    LOCATE "AMOUNT" IN D.FIELDS<1> SETTING POS THEN
    Y.AMOUNT = D.RANGE.AND.VALUE<POS>
    END

;*debug
Y.FECHA.INICIO = '20171021'
Y.FECHA.FIN = '20171021'
Y.AMOUNT = '20000.01'

    EQU DEF.COMPANY TO 'SV0010001'
    EQU PERSONA.NAT.INIT TO '' : @VM : '' : @VM : '' : @VM : '' : @VM : '' : @VM : '' : @VM : '' : @VM : '' : @VM : '' : @VM : '' : @VM : '' : @VM : '' : @VM : '' : @VM : '' : @VM : ''
    EQU PERSONA.JUR.INIT TO '' : @VM : '' : @VM : '' : @VM : '' : @VM : ''

    Y.TIMEZONE = R.COMPANY(EB.COM.RESERVED.5)
    Y.DOT	   = "."
    Y.SEPARADOR= "T"
;*Pruebas debug
*		Y.FECHA.INICIO  = '20160101'
*		Y.FECHA.FIN     = '20171017'
*		Y.AMOUNT        = 3

    RETURN

OPENFILE:
    CALL OPF(FN.TXN, F.TXN)
    CALL OPF(FN.ACC, F.ACC)
    CALL OPF(FN.ATM, F.ATM)
    CALL OPF(FN.FT.TXN, F.FT.TXN)
    CALL OPF(FN.MUN, F.MUN)
    CALL OPF(FN.COM, F.COM)
    CALL OPF(FN.FT, F.FT)
    CALL OPF(FN.FUNDS.TRANSFER.NAU,F.FUNDS.TRANSFER.NAU)
    CALL OPF(FN.FT$HIS,F.FT$HIS)
    CALL OPF(FN.EB.KEYS.PARAM,F.EB.KEYS.PARAM)
    CALL OPF(FN.BULK.MASTER, F.BULK.MASTER)
    CALL OPF(FN.PRM, F.PRM)
    RETURN

PROCESS:
;*Lectura a la Tabla
;*------------------
    SELECT.STMT =  "SELECT FBNK.EB.SLV.UIF.CHNL.TRX" : " WITH TXN.DATE GE '" : Y.FECHA.INICIO : "'"
    SELECT.STMT	:= " AND TXN.DATE LE '" : Y.FECHA.FIN : "'"
    SELECT.STMT := " AND AMOUNT GE " : Y.AMOUNT
    CALL EB.READLIST(SELECT.STMT, Y.LIST, '', NO.REC, ERR.LIST)

;* Recorrer cada id de los registros de TXN
    LOOP
        REMOVE TXN.ID FROM Y.LIST SETTING POS01
    WHILE TXN.ID:POS01
        Y.COMPANY = DEF.COMPANY
        conceptoTransaccionPO = ''
        ;*Leer Registro
        ;*-------------
        CALL F.READ(FN.TXN, TXN.ID, R.TXN, F.TXN, ERR.TXN)

        ;*Llenar Campos
        ;*-------------
        Y.MONTOTRANS  = R.TXN<EB.SLV80.AMOUNT>
        Y.ESTADOTRANS = R.TXN<EB.SLV80.RECORD.STATUS>
        numeroRegistroBancario = TXN.ID[0,LEN(TXN.ID) - 9]

        IF Y.MONTOTRANS GE Y.AMOUNT AND Y.ESTADOTRANS EQ "AUTH" THEN
            ;*Continuar Llenado
            ;*-----------------
            ;*Conversion formato de fecha
            fechaTransaccion = R.TXN<EB.SLV80.DATE.TIME>
            GOSUB CONV_DATETIME

            ;*Persona que Realiza la Transaccion
            ;*----------------------------------
            TIPO.PERSONA.A = ''
            PERSONA.NAT.A  = PERSONA.NAT.INIT
            PERSONA.JUR.A  = PERSONA.JUR.INIT

            ;*Si la Cuenta es Interna No Hacer Nada y Dejar en Blanco los Campos
            ;*------------------------------------------------------------------
            IF R.TXN<EB.SLV80.ORIGIN.ACC>[0,3] NE 'USD' THEN
                CALL SLV.S.CUST.UIF.OTROS.MEDIOS(R.TXN<EB.SLV80.ORIGIN.CUST>, TIPO.PERSONA.A, PERSONA.NAT.A, PERSONA.JUR.A)
            END

            ;*Settear Persona A
            ;*-----------------
            tipoPersonaA   = TIPO.PERSONA.A
            listaPersonaA  = PERSONA.NAT.A
            listaJuridicaA = PERSONA.JUR.A

            ;*Persona que Recibe la Transaccion
            ;*---------------------------------
            TIPO.PERSONA.B = ''
            PERSONA.NAT.B  = PERSONA.NAT.INIT
            PERSONA.JUR.B  = PERSONA.JUR.INIT

            ;*Si la Cuenta es Interna No Hacer Nada y Dejar en Blanco los Campos
            ;*------------------------------------------------------------------
            IF R.TXN<EB.SLV80.DESTIN.ACC>[0,3] NE 'USD' THEN
                CALL SLV.S.CUST.UIF.OTROS.MEDIOS(R.TXN<EB.SLV80.DESTIN.CUST>, TIPO.PERSONA.B, PERSONA.NAT.B, PERSONA.JUR.B)
            END

            ;*Settear Persona B
            ;*-----------------
            tipoPersonaB   = TIPO.PERSONA.B
            listaPersonaB  = PERSONA.NAT.B
            listaJuridicaB = PERSONA.JUR.B

            ;*Cuenta Origen Transaccion
            ;*-------------------------
            CALL F.READ (FN.ACC, R.TXN<EB.SLV80.ORIGIN.ACC>, R.ACC, F.ACC, ERR.ACC)
            numeroCuentaPO = R.TXN<EB.SLV80.ORIGIN.ACC>
            claseCuentaPO  = R.ACC<AC.ACCOUNT.TITLE.1>

            ;*Leer FT para Concatenar Narrative configurado en App Local EB.SLV.KEYS.PARAMS
            GOSUB READ.FT

            ;*Leer informacion de Bulk Master para TCE
            Y.FLAG.BULK = 0
            IF Y.INWARD.PAY.TYPE THEN ;*Proviene de Bulk Payments
                GOSUB VAL.BULK.MASTER ;* Salida con bandera Y.FLAG.BULK = 1 que indica que es de Bulk Payment Empresas
            END

            ;*Buscando Concepto de Transaccion
            ;*--------------------------------
            SELECT.ATM = "SELECT FBNK.ATM.TRANSACTION" : " WITH TRANS.REF EQ '" : numeroRegistroBancario : "'"
            CALL EB.READLIST(SELECT.ATM, LIST.ATM, '', NO.REC.ATM, ERR.CODE.ATM)

            IF NO.REC.ATM GT 0 THEN
                Y.ID = LIST.ATM<NO.REC.ATM>
                CALL F.READ (FN.ATM, Y.ID, R.ATM, F.ATM, ERR.ATM)
                IF R.ATM THEN
                    conceptoTransaccionPO = R.ATM<AT.REV.NARRATIVE> : ' ' : R.ATM<AT.REV.CARD.ACC.NAME.LOC>
                    Y.COMPANY = R.ATM<AT.REV.COMPANY.CODE>
                END
            END

            IF conceptoTransaccionPO EQ '' THEN
            	DESC.CMPLMNT = ''
                IF  R.TXN<EB.SLV80.TXN.TYPE>  = 'AC64' THEN
                	GOSUB VAL.DESC.MH	
                END
                ;* Tratamiento de Narrative para Transacciones Banca Empresas
                IF LF.TCE.NARR NE '' OR Y.FLAG.BULK EQ 1 THEN
                    ID.KEYS.PARAMS 	= 'SUB.TXN.TYPE' ;*id con Narratives parametrizados en EB.KEYS.PARAMS

                    CALL F.READ(FN.EB.KEYS.PARAM, ID.KEYS.PARAMS, REC.SUBTYPE.TXN, F.EB.KEYS.PARAM, KEYS.PARAM.ER)

                    IF REC.SUBTYPE.TXN THEN
                        ;*Se forma el Id para traer Narrative en app KEYS.PARAM dentro de registro SUB.TXN.TYPE
                        IF Y.FLAG.BULK EQ 1 THEN ;*Elegir Narratives para Pagos Masivos desde TCE
                            ID.SUB.TYPE 	= R.TXN<EB.SLV80.TXN.TYPE> : '.' : Y.LF.TXN.BP
                        END ELSE ;* Transacciones no masivas desde TCE
                            ID.SUB.TYPE 	= R.TXN<EB.SLV80.TXN.TYPE> : '.' : LF.TCE.NARR
                        END

                        ;*Se lee ids de app Local
                        ID.KEYS.PARAM 	= REC.SUBTYPE.TXN<EB.SLV18.PARAM.ID>
                        ;*Se busca posición del id para leer narrative
                        FIND ID.SUB.TYPE IN ID.KEYS.PARAM SETTING POS.SUB.TYPE, POS.SUB.TYPE2 THEN
                        ;* Si tiene descripcion ES
                        IF DCOUNT(A.TXN.TYPE<FT6.DESCRIPTION>, VM) GT 1 THEN
                            conceptoTransaccionPO = A.TXN.TYPE<FT6.DESCRIPTION><1,1>: " " : REC.SUBTYPE.TXN<EB.SLV18.VALOR><1,POS.SUB.TYPE2> : ' ' :  DESC.CMPLMNT
                        END ELSE
                            conceptoTransaccionPO = A.TXN.TYPE<FT6.DESCRIPTION> : " " : REC.SUBTYPE.TXN<EB.SLV18.VALOR><1,POS.SUB.TYPE2> : ' ' :  DESC.CMPLMNT
                        END
                        
                    END
                END
            END ELSE
                CALL F.READ(FN.FT.TXN, R.TXN<EB.SLV80.TXN.TYPE>, R.FT.TXN, F.FT.TXN, ERR.FT.TXN)
                conceptoTransaccionPO = R.FT.TXN<FT6.DESCRIPTION><1, 1>: ' ' :  DESC.CMPLMNT
            END
        END

        ;*estacionServicio
        ;*----------------
        CALL F.READ (FN.COM, Y.COMPANY, R.COM, F.COM, ERR.COM)
        direccionAgencia = TRIM(R.COM<Company_NameAddress><1, 1>) : ' ' : TRIM(R.COM<Company_NameAddress><1, 2>)

        valorOtrosMediosElectronicoPO = R.TXN<EB.SLV80.AMOUNT>

        ;*Definicion de TIMEZONE
        Y.TIMEZONE.DEF = R.COM<EB.COM.RESERVED.5>

        ;*Evaluar nombre de municipio segun descripcion de catalago
        Y.NOMBRE.MUNICIPIO = UPCASE(R.COM<Company_NameAddress><1, 4>)
        IF Y.NOMBRE.MUNICIPIO EQ 'SANTA TECLA' THEN
            Y.NOMBRE.MUNICIPIO = 'NUEVA SAN SALVADOR'
        END

        SELECT.MUN = "SELECT " : FN.MUN : " WITH DESCRIPTION EQ '" : UPCASE(R.COM<Company_NameAddress><1, 4>) : "'"
        CALL EB.READLIST (SELECT.MUN, LIST.MUN, '', NO.REC.MUN, ERR.CODE.MUN)
        idDepartamento   = LIST.MUN<1>[3,2]
        idMunicipio      = LIST.MUN<1>[3,LEN(LIST.MUN<NO.REC.MUN>)]

        ;*Cuenta Destino Transaccion
        ;*--------------------------
        numeroProductoPB		  = ''
        claseCuentaPB  			  = ''
        montoTransaccionPB		  = ''
        valorMedioElectronicoPB   = ''
        bancoCuentaDestinatariaPB = ''

        ;*Validando antes de Extraer Informacion
        ;*--------------------------------------
        IF R.TXN<EB.SLV80.DESTIN.ACC>[0,3] NE 'USD' THEN
            CALL F.READ (FN.ACC, R.TXN<EB.SLV80.DESTIN.ACC>, R.ACC, F.ACC, ERR.ACC)
            numeroProductoPB		  = R.TXN<EB.SLV80.DESTIN.ACC>
            claseCuentaPB  			  = R.ACC<AC.ACCOUNT.TITLE.1>
            montoTransaccionPB		  = R.TXN<EB.SLV80.AMOUNT>
            valorMedioElectronicoPB   = R.TXN<EB.SLV80.AMOUNT>
            bancoCuentaDestinatariaPB = 'BANCO AZUL DE EL SALVADOR'
        END

        GOSUB SET
    END
    REPEAT
    RETURN

SET:
;*Construccion de arreglo
;*-----------------------
    STR.MOV = ''
    STR.MOV := numeroRegistroBancario 			: "*"	;* 1
    STR.MOV := direccionAgencia 				: "*"	;* 2
    STR.MOV := idMunicipio 						: "*"	;* 3
    STR.MOV := idDepartamento 					: "*"	;* 4
    STR.MOV := fechaTransaccion					: "*"	;* 5
    STR.MOV := tipoPersonaA   					: "*"	;* 6
    STR.MOV := CHANGE(listaPersonaA,@VM,'*')	: "*"	;* 7  - 21
    STR.MOV := CHANGE(listaJuridicaA,@VM,'*')	: "*"	;* 22 - 26
    STR.MOV := tipoPersonaB  					: "*"	;* 27
    STR.MOV := CHANGE(listaPersonaB,@VM,'*')	: "*"	;* 28 - 42
    STR.MOV := CHANGE(listaJuridicaB,@VM,'*')	: "*"	;* 43 - 47
    STR.MOV := numeroCuentaPO  					: "*"	;* 48
    STR.MOV := claseCuentaPO  					: "*"	;* 49
    STR.MOV := conceptoTransaccionPO			: "*"	;* 50
    STR.MOV := valorOtrosMediosElectronicoPO	: "*"	;* 51
    STR.MOV := numeroProductoPB					: "*"	;* 52
    STR.MOV := claseCuentaPB					: "*"	;* 53
    STR.MOV := montoTransaccionPB				: "*"	;* 54
    STR.MOV := valorMedioElectronicoPB			: "*"	;* 55
    STR.MOV := bancoCuentaDestinatariaPB				;* 56

    ENQ.DATA<-1> = STR.MOV
    RETURN

CONV_DATETIME:
    Y.FECHA = fechaTransaccion
    Y.TIME = ''
    Y.DATE = R.TXN<EB.SLV80.TXN.DATE>
    IF R.TXN<EB.SLV80.DATE.TIME> THEN
        FINDSTR Y.DOT IN Y.FECHA SETTING Ap, Vp THEN
            Y.TIME = OCONV(LOCALTIME(R.TXN<EB.SLV80.DATE.TIME>, Y.TIMEZONE),"MTS")
        END ELSE
            Y.TIME = OCONV(R.TXN<EB.SLV80.DATE.TIME>,"MT"):"00"
        END
    END ELSE
        Y.TIME = R.TXN<EB.SLV80.RESERVED.1>
    END
    Y.FECHA = Y.DATE:Y.SEPARADOR:Y.TIME
    fechaTransaccion = Y.FECHA
    RETURN

READ.FT:
    CALL F.READ(FN.FT, numeroRegistroBancario, A.RECORD, F.FT, ERR5)
    IF ERR5 NE '' THEN
        Y.FT.NAU.ERR = ""
        CALL F.READ(FN.FUNDS.TRANSFER.NAU, numeroRegistroBancario, A.RECORD, F.FUNDS.TRANSFER.NAU, Y.FT.NAU.ERR)
        IF Y.FT.NAU.ERR THEN
            CALL F.READ.HISTORY(FN.FT$HIS, numeroRegistroBancario, A.RECORD, F.FT$HIS, ERR.HIS)
            CALL F.READ(FN.FT.TXN, A.RECORD<FT.TRANSACTION.TYPE>, A.TXN.TYPE, F.FT.TXN, ERR6)
        END
        ELSE
        CALL F.READ(FN.FT.TXN, A.RECORD<FT.TRANSACTION.TYPE>, A.TXN.TYPE, F.FT.TXN, ERR6)
    END
    END
    ELSE
    CALL F.READ(FN.FT.TXN, A.RECORD<FT.TRANSACTION.TYPE>, A.TXN.TYPE, F.FT.TXN, ERR6)
    END

;* Obtener LF de Banca Empresas con id de Narrative para FT
    CALL GET.LOC.REF('FUNDS.TRANSFER','LF.TCE.NARR', POS.LF.TEC.NARR)
    LF.TCE.NARR		  = A.RECORD<FT.LOCAL.REF, POS.LF.TEC.NARR>
    Y.INWARD.PAY.TYPE = A.RECORD<FT.INWARD.PAY.TYPE> ;*Campo con id Bulk Master para Bulk Payment
    RETURN

VAL.BULK.MASTER:
    Y.ID.MASTER	= FIELD(Y.INWARD.PAY.TYPE, '-', 2)
	FINDSTR '.' IN Y.ID.MASTER SETTING Ap, Vp THEN ;*Caso para abonos a Clientes, viene id item : Ejm BKM0015700001152218L0W1.18219216875662502
		Y.ID.MASTER	= FIELD(Y.ID.MASTER, '.' , 1)
	END
	
    CALL CACHE.READ(FN.BULK.MASTER, Y.ID.MASTER, REC.MASTER, ERR.MASTER)

    IF REC.MASTER THEN
        ;*Tipo de Transaccion del Master Bulk Payments desde Banca Empresas
        CALL GET.LOC.REF('FT.BULK.MASTER','LF.TXN.BP', POS.TXN.BP)
        Y.LF.TXN.BP = REC.MASTER<FT.BLK.MAS.LOCAL.REF, POS.TXN.BP>

        IF Y.LF.TXN.BP THEN
            Y.FLAG.BULK = 1 ;*Viene desde bulk Payment TCE
        END
    END
RETURN

VAL.DESC.MH:
	;* Para obtener la cta. transitoria de MH NPE
	Y.PRM.ID = 'ACC.NPE.MH'
						
	CALL F.READ(FN.PRM, Y.PRM.ID, R.PRM, F.PRM, ERR.PRM)
						    
	IF R.PRM THEN
		CRED.ACCT = R.TXN<EB.SLV80.DESTIN.ACC>
		Y.CTA.MH = R.PRM<EB.SLV39.VALOR.PARAM>
		
		Y.PRM.ID = 'ACC.PAGOES.MH'
		CALL F.READ(FN.PRM, Y.PRM.ID, R.PRM, F.PRM, ERR.PRM)
		
		IF R.PRM THEN
			Y.CTA.MH.PAGOES = R.PRM<EB.SLV39.VALOR.PARAM>
		END
						
		IF CRED.ACCT EQ Y.CTA.MH OR CRED.ACCT EQ Y.CTA.MH.PAGOES THEN
			DESC.CMPLMNT = 'Ministerio de Hacienda'						           						         
		END
   	END
RETURN


END


