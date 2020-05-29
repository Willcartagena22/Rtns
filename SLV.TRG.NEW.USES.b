*-----------------------------------------------------------------------------
* <Rating>426</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE SLV.TRG.NEW.USES
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------

*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.EB.EXTERNAL.USER
    $INSERT I_F.CUSTOMER
    $INSERT I_TSS.COMMON
*-----------------------------------------------------------------------------
    ACC.ID =""
    CUS.ID =""
    FN.EXT.US  = "F.EB.EXTERNAL.USER"
    F.EXT.US   = ""
    FN.CUS = "F.CUSTOMER"
    F.CUS  = ""
    ;*Y.CUS.ID = '100694'
    ;*Y.US.ID = 'CRIVAS'
    ;*Y.IP = '172.16.12.93'
    ;*Y.MOV = '78425656'
*-----------------------------------------------------------------------------

    Y.CUS.ID = R.NEW(EB.XU.CUSTOMER)
	Y.US.ID = R.NEW(EB.XU.NAME)
	Y.IP = TSS$CLIENTIP


    Y.PORT=8000
    Y.COD.ERR = '2'
    Y.COD.SUC = '1'
*   Abrir External User
    CALL OPF (FN.EXT.US, F.EXT.US)
*   Abrir Account
    CALL OPF (FN.CUS, F.CUS)

    CALL F.READ (FN.CUS,Y.CUS.ID,R.CUS,F.CUS,ER)
    IF R.CUS NE '' THEN
        ;*Parametros para crear usuario

        Y.PRIMER.NOMBRE = R.CUS<EB.CUS.NAME.1>
        Y.PRIMER.APELLIDO = R.CUS<EB.CUS.TEXT>
        ;*TODO: Validar cuando email sea nulo
        Y.EMAIL = R.CUS<EB.CUS.EMAIL.1>

        THIS.PACKAGE.CLASS = "com.mad.banco.azul.hid.t24.T24HID"
        THIS.METHOD.USU = "createUser"
        ;*CALLJ.ARGUMENTS.USU = Y.US.ID:'%M':Y.PRIMER.NOMBRE:'%M':Y.PRIMER.APELLIDO:'%M':Y.EMAIL:'%M':Y.MOV
        CALLJ.ARGUMENTS.USU = Y.US.ID:'%M':Y.PRIMER.NOMBRE:'%M':Y.PRIMER.APELLIDO:'%M':Y.EMAIL

        CALLJ.ERROR.USU = ""
        CALLJ.RESPONSE.USU = ""

        THIS.METHOD.PWD = "createClaveEstatica"
        CALLJ.ARGUMENTS.PWD = ""
        CALLJ.ERROR.PWD = ""
        CALLJ.RESPONSE.PWD = ""

        THIS.METHOD.DEL = "borrarUsuario"
        CALLJ.ARGUMENTS.DEL = Y.US.ID
        CALLJ.ERROR.DEL = ""
        CALLJ.RESPONSE.DEL = ""

        ;*Llamada para la creacion de usuario
        CALL EB.CALLJ(THIS.PACKAGE.CLASS,THIS.METHOD.USU,CALLJ.ARGUMENTS.USU,CALLJ.RESPONSE.USU,CALLJ.ERROR.USU)

        IF CALLJ.ERROR.USU THEN
            E = "No Se puede crear el usuario - no se pudo invocar libreria: ":CALLJ.ERROR.USU:" ":Y.US.ID
            CALL ERR
            CRT E

        END ELSE
            Y.COD = FIELD(CALLJ.RESPONSE.USU,'%M',1)
            Y.MSG = FIELD(CALLJ.RESPONSE.USU,'%M',2)

            IF Y.COD EQ Y.COD.SUC THEN
                CALLJ.ARGUMENTS.PWD= Y.IP:':':Y.PORT:'%M':Y.US.ID

                ;*Llamada para la creacion de clave estatica
                CALL EB.CALLJ(THIS.PACKAGE.CLASS,THIS.METHOD.PWD,CALLJ.ARGUMENTS.PWD,CALLJ.RESPONSE.PWD,CALLJ.ERROR.PWD)
                IF CALLJ.ERROR.PWD THEN
                    ;*Si existe algun error en la creacion de clave estatica borrar usuario creado
                    CALL EB.CALLJ(THIS.PACKAGE.CLASS,THIS.METHOD.DEL,CALLJ.ARGUMENTS.DEL,CALLJ.RESPONSE.DEL,CALLJ.ERROR.DEL)
                    E = "No Se puede generar credenciales - no se pudo invocar libreria: ":CALLJ.ERROR.PWD:" ":Y.US.ID
                    CALL ERR
                    CRT E
                END ELSE
                    Y.COD = FIELD(CALLJ.RESPONSE.PWD,'%M',1)
                    Y.MSG = FIELD(CALLJ.RESPONSE.PWD,'%M',2)
                    CRT Y.COD:" ": Y.MSG
                    IF Y.COD NE Y.COD.SUC THEN
                        ;*Si existe algun error en la creacion de clave estatica borrar usuario creado
                        CALL EB.CALLJ(THIS.PACKAGE.CLASS,THIS.METHOD.DEL,CALLJ.ARGUMENTS.DEL,CALLJ.RESPONSE.DEL,CALLJ.ERROR.DEL)
                        E = "No Se puede generar credenciales - CODIGO DE ERROR : ":Y.COD:" ":Y.MSG:" ":Y.US.ID
                        CALL ERR
                        CRT E
                    END

                END

            END ELSE
                E = "No Se puede crear el usuario - CODIGO DE ERROR: ":Y.COD:" ":Y.MSG:" ":Y.US.ID
                CALL ERR
                CRT E
            END


        END

    END ELSE
        E = "No Se puede crear el usuario no existe: ":Y.MSG:'-':CALLJ.ERROR.USU:" ":Y.US.ID
        CALL ERR
        CRT E
    END
    END
