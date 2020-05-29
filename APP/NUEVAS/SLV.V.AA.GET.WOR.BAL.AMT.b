*-----------------------------------------------------------------------------
* <Rating>1206</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE SLV.V.AA.GET.WOR.BAL.AMT(PROPERTY.ID, START.DATE, END.DATE, CURRENT.DATE, BALANCE.TYPE, ACTIVITY.IDS,CURRENT.VALUE, START.VALUE, END.VALUE)
*-----------------------------------------------------------------------------
*
* Nombre: SLV.V.AA.GET.WOR.BAL.AMT
* Descripción: Rutina que se utiliza desde la aplicacion AA.PERIODIC.ATTRIBUTE.CLASS (Donde se definen la acción que debe activar esta regla para ser validada),
*			   se realiza la validacion de la aplicacion, el cliente, y si el proceso necesita Liberacion de Fondos por cheque en compensa
*
*
*-----------------------------------------------------------------------------
* Modification History :
* Autor		      Fecha		 Comentario
* JHenriquez	23.02.2017	Initial Code
* Jhenriquez    21.03.2018  Se agrega una inicializacion para la rutina CORE AA.GET.WORKING.BALANCE.AMOUNT al comienzo de la rutina
*							El motivo del cambio fue porque se detecto, que la cuenta aunque posea fondos, la rutina CORE detectaba en la varible
*							END.VALUE un saldo negativo, se declaro la variable con el valor 1, para que el flujo no se interrumpa y puede seguir
*							con la logica de la rutina, que se encarga de validar el saldo en las cuentas vinculadas en la transaccion de caja. 
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.ACCOUNT
    $INSERT I_F.CUSTOMER
    $INSERT I_AA.APP.COMMON
    $INSERT I_AA.ACTION.CONTEXT
    $INSERT I_F.EB.SLV.CUS.EARLY.RELEASE
    $INSERT I_F.AC.LOCKED.EVENTS
    $INSERT I_F.EB.SLV.GLOBAL.PARAM
    $INSERT I_F.EB.SLV.KEYS.PARAMS
    $INSERT I_F.FUNDS.TRANSFER
    $INSERT I_SLV.LIB.FDS.COMMON
    $INSERT I_GTS.COMMON
    $INSERT I_SLV.SET.VAR.IF

*================================================================
;*Inicializando la rutina CORE, con saldo a 1 para que permita ingresar en el proceso normal de la validacion de saldos
CALL AA.GET.WORKING.BALANCE.AMOUNT(PROPERTY.ID, START.DATE, END.DATE, CURRENT.DATE, BALANCE.TYPE, ACTIVITY.IDS,CURRENT.VALUE, START.VALUE, END.VALUE)
END.VALUE = 1

GOSUB INI

IF Y.CONTINUE EQ 'Y' THEN
   	CALL AA.GET.WORKING.BALANCE.AMOUNT(PROPERTY.ID, START.DATE, END.DATE, CURRENT.DATE, BALANCE.TYPE, ACTIVITY.IDS,CURRENT.VALUE, START.VALUE, END.VALUE)
   	END.VALUE = 1
END

GOSUB OPENFILE
;* El proceso que sigue esta rutina es validar si las TXN estan parametrizadas para el uso de Liberacion de Fondos en Compensa Local
;* Se observo el comportamiento de esta rutina, y en el resultado se encontro que por cada TT definida en la TFS el core intera con esta rutina
;* para controlar esa iteraciones, se utiliza una variables bandera, dicha variable de inicaliza en la rutina SLV.SET.AMT.TXN.ER y es una variable I_F
;* el motivo fue para controlar que no realize calculos despues de la primera iteracion ya que en la primera iteracion se realizan si aplica o no a
;* liberacion y si posee saldo suficiente en la cuenta 
V.OK = SUBSTRINGS(Y.ID.TXN, 1, 3)
IF V.OK EQ 'TFS'THEN
	IF Y.BANDERA.CONTROL EQ 1 THEN
		GOSUB PROCESS
    	RETURN
	END
	ELSE
;* si la variable bandera es distinto a 1 se altera la rutina valida si la cuenta posee saldo o no, ya el envio de OFS para controlar el saldo
;* se realiza cuando la bandera es 1, y se le deja quemado el valor 1 para que no muestre el mensaje CORE que detiene la transaccion y no permite el envio de OFS
	CALL AA.GET.WORKING.BALANCE.AMOUNT(PROPERTY.ID, START.DATE, END.DATE, CURRENT.DATE, BALANCE.TYPE, ACTIVITY.IDS,CURRENT.VALUE, START.VALUE, END.VALUE)
	END.VALUE = 1
END
END	
ELSE
;* si la variable bandera es distinto a 1 se altera la rutina valida si la cuenta posee saldo o no, ya el envio de OFS para controlar el saldo
;* se realiza cuando la bandera es 1, y se le deja quemado el valor 1 para que no muestre el mensaje CORE que detiene la transaccion y no permite el envio de OFS
	CALL AA.GET.WORKING.BALANCE.AMOUNT(PROPERTY.ID, START.DATE, END.DATE, CURRENT.DATE, BALANCE.TYPE, ACTIVITY.IDS,CURRENT.VALUE, START.VALUE, END.VALUE)
	END.VALUE = 1
END
RETURN

INI:
    FN.EARLY.RELEASE = 'F.EB.SLV.CUS.EARLY.RELEASE'
    F.EARLY.RELEASE = ''

    FN.ACCOUNT = 'F.ACCOUNT'
    F.ACCOUNT = ''

    FN.CUSTOMER = 'F.CUSTOMER'
    F.CUSTOMER = ''

    FN.LCK = 'F.AC.LOCKED.EVENTS'
    F.LCK=''

    FN.GLOBAL.PARAM = 'F.EB.SLV.GLOBAL.PARAM'
    F.GLOBAL.PARAM = ''

    FN.KEYS.PARAM = 'F.EB.SLV.KEYS.PARAMS'
    F.KEYS.PARAM = ''

    FN.FT = 'F.FUNDS.TRANSFER'
    F.FT = ''
    
    Y.STR.INPUTER = AA$R.ARRANGEMENT.ACTIVITY<58>
    Y.ID.TXN = FIELD(Y.STR.INPUTER,'_',9)
   
    Y.TYPE.TXN = SUBSTRINGS(Y.ID.TXN,1,3)
    IF Y.TYPE.TXN NE 'TFS' THEN
     	Y.CONTINUE = 'Y'
    END   
RETURN
    

OPENFILE:
    CALL OPF(FN.LCK,F.LCK)
    CALL OPF(FN.ACCOUNT,F.ACCOUNT)
    CALL OPF(FN.CUSTOMER,F.CUSTOMER)
    CALL OPF(FN.EARLY.RELEASE,F.EARLY.RELEASE)
    CALL OPF(FN.GLOBAL.PARAM,F.GLOBAL.PARAM)
    CALL OPF(FN.KEYS.PARAM,F.KEYS.PARAM)
    CALL OPF(FN.FT,F.FT)
RETURN

PROCESS:
*** <region name = Ruta archivo temporal>
    
    Path = 'LibCompensaLocal.Info.TFS.FT.TT'
    Archive   = Y.ID.TXN:'.txt'
    
*** </region>

;*Se abre el archivo y si no se encuentra mostrar ERROR
;*-----------------------------------------------------
    OPENSEQ Path,Archive TO MyPath ELSE
        ;*Si no existe el archivo texto mostrar Error
        ;*-------------------------------------------
        E = 'EB-SLV.LIB.ARCHIVO'
        CALL ERR
        RETURN
    END
;*Recorriendo el .txt para obtener la data de cada TT en el TFS
;*-------------------------------------------------------------
    LOOP
        READSEQ Line FROM MyPath ELSE EXIT
        Y.COUNT.TT = DCOUNT(Line,'/') - 1
        FOR J = 1 TO Y.COUNT.TT

            ;*Limpiando variables de uso multiple
            ;*-----------------------------------
            Y.CATEGORY.AUTH = 'N'
            Y.SUM.LCK = 0
            Y.OUT = ''
            Y.CUSTOMER = ''

            Y.WORKING.BALANCE = ''
            Y.FORMA.PAGO = ''
            Y.AMT.TXN = ''
            Y.NUMERO.CUENTA= ''
            Y.APLICA.ER = ''
            Y.AMT.DISPO.ER.AFTER = 0
            Y.AMT.DISPO.COMPENSA.UTILIZADO = 0

            ;*Obtener  data del .txt
            ;*-----------------------
            Y.INFO.TT = FIELD(Line,'/',J)
            Y.WORKING.BALANCE = FIELD(Y.INFO.TT,'*',1)
            Y.FORMA.PAGO = FIELD(Y.INFO.TT,'*',2)
            P.OUT.NO.CHQ = Y.FORMA.PAGO
            Y.AMT.TXN = FIELD(Y.INFO.TT,'*',3)
            Y.NUMERO.CUENTA = FIELD(Y.INFO.TT,'*',4)
            Y.APLICA.ER = FIELD(Y.INFO.TT,'*',5)
            Y.NUMERO.CHEQUE = FIELD(Y.INFO.TT,'*',6)
            Y.MONTO.LIOF = FIELD(Y.INFO.TT,'*',7)
            Y.AMT.BLOQUEADO = FIELD(Y.INFO.TT,'*',8)
            
;*Validacion que se encarga de consultar, si la cuenta definida como debito en la transaccion es de tipo ANY/CUSTOMER segun la parametria
;*en la TELLER.TRANSACTION.          
            CALL SLV.V.INITIATION.TXN(Y.FORMA.PAGO, OUT.INITIATION)               
            IF OUT.INITIATION NE 'ANY' THEN
            	IF OUT.INITIATION NE 'CUSTOMER' THEN
            		CONTINUE
            	END	
            END 
            
            PathAfter = 'LibCompensaLocal.Saldos'
            ArchiveAfter = Y.ID.TXN:'.':Y.NUMERO.CUENTA:'.txt'

*** <region name = Info de la Cuenta>
            ;*Consultanto data de la cuenta
            ;*----------------------------
            CALL F.READ(FN.ACCOUNT,Y.NUMERO.CUENTA,RECORD.ACCOUNT,F.ACCOUNT,ERROR.ACC)
            Y.CUSTOMER = RECORD.ACCOUNT<AC.CUSTOMER>
            Y.CATEGORY.ACC = RECORD.ACCOUNT<AC.CATEGORY>
*** </region>

*** <region name = Posee Cupo Liberacion>
            ;*Select Registro Activo en el catalogo para liberacion de fondos
            ;*----------------------------------------------------------------
            STMT.CON.LIBERACION = "SELECT ":FN.EARLY.RELEASE:" WITH CUPO.ACCOUNT EQ '":Y.NUMERO.CUENTA:"' AND CUPO.STATUS EQ 'Activo' AND RECORD.STATUS NE INAO"
            CALL EB.READLIST (STMT.CON.LIBERACION, KEY.LIST.POSEE.LIBERACION,'', NO.RECORD.POSEE.LIBERACION, ERROR.POSEE.LIBERACION)

            CALL F.READ(FN.EARLY.RELEASE, KEY.LIST.POSEE.LIBERACION, RECORD.OBTENER.LIBERACION, F.EARLY.RELEASE, ERR.OBTENER.LIBERACION)
            
            IF RECORD.OBTENER.LIBERACION THEN
                Y.POSEE.LIBERACION = 'Y'
                Y.MONTO.DISPONIBLE.ER = RECORD.OBTENER.LIBERACION<EB.SLV43.CUPO.AMT.DISPO>
        		Y.ID.ER = KEY.LIST.POSEE.LIBERACION
            END
            ELSE
            	Y.POSEE.LIBERACION = 'N'
            	Y.MONTO.DISPONIBLE.ER = 0
        		Y.ID.ER = 0
        	END
*** </region>
;* Lectura del archivo .txt saldos para obtener como va quedando el WORKING.BALANCE Temporal
;*------------------------------------------------------------------------------------------
        	OPENSEQ PathAfter,ArchiveAfter TO MyPathAfter THEN
            	READSEQ LineAfter FROM MyPathAfter THEN END
        		Y.MONTO.CUENTA = FIELD(LineAfter,'*',1)
    		END
    		ELSE
    			Y.MONTO.CUENTA  = Y.WORKING.BALANCE ;*- Y.SUM.LCK
    		END
    		
;*Reversa de bloqueo cuando la txn sea pago de DepChPropio/PagoChPropioTrc/PagoChPropio
;*-------------------------------------------------------------------------------------
    		IF Y.FORMA.PAGO EQ 'DepChPropio' OR Y.FORMA.PAGO EQ 'PagoChPropioTrc' OR Y.FORMA.PAGO EQ 'PagoChPropio' THEN

        		SMT = "SELECT ": FN.LCK : " WITH ACCOUNT.NUMBER EQ '":Y.NUMERO.CUENTA:"' AND LF.CHQ EQ '":Y.NUMERO.CHEQUE:"' AND LF.AMOUNT EQ '":Y.AMT.TXN:"'"
        		CALL EB.READLIST (SMT,LIST.LCK,NAME.LCK, SELECTED.LCK, ERR.LCK)

        		IF SELECTED.LCK GT 0 THEN
            		CALL F.READ(FN.LCK,LIST.LCK<1>,RECORD.LCK,F.LCK,ERROR.LCK)
            		Y.LCK.RESERVADOR = RECORD.LCK<AC.LCK.LOCKED.AMOUNT>
            		;*OFS
            		;*---
            		CALL SLV.I.OFS.REVERSE.LCK.ER(LIST.LCK,0)
            		Y.OUT = Y.LCK.RESERVADOR
            		CALL AA.GET.WORKING.BALANCE.AMOUNT(PROPERTY.ID, START.DATE, END.DATE, CURRENT.DATE, BALANCE.TYPE, ACTIVITY.IDS,CURRENT.VALUE, START.VALUE, END.VALUE)
            		END.VALUE = Y.OUT
        		END
        		ELSE
        			GOSUB VALIDACION
    			END
    		END
    		ELSE
    			GOSUB VALIDACION
    		END

    	NEXT J
    	REPEAT

;*Borrando los txt temporales creados
;*------------------------------------
    GOSUB DELETE.FILE.SALDO.TEMPORAL
    ;*GOSUB DELETE.FILE.INFO.TXT
    
    Y.BANDERA.CONTROL = 2
    GOSUB RUN.OFS.BLOQUEO.FONDOS
RETURN

VALIDACION:
    IF Y.APLICA.ER EQ 'N' THEN
        Y.OUT = Y.MONTO.CUENTA - Y.AMT.TXN
        GOSUB WRITE.SALDO.TEMPORAL
        ;*IF Y.OUT GE 0 THEN
            FINDSTR 'USD' IN Y.NUMERO.CUENTA SETTING Ap,Vp ELSE
            	IF Y.OUT LT 0 THEN
            		ETEXT = 'EB-SLV.AMT.ER': FM : Y.NUMERO.CUENTA
	        		AF = TFS.OVERRIDE
	        	  	AV = 1
	        		CALL STORE.END.ERROR

                	END.VALUE = Y.OUT
                	CALL AA.GET.WORKING.BALANCE.AMOUNT(PROPERTY.ID, START.DATE, END.DATE, CURRENT.DATE, BALANCE.TYPE, ACTIVITY.IDS,CURRENT.VALUE, START.VALUE, END.VALUE)
                	END.VALUE = Y.OUT
                END	
            END
        ;*END
    END
    ELSE ;*valida si la cuenta tiene categoria para aplicar liberacion de fondos en compensa local
    	CALL F.READ(FN.KEYS.PARAM, 'SLV.LIST.ACC.ER', RECORD.KEYS.PARAM, F.KEYS.PARAM, ERR.KEYS.PARAM)
    	TIPO.ACC = RECORD.KEYS.PARAM<EB.SLV18.VALOR>
    	FINDSTR Y.CATEGORY.ACC IN TIPO.ACC SETTING Ap, Vp THEN
        	Y.CATEGORY.AUTH = 'Y'
        	IF Y.POSEE.LIBERACION EQ 'N' THEN
            	FINDSTR 'USD' IN Y.NUMERO.CUENTA SETTING Ap,Vp ELSE
               		Y.OUT = Y.MONTO.CUENTA - Y.AMT.TXN
               	 	GOSUB WRITE.SALDO.TEMPORAL
               	 	IF Y.OUT LT 0 THEN
            			ETEXT = 'EB-SLV.AMT.ER': FM : Y.NUMERO.CUENTA
	        			AF = TFS.OVERRIDE
	        	  		AV = 1
	        			CALL STORE.END.ERROR
            	        
                		END.VALUE = Y.OUT
                		CALL AA.GET.WORKING.BALANCE.AMOUNT(PROPERTY.ID, START.DATE, END.DATE, CURRENT.DATE, BALANCE.TYPE, ACTIVITY.IDS,CURRENT.VALUE, START.VALUE, END.VALUE)
                		END.VALUE = Y.OUT
                	END	
            	END
        	END
        	ELSE
        		;*Generando el ID de la AppLocal EARLY.RELEASE.DETAIL
        		;*---------------------------------------------------
        		Y.HORA = TIME()
        		Y.TIME = SUBSTRINGS(Y.HORA,1,3)
        		ID.ER.DETAIL =  Y.NUMERO.CUENTA :'.':TODAY:'.':Y.TIME

        		GOSUB LOGICA.LIBERACION
    		END
        END
    END
RETURN

ARR.OFS.BLOQUEO.FONDOS:
    FIND Y.NUMERO.CUENTA IN ARR.OFS.BLOQUEO.ACC SETTING Ap,Vp THEN
    	ARR.OFS.BLOQUEO.ACC<Ap,2> = Y.OUT;*RR.OFS.BLOQUEO.ACC<Ap,2> - Y.AMT.TXN
    END
    ELSE
	    STR.MOV = ''
	    STR.MOV := Y.NUMERO.CUENTA :VM
	    STR.MOV := Y.OUT           :VM
	    STR.MOV := ID.ER.DETAIL    :VM
	    ARR.OFS.BLOQUEO.ACC<-1> = STR.MOV
    END

RETURN

RUN.OFS.BLOQUEO.FONDOS:
    Y.COUNT.ARR.OFS = DCOUNT(ARR.OFS.BLOQUEO.ACC,FM)
    FOR B = 1 TO Y.COUNT.ARR.OFS
        IF ARR.OFS.BLOQUEO.ACC<B,2> GT 0 THEN
            CALL SLV.I.OFS.LOCKED.FUNDS.ER(ARR.OFS.BLOQUEO.ACC<B,1>, ARR.OFS.BLOQUEO.ACC<B,2>, ARR.OFS.BLOQUEO.ACC<B,3>, P.AMT.LIBERAR,Y.ID.ER,0,Y.OUT.LCK,P.OUT.NO.CHQ)
        END
    NEXT B
RETURN

LOGICA.LIBERACION:
;*Buscando si existe el .txt Temporal para consultar como quedara el W.B despues de cada TXN
;*------------------------------------------------------------------------------------------
    OPENSEQ PathAfter,ArchiveAfter TO MyPathAfter THEN
        ;* Si el Archivo existe, se Recorre los valores temporales que posee
        ;*---------------------------------------------------------------------
        LOOP
            READSEQ LineAfter FROM MyPathAfter ELSE EXIT

            ;*Obteniendo los saldos temporales
            ;*--------------------------------
            Y.WORKING.BALANCE = FIELD(LineAfter,'*',1)
            Y.MONTO.DISPONIBLE.ER = FIELD(LineAfter,'*',2)
            ID.ER.DETAIL = FIELD(LineAfter,'*',3)
            Y.AMT.COMPENSA.DISPO = FIELD(LineAfter,'*',4)

            ;*Eliminando .txt Temporal
            ;*------------------------
            GOSUB DELETE.FILE.SALDO

            ;* Se calcula el monto disponible en cuenta
            ;*-----------------------------------------
            Y.MONTO.CUENTA  = Y.WORKING.BALANCE

            ;*Validar que la cuenta tenga fondos suficientes
            ;*----------------------------------------------
            IF Y.MONTO.CUENTA LT Y.AMT.TXN THEN
                ;*-------------------------------
                ;*REALIZANDO LIBERACION DE FONDOS
                ;*-------------------------------
                CALL SLV.I.EARLY.RELEASE.START(Y.NUMERO.CUENTA,Y.AMT.TXN, Y.CUSTOMER, Y.ID.TXN, Y.MONTO.CUENTA,ID.ER.DETAIL,P.MONTO.BLOQUEADO,   P.AMT.LIBERAR, Y.MONTO.LIOF, Y.AMT.COMPENSA.DISPO,Y.OUT,P.OUT.NO.CHQ,P.OUT.ARR.CHQ.UTILIZADOS,P.OUT.ARR.LCK.UTILIZADOS)
                Y.MONTO.DISPONIBLE.ER = Y.MONTO.DISPONIBLE.ER - P.AMT.LIBERAR
                Y.AMT.DISPO.COMPENSA.UTILIZADO = Y.OUT
                P.OUT.ARR.CHQ.UTILIZADOS = P.OUT.ARR.CHQ.UTILIZADOS
                P.OUT.ARR.LCK.UTILIZADOS = P.OUT.ARR.LCK.UTILIZADOS

                GOSUB ARR.OFS.BLOQUEO.FONDOS
                
                ;*Sobreescribir .txt saldos Temporales
                ;*-----------------------------------------
                GOSUB WRITE.SALDO.TEMPORAL
                
                CALL AA.GET.WORKING.BALANCE.AMOUNT(PROPERTY.ID, START.DATE, END.DATE, CURRENT.DATE, BALANCE.TYPE, ACTIVITY.IDS,CURRENT.VALUE, START.VALUE, END.VALUE)
                END.VALUE = Y.OUT
            END
            ELSE IF Y.APLICA.ER EQ 'Y' THEN
            	CALL SLV.I.EARLY.RELEASE.START(Y.NUMERO.CUENTA,Y.AMT.TXN, Y.CUSTOMER, Y.ID.TXN, Y.MONTO.CUENTA,ID.ER.DETAIL,P.MONTO.BLOQUEADO,   P.AMT.LIBERAR, Y.MONTO.LIOF, Y.AMT.COMPENSA.DISPO,Y.OUT,P.OUT.NO.CHQ,P.OUT.ARR.CHQ.UTILIZADOS,P.OUT.ARR.LCK.UTILIZADOS)
            	Y.MONTO.DISPONIBLE.ER = Y.MONTO.DISPONIBLE.ER - P.AMT.LIBERAR
            	Y.AMT.DISPO.COMPENSA.UTILIZADO = Y.OUT
            	P.OUT.ARR.CHQ.UTILIZADOS = P.OUT.ARR.CHQ.UTILIZADOS
            	P.OUT.ARR.LCK.UTILIZADOS = P.OUT.ARR.LCK.UTILIZADOS

            	GOSUB WRITE.SALDO.TEMPORAL
            	GOSUB ARR.OFS.BLOQUEO.FONDOS
            	CALL AA.GET.WORKING.BALANCE.AMOUNT(PROPERTY.ID, START.DATE, END.DATE, CURRENT.DATE, BALANCE.TYPE, ACTIVITY.IDS,CURRENT.VALUE, START.VALUE, END.VALUE)
            	END.VALUE = Y.OUT
        	END
        	ELSE
        		Y.OUT = Y.MONTO.CUENTA - Y.AMT.TXN
        		GOSUB WRITE.SALDO.TEMPORAL
        		
        		CALL AA.GET.WORKING.BALANCE.AMOUNT(PROPERTY.ID, START.DATE, END.DATE, CURRENT.DATE, BALANCE.TYPE, ACTIVITY.IDS,CURRENT.VALUE, START.VALUE, END.VALUE)
        		END.VALUE = Y.OUT
    		END
    REPEAT
    END ;*---Termina el OPENSEG
    ELSE ;* si el .txt no Existe

;*Validar que la cuenta tenga fondos suficientes
;*----------------------------------------------
    	IF Y.MONTO.CUENTA LT Y.AMT.TXN THEN
        ;*-------------------------------
        ;*REALIZANDO LIBERACION DE FONDOS
        ;*-------------------------------
        	CALL SLV.I.EARLY.RELEASE.START(Y.NUMERO.CUENTA,Y.AMT.TXN, Y.CUSTOMER, Y.ID.TXN, Y.MONTO.CUENTA,ID.ER.DETAIL,P.MONTO.BLOQUEADO,   P.AMT.LIBERAR, Y.MONTO.LIOF, Y.AMT.COMPENSA.DISPO,Y.OUT,P.OUT.NO.CHQ,P.OUT.ARR.CHQ.UTILIZADOS,P.OUT.ARR.LCK.UTILIZADOS)
        	Y.MONTO.DISPONIBLE.ER = Y.MONTO.DISPONIBLE.ER - P.AMT.LIBERAR
        	Y.AMT.DISPO.COMPENSA.UTILIZADO = Y.OUT
        	P.OUT.ARR.CHQ.UTILIZADOS = P.OUT.ARR.CHQ.UTILIZADOS
        	P.OUT.ARR.LCK.UTILIZADOS = P.OUT.ARR.LCK.UTILIZADOS
        	GOSUB ARR.OFS.BLOQUEO.FONDOS
        	;*Creando el archivo .txt Temporal
        	;*--------------------------------
        	GOSUB WRITE.SALDO.TEMPORAL
        	;*--------------------------------
        	;*Fin Creando el archivo .txt Temporal
        	CALL AA.GET.WORKING.BALANCE.AMOUNT(PROPERTY.ID, START.DATE, END.DATE, CURRENT.DATE, BALANCE.TYPE, ACTIVITY.IDS,CURRENT.VALUE, START.VALUE, END.VALUE)
        	END.VALUE = Y.OUT
    	END
    	ELSE
    		Y.OUT = Y.MONTO.CUENTA - Y.AMT.TXN
    		GOSUB WRITE.SALDO.TEMPORAL
    		
    		CALL AA.GET.WORKING.BALANCE.AMOUNT(PROPERTY.ID, START.DATE, END.DATE, CURRENT.DATE, BALANCE.TYPE, ACTIVITY.IDS,CURRENT.VALUE, START.VALUE, END.VALUE)
    		END.VALUE = Y.OUT
    	END
    END
RETURN

WRITE.SALDO.TEMPORAL:
;*Sobreescribir .txt saldos Temporales
;*------------------------------------
	CALL SLV.V.INITIATION.TXN(Y.FORMA.PAGO, OUT.INITIATION)
	
	IF (OUT.INITIATION EQ 'ANY') OR (OUT.INITIATION EQ 'CUSTOMER') THEN
		    PathSaldoTemp= 'LibCompensaLocal.Saldos'
		    FileTemp   = Y.ID.TXN:'.':Y.NUMERO.CUENTA:'.txt'
		    
		    Y.AMT.DISPO.ER.AFTER = Y.MONTO.DISPONIBLE.ER - P.AMT.LIBERAR
		    SALDO.TEMPORAL = Y.OUT:'*':Y.AMT.DISPO.ER.AFTER:'*':ID.ER.DETAIL:'*':Y.AMT.DISPO.COMPENSA.UTILIZADO
		    FIND FileTemp IN ARR.TXT.TEMP SETTING Ap,Vp THEN
		    	DELETESEQ PathSaldoTemp,FileTemp SETTING Seq.Saldo.Temp
		    	CLOSESEQ Seq.Saldo.Temp
		    END
		    ELSE
		    	STR.MOV = ''
		    	STR.MOV := FileTemp
		    	ARR.TXT.TEMP<-1> = STR.MOV
		    END
		   
		    OPENSEQ PathSaldoTemp,FileTemp TO Seq.Saldo.Temp
		    	WRITESEQ SALDO.TEMPORAL APPEND TO Seq.Saldo.Temp THEN
		    END
		    CLOSESEQ Seq.Saldo.Temp
	END
;*-----------------------------------------
;*Fin Sobreescribir .txt saldos temporales
RETURN

DELETE.FILE.SALDO:
    PathSaldoTemp= 'LibCompensaLocal.Saldos'
    FileTemp   = Y.ID.TXN:'.':Y.NUMERO.CUENTA:'.txt'
    DELETESEQ PathSaldoTemp,FileTemp SETTING Seq.Saldo.Temp
    CLOSESEQ Seq.Saldo.Temp
RETURN

DELETE.FILE.SALDO.TEMPORAL:
    PathSaldoTemp= 'LibCompensaLocal.Saldos'
    Y.ARR.SALDO.TEMPORAL = DCOUNT(ARR.TXT.TEMP,FM)
    FOR H = 1 TO Y.ARR.SALDO.TEMPORAL
        DELETESEQ PathSaldoTemp, ARR.TXT.TEMP<H> SETTING Seq.Saldo.Temp
        CLOSESEQ Seq.Saldo.Temp
    NEXT H
RETURN

DELETE.FILE.INFO.TXT:
    Path = 'LibCompensaLocal.Info.TFS.FT.TT'
    Archive   = Y.ID.TXN:'.txt'
    DELETESEQ Path,Archive SETTING Seq.Saldo.Temp
    CLOSESEQ Seq.Saldo.Temp
RETURN
END
