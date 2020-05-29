*-----------------------------------------------------------------------------
* <Rating>4092</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.UTIL.IMP.GET.NUMLETRAS(AMOUNT.LOCAL.1)
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
*-----------------------------------------------------------------------------
UNIDADES.ARR<-1> = "UNO ,DOS ,TRES ,CUATRO ,CINCO ,SEIS ,SIETE ,OCHO ,NUEVE ,DIEZ ,ONCE ,DOCE ,TRECE ,CATORCE ,QUINCE ,DIECISEIS ,DIECISIETE ,DIECIOCHO ,DIECINUEVE ,VEINTE " 
DECENAS.ARR<-1> = " ,VEINTI,TREINTA ,CUARENTA ,CINCUENTA ,SESENTA ,SETENTA ,OCHENTA ,NOVENTA ,CIEN"
CENTENAS.ARR<-1> = "CIENTO ,DOSCIENTOS ,TRESCIENTOS ,CUATROCIENTOS ,QUINIENTOS ,SEISCIENTOS ,SETECIENTOS ,OCHOCIENTOS ,NOVECIENTOS " 
UNIDADES.N.ARR<-1> = "UN ,DOS ,TRES ,CUATRO ,CINCO ,SEIS ,SIETE ,OCHO ,NUEVE ,DIEZ ,ONCE ,DOCE ,TRECE ,CATORCE ,QUINCE ,DIECISEIS ,DIECISIETE ,DIECIOCHO ,DIECINUEVE ,VEINTE " 


RESULTADO = ''
MONTO.MAXIMO=999999999
PUNTO.DECIMAL= 0
LONGITUD = 0
ENTEROS = 0
DECIMALES = 0
*Eliminando caracteres diferentes a numeros
*Formato Legal
AMOUNT.LOCAL.1 = EREPLACE(AMOUNT.LOCAL.1,"-","")
AMOUNT.LOCAL.1 = EREPLACE(AMOUNT.LOCAL.1,",","")

*CRT 'CANTIDAD RECIBIDAD =':AMOUNT.LOCAL.1

PUNTO.DECIMAL = COUNT (AMOUNT.LOCAL.1,".") 

IF PUNTO.DECIMAL = 1 THEN

		ENTEROS = FIELD (AMOUNT.LOCAL.1,'.',1)
		
		DECIMALES = FIELD (AMOUNT.LOCAL.1,'.',2)
		
		DECIMALES = DECIMALES[1,2]

END
ELSE

ENTEROS = AMOUNT.LOCAL.1
DECIMALES= 0
END




*CRT 'CANTIDAD DE PUNTOS DECIMALES=':PUNTO.DECIMAL
*CRT 'CANTIDAD DE DIGITOS ENTEROS=':LONGITUD


*NUMERO PERMITIDO

*CRT ENTEROS
*CRT DECIMALES

IF ENTEROS <= MONTO.MAXIMO THEN
		IF ENTEROS >= 0  THEN
			;* INICIA CONVERSION DEL NUMERO
			
			;* 9 DIGITOS
			
			IF LEN(ENTEROS) = 9  THEN
			
					IF ENTEROS[1,3] = 100 THEN
					RESULTADO = RESULTADO :' CIEN '
					END
					
					ELSE
							
							RESULTADO = RESULTADO: FIELD(CENTENAS.ARR,',',ENTEROS[1,1])
				
							
							IF ENTEROS[2,2] <= 20 AND ENTEROS[2,2] > 0  THEN
							RESULTADO = RESULTADO: FIELD(UNIDADES.N.ARR,',',ENTEROS[2,2])
							
							END
							
							ELSE
							
								IF ENTEROS[2,2] > 20 AND ENTEROS[2,2] < 30  THEN
								RESULTADO = RESULTADO:FIELD(DECENAS.ARR,',',ENTEROS[2,1])
								RESULTADO = RESULTADO:FIELD(UNIDADES.N.ARR,',',ENTEROS[3,1])								
								END
								
								ELSE
									IF ENTEROS[2,2] > 0 THEN
									RESULTADO = RESULTADO:FIELD(DECENAS.ARR,',',ENTEROS[2,1])
										IF ENTEROS[3,1] > 0 THEN
										
										RESULTADO = RESULTADO:'Y ':FIELD(UNIDADES.N.ARR,',',ENTEROS[3,1])
									    END
									END
								END
							END
							
							

					END
					
					RESULTADO = RESULTADO: 'MILLONES '
	
					ENTEROS = ENTEROS[4,6]	
				
			 END
			
			;* 9 DIGITOS
					
			;* 8 DIGITOS
			
			IF LEN(ENTEROS) = 8 THEN
			
  							IF ENTEROS[1,2] <= 20 AND ENTEROS[1,2] > 0  THEN
							RESULTADO = RESULTADO: FIELD(UNIDADES.N.ARR,',',ENTEROS[1,2])
						
							END
							ELSE
								
								IF ENTEROS[1,2] > 20 AND ENTEROS[1,2] < 30  THEN
								RESULTADO = RESULTADO:FIELD(DECENAS.ARR,',',ENTEROS[1,1])
								RESULTADO = RESULTADO:FIELD(UNIDADES.N.ARR,',',ENTEROS[2,1])
								
								END
								ELSE
								RESULTADO = RESULTADO:FIELD(DECENAS.ARR,',',ENTEROS[1,1])
								IF ENTEROS[2,1] > 0 THEN
							
								RESULTADO = RESULTADO:'Y ':FIELD(UNIDADES.N.ARR,',',ENTEROS[2,1])
								END
								END
							END
							
						    RESULTADO = RESULTADO: 'MILLONES '
						    ENTEROS = ENTEROS[3,6]
						   
			
			END
			
			;* 8 DIGITOS
							
			;* 7 DIGITOS
			
			IF LEN(ENTEROS) = 7 THEN
			
				IF ENTEROS[1,1] = 1 THEN
				RESULTADO= RESULTADO:' UN MILLON '
				END

				
				IF ENTEROS[1,1] > 1 THEN
				RESULTADO = RESULTADO:FIELD(UNIDADES.ARR,',',ENTEROS[1,1]):'MILLONES '
				END
				
				ENTEROS = ENTEROS[2,6]
			
			END
			
	
			
			;* 7 DIGITOS
						
			;* 6 DIGITOS
			
			IF LEN(ENTEROS) = 6  AND ENTEROS[1,3] > 0 THEN
			
				
			
					IF ENTEROS[1,3] = 100 THEN
					RESULTADO = RESULTADO : ' CIEN '
					END
					
					ELSE
							
							IF ENTEROS[1,1] > 0 THEN
							RESULTADO = RESULTADO: FIELD(CENTENAS.ARR,',',ENTEROS[1,1])
							END

				
							
							IF ENTEROS[2,2] <= 20 AND ENTEROS[2,2] >= 1  THEN
	
							RESULTADO = RESULTADO: FIELD(UNIDADES.N.ARR,',',ENTEROS[2,2])
							END
							ELSE
								
								IF ENTEROS[2,2] > 20 AND ENTEROS[2,2] < 30  THEN
								RESULTADO = RESULTADO:FIELD(DECENAS.ARR,',',ENTEROS[2,1])
								RESULTADO = RESULTADO:FIELD(UNIDADES.N.ARR,',',ENTEROS[3,1])
								END
								ELSE
									IF ENTEROS[2,2] > 1 THEN
									RESULTADO = RESULTADO:FIELD(DECENAS.ARR,',',ENTEROS[2,1])
									
										IF ENTEROS[3,1] > 0 THEN
									
										RESULTADO = RESULTADO:'Y ':FIELD(UNIDADES.N.ARR,',',ENTEROS[3,1])
										END
									END
								END
							END
							
							

					END
					
					RESULTADO = RESULTADO: 'MIL '	
					ENTEROS = ENTEROS[4,3]	
			
			
			 END
			
			;* 6 DIGITOS			
			
			;* 5 DIGITOS
			
			IF LEN(ENTEROS) = 5 THEN
			
							
			
  							IF ENTEROS[1,2] <= 20 AND ENTEROS[1,2] > 0  THEN
							RESULTADO = RESULTADO: FIELD(UNIDADES.N.ARR,',',ENTEROS[1,2])
							END
							ELSE
							
										IF ENTEROS[1,2] > 20 AND ENTEROS[1,2] < 30  THEN
										RESULTADO = RESULTADO:FIELD(DECENAS.ARR,',',ENTEROS[1,1])
										RESULTADO = RESULTADO:FIELD(UNIDADES.N.ARR,',',ENTEROS[2,1])
										END
									    ELSE
											RESULTADO = RESULTADO:FIELD(DECENAS.ARR,',',ENTEROS[1,1])
											IF ENTEROS[2,1] > 0 THEN
											RESULTADO = RESULTADO:'Y ':FIELD(UNIDADES.N.ARR,',',ENTEROS[2,1])
											END
										END
							END			
							
						    RESULTADO = RESULTADO: 'MIL '
						    ENTEROS = ENTEROS[3,3]	
*						    CRT RESULTADO
			
			END
			
			;* 5 DIGITOS
					
			;* 4 DIGITOS
			
			IF LEN(ENTEROS) = 4 THEN
			
				IF ENTEROS[1,1] = 1 THEN
				RESULTADO= RESULTADO:' MIL '
				END

				
				IF ENTEROS[1,1] > 1 THEN
				RESULTADO = RESULTADO:FIELD(UNIDADES.ARR,',',ENTEROS[1,1]):' MIL '
				END
				
				ENTEROS = ENTEROS[2,3]
				
			
			END
			
			
			
			;* 4 DIGITOS		
			
			;* 3 DIGITOS
			
			IF LEN(ENTEROS) = 3 AND ENTEROS[1,3] > 0 THEN
			
			
					
					IF ENTEROS[1,3] = 100 THEN
					RESULTADO = RESULTADO: ' CIEN '
					END
					
					ELSE
							
							
							IF ENTEROS[1,1] > 0 THEN
							RESULTADO = RESULTADO: FIELD(CENTENAS.ARR,',',ENTEROS[1,1])
							END							
				
							
							IF ENTEROS[2,2] <= 20 AND ENTEROS[2,2] > 0  THEN
							RESULTADO = RESULTADO: FIELD(UNIDADES.ARR,',',ENTEROS[2,2])
							END
							ELSE
								
								
								IF ENTEROS[2,2] > 20 AND ENTEROS[2,2] < 30  THEN
								RESULTADO = RESULTADO:FIELD(DECENAS.ARR,',',ENTEROS[2,1])
								RESULTADO = RESULTADO:FIELD(UNIDADES.ARR,',',ENTEROS[3,1])
								END
								ELSE
						
								IF ENTEROS[2,2] > 0 THEN
								RESULTADO = RESULTADO:FIELD(DECENAS.ARR,',',ENTEROS[2,1])
									IF ENTEROS[3,1] > 0 THEN
									RESULTADO = RESULTADO:'Y ':FIELD(UNIDADES.ARR,',',ENTEROS[3,1])
								    END
								END
								END
								
								
							END
							
							

					END
					
					
					
						
*					CRT RESULTADO
			 END
			
			;* 3 DIGITOS
								
			;* 2 DIGITOS
			IF LEN(ENTEROS) = 2 THEN
			
  							IF ENTEROS[1,2] <= 20 AND ENTEROS[1,2] > 0  THEN
							RESULTADO = RESULTADO: FIELD(UNIDADES.ARR,',',ENTEROS[1,2])
							END
							ELSE
								IF ENTEROS[1,2] > 20 AND ENTEROS[1,2] < 30  THEN
								RESULTADO = RESULTADO:FIELD(DECENAS.ARR,',',ENTEROS[1,1])
								RESULTADO = RESULTADO:FIELD(UNIDADES.ARR,',',ENTEROS[2,1])
								END
								ELSE
								RESULTADO = RESULTADO:FIELD(DECENAS.ARR,',',ENTEROS[1,1])
									IF ENTEROS[2,1] > 0 THEN
									RESULTADO = RESULTADO:'Y ':FIELD(UNIDADES.ARR,',',ENTEROS[2,1])
									END
								END
							END
							
*						    RESULTADO = RESULTADO: 'MIL'
	
*						    CRT RESULTADO
			
			END
			
			;* 2 DIGITOS
						
			;* 1 DIGITO
			IF LEN(ENTEROS) = 1 THEN
			
				IF ENTEROS[1,1] = 1 THEN
				RESULTADO= RESULTADO:'UNO '
				END
				
				IF ENTEROS[1,1] = 0 THEN
				RESULTADO= RESULTADO:'CERO '
				END
				
				IF ENTEROS[1,1] > 1 THEN
				RESULTADO = RESULTADO:FIELD(UNIDADES.ARR,',',ENTEROS[1,1])
				END
				
			
			END
			
			;* 1 DIGITO
			
			
			
			
		
				
				IF DECIMALES >= 1 AND LEN(DECIMALES) = 2 THEN
			    RESULTADO = RESULTADO : 'CON ': DECIMALES : '/100'
			    END
			    
			    ELSE
			    	IF DECIMALES = 0 THEN
			    	RESULTADO = RESULTADO: 'CON 00/100'
			    	END
			    	ELSE
			    	RESULTADO = RESULTADO: 'CON ': DECIMALES : '0/100'
			    	END
			    END
			
*			CRT RESULTADO 
			 AMOUNT.LOCAL.1 = RESULTADO 
		END
		ELSE
			RESULTADO = 'EL NUMERO A CONVERTIR DEBE SER MAYOR A 0' 
		END		
END
ELSE
	RESULTADO = 'NO ES POSIBLE CONVERTIRLO A TEXTO , NUMERO DEBE SER MENOR O IGUAL A 999,999,999.99'
END



END
