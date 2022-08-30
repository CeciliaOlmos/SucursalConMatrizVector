      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EJERCICIO_SALA2.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION. SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
      *****************************************************************
      ********************** LINEAS DE IMPRESION **********************
      *****************************************************************
        01 lin-titulo.
           03 filler pic x(30) value spaces.
           03 filler pic x(19) value "Banco El Corralito".
           03 filler pic x(31) value spaces.

       01  lin-sub-titulo.
           03 filler pic x(11) value spaces.
           03 filler pic x(54) value "Totales de Transferencias de Caja
      -    " de Ahorro del anio ".
           03 lin-fecha.
               05 filler pic 99 value 20.
               05 lin-anio   pic 99.
           03 filler pic x(11) value spaces.

       01  lin-guion.
           03 filler pic x(80) value all "-".

       01  lin-titulo-mes.
           03 filler pic x(29) value space.
           03 filler pic x(7) value "MES DE".
           03 lin-mes pic x(11).

       01  lin-titulo-cabecera.
           03 filler pic x(08) value space.
           03 lin-tit-cod-suc pic x(8) value "COD. SUC".
           03 filler pic x(8) value space.
           03 lin-tit-nom-suc pic x(6) value "NOMBRE".
           03 filler pic x(10) value space.
           03 lin-tit-tot-trans pic x(20) value "TOTAL TRANSFERENCIAS".

       01  lin-datos-suc.
           03  filler pic x(10) value space.
           03 lin-cod-suc pic x(3).
           03  filler pic x(11) value space.
           03 lin-nom-suc pic x(11).
           03  filler pic x(6) value space.
           03 lin-tot-trans pic zzz.zzz.zz9,99.

       01  lin-tot-mes.
           03 filler pic x(13) value "Total mes de".
           03 lin-nom-mes pic x(10).
           03 filler pic x value ":".
           03 lin-total pic z.zzz.zzz.zz9,99.

       01  lin-tot-anual pic x(16) value "Totales Anuales:".

       01  lin-tot-monto.
           03 filler pic x(28) value "Monto Transferido en total:".
           03 lin-total-monto pic zzz.zzz.zzz.zz9,99.

       01  lin-mes-monto.
           03 filler pic x(32) value "Mes de mayor monto transferido:".
           03 lin-max-nom-mes pic x(10).

       01  lin-sucursal.
           03 filler pic x(40) value "Sucursal que mas transfirio en el
      -     "anio:".
           03 lin-max-suc.
               05 lin-max-cod-suc pic xxx.
               05 filler pic xxx value " - ".
               05 lin-max-nom-suc pic x(11).

      *****************************************************************
      ********************** VARIABLES DE TRABAJO *********************
      *****************************************************************
       01  fecha.
           03 anio pic 99.
           03 mes pic 99.
           03 dia pic 99.

       01  posI pic 99.
       01  posJ pic 99.
       01  acum-importe pic 9(10)v99.
       01  acum-importe-total pic 9(12)v99.
       01  pos-max-mes pic 99.
       01  max-importe-mes pic s9(10)v99.
       01  pos-max-sucursal pic 9.
       01  max-importe-sucursal pic s9(10)v99.
       01  codigo-sucursal pic x(19).
       01  cont pic 99 value 1.

       01  meses.
           03 filler pic x(10) value "ENERO".
           03 filler pic x(10) value "FEBRERO".
           03 filler pic x(10) value "MARZO".
           03 filler pic x(10) value "ABRIL".
           03 filler pic x(10) value "MAYO".
           03 filler pic x(10) value "JUNIO".
           03 filler pic x(10) value "JULIO".
           03 filler pic x(10) value "AGOSTO".
           03 filler pic x(10) value "SEPTIEMBRE".
           03 filler pic x(10) value "OCTUBRE".
           03 filler pic x(10) value "NOVIEMBRE".
           03 filler pic x(10) value "DICIEMBRE".
       01  tab-meses redefines meses.
           03  vec-mes pic x(10) occurs 12 times.
       01  tabla.
           03 vec-meses-cont occurs 12 times.
               05 vec-sucursales-cont occurs 5 times.
                   07 mat-importe pic 9(9)v99.
       01  tab-tot-suc.
           03 vec-tot-suc  pic 9(10)v99 occurs 5 times.
      *****************************************************************
      ********************** CONSTANTES QUE ME DAN ********************
      *****************************************************************
       01  sucursales.
           03 filler pic x(14) value "PalPalermo".
           03 filler pic x(14) value "RecRecoleta".
           03 filler pic x(14) value "MicMicrocentro".
           03 filler pic x(14) value "LinLiniers".
           03 filler pic x(14) value "MatMataderos".
       01  vec-sucursal redefines sucursales occurs 5 times.
           03 cod-sucursal pic xxx.
           03 nom-sucursal pic x(11).
      *****************************************************************
      ********************** VARIABLES DE INGRESO *********************
      *****************************************************************
       01  ingresos.
           03 ing-socio    pic s9(6).
           03 ing-sucursal pic x(3).
           03 ing-fecha.
               05 ing-dia  pic 99.
               05 ing-mes  pic 99.
               05 ing-anio pic 99.
           03 ing-importe  pic s9(7)v99.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
      *****************************************************************
      ********************* PROGRAMA PRINCIPAL ************************
      *****************************************************************
           PERFORM 100-INICIO-GENERAL.
           PERFORM 200-INGRESO-SOCIO.
           PERFORM UNTIL ing-socio = 0
               PERFORM 210-INGRESO-RESTO-DATOS
               PERFORM 300-PROCESO-DATOS
               PERFORM 200-INGRESO-SOCIO
           END-PERFORM.
           PERFORM 400-FIN-GENERAL.
            STOP RUN.
      *****************************************************************
      *************************** RUTINAS *****************************
      *****************************************************************
       100-INICIO-GENERAL.

           PERFORM 130-INICIALIZO-MATRIZ-EN-CERO.
           PERFORM 140-INICIALIZO-VECTOR-TOT-SUC.
           PERFORM 150-OBTENGO-FECHA-ACTUAL.

       120-INICIALIZO-VARIABLES.
           MOVE ZERO TO acum-importe-total.
           MOVE ZERO TO pos-max-mes.
           MOVE ZERO TO max-importe-mes.
           MOVE ZERO TO pos-max-sucursal.
           MOVE ZERO TO max-importe-sucursal.
           PERFORM 125-ARMO-CODIGO-SUCURSAL.

       125-ARMO-CODIGO-SUCURSAL.
           PERFORM VARYING posJ FROM 1 BY 1 UNTIL posJ > 5
               STRING cod-sucursal(posJ) DELIMITED BY SIZE
                   '-' DELIMITED BY SIZE
                   INTO codigo-sucursal WITH POINTER cont
               END-STRING
           END-PERFORM.

       130-INICIALIZO-MATRIZ-EN-CERO.
           PERFORM VARYING posI FROM 1 BY 1 UNTIL posI > 12
               AFTER posJ FROM 1 BY 1 UNTIL posJ > 5
                   MOVE ZERO TO mat-importe(posI,posJ)
           END-PERFORM.

       140-INICIALIZO-VECTOR-TOT-SUC.
           PERFORM VARYING posJ FROM 1 BY 1 UNTIL posJ > 5
               MOVE ZERO TO vec-tot-suc(posJ)
           END-PERFORM.

       150-OBTENGO-FECHA-ACTUAL.
           ACCEPT fecha FROM DATE.

       200-INGRESO-SOCIO.
           PERFORM 205-TRAIGO-CODIGO-SOCIO.
           PERFORM UNTIL ing-socio >= 0
               DISPLAY "Error!!!"
               PERFORM 205-TRAIGO-CODIGO-SOCIO
           END-PERFORM.

       205-TRAIGO-CODIGO-SOCIO.
           DISPLAY "Ingrese codigo del socio".
           ACCEPT ing-socio.

       210-INGRESO-RESTO-DATOS.
           PERFORM 230-INGRESO-SUCURSAL.
           PERFORM 240-INGRESO-FECHA.
           PERFORM 250-INGRESO-IMPORTE.

       230-INGRESO-SUCURSAL.
           PERFORM 235-TRAIGO-SUCURSAL.
           PERFORM UNTIL posJ < 6
               DISPLAY "Error!!!"
               PERFORM 235-TRAIGO-SUCURSAL
           END-PERFORM.

       235-TRAIGO-SUCURSAL.
           DISPLAY "Ingrese codigo de sucursal (",codigo-sucursal,")".
           ACCEPT ing-sucursal.
           PERFORM 350-BUSCAR-POS-VEC-SUC.

       240-INGRESO-FECHA.
           PERFORM 242-TRAIGO-FECHA.
           PERFORM 245-VALIDAR-FECHA.

       242-TRAIGO-FECHA.
           DISPLAY "Ingrese fecha (DDMMAA)".
           ACCEPT ing-fecha.

       245-VALIDAR-FECHA.
           PERFORM UNTIL ing-dia >0 AND ing-dia < 31
               PERFORM 246-ERROR-FECHA
           END-PERFORM.
           PERFORM UNTIL ing-mes > 0 AND ing-mes < 13
               PERFORM 246-ERROR-FECHA
           END-PERFORM.

       246-ERROR-FECHA.
           DISPLAY "Error".
           PERFORM 242-TRAIGO-FECHA.

       250-INGRESO-IMPORTE.
           PERFORM 255-TRAIGO-IMPORTE.
           PERFORM UNTIL ing-importe > 0
               DISPLAY "Error"
               PERFORM 255-TRAIGO-IMPORTE
           END-PERFORM.

       255-TRAIGO-IMPORTE.
           DISPLAY "Ingrese monto:"
           ACCEPT ing-importe.

       300-PROCESO-DATOS.
           IF ing-anio IS EQUALS anio THEN
               PERFORM 350-BUSCAR-POS-VEC-SUC
               ADD ing-importe TO mat-importe(ing-mes,posJ)
               ADD ing-importe to vec-tot-suc(posJ)
           END-IF.

       350-BUSCAR-POS-VEC-SUC.
           PERFORM VARYING posJ FROM 1 BY 1 UNTIL posJ > 5 OR
                             cod-sucursal(posJ) = ing-sucursal
           END-PERFORM.

       400-FIN-GENERAL.
           PERFORM 120-INICIALIZO-VARIABLES.
           PERFORM 410-ENCABEZADO.
           PERFORM 450-PROCESO-MES.
           PERFORM 460-PROCESO-SUCURSAL.
           PERFORM 430-ARMO-LINEAS_TOTALES-ANUALES.
           PERFORM 420-MUESTRO-TOTALES-ANUALES.

       410-ENCABEZADO.
           PERFORM 430-ARMO-LIN-FECHA.
           DISPLAY lin-titulo.
           DISPLAY lin-sub-titulo.
           DISPLAY lin-guion.

       411-MUESTRO-LIN-CABEZERA.
           PERFORM 412-MUESTRO-LIN-TITULO-MES.
           PERFORM 415-MUESTRO-LIN-TITULO-CABEZERA.

       412-MUESTRO-LIN-TITULO-MES.
           PERFORM 438-ARMO-LIN-TITULO-MES.
           DISPLAY lin-titulo-mes.

       415-MUESTRO-LIN-TITULO-CABEZERA.
           DISPLAY lin-titulo-cabecera.

       416-MUESTRO-LIN-DATOS-SUC.
           PERFORM 440-ARMO-DATOS-SUC.
           DISPLAY lin-datos-suc.

       417-MUESTRO-LIN-TOT-MES.
           PERFORM 439-ARMO-LIN-TOT-MES.
           DISPLAY lin-tot-mes.

       420-MUESTRO-TOTALES-ANUALES.
           DISPLAY lin-tot-anual.
           DISPLAY lin-tot-monto.
           Display lin-mes-monto.
           DISPLAY lin-sucursal.

       430-ARMO-LINEAS_TOTALES-ANUALES.
           PERFORM 435-ARMO-LIN-TOT-MONTO.
           PERFORM 436-ARMO-LIN-MES-MONTO.
           PERFORM 437-ARMO-LIN-SUCURSAL.

       430-ARMO-LIN-FECHA.
           MOVE anio TO lin-anio.

       435-ARMO-LIN-TOT-MONTO.
           MOVE acum-importe TO lin-total-monto.

       436-ARMO-LIN-MES-MONTO.
           MOVE vec-mes(pos-max-mes) TO lin-max-nom-mes.

       437-ARMO-LIN-SUCURSAL.
           MOVE cod-sucursal(pos-max-sucursal) TO lin-max-cod-suc.
           MOVE nom-sucursal(pos-max-sucursal) TO lin-max-nom-suc.

       438-ARMO-LIN-TITULO-MES.
           MOVE vec-mes(posI) TO lin-mes.

       439-ARMO-LIN-TOT-MES.
           MOVE vec-mes(posI) TO lin-nom-mes.
           MOVE acum-importe TO lin-total.

       440-ARMO-DATOS-SUC.
           MOVE cod-sucursal(posJ) TO lin-cod-suc.
           MOVE nom-sucursal(posJ) TO lin-nom-suc.
           MOVE mat-importe(posI,posJ) TO lin-tot-trans.

       445-BUSCO-MAXIMO-MES.
           IF acum-importe > max-importe-mes THEN
               MOVE acum-importe TO max-importe-mes
               MOVE posI TO pos-max-mes
           END-IF.

       450-PROCESO-MES.
           PERFORM VARYING posI FROM 1 BY 1 UNTIL posI > 12
               MOVE ZERO TO acum-importe
               PERFORM 411-MUESTRO-LIN-CABEZERA
               PERFORM VARYING posJ FROM 1 BY 1 UNTIL posJ > 5
                   ADD mat-importe(posI,posJ) TO acum-importe
                   PERFORM 416-MUESTRO-LIN-DATOS-SUC
               END-PERFORM
               PERFORM 417-MUESTRO-LIN-TOT-MES
               PERFORM 445-BUSCO-MAXIMO-MES
           END-PERFORM.

       455-BUSCO-MAXIMO-SUCURSAL.
           IF vec-tot-suc(posJ) > max-importe-sucursal THEN
               MOVE vec-tot-suc(posJ) TO max-importe-sucursal
               MOVE posJ TO pos-max-sucursal
           END-IF.

       460-PROCESO-SUCURSAL.
           MOVE ZERO TO acum-importe
           PERFORM VARYING posJ FROM 1 BY 1 UNTIL posJ > 5
               ADD vec-tot-suc(posJ) TO acum-importe
               PERFORM 455-BUSCO-MAXIMO-SUCURSAL
           END-PERFORM.

       END PROGRAM EJERCICIO_SALA2.
