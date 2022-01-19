'******************************
'***Zamek Sejfu z Enkoderem ***
'*** Autor: Jakub Bogusz,   ***
'***  Mateusz Bonar,        ***
'***  Sebastian Bysiecki    ***
'*** II/stc/UP/Krakow/2021  ***
'***      SW/Grupa 2        ***
'******************************
$regfile = "m32def.dat"
$crystal = 16000000
$hwstack = 40
$swstack = 16
$framesize = 32
$sim

Dim Li As Byte
Dim Licz As Byte
Dim Miejsce As Byte
Dim Tryb As Byte
Dim Liczba_znakow As Byte
Dim Znaki As String * 8
Dim Haslo As String * 4
Dim Haslo_Obr As String * 8
Dim Zmiana As String * 4
Dim Zmiana_Obr As String * 4
Dim Kier As String * 1

Config Lcd = 16 * 2
Config Lcdpin = Pin , Db4 = Portb.4 , Db5 = Portb.5 , Db6 = Portb.6 , Db7 = Portb.7 , E = Portb.2 , Rs = Portb.3
Cursor Off

Config Porta.0 = Input
Porta.0 = 1
Config Porta.1 = Input
Porta.1 = 1
Config Porta.2 = Input
Porta.2 = 1
Config Portc.0 = Output
Portc.0 = 0
Pina.2 = 1

Miejsce = 6
Zmiana = "0000"
Haslo = "1111"
Zmiana_Obr = "2222"
Liczba_znakow = 4

Deflcdchar 0 , 32 , 32 , 14 , 17 , 31 , 16 , 14 , 2       ' ê
Deflcdchar 1 , 12 , 4 , 6 , 12 , 4 , 4 , 14 , 32  ' ³

Gosub Wyswietl

Do
   Do
     If Liczba_znakow = 8 Then
      Li = Encoder(pina.0 , Pina.1 , Lewo_2 , Prawo_2 , 0)
     Else
      Li = Encoder(pina.0 , Pina.1 , Lewo , Prawo , 0)
     Endif
     Waitus 200

     If Pina.2 = 0 Then
       Gosub Akceptuj
      If Liczba_znakow = 8 Then
       Miejsce = Miejsce + 2
      Else
       Miejsce = Miejsce + 1
      Endif
     End if
   Loop Until Len(znaki) = Liczba_znakow

   Gosub Sprawdzenie
Loop
End

'*******************  Podprogramy  ******************
Lewo:
     If Li = 0 Then
     If Licz > 0 Then Licz = Licz - 1
     Gosub Disp
     End If
Return

Prawo:
      If Li = 0 Then
      If Licz < 9 Then Incr Licz
      Gosub Disp
      End If
Return

Disp:
     Locate 2 , Miejsce
     Lcd Licz
Return

Lewo_2:
     If Li = 0 Then
     If Licz > 0 Then Licz = Licz - 1
     Gosub Disp_Obr
     End If
     Kier = "L"
Return

Prawo_2:
      If Li = 0 Then
      If Licz < 9 Then Incr Licz
      Gosub Disp_Obr
      End If
      Kier = "P"
Return
Disp_Obr:
     Locate 2 , Miejsce
     Lcd Kier
     Locate 2 , Miejsce + 1
     Lcd Licz
Return

Akceptuj:
   'Waitus 500
   Pina.2 = 1
   If Liczba_Znakow = 8 Then
      Znaki = Znaki + Str(licz) + Kier
      Locate 2 , Miejsce
      Lcd "*"
      Locate 2 , Miejsce + 1
      Lcd "*"
   Else
      Znaki = Znaki + Str(licz)
      Locate 2 , Miejsce
      Lcd "*"
   EndIf
Return

Akceptuj_Zmiana:
   'Waitus 500
   Pina.2 = 1
   Haslo = Haslo + Str(licz)
   Locate 2 , Miejsce
   Lcd "*"
Return

Akceptuj_Obr:
   'Waitus 500
   Pina.2 = 1
   Haslo = Haslo + Str(licz) + Kier
   Locate 2 , Miejsce
   Lcd "*"
   Locate 2 , Miejsce + 1
   Lcd "*"
Return

Sprawdzenie:
   If Znaki = Haslo Then
      Gosub Otwarte
   Elseif Znaki = Zmiana Then
      Gosub Nowe_Haslo
   Elseif Znaki = Zmiana_Obr Then
      Gosub Nowe_Haslo_Obr
   Else
      Gosub Zamkniete
   End If

   Gosub Wyswietl

Return

Nowe_Haslo:
   Cls
   Locate 1,1
   Haslo = ""
   Znaki = ""
   Miejsce = 6
   Lcd "Podaj nowe has"; Chr(1); "o:"
   Do
     Li = Encoder(pina.0 , Pina.1 , Lewo , Prawo , 0)
     Waitus 200

     If PINA.2 = 0 Then
      Gosub Akceptuj_Zmiana
      Miejsce = Miejsce + 1
     End if

   Loop Until Len(Haslo) = 4
   Miejsce = 6
Return

Nowe_Haslo_Obr:
   Cls
   Locate 1,1
   Haslo = ""
   Znaki = ""
   Miejsce = 6
   Liczba_znakow = 8
   Lcd "Podaj nowe has"; Chr(1); "o:"
   Do
     Li = Encoder(pina.0 , Pina.1 , Lewo_2 , Prawo_2 , 0)
     Waitus 200

     If PINA.2 = 0 Then
      Gosub Akceptuj_Obr
      Miejsce = Miejsce + 2
     End if

   Loop Until Len(Haslo) = 8
   Miejsce = 6
Return

Wyswietl:
   Cls
   Locate 1,4
   Lcd "Wpisz kod:";
Return

Otwarte:
   Cls
   Locate 1, 5
   Lcd "Otwarte!"
   PortC.0 = 1
   Wait 3
   Portc.0 = 0
   Cls
   Locate 1, 4
   Lcd "Zamkni"; Chr(0); "te!"
   Wait 2
   Znaki = ""
   Miejsce = 6

Zamkniete:
   Cls
   Locate 1,3
   Lcd "Niepoprawny"
   Locate 2, 7
   Lcd "kod!"
   Wait 2
   Znaki = ""
   Miejsce = 6