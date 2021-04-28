LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.std_logic_unsigned.ALL;
USE ieee.std_logic_misc.ALL;

ENTITY czekanie IS
    GENERIC (
        F_ZEGARA : NATURAL := 20_000_000; -- czestotliwosc zegata w [Hz]
        L_BODOW : NATURAL := 9600; -- predkosc nadawania w [bodach]
        B_SLOWA : NATURAL := 8; -- liczba bitow slowa danych (5-8)
        B_PARZYSTOSCI : NATURAL := 1; -- liczba bitow parzystosci (0-1)
        B_STOPOW : NATURAL := 2; -- liczba bitow stopu (1-2)
        N_RX : BOOLEAN := FALSE; -- negacja logiczna sygnalu szeregowego
        N_SLOWO : BOOLEAN := FALSE -- negacja logiczna slowa danych
    );
    PORT (
        WORK : IN STD_LOGIC; -- sygnal czy dziala
        C : IN STD_LOGIC; -- zegar taktujacy
        RX : IN STD_LOGIC; -- odebrany sygnal szeregowy
        SLOWO : OUT STD_LOGIC_VECTOR(B_SLOWA - 1 DOWNTO 0); -- odebrane slowo danych
        GOTOWE : OUT STD_LOGIC; -- flaga potwierdzenia odbioru
        BLAD : OUT STD_LOGIC -- flaga wykrycia bledu w odbiorze

        wejscie : INOUT STD_LOGIC_VECTOR(0 TO 1); -- podwojny rejestr sygnalu RX
        l_czasu : INOUT NATURAL RANGE 0 TO T; -- licznik czasu jednego bodu
        l_bitow : INOUT NATURAL RANGE 0 TO B_SLOWA - 1; -- licznik odebranych bitow danych lub stopu
        bufor : INOUT STD_LOGIC_VECTOR(SLOWO'RANGE); -- rejestr kolejno odebranych bitow danych
        problem : INOUT STD_LOGIC; -- rejestr (flaga) wykrytego bledu odbioru

    );
END czekanie;

ARCHITECTURE behavioural OF czekanie IS

    --SIGNAL wejscie : STD_LOGIC_VECTOR(0 TO 1); -- podwojny rejestr sygnalu RX

    --type     ETAP		is (CZEKANIE, START, DANA, PARZYSTOSC, STOP); -- lista etapow pracy odbiornika
    --signal   stan		:ETAP;					-- rejestr maszyny stanow odbiornika

    CONSTANT T : POSITIVE := F_ZEGARA/L_BODOW - 1; -- czas jednego bodu - liczba taktów zegara
    --SIGNAL l_czasu : NATURAL RANGE 0 TO T; -- licznik czasu jednego bodu
    --SIGNAL l_bitow : NATURAL RANGE 0 TO B_SLOWA - 1; -- licznik odebranych bitow danych lub stopu

    --SIGNAL bufor : STD_LOGIC_VECTOR(SLOWO'RANGE); -- rejestr kolejno odebranych bitow danych
    --SIGNAL problem : STD_LOGIC; -- rejestr (flaga) wykrytego bledu odbioru

BEGIN

    PROCESS (WORK, C)
    BEGIN
        IF (WORK = '1') THEN

            IF (rising_edge(C)) THEN

                IF (l_czasu /= T) THEN -- badanie odliczania okresu T
                    l_czasu <= l_czasu + 1; -- zwiekszenie o 1 stanu licznika czasu
                ELSE -- zakonczenie odliczania czasu T
                    l_czasu <= 0; -- wyzerowanie licznika czasu bodu

                    IF (l_bitow /= B_STOPOW - 1) THEN -- badanie odliczania bitow stopu
                        l_bitow <= l_bitow + 1; -- zwiekszenie o 1 liczby bitow stopu
                        IF (wejscie(1) /= '0') THEN -- badanie nieprawidlowego stanu bitu STOP
                            problem <= '1'; -- ustawienie rejestru bledu odbioru
                        END IF; -- zakonczenie instukcji warunkowej 
                    ELSE -- zakonczenie odliczania bitow stopu
                        IF (problem = '0' AND wejscie(1) = '0') THEN -- badanie prawidlowego odbioru szeregowego
                            SLOWO <= bufor; -- ustawienie na wyjsciu SLOWO odebranego slowa 
                            IF (N_SLOWO = TRUE) THEN -- badanie warunku zanegowania odebranego slowa
                                SLOWO <= NOT(bufor); -- ustawienie na wyjsciu SLOWO odebranego slowa 
                            END IF; -- zakonczenie instukcji warunkowej  
                            GOTOWE <= '1'; -- ustawienie na wyjsciu flagi potwierdzenia
                        ELSE -- wykryto nieprawidlowy odbioru szeregowy
                            SLOWO <= (OTHERS => '0'); -- wyzerowanie wyjscia danych
                            BLAD <= '1'; -- ustawienie na wyjsciu flagi bledu odbioru
                        END IF; -- zakonczenie instukcji warunkowej
                        stan <= CZEKANIE; -- przejscie do stanu CZEKANIE
                    END IF; -- zakonczenie instukcji warunkowej 

                END IF; -- zakonczenie instukcji warunkowej
            END IF;

        END IF;
    END PROCESS;

END behavioural;