LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.std_logic_unsigned.ALL;
USE ieee.std_logic_misc.ALL;

ENTITY SERIAL_RX IS
    GENERIC (
        MAX_LICZBA : NATURAL := FALSE -- negacja logiczna slowa danych
        B_SLOWA : NATURAL := 8; -- liczba bitow slowa danych (5-8)
    );
    PORT (
        R : IN STD_LOGIC; -- sygnal resetowania
        DANA : IN STD_LOGIC;
        C : IN STD_LOGIC; -- zegar taktujacy
        SLOWO : IN STD_LOGIC_VECTOR(B_SLOWA - 1 DOWNTO 0); -- odbirane slowo danych

        GOTOWE : OUT STD_LOGIC;
        LICZBA : OUT NATURAL RANGE 0 TO MAX_LICZBA;
        PLUS : OUT STD_LOGIC;
        MINUS : OUT STD_LOGIC;
        RAZY : OUT STD_LOGIC;
        ROWNA : OUT STD_LOGIC;

        BLAD : OUT STD_LOGIC -- flaga wykrycia bledu w odbiorze
    );
END SERIAL_RX;

ARCHITECTURE behavioural OF SERIAL_RX IS

    TYPE ETAP IS (CZEKANIE, LICZENIE, WYSYLANIE); -- lista etapow pracy odbiornika
    SIGNAL stan : ETAP; -- rejestr maszyny stanow odbiornika

    SIGNAL odebrane : STD_LOGIC_VECTOR(B_SLOWA - 1 DOWNTO 0);
    SIGNAL cyfra : NATURAL RANGE 0 TO MAX_LICZBA;
    SIGNAL liczba : NATURAL RANGE 0 TO MAX_LICZBA;

BEGIN

    PROCESS (R, C) IS -- proces odbiornika
    BEGIN -- cialo procesu odbiornika

        IF (R = '1') THEN -- asynchroniczna inicjalizacja rejestrow
            stan <= CZEKANIE; -- poczatkowy stan pracy odbiornika

            GOTOWE <= '0'; -- wyzerowanie flagi potwierdzenia odbioru
            LICZBA <= 0;
            PLUS <= '0';
            MINUS <= '0';
            RAZY <= '0';
            ROWNA <= '0';
            BLAD <= '0'; -- wyzerowanie flagi wykrycia bledu w odbiorze

        ELSIF (rising_edge(C)) THEN -- synchroniczna praca odbiornika

            IF (DANA = '1') THEN
                odebrane <= SLOWO;
                stan <= LICZENIE;
            END IF;
            
            CASE stan IS -- badanie aktualnego stanu maszyny stanow 

                WHEN CZEKANIE => NULL -- obsluga stanu CZEKANIE

                WHEN DANA => -- obsluga stanu DANA
                    IF (l_czasu /= T) THEN -- badanie odliczania okresu T
                        l_czasu <= l_czasu + 1; -- zwiekszenie o 1 stanu licznika czasu
                    ELSE -- zakonczenie odliczania czasu T
                        bufor(bufor'left) <= wejscie(1); -- zapamietanie stanu bitu danych
                        bufor(bufor'left - 1 DOWNTO 0) <= bufor(bufor'left DOWNTO 1); -- przesuniecie bitow w buforze
                        l_czasu <= 0; -- wyzerowanie licznika czasu bodu

                        IF (l_bitow /= B_SLOWA - 1) THEN -- badanie odliczania bitow danych
                            l_bitow <= l_bitow + 1; -- zwiekszenie o 1 liczby bitow danych
                        ELSE -- zakonczenie odliczania bitow danych
                            l_bitow <= 0; -- wyzerowanie licznika odebranych bitow
                            IF (B_PARZYSTOSCI = 1) THEN -- badanie odbioru bitu parzystosci
                                stan <= PARZYSTOSC; -- przejscie do stanu PARZYSTOSC
                            ELSE -- brak odbioru bitu parzystosci  
                                stan <= STOP; -- przejscie do stanu STOP
                            END IF; -- zakonczenie instukcji warunkowej
                        END IF; -- zakonczenie instukcji warunkowej 

                    END IF; -- zakonczenie instukcji warunkowej

                WHEN PARZYSTOSC => -- obsluga stanu PARZYSTOSC
                    IF (l_czasu /= T) THEN -- badanie odliczania okresu T
                        l_czasu <= l_czasu + 1; -- zwiekszenie o 1 stanu licznika czasu
                    ELSE -- zakonczenie odliczania czasu T
                        l_czasu <= 0; -- wyzerowanie licznika czasu bodu
                        stan <= STOP; -- przejscie do stanu STOP
                        IF ((wejscie(1) = XOR_REDUCE(bufor)) = N_SLOWO) THEN -- badanie nieprawidlowej parzystosci bitow
                            problem <= '1'; -- ustawienie rejestru bledu odbioru
                        END IF; -- zakonczenie instukcji warunkowej 
                    END IF; -- zakonczenie instukcji warunkowej

                WHEN STOP => -- obsluga stanu STOP
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

            END CASE; -- zakonczenie instukcji warunkowego wyboru

        END IF; -- zakonczenie instukcji warunkowej porcesu

    END PROCESS; -- zakonczenie ciala procesu

END behavioural;