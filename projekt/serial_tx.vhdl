LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.std_logic_unsigned.ALL;
USE ieee.std_logic_misc.ALL;

ENTITY SERIAL_TX IS
    GENERIC (
        F_ZEGARA : NATURAL := 20000000; -- czestotliwosc zegata w [Hz]
        L_BODOW : NATURAL := 9600; -- predkosc nadawania w [bodach]
        B_SLOWA : NATURAL := 8; -- liczba bitow slowa danych (5-8)
        B_PARZYSTOSCI : NATURAL := 1; -- liczba bitow parzystosci (0-1)
        B_STOPOW : NATURAL := 2; -- liczba bitow stopu (1-2)
        N_TX : BOOLEAN := FALSE; -- negacja logiczna sygnalu szeregowego
        N_SLOWO : BOOLEAN := FALSE -- negacja logiczna slowa danych
    );
    PORT (
        R : IN STD_LOGIC; -- sygnal resetowania
        C : IN STD_LOGIC; -- zegar taktujacy
        TX : OUT STD_LOGIC; -- wysylany sygnal szeregowy
        SLOWO : IN STD_LOGIC_VECTOR(B_SLOWA - 1 DOWNTO 0); -- wysylane slowo danych
        NADAJ : IN STD_LOGIC; -- flaga zadania nadania
        WYSYLANIE : OUT STD_LOGIC -- flaga potwierdzenia wysylanie
    );
END SERIAL_TX;

ARCHITECTURE behavioural OF SERIAL_TX IS

    SIGNAL bufor : STD_LOGIC_VECTOR(SLOWO'RANGE); -- rejestr kolejno odebranych bitow danych
    SIGNAL f_parzystosc : STD_LOGIC; -- flaga parzystosci

    TYPE ETAP IS (CZEKANIE, START, DANA, PARZYSTOSC, STOP); -- lista etapow pracy odbiornika
    SIGNAL stan : ETAP; -- rejestr maszyny stanow odbiornika

    SIGNAL nadawaj : STD_LOGIC; -- wysylany sygnal szeregowy

    CONSTANT T : POSITIVE := F_ZEGARA/L_BODOW - 1; -- czas jednego bodu - liczba taktów zegara
    SIGNAL l_czasu : NATURAL RANGE 0 TO T; -- licznik czasu jednego bodu
    SIGNAL l_bitow : NATURAL RANGE 0 TO B_SLOWA - 1; -- licznik odebranych bitow danych lub stopu
BEGIN

    PROCESS (R, C) IS -- proces odbiornika
    BEGIN

        IF (R = '1') THEN -- asynchroniczna inicjalizacja rejestrow
            bufor <= (OTHERS => '0'); -- wyzerowanie bufora bitow danych
            f_parzystosc <= '0'; -- wyzerowanie flagi parzystosci
            stan <= CZEKANIE; -- poczatkowy stan pracy odbiornika
            l_czasu <= 0; -- wyzerowanie licznika czasu bodu
            l_bitow <= 0; -- wyzerowanie licznika odebranych bitow
            nadawaj <= '0'; -- wyzerowanie sygnalu nadawania szeregowego
            WYSYLANIE <= '0'; -- wyzerowanie flagi potwierdzenia nadania

        ELSIF (rising_edge(C)) THEN -- synchroniczna praca nadajnika

            nadawaj <= '0'; -- defaultowe ustawienie sygnalu nadawania szeregowego
            WYSYLANIE <= '1'; -- defaultowe ustawienie flagi potwierdzenia wysylania

            CASE stan IS -- badanie aktualnego stanu maszyny stanow 

                WHEN CZEKANIE => -- obsluga stanu CZEKANIE
                    l_czasu <= 0; -- wyzerowanie licznika czasu bodu
                    l_bitow <= 0; -- wyzerowanie licznika odebranych bitow
                    IF (NADAJ = '1') THEN -- wykrycie zadania nadawania
                        stan <= START; -- przejscie do stanu START
                        bufor <= SLOWO; -- zapisanie bufora bitow danych
                        f_parzystosc <= XOR_REDUCE(SLOWO); -- wyznaczenie flagi parzystosci
                        IF (N_SLOWO = TRUE) THEN -- badanie warunku zanegowania odebranego slowa
                            bufor <= NOT(SLOWO); -- zapisanie bufora zanegowanych bitow danych
                            f_parzystosc <= NOT(XOR_REDUCE(SLOWO)); -- wyznaczenie flagi parzystosci
                        END IF; -- zakonczenie instukcji warunkowej
                    ELSE -- wariant braku zadania nadawania
                        WYSYLANIE <= '0'; -- kasowanie flagi potwierdzenia wysylania
                    END IF; -- zakonczenie instukcji warunkowej

                WHEN START => -- obsluga stanu START
                    nadawaj <= '1'; -- wysylanie bitu STRAT
                    IF (l_czasu /= T) THEN -- badanie odliczania okresu T
                        l_czasu <= l_czasu + 1; -- zwiekszenie o 1 stanu licznika czasu
                    ELSE -- zakonczenie odliczania czasu T/2
                        l_czasu <= 0; -- wyzerowanie licznika czasu bodu
                        stan <= DANA; -- przejscie do stanu DANA
                    END IF; -- zakonczenie instukcji warunkowej

                WHEN DANA => -- obsluga stanu DANA
                    nadawaj <= bufor(0); -- wysylanie najmlodszego bitu danych bufora
                    IF (l_czasu /= T) THEN -- badanie odliczania okresu T
                        l_czasu <= l_czasu + 1; -- zwiekszenie o 1 stanu licznika czasu
                    ELSE -- zakonczenie odliczania czasu T
                        bufor(bufor'left) <= '0'; -- kasowanie najstarszego bitu danych
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
                    nadawaj <= f_parzystosc; -- wysylanie bitu parzystosci
                    IF (l_czasu /= T) THEN -- badanie odliczania okresu T
                        l_czasu <= l_czasu + 1; -- zwiekszenie o 1 stanu licznika czasu
                    ELSE -- zakonczenie odliczania czasu T
                        l_czasu <= 0; -- wyzerowanie licznika czasu bodu
                        stan <= STOP; -- przejscie do stanu STOP
                    END IF; -- zakonczenie instukcji warunkowej

                WHEN STOP => -- obsluga stanu STOP
                    IF (l_czasu /= T) THEN -- badanie odliczania okresu T
                        l_czasu <= l_czasu + 1; -- zwiekszenie o 1 stanu licznika czasu
                    ELSE -- zakonczenie odliczania czasu T
                        l_czasu <= 0; -- wyzerowanie licznika czasu bodu

                        IF (l_bitow /= B_STOPOW - 1) THEN -- badanie odliczania bitow stopu
                            l_bitow <= l_bitow + 1; -- zwiekszenie o 1 liczby bitow stopu
                        ELSE -- zakonczenie odliczania bitow stopu
                            WYSYLANIE <= '0'; -- kasowanie flagi potwierdzenia wysylania
                            stan <= CZEKANIE; -- przejscie do stanu CZEKANIE
                        END IF; -- zakonczenie instukcji warunkowej 

                    END IF; -- zakonczenie instukcji warunkowej

            END CASE; -- zakonczenie instukcji warunkowego wyboru

        END IF; -- zakonczenie instukcji warunkowej porcesu

    END PROCESS; -- zakonczenie ciala procesu

    TX <= nadawaj WHEN N_TX = FALSE ELSE
        NOT(nadawaj); -- opcjonalne zanegowanie sygnalu TX

END behavioural;