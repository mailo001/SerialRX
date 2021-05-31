LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.std_logic_unsigned.ALL;
USE ieee.std_logic_misc.ALL;

ENTITY SERIAL_SUM IS
    GENERIC (
        F_ZEGARA : NATURAL := 20_000_000; -- czestotliwosc zegata w [Hz]
        L_BODOW : NATURAL := 9600; -- predkosc nadawania w [bodach]
        B_SLOWA : NATURAL := 8; -- liczba bitow slowa danych (5-8)
        B_PARZYSTOSCI : NATURAL := 1; -- liczba bitow parzystosci (0-1)
        B_STOPOW : NATURAL := 2; -- liczba bitow stopu (1-2)
        N_SERIAL : BOOLEAN := FALSE; -- negacja logiczna sygnalu szeregowego
        N_SLOWO : BOOLEAN := FALSE; -- negacja logiczna slowa danych
        L_CYFR : NATURAL := 3; -- liczba cyfr dziesietnych
        L_BODOW_PRZERWY : NATURAL := 0); -- czas przerwy w nadawaniu w [bodach]
    PORT (
        R : IN STD_LOGIC; -- sygnal resetowania
        C : IN STD_LOGIC; -- zegar taktujacy
        RX : IN STD_LOGIC; -- odbierany sygnal szeregowy
        TX : OUT STD_LOGIC); -- wysylany sygnal szeregowy
END SERIAL_SUM;

ARCHITECTURE behavioural OF SERIAL_SUM IS
    SIGNAL rx_slowo : STD_LOGIC_VECTOR(B_SLOWA - 1 DOWNTO 0); -- odebrane slowo danych
    SIGNAL rx_gotowe : STD_LOGIC; -- flaga potwierdzenia odbioru
    SIGNAL rx_blad : STD_LOGIC; -- flaga wykrycia bledu w odbiorze

    SIGNAL tx_slowo : STD_LOGIC_VECTOR(B_SLOWA - 1 DOWNTO 0); -- wysylane slowo danych
    SIGNAL tx_nadaj : STD_LOGIC; -- flaga zadania nadawania
    SIGNAL tx_wysylanie : STD_LOGIC; -- flaga potwierdzenia nadawania

    TYPE INSTRUKCJA IS (ARGUMENT1, ARGUMENT2, OBLICZAJ, STOJ); -- lista instrukcji pracy interpretera
    SIGNAL rozkaz : INSTRUKCJA; -- rejestr maszyny stanow interpretera

    SUBTYPE CYFRA IS NATURAL RANGE 0 TO 9; -- typ cyfry dziesietnej
    TYPE LICZBA IS ARRAY(NATURAL RANGE <>) OF CYFRA; -- typ liczby dziesietnej zlozonej z cyfr

    SIGNAL arg1 : LICZBA(L_CYFR - 1 DOWNTO 0); -- liczba argumentu 1
    SIGNAL arg2 : LICZBA(L_CYFR - 1 DOWNTO 0); -- liczba argumentu 2
    SIGNAL suma : LICZBA(L_CYFR - 1 DOWNTO 0); -- liczba sumy argumentow
    SIGNAL lcyfr : NATURAL RANGE 0 TO L_CYFR; -- licznik cyfr argumentu

    TYPE OBLICZANIE IS (SUMOWANIE, WYSYLANIE, CZEKANIE); -- lista instrukcji wyznaczania wyniku
    SIGNAL liczenie : OBLICZANIE; -- rejestr maszyny stanow wyznaczania wyniku
    SIGNAL przenos : NATURAL RANGE 0 TO 1; -- wartosc przeniesienia czastkowego sumowania

    CONSTANT SLOWO_ZERO : STD_LOGIC_VECTOR(B_SLOWA - 1 DOWNTO 0) := (OTHERS => '0'); -- slowo z ustawiona wartoscia 0

    CONSTANT T_PRZERWY : INTEGER := (F_ZEGARA/L_BODOW) * L_BODOW_PRZERWY; -- liczba okresow zegara przerwy w nadawaniu
    SIGNAL lprzerwy : NATURAL RANGE 0 TO T_PRZERWY; -- licznik taktow przerwy w nadawaniu

BEGIN -- cialo architekury sumowania

    srx : ENTITY work.SERIAL_RX(behavioural) -- instancja odbirnika szeregowego 'SERIAL_RX'
        GENERIC MAP(-- mapowanie parametrow biezacych
            F_ZEGARA => F_ZEGARA, -- czestotliwosc zegata w [Hz]
            L_BODOW => L_BODOW, -- predkosc odbierania w [bodach]
            B_SLOWA => B_SLOWA, -- liczba bitow slowa danych (5-8)
            B_PARZYSTOSCI => B_PARZYSTOSCI, -- liczba bitow parzystosci (0-1)
            B_STOPOW => B_STOPOW, -- liczba bitow stopu (1-2)
            N_RX => N_SERIAL, -- negacja logiczna sygnalu szeregowego
            N_SLOWO => N_SLOWO -- negacja logiczna slowa danych
        )
        PORT MAP(-- mapowanie sygnalow do portow
            R => R, -- sygnal resetowania
            C => C, -- zegar taktujacy
            RX => RX, -- odebrany sygnal szeregowy
            SLOWO => rx_slowo, -- odebrane slowo danych
            GOTOWE => rx_gotowe, -- flaga potwierdzenia odbioru
            BLAD => rx_blad -- flaga wykrycia bledu w odbiorze
        );

    stx : ENTITY work.SERIAL_TX(behavioural) -- instancja nadajnika szeregowego 'SERIAL_TX'
        GENERIC MAP(-- mapowanie parametrow biezacych
            F_ZEGARA => F_ZEGARA, -- czestotliwosc zegata w [Hz]
            L_BODOW => L_BODOW, -- predkosc nadawania w [bodach]
            B_SLOWA => B_SLOWA, -- liczba bitow slowa danych (5-8)
            B_PARZYSTOSCI => B_PARZYSTOSCI, -- liczba bitow parzystosci (0-1)
            B_STOPOW => B_STOPOW, -- liczba bitow stopu (1-2)
            N_TX => N_SERIAL, -- negacja logiczna sygnalu szeregowego
            N_SLOWO => N_SLOWO -- negacja logiczna slowa danych
        )
        PORT MAP(-- mapowanie sygnalow do portow
            R => R, -- sygnal resetowania
            C => C, -- zegar taktujacy
            TX => tx, -- nadawany sygnal szeregowy
            SLOWO => tx_slowo, -- nadawane slowo danych
            NADAJ => tx_nadaj, -- flaga zadania nadawania
            WYSYLANIE => tx_wysylanie -- flaga potwierdzenia nadawania
        );

    PROCESS (R, C) IS -- proces kalkulatora

        FUNCTION kod_znaku(c : CHARACTER) RETURN STD_LOGIC_VECTOR IS -- konwersja kodu znaku do rozmiaru slowa
        BEGIN -- cialo funkcji
            RETURN(SLOWO_ZERO + CHARACTER'pos(c)); -- wyznaczenia i zwrocenie wartosci slowa
        END FUNCTION; -- zakonczenie funkcji

        CONSTANT BLAD_ODBIORU : STD_LOGIC_VECTOR := kod_znaku('!'); -- slowo z kodem przypisanym do bledu odbioru
        CONSTANT BLAD_INSTRUKCJI : STD_LOGIC_VECTOR := kod_znaku('?'); -- slowo z kodem przypisanym do bledu instrukcji

        FUNCTION wyzn_cyfre(a : STD_LOGIC_VECTOR) RETURN NATURAL IS -- konwersja kodu slowa zawierajacego cyfre na warosc
        BEGIN -- cialo funkcji
            IF (a >= kod_znaku('0') AND a <= kod_znaku('9')) THEN -- zbadanie czy kod slowa jest cyfra
                RETURN(CONV_INTEGER(a) - CHARACTER'pos('0')); -- wyznaczenia i zwrocenie wartosci cyfry
            ELSE -- lowo nie jest cyfra
                RETURN(10); -- zwrocenie flagi bledu jako wartosci 10
            END IF; -- zakonczenie instukcji warunkowej
        END FUNCTION; -- zakonczenie funkcji

        VARIABLE suma_cyfr : NATURAL RANGE 0 TO 19;

    BEGIN -- poczatek ciala procesu kalkulatora

        IF (R = '1') THEN -- asynchroniczna inicjalizacja rejestrow

            tx_slowo <= (OTHERS => '0'); -- wyzerowanie nadawanego slowa danych
            tx_nadaj <= '0'; -- wyzerowanie flagi zadania nadawania
            rozkaz <= ARGUMENT1; -- poczatkowy stan pracy interpretera
            arg1 <= (OTHERS => 0); -- wyzerowanie argumetu 1
            arg2 <= (OTHERS => 0); -- wyzerowanie argumetu 2
            suma <= (OTHERS => 0); -- wyzerowanie sumy argumentow
            lcyfr <= 0; -- wyzerowanie licznika cyfr
            przenos <= 0; -- wyzerowanie wartosci przeniesienia
            lprzerwy <= 0; -- wyzerowanie licznika przerwy w nadawaniu

        ELSIF (rising_edge(C)) THEN -- synchroniczna praca kalkulatora

            tx_nadaj <= '0'; -- defaultowe ustawienie flagi zadania nadawania

            IF (rx_blad = '1') THEN -- obsluga bledu odbioru zgloszonego przez 'SERIAL_RX'
                tx_slowo <= BLAD_ODBIORU; -- ustawienie slowa nadawania na BLAD_ODBIORU
                tx_nadaj <= '1'; -- ustawienie flagi zadania nadawania przez 'SERIAL_TX'
                rozkaz <= STOJ; -- przejscie awaryjne do stanu STOJ
            ELSIF (rx_gotowe = '1') THEN -- obsluga potwierdzenia odbioru slowa przez 'SERIAL_RX'
                tx_slowo <= rx_slowo; -- ustawienie slowa nadawania na slowo odebrane (echo)
                tx_nadaj <= '1'; -- ustawienie flagi zadania nadawania przez 'SERIAL_TX'
                IF (rx_slowo = kod_znaku(LF) OR rx_slowo = kod_znaku(CR)) THEN -- zbadanie zadania inicjalizacji
                    rozkaz <= ARGUMENT1; -- poczatkowy stan pracy interpretera
                    arg1 <= (OTHERS => 0); -- wyzerowanie argumetu 1
                    arg2 <= (OTHERS => 0); -- wyzerowanie argumetu 2
                    suma <= (OTHERS => 0); -- wyzerowanie sumy argumentow
                    lcyfr <= 0; -- wyzerowanie licznika cyfr
                ELSE -- interpretacja odebranego slowa
                    CASE rozkaz IS -- badanie aktualnego stanu maszyny interpretera 

                        WHEN ARGUMENT1 => -- obsluga stanu ARGUMENT1
                            IF (rx_slowo = kod_znaku('+')) THEN -- odebrano znak operatora sumowania
                                lcyfr <= 0; -- wyzerowanie licznika cyfr
                                rozkaz <= ARGUMENT2; -- przejscie do stanu ARGUMENT2
                            ELSIF (wyzn_cyfre(rx_slowo) /= 10) THEN -- odebrano znak cyfry
                                arg1(0) <= wyzn_cyfre(rx_slowo); -- zapamietanie warosci cyfry w wektorze arg1
                                arg1(arg1'left DOWNTO 1) <= arg1(arg1'left - 1 DOWNTO 0); -- przesuniecie w lewo wektora arg1
                                IF (lcyfr /= L_CYFR) THEN -- badanie liczby odebrsnych cyfr
                                    lcyfr <= lcyfr + 1; -- zwiekszenie o 1 liczby odebrsnych cyfr
                                ELSE -- przekroczono liczbe odebrsnych cyfr
                                    tx_slowo <= BLAD_INSTRUKCJI; -- ustawienie slowa nadawania na BLAD_INSTRUKCJI
                                    rozkaz <= STOJ; -- przejscie awaryjne do stanu STOJ
                                END IF; -- zakonczenie instukcji warunkowej
                            ELSE -- odebrano nieprawidlowy znak
                                tx_slowo <= BLAD_INSTRUKCJI; -- ustawienie slowa nadawania na BLAD_INSTRUKCJI
                                rozkaz <= STOJ; -- przejscie awaryjne do stanu STOJ
                            END IF; -- zakonczenie instukcji warunkowej

                        WHEN ARGUMENT2 => -- obsluga stanu ARGUMENT2
                            IF (rx_slowo = kod_znaku('=')) THEN -- odebrano znak operatora rownosci
                                lcyfr <= 0; -- wyzerowanie licznika cyfr
                                rozkaz <= OBLICZAJ; -- przejscie do stanu OBLICZAJ
                            ELSIF (wyzn_cyfre(rx_slowo) /= 10) THEN -- odebrano znak cyfry
                                arg2(0) <= wyzn_cyfre(rx_slowo); -- zapamietanie warosci cyfry w wektorze arg2
                                arg2(arg2'left DOWNTO 1) <= arg2(arg2'left - 1 DOWNTO 0); -- przesuniecie w lewo wektora arg2
                                IF (lcyfr /= L_CYFR) THEN -- badanie liczby odebrsnych cyfr
                                    lcyfr <= lcyfr + 1; -- zwiekszenie o 1 liczby odebrsnych cyfr
                                ELSE -- przekroczono liczbe odebrsnych cyfr
                                    tx_slowo <= BLAD_INSTRUKCJI; -- ustawienie slowa nadawania na BLAD_INSTRUKCJI
                                    rozkaz <= STOJ; -- przejscie awaryjne do stanu STOJ
                                END IF; -- zakonczenie instukcji warunkowej
                            ELSE -- odebrano nieprawidlowy znak
                                tx_slowo <= BLAD_INSTRUKCJI; -- ustawienie slowa nadawania na BLAD_INSTRUKCJI
                                rozkaz <= STOJ; -- przejscie awaryjne do stanu STOJ
                            END IF; -- zakonczenie instukcji warunkowej

                        WHEN OBLICZAJ => NULL; -- pusta obsluga stanu OBLICZAJ

                        WHEN STOJ => -- pusta obsluga stanu STOJ
                            tx_slowo <= BLAD_INSTRUKCJI; -- ustawienie slowa nadawania na BLAD_INSTRUKCJI

                    END CASE; -- zakonczenie instukcji warunkowego wyboru
                END IF; -- zakonczenie instukcji warunkowej
            END IF; -- zakonczenie instukcji warunkowej

            IF (rozkaz /= OBLICZAJ) THEN -- oczekiwanie na stan OBLICZAJ interpretera
                przenos <= 0; -- wyzerowanie wartosci przeniesienia
                liczenie <= SUMOWANIE; -- ustawienie poczatkowe stanu SUMOWANIE
                lprzerwy <= 0; -- wyzerowanie licznika przerwy w nadawaniu
            ELSE -- osiagnieto stan OBLICZAJ
                CASE liczenie IS

                    WHEN SUMOWANIE => -- obsluga stanu SUMOWANIE
                        suma_cyfr := arg1(0) + arg2(0) + przenos; -- wyznaczenie sumy czastkowej
                        arg1(arg1'left - 1 DOWNTO 0) <= arg1(arg1'left DOWNTO 1); -- przesuniecie w prawo wektora arg1
                        arg2(arg2'left - 1 DOWNTO 0) <= arg2(arg2'left DOWNTO 1); -- przesuniecie w prawo wektora arg2
                        IF (suma_cyfr < 10) THEN -- zbadanie czy nie powstalo przeniesienie
                            suma(0) <= suma_cyfr; -- zapamietanie warosci cyfry w wektorze sumy
                            przenos <= 0; -- wyzerowanie wartosci przeniesienia
                        ELSE -- wariant gdy powstalo przeniesienie
                            suma(0) <= suma_cyfr - 10; -- zapamietanie warosci cyfry w wektorze sumy
                            przenos <= 1; -- ustawienie wartosci przeniesienia
                        END IF; -- zakonczenie instukcji warunkowej
                        suma(suma'left DOWNTO 1) <= suma(suma'left - 1 DOWNTO 0); -- przesuniecie w lewo wektora sumy
                        IF (lcyfr /= L_CYFR - 1) THEN -- badanie czy pozostaly cyfry do sunowania
                            lcyfr <= lcyfr + 1; -- zwiekszenie o 1 liczby zsumowanych cyfr
                        ELSE -- wykonano sumowanie czastkowe wszystkich cyfr
                            lcyfr <= 0; -- wyzerowanie licznika cyfr
                            przenos <= 0; -- wyzerowanie wartosci przeniesienia
                            liczenie <= CZEKANIE; -- przejscie do stanu CZEKANIE

                        END IF; -- zakonczenie instukcji warunkowej

                    WHEN WYSYLANIE => -- obsluga stanu WYSYLANIE
                        IF (lcyfr /= L_CYFR) THEN -- badanie czy pozostaly cyfry do wyslania
                            lcyfr <= lcyfr + 1; -- zwiekszenie o 1 liczby wyslanych cyfr
                            IF (przenos = 1 OR suma(0) /= 0 OR lcyfr = L_CYFR - 1) THEN -- badanie czy nalezy wyslac cyfre 
                                tx_slowo <= SLOWO_ZERO + CHARACTER'pos('0') + suma(0); -- wyznaczenie i ustawienie kodu wysylanej cyfry
                                tx_nadaj <= '1'; -- ustawienie flagi zadania nadawania przez 'SERIAL_TX'
                                przenos <= 1; -- ustawienie flagi przeniesienia jako znacznika wysylania
                                liczenie <= CZEKANIE; -- przejscie do stanu CZEKANIE
                            END IF; -- zakonczenie instukcji warunkowej
                        ELSE -- wykonano wyslanie wszystkich cyfr
                            liczenie <= SUMOWANIE; -- przejscie do stanu SUMOWANIE
                            rozkaz <= STOJ; -- przejscie do stanu STOJ interpretera
                        END IF; -- zakonczenie instukcji warunkowej
                        suma(suma'left - 1 DOWNTO 0) <= suma(suma'left DOWNTO 1); -- przesuniecie w prawo wektora sumy

                    WHEN CZEKANIE => -- obsluga stanu CZEKANIE
                        IF (tx_nadaj = '0' AND tx_wysylanie = '0') THEN -- badanie czy 'SERIAL_TX' nie jest aktywny
                            IF (lprzerwy /= T_PRZERWY) THEN -- badanie czy trwa przerwa w nadawniu
                                lprzerwy <= lprzerwy + 1; -- wzwieksza licznika przerwy w nadawaniu
                            ELSE -- osiagnieto czas przerwy w nadawaniu
                                lprzerwy <= 0; -- wyzerowanie licznika przerwy w nadawaniu
                                liczenie <= WYSYLANIE; -- przejscie do stanu WYSYLANIE
                            END IF; -- zakonczenie instukcji warunkowej
                        ELSE -- wariant, gdy 'SERIAL_TX' jest aktywny
                            lprzerwy <= 0; -- wyzerowanie licznika przerwy w nadawaniu
                        END IF; -- zakonczenie instukcji warunkowej

                END CASE;
            END IF; -- zakonczenie instukcji warunkowej

        END IF; -- zakonczenie instukcji warunkowej procesu

    END PROCESS; -- zakonczenie ciala kalkulatora

END behavioural;