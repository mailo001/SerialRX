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
        N_SLOWO : BOOLEAN := FALSE; -- negacja logiczna slowa danych
        
        T : POSITIVE := 2082 --F_ZEGARA/L_BODOW - 1 -- czas jednego bodu - liczba takt�w zegara
    );
    PORT (
        WORK : INOUT STD_LOGIC; -- sygnal czy komponent ma działać
        C : IN STD_LOGIC; -- zegar taktujacy
        --RX : IN STD_LOGIC; -- odebrany sygnal szeregowy
        --SLOWO : OUT STD_LOGIC_VECTOR(B_SLOWA - 1 DOWNTO 0); -- odebrane slowo danych
        --GOTOWE : OUT STD_LOGIC; -- flaga potwierdzenia odbioru
        --BLAD : OUT STD_LOGIC -- flaga wykrycia bledu w odbiorze

        wejscie : INOUT STD_LOGIC_VECTOR(0 TO 1); -- podwojny rejestr sygnalu RX
        l_czasu : INOUT NATURAL RANGE 0 TO T; -- licznik czasu jednego bodu
        l_bitow : INOUT NATURAL RANGE 0 TO B_SLOWA - 1; -- licznik odebranych bitow danych lub stopu
        bufor : INOUT STD_LOGIC_VECTOR(B_SLOWA - 1 DOWNTO 0); -- rejestr kolejno odebranych bitow danych
        problem : INOUT STD_LOGIC; -- rejestr (flaga) wykrytego bledu odbioru

        stan_start : OUT STD_LOGIC

    );
END czekanie;

ARCHITECTURE behavioural OF czekanie IS

    --SIGNAL wejscie : STD_LOGIC_VECTOR(0 TO 1); -- podwojny rejestr sygnalu RX

    --type     ETAP		is (CZEKANIE, START, DANA, PARZYSTOSC, STOP); -- lista etapow pracy odbiornika
    --signal   stan		:ETAP;					-- rejestr maszyny stanow odbiornika

    --CONSTANT T : POSITIVE := F_ZEGARA/L_BODOW - 1; -- czas jednego bodu - liczba taktów zegara
    --SIGNAL l_czasu : NATURAL RANGE 0 TO T; -- licznik czasu jednego bodu
    --SIGNAL l_bitow : NATURAL RANGE 0 TO B_SLOWA - 1; -- licznik odebranych bitow danych lub stopu

    --SIGNAL bufor : STD_LOGIC_VECTOR(SLOWO'RANGE); -- rejestr kolejno odebranych bitow danych
    --SIGNAL problem : STD_LOGIC; -- rejestr (flaga) wykrytego bledu odbioru

BEGIN

    PROCESS (WORK, C)
    BEGIN
        IF (WORK = '1') THEN
            IF (rising_edge(C)) THEN

                l_czasu <= 0; -- wyzerowanie licznika czasu bodu
                l_bitow <= 0; -- wyzerowanie licznika odebranych bitow
                bufor <= (OTHERS => '0'); -- wyzerowanie bufora bitow danych
                problem <= '0'; -- wyzerowanie rejestru bledu odbioru
                IF (wejscie(1) = '0' AND wejscie(0) = '1') THEN -- wykrycie poczatku bitu START
                    --stan <= START; -- przejscie do stanu START
                    stan_start <= '1';
                    WORK <= '0';
                END IF; -- zakonczenie instukcji warunkowej

            END IF;
        END IF;
    END PROCESS;

END behavioural;