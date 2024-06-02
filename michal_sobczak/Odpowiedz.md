## Komentarz do maila

1) Rzeczywiście zapomniałem rozwinąć instrukcję Empty - stąd pierwszy błąd który Pan wysłał.

2) Podzieliłem Typy i Błędy na grupy, czyli które pliku się tyczą.

3) Stworzyłem folder : moodle, który zawiera testy, które Pan napisał w mailu. Teraz w tych dwóch przypadkach rzucam błąd, ma to większy sens. Napisałem również testy na And i Or

4) Poprawiłem również funkcję lambdę, modify było zbędne

5) Poprawiłem że operatory logiczne są leniwe

6) interpretBlock był haniebną pozostałością, której nie usunąłem. Teraz już to poprawiłem - teraz według mnie to ma więcej sensu.

7) Uruchomiłem program z flagą, którą Pan mi podesłał. Rzeczywiście okazała się bardzo przydatna. Jedną rzecz pozostawiłem ponieważ, ja tworzę te dwie tablicę i na pewno mają tę samą długość, więc nie trzeba rozpatrywać pozostałych przypadków.

8) Zmodyfikowałem również Typechecker, żeby mógł sprawdzać poprawność zwracanych typów. Teraz Ewaluacja zwraca parę środowisko oraz zwracany typ -> podobnie jak w RunProgram. Dodatkowo niesie ze sobą typ, który powinien zostać zwrócony, żeby sprawdząc w returnie czy typy się zgadzają.
