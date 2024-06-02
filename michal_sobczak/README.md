# Interpreter
JPP project

# Zmiany w gramatyce
- Zmieniłem if ( Cond, CondElse). Wcześniej mogłem wykonać tylko jednego stmt. Był to oczywiście błąd, bo może być w ifie wiele instrukcji, dlatego teraz jest tam block.

- zmieniłem 64 linijkę (Types). Wcześniej był tam typ, czyli nie mogłem odróżnić czy jest podany wskaźnik czy nie. 

- zrezygnowałem z TopDef zgodnie z poradą z moodla

# Odwołanie się do odpowiedzi z moodle: 
'''Czy użycie funkcji z parametrem przez zmienną jest legalne w miejscu
gdzie jest oczekiwana funkcja z parametrem przez wartość?
Jeśli jest nielegalne, to trzeba wykryć tę sytuację i odrzucić. '''
- w moim przypadku jest to niedozwolone i wykrywam taką sytuację. Napisałem test w bad "file_from_moodle" który jest bardzo analogiczny do testu, który Pan napisał na moodlu.

# Pewne założenia projektu :
- Używam tylko jednego środowiska, więc niedozwolne jest u mnie trzymać zmienną i funkcję o tej samej nazwie.

# Testy
- testy odpala się poleceniem `./run_tests.sh`

# Zrealizowane punkty
Zreazlizowałem wszystkie zadeklarowane punkty, to znaczy :
• 01 trzy typy
• 02 literały, arytmetyka, porównania
• 03 zmienne, przypisanie
• 04 print
• 05 while, if
• 06 funkcje lub procedury, rekurencja
• 07 przez zmienną / przez wartość
• 09 przesłanianie i statyczne wiązanie
• 10 obsługa błędów wykonania
• 11 funkcje zwracające wartość
• 12 statyczne typowanie
• 13 funkcje zagnieżdżone ze statycznym wiązaniem
• 17 funkcje wyższego rzędu, anonimowe, domknięcia