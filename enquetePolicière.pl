% Types de crime
crime_type(vol).
crime_type(assassinat).
crime_type(escroquerie).

% Faits (preuves)
suspect(john).
suspect(mary).
suspect(alice).
suspect(bruno).
suspect(sophie).

has_motive(john, vol).
was_near_crime_scene(john, vol).
has_fingerprint_on_weapon(john, vol).

has_motive(mary, assassinat).
was_near_crime_scene(mary, assassinat).
has_fingerprint_on_weapon(mary, assassinat).
eyewitness_identification(mary, assassinat).  % Ajout� pour tester la r�gle

has_motive(alice, escroquerie).
has_bank_transaction(alice, escroquerie).
has_bank_transaction(bruno, escroquerie).
owns_fake_identity(sophie, escroquerie).

% R�gles pour d�terminer la culpabilit�
is_guilty(Suspect, vol) :-
    has_motive(Suspect, vol),
    was_near_crime_scene(Suspect, vol),
    has_fingerprint_on_weapon(Suspect, vol).

is_guilty(Suspect, assassinat) :-
    has_motive(Suspect, assassinat),
    was_near_crime_scene(Suspect, assassinat),
    ( has_fingerprint_on_weapon(Suspect, assassinat)
    ; eyewitness_identification(Suspect, assassinat)
    ).

is_guilty(Suspect, escroquerie) :-
    has_motive(Suspect, escroquerie),
    ( has_bank_transaction(Suspect, escroquerie)
    ; owns_fake_identity(Suspect, escroquerie)
    ).

% Entr�e principale (lire un input comme crime(suspect, crime_type) et afficher le r�sultat)
main :-
    current_input(Input),
    read(Input, crime(Suspect, CrimeType)),
    (   is_guilty(Suspect, CrimeType) ->
        format('~w est coupable de ~w.~n', [Suspect, CrimeType])
    ;   format('~w n\'est pas coupable de ~w.~n', [Suspect, CrimeType])
    ),
    halt.
